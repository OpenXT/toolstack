(*
 * Copyright (c) 2013 Citrix Systems, Inc.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 *)

(* An example one:
 /usr/lib/xen/bin/qemu-dm -d 39 -m 256 -boot cd -serial pty -usb -usbdevice tablet -domain-name bee94ac1-8f97-42e0-bf77-5cb7a6b664ee -net nic,vlan=1,macaddr=00:16:3E:76:CE:44,model=rtl8139 -net tap,vlan=1,bridge=xenbr0 -vnc 39 -k en-us -vnclisten 127.0.0.1
*)

open Printf
open Pervasiveext

module D = Debug.Debugger(struct let name = "xenops" end)
open D

exception Ioemu_failed of string
exception Ioemu_failed_dying

let qemu_dm_ready_timeout = 60. *. 20. (* seconds *)
let qemu_dm_shutdown_timeout = 60. *. 20. (* seconds *)

type disp_opt =
	| NONE
	| VNC of bool * string * int * string (* auto-allocate, bind address could be empty, port if auto-allocate false, keymap *)
	| SDL of string (* X11 display *)

type info = {
	hvm: bool;
	memory: int64;
	boot: string;
	serial: string;
	vcpus: int;
	usb: string list;
	nics: (string * (string*string) * string option * bool * int) list;
	acpi: bool;
	disp: disp_opt;
	pci_emulations: string list;
	sound: string option;
	vsnd: bool;
	power_mgmt: int;
	oem_features: int;
	inject_sci: int;
	videoram: int;
	extras: (string * string option) list;
	disks: Device.Vbd.info list;
	vifs: (int * Device.Vif.info) list; (* backend-domid + info list. fold nics with this one *)
	pcis: (int * Device.PCI.dev list) list;
	stubdom: ([`domain] Uuid.t) option;
	stubdom_memory: int64;
	stubdom_kernel: string;
	stubdom_initrd: string option;
	stubdom_cmdline: string;
}

(* Path to redirect qemu's stdout and stderr *)
let logfile domid = Printf.sprintf "/tmp/qemu.%d" domid

(* Called when destroying the domain to spool the log to the main debug log *)
let write_logfile_to_log domid =
	let logfile = logfile domid in
	try
		let fd = Unix.openfile logfile [ Unix.O_RDONLY ] 0o0 in
		finally
		  (fun () -> debug "qemu-dm: logfile contents: %s" (Unixext.read_whole_file 1024 1024 fd))
		  (fun () -> Unix.close fd)
	with e ->
		debug "Caught exception reading qemu log file from %s: %s" logfile (Printexc.to_string e);
		raise e

let unlink_logfile domid = Unix.unlink (logfile domid)

(* Where qemu writes its port number *)
let vnc_port_path domid = sprintf "/local/domain/%d/console/vnc-port" domid

(* Where qemu writes its state and is signalled *)
let device_model_path domid = sprintf "/local/domain/0/device-model/%d" domid
let dmhelpers_path domid = sprintf "/local/domain/0/device-model/%d/dm-helpers" domid
let dmhelper_path domid index = sprintf "/local/domain/0/device-model/%d/dm-helpers/%d" domid index

let power_mgmt_path domid = sprintf "/local/domain/0/device-model/%d/xen_extended_power_mgmt" domid
let oem_features_path domid = sprintf "/local/domain/0/device-model/%d/oem_features" domid
let inject_sci_path domid = sprintf "/local/domain/0/device-model/%d/inject-sci" domid

let signal ~xs ~domid ?wait_for ?param cmd =
	let cmdpath = device_model_path domid in
	Xs.transaction xs (fun t ->
		t.Xst.write (cmdpath ^ "/command") cmd;
		match param with
		| None -> ()
		| Some param -> t.Xst.write (cmdpath ^ "/parameter") param
	);
	match wait_for with
	| Some state ->
		let pw = cmdpath ^ "/state" in
		Watch.wait_for ~xs (Watch.value_to_become pw state)
	| None -> ()

(* Prepare some XenStore node it's common with dm-agent *)
let prepare_domain ~xs info domid =
	if info.power_mgmt <> 0 then begin
		try if (Unix.stat "/proc/acpi/battery").Unix.st_kind == Unix.S_DIR then
				xs.Xs.write (power_mgmt_path domid) (string_of_int info.power_mgmt);
		with _ -> () ;
	end;

	if info.oem_features <> 0 then
		xs.Xs.write (oem_features_path domid) (string_of_int info.oem_features);

	if info.inject_sci <> 0 then
		xs.Xs.write (inject_sci_path domid) (string_of_int info.inject_sci)

let cmdlineargs ~xs ~restore info domid =
	let usb' =
		if info.usb = [] then
			[]
		else
			("-usb" :: (List.concat (List.map (fun device ->
					   [ "-usbdevice"; device ]) info.usb))) in
	(* qemu need a different id for every vlan, or things get very bad *)
	let vlan_id = ref 0 in
	let if_number = ref 0 in
	let nics' = List.map (fun (mac, (netpath,bridge), model, wireless, id) ->
		let modelstr =
			match model with
			| None   -> "rtl8139"
			| Some m -> m
			in
		let r = [
		"-net"; sprintf "nic,vlan=%d,macaddr=%s,model=%s" !vlan_id mac modelstr;
		"-net"; sprintf "tap,vlan=%d,bridge=%s,ifname=%s" !vlan_id bridge (Printf.sprintf "tap%d.%d" domid !if_number)] in
		incr if_number;
		incr vlan_id;
		r
	) info.nics in

	prepare_domain ~xs info domid;

	let restorefile = sprintf "/tmp/xen.qemu-dm.%d" domid in
	let disp_options, wait_for_port =
		match info.disp with
		| NONE                     -> [], false
		| SDL (x11name)            -> [], false
		| VNC (auto, bindaddr, port, keymap) ->
			if auto
			then [ "-vncunused"; "-k"; keymap ], true
			else [ "-vnc"; bindaddr ^ ":" ^ string_of_int port; "-k"; keymap ], true
		in
	let sound_options =
		match info.sound with
		| None        -> []
		| Some device -> [ "-soundhw"; device ]
		in
	let args = [ "-d"; string_of_int domid; ]
	  @ (if info.hvm then [
		"-m"; Int64.to_string (Int64.div info.memory 1024L);
		"-videoram"; string_of_int info.videoram;
		"-vcpus"; string_of_int info.vcpus; ]
		else ["-videoram"; string_of_int info.videoram])
	  @ (if info.boot   = "" then [] else [ "-boot"; info.boot; ])
	  @ (if info.serial = "" then [] else [ "-serial"; info.serial; ])
	  @ [ "-M"; (if info.hvm then "xenfv" else "xenpv"); ]
	  @ disp_options @ sound_options @ usb' @ (if nics' <> [] then (List.concat nics') else ["-net none"])
	  @ (if info.acpi then [ "-acpi" ] else [])
	  @ (if restore then [ "-loadvm"; restorefile ] else [])
	  @ (List.fold_left (fun l pci -> "-pciemulation" :: pci :: l) [] (List.rev info.pci_emulations))
	  @ (List.fold_left (fun l (k, v) -> ("-" ^ k) :: (match v with None -> l | Some v -> v :: l)) [] info.extras)
	  @ (if info.vcpus > 1
		  then ["-smp"; string_of_int info.vcpus]
		  else [])
		in
	(args, wait_for_port)

let stubdom_mac mac =
	let msb  = int_of_string ("0x" ^ String.sub mac 0 2) in
	let tail = String.sub mac 2 (String.length mac - 2) in
	let msb' = msb lxor 0x02 in
	sprintf "%02x" msb' ^ tail

type helperapp = {
	path : string;
}

let stubdom_helpers =
	[
		{ path = "/usr/lib/xen/bin/atapi_pt_helper" };
		{ path = "/usr/lib/xen/bin/audio_helper_start" };
	]
		
let fork_stubdom_helpers ~xs uuid target_domid stubdom_domid =
	info "starting stubdom helpers";
	let run_helper h =
		if Sys.file_exists h.path then (
			let pid = Forkhelpers.safe_close_and_exec ~withpath:true [] [Unix.stdout; Unix.stderr] h.path [ string_of_int target_domid; string_of_int stubdom_domid ] in
			info "started %s (pid: %d) for %s" h.path pid (Uuid.to_string uuid);
			xs.Xs.write (dmhelper_path target_domid pid) h.path
		)
	in
	List.iter run_helper stubdom_helpers

let fork_dm_helpers ~xs vsnd target_domid =
	info "starting dm helpers";
	let run_helper path args =
		if Sys.file_exists path then (
			let pid = Forkhelpers.safe_close_and_exec ~withpath:true [] [Unix.stdout; Unix.stderr] path args in
			info "started %s (pid: %d) for domain %d" path pid target_domid;
			xs.Xs.write (dmhelper_path target_domid pid) path
		)
	in
	(* audio daemon *)
	if vsnd then run_helper "/usr/lib/xen/bin/audio-daemon-start" [ string_of_int target_domid ]

let list_helpers ~xs target_domid =
	try  List.map int_of_string (xs.Xs.directory (dmhelpers_path target_domid))
	with Xb.Noent -> []

let stop_helpers ~xs target_domid =
	info "stopping dm helpers";
	let hs = list_helpers ~xs target_domid in
	let stop h =
		try
			info "stopping dm helper pid=%d" h;
			Unix.kill h 15;
			let _ = Unix.waitpid [] h in
			xs.Xs.rm (dmhelper_path target_domid h);
			info "stopped dm helper pid=%d" h
		with _ ->
			warn "problem destroying dm helper pid=%d" h
			
	in
	List.iter stop hs
	
let create_dm_stubdom ~xc ~xs dmargs info target_domid uuid =
	let stubinfo = { Domain_common.stubdom_target = target_domid;
		         Domain_common.stubdom_memory = info.stubdom_memory;
			 Domain_common.stubdom_kernel = info.stubdom_kernel;
			 Domain_common.stubdom_initrd = info.stubdom_initrd;
			 Domain_common.stubdom_cmdline= info.stubdom_cmdline } in
	let rec filter f l = match l with [] -> [] | x::xs -> if f x then filter f xs else x :: filter f xs in
	let filtered_args = filter (fun x -> x = "xenfv" || x = "-M" || x = "-sdl") dmargs in
	let stubdom_domid = Domain_common.make_stubdom ~xc ~xs ~ioemuargs:filtered_args stubinfo uuid in

        debug "created stubdom as %d" stubdom_domid;

	let vmpath = xs.Xs.read (sprintf "%s/vm" (xs.Xs.getdomainpath target_domid)) in
	let dmargspath = vmpath ^ "/image/dmargs" in

	xs.Xs.write dmargspath (String.concat " " filtered_args);

        (* set permissions to allow stubdom to read power managemment xenstore node *)
        xs.Xs.setperms (power_mgmt_path target_domid) (0, Xsraw.PERM_NONE, [stubdom_domid, Xsraw.PERM_READ]);
	(* allow stubdom to read/write rtc offsets *)
	let timeoffset_path = vmpath ^ "/rtc/timeoffset" in
	xs.Xs.setperms timeoffset_path (0, Xsraw.PERM_NONE, 
					[
					 (target_domid , Xsraw.PERM_READ);
					 (stubdom_domid, Xsraw.PERM_RDWR)
					]);

	(* adding discs *)
	List.iter (fun disk ->
		let (_ : Device_common.device) = Device.Vbd.add_struct ~xc ~xs ~hvm:false disk stubdom_domid in
		()
	) info.disks;
	(* adding vifs *)
	List.iter (fun (backend_domid,nic) ->
	        let mac' = stubdom_mac nic.Device.Vif.mac in
		let nic' = { nic with Device.Vif.mac = mac' } in
		let (_ : Device_common.device) = Device.Vif.add_struct ~xc ~xs ~backend_domid nic' stubdom_domid in
		()
	) info.vifs;
	(* adding pcis *)
	List.iter (fun (devid, devs) ->
		(* stubdom PCIs are not actually assigned.
		 * Grant PCI devices config-space write-access to the stub-domain *)
		Device.PCI.add ~xc ~xs ~hvm:false ~assign:false ~pvpci:true ~permissive:true devs stubdom_domid devid
	) info.pcis;

	Device.Vfb.add ~xc ~xs ~hvm:false stubdom_domid;
	Device.Vkb.add ~xc ~xs ~hvm:false stubdom_domid 0;
	Device.Console.add ~xs ~hvm:false ~consback:Device.Console.Ioemu ~output:(sprintf "file:/var/log/xen/qemu-dm-%d" stubdom_domid) ~devid:0 stubdom_domid;

	(* v4v firewall routes need to be opened before unpausing stubdom, preferably also before forking the helpers. 
	 * Done by xenmgr. *)
        let firewall_ready = xs.Xs.getdomainpath target_domid ^ "/v4v-firewall-ready" in
	begin
	  try  ignore(Watch.wait_for ~xs ~timeout:20. (Watch.value_to_appear firewall_ready))
	  with Watch.Timeout _ ->
	    debug "qemu-dm: timeout waiting for %s" firewall_ready;
	    raise (Ioemu_failed ("Timeout waiting for " ^ firewall_ready))
	end;

	fork_stubdom_helpers ~xs uuid target_domid stubdom_domid;

	Domain_common.unpause ~xc stubdom_domid;
	stubdom_domid


(* Returns the allocated vnc port number *)
let __start ~xc ~xs ~dmpath ~restore ?(timeout=qemu_dm_ready_timeout) info domid =
	let (qemuargs, wait_for_port) = cmdlineargs ~xs ~restore info domid in
	let log = logfile domid in
	let l = [ string_of_int domid; (* absorbed by qemu-dm-wrapper *)
		  log;                 (* absorbed by qemu-dm-wrapper *)
		] @ qemuargs in
       let stubdom_domid =
		match info.stubdom with
		| None      -> debug "not using stubdomain"; None
		| Some uuid -> debug "using stubdomain"; Some (create_dm_stubdom ~xc ~xs qemuargs info domid uuid)
		in
       (* non-stubdom helpers *)
       fork_dm_helpers ~xs info.vsnd domid;

       (* only start qemu if we are not using stub domain *)
       if stubdom_domid = None then (
               (* Now add the close fds wrapper *)
               let cmdline = Forkhelpers.close_and_exec_cmdline [] dmpath l in
               debug "qemu-dm: executing commandline: %s" (String.concat " " cmdline);

               let argv_0 = List.hd cmdline and argv = Array.of_list cmdline in
               Unixext.double_fork (fun () ->
                       Sys.set_signal Sys.sigint Sys.Signal_ignore;
                       Unix.execvp argv_0 argv
               )
        );

	debug "qemu-dm: should be running in the background (stdout and stderr redirected to %s)" log;

	(* We know qemu is ready (and the domain may be unpaused) when
	   device-misc/dm-ready is set in the store. See xs-xen.pq.hg:hvm-late-unpause *)
        let dm_ready = xs.Xs.getdomainpath domid ^ "/device-misc/dm-ready" in
	begin
	  try
	    ignore(Watch.wait_for ~xs ~timeout (Watch.value_to_appear dm_ready))
	  with Watch.Timeout _ ->
	    debug "qemu-dm: timeout waiting for %s" dm_ready;
	    raise (Ioemu_failed ("Timeout waiting for " ^ dm_ready))
	end;

	(* If the wrapper script didn't write its pid to the store then fail *)
	if stubdom_domid = None then (
		let qemu_pid_path = xs.Xs.getdomainpath domid ^ "/qemu-pid" in
		let qemu_pid = ref 0 in
		begin
			try
				qemu_pid := int_of_string (xs.Xs.read qemu_pid_path);
			with _ ->
				debug "qemu-dm: Failed to read qemu pid from xenstore (normally written by qemu-dm-wrapper)";
				raise (Ioemu_failed "Failed to read qemu-dm pid from xenstore")
		end;
		debug "qemu-dm: pid = %d" !qemu_pid;

		(* Verify that qemu still exists at this point (of course it might die anytime) *)
		let qemu_alive = try Unix.kill !qemu_pid 0; true with _ -> false in
		if not qemu_alive then
			raise (Ioemu_failed (Printf.sprintf "The qemu-dm process (pid %d) has vanished" !qemu_pid));
	);

	(* Block waiting for it to write the VNC port into the store *)
	if wait_for_port then (
		try
			let port = Watch.wait_for ~xs (Watch.value_to_appear (vnc_port_path domid)) in
			debug "qemu-dm: wrote vnc port %s into the store" port;
			(stubdom_domid, Some (int_of_string port))
		with Watch.Timeout _ ->
			warn "qemu-dm: Timed out waiting for qemu's VNC server to start";
			raise (Ioemu_failed (Printf.sprintf "The qemu-dm process failed to write a vnc port")) 
	) else
		(stubdom_domid, None)	
	
let start ~xc ~xs ~dmpath ?timeout info domid = __start ~xc ~xs ~restore:false ~dmpath ?timeout info domid
let restore ~xc ~xs ~dmpath ?timeout info domid = __start ~xc ~xs ~restore:true ~dmpath ?timeout info domid

(* suspend/resume is a done by sending signals to qemu *)
let suspend ~xs domid = signal ~xs ~domid "save" ~wait_for:"paused"
let resume ~xs domid = signal ~xs ~domid "continue" ~wait_for:"running"

(* Called by every domain destroy, even non-HVM *)
let stop ~xs domid signal =
	let qemu_pid_path = sprintf "/local/domain/%d/qemu-pid" domid in
	let qemu_pid =
		try int_of_string (xs.Xs.read qemu_pid_path)
		with _ -> 0 in
	if qemu_pid = 0
	then debug "No qemu-dm pid in xenstore; assuming this domain was PV"
	else begin
		debug "qemu-dm: stopping qemu-dm with %s (domid = %d)"
		  (if signal = Sys.sigterm then "SIGTERM" 
		   else if signal = Sys.sigusr1 then "SIGUSR1"
		   else "(unknown)") domid;

		let proc_entry_exists pid =
			try Unix.access (sprintf "/proc/%d" pid) [ Unix.F_OK ]; true
			with _ -> false
			in
		if proc_entry_exists qemu_pid then (
			let loop_time_waiting = 0.03 in
			let left = ref qemu_dm_shutdown_timeout in
			let readcmdline pid =
				try Unixext.read_whole_file_to_string (sprintf "/proc/%d/cmdline" pid)
				with _ -> ""
				in
			let reference = readcmdline qemu_pid and quit = ref false in
			debug "qemu-dm: process is alive so sending signal now (domid %d pid %d)" domid qemu_pid;
			Unix.kill qemu_pid signal;

			(* We cannot do a waitpid here, since we're not parent of
			   the ioemu process, so instead we are waiting for the /proc/%d to go
			   away. Also we verify that the cmdline stay the same if it's still here
			   to prevent the very very unlikely event that the pid get reused before
			   we notice it's gone *)
			while proc_entry_exists qemu_pid && not !quit && !left > 0.
			do
				let cmdline = readcmdline qemu_pid in
				if cmdline = reference then (
					(* still up, let's sleep a bit *)
					ignore (Unix.select [] [] [] loop_time_waiting);
					left := !left -. loop_time_waiting
				) else (
					(* not the same, it's gone ! *)
					quit := true
				)
			done;
			if !left <= 0. then begin
				debug  "qemu-dm: failed to go away %f seconds after receiving signal (domid %d pid %d)" qemu_dm_shutdown_timeout domid qemu_pid;
				raise Ioemu_failed_dying
			end;
			(try xs.Xs.rm qemu_pid_path with _ -> ());
			(* best effort to delete the qemu chroot dir; we deliberately want this to fail if the dir is not empty cos it may contain
			   core files that bugtool will pick up; the xapi init script cleans out this directory with "rm -rf" on boot *)
			(try Unix.rmdir ("/var/xen/qemu/"^(string_of_int qemu_pid)) with _ -> ())
		);
		(* Even if it's already dead (especially if it's already dead!) inspect the logfile *)
		begin try write_logfile_to_log domid
		with _ ->
			debug "qemu-dm: error reading stdout/stderr logfile (domid %d pid %d)" domid qemu_pid;
		end;
		begin try unlink_logfile domid
		with _ ->
			debug "qemu-dm: error unlinking stdout/stderr logfile (domid %d pid %d), already gone?" domid qemu_pid
		end
	end;
	stop_helpers ~xs domid;
	(try xs.Xs.rm (device_model_path domid) with _ -> ())

