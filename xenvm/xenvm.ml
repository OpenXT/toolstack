(*
 * Copyright (C) 2006-2007 XenSource Ltd.
 * Copyright (C) 2008-2009 Citrix Ltd.
 * Author Vincent Hanquez <vincent.hanquez@eu.citrix.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

(*
 * Copyright (c) 2012 Citrix Systems, Inc.
 *)


open Printf
open Pervasiveext
open Stringext
open Threadext
open Vmconfig
open Vmstate

module D=Debug.Debugger(struct let name="xenvm" end)
open D

let thread_create f x =
	let (_: Thread.t) = Thread.create f x in
	()

let notify_quit state =
	Mutex.lock state.quit_mutex;
	state.quit_requested <- true;
	Condition.signal state.quit_condition;
	Mutex.unlock state.quit_mutex;
	()

let string_of_exn exn =
	match exn with
	| Hotplug.Device_timeout dev ->
		let frontend = dev.Device_common.frontend
		and backend = dev.Device_common.backend in
		sprintf "error: device timed out: backend %s - frontend %s"
		         (Device_common.string_of_endpoint backend)
		         (Device_common.string_of_endpoint frontend);
	| Device.PV_Vnc.Failed_to_start ->
		sprintf "vncterm failed to start";
	| Device.PCI.Cannot_add (devs, exn) ->
		let devs_str = String.concat "," (List.map (fun dev ->
			let desc = dev.Device.PCI.desc in
			sprintf "%d:%d:%d.%d" desc.Device.PCI.domain desc.Device.PCI.bus desc.Device.PCI.slot desc.Device.PCI.func
		) devs) in
		let exn_str = match exn with
			| Unix.Unix_error (err, act, d) ->
				sprintf "unix error doing %s: %s" act (Unix.error_message err)
			| _                             ->
				Printexc.to_string exn
			in
		sprintf "cannot add pci devices [%s]: %s" devs_str exn_str
	| Forkhelpers.Spawn_internal_error (log, output, process) ->
		let string_of_unix_process process =
			match process with
			| Unix.WEXITED i -> sprintf "exited(%d)" i
			| Unix.WSIGNALED i -> sprintf "signaled(%d)" i
			| Unix.WSTOPPED i -> sprintf "stopped(%d)" i
			in
		sprintf "forkhelpers received an error: output=%S process=%s"
		        output (string_of_unix_process process)
	| Device.Tap2.Mount_failure (ty, path, error) ->
		sprintf "mount failed for %s:%S: %s" ty path error
	| Vmact.Not_enough_free_memory cfg ->
		sprintf "error: not enough memory: %Ld Mb requested, %Ld Mb available"
		        (Int64.div cfg.memory 1024L)
		        (Xc.pages_to_mib (Int64.of_nativeint (Xc.with_intf Xc.physinfo).Xc.free_pages));
	| Vmact.Vm_not_listening_shutdown_request req ->
		sprintf "error: shutdown request %s not acknowledged"
		        (Domain.string_of_shutdown_reason req)
	| Vmact.Vm_shutdown_wrong_reason (expected, actual) ->
		sprintf "error: shutdown for wrong reason: expected %s, got %s"
		        (Domain.string_of_shutdown_reason expected)
		        (Xal.string_of_died_reason actual)
	| Vmact.Vm_didnt_shutdown ->
		sprintf "error: shutdown didn't complete"
	| Vmact.Vm_bad_state (expected, actual) ->
		sprintf "operation cannot be performed: vm is %s, expecting %s"
		        (string_of_vmlifestate actual) (string_of_vmlifestate expected)
	| Vmact.Vm_is_already_created (domid, lifestate) ->
		sprintf "vm is apparenlty already created: domid %d, state %s"
		        domid (string_of_vmlifestate lifestate)
	| Tasks.Invalid_type_registered n ->
		sprintf "tasks argument using invalid type: %s" n
	| Tasks.Argument_not_found n ->
		sprintf "tasks argument not found: %s" n
	| _ -> Printexc.to_string exn

let bind_unix_socket filename listen_queue =
	let socket = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
	Unix.bind socket (Unix.ADDR_UNIX filename);
	Unix.listen socket listen_queue;
	socket

let open_monitor_socket uuid name =
	(* look if we have forced the path to something else *)
	let dir = try Sys.getenv "XENVM_SOCKET_DIR" with Not_found -> "/var/run/xenvm" in
	Unixext.mkdir_rec dir 0o640;
	let filename = sprintf "%s/vm-%s" dir uuid in
	bind_unix_socket filename 10, filename

let close_monitor_socket socket filename = Unix.close socket; Unixext.unlink_safe filename; ()

let with_datadir cfg file =
	if cfg.datadir <> "" && not (String.startswith "xenserver:" file) then (
		if cfg.datadir.[String.length cfg.datadir - 1] = '/'
		|| file.[0] = '/' then
			cfg.datadir ^ file
		else
			cfg.datadir ^ "/" ^ file
	) else
		file

let with_xc f = Xc.with_intf f

let with_xs f =
	let xs = Xs.daemon_open () in
	finally (fun () -> f xs) (fun () -> Xs.close xs)

let with_xcs f =
	with_xc (fun xc -> with_xs (fun xs -> f xc xs))

let check_vm_uuid uuid =
	with_xc (fun xc ->
		let domid = Vmact.domid_of_uuid xc (Uuid.uuid_of_string uuid) in
		match domid with
		| Some domid ->
			info "domain still present at domid=%d. exiting" domid;
			exit 2
		| None -> ()
	)

let check_vm uuid =
	let is_running = Xenvmlib.check ~timeout:10. uuid in
	if is_running then (
		info "VM is already handled by another xenvm";
		exit 1;
	);
	let path = Xenvmlib.Socket.path_of_socket uuid in
	if Sys.file_exists path then (
		info "stale socket left by previous xenvm. removing";
		Unixext.unlink_safe path
	);

	(* ignore exception in case we don't have a privcmd.
         * in this case a domain isn't running anyway *)
	ignore_exn (fun () -> check_vm_uuid uuid)

(* cmd parsing is the legacy text interface that need to be replaced by a more
   json'ish interface. this does for now the parsing from string to (task + command) *)
let do_cmd_parsing cmd =
	info "received cmd: \"%s\"" cmd;
	let ls = String.split ' ' cmd in
	match ls with
	| "help" :: [] -> Tasks.Help, []
	| "read-config" :: path :: [] -> Tasks.ReadConfig, [ "path", Tasks.ValString path ]
	| "read-config" :: _ -> Tasks.ReadConfig, []
	| "quit" :: _     -> Tasks.Quit, []
	| "destroy" :: _  -> Tasks.Destroy, []
	| "shutdown" :: [] | "halt" :: [] -> Tasks.Halt, [ "forced", Tasks.ValBool false ]
	| "shutdown" :: "force" :: [] | "halt" :: "force" :: [] -> Tasks.Halt, [ "forced", Tasks.ValBool true ]
	| "reboot" :: _ | "restart" ::  _ -> Tasks.Reboot, [ "forced", Tasks.ValBool false ]
	| "start" :: _    -> Tasks.Start, []
	| "pause" :: _    -> Tasks.Pause, []
	| "unpause" :: _  -> Tasks.Unpause, []
	| "suspend" :: "live" :: file :: [] -> Tasks.Suspend, [ "file", Tasks.ValString file; "live", Tasks.ValBool true ]
	| "suspend" :: file :: [] -> Tasks.Suspend, [ "file", Tasks.ValString file; "live", Tasks.ValBool false ]
	| "restore-no-del" :: file :: [] -> Tasks.Restore, [ "file", Tasks.ValString file; "delete", Tasks.ValBool false ]
	| "restore" :: file :: [] -> Tasks.Restore, [ "file", Tasks.ValString file; "delete", Tasks.ValBool true ]
	| "checkpoint" :: file :: [] -> Tasks.Checkpoint, [ "file", Tasks.ValString file; ]
	| "domid" :: _ | "get-domid" :: _ -> Tasks.GetDomid, []
	| "get-stubdomid" :: _ -> Tasks.GetStubDomid, []
	| "status" :: _ | "get-status" :: _ -> Tasks.GetStatus, []
	| "vnc" :: [] | "get-vnc" :: [] -> Tasks.GetVNC, []
	| "trigger" :: args -> Tasks.Trigger, [ "params", Tasks.ValListString args ]
	| "device" :: []   -> failwith "device command args missing"
	| "device" :: ty :: [] -> failwith (sprintf "device type %s: subcommand missing" ty)
	| "device" :: ty :: subcmd :: args -> Tasks.Device, [ "type", Tasks.ValString ty; "cmd", Tasks.ValString subcmd; "extra", Tasks.ValListString args ]
	| _ ->
		failwith (sprintf "unknown command: %s" cmd)

let do_help () =
	let string_of_argreq argreq =
		match argreq with Tasks.R -> "R" | Tasks.O -> "O" | Tasks.D _ -> "D"
		in
	let string_of_argtype argty =
		match argty with
		| Tasks.ArgBool -> "bool"
		| Tasks.ArgInt  -> "int"
		| Tasks.ArgString -> "string"
		| Tasks.ArgListString -> "list-string"
		in
	let b = Buffer.create 1024 in
	Buffer.add_string b "  command name |      params                                            | description";
	List.iter (fun (_, t) ->
		let params = List.map (fun (name, arity, ty) ->
			sprintf "%s=%s %s" name (string_of_argreq arity) (string_of_argtype ty)
		) t.Tasks.args in
		let params = String.concat "," params in
		let s = sprintf "%.16s | %.60s | %30s\n" t.Tasks.name params t.Tasks.description in
		Buffer.add_string b s
	) Tasks.actions_table;
	Xenvmlib.Msg (Buffer.contents b)

(****************************************************************************************
 *)
type connection = {
	con_fd: Unix.file_descr;
	con_buf: Buffer.t;
	mutable con_tasks: (Tasks.action * (Tasks.argval list)) Queue.t;
	mutable con_replies: Xenvmlib.answer Queue.t;
	mutable con_closing: bool;
	mutable con_header: int * int;
	con_legacy: bool;
}

let con_new fd legacy =
	{
		con_fd = fd;
		con_buf = Buffer.create 128;
		con_tasks = Queue.create ();
		con_replies = Queue.create ();
		con_closing = false;
		con_header = (4, 0);
		con_legacy = legacy;
	}
let con_close con = Unix.close con.con_fd
let con_get_fd con = con.con_fd

let con_has_tasks con = not (Queue.is_empty con.con_tasks)
let con_has_reply con = not (Queue.is_empty con.con_replies)
let con_has_work con = (con_has_tasks con || con_has_reply con)

let con_add_task con t = Queue.add t con.con_tasks
let con_pop_task con = Queue.pop con.con_tasks

let con_add_reply con r = Queue.add r con.con_replies
let con_pop_reply con r = Queue.pop con.con_replies

(****************************************************************************************
 *)
let reread_config state path =
	let path =
		match path with
		| None -> state.vm_config_path
		| Some path -> path
		in
	let cfg = Config.of_file state.vm_uuid (fun errors -> ()) path in
	state.vm_next_cfg <- Some cfg

(* Monitor acpi state and notify when it changes from S3 to S0.
 * Other cases are already signalled from qemu *)
let monitor_acpi state =
	let rec poll acpi_state =
		let s' = match state.vm_lifestate with
		| VmShutdown -> 5
		| _ ->
			  with_xc (fun xc ->
				  let s' = try Xc.domain_get_acpi_s_state xc state.vm_domid with _ -> 5 in
				  match (acpi_state,s') with
				  | (3,0) ->
					    state.vm_power_state <- 0;
					    Vmact.notify state Xenvmlib.code_vmtrigger [ "power-state"; "0" ];
					    s'
				  | _ ->    s'
			  )
		in
	        Thread.delay 1.;
	        poll s'
	in
	if state.vm_cfg.hvm then (
		poll 0
	)
	
let monitor_vm state =
	let xs = Xs.daemon_open () in
	let callback_introduce ctx id =
		let xc = Xal.xc_of_ctx ctx in
		try
			if Uuid.to_string (Domain.get_uuid ~xc id) = state.vm_uuid then
				state.vm_domid <- id
		with Xc.Error _ -> ()
	and callback_release ctx id =
		if state.vm_domid = id && not state.vm_expect_shutdown then (
			let xc = Xal.xc_of_ctx ctx in
			let reason = Xal.domain_get_dead ctx id in
			info "domain died asynchronously: %s" (Xal.string_of_died_reason reason);
			let action = match reason with
			| Xal.Crashed   ->
				Vmact.notify state Xenvmlib.code_error [ "error"; "crashed" ];
				state.vm_cfg.on_crash
			| Xal.Vanished  -> ActionPreserve
			| Xal.Halted    -> state.vm_cfg.on_halt
			| Xal.Rebooted  ->
			        Vmact.stop_vm xc xs state;
				Vmact.change_vmstate state VmRebooted;
                                state.vm_cfg.on_restart
			| Xal.Suspended -> state.vm_on_suspend_action
			| Xal.Shutdown i -> state.vm_cfg.on_halt in

			begin match action with
			| ActionSuspend ->
				(* Vmact.stop_vm xc xs state; *)
				()
			| ActionResume ->
				let cooperative = not state.vm_cfg.hvm in
				Domain.resume ~xc ~xs ~cooperative id;
				Vmact.change_vmstate state VmRunning;
			| ActionDestroy  ->
				Vmact.stop_vm xc xs state;
				Vmact.change_vmstate state VmShutdown;
			| ActionRestart  ->
				Vmact.stop_vm xc xs state;
			        (* reread the configuration file before start to support graphics passthru
                                 * setting changes between reboots *)
			        reread_config state None; 
				Vmact.start_vm xc xs state false;
			| ActionPreserve ->
				()
			end;
		)
	and callback_devices ctx id dev_event =
		let do_hotplugchange_vif nic device =
			let online = Hotplug.device_is_online ~xs device in
			let connected = Hotplug.device_is_connected ~xs device in
			if online && not connected  then (
				let netty = nic.nic_netty in
				let mac = nic.nic_mac in
				let protocol = Vmact.devproto_of_state state in
				let (_: Device_common.device) = Device.Vif.plug ~xs ~netty ~mac ~protocol device in
				()
			);
			if not online then (
				Device.Vif.release ~xs device;
				Device.Generic.rm_device_state ~xs device;
			);
			in
		let do_devshutdowndone disk device =
			let online = Hotplug.device_is_online ~xs device in
			if not online && false then (
				Device.Vbd.release ~xs device;
				Device.Generic.rm_device_state ~xs device;
			)
			in
		let change_rtc uuid data =
			if uuid = state.vm_uuid then
				Vmact.notify state Xenvmlib.code_vmset [ "rtc"; data ];
			in
		let extra_notification uuid node value =
			(* either we are because it's a domain extra event in this case we
			   have a null uuid or in vm event we're suppose to get the vm's uuid *)
			if uuid = "" || uuid = state.vm_uuid then (
				(match (node, value) with
					| ("power-state", Some ps) ->
						(try state.vm_power_state <- int_of_string ps;
						     debug "xal power-state=%s" ps;
						     Vmact.notify state Xenvmlib.code_vmtrigger [node; ps]
						 with _ -> ())
					| ("power-state", None) -> ()
					| _ ->
						let data = node :: match value with None -> [] | Some d -> [ d ] in
						Vmact.notify state Xenvmlib.code_vmtrigger data)
			)
			in
		if state.vm_domid = id then (
		debug "xal device callback device (domid = %d, state domid = %d) event: %s" id state.vm_domid (Xal.string_of_dev_event dev_event);
		match dev_event with
		| Xal.HotplugChanged (true, "vif", devid, oldextra, newextra) ->
			let devid = int_of_string devid in
			let nic =
				try Some (List.find (fun nic -> nic.nic_id = devid) state.vm_cfg.nics)
				with Not_found -> None in
			let device = may (fun nic ->
				let backend = {
					Device_common.domid = nic.nic_backend_dom;
					kind = Device_common.Vif;
					devid = devid
				} in
				Device_common.device_of_backend backend state.vm_domid
			) nic in
			maybe (fun nic -> maybe (fun device ->
					do_hotplugchange_vif nic device
				) device
			) nic;
		| Xal.DevShutdownDone (ty, devid) ->
			let devid = int_of_string devid in
			let disk =
				try
					Some (List.find (fun disk ->
						(Device.Vbd.device_number disk.disk_virtpath) = devid
					) state.vm_cfg.disks)
				with Not_found -> None in
			let device = may (fun disk ->
				let kind = match disk.disk_physty with
					| ExternalVDI -> failwith "external VDI not supported here"
					| Xenops s    -> Device.Vbd.kind_of_physty s in
				let backend = {
					Device_common.domid = 0;
					kind = kind;
					devid = devid
				} in
				Device_common.device_of_backend backend state.vm_domid
			) disk in
			maybe (fun disk -> maybe (fun device ->
					do_devshutdowndone disk device
				) device
			) disk;
		| Xal.ChangeRtc (uuid, data) -> change_rtc uuid data
		| Xal.Extra (uuid, node, value) -> extra_notification uuid node value
		| _ ->
			()
		(* some event come with a -1 domid since they have uuid embedded *)
		) else (
		match dev_event with
		| Xal.ChangeRtc (uuid, data) -> change_rtc uuid data
		| Xal.Extra (uuid, node, value) -> extra_notification uuid node value
		| _ -> ()
		)
		in
	while true
	do
		try Xal.loop ~callback_introduce ~callback_release ~callback_devices
		             ~extra_local_watches:state.vm_cfg.extra_local_watches
		             ~extra_vm_watches:state.vm_cfg.extra_vm_watches ()
		with exn ->
			warn "exception in xal: %s" (string_of_exn exn);
			Thread.delay 1.
	done

let do_task state (task, args) =
	let optional_arg default f args s =
		try f args s with Tasks.Argument_not_found _ -> default
		in
	let optional_arg_nodef f args s =
		try Some (f args s) with Tasks.Argument_not_found _ -> None
		in

	let task_desc = List.assoc task Tasks.actions_table in
	(*maybe assert_vmstate task_vmstate_required;*)
	match task with
	| Tasks.Quit ->
		notify_quit state; Xenvmlib.Ok
	| Tasks.Help ->
		do_help ();
	| Tasks.Destroy -> with_xcs (fun xc xs -> Vmact.stop_vm xc xs state; Vmact.change_vmstate state VmShutdown); Xenvmlib.Ok
	| Tasks.Halt ->
		let force = optional_arg false Tasks.args_get_bool args "forced" in
		with_xcs (fun xc xs ->
			Misc.with_xal (fun xal -> Vmact.shutdown_vm xc xs xal state force Domain.Halt);
			Vmact.stop_vm xc xs state;
			Vmact.change_vmstate state VmShutdown;
		);
		Xenvmlib.Ok
	| Tasks.Reboot ->
		let force = optional_arg false Tasks.args_get_bool args "forced"
		and auto_start = optional_arg false Tasks.args_get_bool args "auto-start" in
		with_xcs (fun xc xs ->
			Misc.with_xal (fun xal -> Vmact.shutdown_vm xc xs xal state force Domain.Reboot);
			Vmact.stop_vm xc xs state;
			if auto_start
			then Vmact.start_vm xc xs state false
			else Vmact.change_vmstate state VmRebooted
		);
		Xenvmlib.Ok
	| Tasks.Start ->
		let paused = optional_arg false Tasks.args_get_bool args "paused" in
		with_xcs (fun xc xs -> Vmact.start_vm xc xs state paused); Xenvmlib.Ok
	| Tasks.Pause -> with_xc (fun xc -> Vmact.pause_vm xc state); Xenvmlib.Ok
	| Tasks.Unpause -> with_xc (fun xc -> Vmact.unpause_vm xc state); Xenvmlib.Ok
	| Tasks.Suspend ->
		let file = Tasks.args_get_string args "file" in
		let live = optional_arg false Tasks.args_get_bool args "live" in
		state.vm_on_suspend_action <- ActionSuspend;
		with_xcs (fun xc xs ->
			Vmact.suspend xc xs state (if live then [ Domain.Live ] else [])
			                          (with_datadir state.vm_cfg file);
			Vmact.stop_vm xc xs state;
		        Vmact.change_vmstate state VmSuspended;
		);
		Xenvmlib.Ok
	| Tasks.Restore ->
		let file = Tasks.args_get_string args "file" in
		let paused = optional_arg false Tasks.args_get_bool args "paused" in
		let delete = optional_arg true Tasks.args_get_bool args "delete" in
		Vmact.change_vmstate state VmRestoring;
		with_xcs (fun xc xs -> Vmact.restore xc xs state delete paused (with_datadir state.vm_cfg file));
		Xenvmlib.Ok
	| Tasks.S3Suspend ->
		let timeout = optional_arg (Int64.of_int 30) Tasks.args_get_int args "timeout" in
		let timeout = Int64.to_int timeout in
		(match with_xcs (fun xc xs -> Vmact.s3_suspend xc xs state timeout) with
		| true  -> Xenvmlib.Ok
		| false -> Xenvmlib.Error "failed to put domain into s3")
	| Tasks.S4Suspend ->
		let timeout = optional_arg (Int64.of_int 30) Tasks.args_get_int args "timeout" in
		let timeout = Int64.to_int timeout in
		(match with_xcs (fun xc xs -> Vmact.s4_suspend xc xs state timeout) with
		| true  -> Xenvmlib.Ok
		| false -> Xenvmlib.Error "failed to put domain into s4")
	| Tasks.Checkpoint ->
		let file = Tasks.args_get_string args "file" in
		state.vm_on_suspend_action <- ActionResume;
		with_xcs (fun xc xs ->
		  Vmact.suspend xc xs state [] (with_datadir state.vm_cfg file);
		  Vmact.change_vmstate state VmSuspended;
		);
		Xenvmlib.Ok
	| Tasks.GetDomid -> Xenvmlib.Msg (string_of_int state.vm_domid)
	| Tasks.GetStubDomid -> Xenvmlib.Msg (string_of_int (default (-1) state.vm_stubdomid))
	| Tasks.GetStatus -> Xenvmlib.Msg (string_of_vmlifestate state.vm_lifestate)
	| Tasks.GetAcpiState ->
	        let acpi = with_xc (fun xc -> try Xc.domain_get_acpi_s_state xc state.vm_domid with _ -> 5) in
	        Xenvmlib.Msg (string_of_int acpi)
	| Tasks.GetVNC -> Xenvmlib.Msg (default "" (may string_of_int state.vm_vnc_port))
	| Tasks.AddDisk ->
		let path = Tasks.args_get_string args "path" in
		let device = Tasks.args_get_string args "device" in
		let ty = Tasks.args_get_string args "type" in
		let mode = Tasks.args_get_string args "mode" in
		let devtype = Tasks.args_get_string args "devtype" in
		Vmact.add_disk state path device ty mode devtype
	| Tasks.AddNic ->
		let bridge = optional_arg_nodef Tasks.args_get_string args "bridge" in
		let mac = optional_arg_nodef Tasks.args_get_string args "mac" in
		let model = optional_arg_nodef Tasks.args_get_string args "model" in
		Vmact.add_nic state bridge mac model
	| Tasks.AddPCI ->
		let id = Tasks.args_get_int args "id" in
		let domain = Tasks.args_get_int args "domain" in
		let bus = Tasks.args_get_int args "bus" in
		let slot = Tasks.args_get_int args "slot" in
		let func = Tasks.args_get_int args "func" in
		let bind = optional_arg true Tasks.args_get_bool args "bind" in
		let msitranslate = optional_arg_nodef Tasks.args_get_bool args "msitranslate" in
		let power_mgmt = optional_arg_nodef Tasks.args_get_bool args "power_mgmt" in
		Vmact.add_pci state id domain bus slot func bind msitranslate power_mgmt
	| Tasks.DelDisk | Tasks.DelNic | Tasks.DelPCI ->
		Xenvmlib.Error "not implemented"
	| Tasks.ListDisk ->
		Xenvmlib.Error "not implemented"
	| Tasks.ListNic ->
	        Vmact.list_nic state
	| Tasks.ListPCI ->
	        Vmact.list_pci state
	| Tasks.Get ->
		let field = Tasks.args_get_string args "field" in
		Vmact.get state field
	| Tasks.Set ->
		let field = Tasks.args_get_string args "field" in
		let value = Tasks.args_get_string args "value" in
		Vmact.set state field value
	| Tasks.Trigger ->
		let params = Tasks.args_get_liststring args "params" in
		with_xc (fun xc -> Vmact.do_trigger xc state params)
	| Tasks.SetNicBackendDom ->
		let id = Int64.to_int (Tasks.args_get_int args "id") in
		let domid = Int64.to_int (Tasks.args_get_int args "domid") in
		with_xcs (fun xc xs -> Vmact.set_nic_backend_dom xc xs state id domid)
	| Tasks.Device ->
		let ty = Tasks.args_get_string args "type" in
		let cmd = Tasks.args_get_string args "cmd" in
		let extra = Tasks.args_get_liststring args "extra" in
		with_xcs (fun xc xs -> Vmact.device_cmd xc xs state ty cmd extra)
        | Tasks.CDEject ->
	        let virtpath = Tasks.args_get_string args "virtpath" in
		with_xcs (fun xc xs -> Vmact.cd_eject xs state virtpath)
        | Tasks.CDInsertReal ->
                let virtpath = Tasks.args_get_string args "virtpath" in
                let physpath = Tasks.args_get_string args "physpath" in
                with_xcs (fun xc xs -> Vmact.cd_insert_real xc xs state virtpath physpath)
        | Tasks.CDInsertFile ->
                let virtpath = Tasks.args_get_string args "virtpath" in
                let physpath = Tasks.args_get_string args "physpath" in
                with_xcs (fun xc xs -> Vmact.cd_insert_file xc xs state virtpath physpath)
        | Tasks.NicNetworkSwitch ->
                let id = Tasks.args_get_int args "id" in
                let bridge = Tasks.args_get_string args "network" in
                with_xcs (fun xc xs -> Vmact.nic_network_switch xs state id bridge)
	| Tasks.SetMemTarget ->
		let mb    = Tasks.args_get_int args "mb" in
		let kib   = Int64.mul mb 1024L in
		let domid = state.vm_domid in
		if domid <> -1 then (
			with_xs (fun xs -> Balloon.set_memory_target xs domid kib);
			Xenvmlib.Ok
		) else (
			Xenvmlib.Error "no domain id"
		)
	| Tasks.ReadConfig ->
		let path =
			try Some (Tasks.args_get_string args "path")
			with _ -> None
			in
		reread_config state path;
		Xenvmlib.Ok

let monitor_rpc_json socket state =
	let assert_vmstate expected =
		if expected <> state.vm_lifestate then
			raise (Vmact.Vm_bad_state (expected, state.vm_lifestate));
		in
	let connections = ref [] in

	let reply_error con error =
		let s = Jsonrpc.response_to_string error in
		thread_create (fun () ->
			let len = String.length s in
			let w = Unix.write con.con_fd s 0 len in
			ignore w;
			con_close con;
		) ();
		in
	let do_rpc con rpc task taskdescr =
		let params = match rpc.Jsonrpc.params with
		| Json.Null -> []
		| Json.Object a ->
			Array.to_list (
				Array.map (fun (k, v) ->
					match v with Json.String s -> k, Tasks.ValString s | _ -> failwith "unexpected"
				) a
			)
		| _ -> []
			in
		thread_create (fun () ->
			let reply_id = match rpc.Jsonrpc.request_id with Some id -> id | None -> Json.Int 0L in
			let reply =
				try
					let r = do_task state (task, params) in
					match r with
					| Xenvmlib.Ok -> Jsonrpc.response_make_success reply_id Json.Null
					| Xenvmlib.Msg msg -> Jsonrpc.response_make_success reply_id (Json.String msg)
					| _ -> assert false
				with exn ->
					let err = string_of_exn exn in
					Jsonrpc.response_make_error reply_id (Jsonrpc.code_error_invalid_params) err None
				in
			let s = Jsonrpc.response_to_string reply in
			let len = String.length s in
			let w = Unix.write con.con_fd s 0 len in
			ignore w;
			con_close con;
		) ()
		in
	let connection_do_task con =
		let rpc =
			try Right (Jsonrpc.request_of_string (Buffer.contents con.con_buf))
			with exn -> Left (string_of_exn exn)
			in
		match rpc with
		| Right rpc -> (
			let t = try Some (Tasks.find rpc.Jsonrpc.method_name) with exn -> None in
			match t with
			| None -> 
				let error = Jsonrpc.response_make_error (Json.Int 0L) (Jsonrpc.code_error_invalid_params) "unknown RPC" None in
				reply_error con error
			| Some (act, task_descr) ->
				do_rpc con rpc act task_descr
			)
		| Left err  ->
			let error = Jsonrpc.response_make_error (Json.Int 0L) (Jsonrpc.code_error_invalid_params) err None in
			reply_error con error
		in
	(* process a jsonrpc type of connection *)
	let connection_process con =
		if con.con_header = (0, 0) then (
			let htonl_of_chars a b c d =
				((Char.code a) lsl 24) + ((Char.code b) lsl 16) +
				((Char.code c) lsl 8) + (Char.code d)
				in
			let s = Buffer.contents con.con_buf in
			Buffer.clear con.con_buf;
			let len = htonl_of_chars s.[0] s.[1] s.[2] s.[3] in
			con.con_header <- (0, len)
		);
		let (left, len) = con.con_header in
		if left > 0 then (
			let s = String.create left in
			let r = Unix.read con.con_fd s 0 left in
			if r = 0 then (
				con.con_closing <- true;
				con_close con
			) else (
				Buffer.add_substring con.con_buf s 0 r;
				con.con_header <- (left - r, 0)
			)
		) else (
			let clen = Buffer.length con.con_buf in
			let to_read = min (len - clen) 4096 in
			let s = String.create to_read in
			let r = Unix.read con.con_fd s 0 to_read in
			if r = 0 then (
				con.con_closing <- true;
				con_close con
			) else (
				Buffer.add_substring con.con_buf s 0 r;
				if Buffer.length con.con_buf = len then (
					con.con_closing <- true;
					connection_do_task con
				);
			)
		)
		in

	while not state.vm_monitors.monitor_json_quit
	do
		let has_work = List.fold_left (fun acc con -> acc || con_has_work con) false !connections in
		let timeout = if has_work then 0. else 1. in
		let readset = socket :: (List.map con_get_fd !connections) in
		let r, _, _ =
			try Unix.select readset [] [] timeout
			with _ -> [], [], []
			in
		if List.mem socket r then (
			let (fd, _) = Unix.accept socket in
			connections := (con_new fd false) :: !connections;
		);
		List.iter (fun con ->
			let fd = con_get_fd con in
			if List.mem fd r then (
				if con.con_legacy then (
					con_close con;
					connections := List.filter (fun c -> c <> con) !connections;
				) else (
					connection_process con;
					if con.con_closing then
						connections := List.filter (fun c -> c <> con) !connections;
				)
			)
		) !connections;
	done;
	notify_quit state;
	if state.vm_domid <> (-1) then (
		with_xcs (fun xc xs ->
			Vmact.stop_vm xc xs state;
			Vmact.change_vmstate state VmShutdown
		)
	)

let introspect state msg =
	let header =
		"<!DOCTYPE node PUBLIC \"-//freedesktop//DTD D-BUS Object Introspection 1.0//EN\" " ^
		"\"http://www.freedesktop.org/standards/dbus/1.0/introspect.dtd\">\n"
	in
	let method_desc m =
		"<method name=\"" ^ m ^ "\">"
		^ "<arg name=\"params\" type=\"a{ss}\" direction=\"in\"/>"
		^ "<arg name=\"rval\" type=\"s\" direction=\"out\"/>"
		^ "</method>"
	in
	let methods =
		let get_m (task, desc) = String.replace "-" "_" desc.Tasks.name in
		List.map get_m Tasks.actions_table
	in
	let node = Printf.sprintf "org/xen/vm/%s" (String.replace "-" "_" state.vm_uuid) in
	let intf = Printf.sprintf "org.xen.vm.uuid_%s" (String.replace "-" "_" state.vm_uuid) in
	let body =
		header
		^ "<node name=\"" ^ node ^ "\">\n"
		^ "<interface name=\"" ^ intf ^ "\">\n"
		^ List.fold_left (fun acc m -> acc ^ (method_desc m) ^ "\n") "" methods
		^ "</interface>"
		^ "</node>"
	in
	let r = DBus.Message.new_method_return msg in
	DBus.Message.append r [DBus.String body];
	r

let monitor_rpc_dbus state =
	let use_session = state.vm_monitors.monitor_use_dbus_session in
	let intf = Printf.sprintf "org.xen.vm.uuid_%s" (String.replace "-" "_" state.vm_uuid) in
	let match_s = sprintf "type='method',interface='%s'" intf in
	let bus = DBus.Bus.get (if use_session then DBus.Bus.Session else DBus.Bus.System) in
	let reply = DBus.Bus.request_name bus intf [ DBus.Bus.DoNotQueue ] in
	(match reply with
	| DBus.Bus.PrimaryOwner -> ()
	| _                     -> failwith (Printf.sprintf "cannot grab dbus intf %s" intf)
	);
	(*DBus.Bus.add_match bus match_s false;*)
	(* listen to Network Manager notificatons *)
	DBus.Bus.add_match bus "type='signal',interface='org.freedesktop.NetworkManager.Device'" false;

	DBus.Connection.flush bus;

	let calltask msg msg_method params =
		let xenvmlib_to_dbus rep =
			match rep with
			| Xenvmlib.Ok        -> DBus.Message.new_method_return msg
			| Xenvmlib.Msg s     ->
				let rmsg = DBus.Message.new_method_return msg in
				DBus.Message.append rmsg [ DBus.String s ];
				rmsg
			| Xenvmlib.Error err -> DBus.Message.new_error msg DBus.ERR_FAILED err
			| _                  -> DBus.Message.new_error msg DBus.ERR_FAILED "?"
			in
		let do_task_exn state x =
			try xenvmlib_to_dbus (do_task state x)
			with exn ->
				let s = sprintf "exception: %s" (Printexc.to_string exn) in
				DBus.Message.new_error msg DBus.ERR_FAILED s
			in
		(* if the tasks need to be threaded like start,reboot,.. we returns
		  none to the caller and create a thread that is going to populate a queue
		  with the return message when ready *)
		let t = try Some (Tasks.find msg_method) with exn -> None in
		match t with
		| None -> Some (DBus.Message.new_error msg DBus.ERR_SERVICE_UNKNOWN "no rpc")
		| Some (act, task_descr) ->
			let taskargs = List.map (fun (k, v) -> (k, Tasks.ValString v)) params in
			if task_descr.Tasks.need_threading then (
				info "creating thread for handling %s" msg_method;
				thread_create (fun () ->
					let rep = do_task_exn state (act, taskargs) in
					Mutex.execute Vmact.outgoing_mutex (fun () -> Queue.push rep Vmact.outgoing);
				) ();
				None
			) else
				Some (do_task_exn state (act, taskargs))
		in

	let process_method_call msg =
		let params = DBus.Message.get msg in
		let msg_method = match DBus.Message.get_member msg with None -> "missingmethod" | Some m -> m in
		let msg_method = String.replace "_" "-" msg_method in
		let interface = match DBus.Message.get_interface msg with None -> "" | Some i -> i in
		match interface, msg_method, params with
		| "org.freedesktop.DBus.Introspectable", "Introspect", _ ->
			  Some (introspect state msg)
		| _, _, [ DBus.Array DBus.Dicts ((_, _), msg_params) ] ->
			let params = List.map (fun (k, v) ->
				match k, v with
				| DBus.String key, DBus.String value -> key, value
				| DBus.String key, DBus.Variant (DBus.String value) -> key, value
				| _ -> assert false (* replace by sensible error *)
			) msg_params in
			(try calltask msg msg_method params
			with exn ->
				warn "dbus_monitor received exception: %s\n" (Printexc.to_string exn);
				Some (DBus.Message.new_error msg DBus.ERR_FAILED "?")
			)
		| _, _, _ ->
			let err_msg = DBus.Message.new_error msg DBus.ERR_INVALID_ARGS
			              "expecting string method followed by dictionnary" in
			Some (err_msg)
		in
	let process_signal msg =
		()
	in
	let process_message msg =
		let ty = DBus.Message.get_type msg in
		match ty with
		| DBus.Message.Error ->
			let error_name = match DBus.Message.get_error_name msg with None -> (-1) | Some x -> (Obj.magic x) in
			info "processing error message %d" error_name;
			None
		| DBus.Message.Method_call ->
			process_method_call msg
		| DBus.Message.Signal ->
			process_signal msg;
			None
		| DBus.Message.Method_return ->
		        None
		| _ ->
			info "unknown dbus message %s" (DBus.Message.string_of_message_ty ty);
			let err_msg = DBus.Message.new_error msg DBus.ERR_INVALID_ARGS
			              "expecting string method followed by dictionnary" in
			Some (err_msg)
		in
	(* notify that dbus RPC is up and running *)
	Vmact.notify state Xenvmlib.code_ping [ "dbus-rpc-up" ];

	while not state.vm_monitors.monitor_dbus_quit do
		(* check for outgoing work *)
		let outmsgs = Mutex.execute Vmact.outgoing_mutex (fun () ->
			let m = List.rev (Queue.fold (fun acc x -> x :: acc) [] Vmact.outgoing) in
			Queue.clear Vmact.outgoing;
			m
		) in
		if outmsgs <> [] then (
			List.iter (fun out -> let (_: int32) = DBus.Connection.send bus out in ()) outmsgs;
			DBus.Connection.flush bus
		);
		let still_connected = DBus.Connection.read_write bus 200 in
		ignore still_connected; (* FIXME we should probably do something more sensible here *)
		let rec drain () =
			match DBus.Connection.pop_message bus with
			| None     -> ()
			| Some msg ->
				  let reply = process_message msg in
				  maybe (fun reply ->
					  Mutex.execute Vmact.outgoing_mutex (fun () ->
						  Queue.push reply Vmact.outgoing
					  ) 
		                  ) reply;
				  drain ()
		in
		drain ()
	done;
	notify_quit state;
	()

(*
 * Monitor is in charge of 2 things: listen to VM requests, and listen to user queries.
 * - vm requests are: spontaneous shutdown and reboot
 * - user queries are commands coming from the unix socket
 *)
let monitor socket state =
	thread_create monitor_vm state;
	thread_create monitor_acpi state;
	if state.vm_monitors.monitor_use_json then
		thread_create (monitor_rpc_json socket) state;
	if state.vm_monitors.monitor_use_dbus then
		thread_create monitor_rpc_dbus state;

	let read_config_noexn state path =
		try reread_config state path
		with exn ->
			info "receive exception reading config file ignoring: %s"
			     (string_of_exn exn)
		in

	Sys.set_signal Sys.sigint (Sys.Signal_handle (fun i -> notify_quit state));
	Sys.set_signal Sys.sighup (Sys.Signal_handle (fun i -> read_config_noexn state None));

	Mutex.lock state.quit_mutex;
	while not state.quit_requested do
		Condition.wait state.quit_condition state.quit_mutex;
	done;
	Mutex.unlock state.quit_mutex;
	Thread.delay 0.5; (* give time for others threads to finish what they are doing .. *)
	exit 0

let main state =
	let socket, name = open_monitor_socket state.vm_uuid state.vm_cfg.name in

	Vmact.notify state Xenvmlib.code_ping [ "ping" ];
	finally (fun () ->
		try
			(* start the domain *)
			(match state.vm_cfg.startup with
			| StartupShutdown -> ()
			| StartupPause | StartupStart ->
				with_xcs (fun xc xs -> Vmact.start_vm xc xs state false);
				if state.vm_cfg.verbose then (
					info "started domain: %s" state.vm_uuid;
				);
			| StartupRestore (file, del) ->
				Unixext.with_file (with_datadir state.vm_cfg file) [ Unix.O_RDONLY ] 0o640 (fun fd ->
					with_xcs (fun xc xs -> Vmact.restart_vm xc xs state false fd)
				);
				if del then (
					try Sys.remove file with _ -> ()
				);
				if state.vm_cfg.verbose then (
					info "resumed domain: %s" state.vm_uuid;
				);
			);
			monitor socket state;
		with exn ->
			let exnstr = string_of_exn exn in
			error "fatal exception: %s" exnstr;
			Vmact.notify state Xenvmlib.code_error [ "error"; exnstr ];
			raise exn
	) (fun () ->
		Vmact.notify state Xenvmlib.code_hup [ "hup" ];
		close_monitor_socket socket name
	)

let () =
	let anon = ref [] in
	let config = ref "" in
	let strict_config = ref false in
	let config_errors = ref [] in
	let uuid = ref "" in
	let daemonize = ref true in
	let monitor_dbus = ref true in
	let monitor_dbus_session = ref false in
	let monitor_json = ref false in

	let usage_msg = sprintf "usage: %s [--config <config>] [--uuid <uuid>] [<config>-deprecated]" Sys.argv.(0) in
	Arg.parse [
		("--config", Arg.Set_string config, "Set config file to be used");
		("--uuid", Arg.Set_string uuid, "Force uuid to be the one specified");
		("--strict-config", Arg.Set strict_config, "Error out if the config contains errors");
		("--no-daemonize", Arg.Clear daemonize, "leave the daemon in foreground");
		("--monitor-dbus", Arg.Bool (fun b -> monitor_dbus := b), "a monitor will listen on the dbus");
		("--monitor-json", Arg.Bool (fun b -> monitor_json := b), "a monitor will listen on a unix/json interface");
		("--monitor-dbus-session", Arg.Set monitor_dbus_session, "dbus monitor will listen on session bus, not system");
	] (fun s -> anon := s :: !anon) usage_msg;

	if !config = "" && List.length !anon > 0 then (
		config := List.hd !anon;
	);

	Random.self_init ();

	let error_report errs =
		let errors_str = List.map (fun (p,s) -> sprintf "config error: %s: %s" p s) errs in
		if !strict_config then (
			List.iter (fun s -> eprintf "%s\n" s) errors_str;
			exit 3
		) else
			config_errors := errors_str;
		in

	let uuid, state =
		match !uuid, !config with
		| "", "" ->
			eprintf "error: you need to specify a uuid or a config file\n";
			exit 3;
		| "", configfile ->
			let cfg = Config.of_file !uuid error_report configfile in
			let uuid =
				match cfg.__uuid with
				| None      -> eprintf "error: you need to specify a uuid in your config file\n"; exit 3;
				| Some uuid -> uuid
				in
			(* reparse the config for things that are dependents of the uuid *)
			let cfg = Config.of_file uuid error_report configfile in
			uuid, (state_init uuid configfile cfg)
		| uuid, "" ->
			uuid, (state_init uuid "" Config.empty)
		| uuid, configfile ->
			let cfg = Config.of_file uuid error_report configfile in
			uuid, (state_init uuid configfile cfg)
		in

	(*
	if uuid = "" && cfg.__uuid !config = "" then (
		eprintf "error: you need to specify a uuid or a config\n";
		exit 2
	);
	*)
	state.vm_monitors.monitor_use_dbus <- !monitor_dbus;
	state.vm_monitors.monitor_use_dbus_session <- !monitor_dbus_session;
	state.vm_monitors.monitor_use_json <- !monitor_json;

	(*let glob_debug = try Some (Sys.getenv "XENVM_DEBUG") with Not_found -> None in*)
	let glob_debug = Some "syslog:xenvm" in
	if state.vm_cfg.debug || glob_debug <> None then (
		let a =
			if state.vm_cfg.debug then (
				if state.vm_cfg.output = "" then
					sprintf "file:/tmp/xenvm-debug-%s" uuid
				else
					"file:/" ^ state.vm_cfg.output
			) else (
				match glob_debug with
				| None -> assert false
				| Some dbg -> dbg
			)
			in
		(* register a unique id if we are using syslog *)
		if String.startswith "syslog:" a then
			Debug.uid := Some uuid;

		Logs.set_default Log.Debug [ a ];
		Logs.set_default Log.Info [ a ];
		Logs.set_default Log.Warn [ a ];
		Logs.set_default Log.Error [ a ]
	);

	if !config_errors <> [] then (
		warn "config contains %d errors that have been ignored" (List.length !config_errors);
		List.iter (fun s -> warn "%s" s) !config_errors
	);

	check_vm uuid;
	if !daemonize then
		Unixext.daemonize ();
	main state
