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
 * Copyright (c) 2014 Citrix Systems, Inc.
 *)


open Printf
open Pervasiveext
open Stringext
open Vmconfig
open Vmstate
open Threadext

module D=Debug.Debugger(struct let name="vmact" end)
open D

exception Not_enough_free_memory of config
exception Vm_bad_state of vmlifestate * vmlifestate (* expected * actual *)
exception Vm_not_listening_shutdown_request of Domain.shutdown_reason
exception Vm_shutdown_wrong_reason of Domain.shutdown_reason * Xal.died_reason
exception Vm_didnt_shutdown
exception Vm_is_already_created of int * vmlifestate

let externalVDImap = Hashtbl.create 4

let mntdir_path uuid = "/var/lib/xenvm/mnt-" ^ uuid

let outgoing_mutex = Mutex.create ()
let (outgoing : DBus.message Queue.t) = Queue.create ()

let _notify state code l =
	let connect_and_send addr fd =
		finally (fun () ->
			Unix.connect fd addr;
			(* notification format is "<uuid>:<code>:<message>\n" *)
			let s = String.concat ":" (state.vm_uuid :: string_of_int code :: l) ^ "\n" in
			let (_: int) = Unix.write fd s 0 (String.length s) in
			()
		) (fun () -> Unix.close fd)
		in
	match state.vm_cfg.notify with
	| NotifyUnix path ->
		let fd = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
		let addr = Unix.ADDR_UNIX path in
		connect_and_send addr fd
	| NotifyTcp (host, port) ->
		let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
		let addr = Unix.ADDR_INET (host, port) in
		connect_and_send addr fd
	| NotifyTcp6 _ ->
		()
	| NotifyDBus path ->
		(* path/intf/method arbitrary for now. *)
		let dbus_path = sprintf "/org/xen/vm/%s" (String.replace "-" "_" state.vm_uuid) in
		let dbus_intf = "xenvm.signal.notify" in
		let dbus_meth = "notify" in
		let msg = DBus.Message.new_signal dbus_path dbus_intf dbus_meth in
		DBus.Message.append msg [ (DBus.String state.vm_uuid);
					  (DBus.Int32 (Int32.of_int code));
					  (DBus.String (String.concat ":" l)) ];
		(* send has to be invoked on main dbus thread or segfaulting misery ensures *)
		Mutex.execute outgoing_mutex (fun () -> Queue.push msg outgoing);
		()
	| NotifyNone ->
		()

let notify state code l =
	info "sending notification %s" (String.concat ":" l);
	try
		_notify state code l
	with exn ->
		warn "notify failed: %s" (Printexc.to_string exn)


let devproto_of_state state =
	match state.vm_arch with
	| Domain.Arch_HVM | Domain.Arch_native -> Device_common.Protocol_Native
	| Domain.Arch_X32                      -> Device_common.Protocol_X86_32
	| Domain.Arch_X64                      -> Device_common.Protocol_X86_64

exception Cryptsetup_failure of string * string * string

exception Storage_daemon_timeout of string

let string_of_unix_process process =
	match process with
	| Unix.WEXITED i -> sprintf "exited(%d)" i
	| Unix.WSIGNALED i -> sprintf "signaled(%d)" i
	| Unix.WSTOPPED i -> sprintf "stopped(%d)" i

let cryptsetup_create name device cipher keysize keyfile =
	let opts = [
		"create"; name; device;
		"--cipher"; cipher;
		"--key-size"; string_of_int keysize;
		"--key-file"; keyfile
	] in
	let _ =
		try Forkhelpers.execute_command_get_output ~withpath:true "/usr/sbin/cryptsetup" opts
		with Forkhelpers.Spawn_internal_error (log, output, status) ->
			let s = sprintf "output=%S status=%s" output (string_of_unix_process status) in
			raise (Cryptsetup_failure ("create", name, s))
		in
	name

let cryptsetup_remove name =
	let opts = [ "remove"; name ] in
	let _ =
		try Forkhelpers.execute_command_get_output ~withpath:true "/usr/sbin/cryptsetup" opts
		with Forkhelpers.Spawn_internal_error (log, output, status) ->
			let s = sprintf "output=%S status=%s" output (string_of_unix_process status) in
			raise (Cryptsetup_failure ("remove", name, s))
		in
	()

let xenops_disk_of_disk cfg disk =
	let physty = match disk.disk_physty with
		| ExternalVDI -> Device.Vbd.Phys
		| Xenops s    -> s in
	let physpath = match disk.disk_physty with
		| ExternalVDI -> (
			try (Hashtbl.find externalVDImap disk.disk_physpath)
			with e -> (
				debug "lookup within external VDI map failed for path '%s'"
					disk.disk_physpath;
				raise e))
		| Xenops _    -> disk.disk_physpath in
	{ Device.Vbd.mode               = disk.disk_mode
	; Device.Vbd.virtpath           = disk.disk_virtpath
	; Device.Vbd.phystype           = physty
	; Device.Vbd.physpath           = physpath
	; Device.Vbd.dev_type           = disk.disk_devtype
	; Device.Vbd.unpluggable        = false
	; Device.Vbd.info_pt            = cfg.diskinfo_pt
	; Device.Vbd.extra_backend_keys = None
	}

let xenops_vif_of_nic cfg nic =
	{ Device.Vif.vifid = nic.nic_id
	; Device.Vif.netty = nic.nic_netty
	; Device.Vif.mac   = nic.nic_mac
	; Device.Vif.mtu   = None
	; Device.Vif.rate  = None
	}

let xenops_pci_of_pci cfg dev =
	{ Device.PCI.desc         =
			{ Device.PCI.domain = dev.pci_domain
			; Device.PCI.bus    = dev.pci_bus
			; Device.PCI.slot   = dev.pci_slot
			; Device.PCI.func   = dev.pci_func }
	; Device.PCI.guest_slot   = dev.pci_guest_slot
	; Device.PCI.msitranslate = default cfg.global_pci_msitranslate dev.pci_msitranslate
	; Device.PCI.power_mgmt   = default cfg.global_pci_power_mgmt dev.pci_power_mgmt
	; Device.PCI.vdevfn       = dev.pci_vdevfn }

let tapdev_of_tap2disk (disk,tap2dev,cryptdev) =
	default tap2dev (may (fun x -> "/dev/mapper/" ^ x) cryptdev)

let dbus_send_one ?(timeout=60.) debug msg =
        let start_time = Unix.gettimeofday () in
        let is_timeout () = Unix.gettimeofday () -. start_time > timeout in
	let bus = DBus.Bus.get DBus.Bus.System in
	let i = DBus.Connection.send bus msg in
	DBus.Connection.flush bus;
	let rec drain () =
                if is_timeout () then
                        raise (Storage_daemon_timeout debug);
		let _ = DBus.Connection.read_write bus 200 in
		match DBus.Connection.pop_message bus with
		| None     -> drain ()
		| Some rmsg ->
			let ri = DBus.Message.get_reply_serial rmsg in
			if ri = i then (
				let ty = DBus.Message.get_type rmsg in
				match ty with
				| DBus.Message.Error         ->
					(*
					(match DBus.Message.get_error_name rmsg with
					| None     -> ()
					| Some err -> debug "dbus call failed, returned error: %s" (DBus.string_of_error_name err)
					);
					*)
					None
				| DBus.Message.Method_return -> Some rmsg
				| _                          -> None
			) else
				drain ()
		in
	drain ()

let storage_get_blockdevice blockid =
	(* Storehouse and com.citrix.xenclient.storehouse no longer exist *)
	failwith "Storehouse VDI feature no longer available"

let prepare_disk ~xs state disk = match disk.disk_physty with
	| Xenops Device.Vbd.Vhd ->
		(* if keydir is specified, we don't use cryptsetup explicitly but assume
		 * blktap has built in encryption support *)
		let keydir = match disk.disk_crypt with
			| None    -> None
			| Some dc -> ( match dc.disk_crypt_key_dir with
				       | ""  -> None
				       | dir -> Some dir ) in
		let tap2dev = Device.Tap2.mount "vhd" (disk.disk_mode = Device.Vbd.ReadOnly) disk.disk_physpath keydir in
		let cryptdev =
			if keydir <> None
			then None
			else may (fun dc ->
					  let randomdev = Filename.basename tap2dev ^ ".crypt" in
					  cryptsetup_create randomdev tap2dev dc.disk_crypt_cipher
						  dc.disk_crypt_key_size
						  dc.disk_crypt_key_file) disk.disk_crypt
		in
		let tap2disk = (disk, tap2dev, cryptdev) in
		state.vm_tap2_disks <- tap2disk :: state.vm_tap2_disks;
		{ disk with disk_physty   = Xenops Device.Vbd.Phys;
			    disk_physpath = tapdev_of_tap2disk tap2disk }
	| ExternalVDI ->
		(* contact storage and get a block device *)
		let blkdev = storage_get_blockdevice disk.disk_physpath in
		{ disk with disk_physty   = Xenops Device.Vbd.Phys;
			    disk_physpath = blkdev }
	| _ -> disk

(* prepare disks will prepare vhds and external VDI to be available as block device, *)
let prepare_disks ~xs state disks =
	List.map (prepare_disk ~xs state) disks

let add_disk_to_vm ~xc ~xs state disk =
	debug "add_disk_to_vm physpath=%s" disk.disk_physpath;
	let use_stubdom = state.vm_cfg.stubdom <> None  in
	let protocol = devproto_of_state state in
	let physpath, phystype =
		let disk' = prepare_disk ~xs state disk in
		match disk'.disk_physty with
		| Xenops s -> disk'.disk_physpath, s
		| _        -> failwith "not a valid disk device for Vbd.add"   
		in
	let (_: Device_common.device) =
		Device.Vbd.add ~xc ~xs ~hvm:state.vm_cfg.hvm ~mode:disk.disk_mode ~virtpath:disk.disk_virtpath
		               ~phystype ~physpath ~dev_type:disk.disk_devtype
		               ~unpluggable:false ~diskinfo_pt:state.vm_cfg.diskinfo_pt
		               ~use_stubdom ~protocol ~backend_domid:disk.disk_backend_dom state.vm_domid in
	()

let cd_eject ~xs state virtpath =
        Device.Vbd.media_eject ~xs ~virtpath state.vm_domid;
	Xenvmlib.Ok

let cd_insert_as ~xc ~xs state ty virtpath physpath =
        match (ty, state.vm_stubdomid) with
	| Device.Vbd.File, Some stubid -> (
		(* the stubdom is way more complicated. yay for the fun ! *)
		match Device.Vbd.get_loop_device ~xs ~virtpath stubid with
		| None     ->
			error "cannot find an existing loop device for raw export to stubdom";
			Xenvmlib.Ok (* should be error *)
		| Some dev -> (
			debug "ejecting media in stubdom";
			Device.Vbd.media_eject ~xs ~virtpath stubid;
			let devid = Device.Vbd.device_number virtpath in
			let olddev = {
				Device_common.frontend = { Device_common.domid = stubid; Device_common.kind = Device_common.Vbd; Device_common.devid = devid  };
				Device_common.backend = { Device_common.domid = 0; Device_common.kind = Device_common.Vbd; Device_common.devid = devid }
			} in
			debug "shutting down media backend for stubdom";
			Device.clean_shutdown ~xs olddev;
			let diskinfo = {
				Device.Vbd.mode = Device.Vbd.ReadOnly;
				Device.Vbd.virtpath = virtpath;
				Device.Vbd.phystype = ty;
				Device.Vbd.physpath = physpath;
				Device.Vbd.dev_type = Device.Vbd.CDROM;
				Device.Vbd.unpluggable = true;
				Device.Vbd.info_pt = false;
				Device.Vbd.extra_backend_keys = None;
			} in
			debug "adding new media backend for stubdom";
			ignore (Device.Vbd.add_struct ~xc ~xs ~hvm:false diskinfo stubid);
			debug "inserting media";
			Device.Vbd.media_insert ~xs ~virtpath ~physpath ~phystype:ty stubid;
			Xenvmlib.Ok
			)
		)
	| _, Some _ ->
		error "cannot insert non raw file to stubdom";
		Xenvmlib.Ok
	| _, None ->
		Device.Vbd.media_insert ~xs ~virtpath ~physpath ~phystype:ty state.vm_domid;
		Xenvmlib.Ok

let cd_insert_file ~xc ~xs state virtpath physpath =
        cd_insert_as ~xc ~xs state Device.Vbd.File virtpath physpath

let cd_insert_real ~xc ~xs state virtpath physpath =
        cd_insert_as ~xc ~xs state Device.Vbd.Phys virtpath physpath

let add_pci_to_vm ~xs state pci =
        state.vm_pcis <- pci :: state.vm_pcis

let add_nic_to_vm ~xc ~xs state nic append_state =
	let netty = nic.nic_netty in
	let (_: Device_common.device) =
		let add = if nic.nic_wireless then Device.Vwif.add else Device.Vif.add in
		add ~xc ~xs ~devid:nic.nic_id ~netty ~mac:nic.nic_mac
			~protocol:(devproto_of_state state) ~backend_domid:nic.nic_backend_dom state.vm_domid in
	if append_state then (
		state.vm_nics <- {ns_id=nic.nic_id; ns_netty=nic.nic_netty}::state.vm_nics
	)

let set_nic_backend_dom ~xc ~xs state nic_id backend_dom =
	let cfg = state.vm_cfg in
	let set_dom nic =
		if nic.nic_id = nic_id then (
			let nic'  = { nic with nic_backend_dom = backend_dom } in
			let swap oldnic = if (oldnic.nic_id == nic_id) then nic' else oldnic in
			let nics' = List.map swap state.vm_cfg.nics in
			state.vm_cfg <- { state.vm_cfg with nics = nics' };
			add_nic_to_vm xc xs state nic' false;
		)
	in
	List.iter set_dom cfg.nics;
	Xenvmlib.Ok
		
let get_nics cfg =
	(* then sort the nics by ascending order *)
	List.sort (fun nic1 nic2 -> if nic1.nic_id > nic2.nic_id then 1 else -1) cfg.nics

let device_of_vif domid id =
        let backend = { Device_common.domid = 0; 
                  kind = Device_common.Vif;
                  devid = id } in
        Device_common.device_of_backend backend domid

let nic_network_switch ~xs state id new_network_path =
        let id = Int64.to_int id in
        let ns = List.find (fun ns -> ns.ns_id = id) state.vm_nics in
        let cur_netty = ns.ns_netty in
        let new_netty = match cur_netty with
		| Netman.DriverDomain (_, bridge) -> Netman.DriverDomain (new_network_path, bridge)
		| Netman.Bridge (_, bridge) -> Netman.Bridge (new_network_path, bridge)
		| _ -> Netman.Bridge (new_network_path, "") in
        let device = device_of_vif state.vm_domid id in
        let backend_dev = Device.Vif.get_backend_dev xs device in
	let backend_path = Device_common.backend_path_of_device xs device in
        Netman.offline backend_dev cur_netty;
        Netman.online ~xs backend_path backend_dev new_netty;
	(* also move the qemu emulated devices *)
	let emu_dev = Printf.sprintf "tap%d.%d" state.vm_domid id in
	if List.mem emu_dev (Netdev.list())
	  then (
	    Netman.offline emu_dev cur_netty;  
	    Netman.online ~xs backend_path emu_dev new_netty;
	  );
        state.vm_nics <- List.map (fun ns -> if ns.ns_id=id then {ns with ns_netty=new_netty} else ns) state.vm_nics;
        Xenvmlib.Ok

let get_pcis cfg =
	let ids = ref [] in
	List.iter (fun (id, dev) ->
		if not (List.mem id !ids) then
			ids := id :: !ids
	) cfg.pcis;
	List.map (fun id ->
		let ds = List.map (fun (_, dev) -> dev)
				  (List.filter (fun (x, dev) -> x = id) cfg.pcis) in
		id, ds
	) !ids

let network_of_netty =
	function | Netman.Bridge (name,_) -> name
  		 | Netman.DriverDomain (name,_) -> name
		 | _                      -> ""

let bridge_of_netty =
	function | Netman.Bridge (_,name) -> name
		 | Netman.DriverDomain (_,name) -> name
		 | _                      -> ""

let dm_videoram_mib_of_cfg cfg =
	match cfg.videoram with None -> 0 | Some i -> i

let dm_info_of_cfg cfg =
	let stubdom =
		match cfg.stubdom with
		| None      -> None
		| Some uuid -> Some (Uuid.uuid_of_string uuid)
		in
	(* only nics which are in dom0 are given to ioemu - unless we are using qemu stubdoms *)
	let nics = List.filter (fun nic -> stubdom <> None || nic.nic_backend_dom = 0) (get_nics cfg) in
	let nic_description nic =
		(nic.nic_mac,
		 (network_of_netty nic.nic_netty, bridge_of_netty nic.nic_netty),
		 (if nic.nic_model="" then None else Some nic.nic_model),
		 nic.nic_wireless, nic.nic_id) in
	let nics = List.map nic_description nics in

	let extrahvm =
		(match cfg.cdrom_pt with
			| None      -> []
			| Some path -> [ ("cdrom-pt", Some path) ]
		) @ cfg.extrahvm
		in

	let disp =
		match cfg.display with
		| DisplayNone    -> Dm.NONE
		| DisplaySDL _   -> Dm.NONE
		| DisplayIntel _ -> Dm.NONE
		| DisplayVNC (unused, port, keymap) -> Dm.VNC (unused, "", port, keymap)
		in
	let mk_pci (devid,devs) = (devid, List.map (xenops_pci_of_pci cfg) devs) in
	{
		Dm.hvm = cfg.hvm;
		Dm.memory = cfg.memory;
		Dm.boot = cfg.boot;
		Dm.serial = if cfg.serial = "" then "pty" else (if cfg.serial = "none" then "" else cfg.serial);
		Dm.vcpus = cfg.vcpus;
		Dm.nics = nics;
		Dm.pci_emulations = [];
		Dm.sound = cfg.sound;
		Dm.vsnd = cfg.vsnd;
		Dm.usb = [];
		Dm.acpi = cfg.acpi;
		Dm.disp = disp;
		Dm.power_mgmt = cfg.power_management;
		Dm.oem_features = cfg.oem_features;
		Dm.inject_sci = cfg.inject_sci;
		Dm.videoram = dm_videoram_mib_of_cfg cfg;
		Dm.extras = extrahvm;
		Dm.disks = List.map (xenops_disk_of_disk cfg) cfg.disks;
		Dm.vifs = List.map (fun n -> (n.nic_backend_dom, xenops_vif_of_nic cfg n)) cfg.nics;
		Dm.pcis = List.map mk_pci (get_pcis cfg);
		Dm.stubdom = stubdom;
		Dm.stubdom_memory = cfg.stubdom_memory;
		Dm.stubdom_kernel = cfg.stubdom_kernel;
		Dm.stubdom_initrd = cfg.stubdom_initrd;
		Dm.stubdom_cmdline= cfg.stubdom_cmdline;
	}

(* TODO: maybe unhardcode this? *)
let snapshotkeydir_path = "/config/platform-crypto-keys"

let snapshot_keypath snapshot_path =
	let basename = Filename.basename snapshot_path in
	let n = String.sub basename 0 (String.index basename '.') in
	snapshotkeydir_path ^ "/" ^ n ^ ".snap.tmp,aes-xts-plain,256.key"

let vhd_snap_path physpath = physpath ^ ".snap"
let vhd_tmp_path physpath  = String.sub physpath 0 (String.rindex physpath '.') ^ ".snap.tmp.vhd"
let vhd_hib_path physpath = physpath ^ ".hib"

let close_snapshots disks hibernated =
	List.iter (fun disk ->
		(* snapshots managed with scripts are not removed here *)
		match disk.disk_snapshot_mode with
	        | Snapshot_scripted -> ()
		| Snapshot_scripted_author -> ()
		| Snapshot_scripted_no_snapshot -> ()
		| _ -> (
			match disk.disk_physty with
			| Xenops Device.Vbd.Vhd ->
				  let snappath = vhd_snap_path disk.disk_physpath in
				  let hibpath = vhd_hib_path disk.disk_physpath in
				  let tmppath = vhd_tmp_path disk.disk_physpath in
				  let tmpkeypath = snapshot_keypath tmppath in
				  info "rm snapshot %s %s %s" snappath tmppath tmpkeypath;
				  if Sys.file_exists snappath then (
					  debug "removing snapshot file %s" snappath;
					  Unixext.unlink_safe snappath);
				  if Sys.file_exists hibpath then (
					  debug "removing snapshot file %s" hibpath;
					  Unixext.unlink_safe hibpath);
				  if Sys.file_exists tmppath then (
					  if not hibernated then (
						  debug "removing snapshot file %s" tmppath;
						  Unixext.unlink_safe tmppath
					  ) else (
						  debug "preserving hibernate snapshot file %s %s" tmppath hibpath;
						  Unix.rename tmppath hibpath
					  )
				  );
				  if Sys.file_exists tmpkeypath then (
					  debug "removing shapshot key file %s" tmpkeypath;
					  Unixext.unlink_safe tmpkeypath);
			| _ -> ()
	        )
        ) disks

exception Snapshot_failure of string * string

(* Set when booting in author mode fails *)
let author_recovery_mode = ref false

let make_snapshots uuid disks =
	let scripted_snapshot opts =
		(try 
			Forkhelpers.execute_command_get_output
			~withpath:true
			"/usr/sbin/prepare-system-snapshot"
			opts
		with
			Forkhelpers.Spawn_internal_error (log, output, status) ->
				info "prepare-system-snapshot stderr: %s" log;
				author_recovery_mode := true;
				let s = sprintf "output=%S status=%s" output (string_of_unix_process status) in
				raise (Snapshot_failure (uuid, s)))
	in
	List.map (fun disk ->
		let hibpath = vhd_hib_path disk.disk_physpath in
		match disk.disk_snapshot_mode with
		| NoSnapshot -> disk
		| Snapshot_scripted ->
			  (* snapshots managed by external script *)
			  let physpath = disk.disk_physpath in
			  let tmppath = vhd_tmp_path physpath in
			  let opts = [
				  "-i"; physpath;
				  "-s"; tmppath;
				  "-u"; uuid
				] in
			  let _ = scripted_snapshot opts in
			  { disk with disk_physpath = tmppath } (* boot from snapshot *)
		| Snapshot_scripted_author ->
			  (* snapshots managed by external script in author mode *)
			  let physpath = disk.disk_physpath in
			  let tmppath = vhd_tmp_path physpath in
			  let opts = [
				  "-i"; physpath;
				  "-s"; tmppath;
				  "-u"; uuid;
			          "--author-mode"
				] in
			  let _ = match !author_recovery_mode with
                                  | false -> scripted_snapshot opts 
				  | true  -> scripted_snapshot ("--recovery" :: opts)
			  in
                          author_recovery_mode := false;			  
			  disk (* boot from snapshot parent *)
		| Snapshot_scripted_no_snapshot ->
		          (* snapshots managed by external script in no-snapshot mode *)
			  let opts = [
				  "-i"; disk.disk_physpath;
				  "-u"; uuid;
			          "--no-snapshot"
				] in
                          let _ = scripted_snapshot opts in
			  disk (* boot from original disk *)
		| Snapshot_temporary ->
			  (match disk.disk_physty with
			  | Xenops Device.Vbd.Vhd ->
				    if Sys.file_exists hibpath then (
					    info "using hibernate snapshot %s" hibpath;
					    { disk with disk_physpath = hibpath }
				    ) else (
					    debug "creating temporary snapshot for %s" disk.disk_physpath;
					    let tmppath = vhd_tmp_path disk.disk_physpath in
					    if Sys.file_exists tmppath then
						    Unixext.unlink_safe tmppath;
					    Misc.vhd_create_snapshot disk.disk_physpath tmppath;
					    { disk with disk_physpath = tmppath }
				    )
			  | _ -> disk)
		| Snapshot_temporary_encrypted ->
			  (match disk.disk_physty with
			  | Xenops Device.Vbd.Vhd ->
				    debug "creating temporary encrypted snapshot for %s" disk.disk_physpath;
				    let tmppath = vhd_tmp_path disk.disk_physpath in
				    if Sys.file_exists tmppath then
					    Unixext.unlink_safe tmppath;
				    Misc.vhd_create_snapshot disk.disk_physpath tmppath;
				    let key = Misc.vhd_gen_random_key () in
				    let keyfile = snapshot_keypath tmppath in
				    Misc.vhd_write_key keyfile key;
				    Misc.vhd_set_key tmppath keyfile;
				    { disk with disk_physpath = tmppath }
			  | _ -> disk)
		| Snapshot_coalesce ->
			  (match disk.disk_physty with
			  | Xenops Device.Vbd.Vhd ->
				    debug "creating coalesce snapshot for %s" disk.disk_physpath;
				    let snappath = vhd_snap_path disk.disk_physpath in
				    let tmppath = vhd_tmp_path disk.disk_physpath in
				    begin match Sys.file_exists snappath,
				      Sys.file_exists tmppath with
				    | false, false | false, true ->
					      Misc.vhd_create_snapshot disk.disk_physpath snappath;
					      Misc.vhd_create_snapshot snappath tmppath;
				    | true, false ->
					      Misc.vhd_create_snapshot snappath tmppath;
				    | true, true ->
					      Misc.vhd_coalesce_snapshot tmppath snappath;
				    end;
				    { disk with disk_physpath = tmppath }
			  | _ -> disk)
		) disks

let add_devices xc xs domid state restore =
	let use_stubdom = state.vm_cfg.stubdom <> None  in
	let cfg = state.vm_cfg in
	let pcis = get_pcis cfg in
	let nics = get_nics cfg in

	(* create disk snapshots, mount vhds *)
	let snap_disks = prepare_disks ~xs state (make_snapshots state.vm_uuid cfg.disks) in
	let cfg = { cfg with disks = snap_disks } in
	(* add disks and nics *)
	(
		debug "add_devices: adding disks";
		List.iter (fun x -> add_disk_to_vm ~xc ~xs state x) snap_disks;

		debug "add_devices: adding nics";
		List.iter (fun nic -> add_nic_to_vm ~xc ~xs state nic true) nics;

		(* add vcpus *)
		debug "add_devices: adding vcpus";
		for i = 0 to cfg.vcpus - 1 do Device.Vcpu.add ~xs ~devid:i domid done;

		(* add pcis *)
		debug "add_devices: adding pcis";
		state.vm_pcis <- [];
		List.iter (fun (devid, devs) ->
			let bind = ref false in
			let devs = List.map (fun dev ->
				bind := !bind || dev.pci_bind;
				xenops_pci_of_pci cfg dev
                        ) devs in
			if !bind then
				Device.PCI.bind devs;
			Device.PCI.add ~xc ~xs ~hvm:cfg.hvm ~pvpci:(not use_stubdom) ~flr:true devs domid devid;
			(* store actual parameters this is launched with with inside vm state *)
			let pcis = List.map (fun dev -> {
				Vmconfig.pci_bind = !bind;
				Vmconfig.pci_domain = dev.Device.PCI.desc.Device.PCI.domain;
				Vmconfig.pci_bus = dev.Device.PCI.desc.Device.PCI.bus;
				Vmconfig.pci_slot = dev.Device.PCI.desc.Device.PCI.slot;
				Vmconfig.pci_func = dev.Device.PCI.desc.Device.PCI.func;
				Vmconfig.pci_msitranslate = Some dev.Device.PCI.msitranslate;
				Vmconfig.pci_power_mgmt = Some dev.Device.PCI.power_mgmt;
				Vmconfig.pci_guest_slot = dev.Device.PCI.guest_slot;
				Vmconfig.pci_vdevfn = dev.Device.PCI.vdevfn;
			      } ) devs in
			List.iter (add_pci_to_vm ~xs state) pcis
	        ) pcis;


		if cfg.hvm then (
			debug "add_devices: process PCI hole size";
			let mmio_size dev =
				let rec __mmio_size res = match res with
				| [] -> 0L
				| (base, limit, _)::l ->
                                        Int64.add (Int64.sub (Int64.add limit 1L) base) (__mmio_size l)
				in __mmio_size (Device.PCI.mmio dev)
			in
			let mmio_size_total cfg_pcis =
				let rec __mmio_size_total pcidevs = match pcidevs with
				| [] -> 0L
				| dev::l -> Int64.add (mmio_size dev) (__mmio_size_total l) in
				List.fold_left (fun acc (devid, cfg_devs) ->
					let pcidevs = List.map (fun dev ->
						xenops_pci_of_pci cfg dev
				        ) cfg_devs in
                                        Int64.add acc (__mmio_size_total pcidevs)
				) 0x2000000L cfg_pcis
				(* We assume 32MB (0x2000000B) are required for QEMU default configuration. *)
			in
			let pci_hole_size = mmio_size_total pcis in
			let rec pci_hole_base_adjust base size =
				(* HVMLOADER assumes the PCI hole cannot grow below the 2G limit. *)
				if (base <= 0x80000000L) then 0x80000000L
				(* Another hardcoded limit in HVMLOADER is 0xfc000000 for the PCI hole top limit.
				 * Above that are Xen shared pages, MSI regions...*)
				else if (Int64.sub 0xfc000000L base < size) then
					pci_hole_base_adjust (Int64.logand (Int64.shift_left base 1) 0xffffffffL) size
				else
					base
			in
			(* Write the Top of Low Usable Memory in XenStore for dm-agent. *)
			let path = sprintf "/local/domain/%d/memory/tolum" domid in
				debug "Required PCI hole size: 0x%LxB, base adjusted to %Lx"
					pci_hole_size (pci_hole_base_adjust 0xf0000000L pci_hole_size);
				xs.Xs.write path (Int64.to_string (pci_hole_base_adjust 0xf0000000L pci_hole_size))
		);

		debug "add_devices: adding IOs passthrough";
		List.iter (fun x ->
			Device.PCI.passthrough_io ~xc domid x true;
			state.vm_passthrough_ios <- x :: state.vm_passthrough_ios;
		) cfg.Vmconfig.passthrough_ios;

		debug "add_devices: adding MMIOs passthrough";
		List.iter (fun x ->
			Device.PCI.passthrough_mmio ~xc domid x true;
			state.vm_passthrough_mmios <- x :: state.vm_passthrough_mmios;
		) cfg.Vmconfig.passthrough_mmios;

		let dmpath = cfg.qemu_dm_path in
		let use_dm = dmpath <> "" in
                let protocol = devproto_of_state state in

                (* Add the PV keyboard and mouse devices *)
                if cfg.vkbd then (
                    Device.Vkb.add ~xc ~xs ~hvm:cfg.hvm ~protocol domid 0;
                    Device.Vkb.add ~xc ~xs ~hvm:cfg.hvm ~protocol domid 1;
                    Device.Vkb.dbus_vkbd domid "attach_vkbd"
                );

                (* Add the PV framebufer device *)
                if cfg.vfb then Device.Vfb.add ~xc ~xs ~hvm:cfg.hvm ~protocol domid;

		(* Add the PV audio device *)
		if cfg.vsnd then Device.Vsnd.add ~xc ~xs ~hvm:cfg.hvm domid;

		(* Add the V4V device *)
                if cfg.v4v then Device.V4V.add ~xc ~xs ~hvm:cfg.hvm domid;

		(* Add the VTPM device *)
		(match cfg.vtpm_instance with
		| None -> ()
		| Some instance ->
			debug "add_devices: adding vtpm device, backend=%d instance=%d" cfg.vtpm_backend instance;
			Device.Vtpm.add ~xc ~xs ~hvm:cfg.hvm domid ~instance
		);
                (* Only start dmagent for hvm guests *)
		if cfg.hvm then (
			(* add device model *)
			debug "add_devices: adding device model";
			if use_dm then (
				(* Enable by default dm-agent, if /config/dm-wrapper exits, disable it *)
				let is_dmagent = try ignore (Unix.stat "/config/dm-wrapper"); false
								 with _ -> true in
				let dmstart = if is_dmagent then Dmagent.start
							 else if restore then Dm.restore else Dm.start in
				let info = dm_info_of_cfg cfg in
				let (stubdom_domid, vnc_port) = dmstart ~xc ~xs ~dmpath ~timeout:(float_of_int cfg.qemu_dm_timeout) info domid in
				state.vm_vnc_port <- vnc_port;
				state.vm_stubdomid <- stubdom_domid
			);
		);

		(* tweak permissions for stubdom related things *)
		match state.vm_stubdomid with
		| None           -> ()
		| Some stubdomid ->
			let path = sprintf "/local/domain/%d/power-state" domid in
			let exists =
                            try ignore (xs.Xs.read path); true
                            with _ -> false
                        in
			if not exists then (
				xs.Xs.write path ""
			);
			xs.Xs.setperms path (domid, Xsraw.PERM_NONE, [stubdomid, Xsraw.PERM_RDWR])
	)

let set_cpuid xc domid cfg =
	(* do cpuid setting.
	 * we ignore errors since we might have a old xen which doesn't support cpuid *)
	try
		Domain.cpuid_apply ~xc ~hvm:cfg.hvm domid;
		if cfg.cpuid <> [] then (
			let r = Domain.cpuid_set ~xc ~hvm:cfg.hvm domid cfg.cpuid in
			ignore (r);
		)
	with exn ->
		warn "exception ignored during cpuid: %s" (Printexc.to_string exn)

let set_affinity xc domid cfg =
	try
		List.iter (fun (id, l) ->
			let cpumap = Array.make 64 false in
			(* make the bitmap *)
			List.iter (fun x ->
				if x >= 0 && x < 64 then
					cpumap.(x) <- true
			) l;
			(* and dump in into xen *)
			Domain.vcpu_affinity_set ~xc domid id cpumap
		) cfg.cpus_affinity;
	with exn ->
		warn "exception ignored during affinity setting: %s" (Printexc.to_string exn)

let set_cores_per_socket xc domid cfg =
	match cfg.cores_per_socket with
	| None -> ()
	| Some value ->
		  try (debug "setting cores per socket: %d" value;
		       Domain.set_cores_per_socket ~xc domid value)
		  with exn -> warn "exception ignored during cores-per-socket setting: %s" (Printexc.to_string exn)
		  
		  
let do_trigger xc xs state args =
	match args with
	| "s3resume" :: _ ->
		if state.vm_lifestate <> VmRunning then
			Xenvmlib.Error ("cannot do s3resume on a non-running guest")
		else if not state.vm_cfg.hvm then
			Xenvmlib.Error ("cannot do s3resume on a non-hvm guest")
		else (
			Domain.send_s3resume ~xc ~xs state.vm_domid;
			Xenvmlib.Ok
		)
	| _ ->
		Xenvmlib.Error (sprintf "unknown trigger: %s" (String.concat " " args))

let change_vmstate state newstate =
	state.vm_lifestate <- newstate;
	notify state Xenvmlib.code_vmstate [ "vm"; "state"; string_of_vmlifestate newstate ];
	if newstate = VmShutdown then (
		maybe (fun cfg -> state.vm_cfg <- cfg) state.vm_next_cfg
	)

let stop_vm xc xs state =
	if state.vm_domid = -1 then
		warn "not destroying domain: domid = -1"
	else (
		let domid = state.vm_domid in
		state.vm_domid <- (-1);

		info "destroying MMIOs passthrough";
		List.iter (fun x -> Device.PCI.passthrough_mmio ~xc domid x false) state.vm_passthrough_mmios;
		state.vm_passthrough_mmios <- [];

		info "destroying IOs passthrough";
		List.iter (fun x -> Device.PCI.passthrough_io ~xc domid x false) state.vm_passthrough_ios;
		state.vm_passthrough_ios <- [];

		info "destroying domain %d" domid;
		Domain.destroy ~xc ~xs domid;
	);

	info "closing snapshots";
	close_snapshots state.vm_cfg.disks (state.vm_power_state = 4);

	info "cleaning up tap2 devices";
	List.iter (fun (d, path, cryptpath) ->
		(try maybe (cryptsetup_remove) cryptpath;
		with exn -> warn "unmounting cryptsetup exception: %s" (Printexc.to_string exn));
		try Device.Tap2.unmount path
		with exn -> info "unmounting tap2 exception: %s" (Printexc.to_string exn)
	) state.vm_tap2_disks;
	state.vm_tap2_disks <- [];
	()

let shutdown_vm xc xs xal state force reason =
	let reason_str = Domain.string_of_shutdown_reason reason in
	let domid = state.vm_domid in

	info "requesting vm shutdown with reason=%s force=%b domid=%d"
	     reason_str force domid;

	state.vm_expect_shutdown <- true;
	finally (fun () ->
		(* send request and check that it's acked -- when possible|relevant *)
		(try
			let acked =
				if force then (
					Domain.hard_shutdown ~xc domid reason; true
				) else (
					Domain.shutdown_ack ~timeout:30. ~xc ~xs domid reason
				) in
			if not acked then (
				warn "vm didn't acked shutdown request %s" reason_str;
				raise (Vm_not_listening_shutdown_request reason);
			);
			if reason <> Domain.Suspend then change_vmstate state VmShutdowning
		with e ->
			(* eat shutdown errors if domain is already dead, likely due
			 * to another asynchronous shutdown happening *)
			if not (Xal.domain_is_dead xal domid) then raise e);

		info "vm acknowledged shutdown request. waiting release";
		(* wait domain release with xal *)
		let xalreason =
			try
				 Xal.wait_release xal ~timeout:(60. *. 30.) domid
			with Xal.Timeout ->
				info "vm didn't shutdown after 30min";
				raise Vm_didnt_shutdown;
		in
		let matchreason =
			match xalreason, reason with
			| Xal.Crashed, _  -> false
			| Xal.Vanished, _ -> true
			| Xal.Halted, (Domain.Halt | Domain.PowerOff) -> true
			| Xal.Rebooted, Domain.Reboot       -> true
			| Xal.Halted, Domain.Reboot         -> true
			| Xal.Suspended, Domain.Suspend     -> true
			| Xal.Shutdown i, Domain.Unknown i2 -> i = i2
			| _ -> false
			in
		if not matchreason then (
			let xalreason_str = Xal.string_of_died_reason xalreason in
			error "vm shutdown for wrong reason: expecting %s, got %s"
			      reason_str xalreason_str;
			raise (Vm_shutdown_wrong_reason (reason, xalreason))
		);
		if reason = Domain.Reboot then (
			change_vmstate state VmRebooting
		);
	) (fun () -> state.vm_expect_shutdown <- false)

let pause_vm xc state =
	Domain.pause ~xc state.vm_domid;
	change_vmstate state VmPaused

let unpause_vm xc state =
	Domain.unpause ~xc state.vm_domid;
	change_vmstate state VmRunning

let domid_of_uuid xc uuid =
	let domid = ref None in
	List.iter (fun dom ->
		let duuid = (Uuid.uuid_of_int_array dom.Xc.handle) in
		if duuid = uuid then
			domid := Some dom.Xc.domid
	) (Xc.domain_getinfolist xc 0);
	!domid

let check_uuid_exists xc uuid =
	let ret = domid_of_uuid xc uuid in
	(ret <> None)

let create_vm xc xs state =
	with_create_lock state (fun () ->
		let uuid = Uuid.uuid_of_string state.vm_uuid in
		if state.vm_domid <> -1 then (
			let uuid_exists = check_uuid_exists xc uuid in
			if uuid_exists then
				raise (Vm_is_already_created (state.vm_domid, state.vm_lifestate));
			warn "internal error: domain was marked as existing but wasn't -- state corrected"
		);
		let ssidref =
			match state.vm_cfg.flask_label with
			| None       -> 0l
			| Some label ->
				try
					Xc.flask_context_to_sid xc label
				with exn ->
					warn "flask failure : exception getting a ssidref: %s" (Printexc.to_string exn);
					0l
			in
		let platformdata =
			[ "pae", string_of_bool state.vm_cfg.pae
			; "apic", string_of_bool state.vm_cfg.apic
			; "acpi", (if state.vm_cfg.acpi then "1" else "0")
			; "acpi_s3", (if state.vm_cfg.acpi then "1" else "0")
			; "acpi_s4", (if state.vm_cfg.acpi then "1" else "0")
			; "nx", string_of_bool state.vm_cfg.nx
			; "smbios-pt", string_of_bool state.vm_cfg.smbios_pt
			; "smbios-oem-types-pt", state.vm_cfg.smbios_oem_types_pt
			; "acpi-pt", string_of_bool state.vm_cfg.acpi_pt
			; "viridian", string_of_bool state.vm_cfg.viridian
			; "nested", string_of_bool state.vm_cfg.nested
			; "vcpu_number", string_of_int state.vm_cfg.vcpus
			; "vcpu_current", string_of_int state.vm_cfg.vcpus
			]
			@ (default [] (may (fun x -> [ ("timer-mode", string_of_int x) ]) state.vm_cfg.timer_mode))
			@ (default [] (may (fun x -> [ ("hpet", string_of_int x) ]) state.vm_cfg.hpet))
			@ (default [] (may (fun x -> [ ("vpt-align", string_of_int x) ]) state.vm_cfg.vpt_align))
			@ state.vm_cfg.platform in
		let info = {
			Domain.ssidref = ssidref;
			Domain.hvm = state.vm_cfg.hvm;
			Domain.hap = state.vm_cfg.hap;
			Domain.name = (match state.vm_cfg.name with Some n -> n | _ -> "");
			Domain.platformdata = platformdata;
			Domain.xsdata = [];
		} in
		change_vmstate state VmCreatingDomain;
		try
			let domid = Domain.make ~xc ~xs info uuid in
			state.vm_domid <- domid
		with exn ->
			change_vmstate state VmShutdown;
			raise exn
	)

let build_info_of_cfg cfg =
	let kernel = if cfg.hvm && cfg.kernel = "" then "/usr/lib/xen/boot/hvmloader" else cfg.kernel in
	let spec_info =
		match cfg.hvm with
		| true  ->
			let timeoffset_of_cfg t = match t with None -> "0" | Some s -> s in
			let hvminfo = {
				Domain.shadow_multiplier = 1.;
				Domain.timeoffset = (timeoffset_of_cfg cfg.timeoffset);
				Domain.xci_cpuid_signature = cfg.xci_cpuid_signature;
			} in
			Domain.BuildHVM hvminfo
		| false ->
			let pvinfo = {
				Domain.cmdline = cfg.cmdline;
				Domain.ramdisk = cfg.initrd;
			} in
			Domain.BuildPV pvinfo
		in
	{
		Domain.memory_max = (match cfg.memory_max with Some x -> x | None -> cfg.memory);
		Domain.memory_target = cfg.memory;
		Domain.memory_video = Int64.of_int (default 0 cfg.videoram);
		Domain.kernel = kernel;
		Domain.vcpus = cfg.vcpus;
		Domain.priv = spec_info;
	}

let add_usb domid uuid =
	let domid_s = string_of_int domid in
	let _ = Forkhelpers.execute_command_get_output ~withpath:true ~env:[| "DBUS_SYSTEM_BUS_ADDRESS=unix:path=/var/run/dbus/usb_proxy_socket" |]
		"/usr/bin/ctxusb" ["v4v"; domid_s; uuid; "0"] in
	info "started ctxusb"

let buildlock_fd = Unix.openfile "/var/lock/xen-buildvm-lock" [ Unix.O_CREAT ] 0o644

let build_vm xc xs state f restore =
	let cfg = state.vm_cfg in
	let domid = state.vm_domid in
	try
		let dom_path = xs.Xs.getdomainpath domid in
		Xs.transaction xs
			(fun t ->
				 List.iter (fun (k,v) ->
						    t.Xst.write (dom_path ^ "/bios-strings/" ^ k) v
					   ) cfg.bios_strings
			);
		Unixext.with_flock_ex buildlock_fd (fun () -> f state cfg);

		if cfg.disable_migrate then (
			Xc.domain_disable_migrate xc domid
		);

		set_cpuid xc domid cfg;
		set_affinity xc domid cfg;
		set_cores_per_socket xc domid cfg;

		change_vmstate state VmCreatingDevices;

		add_devices xc xs domid state restore;
		change_vmstate state VmCreated;
	with exn ->
		warn "receive exception building vm: %s" (Printexc.to_string exn);
		stop_vm xc xs state;
		change_vmstate state VmShutdown;
		raise exn

let required_videoram_kib cfg = 0L
(*
	let rec scan = function
		| [] -> false
		| ("vga-passthrough",_) :: _ -> true
		| _ :: xs -> scan xs
	in
	let vga_passthrough = scan cfg.extrahvm in
	(* non hvm require 0 additional *)
	(* pvms require 0 additional *)
	if vga_passthrough || (not cfg.hvm)
		then 0L
		else Memory.kib_of_mib (Int64.of_int (dm_videoram_mib_of_cfg cfg))

*)
let required_to_boot_kib cfg =
	let memmax = match cfg.memory_max with
		| Some x -> x
		| None -> cfg.memory in
	let requested_kib = Memory.required_to_boot_kib cfg.hvm cfg.vcpus memmax cfg.memory 1. in
	let stubdom_kib = if cfg.stubdom = None then 0L else (Memory.required_to_boot_kib false 1 cfg.stubdom_memory cfg.stubdom_memory 1.) in
	(* non vga-passthrough domains require videoram reservation *)
	let video_kib = required_videoram_kib cfg in
	Int64.add stubdom_kib (Int64.add requested_kib video_kib)

let start_vm xc xs state paused =
	maybe (fun cfg -> state.vm_next_cfg <- None; state.vm_cfg <- cfg) state.vm_next_cfg;
	let cfg = state.vm_cfg in
	(* verify if machine supports directio & hvm if config requires it *)
	Config.verify_hwcaps cfg;
	if not cfg.no_mem_check then (
		let requested_kib = required_to_boot_kib cfg in
		if not (Memory.wait_xen_free_mem ~xc requested_kib) then (
			let avail_kib = Memory.get_free_memory_kib ~xc in
			warn "failed to reserve enough memory, requested %Ld, available %Ld" requested_kib avail_kib;
			if state.vm_lifestate = VmRebooted then (
				change_vmstate state VmShutdown
			);
			raise (Not_enough_free_memory cfg)
		)
	);

	let from_scratch =
		(fun state cfg ->
			let info = build_info_of_cfg cfg in
			let arch = Domain.build ~xc ~xs info state.vm_domid in
			state.vm_arch <- arch;
		) in


	create_vm xc xs state;
	build_vm xc xs state from_scratch false;
	
	if cfg.startup <> StartupPause && (not paused) then (
		try
			Domain.unpause ~xc state.vm_domid;
			change_vmstate state VmRunning;
		with exn ->
			(* leave it in VmCreated state on failure *)
			warn "receive exception unpausing vm: %s" (Printexc.to_string exn)
	)
	
let restart_vm xc xs state paused fd =
	let from_fd =
		(fun state cfg ->
			let info = build_info_of_cfg cfg in
			Domain.restore ~xc ~xs info state.vm_domid fd;
		) in
	create_vm xc xs state;
	build_vm xc xs state from_fd true;
	if not paused then (
		try
			Domain.unpause ~xc state.vm_domid;
			change_vmstate state VmRunning;
		with exn ->
			(* leave it in VmCreated state on failure *)
			warn "receive exception unpausing vm: %s" (Printexc.to_string exn)
	)

let suspend_to_file xc xs xal state flags file =
	let callback () = shutdown_vm xc xs xal state false Domain.Suspend in

	change_vmstate state VmSuspending;
	Unixext.with_file file [ Unix.O_WRONLY; Unix.O_CREAT; ] 0o640 (fun sfd ->
		Domain.suspend ~xc ~xs ~hvm:state.vm_cfg.hvm state.vm_domid sfd flags callback;
	);
	()
	(*
	match state.vm_on_suspend_action with
	| ActionSuspend ->
		stop_vm xc xs state;
		change_vmstate state VmSuspended;
	| ActionResume ->
		let cooperative = not state.vm_cfg.hvm in
		Domain.resume ~xc ~xs ~cooperative state.vm_domid;
		change_vmstate state VmRunning;
	| _ ->
		assert false
	*)

let suspend xc xs state flags file =
	match String.split ':' file with
	| "xenserver" :: dev :: _ ->
		let mntpoint = mntdir_path state.vm_uuid in
		Misc.create_ext3fs_on dev;
		(* mount dev to some mkdir'ed tmp directory *)
		Misc.with_mounted_fs dev mntpoint (fun () ->
			Misc.with_xal (fun xal ->
				suspend_to_file xc xs xal state flags (mntpoint ^ "/suspend-image")
			)
		)
	| file :: [] ->
		Misc.with_xal (fun xal -> suspend_to_file xc xs xal state flags file)
	| _ -> assert false

let restore_from_file xc xs state paused file =
	Unixext.with_file file [ Unix.O_RDONLY; ] 0o640 (fun sfd ->
		restart_vm xc xs state paused sfd;
	)

let restore xc xs state delete paused file =
	match String.split ':' file with
	| "xenserver" :: dev :: _ ->
		let mntpoint = mntdir_path state.vm_uuid in
		Misc.with_mounted_fs dev mntpoint (fun () ->
			restore_from_file xc xs state paused (mntpoint ^ "/suspend-image")
		)
	| file :: [] ->
		restore_from_file xc xs state paused file;
		if delete then
			Unix.unlink file;
	| _ ->
		assert false

let s3_suspend xc xs state timeout =
	let has_pv_driver = Xc.hvm_check_pvdriver xc state.vm_domid in
	let domid = string_of_int state.vm_domid in
	(* fail immediately if domain has no PV driver *)
	if not has_pv_driver then (
		warn "failed to put domain %s into S3: domain has no PV driver" domid;
		false
        ) else (
		let path = "/local/domain/" ^ domid ^ "/control/shutdown" in
		xs.Xs.write path "s3";
		(* wait until domain gets into s3 by polling acpi state *)
		let start_time = Unix.time () in
		let rec wait () =
			Unix.sleep 1;
			let t = Unix.time () in
			let diff = int_of_float (t -. start_time) in
			if diff >= timeout
			then (
				warn "failed to put domain %s into S3 - timeout" domid;
				false
			) else
				match (try Xc.domain_get_acpi_s_state xc state.vm_domid with _ -> 5) with
				| 3 -> 
					  (* acpi state is S3 *)
					  info "succeeded to put domain %s into S3" domid;
					  true
				| 4 ->
					  (* sometimes domain can enter S4 instead of S3, if S3
					     state is blocked *)
					  warn "domain %s entered S4 instead of S3" domid;
					  false
				| _ ->
					  match state.vm_lifestate with
					  | VmShutdown ->
						    (* sometimes it crashes and shutdowns instead of going into S3.. *)
						    warn "domain %s shutdowned instead of entering S3" domid;
						    false
					  | _ ->
						    wait () (* continue wait *)
		in
		info "waiting for domain %s to go into S3" domid;
		wait()
        )

let s4_suspend xc xs state timeout =
	let has_pv_driver = Xc.hvm_check_pvdriver xc state.vm_domid in
	let domid = string_of_int state.vm_domid in
	(* fail immediately if domain has no PV driver *)
	if not has_pv_driver then (
		warn "failed to put domain %s into S4: domain has no PV driver" domid;
		false
        ) else (
		let path = "/local/domain/" ^ domid ^ "/control/shutdown" in
		xs.Xs.write path "hibernate";
		(* wait until domain shutdowns *)
		let start_time = Unix.time () in
		let rec wait () =
			Unix.sleep 1;
			let t = Unix.time () in
			let diff = int_of_float (t -. start_time) in
			if diff >= timeout
			then false (* timed out, failed to put domain to hibernate *)
			else match state.vm_lifestate with
			| VmShutdown -> true
			| _ -> wait () (* continue waiting *)
		in
		info "waiting for domain %s to go into S4" domid;
		match wait () with
		| true  -> info "succeeded to put domain %s into S4" domid; true
		| false -> warn "failed to put domain %s into S4" domid; false
        )

let device_cmd xc xs state ty subcmd args =
	let cfg = state.vm_cfg in
	(* specific handler *)
	let unimplemented args = Xenvmlib.Error "unimplemented command" in
	let pci_list args =
		let pcis = get_pcis cfg in
		let l = List.map (fun (id, devs) ->
			let devstrs = List.map (fun dev ->
				sprintf "  domain:%d, bus:%d, slot: %d, func: %d\n"
					dev.pci_domain dev.pci_bus dev.pci_slot dev.pci_func
			) devs in
			(sprintf "id:%d\n" id) ^ (String.concat "" devstrs)
		) pcis in
		Xenvmlib.Msg (String.concat "" l)
		in
	let disk_list args =
		let l = List.map (fun x ->
			let ty = match x.disk_physty with
				| Xenops s    -> Device.Vbd.string_of_physty s
				| ExternalVDI -> "external" in
			sprintf "physpath: %s, type: %s, virtpath: %s, mode: %s, devtype: %s\n"
				x.disk_physpath ty
				x.disk_virtpath (Device.Vbd.string_of_mode x.disk_mode)
				(Device.Vbd.string_of_devty x.disk_devtype)
		) cfg.disks in
		Xenvmlib.Msg (String.concat "" l)
		in
	let nic_list args =
		let l = List.map (fun nic ->
			sprintf "id: %d, network:%s, mac:%s\n"
				nic.nic_id (network_of_netty nic.nic_netty) nic.nic_mac
		) cfg.nics in
		Xenvmlib.Msg (String.concat "" l)
		in
	let pci_add args =
		let pci = Config.config_pci_of_string (List.hd args) in
		(* FIXME this has to do something however we don't have what
		   is necessary to do the hotplug yet *)
		ignore pci;
		Xenvmlib.Ok
		in
	let disk_add args =
		let disk = Config.config_disk_of_string (List.hd args) in
		add_disk_to_vm ~xc ~xs state disk;
		Xenvmlib.Ok
		in
	let nic_add args =
		let nic = Config.config_nic_of_string (List.hd args) in
		add_nic_to_vm ~xc ~xs state nic true;
		Xenvmlib.Ok
		in
	
	(* first match the type of device *)
	let (f_list, f_add, f_del) = match ty with
	| "pci"          -> (pci_list, pci_add, unimplemented) (* do_device_pci_cmd subcmd args *)
	| "disk" | "vbd" -> (disk_list, disk_add, unimplemented) (* do_device_disk_cmd subcmd args *)
	| "nic" | "vif"  -> (nic_list, nic_add, unimplemented) (*do_device_nic_cmd subcmd args *)
	| _              -> failwith (sprintf "device type unknown: %s" ty) in
	(* match the subcommand of the device type *)
	let f =
		match subcmd with
		| "list" -> f_list | "add" -> f_add | "del" | "rm" -> f_del
		| _ -> failwith (sprintf "device subcommand unknown: %s" subcmd)
		in
	try f args
	with exn ->
		Xenvmlib.Error (sprintf "%s %s: %s" ty subcmd (Printexc.to_string exn))

let get_new_config state =
	match state.vm_next_cfg with
	| None -> state.vm_cfg
	| Some cfg -> cfg
	    
let set_new_config state cfg =
	state.vm_next_cfg <- Some cfg

let set state field value =
	set_new_config state (Config.set (get_new_config state) field value);
	Xenvmlib.Ok

let get state field =
	let cfg = get_new_config state in
	match field with
	| "required-to-boot-kib" -> Xenvmlib.Msg (sprintf "%Ld" (required_to_boot_kib cfg))
	| _ -> let value = Config.get cfg field in
		       Xenvmlib.Msg value

let add_disk state path device ty mode devtype =
	let disk_physty = match ty with
		| "vdi" -> ExternalVDI
		| _     -> Xenops (Device.Vbd.physty_of_string ty) in
	let disk = {
		Vmconfig.disk_backend_dom = 0;
		Vmconfig.disk_physpath = path;
		Vmconfig.disk_physty = disk_physty;
		Vmconfig.disk_virtpath = device;
		Vmconfig.disk_mode = Device.Vbd.mode_of_string mode;
		Vmconfig.disk_devtype = Device.Vbd.devty_of_string devtype;
		Vmconfig.disk_dynadded = false;
		Vmconfig.disk_crypt = None;
		Vmconfig.disk_snapshot_mode = NoSnapshot;
	} in
	let cfg = get_new_config state in
	let cfg = { cfg with disks = cfg.disks @ [ disk ] } in
	set_new_config state cfg;
	Xenvmlib.Ok

let add_nic state network mac model =
        let nic = Vmconfig.default_nic in
	let nic = match network with Some x -> {nic with nic_netty=Netman.Bridge (x,"") } | None -> nic in
	let nic = match mac with Some x -> {nic with nic_mac=x} | None -> nic in
	let nic = match model with Some x -> {nic with nic_model=x} | None -> nic in
	let cfg = get_new_config state in
	let nics = Vmconfig.Config.sanitise_nics state.vm_uuid (cfg.nics @ [ nic ]) in
	let cfg = { cfg with nics = nics } in
	set_new_config state cfg;
	Xenvmlib.Ok

let add_pci state id domain bus slot func bind msitranslate power_mgmt =
	let msitranslate = may (fun b -> if b then 1 else 0) msitranslate in
	let power_mgmt = may (fun b -> if b then 1 else 0) power_mgmt in
	let id = Int64.to_int id in
	let pci = {
		Vmconfig.pci_bind = bind;
		Vmconfig.pci_domain = Int64.to_int domain;
		Vmconfig.pci_bus = Int64.to_int bus;
		Vmconfig.pci_slot = Int64.to_int slot;
		Vmconfig.pci_func = Int64.to_int func;
		Vmconfig.pci_msitranslate = msitranslate;
		Vmconfig.pci_power_mgmt = power_mgmt;
		Vmconfig.pci_guest_slot = None; (* not supported *)
		Vmconfig.pci_vdevfn = None; (* not supported *)
	} in
	let cfg = get_new_config state in
	let cfg = { cfg with pcis = cfg.pcis @ [ id, pci ] } in
	set_new_config state cfg;
	Xenvmlib.Ok

let list_nic state =
        let cfg = state.vm_cfg in
	let header = " id |         network         |       mac         | model" in
	let nic_string nic =
	  Printf.sprintf "%3d |%24s |%18s |%30s"
	    nic.Vmconfig.nic_id
	    (network_of_netty nic.Vmconfig.nic_netty)
	    nic.Vmconfig.nic_mac
	    nic.Vmconfig.nic_model
	in
	let nics = List.sort (fun a b -> compare a.nic_id b.nic_id) cfg.nics in
	let nic_lines = List.map nic_string nics in
	let msg = List.fold_left (fun acc x -> acc ^ "\n" ^ x) "" nic_lines in
	Xenvmlib.Msg (header ^ msg)

let list_pci state =
        let header = "pci-dom | bus | slot | func | bind | msi-trans | power-mgmt | guest-slot" in
        let pci_dev_string p =
	  let bind       = match p.pci_bind with true -> "y" | false -> "n"
	  and msi_trans  = match p.pci_msitranslate with Some i -> i | _ -> -1
	  and power_mgmt = match p.pci_power_mgmt with Some i -> i | _ -> -1
	  and guest_slot = match p.pci_guest_slot with Some s -> sprintf "%d" s | _ -> ""
	  in
	  Printf.sprintf "%7d |%4d |%5d |%5d |%5s |%10d |%11d |%11s" 
	    p.pci_domain p.pci_bus p.pci_slot p.pci_func bind msi_trans power_mgmt guest_slot in
        let pci_lines = List.map pci_dev_string state.vm_pcis in
	let msg = List.fold_left (fun acc x -> acc ^ "\n" ^ x) "" pci_lines in
	Xenvmlib.Msg (header ^ msg)
