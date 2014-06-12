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
 * Copyright (c) 2013 Citrix Systems, Inc.
 *)


open Printf
open Stringext

module D = Debug.Debugger(struct let name="vmconfig" end)
open D

type action = ActionSuspend | ActionResume | ActionRestart | ActionDestroy | ActionPreserve
type startupstate = StartupShutdown | StartupPause | StartupStart | StartupRestore of (string * bool)

exception InvalidConfig 

type notify_ty =
	| NotifyTcp6 of string
	| NotifyTcp of Unix.inet_addr * int
	| NotifyUnix of string
	| NotifyDBus of string option
	| NotifyNone

let string_of_notify ty =
	match ty with
	| NotifyTcp6 s            -> sprintf "tcp6,%s" s
	| NotifyTcp (iaddr, port) -> sprintf "tcp,%s:%d" "" port
	| NotifyUnix path         -> sprintf "unix,%s" path
	| NotifyDBus p            -> sprintf "dbus%s" (match p with None -> "" | Some path -> "," ^ path)
	| NotifyNone              -> ""

type snapshot_mode =
	| NoSnapshot
	| Snapshot_temporary
	| Snapshot_temporary_encrypted
	| Snapshot_coalesce
	| Snapshot_scripted
	| Snapshot_scripted_author
	| Snapshot_scripted_no_snapshot

let snapshot_mode_of_string s =
	match s with
	| "" | "none" -> NoSnapshot
	| "temporary" -> Snapshot_temporary
	| "temporary-encrypted" -> Snapshot_temporary_encrypted
	| "coalesce"  -> Snapshot_coalesce
	| "scripted"  -> Snapshot_scripted
	| "scripted-author" -> Snapshot_scripted_author
	| "scripted-no-snapshot"  -> Snapshot_scripted_no_snapshot
	| _           -> failwith "unknown snapshot mode"

type display =
	| DisplayNone
	| DisplaySDL of string option
	| DisplayVNC of bool * int * string
	| DisplayIntel of string option

let string_of_display disp =
	match disp with
	| DisplayNone       -> "none"
	| DisplaySDL x      -> String.concat ":" ([ "sdl" ] @ (match x with None -> [] | Some s -> [ s ]))
	| DisplayIntel x    -> String.concat ":" ([ "intel" ] @ (match x with None -> [] | Some s -> [ s ]))
	| DisplayVNC (u, port, k) ->
		let args = (if u then [ "use-port-unused" ] else []) @ [ "port=" ^ (string_of_int port) ] @ [ "keymap=" ^ k ] in
		String.concat ":" ([ "vnc" ] @ [ String.concat "," args ])

type config_pci = {
	pci_bind: bool;
	pci_domain: int;
	pci_bus: int;
	pci_slot: int;
	pci_func: int;
	pci_msitranslate: int option;
	pci_power_mgmt: int option;
	pci_guest_slot: int option; (* force qemu to put device on this slot in guest space *)
	pci_vdevfn: int option;
}

type config_disk_crypt = {
	disk_crypt_cipher: string;
	disk_crypt_key_size: int;
	disk_crypt_key_file: string;
	disk_crypt_key_dir: string;
}

type disk_physty = ExternalVDI | Xenops of Device.Vbd.physty

type config_disk = {
	disk_backend_dom: int;
	disk_physpath: string;
	disk_physty: disk_physty;
	disk_virtpath: string;
	disk_mode: Device.Vbd.mode;
	disk_devtype: Device.Vbd.devty;
	disk_dynadded: bool;
	disk_crypt: config_disk_crypt option;
	disk_snapshot_mode: snapshot_mode;
}

type config_nic = {
	nic_backend_dom: int;
	nic_id: int;
	nic_netty: Netman.netty;
	nic_mac: string;
	nic_model: string;
	nic_dynadded: bool;
	nic_wireless: bool;
}

type config = {
	__uuid: string option; (* do not use anymore *)
	name: string option;
	verbose: bool;
	debug: bool;
	no_mem_check: bool;
	output: string;
	(* kernel *)
	startup: startupstate;
	hvm: bool;
	hap: bool;
	kernel: string;
	cmdline: string;
	serial: string;
	initrd: string option;
	memory: int64; (* kilobytes *)
	memory_max: int64 option; (* kilobytes *)
	vcpus: int;
	pae: bool;
	apic: bool;
	acpi: bool;
	nx: bool;
	usb: bool;
	vsnd: bool;
        vkbd: bool;
        vfb: bool;
        v4v: bool;
	platform: (string * string) list;
	timeoffset: string option;
	smbios_pt: bool;
        smbios_oem_types_pt: string;
	acpi_pt: bool;
	diskinfo_pt: bool;
	viridian: bool;
	nested: bool;
	xci_cpuid_signature: bool;
	videoram: int option;
	extra_maxmem: int;
	on_halt: action;
	on_restart: action;
	on_crash: action;
	extrahvm: (string * string option) list;
	(* devices *)
	disks: config_disk list;
	nics: config_nic list;
	pcis: (int * config_pci) list;
	(* others *)
	boot: string;
	display: display;
	cpuid: Domain.cpuid_config;
	datadir: string;
	notify: notify_ty;
	daemonize: bool;
	power_management: int;
	oem_features: int;
	timer_mode: int option;
	hpet: int option;
	vpt_align: int option;
	extra_local_watches: string list;
	extra_vm_watches: string list;
	global_pci_msitranslate: int;
	global_pci_power_mgmt: int;
	global_pci_script: string option;
	sound: string option;
	inject_sci: int;
	qemu_pv: bool;
	cdrom_pt: string option;
	cpus_affinity: (int * (int list)) list;
	passthrough_ios: (int * int) list;
	passthrough_mmios: (int64 * int64) list;
	flask_label: string option;
	cores_per_socket: int option;
	stubdom: string option;
	stubdom_memory: int64; (* kilobytes *)
	stubdom_kernel: string;
	stubdom_initrd: string option;
	stubdom_cmdline: string;
	qemu_dm_path: string;
	qemu_dm_timeout: int;
	bios_strings: (string*string) list;
	vtpm_backend: int;
	vtpm_instance: int option;
	xciservice: bool;
	disable_migrate: bool;
}

let default_nic =
	{
		nic_backend_dom = 0;
		nic_id = -1;
		nic_netty = Netman.DriverDomain ("", "");
		nic_mac = "";
		nic_model = "";
		nic_dynadded = false;
	        nic_wireless = false;
	}

module Config = struct

let config_pci_of_string s =
	(* format is : domain:bus:slot.func *)
	let l = String.split ',' s in
	let id, bind, pcidev_str, params =
		match l with
		| id :: "bind" :: pcidev :: params ->
			int_of_string id, true, pcidev, params
		| id :: pcidev :: params ->
			int_of_string id, false, pcidev, params
		| _ ->
			failwith "unexpected format for pci config"
		in
	let domain, bus, slot, func =
		try Scanf.sscanf pcidev_str "%x:%x:%x.%x" (fun a b c d -> (a, b, c, d))
		with _ -> failwith "unexpected format for pci descriptor"
		in
	let split_kv s =
		match String.split ~limit:2 '=' s with
		| k :: v :: [] -> k, v
		| _            -> failwith "expected key value in params"
		in
	let params = List.map split_kv params in
	let msitranslate =
		try Some (int_of_string (snd (List.find (fun p -> fst p = "msitranslate") params)))
		with Not_found -> None
		in
	let power_mgmt =
		try Some (int_of_string (snd (List.find (fun p -> fst p = "power_mgmt") params)))
		with Not_found -> None
		in
	let guest_slot =
		try Some (int_of_string (List.assoc "guest_slot" params))
		with Not_found -> None 
		in
	let vdevfn =
		try Some (int_of_string (List.assoc "vdevfn" params))
		with Not_found -> None 
		in
	let pcidev = {
		pci_bind = bind;
		pci_domain = domain;
		pci_bus = bus;
		pci_slot = slot;
		pci_func = func;
		pci_msitranslate = msitranslate;
		pci_power_mgmt = power_mgmt;
		pci_guest_slot = guest_slot;
		pci_vdevfn = vdevfn;
	} in
	id, pcidev

let config_nic_of_string s =
	let ls = if s = "" then [] else String.split ',' s in

	let id = ref (-1)
	and bridge = ref ""
	and networkpath = ref ""
	and model = ref ""
	and mac = ref ""
	and wireless = ref false
	and backend_dom = ref 0 in

	List.iter (fun v ->
		let lv = String.split '=' v in
		let lvalue = List.nth lv 0
		and value = List.nth lv 1 in
		match lvalue with
		| "id"       -> id := int_of_string value
		| "network"  -> networkpath := value
		| "bridge"   -> bridge := value
		| "model"    -> model := value
		| "mac"      -> mac := value
		| "wireless" -> wireless := bool_of_string value
		| "backend-domid" -> backend_dom := int_of_string value
		| _          -> ()
	) ls;

	let netty =
		match (!backend_dom, !networkpath, !bridge) with
		| (0, np, br) -> Netman.Bridge (np,br)
		| (_, np, br) -> Netman.DriverDomain (np,br)
		in

	{ default_nic with
		nic_id = !id;
		nic_netty = netty;
		nic_mac = !mac;
		nic_model = !model;
	        nic_wireless = !wireless;
		nic_backend_dom = !backend_dom;
	}

(* Where NIC IDs have been left blank (or explicitly set to -1), here we allocate them a reasonable number. We also allocate a bridge and a mac, if necessary *)
let sanitise_nics vm_uuid nics =
	let sort_nics nics = List.sort (fun nic1 nic2 -> compare nic1.nic_id nic2.nic_id) nics in
	let assert_ok nics =
		ignore(List.fold_left (fun last_id nic -> 
			let id = nic.nic_id in
			if last_id=id 
			then (error "Duplicate NIC ids found (%d)" id; raise InvalidConfig);
			if id<0 || id>31 
			then (error "NIC id %d out of range (must be between 0 and 31 inclusive)" id; raise InvalidConfig);
			id)
			(-1) nics) 
	in
	let allocate_nic_id nic nics =
		let rec find_unused n =
			if List.exists (fun nic -> nic.nic_id = n) nics then find_unused (n+1) else n
		in
		let id = find_unused 0 in
		sort_nics ({nic with nic_id = id}::nics)
	in
	let (assigned,unassigned) = List.partition (fun nic -> nic.nic_id >= 0) nics in
	let nics = List.fold_right allocate_nic_id unassigned (sort_nics assigned) in
	assert_ok nics;

	let fix_bridge nic = 
	  {nic with nic_netty = 
	      match nic.nic_netty with
	      | Netman.Bridge ("","") ->
		  let l = Netdev.Bridge.list () in
		  if List.length l > 0 then
		    Netman.Bridge ("",(List.hd l))
		  else
		    Netman.DriverDomain ("","")
	      | other -> other } in

	let nics = List.map fix_bridge nics in

	let fix_mac nic =
	  {nic with nic_mac = 
	      if nic.nic_mac <> "" then nic.nic_mac else (
		let generate_random_mac () =
		  String.concat ":" (List.map (sprintf "%02x")
					([0x00; 0x16; 0x3e] @
					    List.map Random.int [0x80; 0x100; 0x100]))
		in
		let generate_vm_mac nic_id nic_model vm_uuid =
		  let s_of_c c1 c2 =
		    let s = String.create 2 in
		    s.[0] <- c1; s.[1] <- c2; s
		  in
		  let unicast_local h =
		    let v = int_of_string ("0x" ^ (List.hd h)) in
		    let n = sprintf "%02x" (0x2 lor (v land 0xfe)) in
		    n :: (List.tl h)
		  in
		  let vector =
		    (if nic_id = -1 then [] else [ string_of_int nic_id ]) @
				(match nic_model with "" -> [] | x -> [ x ]) @
		      [ vm_uuid ]
		  in
		  let digest = Digest.to_hex (Digest.string (String.concat " " vector)) in
		  let hexs = List.map (fun i -> s_of_c digest.[i] digest.[i + 12]) [ 0; 2; 1; 7; 6; 4 ] in
		  String.concat ":" (unicast_local hexs)
		in
		let is_valid_mac s =
		  let x = String.split ':' s in
		  if List.length x != 6 then
		    false
		  else (
		    try ignore (List.map (fun s -> int_of_string ("0x" ^ s)) x); true
		    with _ -> false
		  )
		in
		match nic.nic_mac with
		  | "random"    -> generate_random_mac ();
		  | "" | "vm" -> generate_vm_mac nic.nic_id nic.nic_model vm_uuid
		  | mac         ->
		      if is_valid_mac mac then mac else generate_vm_mac nic.nic_id nic.nic_model vm_uuid
	      )
	  }
	in
	List.map fix_mac nics
		
let config_disk_of_string s =
	(* physpath:phystype:virtpath:mode:devtype:snapshot_mode *)
	let ls = String.split ':' s in

	let (physpath, phystype, virtpath, mode, devtype, snapshotmode, kvs) =
		match ls with
		| physpath :: physty_s :: virtpath :: mode_s :: devtype_s :: snapshotmode_s :: left ->
			let kvs = List.fold_left (fun acc s ->
				match String.split '=' s with
				| [ k; v ] -> (k, v) :: acc
				| _        -> acc
			) [] left in
			let physty = match physty_s with
				| "vdi" -> ExternalVDI
				| _     -> Xenops (Device.Vbd.physty_of_string physty_s) in
			(physpath, physty,
			 virtpath, Device.Vbd.mode_of_string mode_s,
			 Device.Vbd.devty_of_string devtype_s,
			 snapshot_mode_of_string snapshotmode_s,
			 kvs)
		| _ ->
			failwith "need at least 6 arguments for disk"
		in
	let backend_dom = try int_of_string (List.assoc "backend-domid" kvs) with _ -> 0 in
	let dc =
		try Some {
				disk_crypt_key_file = (List.assoc "key-file" kvs);
				disk_crypt_cipher = (try (List.assoc "cipher" kvs) with Not_found -> "aes-xts-plain");
				disk_crypt_key_size = (try int_of_string (List.assoc "key-size" kvs) with Not_found -> 256);
				disk_crypt_key_dir = ""
			}
		with Not_found ->
			try Some {
				disk_crypt_key_dir = (List.assoc "key-dir" kvs);
				disk_crypt_key_file = "";
				disk_crypt_cipher = "";
				disk_crypt_key_size = 0;
			}
			with Not_found ->
				None
		in
	{
		disk_backend_dom = backend_dom;
		disk_physpath = physpath;
		disk_physty = phystype;
		disk_virtpath = virtpath;
		disk_mode = mode;
		disk_devtype = devtype;
		disk_dynadded = false;
		disk_crypt = dc;
	        disk_snapshot_mode = snapshotmode;
	}

let config_cpuid_of_string s =
	(* that's the same format as xend. i.e. NODE+NODE:REG=[01xks]{32}{,REG=[01xks]{32}}* *)
	match String.split ':' s with
	| [ nodes; x ] ->
		let nodetuple =
			match String.split '+' nodes with
			| [ x; y ] -> Int64.of_string x, Some (Int64.of_string y)
			| [ x ]    -> Int64.of_string x, None
			| _        -> failwith "cannot parse node format"
			in
		let l = String.split ',' x in
		let x = List.map (fun x ->
			match String.split '=' x with
			| [ reg_str; mask ] ->
				let reg = Domain.cpuid_reg_of_string reg_str in
				if String.startswith "0x" mask then (
					failwith "FIXME hexadecimal mask not supported yet"
				) else if String.length mask = 32 then (
					let a = Array.create 32 Domain.Default in
					for i = 0 to (String.length mask - 1); do
						a.(i) <- Domain.cpuid_rtype_of_char mask.[i]
					done;
					reg, a
				) else
					failwith "mask is not 32 characters long"
			| _ ->
				failwith "cannot parse register mask"
		) l in
		nodetuple, x
	| _           ->
		failwith "cannot parse format"

let config_notify_of_string s =
	match (String.split ~limit:2 ',' s) with
	| "tcp" :: addr :: [] -> (
		match String.split ~limit:2 ':' addr with
		| host :: port :: [] ->
			NotifyTcp (Unix.inet_addr_of_string host, int_of_string port)
		| _ ->
			failwith "notify: bad tcp format"
		)
	| "tcp6" :: addr6 :: [] ->
		(* we missing functions in previous ocaml versions. let just return error for now *)
		failwith "notify: tcp6 not implemented"
	| "unix" :: path :: [] ->
		NotifyUnix path
	| "dbus" :: args -> (
		match args with
		| []         -> NotifyDBus None
		| path :: [] -> NotifyDBus (Some path)
		| _          -> failwith "notify: bad dbus format"
		)
	| _ ->
		failwith "bah"

let config_display_of_string s =
	match String.split ~limit:2 ':' s with
	| "vnc" :: options ->
		let options = try List.hd options with _ -> "" in
		let keymap = ref "en-us" in
		let port = ref 0 in
		let unused = ref false in
		let opts = String.split ',' options in
		List.iter (fun opt ->
			match String.split '=' ~limit:2 opt with
			| "use-port-unused" :: [] -> unused := true
			| "keymap" :: s :: []     -> keymap := s
			| "port" :: s :: []       -> port := int_of_string s
			| _                       -> ()
		) opts;
		DisplayVNC (!unused, !port, !keymap)
	| "sdl" :: options   ->
		DisplaySDL (try Some (List.hd options) with _ -> None)
	| "intel" :: options ->
		DisplayIntel (try Some (List.hd options) with _ -> None)
	| "none" :: _        ->
		DisplayNone
	| _ ->
		failwith "unknown display format"

exception Unknown_boolean of string

let string_of_string_option opt = match opt with None -> "" | Some v -> v
let string_option_of_string s = match s with "" -> None | _ -> Some s
let string_of_int_option opt = match opt with None -> "" | Some v -> string_of_int v
let string_of_int64_option opt = match opt with None -> "" | Some v -> Int64.to_string v
let int_option_of_string s = if s = "" then None else Some (int_of_string s)
let int64_option_of_string s = if s = "" then None else Some (Int64.of_string s)
let bool_of_string s =
	match (String.lowercase s) with
	| "1" | "y" | "yes" | "true" -> true
	| "0" | "n" | "no" | "false" -> false
	| _                          -> raise (Unknown_boolean s)

exception Unknown_field of string

let get cfg field =
	match field with
	| "name"      -> string_of_string_option cfg.name
	| "hvm"       -> string_of_bool cfg.hvm
	| "hap"       -> string_of_bool cfg.hap
	| "kernel"    -> cfg.kernel
	| "cmdline"   -> cfg.cmdline
	| "serial"    -> cfg.serial
	| "initrd"    -> string_of_string_option cfg.initrd
	| "memory"    -> Int64.to_string cfg.memory
	| "memory-max" -> string_of_int64_option cfg.memory_max
	| "vcpus"     -> string_of_int cfg.vcpus
	| "pae"       -> string_of_bool cfg.pae
	| "apic"      -> string_of_bool cfg.apic
	| "acpi"      -> string_of_bool cfg.acpi
	| "nx"        -> string_of_bool cfg.nx
	| "viridian"  -> string_of_bool cfg.viridian
	| "nested"  -> string_of_bool cfg.nested
	| "xci-cpuid-signature"  -> string_of_bool cfg.xci_cpuid_signature
	| "usb"       -> string_of_bool cfg.usb
	| "vsnd"      -> string_of_bool cfg.vsnd
	| "vkbd"      -> string_of_bool cfg.vkbd
	| "vfb"       -> string_of_bool cfg.vfb
	| "v4v"       -> string_of_bool cfg.v4v
	| "videoram"  -> string_of_int_option cfg.videoram
	| "extra-maxmem"  -> string_of_int cfg.extra_maxmem
	| "smbios-pt" -> string_of_bool cfg.smbios_pt
        | "smbios-oem-types-pt" -> cfg.smbios_oem_types_pt
	| "acpi-pt"   -> string_of_bool cfg.acpi_pt
	| "diskinfo-pt" -> string_of_bool cfg.diskinfo_pt
	| "boot"      -> cfg.boot
	| "timer-mode" -> string_of_int_option cfg.timer_mode
	| "time-offset" -> string_of_string_option cfg.timeoffset
	| "hpet"      -> string_of_int_option cfg.hpet
	| "display"   -> string_of_display cfg.display
	| "vpt-align" -> string_of_int_option cfg.vpt_align
	| "power-management" -> string_of_int cfg.power_management
	| "oem-features" -> string_of_int cfg.oem_features
	| "pci-msitranslate" -> string_of_int cfg.global_pci_msitranslate
        | "pci-power-management" -> string_of_int cfg.global_pci_power_mgmt
	| "pci-script" -> string_of_string_option cfg.global_pci_script
	| "inject-sci" -> string_of_int cfg.inject_sci
	| "sound"      -> string_of_string_option cfg.sound
	| "notify"     -> string_of_notify cfg.notify
	| "qemu-pv"    -> string_of_bool cfg.qemu_pv
	| "cdrom-pt"   -> string_of_string_option cfg.cdrom_pt
	| "flask-label"-> string_of_string_option cfg.flask_label
	| "cores-per-socket" -> string_of_int_option cfg.cores_per_socket
	| "stubdom"    -> string_of_string_option cfg.stubdom
	| "stubdom-memory"  -> Int64.to_string cfg.stubdom_memory
	| "stubdom-kernel"  -> cfg.stubdom_kernel
	| "stubdom-initrd"  -> string_of_string_option cfg.stubdom_initrd
	| "stubdom-cmdline" -> cfg.stubdom_cmdline
	| "qemu-dm-path" -> cfg.qemu_dm_path
	| "qemu-dm-timeout" -> string_of_int cfg.qemu_dm_timeout
	| "vtpm-backend" -> string_of_int cfg.vtpm_backend
	| "vtpm-instance" -> string_of_int_option cfg.vtpm_instance
	| "xci-service" -> string_of_bool cfg.xciservice
	| "disable-migrate" -> string_of_bool cfg.disable_migrate
	| _            -> raise (Unknown_field field)

let set cfg field value =
	match field with
	| "name"      -> { cfg with name = Some value }
	| "hvm"       -> { cfg with hvm = bool_of_string value }
	| "hap"       -> { cfg with hap = bool_of_string value }
	| "kernel"    -> { cfg with kernel = value }
	| "cmdline"   -> { cfg with cmdline = value }
	| "serial"    -> { cfg with serial = value }
	| "initrd"    -> { cfg with initrd = string_option_of_string value }
	| "memory"    -> { cfg with memory = Int64.of_string value }
	| "memory-max" -> { cfg with memory_max = int64_option_of_string value }
	| "vcpus"     -> { cfg with vcpus = int_of_string value }
	| "pae"       -> { cfg with pae = bool_of_string value }
	| "apic"      -> { cfg with apic = bool_of_string value }
	| "acpi"      -> { cfg with acpi = bool_of_string value }
	| "nx"        -> { cfg with nx = bool_of_string value }
	| "viridian"  -> { cfg with viridian = bool_of_string value }
	| "nested"  -> { cfg with nested = bool_of_string value }
	| "xci-cpuid-signature"  -> { cfg with xci_cpuid_signature = bool_of_string value }
	| "usb"       -> { cfg with usb = bool_of_string value }
	| "vsnd"      -> { cfg with vsnd = bool_of_string value }
	| "vkbd"      -> { cfg with vkbd = bool_of_string value }
	| "vfb"       -> { cfg with vfb  = bool_of_string value }
	| "v4v"       -> { cfg with v4v  = bool_of_string value }
	| "videoram"  -> { cfg with videoram = int_option_of_string value }
	| "extra-maxmem"  -> { cfg with extra_maxmem = int_of_string value }
	| "smbios-pt" -> { cfg with smbios_pt = bool_of_string value }
        | "smbios-oem-types-pt" -> { cfg with smbios_oem_types_pt = value }
	| "acpi-pt"   -> { cfg with acpi_pt = bool_of_string value }
	| "diskinfo-pt"      -> { cfg with diskinfo_pt = bool_of_string value }
	| "boot"             -> { cfg with boot = value }
	| "timer-mode"       -> { cfg with timer_mode = int_option_of_string value }
	| "time-offset"      -> { cfg with timeoffset = string_option_of_string value }
	| "hpet"             -> { cfg with hpet = int_option_of_string value }
	| "display"          -> { cfg with display = config_display_of_string value }
	| "vpt-align"        -> { cfg with vpt_align = int_option_of_string value }
	| "power-management" -> { cfg with power_management = int_of_string value }
	| "oem-features"     -> { cfg with oem_features = int_of_string value }
	| "pci-msitranslate" -> { cfg with global_pci_msitranslate = int_of_string value }
	| "pci-power-management" -> { cfg with global_pci_power_mgmt = int_of_string value }
	| "pci-script"       -> { cfg with global_pci_script = string_option_of_string value }
	| "inject-sci"       -> { cfg with inject_sci = int_of_string value } 
	| "sound"            -> { cfg with sound = string_option_of_string value }
	| "notify"           -> { cfg with notify = config_notify_of_string value }
	| "qemu-pv"          -> { cfg with qemu_pv = bool_of_string value }
	| "cdrom-pt"         -> { cfg with cdrom_pt = string_option_of_string value }
	| "flask-label"      -> { cfg with flask_label = string_option_of_string value }
	| "cores-per-socket" -> { cfg with cores_per_socket = int_option_of_string value }
	| "stubdom"          -> { cfg with stubdom = string_option_of_string value }
	| "stubdom-memory"   -> { cfg with stubdom_memory = Int64.of_string value }
	| "stubdom-kernel"   -> { cfg with stubdom_kernel = value }
	| "stubdom-initrd"   -> { cfg with stubdom_initrd = string_option_of_string value }
	| "stubdom-cmdline"  -> { cfg with stubdom_cmdline = value }
	| "qemu-dm-path"     -> { cfg with qemu_dm_path = value }
	| "qemu-dm-timeout"  -> { cfg with qemu_dm_timeout = int_of_string value }
	| "vtpm-backend"     -> { cfg with vtpm_backend = int_of_string value }
	| "vtpm-instance"    -> { cfg with vtpm_instance = int_option_of_string value }
	| "xci-service"      -> { cfg with xciservice = bool_of_string value }
	| "disable-migrate"  -> { cfg with disable_migrate = bool_of_string value }
	| _           -> raise (Unknown_field field)

let list_add cfg field value =
	let config_extrahvm_of_string s =
		match String.split ~limit:2 '=' s with
		| k :: v :: [] -> k, Some v
		| k :: []      -> k, None
		| _            -> failwith "bad format for extrahvm. expecting k=v"
		in
	let config_platform_of_string s =
		match String.split ~limit:2 '=' s with
		| k :: v :: [] -> k, v
		| k :: []      -> k, ""
		| _            -> failwith "bad format for platform. expecting k=v"
		in
	let config_cpus_affinity_of_string s =
		match String.split ~limit:2 ':' s with
		| k :: v :: [] ->
			let id = int_of_string k in
			let args = String.split ',' v in
			let vcpus = List.map int_of_string args in
			(id, vcpus)
		| _            -> failwith "bad format for cpu affinity. expecting id:list"
		in
	let config_passthrough_mmio_of_string s =
		match String.split ~limit:2 '-' s with
		| s :: e :: [] ->
			let startreg = Int64.of_string s
			and endreg = Int64.of_string s in
			(startreg, endreg)
		| _            -> failwith "bad format for passthrough mmio. expecting int-int"
		in
	let config_passthrough_io_of_string s =
		match String.split ~limit:2 '-' s with
		| s :: e :: [] ->
			let startreg = int_of_string s
			and endreg = int_of_string s in
			(startreg, endreg)
		| _            -> failwith "bad format for passthrough io. expecting int-int"
		in
	let config_bios_string_of_string s =
		match String.split ~limit:2 '=' s with
		| k :: v :: [] -> k, v
		| _            -> failwith "bad format for bios-string. expecting k=v"
		in

	match field with
	| "disk"        -> { cfg with disks = cfg.disks @ [ config_disk_of_string value ] }
	| "nic" | "vif" -> { cfg with nics = cfg.nics @ [ config_nic_of_string value ] }
	| "pci"         -> { cfg with pcis = cfg.pcis @ [ config_pci_of_string value ] }
	| "cpuid"       -> { cfg with cpuid = cfg.cpuid @ [ config_cpuid_of_string value ] }
	| "extra-hvm"   -> { cfg with extrahvm = cfg.extrahvm @ [ config_extrahvm_of_string value ] }
	| "platform"    -> { cfg with platform = cfg.platform @ [ config_platform_of_string value ] }
	| "extra-local-watch" -> { cfg with extra_local_watches = cfg.extra_local_watches @ [ value ] }
	| "extra-vm-watch"    -> { cfg with extra_vm_watches = cfg.extra_vm_watches @ [ value ] }
	| "cpus-affinity"     -> { cfg with cpus_affinity = cfg.cpus_affinity @ [ config_cpus_affinity_of_string value ] }
	| "passthrough-io"    -> { cfg with passthrough_ios = cfg.passthrough_ios @ [ config_passthrough_io_of_string value ] }
	| "passthrough-mmio"  -> { cfg with passthrough_mmios = cfg.passthrough_mmios @ [ config_passthrough_mmio_of_string value ] }
	| "bios-string"       -> { cfg with bios_strings = cfg.bios_strings @ [ config_bios_string_of_string value ] }
	| _             -> raise (Unknown_field field)

let list_del cfg field index =
	match field with
	| _            -> raise (Unknown_field field)

let list_get cfg field =
	match field with
	| _            -> raise (Unknown_field field)

let empty =
	{
		__uuid = None;
		name = None;
		verbose = false;
		no_mem_check = false;
		output = "";
		startup = StartupShutdown;

		(* get/set *)
		debug = false;
		hvm = false;
		hap = true;
		kernel = "";
		cmdline = "";
		serial = "";
		initrd = None;
		memory = -1L;
		memory_max = None;
		vcpus = 1;
		pae = false;
		acpi = false;
		apic = false;
		nx = false;
		smbios_pt = false;
                smbios_oem_types_pt = "";
		acpi_pt = false;
		diskinfo_pt = false;
		viridian = false;
		nested = false;
		xci_cpuid_signature = false;
		usb = true;
		vsnd = false;
                vkbd = false;
                vfb = false;
                v4v = false;
		videoram = None;
		extra_maxmem = 0;
		boot = "cd";
		power_management = 0;
		oem_features = 0;
		timer_mode = None;
		timeoffset = None;
		hpet = None;
		display = DisplayVNC (false, 0, "en-us");
		vpt_align = None;
		global_pci_msitranslate = 1;
		global_pci_power_mgmt = 1;
		global_pci_script = None;
		inject_sci = 0;
		sound = None;
		qemu_pv = true;
		cdrom_pt = None;

		(* list_{get/del/add} *)
		disks = [];
		nics = [];
		pcis = [];
		cpuid = [];
		platform = [];
		extrahvm = [];
		extra_local_watches = [];
		extra_vm_watches = [];

		(* others *)
		datadir = "";
		notify = NotifyNone;
		daemonize = false;
		on_halt = ActionDestroy;
		on_restart = ActionRestart;
		on_crash = ActionDestroy;
		cpus_affinity = [];
		passthrough_ios = [];
		passthrough_mmios = [];
		flask_label = None;
		cores_per_socket = None;
		stubdom = None;
		stubdom_memory = 48L;
		stubdom_initrd = Some "/usr/lib/xen/boot/stubdomain-initramfs";
		stubdom_kernel = "/usr/lib/xen/boot/stubdomain-bzImage";
		stubdom_cmdline = "init=/init xen_blkfront.max=8";
		qemu_dm_path = "";
		qemu_dm_timeout = 30;
		bios_strings = [];
		vtpm_backend = 0;
		vtpm_instance = None;
		xciservice = false;
		disable_migrate = false;
	}

let of_file uuid error_report file =
	let cfg = ref empty in

	let __uuid = ref None
	and startup = ref StartupStart
	and debug = ref (!cfg.debug)
	and output = ref (!cfg.output)
	and no_mem_check = ref (!cfg.no_mem_check)
	and memory = ref (-1)
	and stubdom_memory = ref (Int64.to_int empty.stubdom_memory)
		in

	let action_of_string s =
		match s with
		| "restart"  -> ActionRestart
		| "destroy"  -> ActionDestroy
		| "preserve" -> ActionPreserve
		| _          -> failwith "unknown action state"
		in
	let set_startup s =
		match String.split ~limit:2 ' ' s with
		| "started" :: _ | "start" :: _     -> startup := StartupStart 
		| "paused" :: _                     -> startup := StartupPause
		| "shutdown" :: _ | "poweroff" :: _ -> startup := StartupShutdown
		| "restore" :: file :: _            -> startup := StartupRestore (file, false)
		| "restore-del" :: file :: _        -> startup := StartupRestore (file, true)
		| _                                 -> ()
		in
	let cfg_args = [
		("startup", Config.String set_startup);
		("output", Config.Set_string output);
		("uuid", Config.String (fun s -> __uuid := Some s));
		("debug", Config.Set_bool debug);
		("memory", Config.Set_int memory);
		("stubdom-memory", Config.Set_int stubdom_memory);
		("no_mem_check", Config.Set_bool no_mem_check);
	] in
	let kv k v =
		match k with
		| "disk" | "vif" | "nic" | "pci" | "cpuid" | "cpus-affinity"
		| "extra-hvm" | "extra-local-watch" | "extra-vm-watch"
		| "passthrough-io" | "passthrough-mmio" | "bios-string" | "platform" ->
			cfg := list_add !cfg k v
		| "on_halt"    -> cfg := { !cfg with on_halt    = action_of_string v }
		| "on_restart" -> cfg := { !cfg with on_restart = action_of_string v }
		| "on_crash"   -> cfg := { !cfg with on_crash   = action_of_string v }
		| _      ->
			cfg := set !cfg k v
		in

	begin try
		Config.read file cfg_args kv;
	with
		Config.Error ls -> error_report ls
	end;

	let nics = sanitise_nics uuid !cfg.nics in

	{ !cfg with
		__uuid = !__uuid;
		debug = !debug;
		memory = Int64.mul (Int64.of_int !memory) 1024L;
		memory_max = (match !cfg.memory_max with None -> None | Some x -> Some (Int64.mul x 1024L));
		stubdom_memory = Int64.mul (Int64.of_int !stubdom_memory) 1024L;
		startup = !startup;
		output = !output;
		nics = nics;
		no_mem_check = !no_mem_check;
	}

exception RequiresMoreCapabilities of string

let verify_hwcaps cfg =
	let verify_hvm = cfg.hvm in
	let verify_directio = cfg.hvm && cfg.pcis <> [] in
	if verify_hvm || verify_directio then (
		let physinfo = Xc.with_intf (fun xc -> Xc.physinfo xc) in
		let verify =
			(if verify_hvm then [ ("hvm", List.mem Xc.CAP_HVM physinfo.Xc.capabilities) ] else []) @
			(if verify_directio then [ ("directio", List.mem Xc.CAP_DirectIO physinfo.Xc.capabilities) ] else [])
			in
		let missings = List.map fst (List.filter (fun (_, found) -> not found) verify) in
		if missings <> [] then (
			raise (RequiresMoreCapabilities (sprintf "config requires %s" (String.concat ", " missings)))
		)
	)
end
