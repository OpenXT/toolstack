(*
 * Copyright (C) 2006-2007 XenSource Ltd.
 * Copyright (C) 2008      Citrix Ltd.
 * Author Vincent Hanquez <vincent.hanquez@eu.citrix.com>
 * Author Dave Scott <dave.scott@eu.citrix.com>
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
open Printf
open Stringext
open Listext
open Pervasiveext

module D = Debug.Debugger(struct let name = "xenops" end)
open D

type create_info = {
	ssidref: int32;
	hvm: bool;
	hap: bool;
	name: string;
	xsdata: (string * string) list;
	platformdata: (string * string) list;
}

type build_hvm_info = {
	shadow_multiplier: float;
	timeoffset: string;
	xci_cpuid_signature: bool;
}

type build_pv_info = {
	cmdline: string;
	ramdisk: string option;
}

type builder_spec_info = BuildHVM of build_hvm_info | BuildPV of build_pv_info

type build_info = {
	memory_max: int64;    (* memory max in kilobytes *)
	memory_target: int64; (* memory target in kilobytes *)
        memory_video: int64;  (* memory video in megabytes *)
	kernel: string;       (* in hvm case, point to hvmloader *)
	vcpus: int;           (* vcpus max *)
	priv: builder_spec_info;
}

type stubdom_info = {
	stubdom_target: int;
	stubdom_memory: int64;
	stubdom_kernel: string;
	stubdom_initrd: string option;
	stubdom_cmdline: string;
}

type domid = int

exception Restore_signature_mismatch
exception Domain_build_failed
exception Domain_restore_failed
exception Domain_restore_truncated_hvmstate
exception Xenguest_protocol_failure of string (* internal protocol failure *)
exception Xenguest_failure of string (* an actual error is reported to us *)
exception Timeout_backend
exception Could_not_read_file of string (* eg linux kernel/ initrd *)
exception Domain_stuck_in_dying_state of Xc.domid
exception Stubdom_didnt_shutdown of Xc.domid

let xend_save_signature = "LinuxGuestRecord"
let save_signature = "XenSavedDomain\n"
let qemu_save_signature = "QemuDeviceModelRecord\n"
let hvmloader = "/usr/lib/xen/boot/hvmloader"
let releaseDomain = "@releaseDomain"
let introduceDomain = "@introduceDomain"

let log_exn_continue msg f x = try f x with e -> debug "Ignoring exception: %s while %s" (Printexc.to_string e) msg

let log_exn_rm ~xs x = log_exn_continue ("xenstore-rm " ^ x) xs.Xs.rm x

let set_difference a b = List.filter (fun x -> not(List.mem x b)) a

let assert_file_is_readable filename = 
	try Unix.access filename [ Unix.F_OK; Unix.R_OK ]
	with _ -> raise (Could_not_read_file filename)
let maybe f = function None -> () | Some x -> f x

type domarch = Arch_HVM | Arch_native | Arch_X64 | Arch_X32

let string_of_domarch = function
	| Arch_HVM    -> "hvm"
	| Arch_native -> ""
	| Arch_X64    -> "x64"
	| Arch_X32    -> "x32"

let domarch_of_string = function
	| "hvm" -> Arch_HVM
	| "x64" -> Arch_X64
	| "x32" -> Arch_X32
	| _     -> Arch_native

let read_platformflags ~xs domid =
	let path = xs.Xs.getdomainpath domid ^ "/platform" in
	let entries = try xs.Xs.directory path with Xb.Noent -> [] in
	List.map (fun k ->
		let v = xs.Xs.read (path ^ "/" ^ k) in
		(k,v)
	) entries

let make ~xc ~xs info uuid =
	let flags = if info.hvm then (
	  let default_flags =
		(if info.hvm then [ Xc.CDF_HVM ] else []) @
		(if (info.hvm && info.hap) then [ Xc.CDF_HAP ] else []) in
	   if (List.mem_assoc "hap" info.platformdata) then (
              if (List.assoc "hap" info.platformdata) = "false" then (
                 debug "HAP will be disabled for VM %s." (Uuid.to_string uuid);
                 [ Xc.CDF_HVM ]
              ) else if (List.assoc "hap" info.platformdata) = "true" then (
                 debug "HAP will be enabled for VM %s." (Uuid.to_string uuid);
                 [ Xc.CDF_HVM; Xc.CDF_HAP ] 
              ) else (
                 debug "Unrecognized HAP platform value.  Assuming default settings for VM %s." (Uuid.to_string uuid);
                 default_flags
              )
           ) else
              default_flags
        ) else [] in
	let domid = Xc.domain_create xc info.ssidref flags uuid in
	let name = if info.name <> "" then info.name else sprintf "Domain-%d" domid in
	try
		let dom_path = xs.Xs.getdomainpath domid in
		let vm_path = "/vm/" ^ (Uuid.to_string uuid) in
		let vss_path = "/vss/" ^ (Uuid.to_string uuid) in
		let roperm = Xenbus.roperm_for_guest domid in
		let rwperm = Xenbus.rwperm_for_guest domid in
		debug "Regenerating the xenstored tree under: [%s]" dom_path;

		Xs.transaction xs (fun t ->
			(* Clear any existing rubbish in xenstored *)
			t.Xst.rm dom_path;
			t.Xst.mkdir dom_path;
			t.Xst.setperms dom_path roperm;

			t.Xst.rm vm_path;
			t.Xst.mkdir vm_path;
			t.Xst.setperms vm_path roperm;

			t.Xst.rm vss_path;
			t.Xst.mkdir vss_path;
			t.Xst.setperms vss_path rwperm;

			t.Xst.write (dom_path ^ "/vm") vm_path;
			t.Xst.write (dom_path ^ "/vss") vss_path;
			t.Xst.write (dom_path ^ "/name") name;

			(* create cpu and memory directory with read only perms *)
			List.iter (fun dir ->
				let ent = sprintf "%s/%s" dom_path dir in
				t.Xst.mkdir ent;
				t.Xst.setperms ent roperm
			) [ "cpu"; "memory" ];
			(* create read/write nodes for the guest to use *)
			List.iter (fun dir ->
				let ent = sprintf "%s/%s" dom_path dir in
				t.Xst.mkdir ent;
				t.Xst.setperms ent rwperm
			) [ "device"; "error"; "drivers"; "control"; "attr"; "data"; "messages" ];
		);

		xs.Xs.writev vm_path [
			"uuid", (Uuid.to_string uuid);
			"name", name;
		];

		xs.Xs.writev dom_path info.xsdata;
		xs.Xs.writev (dom_path ^ "/platform") info.platformdata;
		xs.Xs.write (dom_path ^ "/control/platform-feature-multiprocessor-suspend") "1";

		debug "Created domain with id: %d" domid;
		domid
	with e ->
		debug "Caught exception in domid %d creation: %s" domid (Printexc.to_string e);
		raise e

(** create store and console channels *)
let create_channels ~xc domid =
	let store = Xc.evtchn_alloc_unbound xc domid 0 in
	let console = Xc.evtchn_alloc_unbound xc domid 0 in
	store, console

let build_pre ~xc ~xs ~vcpus ~xen_max_mib ~shadow_mib ~required_host_free_mib domid =
	debug "build_pre domid=%d; max=%Ld MiB; shadow=%Ld MiB; required=%Ld MiB"
		domid xen_max_mib shadow_mib required_host_free_mib;

	(* CA-39743: Wait, if necessary, for the Xen scrubber to catch up. *)
	Memory.wait_xen_free_mem ~xc (Memory.kib_of_mib required_host_free_mib);

	let shadow_mib = Int64.to_int shadow_mib in

	let dom_path = xs.Xs.getdomainpath domid in
	let read_platform flag = xs.Xs.read (dom_path ^ "/platform/" ^ flag) in
	let int_platform_flag flag = try Some (int_of_string (read_platform flag)) with _ -> None in
	let timer_mode = int_platform_flag "timer-mode" in
	let hpet = int_platform_flag "hpet" in
	let vpt_align = int_platform_flag "vpt-align" in

	let maybe_exn_ign name f opt =
		maybe (fun opt -> try f opt with exn -> warn "exception setting %s: %s" name (Printexc.to_string exn)) opt
		in

	maybe_exn_ign "timer mode" (fun mode -> Xc.domain_set_timer_mode xc domid mode) timer_mode;
	maybe_exn_ign "hpet" (fun hpet -> Xc.domain_set_hpet xc domid hpet) hpet;
	maybe_exn_ign "vpt align" (fun vpt_align -> Xc.domain_set_vpt_align xc domid vpt_align) vpt_align;

	Xc.domain_max_vcpus xc domid vcpus;
	info "domid=%d set maxmem %Ld kb" domid (Memory.kib_of_mib xen_max_mib);
	Xc.domain_setmaxmem xc domid (Memory.kib_of_mib xen_max_mib);
	Xc.shadow_allocation_set xc domid shadow_mib;
	create_channels ~xc domid

let resume_post ~xc ~xs domid =
	let dom_path = xs.Xs.getdomainpath domid in
	let store_mfn_s = xs.Xs.read (dom_path ^ "/store/ring-ref") in
	let store_mfn = Nativeint.of_string store_mfn_s in
	let store_port = int_of_string (xs.Xs.read (dom_path ^ "/store/port")) in
	xs.Xs.introduce domid store_mfn store_port

(* puts value in store after the domain build succeed *)
let build_post ~xc ~xs ~vcpus ~static_max_mib ~target_mib domid
		store_mfn store_port ents vments =
	let dom_path = xs.Xs.getdomainpath domid in
	(* Unit conversion. *)
	let static_max_kib = Memory.kib_of_mib static_max_mib in
	let target_kib = Memory.kib_of_mib target_mib in
	(* expand local stuff with common values *)
	let ents =
		[ ("memory/static-max", Int64.to_string static_max_kib);
		  ("memory/target", Int64.to_string target_kib);
		  ("domid", string_of_int domid);
		  ("store/port", string_of_int store_port);
		  ("store/ring-ref", sprintf "%nu" store_mfn);
		] @ ents in
	Xs.transaction xs (fun t -> t.Xst.writev dom_path ents);
	if vments <> [] then (
		let vm_path = xs.Xs.read (dom_path ^ "/vm") in
		Xs.transaction xs (fun t -> t.Xst.writev vm_path vments)
	);
	xs.Xs.introduce domid store_mfn store_port

(** build a linux type of domain *)
let build_linux ~xc ~xs ~static_max_kib ~target_kib ~video_mib ~kernel ~cmdline ~ramdisk
		~vcpus domid =
	assert_file_is_readable kernel;
	maybe assert_file_is_readable ramdisk;

	(* Convert memory configuration values into the correct units. *)
	let static_max_mib = Memory.mib_of_kib_used static_max_kib in
	let target_mib     = Memory.mib_of_kib_used target_kib in

	(* Sanity check. *)
	assert (target_mib <= static_max_mib);

	(* Adapt memory configuration values for Xen and the domain builder. *)
	let build_start_mib =
		Memory.Linux.build_start_mib video_mib target_mib in
	let xen_max_mib =
		Memory.Linux.xen_max_mib static_max_mib in
	let shadow_multiplier =
		Memory.Linux.shadow_multiplier_default in
	let shadow_mib =
		Memory.Linux.shadow_mib static_max_mib vcpus shadow_multiplier in
	let required_host_free_mib =
		Memory.Linux.footprint_mib target_mib static_max_mib vcpus shadow_multiplier in

	let store_port, console_port = build_pre ~xc ~xs
		~xen_max_mib ~shadow_mib ~required_host_free_mib ~vcpus domid in

	let platformflags = read_platformflags ~xs domid in

	info "building linux domid=%d start_mib=%Ld kernel=%s ramdisk=%s cmdline=%s" domid build_start_mib
	     kernel (default "" ramdisk) cmdline;

	info "domid=%d set memmap limit %Ld kb" domid (Memory.kib_of_mib xen_max_mib);
	Xc.domain_set_memmap_limit xc domid (Memory.kib_of_mib xen_max_mib);

	let xgh = Xg.init () in
	let store_mfn, console_mfn, protocol = Xg.linux_build xgh domid (Int64.to_int build_start_mib)
		kernel ramdisk cmdline "" platformflags 0 store_port console_port in
	Xg.close xgh;

	let local_stuff = [
		"serial/0/limit",    string_of_int 65536;
		"console/limit",     string_of_int 65536;
		"console/port",      string_of_int console_port;
		"console/ring-ref",  sprintf "%nu" console_mfn;
	] in
	let vm_stuff = [] in
	build_post ~xc ~xs ~vcpus ~target_mib ~static_max_mib
		domid store_mfn store_port local_stuff vm_stuff;
	match protocol with
	| "x86_32-abi" -> Arch_X32
	| "x86_64-abi" -> Arch_X64
	| _            -> Arch_native

(** build hvm type of domain *)
let build_hvm ~xc ~xs ~static_max_kib ~target_kib ~video_mib ~shadow_multiplier ~vcpus
              ~kernel ~timeoffset ~xci_cpuid_signature domid =
	assert_file_is_readable kernel;

	(* Convert memory configuration values into the correct units. *)
	let static_max_mib = Memory.mib_of_kib_used static_max_kib in
	let target_mib     = Memory.mib_of_kib_used target_kib in

	(* Sanity check. *)
	assert (target_mib <= static_max_mib);

	(* Adapt memory configuration values for Xen and the domain builder. *)
	let build_max_mib =
		Memory.HVM.build_max_mib video_mib static_max_mib in
	let build_start_mib =
		Memory.HVM.build_start_mib video_mib target_mib in
	let xen_max_mib =
		Memory.HVM.xen_max_mib static_max_mib in
	let shadow_mib =
		Memory.HVM.shadow_mib static_max_mib vcpus shadow_multiplier in
	let required_host_free_mib =
		Memory.HVM.footprint_mib target_mib static_max_mib vcpus shadow_multiplier in

	let store_port, console_port = build_pre ~xc ~xs
		~xen_max_mib ~shadow_mib ~required_host_free_mib ~vcpus domid in

	let platformflags = read_platformflags ~xs domid in

	info "building hvm domid=%d max_mib=%Ld start_mib=%Ld video_mib=%Ld kernel=%s xci-cpuid-signature=%s" domid build_max_mib build_start_mib video_mib kernel (string_of_bool xci_cpuid_signature);
	Xc.domain_set_xci_cpuid_signature xc domid xci_cpuid_signature;

	let xgh = Xg.init () in
	let store_mfn, console_mfn = Xg.hvm_build xgh domid (Int64.to_int build_max_mib) (Int64.to_int build_start_mib) kernel platformflags store_port console_port in
	Xg.close xgh;

	(* XXX: domain builder will reduce our shadow allocation under our feet.
	   Detect this and override. *)
	let requested_shadow_mib = Int64.to_int shadow_mib in
	let actual_shadow_mib = Xc.shadow_allocation_get xc domid in
	if actual_shadow_mib < requested_shadow_mib then begin
		warn
			"HVM domain builder reduced our \
			shadow memory from %d to %d MiB; reverting" 
			requested_shadow_mib actual_shadow_mib;
		Xc.shadow_allocation_set xc domid requested_shadow_mib;
		let shadow = Xc.shadow_allocation_get xc domid in
		debug "Domain now has %d MiB of shadow" shadow;
	end;

	let local_stuff = [
		"serial/0/limit",    string_of_int 65536;
		"console/limit",     string_of_int 65536;
		"console/port",      string_of_int console_port;
		"console/ring-ref",  sprintf "%nu" console_mfn;
		"hvmloader/bios",    "seabios";
		"hvmloader/seabios-legacy-load-roms", "1";
	] in
(*
	let store_mfn =
		try Nativeint.of_string line
		with _ -> raise Domain_build_failed in
*)
	let vm_stuff = [
		"rtc/timeoffset",    timeoffset;
	] in

	build_post ~xc ~xs ~vcpus ~target_mib ~static_max_mib
		domid store_mfn store_port local_stuff vm_stuff;

	Arch_HVM

let build ~xc ~xs info domid =
	match info.priv with
	| BuildHVM hvminfo ->
		build_hvm ~xc ~xs ~static_max_kib:info.memory_max ~target_kib:info.memory_target ~video_mib:info.memory_video
		          ~shadow_multiplier:hvminfo.shadow_multiplier ~vcpus:info.vcpus
		          ~kernel:info.kernel ~timeoffset:hvminfo.timeoffset ~xci_cpuid_signature:hvminfo.xci_cpuid_signature
			   domid
	| BuildPV pvinfo   ->
		build_linux ~xc ~xs ~static_max_kib:info.memory_max ~target_kib:info.memory_target ~video_mib:info.memory_video
		            ~kernel:info.kernel ~cmdline:pvinfo.cmdline ~ramdisk:pvinfo.ramdisk
		            ~vcpus:info.vcpus domid 

let read_signature fd =
	let l_new_sig = String.length save_signature in
	let l_leg_sig = String.length xend_save_signature in
	let minlen = min l_new_sig l_leg_sig in

	let s = Io.read fd minlen in
	let end_to_read, oldformat =
		if String.startswith s save_signature then (
			String.sub save_signature minlen (l_new_sig - minlen), false
		) else if String.startswith s xend_save_signature then
			String.sub qemu_save_signature minlen (l_leg_sig - minlen), true
		else
			raise Restore_signature_mismatch;
		in
	if end_to_read <> "" then (
		if Io.read fd (String.length end_to_read) <> end_to_read then
			raise Restore_signature_mismatch;
	);
	oldformat

(* restore a domain from a file descriptor. it read first the signature
 * to be we are not trying to restore from random data.
 * the linux_restore process is in charge to allocate memory as it's needed
 *)
let restore_common ~xc ~xs ~hvm ~store_port ~console_port ~vcpus ~extras domid fd =
	let oldformat = read_signature fd in
	if oldformat then (
		let cfglen = Io.read_int fd in
		ignore (Io.read fd cfglen)
	);

	Unix.clear_close_on_exec fd;

	let platformflags = read_platformflags ~xs domid in
	let xgh = Xg.init () in
	let store_mfn, console_mfn = Xg.domain_restore xgh fd domid platformflags store_port console_port hvm in
	Xg.close xgh;

	if hvm then (
		let qemu_save_signature =
			if oldformat then
				String.sub qemu_save_signature 0 (String.length qemu_save_signature - 1)
			else
				qemu_save_signature
			in
		(* restore qemu-dm tmp file *)
		if Io.read fd (String.length qemu_save_signature) <> qemu_save_signature then
			raise Restore_signature_mismatch;
		let limit = Int64.of_int (Io.read_int fd) in
		debug "qemu-dm state file size: %Ld" limit;

		let file = sprintf "/tmp/xen.qemu-dm.%d" domid in
		let fd2 = Unix.openfile file [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC; ] 0o640 in
		finally (fun () ->
			if Unixext.copy_file ~limit fd fd2 <> limit then
				raise Domain_restore_truncated_hvmstate
		) (fun () -> Unix.close fd2);
	);
	store_mfn, console_mfn

let resume ~xc ~xs ~cooperative domid =
	if cooperative then
		Xc.domain_resume_fast xc domid
	else (
	)

let pv_restore ~xc ~xs ~static_max_kib ~target_kib ~vcpus domid fd =

	(* Convert memory configuration values into the correct units. *)
	let static_max_mib = Memory.mib_of_kib_used static_max_kib in
	let target_mib     = Memory.mib_of_kib_used target_kib in

	(* Sanity check. *)
	assert (target_mib <= static_max_mib);

	(* Adapt memory configuration values for Xen and the domain builder. *)
	let xen_max_mib =
		Memory.Linux.xen_max_mib static_max_mib in
	let shadow_multiplier =
		Memory.Linux.shadow_multiplier_default in
	let shadow_mib =
		Memory.Linux.shadow_mib static_max_mib vcpus shadow_multiplier in
	let required_host_free_mib =
		Memory.Linux.footprint_mib target_mib static_max_mib vcpus shadow_multiplier in

	let store_port, console_port = build_pre ~xc ~xs
		~xen_max_mib ~shadow_mib ~required_host_free_mib ~vcpus domid in

	let store_mfn, console_mfn = restore_common ~xc ~xs ~hvm:false
	                                            ~store_port ~console_port
	                                            ~vcpus ~extras:[] domid fd in
	let local_stuff = [
		"serial/0/limit",    string_of_int 65536;
		"console/limit",     string_of_int 65536;
		"console/port",      string_of_int console_port;
		"console/ring-ref",  sprintf "%nu" console_mfn;
	] in
	let vm_stuff = [] in
	build_post ~xc ~xs ~vcpus ~target_mib ~static_max_mib
		domid store_mfn store_port local_stuff vm_stuff

let hvm_restore ~xc ~xs ~static_max_kib ~target_kib ~shadow_multiplier ~vcpus  ~timeoffset ~xci_cpuid_signature domid fd =

	(* Convert memory configuration values into the correct units. *)
	let static_max_mib = Memory.mib_of_kib_used static_max_kib in
	let target_mib     = Memory.mib_of_kib_used target_kib in

	(* Sanity check. *)
	assert (target_mib <= static_max_mib);

	(* Adapt memory configuration values for Xen and the domain builder. *)
	let xen_max_mib =
		Memory.HVM.xen_max_mib static_max_mib in
	let shadow_mib =
		Memory.HVM.shadow_mib static_max_mib vcpus shadow_multiplier in
	let required_host_free_mib =
		Memory.HVM.footprint_mib target_mib static_max_mib vcpus shadow_multiplier in

	let store_port, console_port = build_pre ~xc ~xs
		~xen_max_mib ~shadow_mib ~required_host_free_mib ~vcpus domid in
	Xc.domain_set_xci_cpuid_signature xc domid xci_cpuid_signature;

	let store_mfn, console_mfn = restore_common ~xc ~xs ~hvm:true
	                                            ~store_port ~console_port
	                                            ~vcpus ~extras:[] domid fd in
	let local_stuff = [
		"serial/0/limit",    string_of_int 65536;
		"console/limit",     string_of_int 65536;
		"console/port",      string_of_int console_port;
		"console/ring-ref",  sprintf "%nu" console_mfn;
	] in
	let vm_stuff = [
		"rtc/timeoffset",    timeoffset;
	] in
	(* and finish domain's building *)
	build_post ~xc ~xs ~vcpus ~target_mib ~static_max_mib
		domid store_mfn store_port local_stuff vm_stuff

let restore ~xc ~xs info domid fd =
	let restore_fct = match info.priv with
	| BuildHVM hvminfo ->
		hvm_restore ~shadow_multiplier:hvminfo.shadow_multiplier
		  ~timeoffset:hvminfo.timeoffset ~xci_cpuid_signature:hvminfo.xci_cpuid_signature
	| BuildPV pvinfo   ->
		pv_restore
		in
	restore_fct ~xc ~xs
	            ~static_max_kib:info.memory_max ~target_kib:info.memory_target ~vcpus:info.vcpus
	            domid fd

let pause ~xc domid =
	Xc.domain_pause xc domid

let unpause ~xc domid =
	Xc.domain_unpause xc domid

let send_s3resume ~xc domid = Xc.domain_send_s3resume xc domid

let make_stubdom ~xc ~xs ~ioemuargs info uuid =
      let ssidref =
	      try
		      Xc.flask_context_to_sid xc "system_u:system_r:stubdom_t"
	      with exn ->
		      warn "flask failure : exception getting stubdom's ssidref: %s" (Printexc.to_string exn);
		      0l
      in
      let argsstr = List.fold_left (fun acc v -> acc ^ " " ^ v) "" ioemuargs in
      let createinfo =
		{ ssidref = ssidref
		; hvm = false
		; hap = false
		; name = sprintf "stubdom-%d" info.stubdom_target
		; xsdata = []
		; platformdata = [] } in
	let buildinfo =
		{ memory_max = info.stubdom_memory
		; memory_target = info.stubdom_memory
                ; memory_video = 0L
		; kernel = info.stubdom_kernel (* need path *)
		; vcpus = 1
		; priv = BuildPV
                       { cmdline = sprintf "%s %s" info.stubdom_cmdline argsstr
                       ; ramdisk = info.stubdom_initrd }
		} in
	let stubdom_domid = make ~xc ~xs createinfo uuid in
	try (
	build ~xc ~xs buildinfo stubdom_domid;

	(* write to the target where it can find it stubdom *)
	xs.Xs.write (xs.Xs.getdomainpath info.stubdom_target ^ "/image/device-model-domid") (sprintf "%d" stubdom_domid);
	(* write to the stubdom who's the target *)
	xs.Xs.write (xs.Xs.getdomainpath stubdom_domid ^ "/target") (sprintf "%d" info.stubdom_target);

	Xc.domain_set_target xc stubdom_domid info.stubdom_target;
	xs.Xs.set_target stubdom_domid info.stubdom_target;

	let perms = (stubdom_domid, Xsraw.PERM_NONE, [ (info.stubdom_target, Xsraw.PERM_READ) ]) in
	Xs.transaction xs (fun t ->
		let dmpath = sprintf "/local/domain/0/device-model/%d" info.stubdom_target in
		let vfspath = sprintf "/local/domain/%d/device/vfs" stubdom_domid in
		let devmiscpath = sprintf "/local/domain/%d/device-misc" info.stubdom_target in
		let dmspath = sprintf "/local/domain/%d/dms" info.stubdom_target in
		let dmapath = sprintf "/local/domain/%d/dm-agent" stubdom_domid in
		t.Xst.mkdir dmpath;
		t.Xst.setperms dmpath perms;
		t.Xst.mkdir vfspath;
		t.Xst.setperms vfspath perms;
		t.Xst.mkdir devmiscpath;
		t.Xst.setperms devmiscpath perms;
		t.Xst.mkdir dmspath;
		t.Xst.setperms dmspath perms;
		t.Xst.mkdir dmapath;
		t.Xst.setperms dmapath perms;
	);

	stubdom_domid
	) with exn ->	
		warn "Caught exception in stubdom %d creation: %s" stubdom_domid (Printexc.to_string exn);
		log_exn_continue "Xc.domain_destroy" (Xc.domain_destroy xc) stubdom_domid;
		raise exn
			
