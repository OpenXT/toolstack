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

exception Ioemu_failed of string
exception Ioemu_failed_dying

type disp_opt =
	| NONE
	| VNC of bool * string * int * string (* auto-allocate, bind address, port it !autoallocate, keymap *)
	| SDL of string (* X11 display *)

type info = {
	hvm: bool;
	memory: int64;
	boot: string;
	serial: string;
	vcpus: int;
	usb: string list;
	(* mac * (network * bridge) * model * is_wireless * id *)
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
	vifs: (int * Device.Vif.info) list;
	pcis: (int * Device.PCI.dev list) list;
	stubdom: ([`domain] Uuid.t) option;
	stubdom_memory: int64;
	stubdom_kernel: string;
	stubdom_initrd: string option;
	stubdom_cmdline: string;
}

val write_logfile_to_log : int -> unit
val unlink_logfile : int -> unit

val vnc_port_path : Xc.domid -> string

val signal : xs:Xs.xsh -> domid:Xc.domid -> ?wait_for:string -> ?param:string
	  -> string -> unit

val start : xc:Xc.handle -> xs:Xs.xsh -> dmpath:string -> ?timeout:float -> info -> Xc.domid -> (int option) * (int option)
val restore : xc:Xc.handle -> xs:Xs.xsh -> dmpath:string -> ?timeout:float -> info -> Xc.domid -> (int option) * (int option)

val suspend : xs:Xs.xsh -> Xc.domid -> unit
val resume : xs:Xs.xsh -> Xc.domid -> unit
val stop : xs:Xs.xsh -> Xc.domid -> int -> unit

(* Will be used in Dmagent module *)
val prepare_domain : xs:Xs.xsh -> info -> Xc.domid -> unit
val create_dm_stubdom : xc:Xc.handle -> xs:Xs.xsh -> string list -> info -> Xc.domid -> [`domain] Uuid.t -> Xc.domid
val fork_dm_helpers : xs:Xs.xsh -> bool -> Xc.domid -> unit
