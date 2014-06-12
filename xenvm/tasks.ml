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


type action =
	| Quit
	| Destroy
	| Halt
	| Reboot
	| Start
	| Pause
	| Unpause
	| Suspend
	| Restore
	| S3Suspend
	| S4Suspend
	| Checkpoint
	| GetDomid
	| GetStubDomid
	| GetStatus
	| GetAcpiState
	| GetVNC
	| Get
	| Set
	| AddDisk
	| AddNic
	| AddPCI
	| DelDisk
	| DelNic
	| DelPCI
	| ListDisk
	| ListNic
	| ListPCI
	| Trigger
	| Device
	| SetNicBackendDom
        | CDEject
        | CDInsertFile
        | CDInsertReal
        | NicNetworkSwitch
	| ReadConfig
	| SetMemTarget
	| Help

type argty =
	| ArgBool 
	| ArgString
	| ArgInt
	| ArgListString

type argval =
	| ValBool of bool
	| ValInt of int64
	| ValString of string
	| ValListString of string list

type argreq =
	| R           (* required *)
	| O           (* optional *)
	| D of argval (* default *)

type task_descr = {
	name: string;
	description: string;
(*	vmstate: ;*)
	need_threading: bool;
	args: (string * argreq * argty) list; (* name, required, type *)
}

let mk_desc ?(t=true) name = { name = name; description = ""; need_threading = t; args = [] }
let mk_desc_nb name = { name = name; description = ""; need_threading = false; args = [] }
let mk_desc_args ?(t=true) name args = { name = name; description = ""; need_threading = t; args = args }
let mk_desc_args_nb name args = { name = name; description = ""; need_threading = false; args = args }

let actions_table = [
	(Quit,       mk_desc "quit");
	(Destroy,    mk_desc "destroy");
	(Halt,       mk_desc_args "halt" [ ("forced", D (ValBool false), ArgBool) ] );
	(Reboot,     mk_desc_args "reboot" [ ("forced", D (ValBool false), ArgBool); ("auto-start", D (ValBool true), ArgBool) ] );
	(Start,      mk_desc_args "start" [ ("paused", D (ValBool false), ArgBool) ]);
	(Pause,      mk_desc "pause");
	(Unpause,    mk_desc "unpause");
	(Suspend,    mk_desc_args "suspend" [ ("live", D (ValBool false), ArgBool);
	                                      ("file", R, ArgString); ] );
	(Restore,    mk_desc_args "restore" [ ("file", R, ArgString); ("paused", D (ValBool false), ArgBool) ] );
	(S3Suspend,  mk_desc_args "s3suspend" [ "timeout", O, ArgInt ]);
	(S4Suspend,  mk_desc_args "s4suspend" [ "timeout", O, ArgInt ]);
	(GetDomid,   mk_desc_nb "get-domid");
	(GetStubDomid,   mk_desc_nb "get-stubdomid");
	(GetStatus,  mk_desc_nb "get-status");
        (GetAcpiState, mk_desc_nb "get-acpi-state");

	(GetVNC,     mk_desc_nb "get-vnc");
	(Trigger,    mk_desc_args "trigger" [ "params", R, ArgString ]);
	(Device,     mk_desc_args "device" [ "type", R, ArgString;
	                                     "cmd", R, ArgString;
	                                     "extra", O, ArgListString; ]);
	(Get,        mk_desc_args_nb "get" [ "field", R, ArgString; ]);
	(Set,        mk_desc_args_nb "set" [ "field", R, ArgString; "value", R, ArgString ]);
	(AddDisk,    mk_desc_args "disk-add" [ "path", R, ArgString; "device", R, ArgString;
	                                       "type", R, ArgString; "mode", R, ArgString;
	                                       "devtype", R, ArgString ]);
	(AddNic,     mk_desc_args "nic-add" [ "bridge", O, ArgString; "mac", O, ArgString; "model", O, ArgString ]);
	(AddPCI,     mk_desc_args "pci-add" [ "id", R, ArgInt; "domain", R, ArgInt; "bus", R, ArgInt;
	                                      "slot", R, ArgInt; "func", R, ArgInt;
	                                      "msitranslate", O, ArgBool; "power_mgmt", O, ArgBool ]);

	(CDEject,    mk_desc_args "cd-eject" [ "virtpath", R, ArgString; ]);
	(CDInsertFile,mk_desc_args "cd-insert-file" [ "virtpath", R, ArgString; 
				                      "physpath", R, ArgString ]);
	(CDInsertReal,mk_desc_args "cd-insert-real" [ "virtpath", R, ArgString;
                                                      "physpath", R, ArgString ]);
	(NicNetworkSwitch,mk_desc_args "nic-network-switch" [ "id", R, ArgInt;
				                              "network", R, ArgString ]);

		    

	(ReadConfig, mk_desc_args_nb "read-config" [ "path", O, ArgString ]);

	(DelDisk,    mk_desc "disk-del");
	(DelNic,     mk_desc "nic-del");
	(DelPCI,     mk_desc "pci-del");
	(ListDisk,   mk_desc_nb "disk-list");
	(ListNic,    mk_desc_nb "nic-list");
	(ListPCI,    mk_desc_nb "pci-list");
	(SetMemTarget, mk_desc_args "set-mem-target" [ "mb", R, ArgInt ]);
	(SetNicBackendDom, mk_desc_args "set-nic-backend-dom" [ "id", R, ArgInt; "domid", R, ArgInt ]);
	(Help,       mk_desc_nb "help");
	(*
	(Checkpoint, ("checkpoint", Some VmRunning));
	*)
]

exception Invalid_type_registered of string
exception Argument_not_found of string
exception Task_not_found of string

let find name =
	try List.find (fun p -> (snd p).name = name) actions_table
	with Not_found -> raise (Task_not_found name)

let args_assoc name args =
	try List.assoc name args
	with Not_found -> raise (Argument_not_found name)

let args_get_string args name =
	match args_assoc name args with
	| ValString s -> s
	| _           -> raise (Invalid_type_registered name)

let args_get_int args name =
	match args_assoc name args with
	| ValInt b    -> b
	| ValString s -> (try Int64.of_string s with exn -> raise (Invalid_type_registered name))
	| _           -> raise (Invalid_type_registered name)

let args_get_bool args name =
	match args_assoc name args with
	| ValBool b   -> b
	| ValString s -> (try bool_of_string s with exn -> raise (Invalid_type_registered name))
	| _           -> raise (Invalid_type_registered name)

let args_get_liststring args name =
	match args_assoc name args with
	| ValListString l -> l
	| ValString s     -> [ s ]
	| _               -> raise (Invalid_type_registered name)
