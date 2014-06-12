(*
 * Copyright (c) 2009 Citrix Systems, Inc.
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

(* VM state *)
type vmstate =
	| VmCreated     | VmShutdown    | VmShuttingDown
	| VmPaused      | VmSuspending  | VmSuspended
	| VmRestoring   | VmRunning

(* errors *)
type severity = Warning | Critical | Fatal
type error = severity * string

(* failure codes *)
(* TODO: this should be a variant *)
type code = int


(* common rpc responses *)
type rpc_result =
	| Ok
	| Error of code * string

type op_result =
	| Op_started
	| Op_in_progress of float
	| Op_done
	| Op_failed of code * string

(* rpc specific types *)
type suspend_flag =
	| Suspend_flag_live
	| Suspend_flag_debug

type suspend_flags = suspend_flag list

type file_location =
	| File_local of string
	| File_nfs of string

type restore_flag =
	| Restore_flag_delete

type restore_flags = restore_flag list

type vnc = VNC of (* address *) string * (* port *) int

type device_type =
	| Devtype_PCI | Devtype_PBD | Devtype_PIF
	| Devtype_VIF | Devtype_VBD | Devtype_RTC

(* TODO: fix device types *)
type device =
	| Dev_PCI of string
	| Dev_PBD of string
	| Dev_PIF of string
	| Dev_VIF of string
	| Dev_VBD of string
	| Dev_RTC of string

type device_list = device list

type path = string
