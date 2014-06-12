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
module D = Debug.Debugger(struct let name = "netman" end)
open D


type objpath = string
type netty = Bridge of objpath * string | DriverDomain of objpath * string | Nat

let log_exn name f =
	try f ()
	with exn ->
		warn "exception during %s: %s" name (Printexc.to_string exn)

let networkid_of_netty netty =
  match netty with
	| Bridge (objpath, bridgename) -> Some objpath
	| DriverDomain (objpath, bridgename) -> Some objpath
	| _ -> None

let online ~xs backend_path vif netty =
	(* Network Daemon expects the dbus object path in /bridge xenstore node 
	 * todo: maybe appropriate renames? *)
	match networkid_of_netty netty with
	| Some objpath -> xs.Xs.write (backend_path ^ "/bridge") objpath
	| None -> failwith "not supported yet"

let offline vif netty =
	match netty with
	| Bridge _ -> ()
	| DriverDomain _ -> ()
	| _                 ->
		failwith "not supported yet"
