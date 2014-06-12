(*
 * Copyright (C) 2006-2007 XenSource Ltd.
 * Copyright (C) 2008      Citrix Ltd.
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

open Pervasiveext
open Stringext

type query = (string * ((string * string) list))
type answer = Ok | Error of string | Msg of string | Unknown of string | Timeout

exception Partial_Write
exception Partial_Read
exception Write_timeout
exception Read_timeout
exception Connect_refused of string

module Socket = struct

let path_of_socket id =
	let dir = try Sys.getenv "XENVM_SOCKET_DIR" with Not_found -> "/var/run/xenvm" in
	Printf.sprintf "%s/vm-%s" dir id

let connect id =
	let filename = path_of_socket id in
	let fd = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
	begin try
		Unix.connect fd (Unix.ADDR_UNIX filename);
	with Unix.Unix_error (error, _, _) ->
		raise (Connect_refused (Unix.error_message error))
	end;
	fd

let dowrite ?(timeout=(-1.0)) fd buf =
	let len = String.length buf in
	let left = ref len in
	while !left > 0
	do
		let _, w, _ =
			try Unix.select [] [ fd ] [] timeout
			with _ -> [], [], []
			in
		if not (List.mem fd w) then
			raise Write_timeout;

		let wr = Unix.write fd buf (len - !left) (!left) in
		if wr = 0 then
			raise Partial_Write;
		left := !left - wr
	done

let doread_eof ?(timeout=(-1.0)) fd =
	let buf = Buffer.create 1024 in
	let s = String.create 1024 in
	let quit = ref false in
	while not !quit
	do
		let r, _, _ =
			try Unix.select [ fd ] [] [] timeout
			with _ -> [], [], []
			in
		if not (List.mem fd r) then
			raise Read_timeout;

		let rd = Unix.read fd s 0 (String.length s) in
		if rd = 0 then
			quit := true
		else (
			Buffer.add_substring buf s 0 rd
		)
	done;
	Buffer.contents buf

let doread ?(timeout=(-1.0)) fd len =
	let buf = String.create len in
	let offset = ref 0 in
	let left = ref len in
	while !left > 0
	do
		let r, _, _ =
			try Unix.select [ fd ] [] [] timeout
			with _ -> [], [], []
			in
		if not (List.mem fd r) then
			raise Read_timeout;

		let rd = Unix.read fd buf !offset !left in
		if rd = 0 then
			raise Partial_Read;
		offset := !offset + rd;
		left := !left - rd;
	done;
	buf

let send_query ?timeout fd query =
	let query_name, query_params = query in
	let jparams = Json.Object (Array.of_list (List.map (fun (k, v) -> k, Json.String v) query_params)) in
	let req = {
		Jsonrpc.request_id = Some (Json.Int 1L);
		Jsonrpc.method_name = query_name;
		Jsonrpc.params = jparams;
	} in
	let s = Jsonrpc.request_to_string req in
	let len = String.length s in
	let header = String.create 4 in

	(* make header *)
	let char_of_int len shift = Char.chr ((len lsr shift) land 0xff) in
	header.[0] <- char_of_int len 24;
	header.[1] <- char_of_int len 16;
	header.[2] <- char_of_int len 8;
	header.[3] <- char_of_int len 0;

	dowrite ?timeout fd header;
	dowrite ?timeout fd s;
	()

let recv_resp ?timeout fd =
	let s = doread_eof ?timeout fd in
	let jsonrpc = Jsonrpc.response_of_string s in
	match jsonrpc.Jsonrpc.response with
	| Jsonrpc.Result r -> (
		match r with
		| Json.Null     -> Ok
		| Json.String m -> Msg m
		| _             -> Error "unknown return value in success"
		)
	| Jsonrpc.Error (code, message, optdata) ->
		Error message

let request ?timeout id query =
	let fd = connect id in
	finally (fun () ->
		send_query ?timeout fd query;
		recv_resp ?timeout fd
	) (fun () -> Unix.close fd)

let check ?timeout id =
	try
		let reply = request ?timeout id ("status", []) in
		begin match reply with
		| Msg _ -> ()
		| _     -> ()
		end;
		true
	with Connect_refused s ->
		false

end

module Bus = struct

let connect () =
	let using_session = try bool_of_string (Sys.getenv "XENVMLIB_DBUS_SESSION") with _ -> false in
	DBus.Bus.get (if using_session then DBus.Bus.Session else DBus.Bus.System)

let request ?timeout id query =
	let bus = connect () in
	let timeout = match timeout with None -> 0 | Some t -> int_of_float (t *. 1000.) in

	let intf = Printf.sprintf "org.xen.vm.uuid_%s" (String.replace "-" "_" id) in
	let dest = intf in

	let method_name, params = query in
	let method_name = String.replace "-" "_" method_name in
	let msg = DBus.Message.new_method_call dest "/" intf method_name in
	let params = List.map (fun (k, v) -> DBus.String k, DBus.String v) params in
	DBus.Message.append msg [ DBus.Array (DBus.Dicts ((DBus.SigString, DBus.SigString), params)) ];
	let reply = DBus.Connection.send_with_reply_and_block bus msg timeout in
	let args = DBus.Message.get reply in
	match DBus.Message.get_type reply, args with
	| DBus.Message.Error, [ DBus.String s ] -> Error s
	| DBus.Message.Error, _                 -> Error "error with unexpected arguments"
	| DBus.Message.Method_return, []        -> Ok
	| DBus.Message.Method_return, [ DBus.String s ] -> Msg s
	| _                          -> assert false

let check ?timeout id =
	let bus = connect () in
	let intf = Printf.sprintf "org.xen.vm.uuid_%s" (String.replace "-" "_" id) in
	DBus.Bus.has_owner bus intf
end

let request ?(using_socket=false) ?timeout id query =
	if using_socket
	then Socket.request ?timeout id query
	else Bus.request ?timeout id query

let check ?(using_socket=false) ?timeout id =
	if using_socket
	then Socket.check ?timeout id
	else Bus.check ?timeout id

let code_ping = 0x0000
let code_hup = 0x0001
let code_error = 0x0002
let code_vmset = 0x1000
let code_vmtrigger = 0x2000
let code_vmstate = 0xf000
