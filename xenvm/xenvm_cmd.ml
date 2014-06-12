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

open Printf

let kvpair s =
	match Stringext.String.split ~limit:2 '=' s with
	| k :: v :: [] -> Some (k, v)
	| _            -> None

let valid_kvpairs args =
	List.rev (List.fold_left (fun acc x -> match kvpair x with Some x -> x :: acc | None -> acc) [] args)

let _ =
	let using_socket = ref false in
	let timeout = ref 60 in
	let usage_msg = sprintf "usage: %s [--use-socket] [--reply-timeout <secs>] <uuid> <cmd> [cmd args]\n" Sys.argv.(0) in
	let args = ref [] in
	Arg.parse [
		("--use-socket", Arg.Set using_socket, "use socket instead of dbus");
		("--reply-timeout", Arg.Int (fun t -> timeout := t), "set the timeout (in seconds), defaults to 60");
	] (fun s -> args := s :: !args) usage_msg;
	let args = List.rev !args in

	let using_socket = !using_socket in
	let uuid, query =
		match args with
		| uuid :: cmd :: args -> uuid, (cmd, valid_kvpairs args)
		| uuid :: []          -> eprintf "error: missing query\n%s" usage_msg; exit 1
		| []                  -> eprintf "error: missing uuid\n%s" usage_msg; exit 1
		in

	try
		match Xenvmlib.request ~using_socket ~timeout:(float_of_int !timeout) uuid query with
		| Xenvmlib.Ok          -> ()
		| Xenvmlib.Timeout     -> eprintf "timeout\n"; exit 1
		| Xenvmlib.Error error -> eprintf "error: %s\n" error; exit 1
		| Xenvmlib.Msg msg     -> printf "%s\n" msg
		| Xenvmlib.Unknown s   -> eprintf "warning: unknown answer: \"%s\"\n" s
	with
	| Xenvmlib.Write_timeout -> eprintf "cannot send command to xenvm. it is dead ?\n"; exit 1
	| exn                    -> eprintf "receive exception: %s\n" (Printexc.to_string exn); exit 1
