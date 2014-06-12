(*
 * Copyright (C) 2009      Citrix Ltd.
 * Author Prashanth Mundkur <firstname.lastname@citrix.com>
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

module D = Debug.Debugger (struct let name = "jsonrpc_server" end)
open D

(* Default state for incoming connections: we just parse until we get
   our first message.  We then call each of the protocol recognizers
   to check if they will take over the connection.
*)
type unknown_conn_state =
{
	mutable json_parser: Json_parse.parse_state;
}

type conn_type =
  | Unknown of unknown_conn_state
  | Listening

type conn_state =
{
	conn_fd: Unix.file_descr;
	conn_handle: Eventloop.handle;
	conn_evloop: Eventloop.t;
	conn_type: conn_type;
}

module Conns = Connection_table.Make(struct type conn = conn_state end)

let cleanup_and_close conn =
	Conns.cleanup_and_close conn.conn_evloop conn.conn_handle conn.conn_fd

(* Protocol recognition. *)

type recognizer = Json.t -> bool
type acceptor = Eventloop.t -> Eventloop.handle -> Json.t -> (string * int * int) -> unit

let known_protocols = ref ([] : (recognizer * acceptor) list)

let classify_conn conn msg remainder =
	let recognized =
		List.fold_left (fun recognized (recognizer, acceptor) ->
					if recognized then recognized
					else if recognizer msg then begin
						Conns.remove_conn conn.conn_handle;
						acceptor conn.conn_evloop conn.conn_handle msg remainder;
						true
					end else false
			       ) false !known_protocols
	in
	if not recognized then begin
		error "Unrecognized message \"%s\", dropping connection.\n" (Json.to_string msg);
		cleanup_and_close conn
	end

let get_unknown_callbacks ustate =
	let recv_callback ustate el h str off len =
		let conn = Conns.get_conn h in
		try
			match Json_parse.parse_substring ustate.json_parser str off len with
			| Json_parse.Json_parse_incomplete pstate ->
				ustate.json_parser <- pstate
			| Json_parse.Json_value (j, nconsumed) ->
				if Jsonrpc.is_jsonrpc_value j then
					classify_conn conn j (str, off + nconsumed, len - nconsumed)
				else begin
					error "Non-JSONRPC message received, closing connection\n";
					cleanup_and_close conn
				end
		
		with
		| Json_parse.Parse_error e ->
			error "Json_parse error: %s\n" (Json_parse.string_of_error e);
			cleanup_and_close conn
		| Jsonrpc.Invalid_request e ->
			error "%s\n" (Jsonrpc.string_of_req_error e);
			cleanup_and_close conn
		| Jsonrpc.Invalid_response e ->
			error "Unexpected jsonrpc response, with error: %s"
				(Jsonrpc.string_of_resp_error e);
			cleanup_and_close conn in
	let shutdown_callback el h =
		let conn = Conns.get_conn h in
		warn "Close received on unknown connection %d.\n"
			(Unixext.int_of_file_descr conn.conn_fd);
		cleanup_and_close conn in
	let error_callback el h (code, f, m) =
		let conn = Conns.get_conn h in
		warn "Error on unknown connection %d: %s in %s %s!\n"
			(Unixext.int_of_file_descr conn.conn_fd)
			(Unix.error_message code) f m;
		cleanup_and_close conn
	in
	{
		Eventloop.recv_callback = recv_callback ustate;
		Eventloop.shutdown_callback = shutdown_callback;
		Eventloop.error_callback = error_callback;

		(* We don't expect these callbacks. *)
		Eventloop.accept_callback = (fun _ _ _ _ -> assert false);
		Eventloop.connect_callback = (fun _ _ -> assert false);
		Eventloop.send_done_callback = (fun _ _ -> assert false);
	}

(* Listening connections *)

let get_listen_callbacks () =
	let accept_callback el h fd sa =
		(* When connections are first accepted, we don't what
		   protocol they will be using.
		*)
		let ustate = { json_parser = Json_parse.init_parse_state () } in
		let callbacks = get_unknown_callbacks ustate in
		let h = Eventloop.register_conn el fd callbacks in
		let conn = { conn_fd = fd;
			     conn_handle = h;
			     conn_evloop = el;
			     conn_type = Unknown ustate }
		in
		Conns.add_conn h conn in
	let error_callback el h (code, f, m) =
		let conn = Conns.get_conn h in
		error "Error on listening socket %d: %s in %s %s!\n"
			(Unixext.int_of_file_descr conn.conn_fd)
			(Unix.error_message code) f m;
		Conns.cleanup_and_close el h conn.conn_fd
	in
	{
		Eventloop.accept_callback = accept_callback;
		Eventloop.error_callback = error_callback;
		(* We should never get the below callbacks. *)
		Eventloop.connect_callback = (fun _ _ -> assert false);
		Eventloop.recv_callback = (fun _ _ _ _ _ -> assert false);
		Eventloop.send_done_callback = (fun _ _ -> assert false);
		Eventloop.shutdown_callback = (fun _ _ -> assert false);
	}

(* Main event loop *)

let quit = ref false

let stop_server () =
	quit := true

let start_server ?setup_callback protocols listen_sockets =
	known_protocols := protocols;
	let el = Eventloop.create () in
	List.iter (fun fd ->
			let h = Eventloop.register_conn el fd (get_listen_callbacks ()) in
			let conn = { conn_fd = fd;
				     conn_handle = h;
				     conn_evloop = el;
				     conn_type = Listening }
			in
			Conns.add_conn h conn;
			Eventloop.listen el h
		  ) listen_sockets;
	(match setup_callback with
	 | Some f -> f el
	 | None -> ()
	);
	while (not !quit
	       && ((Eventloop.num_connections el > 0) || (Eventloop.num_timers el > 0)))
	do
		Eventloop.dispatch el 1.0
        done
