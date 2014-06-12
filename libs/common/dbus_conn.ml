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

type t =
{
	ev_loop : Eventloop.t;
	ev_handle : Eventloop.handle;
	ev_fd : Unix.file_descr;

	bus : DBus.bus;
	callbacks : callbacks;

	mutable watches  : DBus.watch list;
	mutable timeouts : (DBus.timeout * Eventloop.timer) list;
	mutable inactive_timeouts : DBus.timeout list;
}

and callbacks =
{
	msg_received_callback : t -> DBus.message -> unit;
	error_callback : t -> Eventloop.error -> unit;
}

module Conns = Connection_table.Make(struct type conn = t end)

let send conn msg =
	DBus.Connection.send conn.bus msg

let enable_recv conn =
	Eventloop.enable_recv conn.ev_loop conn.ev_handle

let disable_recv conn =
	Eventloop.disable_recv conn.ev_loop conn.ev_handle

let dispatch conn =
	let rec aux () = 
		match (DBus.Connection.dispatch conn.bus) with
		| DBus.Connection.Data_remains -> aux ()
		| _ -> ()
	in aux ()

let toggle_watch_callback conn watch =
	(* Only handle enables here; disables are handled during event dispatch. *)
	let flags = DBus.Watch.get_flags watch in
	if List.mem DBus.Watch.Readable flags then
		Eventloop.enable_recv conn.ev_loop conn.ev_handle;
	if List.mem DBus.Watch.Writable flags then
		Eventloop.enable_send conn.ev_loop conn.ev_handle

let add_watch_callback conn watch =
	conn.watches <- watch :: conn.watches;
	toggle_watch_callback conn watch;
	true

let remove_watch_callback conn watch =
	conn.watches <- List.filter (fun w -> w != watch) conn.watches

let remove_timeout_callback conn timeout =
	(* Scan both lists to increase robustness (instead
	   of using the timeout state to select the list). *)
	let updated_timeouts =
		List.fold_left (fun acc (t, h) ->
					if t == timeout then begin
						Eventloop.cancel_timer conn.ev_loop h;
						acc
					end else
						(t, h) :: acc
			       ) [] conn.timeouts
	in
	conn.timeouts <- updated_timeouts;
	conn.inactive_timeouts <- List.filter (fun t -> t != timeout) conn.inactive_timeouts

let timeout_handler conn timeout () =
	(* We cannot use remove_timeout_callback here since the
	   eventloop handle is now invalid. *)
	let updated_timeouts =
		List.fold_left (fun acc (t,h) ->
					if t == timeout then acc else (t,h) :: acc
			       ) [] conn.timeouts
	in
	conn.timeouts <- updated_timeouts;
	remove_timeout_callback conn timeout;
	DBus.Timeout.handle timeout

let add_timeout_callback conn timeout =
	if DBus.Timeout.get_enabled timeout then
		let expiry = DBus.Timeout.get_interval timeout in
		(* Eventloop timers are currently in float seconds,
		   whereas DBus timeouts are in int milliseconds.  *)
		let expiry = (float_of_int expiry) /. 1000. in
		let th = Eventloop.start_timer conn.ev_loop expiry (timeout_handler conn timeout) in
		conn.timeouts <- (timeout, th) :: conn.timeouts
	else
		conn.inactive_timeouts <- timeout :: conn.inactive_timeouts;
	true

let toggle_timeout_callback conn timeout =
	(* There is little need to optimize this, since we can
	   assume that DBus uses this only when the state of the
	   timer has changed. *)
	remove_timeout_callback conn timeout;
	ignore (add_timeout_callback conn timeout)

let recv_ready_callback el h fd =
	let conn = Conns.get_conn h in
	(* Since the set of watches might be modified during the
	   callbacks, we need to check if the watches are still
	   active.  Also, keep track whether a watch was dispatched;
	   if none was, we need to disable the event. *)
	let can_dispatch w =
		((DBus.Watch.get_enabled w)
		 && (List.mem DBus.Watch.Readable (DBus.Watch.get_flags w))) in
	let dispatched = ref false in
	let watches = conn.watches in
	List.iter (fun w ->
			if List.memq w conn.watches && can_dispatch w
			then begin
				dispatched := true;
				DBus.Watch.handle w [ DBus.Watch.Readable ]
			end
		  ) watches;
	if not !dispatched then
		Eventloop.disable_recv el h
	else
		dispatch conn

let send_ready_callback el h fd =
	let conn = Conns.get_conn h in
	(* Since the set of watches might be modified during the
	   callbacks, we need to check if the watches are still
	   active.  Also, keep track whether a watch was dispatched;
	   if none was, we need to disable the event. *)
	let can_dispatch w =
		((DBus.Watch.get_enabled w)
		 && (List.mem DBus.Watch.Writable (DBus.Watch.get_flags w))) in
	let dispatched = ref false in
	let watches = conn.watches in
	List.iter (fun w ->
			if List.memq w conn.watches && can_dispatch w
			then begin
				dispatched := true;
				DBus.Watch.handle w [ DBus.Watch.Writable ]
			end
		  ) watches;
	if not !dispatched then
		Eventloop.disable_send el h

let error_callback el h err =
	let conn = Conns.get_conn h in
	conn.callbacks.error_callback conn err

let db_callbacks =
{
	Eventloop.accept_callback = (fun _ _ _ _ -> assert false);
	Eventloop.connect_callback = (fun _ _ -> assert false);
	Eventloop.recv_ready_callback = recv_ready_callback;
	Eventloop.send_ready_callback = send_ready_callback;
	Eventloop.error_callback = error_callback;
}

let attach bus ev_loop callbacks =
	let fd = DBus.Connection.get_fd bus in
	let ev_handle = Eventloop.register_conn ev_loop fd ~enable_send:false ~enable_recv:false db_callbacks in
	let conn = { ev_loop = ev_loop;
		     ev_handle = ev_handle;
		     ev_fd = fd;
		     bus = bus;
		     watches = [];
		     timeouts = [];
		     inactive_timeouts = [];
		     callbacks = callbacks;
		   } in
	let add_watch_fn = add_watch_callback conn in
	let rm_watch_fn = remove_watch_callback conn in
	let toggle_watch_fn = toggle_watch_callback conn in
	let add_timeout_fn = add_timeout_callback conn in
	let rm_timeout_fn = remove_timeout_callback conn in
	let toggle_timeout_fn = toggle_timeout_callback conn in
	let filter _ msg = (callbacks.msg_received_callback conn msg; true) in
	DBus.Connection.set_watch_functions bus (add_watch_fn, rm_watch_fn, Some toggle_watch_fn);
	DBus.Connection.set_timeout_functions bus (add_timeout_fn, rm_timeout_fn, Some toggle_timeout_fn);
	DBus.Connection.add_filter bus filter;
	Conns.add_conn ev_handle conn;
	conn

let detach conn =
	Conns.remove_conn conn.ev_handle;
	Eventloop.remove_conn conn.ev_loop conn.ev_handle
