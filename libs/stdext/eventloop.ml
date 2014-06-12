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

let verbose = ref false

let dbg fmt =
	let logger s = if !verbose then Printf.printf "%s\n%!" s in
	Printf.ksprintf logger fmt

module ConnMap = Map.Make (struct type t = Unix.file_descr let compare = compare end)
	
(* A module that supports finding a timer by handle as well as by expiry time. *)
module Timers = struct

	type 'a entry =
	{
		handle : int;
		mutable expires_at: float; 
		value: 'a;
	}

	module Timers_by_expiry = Map.Make (struct type t = float let compare = compare end)

	type 'a t =
	{
		mutable by_expiry: (('a entry) list) Timers_by_expiry.t;
	}

	let create () = { by_expiry = Timers_by_expiry.empty }

	let is_empty t = Timers_by_expiry.is_empty t.by_expiry

	let next_handle = ref 0

	(** inserts an existing (but not inserted) entry in the map *)
	let submit_timer t at e = 
		e.expires_at <- at;
		let es = try Timers_by_expiry.find e.expires_at t.by_expiry with Not_found -> [] in
		t.by_expiry <- Timers_by_expiry.add e.expires_at (e :: es) t.by_expiry
		
	let add_timer t at v =
		incr next_handle;
		let e = { handle = !next_handle; expires_at = at; value = v } in
		submit_timer t at e;
		e

	let remove_timer t entry =
		let handle = entry.handle in
		let es = Timers_by_expiry.find entry.expires_at t.by_expiry in
		let es = List.filter (fun e' -> e'.handle <> handle) es in
		t.by_expiry <- (match es with
				| [] -> Timers_by_expiry.remove entry.expires_at t.by_expiry
				| _  -> Timers_by_expiry.add entry.expires_at es t.by_expiry
			       )

	exception Found of float

	(* Should only be called on a non-empty Timer set; otherwise,
	   Not_found is raised. *)
	let get_first_expiry_time t =
		try
			(* This should give the earliest expiry time,
			   since iteration is done in increasing order.	*)
			Timers_by_expiry.iter (fun tim -> raise (Found tim)) t.by_expiry;
			raise Not_found
		with Found tim -> tim

	(* Extracts the timers for time t, and return a list of values for those timers *)
	let extract_timers_at t tim =
		try
			let es = Timers_by_expiry.find tim t.by_expiry in
			t.by_expiry <- Timers_by_expiry.remove tim t.by_expiry;
			List.map (fun e -> e.value) es
		with Not_found -> []

end

type error = Unix.error * string * string

type handle = Unix.file_descr

let handle_compare = compare
let handle_hash h = Unixext.int_of_file_descr h

type conn_status =
	| Connecting
	| Listening
	| Connected

type conn_callbacks =
{
	accept_callback : t -> handle -> Unix.file_descr -> Unix.sockaddr -> unit;
	connect_callback : t -> handle -> unit;
	error_callback : t -> handle -> error -> unit;
	recv_ready_callback : t -> handle -> Unix.file_descr -> unit;
	send_ready_callback : t -> handle -> Unix.file_descr -> unit;
}

and conn_state =
{
	mutable callbacks : conn_callbacks;
	mutable status : conn_status;
	mutable send_enabled : bool;
	mutable recv_enabled : bool;
}

and t =
{
	mutable conns: conn_state ConnMap.t;
	mutable timers: (unit -> unit) Timers.t;
	(* select state *)
	readers: Unixext.Fdset.t;
	writers: Unixext.Fdset.t;
	excepts: Unixext.Fdset.t;
	(* dispatch state *)
	mutable d_readers: Unixext.Fdset.t;
	mutable d_writers: Unixext.Fdset.t;
	(** Unix.gettimeofday() at the time the loop iteration started *)
	mutable current_time: float;
}

let create () =
{	conns = ConnMap.empty;
	timers = Timers.create ();
	readers = Unixext.Fdset.create ();
	writers = Unixext.Fdset.create ();
	excepts = Unixext.Fdset.create ();
	d_readers = Unixext.Fdset.create ();
	d_writers = Unixext.Fdset.create ();
	current_time = 0.0;
}

(* connections *)

let register_conn t fd ?(enable_send=false) ?(enable_recv=true) callbacks =
	let conn_state = { callbacks = callbacks;
			   status = Connected;
			   send_enabled = enable_send;
			   recv_enabled = enable_recv;
			 }
	in
	t.conns <- ConnMap.add fd conn_state t.conns;
	Unix.set_nonblock fd;
	if conn_state.recv_enabled then
		Unixext.Fdset.set t.readers fd;
	if conn_state.send_enabled then
		Unixext.Fdset.set t.writers fd;
	fd

let remove_conn t handle =
	Unixext.Fdset.clear t.readers handle;
	Unixext.Fdset.clear t.writers handle;
	(* Also remove this handle from the set we might be
	   dispatching over. *)
	Unixext.Fdset.clear t.d_readers handle;
	Unixext.Fdset.clear t.d_writers handle;
	t.conns <- ConnMap.remove handle t.conns

let get_fd t handle = handle

let connect t handle addr =
	let conn_state = ConnMap.find handle t.conns in
	conn_state.status <- Connecting;
	try
		Unix.connect handle addr;
		conn_state.status <- Connected;
		conn_state.callbacks.connect_callback t handle
	with
	| Unix.Unix_error (Unix.EINPROGRESS, _, _) ->
		Unixext.Fdset.set t.readers handle;
		Unixext.Fdset.set t.writers handle
	| Unix.Unix_error (ec, f, s) ->
		conn_state.callbacks.error_callback t handle (ec, f, s)

let listen t handle =
	let conn_state = ConnMap.find handle t.conns in
	Unix.listen handle 5;
	Unixext.Fdset.set t.readers handle;
	conn_state.recv_enabled <- true;
	conn_state.status <- Listening

let enable_send t handle =
	let conn_state = ConnMap.find handle t.conns in
	conn_state.send_enabled <- true;
	if conn_state.status = Connected then
		Unixext.Fdset.set t.writers handle

let disable_send t handle =
	let conn_state = ConnMap.find handle t.conns in
	conn_state.send_enabled <- false;
	if conn_state.status = Connected then
		Unixext.Fdset.clear t.writers handle

let enable_recv t handle =
	let conn_state = ConnMap.find handle t.conns in
	conn_state.recv_enabled <- true;
	if conn_state.status = Connected then
		Unixext.Fdset.set t.readers handle

let disable_recv t handle =
	let conn_state = ConnMap.find handle t.conns in
	conn_state.recv_enabled <- false;
	if conn_state.status = Connected then
		Unixext.Fdset.clear t.readers handle

let set_callbacks t handle callbacks =
	let conn_state = ConnMap.find handle t.conns in
	conn_state.callbacks <- callbacks

let has_connections t = not (ConnMap.is_empty t.conns)

(* timers *)

type timer = (unit -> unit) Timers.entry

let start_timer t time_offset_sec cb =
	let at = Unixext.gettimeofday_monotonic () +. time_offset_sec in
	Timers.add_timer t.timers at cb

let start_timer_asap t cb =
	Timers.add_timer t.timers t.current_time cb

let start_periodic_timer t time_offset_sec period cb =
	let orig_timer = ref (None: timer option) in
	let resubmit_timer_closure () = 
		let orig_timer = match !orig_timer with None -> raise Not_found | Some x -> x in
		Timers.submit_timer t.timers (t.current_time +. period) orig_timer;
		cb (); (* invoke the user's callback *)
	in
	let new_timer = start_timer t time_offset_sec resubmit_timer_closure in
	orig_timer := Some (new_timer);
	new_timer
	
let cancel_timer t timer =
	Timers.remove_timer t.timers timer

let timer_compare tim1 tim2 = compare tim1.Timers.handle tim2.Timers.handle
let timer_hash tim = tim.Timers.handle

let has_timers t = not (Timers.is_empty t.timers)

(* event dispatch *)

let dispatch_read t fd cs =
	match cs.status with
	| Connecting ->
		(match Unix.getsockopt_error fd with
		| None ->
			cs.status <- Connected;
			if not cs.recv_enabled then
				Unixext.Fdset.clear t.readers fd;
			if not cs.send_enabled then
				Unixext.Fdset.clear t.writers fd;
			cs.callbacks.connect_callback t fd
		| Some err ->
			cs.callbacks.error_callback t fd (err, "connect", "")
		)
	| Listening ->
		(try
			let afd, aaddr = Unix.accept fd in
			cs.callbacks.accept_callback t fd afd aaddr
		 with
		 | Unix.Unix_error (Unix.EWOULDBLOCK, _, _)
		 | Unix.Unix_error (Unix.ECONNABORTED, _, _)
		 | Unix.Unix_error (Unix.EINTR, _, _)
		   -> ()
		 | Unix.Unix_error (ec, f, s) ->
			cs.callbacks.error_callback t fd (ec, f, s)
		)
	| Connected ->
		if cs.recv_enabled
		then cs.callbacks.recv_ready_callback t fd fd
		else Unixext.Fdset.clear t.readers fd

let dispatch_write t fd cs =
	match cs.status with
	| Connecting ->
		(match Unix.getsockopt_error fd with
		| None ->
			cs.status <- Connected;
			if not cs.recv_enabled then
				Unixext.Fdset.clear t.readers fd;
			if not cs.send_enabled then
				Unixext.Fdset.clear t.writers fd;
			cs.callbacks.connect_callback t fd
		| Some err ->
			cs.callbacks.error_callback t fd (err, "connect", "")
		)
	| Listening ->
		(* This should never happen, since listening sockets
		   are not set for writing.  But, to avoid a busy
		   select loop in case this socket keeps firing for
		   writes, we disable the write watch.  *)
		Unixext.Fdset.clear t.writers fd
	| Connected ->
		if cs.send_enabled
		then cs.callbacks.send_ready_callback t fd fd
		else Unixext.Fdset.clear t.writers fd

let dispatch_timers t =
	let break = ref false in
	while ((not (Timers.is_empty t.timers)) && (not !break)) do
		let first_expired = Timers.get_first_expiry_time t.timers in
		if first_expired > t.current_time then
			break := true
		else begin
			let cbs = Timers.extract_timers_at t.timers first_expired in
			List.iter (fun cb -> cb ()) cbs
		end
	done

let dispatch t interval =
	t.current_time <- Unixext.gettimeofday_monotonic ();
	let interval =
		if Timers.is_empty t.timers then interval
		else
			(* the blocking interval for select is the
			   smaller of the specified interval, and the
			   interval before which the earliest timer
			   expires.
			*)
			let block_until = if interval > 0.0 then t.current_time +. interval else t.current_time in
			let first_expiry = Timers.get_first_expiry_time t.timers in
			let block_until = (if first_expiry < block_until then first_expiry else block_until) in
			let interval = block_until -. t.current_time in
			if interval < 0.0 then 0.0 else interval
	in
	let events =
		try Some (Unixext.Fdset.select t.readers t.writers t.excepts interval)
		with Unix.Unix_error (Unix.EINTR, _, _) -> None
	in
	(match events with
	 | Some (r, w, _) ->
		(* Store dispatch set for remove_conn. *)
		t.d_readers <- r;
	 	t.d_writers <- w;
		ConnMap.iter (fun fd cs ->
				if Unixext.Fdset.is_set t.d_readers fd then
					dispatch_read t fd cs;
				if Unixext.Fdset.is_set t.d_writers fd then
					dispatch_write t fd cs
			     ) t.conns
	 | None -> ()
	);
	dispatch_timers t
