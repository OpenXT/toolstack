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
 * Copyright (c) 2009 Citrix Systems, Inc.
 *)


type backend = {
	blksize: int;
	read: string -> int -> int -> int;
	write: string -> int -> int -> int;
	flush: unit -> unit;
	close: unit -> unit;
	selectable: Unix.file_descr option;
}

type cache = {
	read_cache_size: int;
	write_cache_size: int;
	read_ring: Qring.t;
	write_ring: Qring.t;
}

type t = { backend: backend; mutable cache: cache; mutable reached_eof: bool }

exception Cache_not_empty
exception Invalid_cache_size

let check_cache_size sz =
	if sz < 0 || sz > 1024 * 1024 then
		raise Invalid_cache_size

let make rcache wcache backend =
	check_cache_size rcache;
	check_cache_size wcache;
	let cache = {
		read_cache_size = rcache;
		write_cache_size = wcache;
		read_ring = Qring.make rcache;
		write_ring = Qring.make wcache;
	} in
	{ backend = backend; cache = cache; reached_eof = false }

let set_read_cache con sz =
	check_cache_size sz;
	if Qring.to_consume con.cache.read_ring > 0 then
		raise Cache_not_empty;
	con.cache <- {
		con.cache with read_cache_size = sz; read_ring = Qring.make sz
	}

let set_write_cache con sz =
	check_cache_size sz;
	if Qring.to_consume con.cache.write_ring > 0 then
		raise Cache_not_empty;
	con.cache <- {
		con.cache with write_cache_size = sz; write_ring = Qring.make sz
	}

let get_fd con =
	match con.backend.selectable with
	| None    -> assert false
	| Some fd -> fd

let read_fill_cache con =
	if con.reached_eof then
		0
	else
		let tofill = Qring.to_fill con.cache.read_ring in
		let toread = min con.backend.blksize tofill in
		let s = String.create toread in
		let readed = con.backend.read s 0 toread in
		if readed = 0 then
			con.reached_eof <- true
		else
			Qring.feed con.cache.read_ring s 0 readed;
		readed

let has_read_cache con =
	Qring.to_consume con.cache.read_ring > 0

exception Internal_cache_error

let read_once_nocache con buf index hint =
	con.backend.read buf index hint

let read_once_cache con buf index hint =
	let cached = Qring.to_consume con.cache.read_ring in
	if cached >= hint then (
		let rhint = Qring.consume_to con.cache.read_ring buf index hint in
		if rhint < hint then
			raise Internal_cache_error;
		hint
	) else (
		if cached > 0 then (
			let rcached = Qring.consume_to con.cache.read_ring buf index cached in
			if rcached < cached then
				raise Internal_cache_error; 
			()
		);
		let readed = read_fill_cache con in
		if readed > 0 then (
			let left = hint - cached in
			let len = if readed > left then left else readed in
			let rlen = Qring.consume_to con.cache.read_ring buf (index + cached) len in
			if rlen < len then
				raise Internal_cache_error;
			()
		);
		min (readed + cached) hint
	)

let read_once con =
	(if con.cache.read_cache_size = 0 then read_once_nocache else read_once_cache) con

let write_flush_cache con =
	let buf = Qring.consume_all con.cache.write_ring in
	let len = String.length buf in
	if len > 0 then (
		let written = con.backend.write buf 0 len in
		if written = 0 then
			0
		else if written = len then
			Qring.to_fill con.cache.write_ring
		else ( (* 0 < written < len *)
			let to_put_back = len - written in
			Qring.feed con.cache.write_ring buf written to_put_back;
			Qring.to_fill con.cache.write_ring
		)
	) else
		0

let write_once_nocache con buf index hint =
	con.backend.write buf index hint

let write_once_cache con buf index hint =
	let can_cache = Qring.to_fill con.cache.write_ring in
	(* the cache is full, flush it, and fill the cache with the buf as much as we can *)
	if can_cache = 0 then (
		let to_fill = write_flush_cache con in
		if to_fill > 0 then (
			let len = min hint can_cache in
			Qring.feed con.cache.write_ring buf index len;
			len
		) else
			0
	(* the cache is empty *)
	) else if can_cache = con.cache.write_cache_size then (
		(* check if we have enough to send a full buf without copying to the cache *)
		if can_cache <= hint then (
			let written = con.backend.write buf index hint in
			written
		) else (
			Qring.feed con.cache.write_ring buf index hint;
			hint
		)
	(* the cache contains something, try filling it *)
	) else (
		(* the cache will be full *)
		if can_cache <= hint then (
			Qring.feed con.cache.write_ring buf index can_cache;
			let to_fill = write_flush_cache con in
			ignore to_fill;
			can_cache
		) else (
			Qring.feed con.cache.write_ring buf index hint;
			hint
		)
	)

let write_once con =
	(if con.cache.write_cache_size = 0 then write_once_nocache else write_once_cache) con

let do_rw_io f buf index len =
	let left = ref len in
	let index = ref index in
	let end_of_file = ref false in
	while !left > 0 && not !end_of_file
	do
		let ret = f buf !index !left in
		if ret = 0 then
			end_of_file := true
		else if ret > 0 then (
			left := !left - ret;
			index := !index + ret;
		)
	done;
	len - !left

let read con buf index size =
	do_rw_io (read_once con) buf index size

exception Line_limit_reached
exception Buffer_limit_reached
exception Eof_reached

let read_line con max =
	let buffer = Buffer.create 80 in
	let s = String.create 1 in
	let found = ref false and i = ref 0 in
	while not !found && (max = 0 || !i < max)
	do
		let n = read_once con s 0 1 in
		if n = 0 then
			raise Eof_reached;

		if s.[0] = '\n' then
			found := true
		else (
			i := !i + n;
			Buffer.add_string buffer s;
		)
	done;
	if !i = max then
		raise Line_limit_reached;
	Buffer.contents buffer

let readf_eof con f max =
	let end_of_file = ref false in
	let acc = ref 0 in
	let s = String.create 1024 in
	while not !end_of_file
	do
		let ret = read_once con s 0 1024 in
		if ret = 0 then
			end_of_file := true
		else (
			acc := !acc + ret;
			if max > 0 && !acc > max then
				raise Buffer_limit_reached;
			f s 0 ret
		)
	done


let write con buf index size =
	do_rw_io (write_once con) buf index size

let flush con = while write_flush_cache con > 0 do () done

let close con = con.backend.close ()
