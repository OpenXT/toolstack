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

type t = {
	mutable cells: string option array;
	mutable index: int64;
}

let cell_size = 4096 * 8
let default_array_len = 16

let make () = { cells = Array.make default_array_len None; index = 0L }

let length bigbuf = bigbuf.index

let head buf =
	if Array.length buf.cells = 0 then None else (
		match buf.cells.(0) with
		| None -> None
		| Some cell ->
			  let len = Int64.to_int (min (length buf) (Int64.of_int cell_size)) in
			  Some (String.sub cell 0 len)
	)

let rec append_substring bigbuf s offset len =
	let array_offset = Int64.to_int (Int64.div bigbuf.index (Int64.of_int cell_size)) in
	let cell_offset = Int64.to_int (Int64.rem bigbuf.index (Int64.of_int cell_size)) in

	if Array.length bigbuf.cells <= array_offset then (
		(* we need to reallocate the array *)
		bigbuf.cells <- Array.append bigbuf.cells (Array.make default_array_len None)
	);

	let buf = match bigbuf.cells.(array_offset) with
	| None ->
		let newbuf = String.create cell_size in
		bigbuf.cells.(array_offset) <- Some newbuf;
		newbuf
	| Some buf ->
		buf
		in
	if len + cell_offset <= cell_size then (
		String.blit s offset buf cell_offset len;
		bigbuf.index <- Int64.add bigbuf.index (Int64.of_int len);
	) else (
		let rlen = cell_size - cell_offset in
		String.blit s offset buf cell_offset rlen;
		bigbuf.index <- Int64.add bigbuf.index (Int64.of_int rlen);
		append_substring bigbuf s (offset + rlen) (len - rlen)
	);
	()

let append_bigbuffer buf app off len =
	let cell_beg_i = Int64.to_int (Int64.div off (Int64.of_int cell_size)) in
	let beg_off = Int64.to_int (Int64.rem off (Int64.of_int cell_size)) in
	let cell_end_i = Int64.to_int (Int64.div (Int64.add off (Int64.pred len)) (Int64.of_int cell_size)) in
        let rec append i j celloff appended =
		if (i > j || appended >= len)
		then ()
		else
			match app.cells.(i) with
			| None -> ()
			| Some cell ->
				  let app_len = min (cell_size - celloff) (Int64.to_int (Int64.sub len appended)) in
				  append_substring buf cell celloff app_len;
				  append (i+1) j 0 (Int64.add appended (Int64.of_int app_len))
	in

        append cell_beg_i cell_end_i beg_off Int64.zero

let to_fct bigbuf f =
	let array_offset = Int64.to_int (Int64.div bigbuf.index (Int64.of_int cell_size)) in
	let cell_offset = Int64.to_int (Int64.rem bigbuf.index (Int64.of_int cell_size)) in

	(* copy all complete cells *)
	for i = 0 to array_offset - 1
	do
		match bigbuf.cells.(i) with
		| None      -> (* ?!?!? *) ()
		| Some cell -> f cell
	done;

	(* copy last cell *)
	begin match bigbuf.cells.(array_offset) with
	| None      -> (* ?!?!?! *) ()
	| Some cell -> f (String.sub cell 0 cell_offset)
	end;
	()

let to_string bigbuf =
	if bigbuf.index > (Int64.of_int Sys.max_string_length) then
		failwith "cannot allocate string big enough";

	let dest = String.create (Int64.to_int bigbuf.index) in
	let destoff = ref 0 in
	to_fct bigbuf (fun s ->
		let len = String.length s in
		String.blit s 0 dest !destoff len;
		destoff := !destoff + len
	);
	dest

let to_stream bigbuf outchan =
	to_fct bigbuf (fun s -> output_string outchan s)
