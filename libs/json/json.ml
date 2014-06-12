(*
 * Copyright (C) 2008-2009 Citrix Ltd.
 * Author Prashanth Mundkur <firstname.lastname@citrix.com>
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
 * Copyright (c) 2011 Citrix Systems, Inc.
 *)


type t =
	| Null
	| Undefined
	| Bool of bool
	| Int of int64
	| Float of float
	| String of string
	| Object of (string * t) array
	| Array of t array

let string_of_type t =
	match t with
	| Null      -> "null"
	| Undefined -> "undefined"
	| Bool _    -> "bool"
	| Int _     -> "int"
	| Float _   -> "float"
	| String _  -> "string"
	| Array _   -> "array"
	| Object _  -> "object"

let rec list_iter_between f o a =
	let len = Array.length a in
	Array.iteri (fun i v -> f v; if i < len - 1 then o ()) a

let escape_string s =
	let buf = Buffer.create 80 in
	Buffer.add_string buf "\"";
	for i = 0 to String.length s - 1
	do
		let x =
			match s.[i] with
			| '\n'   -> "\\n"
			| '\t'   -> "\\t"
			| '\r'   -> "\\r"
			| '\b'   -> "\\b"
			| '\\'   -> "\\\\"
			| '/'    -> "\\/"
			| '"'    -> "\\\""
			| '\x0c' -> "\\f"
			| c      -> String.make 1 c
			in
		Buffer.add_string buf x
	done;
	Buffer.add_string buf "\"";
	Buffer.contents buf

let rec to_fct t f =
	match t with
	| Int i      -> f (Printf.sprintf "%Ld" i)
	| Float r    -> f (Printf.sprintf "%f" r)
	| String s   -> f (escape_string s)
	| Bool b     -> f (string_of_bool b)
	| Undefined  -> f "undefined"
	| Null       -> f "null"
	| Array a    ->
		f "[";
		list_iter_between (fun i -> to_fct i f) (fun () -> f ", ") a;
		f "]";
	| Object a   ->
		f "{";
		list_iter_between (fun (k, v) -> to_fct (String k) f; f ": "; to_fct v f)
		                  (fun () -> f ", ") a;
		f "}"

let to_fct_pretty t f =
	let il = ref 0 in
	let had_newline = ref true in

	let nl () = f "\n"; had_newline := true in
	let pi () = if !had_newline then (f (String.make (2 * !il) ' '); had_newline := false) in

	let rec process t =
		pi ();
		match t with
		| Int i      -> f (Printf.sprintf "%Ld" i)
		| Float r    -> f (Printf.sprintf "%f" r)
		| String s   -> f (escape_string s)
		| Bool b     -> f (string_of_bool b)
		| Undefined  -> f "undefined"
		| Null       -> f "null"
		| Array a    ->
			if Array.length a = 0 then
				f "[]"
			else (
				f "["; nl (); incr il;
				list_iter_between (fun i -> pi (); process i) (fun () -> f ","; nl ()) a;
				nl (); decr il; pi (); f "]";
			)
		| Object a   ->
			if Array.length a = 0 then
				f "{ }"
			else (
				f "{"; nl (); incr il;
				list_iter_between (fun (k, v) -> process (String k); f ": "; process v)
						  (fun () -> f ","; nl()) a;
				nl (); decr il; pi (); f "}"
			)
		in
	process t;
	nl ()

let to_buffer t buf =
	to_fct t (fun s -> Buffer.add_string buf s)

let to_string t =
	let buf = Buffer.create 2048 in
	to_buffer t buf;
	Buffer.contents buf

let is_null = function Null -> true | _ -> false
let is_bool = function Bool _ -> true | _ -> false
let is_int  = function Int _ -> true | _ -> false
let is_float = function Float _ -> true | _ -> false
let is_string = function String _ -> true | _ -> false
let is_object = function Object _ -> true | _ -> false
let is_array = function Array _ -> true | _ -> false
let is_scalar j = not (is_object j) && not (is_array j)

(* this is a very simple parser that may not do everything right.
   REMOVE ME when the full parser is correctly hooked everywhere *)
exception Parse_unexpected of (char * char)

module SimpleParser = struct

exception Parse_unknown of char
exception End_of_parsing of int
exception Parse_unexpected_eof
exception Unexpected_escape
exception P

let of_string s =
	let ch = ref None in
	let at = ref 0 in

	let is_space () =
		match !ch with Some c when c <= ' ' -> true | _ -> false
	and is_number () =
		match !ch with Some c when c >= '0' && c <= '9' -> true | _ -> false
	and is_char exp =
		match !ch with Some c when c = exp -> true | _ -> false
	and is_eof () =
		match !ch with None -> true | Some _ -> false
	and get_char () =
		match !ch with None -> assert false | Some c -> c
		in

	let next () = try ch := Some s.[!at]; incr at; with Invalid_argument _ -> ch := None in
	let next_expecting c =
		if not (is_char c) then
			raise (Parse_unexpected (c, match !ch with None -> '\000' | Some c -> c));
		next ();
		in
	let number () =
		let buf = Buffer.create 64 in

		if is_char '-' then (
			next (); Buffer.add_char buf '-';
		);

		while is_number () do
			Buffer.add_char buf (get_char ()); next ();
		done;

		if is_char '.' then (
			Buffer.add_char buf '.'; next ();
			while is_number () do
				Buffer.add_char buf (get_char ()); next ()
			done;
			(* do exponants ... *)
			if is_char 'e'  then (
				Buffer.add_char buf (get_char()); next ();
				if is_char '+' or is_char '-' then (
					Buffer.add_char buf (get_char()); next ();
				);
				while is_number () do
					Buffer.add_char buf (get_char ()); next ()
				done
			);
			Float (float_of_string (Buffer.contents buf))
		) else (
			Int (Int64.of_string (Buffer.contents buf))
		)
		in
	let string () =
		next_expecting '"';
		let buf = Buffer.create 512 in
		let end_of_string = ref false in
		while not (is_eof ()) && not !end_of_string do
			if is_char '"' then (
				next ();
				end_of_string := true
			) else (
				if is_char '\\' then (
					next ();
					if is_char 'u' then (
						(* TODO: We currently just leave the unicode escapes in place. *)
						Buffer.add_char buf '\\';
						Buffer.add_char buf 'u';
						next();
						Buffer.add_char buf (get_char());
						next();
						Buffer.add_char buf (get_char());
						next();
						Buffer.add_char buf (get_char());
						next();
						Buffer.add_char buf (get_char())
					) else (
						(* check for common escape *)
						let ch =
							match get_char () with
							| '"' -> '"' | '\\' -> '\\'
							| '/' -> '/' | 'b' -> '\b'
							| 'f' -> Char.chr 0x0c
							| 'n' -> '\n' | 'r' -> '\r'
							| 't' -> '\t'
							| _   -> raise Unexpected_escape
							in
						Buffer.add_char buf ch
					)
				) else (
					Buffer.add_char buf (get_char ())
				);
				next ();
			)
		done;
		if !end_of_string then
			Buffer.contents buf
		else
			raise (Parse_unexpected_eof)
		in
	let white () =
		while is_space () do next () done
		in
	let word () =
		match get_char () with
		| 't' -> List.iter next_expecting [ 't'; 'r'; 'u'; 'e' ]; Bool (true)
		| 'f' -> List.iter next_expecting [ 'f'; 'a'; 'l'; 's'; 'e' ]; Bool (false)
		| 'n' -> List.iter next_expecting [ 'n'; 'u'; 'l'; 'l' ]; Null
		| 'u' -> List.iter next_expecting [ 'u'; 'n'; 'd'; 'e'; 'f'; 'i'; 'n'; 'e'; 'd' ]; Undefined
		| c   -> raise (Parse_unknown c)
		in
	let rec array () =
		if not (is_char '[') then
			raise P;
		next_expecting '[';
		white ();
		let a =
			if is_char ']' then (
				next_expecting ']';
				[| |]
			) else (
				let l = ref [] in
				let quit = ref false in
				while not !quit
				do
					l := value () :: !l;
					white ();
					if is_char ']' then (
						next_expecting ']';
						quit := true;
					) else (
						next_expecting ',';
						white ();
					)
				done;
				Array.of_list (List.rev !l)
			)
			in
		Array a
	and obj () =
		if not (is_char '{') then
			raise P;
		next_expecting '{';
		white ();
		(* check if empty object *)
		let kv =
			if is_char '}' then (
				next_expecting '}';
				[| |]
			) else (
				let o = ref [] in
				let quit = ref false in
				while not !quit
				do
					let key = string () in
					white ();
					next_expecting ':';
					let v = value () in
					o := (key, v) :: !o;
					white ();
					if is_char '}' then (
						next_expecting '}';
						quit := true;
					) else (
						next_expecting ',';
						white ()
					)
				done;
				(Array.of_list (List.rev !o))
			) in
		Object kv
	and value () =
		white ();
		match get_char () with
		| '{' -> obj ()
		| '[' -> array ()
		| '"' -> String (string ())
		| '-' -> number ()
		| c   ->
			if c >= '0' && c <= '9' then number () else word ()
		in
	next ();
	value ()
end

let of_string s = SimpleParser.of_string s
