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
open CamomileLibrary
open Printf

type cursor =
	| Start
	| Expect_value
	| In_null of int
	| In_true of int
	| In_false of int
	| In_int of UChar.t list
	| In_float of UChar.t list * UChar.t list
	| In_int_exp of UChar.t list * UChar.t list
	| In_float_exp of UChar.t list * UChar.t list * UChar.t list
	| In_string of UTF8.Buf.buf
	| In_string_control of UTF8.Buf.buf
	| In_string_hex of UTF8.Buf.buf * int * int
	| In_string_hex_low_surrogate of UTF8.Buf.buf * int * int * int
	| Expect_object_elem_start
	| Expect_object_elem_colon
	| Expect_comma_or_end
	| Expect_object_key
	| Done of Json.t

type int_value =
	| IObject of (string * Json.t) list
	| IObject_needs_key of (string * Json.t) list
	| IObject_needs_value of (string * Json.t) list * string
	| IArray of Json.t list

type parse_state =
{
	mutable cursor: cursor;
	mutable stack: int_value list;
	mutable num_chars_parsed: int;
	mutable line_num: int
}

let init_parse_state () =
{
	cursor = Start;
	stack = [];
	num_chars_parsed = 0;
	line_num = 1
}

let is_parsing_object s =
	match s.stack with
	| IObject _ :: _ | IObject_needs_key _ :: _ | IObject_needs_value _ :: _ -> true
	| IArray _ :: _
	| [] -> false

let get_parse_result s =
	match s.cursor with
	| Done v -> Some v
	| _ -> None

let ivalue_to_str = function
	| IObject _ -> "object"
	| IObject_needs_key _ -> "object_needing_key"
	| IObject_needs_value _ -> "object_needing_value"
	| IArray _ -> "array"

let current_cursor_value = function
	| Start	| Expect_value -> "value"
	| In_null _ -> "null"
	| In_true _ | In_false _ -> "boolean"
	| In_int _ | In_float _ | In_int_exp _ | In_float_exp _	 -> "number"
	| In_string _ | In_string_control _ | In_string_hex _ | In_string_hex_low_surrogate _ -> "string"
	| Expect_object_elem_start | Expect_object_elem_colon | Expect_object_key -> "object"
	| Expect_comma_or_end -> "object/array"
	| Done _ -> ""

let is_space c =
	c = UChar.of_char ' ' || c = UChar.of_char '\t' || c = UChar.of_char '\n' || c = UChar.of_char '\r'

let update_line_num s c =
	if c = UChar.of_char '\n' then
		s.line_num <- s.line_num + 1

let ch_0 = UChar.of_char '0'
let ch_9 = UChar.of_char '9'
let ch_a = UChar.of_char 'a'
let ch_f = UChar.of_char 'f'
let ch_A = UChar.of_char 'A'
let ch_F = UChar.of_char 'F'

let is_digit c = c >= ch_0 && c <= ch_9

let is_hex_char c =
	(c >= ch_0 && c <= ch_9) ||
	(c >= ch_a && c <= ch_f) ||
	(c >= ch_A && c <= ch_F)

let hex_char_value c =
	if (c >= ch_0 && c <= ch_9) then (
		UChar.int_of c - UChar.int_of ch_0
	) else if (c >= ch_a && c <= ch_f) then (
		UChar.int_of c - UChar.int_of ch_a + 10
	) else if (c >= ch_A && c <= ch_F) then (
		UChar.int_of c - UChar.int_of ch_A + 10
	) else 0

let is_valid_unescaped_char c =
	match (UChar.code c) with
	| 0x22 | 0x5c -> false
	| _ -> true (* yey we support unicode now *)

let clist_to_string ( cs : UChar.t list ) =
	let len = List.length cs in
	let buf = UTF8.Buf.create len in
	let append c = UTF8.Buf.add_char buf c in
	List.iter append cs;
	UTF8.Buf.contents buf

type error =
	| Unexpected_char of int * UChar.t * (* json type *) string
	| Invalid_value of int * (* value *) string * (* json type *) string
	| Invalid_leading_zero of int * string
	| Unterminated_value of int * string
	| Internal_error of int * string

let string_of_error = function
	| Unexpected_char (l, c, state) ->
		Printf.sprintf "Line %d: Unexpected char (x%X) encountered in state %s"
			l (UChar.code c) state
	| Invalid_value (l, v, t) ->
		Printf.sprintf "Line %d: '%s' is an invalid %s" l v t
	| Invalid_leading_zero (l, s) ->
		Printf.sprintf "Line %d: '%s' should not have leading zeros" l s
	| Unterminated_value (l, s) ->
		Printf.sprintf "Line %d: unterminated %s" l s
	| Internal_error (l, m) ->
		Printf.sprintf "Line %d: Internal error: %s" l m

exception Parse_error of error

let raise_unexpected_char s c t =
	raise (Parse_error (Unexpected_char (s.line_num, c, t)))
let raise_invalid_value s v t =
	raise (Parse_error (Invalid_value (s.line_num, v, t)))
let raise_invalid_leading_zero s n =
	raise (Parse_error (Invalid_leading_zero (s.line_num, n)))
let raise_unterminated_value s v =
	raise (Parse_error (Unterminated_value (s.line_num, v)))
let raise_internal_error s m =
	raise (Parse_error (Internal_error (s.line_num, m)))

let finish_value s v =
	match s.stack, v with
		| [], _ ->
			s.cursor <- Done v
		| IObject_needs_key fields :: tl, Json.String key ->
			s.stack <- IObject_needs_value (fields, key) :: tl;
			s.cursor <- Expect_object_elem_colon
		| IObject_needs_value (fields, key) :: tl, _ ->
			s.stack <- IObject ((key, v) :: fields) :: tl;
			s.cursor <- Expect_comma_or_end
		| IArray l :: tl, _ ->
			s.stack <- IArray (v :: l) :: tl;
			s.cursor <- Expect_comma_or_end
		| io :: tl, _ ->
			raise_internal_error s ("unexpected " ^ (ivalue_to_str io)
						^ " on stack at finish_value")

let pop_stack s =
	match s.stack with
	| IObject fields :: tl ->
		s.stack <- tl;
		finish_value s (Json.Object (Array.of_list (List.rev fields)))
	| IArray l :: tl ->
		s.stack <- tl;
		finish_value s (Json.Array (Array.of_list (List.rev l)))
	| io :: tl ->
		raise_internal_error s ("unexpected " ^ (ivalue_to_str io)
					^ " on stack at pop_stack")
	| [] ->
		raise_internal_error s "empty stack at pop_stack"

let chr ch = UChar.of_char ch

let ch_n = chr 'n'

let rec parse_char s c =
	(* Printf.printf "parsing %C at line %d in state %s...\n" c s.line_num (current_cursor_value s.cursor); *)
	let tostring_with_leading_zero_check is =
		let ris = List.rev is in
		let check = function
			| [] -> ()
			| [c]     when c = UChar.of_char '0' -> ()
			| c :: tl when c = UChar.of_char '0' && List.length tl > 0 ->
				  raise_invalid_leading_zero s (clist_to_string ris)
			| _ -> () in
		check ris;
		clist_to_string ris in
	let finish_int is =
		let str = tostring_with_leading_zero_check is in
		let int = try Int64.of_string str
			  with Failure _ -> raise_invalid_value s str "int" in
		finish_value s (Json.Int int) in
	let finish_int_exp is es =
		let int = tostring_with_leading_zero_check is in
		let exp = clist_to_string (List.rev es) in
		let str = Printf.sprintf "%s.e%s" int exp in
		  (* If exp is positive, we might actually
		     succeed in making this an int, but
		     returning float is more uniform. *)
		let float = try float_of_string str
			    with Failure _ -> raise_invalid_value s str "float" in
		finish_value s (Json.Float float) in
	let finish_float is fs =
		let int = tostring_with_leading_zero_check is in
		let frac = clist_to_string (List.rev fs) in
		let str = Printf.sprintf "%s.%s" int frac in
		let float = try float_of_string str
			    with Failure _ -> raise_invalid_value s str "float" in
		finish_value s (Json.Float float) in
	let finish_float_exp is fs es =
		let int = tostring_with_leading_zero_check is in
		let frac = clist_to_string (List.rev fs) in
		let exp = clist_to_string (List.rev es) in
		let str = Printf.sprintf "%s.%se%s" int frac exp in
		let float = try float_of_string str
			    with Failure _ -> raise_invalid_value s str "float" in
		finish_value s (Json.Float float) in

	match s.cursor with
	| Start ->
		(match c with
		 | x when x = UChar.of_char 'n' ->
			s.cursor <- In_null 3
		 | x when x = UChar.of_char 't' ->
			s.cursor <- In_true 3
		 | x when x = UChar.of_char 'f' ->
			s.cursor <- In_false 4
		 | x when x = UChar.of_char '-' || is_digit x ->
			s.cursor <- In_int [c]
		 | x when x = UChar.of_char '"' ->
			s.cursor <- In_string (UTF8.Buf.create 8)
		 | x when x = UChar.of_char '{' ->
			s.cursor <- Expect_object_elem_start
		 | x when x = UChar.of_char '[' ->
			s.stack	 <- (IArray []) :: s.stack;
		 | x when x = UChar.of_char ']' && s.stack <> [] ->
			pop_stack s
		 | _ when is_space c ->
			update_line_num s c
		 | _ ->
			raise_unexpected_char s c "start")
	| Expect_value ->
		(match c with
		 | x when x = UChar.of_char 'n' ->
			s.cursor <- In_null 3
		 | x when x = UChar.of_char 't' ->
			s.cursor <- In_true 3
		 | x when x = UChar.of_char 'f' ->
			s.cursor <- In_false 4
		 | x when x = UChar.of_char '-' || is_digit x ->
			s.cursor <- In_int [c]
		 | x when x = UChar.of_char '"' ->
			s.cursor <- In_string (UTF8.Buf.create 8)
		 | x when x = UChar.of_char '{' ->
			s.cursor <- Expect_object_elem_start
		 | x when x = UChar.of_char '[' ->
			s.stack	 <- (IArray []) :: s.stack;
			s.cursor <- Start
		 | _ when is_space c ->
			update_line_num s c
		 | _ ->
			raise_unexpected_char s c "value")
	| In_null rem ->
		(match c, rem with
		 | (x,3) when x = UChar.of_char 'u' ->
			s.cursor <- In_null 2
		 | (x,2) when x = UChar.of_char 'l' ->
			s.cursor <- In_null 1
		 | (x,1) when x = UChar.of_char 'l' ->
			finish_value s Json.Null
		 | _ ->
			raise_unexpected_char s c "null")
	| In_true rem ->
		(match c, rem with
		 | (x,3) when x = UChar.of_char 'r' ->
			s.cursor <- In_true 2
		 | (x,2) when x = UChar.of_char 'u' ->
			s.cursor <- In_true 1
		 | (x,1) when x = UChar.of_char 'e' ->
			finish_value s (Json.Bool true)
		 | _ ->
			raise_unexpected_char s c "true")
	| In_false rem ->
		(match c, rem with
		 | (x,4) when x = UChar.of_char 'a' ->
			s.cursor <- In_false 3
		 | (x,3) when x = UChar.of_char 'l' ->
			s.cursor <- In_false 2
		 | (x,2) when x = UChar.of_char 's' ->
			s.cursor <- In_false 1
		 | (x,1) when x = UChar.of_char 'e' ->
			finish_value s (Json.Bool false)
		 | _ ->
			raise_unexpected_char s c "false")
	| In_int is ->
		(match c with
		 | x when is_digit x ->
			s.cursor <- In_int (c :: is)
		 | x when x = UChar.of_char '.' ->
			s.cursor <- In_float (is, [])
		 | x when x = UChar.of_char 'e' || x = UChar.of_char 'E' ->
			s.cursor <- In_int_exp (is, [])
		 | x when x = UChar.of_char ',' || x = UChar.of_char ']' || x = UChar.of_char '}' ->
			finish_int is;
			parse_char s c
		 | _ when is_space c ->
			update_line_num s c;
			finish_int is
		 | _ ->
			raise_unexpected_char s c "int")
	| In_float (is, fs) ->
		(match c with
		 | x when is_digit x ->
			s.cursor <- In_float (is, c :: fs)
		 | x when x = UChar.of_char 'e' || x = UChar.of_char 'E' ->
			s.cursor <- In_float_exp (is, fs, [])
		 | x when x = UChar.of_char ',' || x = UChar.of_char ']' || x = UChar.of_char '}' ->
			finish_float is fs;
			parse_char s c
		 | _ when is_space c ->
			update_line_num s c;
			finish_float is fs
		 | _ ->
			raise_unexpected_char s c "float")
	| In_int_exp (is, es) ->
		(match c with
		 | x when x = UChar.of_char '+' || x = UChar.of_char '-' || is_digit x ->
			s.cursor <- In_int_exp (is, c :: es)
		 | x when x = UChar.of_char ',' || x = UChar.of_char ']' || x = UChar.of_char '}' ->
			finish_int_exp is es;
			parse_char s c
		 | _ when is_space c ->
			update_line_num s c;
			finish_int_exp is es
		 | _ ->
			raise_unexpected_char s c "int_exp")
	| In_float_exp (is, fs, es) ->
		(match c with
		 | x when x = UChar.of_char '+' || x = UChar.of_char '-' || is_digit x ->
			s.cursor <- In_float_exp (is, fs, c :: es)
		 | x when x = UChar.of_char ',' || x = UChar.of_char ']' || x = UChar.of_char '}' ->
			finish_float_exp is fs es;
			parse_char s c
		 | _ when is_space c ->
			update_line_num s c;
			finish_float_exp is fs es
		 | _ ->
			raise_unexpected_char s c "float_exp")
	| In_string cs ->
		(match c with
		 | x when x = UChar.of_char '\\' ->
			s.cursor <- In_string_control cs
		 | x when x = UChar.of_char '"' ->
			finish_value s (Json.String (UTF8.Buf.contents cs))
		 | _ ->
			if is_valid_unescaped_char c then (
				UTF8.Buf.add_char cs c;
				s.cursor <- In_string cs
			) else (
				raise_unexpected_char s c "string"))
	| In_string_control cs ->
		(match c with
		 | x when x = UChar.of_char '"' || x = UChar.of_char '\\' || x = UChar.of_char '/' ->
			   UTF8.Buf.add_char cs c;
			   s.cursor <- In_string cs
		 | x when x = UChar.of_char 'b' ->
			   UTF8.Buf.add_char cs (UChar.of_char '\b');
			   s.cursor <- In_string cs
		 | x when x = UChar.of_char 'f' ->
			   UTF8.Buf.add_char cs (UChar.of_char '\x0c');
			   s.cursor <- In_string cs
		 | x when x = UChar.of_char 'n' ->
			   UTF8.Buf.add_char cs (UChar.of_char '\n');
			   s.cursor <- In_string cs
		 | x when x = UChar.of_char 'r' ->
			   UTF8.Buf.add_char cs (UChar.of_char '\r');
			   s.cursor <- In_string cs
		 | x when x = UChar.of_char 't' ->
			   UTF8.Buf.add_char cs (UChar.of_char '\t');
			   s.cursor <- In_string cs;
		 | x when x = UChar.of_char 'u' ->
			   s.cursor <- In_string_hex (cs, 0, 4)
		 | _ ->
			raise_unexpected_char s c "string_control")
	| In_string_hex (cs, ch_value, rem) ->
		(if is_hex_char c then begin
			let v' = ch_value + ((hex_char_value c) lsl ((rem-1)*4)) in
			if rem > 1 then
				s.cursor <- In_string_hex (cs, v', rem - 1)
			else (
				if v' >= 0xD800 && v' <= 0xDBFF then (
					(* high surrogate *)
					s.cursor <- In_string_hex_low_surrogate (cs, 0, v', 6)
				) else (
					UTF8.Buf.add_char cs (UChar.of_int v');
					s.cursor <- In_string cs
				)
			);
		 end else
			raise_unexpected_char s c "string_unicode")
	| In_string_hex_low_surrogate (cs, ch_value, high_surr, rem) ->
		  (match (rem, c) with
		  | (6, x) when x = UChar.of_char '\\' ->
			    s.cursor <- In_string_hex_low_surrogate (cs, ch_value, high_surr, rem-1)
		  | (5, x) when x = UChar.of_char 'u' ->
			    s.cursor <- In_string_hex_low_surrogate (cs, ch_value, high_surr, rem-1)
		  | _ ->
			    (if is_hex_char c then begin
				     let v' = ch_value + ((hex_char_value c) lsl ((rem-1)*4)) in
				     if rem > 1 then
					     s.cursor <- In_string_hex_low_surrogate (cs, v', high_surr, rem - 1)
				     else begin
					     let low_surr = v' in
					     if low_surr >= 0xDC00 && low_surr <= 0xDFFF then (
						     let uni_v    = 0x10000 + (low_surr-0xDC00) + ((high_surr-0xD800) lsl 10) in
						     UTF8.Buf.add_char cs (UChar.of_int uni_v);
						     s.cursor <- In_string cs
					     ) else
						     raise_unexpected_char s c "string_unicode_low_surrogate"
			             end
			     end else
				     raise_unexpected_char s c "string_unicode_low_surrogate"
			    )
		  )
	| Expect_object_elem_start ->
		(match c with
		 | x when x = UChar.of_char '"' ->
			s.stack <- (IObject_needs_key []) :: s.stack;
			s.cursor <- In_string (UTF8.Buf.create 8)
		 | x when x = UChar.of_char '}' ->
			finish_value s (Json.Object (Array.of_list []))
		 | _ when is_space c ->
			update_line_num s c
		 | _ ->
			raise_unexpected_char s c "object_start")
	| Expect_object_elem_colon ->
		(match c with
		 | x when x = UChar.of_char ':' ->
			s.cursor <- Start
		 | _ when is_space c ->
			update_line_num s c
		 | _ ->
			raise_unexpected_char s c "object_elem_colon")
	| Expect_comma_or_end ->
		(match c with
		 | x when x = UChar.of_char ',' ->
			if is_parsing_object s then s.cursor <- Expect_object_key
			else s.cursor <- Expect_value
		 | x when x = UChar.of_char '}' ->
			if is_parsing_object s then pop_stack s
			else raise_unexpected_char s c "comma_or_end"
		 | x when x = UChar.of_char ']' ->
			if not (is_parsing_object s) then pop_stack s
			else raise_unexpected_char s c "comma_or_end"
		 | _ when is_space c ->
			update_line_num s c
		 | _ ->
			raise_unexpected_char s c "comma_or_end")
	| Expect_object_key ->
		(match c with
		 | x when x = UChar.of_char '"' ->
			(match s.stack with
			 | IObject fields :: tl -> s.stack <- IObject_needs_key fields :: tl
			 | io :: _ -> raise_internal_error s ("unexpected " ^ (ivalue_to_str io) ^ " on stack at object_key")
			 | [] -> raise_internal_error s "empty stack at object_key");
			s.cursor <- In_string (UTF8.Buf.create 8)
		 | _ when is_space c ->
			update_line_num s c
		 | _ ->
			raise_unexpected_char s c "object_key")
	| Done _ ->
		raise_internal_error s "parse called when parse_state is 'Done'"


type parse_result =
  | Json_value of Json.t * (* number of consumed bytes *) int
  | Json_parse_incomplete of parse_state

let parse state str =
	let i = ref 0 in
	let iter_char ch =
		if (get_parse_result state = None) then (
			parse_char state ch;
			flush stdout;
			state.num_chars_parsed <- state.num_chars_parsed + 1;
			incr i;
		)
	in
	UTF8.iter iter_char str;
	match get_parse_result state with
	| Some v -> Json_value (v, !i)
	| None -> Json_parse_incomplete state


(* This is really only required for numbers, since they are only
   terminated by whitespace, but end-of-file or end-of-connection
   qualifies as whitespace.

   The parser might also be just eating whitespace, expecting the
   start of a json value.
*)
let finish_parse state =
	match parse state " " with
	| Json_value (v, _) -> Some v
	| Json_parse_incomplete _ ->
		if state.cursor = Start then None
		else raise_unterminated_value state (current_cursor_value state.cursor)

let num_chars_parsed state = state.num_chars_parsed


(* convenience function, with same API as old SimpleParser *)
let of_string str =
	match parse (init_parse_state ()) str with
	| Json_value (v, _) ->
		v
	| Json_parse_incomplete st ->
		(match finish_parse st with
		 | Some v -> v
		 | None -> raise_unterminated_value st (current_cursor_value st.cursor)
		)
