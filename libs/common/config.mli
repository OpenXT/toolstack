(*
 * Copyright (c) 2010 Citrix Systems, Inc.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 *)

type ty =
    Set_bool of bool ref
  | Set_int of int ref
  | Set_string of string ref
  | Set_float of float ref
  | Unit of (unit -> unit)
  | Bool of (bool -> unit)
  | Int of (int -> unit)
  | String of (string -> unit)
  | Float of (float -> unit)
exception Error of (string * string) list
val trim_start : char list -> string -> string
val trim_end : char list -> string -> string
val split : ?limit:int -> char -> string -> string list
val parse_line : in_channel -> (string * string) list
val parse : string -> (string * string) list
val validate :
  (string * string) list ->
  (string * ty) list -> (string -> string -> unit) -> unit
val read : string -> (string * ty) list -> (string -> string -> unit) -> unit
