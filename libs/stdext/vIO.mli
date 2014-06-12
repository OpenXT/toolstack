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

exception Line_limit_reached
exception Eof_reached
exception Invalid_cache_size

type t

val make : int -> int -> backend -> t

val set_read_cache : t -> int -> unit
val set_write_cache : t -> int -> unit

val has_read_cache : t -> bool

val get_fd : t -> Unix.file_descr

val read_once : t -> string -> int -> int -> int
val write_once : t -> string -> int -> int -> int

val read : t -> string -> int -> int -> int
val write : t -> string -> int -> int -> int

val read_line : t -> int -> string
val readf_eof : t -> (string -> int -> int -> unit) -> int -> unit

val flush : t -> unit
val close : t -> unit
