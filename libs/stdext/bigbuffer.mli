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
type t
val make : unit -> t
val length : t -> int64
val head : t -> string option
val append_substring : t -> string -> int -> int -> unit
val append_bigbuffer : t -> t -> int64 -> int64 -> unit
val to_fct : t -> (string -> unit) -> unit
val to_string : t -> string
val to_stream : t -> out_channel -> unit
