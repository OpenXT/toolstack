(*
 * Copyright (C) 2006-2007 XenSource Ltd.
 * Copyright (C) 2008      Citrix Ltd.
 * Author Vincent Hanquez <vincent.hanquez@eu.citrix.com>
 * Author Dave Scott <dave.scott@eu.citrix.com>
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
type 'a t = { size : int; mutable current : int; data : 'a array; }
val make : int -> 'a -> 'a t
val length : 'a t -> int
val push : 'a t -> 'a -> unit
val peek : 'a t -> int -> 'a
val top : 'a t -> 'a
val iter_nb : 'a t -> ('a -> 'b) -> int -> unit
val raw_iter : 'a t -> ('a -> unit) -> unit
val iter : 'a t -> ('a -> 'b) -> unit
val get_nb : 'a t -> int -> 'a array
val get_nb_lst : 'a t -> int -> 'a list
val get : 'a t -> 'a array
val get_nb_rev : 'a t -> int -> 'a array
val get_nb_rev_lst : 'a t -> int -> 'a list
