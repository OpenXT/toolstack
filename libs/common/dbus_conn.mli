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

type t = {
  ev_loop : Eventloop.t;
  ev_handle : Eventloop.handle;
  ev_fd : Unix.file_descr;
  bus : DBus.bus;
  callbacks : callbacks;
  mutable watches : DBus.watch list;
  mutable timeouts : (DBus.timeout * Eventloop.timer) list;
  mutable inactive_timeouts : DBus.timeout list;
}
and callbacks = {
  msg_received_callback : t -> DBus.message -> unit;
  error_callback : t -> Eventloop.error -> unit;
}
module Conns :
  sig
    module ConnectionMap :
      sig
        type key = Eventloop.handle
        type +'a t
        val empty : 'a t
        val is_empty : 'a t -> bool
        val add : key -> 'a -> 'a t -> 'a t
        val find : key -> 'a t -> 'a
        val remove : key -> 'a t -> 'a t
        val mem : key -> 'a t -> bool
        val iter : (key -> 'a -> unit) -> 'a t -> unit
        val map : ('a -> 'b) -> 'a t -> 'b t
        val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
        val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
        val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
        val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
      end
    val conns : t ConnectionMap.t ref
    val add_conn : ConnectionMap.key -> t -> unit
    val get_conn : ConnectionMap.key -> t
    val has_conn : ConnectionMap.key -> bool
    val remove_conn : ConnectionMap.key -> unit
  end
val send : t -> DBus.message -> int32
val enable_recv : t -> unit
val disable_recv : t -> unit
val dispatch : t -> unit
val toggle_watch_callback : t -> DBus.watch -> unit
val add_watch_callback : t -> DBus.watch -> bool
val remove_watch_callback : t -> DBus.watch -> unit
val remove_timeout_callback : t -> DBus.timeout -> unit
val timeout_handler : t -> DBus.timeout -> unit -> unit
val add_timeout_callback : t -> DBus.timeout -> bool
val toggle_timeout_callback : t -> DBus.timeout -> unit
val recv_ready_callback :
  Eventloop.t -> Conns.ConnectionMap.key -> 'a -> unit
val send_ready_callback :
  Eventloop.t -> Conns.ConnectionMap.key -> 'a -> unit
val error_callback : 'a -> Conns.ConnectionMap.key -> Eventloop.error -> unit
val db_callbacks : Eventloop.conn_callbacks
val attach : DBus.bus -> Eventloop.t -> callbacks -> t
val detach : t -> unit
