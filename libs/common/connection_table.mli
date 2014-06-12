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

module type ConnectionInfo = sig type conn end
module Make :
  functor (ConnInfo : ConnectionInfo) ->
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
      val conns : ConnInfo.conn ConnectionMap.t ref
      val add_conn : ConnectionMap.key -> ConnInfo.conn -> unit
      val get_conn : ConnectionMap.key -> ConnInfo.conn
      val has_conn : ConnectionMap.key -> bool
      val remove_conn : ConnectionMap.key -> unit
    end
