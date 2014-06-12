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

module D :
  sig
    val output :
      (string -> ?extra:string -> ('a, unit, string, unit) format4 -> 'a) ->
      ('a, unit, string, unit) format4 -> 'a
    val debug : ('a, unit, string, unit) format4 -> 'a
    val warn : ('a, unit, string, unit) format4 -> 'a
    val info : ('a, unit, string, unit) format4 -> 'a
    val error : ('a, unit, string, unit) format4 -> 'a
  end
val dump_msg : DBus.message -> unit
val dbus_conn_ref : Dbus_conn.t option ref
val set_connection : Dbus_conn.t -> unit
type resp_handler = DBus.message -> unit
module PendingCalls :
  sig
    type key = int32
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
val pending_calls : resp_handler PendingCalls.t ref
val send_request : DBus.message -> resp_handler -> unit
val dispatch_response : DBus.message -> unit
val send_error :
  DBus.message -> DBus.error_name -> ?err_args:DBus.ty list -> string -> unit
val send_msg : DBus.message -> unit
val send_response : DBus.message -> DBus.ty list -> unit
type signal_handler = DBus.message -> string -> string -> unit
type signal_handler_map = (string * signal_handler) list
val signal_handlers : signal_handler_map ref
val register_signal_interface : string -> signal_handler -> unit
val remove_signal_interface : string -> unit
val dispatch_signal : DBus.message -> unit
module Property :
  sig
    type getter =
        Get_Byte of (unit -> char)
      | Get_Bool of (unit -> bool)
      | Get_Int16 of (unit -> int)
      | Get_UInt16 of (unit -> int)
      | Get_Int32 of (unit -> int32)
      | Get_UInt32 of (unit -> int32)
      | Get_Int64 of (unit -> int64)
      | Get_UInt64 of (unit -> int64)
      | Get_Double of (unit -> float)
      | Get_String of (unit -> string)
      | Get_ObjectPath of (unit -> string)
    type setter =
        Set_Byte of (char -> bool) * (char -> unit)
      | Set_Bool of (bool -> bool) * (bool -> unit)
      | Set_Int16 of (int -> bool) * (int -> unit)
      | Set_UInt16 of (int -> bool) * (int -> unit)
      | Set_Int32 of (int32 -> bool) * (int32 -> unit)
      | Set_UInt32 of (int32 -> bool) * (int32 -> unit)
      | Set_Int64 of (int64 -> bool) * (int64 -> unit)
      | Set_UInt64 of (int64 -> bool) * (int64 -> unit)
      | Set_Double of (float -> bool) * (float -> unit)
      | Set_String of (string -> bool) * (string -> unit)
      | Set_ObjectPath of (string -> bool) * (string -> unit)
      | Set_Immutable
    val true_predicate : 'a -> bool
    val bool_string_predicate : string -> bool
    val int_string_predicate : string -> bool
    val uint_string_predicate : string -> bool
    val getter_argtype_str : getter -> string
    val setter_argtype_str : setter -> string
    type t = getter * setter
    val separate_errors_rvals :
      ('a, 'b * 'c * 'd) Pervasiveext.either list ->
      ('b * 'c * 'd) list * 'a list
    val dispatch_get :
      (string * (getter * 'a)) list ->
      string ->
      (DBus.ty, DBus.error_name * string * 'b list) Pervasiveext.either
    val dispatch_getall :
      (string * (getter * 'a)) list ->
      string list ->
      (DBus.ty list, (DBus.error_name * string * DBus.ty list) list)
      Pervasiveext.either
    val dispatch_set :
      (string * ('a * setter)) list ->
      string ->
      DBus.ty ->
      ('b list, DBus.error_name * string * DBus.ty list) Pervasiveext.either
    val dispatch_setall :
      (string * ('a * setter)) list ->
      (string * DBus.ty) list ->
      ('b list, (DBus.error_name * string * DBus.ty list) list)
      Pervasiveext.either
  end
type meth_result = Done of DBus.message | Pending
type meth = DBus.message -> DBus.ty list -> meth_result
type interface = { mutable methods : (string * meth) list; }
type node = {
  mutable interfaces : (string * interface) list;
  mutable properties : (string * Property.t) list;
}
val apis : (string * node) list ref
val register_node : string -> node -> unit
val remove_node : string -> unit
val dispatch_getall : DBus.message -> node -> string list -> unit
val dispatch_setall : DBus.message -> node -> (string * DBus.ty) list -> unit
val dispatch_property_interface : DBus.message -> node -> string -> unit
val dispatch_interface : DBus.message -> 'a -> interface -> string -> unit
val dispatch_api : DBus.message -> string -> string -> string -> unit
val dispatch_request : DBus.message -> unit
val error_callback : 'a -> 'b -> unit
val msg_received_callback : 'a -> DBus.message -> unit
val init_connection : ?use_session_bus:bool -> Eventloop.t -> Dbus_conn.t
val async_message_channel : DBus.message Event.channel
val async_send_msg : DBus.message -> unit
val dispatch : unit -> unit
