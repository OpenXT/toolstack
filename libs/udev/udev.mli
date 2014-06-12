(*
 * Copyright (c) 2011 Citrix Systems, Inc.
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

external socket_netlink_udev : unit -> Unix.file_descr
  = "stub_socket_netlink_udev"
external bind_netlink_udev : Unix.file_descr -> unit
  = "stub_bind_netlink_udev"
external receive_events_udev : Unix.file_descr -> string = "stub_receive_events_udev"

exception Timeout

val wait_for : string -> string -> float -> unit
