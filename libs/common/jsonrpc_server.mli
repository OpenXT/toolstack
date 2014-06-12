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

(* Protocol recognizers *)

(* This is a function called on the first JSON message on an incoming
   connection. Note that this may not be a JSONRPC message.
*)
type recognizer = Json.t -> bool

(* This is a function paired with a recognizer; if the recognizer
   returned true, then this function is called to take over the
   management of the connection.  It is called with the first JSON
   message, plus the remaining unparsed input if any.
*)
type acceptor = Eventloop.t -> Eventloop.handle -> Json.t -> (string * int * int) -> unit


(* Event loop *)

(* This function creates and starts the main event loop.  It is called
   with a set of protocol recognizers, and a list of sockets on which
   to listen.  The function returns when there are no more sockets or
   timers registered with the event loop, or when stopped.  The setup
   callback function can be used to start outgoing/client connections.
*)
val start_server: ?setup_callback:(Eventloop.t -> unit) -> (recognizer * acceptor) list -> Unix.file_descr list -> unit

(* Causes the previous function to return as soon as possible from the event loop. *)
val stop_server: unit -> unit
