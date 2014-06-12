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

let dbus_conns = ref ([] : Dbus_conn.t list)

let msg_received_callback conn m =
	Printf.printf "Received %s:\n" (DBus.Message.string_of_message_ty (DBus.Message.get_type m));
	(match DBus.Message.get_sender m with
	 | Some s -> Printf.printf " Sender: %s\n" s
	 | None -> ());
	(match DBus.Message.get_destination m with
	 | Some s -> Printf.printf " Destination: %s\n" s
	 | None -> ());
	(match DBus.Message.get_path m with
	 | Some s -> Printf.printf " Path: %s\n" s
	 | None -> ());
	(match DBus.Message.get_interface m with
	 | Some s -> Printf.printf " Interface: %s\n" s
	 | None -> ());
	(match DBus.Message.get_member m with
	 | Some s -> Printf.printf " Method: %s\n" s
	 | None -> ());
	List.iter (fun arg ->
			Printf.printf " Arg: %s\n" (DBus.string_of_ty arg)
		  ) (DBus.Message.get m);
	Printf.printf "%!"

let error_callback conn err =
	Printf.printf "Received error.\n%!"

let callbacks =
{
	Dbus_conn.msg_received_callback = msg_received_callback;
	Dbus_conn.error_callback = error_callback;
}

let destination = "org.freedesktop.DBus"

let make_ping () =
	let path = "/" in
	let interface = "org.freedesktop.DBus.Peer" in
	let meth = "Ping" in
	let msg = DBus.Message.new_method_call destination path interface meth
	in msg

let make_get_machine_id () =
	let path = "/" in
	let interface = "org.freedesktop.DBus.Peer" in
	let meth = "GetMachineId" in
	let msg = DBus.Message.new_method_call destination path interface meth
	in msg

let make_filter msg_type =
	let path = "/org/freedesktop/DBus" in
	let interface = "org.freedesktop.DBus" in
	let meth = "AddMatch" in
	let msg = DBus.Message.new_method_call destination path interface meth in
	  DBus.Message.append msg [(DBus.String (Printf.sprintf "type='%s'" msg_type))];
	  msg

let loop el conn =
	while Eventloop.has_connections el || Eventloop.has_timers el
	do
		(* We need to keep this select timeout small, due to
		   the broken libdbus async API, which requires
		   dispatch to be called at each iteration of the main
		   loop. *)
		Eventloop.dispatch el 0.5;
		List.iter (fun db -> Dbus_conn.dispatch db) !dbus_conns;
	done

let main () =
	let el = Eventloop.create () in
	let bus = DBus.Bus.get DBus.Bus.System in
	let conn = Dbus_conn.attach bus el callbacks in
	dbus_conns := conn :: !dbus_conns;
	Dbus_conn.enable_recv conn;
	ignore (Dbus_conn.send conn (make_ping ()));
	ignore (Dbus_conn.send conn (make_get_machine_id ()));
	ignore (Dbus_conn.send conn (make_filter "method_call"));
	ignore (Dbus_conn.send conn (make_filter "method_return"));
	ignore (Dbus_conn.send conn (make_filter "error"));
	ignore (Dbus_conn.send conn (make_filter "signal"));

	loop el conn

let _ =
	Printexc.record_backtrace true;

	try main ()
	with e ->
		Printf.printf "Uncaught exception: %s at\n" (Printexc.to_string e);
		Printf.printf "%s\n" (Printexc.get_backtrace ())
