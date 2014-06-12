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

open Pervasiveext
open Stringext

module D=Debug.Debugger(struct let name="dbus-server" end)
open D

(* Utilities *)

let dump_msg m =
	Opt.iter (fun s -> debug " Sender: %s" s) (DBus.Message.get_sender m);
	Opt.iter (fun s -> debug " Destination: %s" s) (DBus.Message.get_destination m);
	Opt.iter (fun s -> debug " Path: %s" s) (DBus.Message.get_path m);
	Opt.iter (fun s -> debug " Interface: %s" s) (DBus.Message.get_interface m);
	Opt.iter (fun s -> debug " Member: %s" s) (DBus.Message.get_member m);
	List.iter (fun arg ->
			debug " Arg: %s" (DBus.string_of_ty arg)
		  ) (DBus.Message.get m);
	()

(* Connection *)

let dbus_conn_ref = ref (None : Dbus_conn.t option)
let set_connection conn =
	dbus_conn_ref := Some conn

(* Client interface *)

type resp_handler = DBus.message -> unit
module PendingCalls = Map.Make (struct type t = int32 let compare = compare end)

let pending_calls = ref (PendingCalls.empty : (resp_handler PendingCalls.t))

let send_request msg resp_handler =
	debug "Sending request...";
	dump_msg msg;
	let serial = Dbus_conn.send (Opt.unbox !dbus_conn_ref) msg in
	pending_calls := PendingCalls.add serial resp_handler !pending_calls

let dispatch_response resp =
	let reply_serial = DBus.Message.get_reply_serial resp in
	let handler = (try Some (PendingCalls.find reply_serial !pending_calls)
		       with Not_found -> None) in
	match handler with
	| Some h ->
		pending_calls := PendingCalls.remove reply_serial !pending_calls;
		h resp
	| None ->
		(* TODO: handle error messages specially here? *)
		()

let send_error req err_name ?(err_args=[]) err_msg =
	let err_msg = DBus.Message.new_error req err_name err_msg in
	DBus.Message.append err_msg err_args;
	info "Sending error message...";
	dump_msg err_msg;
	ignore (Dbus_conn.send (Opt.unbox !dbus_conn_ref) err_msg)

let send_msg msg =
	info "Sending message...";
	dump_msg msg;
	ignore (Dbus_conn.send (Opt.unbox !dbus_conn_ref) msg)

let send_response req args =
	let resp = DBus.Message.new_method_return req in
	DBus.Message.append resp args;
	send_msg resp

(* Signal interface *)

type signal_handler = DBus.message -> (* member *) string -> (* path *) string -> unit
type signal_handler_map = ((* interface name *) string * signal_handler) list

let signal_handlers = ref ( [] : signal_handler_map )

let register_signal_interface interface_name handler =
	signal_handlers := (interface_name, handler) :: !signal_handlers

let remove_signal_interface interface_name =
	signal_handlers := List.filter (fun (n, _) -> n <> interface_name) !signal_handlers

let dispatch_signal msg =
	let opt_intf   = DBus.Message.get_interface msg in
	let opt_member = DBus.Message.get_member msg in
	let opt_path   = DBus.Message.get_path msg in
	match opt_intf, opt_member, opt_path with
	| Some i, Some m, Some p ->
		let handler = (try Some (List.assoc i !signal_handlers) with Not_found -> None) in
		(match handler with
		| None -> ()
		| Some h -> h msg m p
		)
	| _, _, _ ->
		(* Invalid signal *)
		()

(* Internal property interface *)

module Property = struct
	(* only scalars supported for now *)
	type getter =
		| Get_Byte   of (unit   -> char)
		| Get_Bool   of (unit   -> bool)
		| Get_Int16  of (unit   -> int)
		| Get_UInt16 of (unit   -> int)
		| Get_Int32  of (unit   -> int32)
		| Get_UInt32 of (unit   -> int32)
		| Get_Int64  of (unit   -> int64)
		| Get_UInt64 of (unit   -> int64)
		| Get_Double of (unit   -> float)
		| Get_String of (unit   -> string)
		| Get_ObjectPath of (unit -> string)

	(* setters are tuples of predicates and the real_setters; the
	   real setters are only called if the predicate is satisfied on
	   the input value *)
	type setter =
		| Set_Byte   of (char   -> bool) * (char   -> unit)
		| Set_Bool   of (bool   -> bool) * (bool   -> unit)
		| Set_Int16  of (int    -> bool) * (int    -> unit)
		| Set_UInt16 of (int    -> bool) * (int    -> unit)
		| Set_Int32  of (int32  -> bool) * (int32  -> unit)
		| Set_UInt32 of (int32  -> bool) * (int32  -> unit)
		| Set_Int64  of (int64  -> bool) * (int64  -> unit)
		| Set_UInt64 of (int64  -> bool) * (int64  -> unit)
		| Set_Double of (float  -> bool) * (float  -> unit)
		| Set_String of (string -> bool) * (string -> unit)
		| Set_ObjectPath of (string -> bool) * (string -> unit)
		| Set_Immutable

	(* common utility predicate *)
	let true_predicate _ = true
	let bool_string_predicate = function
		| "true" | "false" -> true
		| _ -> false
	let int_string_predicate s =
		try ignore (int_of_string s); true with _ -> false
	let uint_string_predicate s =
		try (int_of_string s) >= 0 with _ -> false

	let getter_argtype_str = function
		| Get_Byte _    -> "byte"
		| Get_Bool _    -> "bool"
		| Get_Int16 _   -> "int16"
		| Get_UInt16 _  -> "uint16"
		| Get_Int32 _   -> "int32"
		| Get_UInt32 _  -> "uint32"
		| Get_Int64 _   -> "int64"
		| Get_UInt64 _  -> "uint64"
		| Get_Double _  -> "double"
		| Get_String _  -> "string"
		| Get_ObjectPath _  -> "objectpath"

	let setter_argtype_str = function
		| Set_Byte _    -> "byte"
		| Set_Bool _    -> "bool"
		| Set_Int16 _   -> "int16"
		| Set_UInt16 _  -> "uint16"
		| Set_Int32 _   -> "int32"
		| Set_UInt32 _  -> "uint32"
		| Set_Int64 _   -> "int64"
		| Set_UInt64 _  -> "uint64"
		| Set_Double _  -> "double"
		| Set_String _  -> "string"
		| Set_ObjectPath _  -> "objectpath"
		| Set_Immutable -> "immutable"

	type t = getter * setter

	let separate_errors_rvals results =
		let errors, rvals = (List.fold_left
					(fun (errors, rvals) r ->
						match r with
						| Left ((c,m,a) as e) -> (e :: errors), rvals
						| Right r -> errors, (r :: rvals)
					) ([],[]) results)
		in List.rev errors, List.rev rvals

	let dispatch_get properties pname =
		let opt_getter = (try Some (fst (List.assoc pname properties))
				  with Not_found -> None)
		in match opt_getter with
		| Some (Get_Byte f)   -> Right (DBus.Byte   (f ()))
		| Some (Get_Bool f)   -> Right (DBus.Bool   (f ()))
		| Some (Get_Int16 f)  -> Right (DBus.Int16  (f ()))
		| Some (Get_UInt16 f) -> Right (DBus.UInt16 (f ()))
		| Some (Get_Int32 f)  -> Right (DBus.Int32  (f ()))
		| Some (Get_UInt32 f) -> Right (DBus.UInt32 (f ()))
		| Some (Get_Int64 f)  -> Right (DBus.Int64  (f ()))
		| Some (Get_UInt64 f) -> Right (DBus.UInt64 (f ()))
		| Some (Get_Double f) -> Right (DBus.Double (f ()))
		| Some (Get_String f) -> Right (DBus.String (f ()))
		| Some (Get_ObjectPath f) -> Right (DBus.ObjectPath (f ()))
		| None ->
			Left (DBus.ERR_INVALID_ARGS,
				(Printf.sprintf "Unknown property \"%s\"" pname),
				[])

	let dispatch_getall properties pnames =
		let results = (try List.map (fun p -> dispatch_get properties p) pnames
			       with e -> [ Left (DBus.ERR_FAILED, "exception",
						 [DBus.String (Printexc.to_string e)]) ]) in
		let errors, rvals = separate_errors_rvals results
		in match errors, rvals with
		| _ :: _, _ -> Left errors
		| [], _     -> Right rvals

	let dispatch_set properties pname dbus_ty =
		let opt_setter = (try Some (snd (List.assoc pname properties))
				  with Not_found -> None) in
		let invalid_error = Left (DBus.ERR_INVALID_ARGS, "invalid value", [])
		in match opt_setter, dbus_ty with
		| Some (Set_Byte (p, f)),   DBus.Byte v   ->
			if p v then begin f v; Right [] end
			else invalid_error
		| Some (Set_Bool (p, f)),   DBus.Bool v   ->
			if p v then begin f v; Right [] end
			else invalid_error
		| Some (Set_Int16 (p, f)),  DBus.Int16 v  ->
			if p v then begin f v; Right [] end
			else invalid_error
		| Some (Set_UInt16 (p, f)), DBus.UInt16 v ->
			if p v then begin f v; Right [] end
			else invalid_error
		| Some (Set_Int32 (p, f)),  DBus.Int32 v  ->
			if p v then begin f v; Right [] end
			else invalid_error
		| Some (Set_UInt32 (p, f)), DBus.UInt32 v ->
			if p v then begin f v; Right [] end
			else invalid_error
		| Some (Set_Int64 (p, f)),  DBus.Int64 v  ->
			if p v then begin f v; Right [] end
			else invalid_error
		| Some (Set_UInt64 (p, f)), DBus.UInt64 v ->
			if p v then begin f v; Right [] end
			else invalid_error
		| Some (Set_Double (p, f)), DBus.Double v ->
			if p v then begin f v; Right [] end
			else invalid_error
		| Some (Set_String (p, f)), DBus.String v ->
			if p v then begin f v; Right [] end
			else invalid_error
		| Some (Set_ObjectPath (p, f)), DBus.ObjectPath v ->
			if p v then begin f v; Right [] end
			else invalid_error
		| Some Set_Immutable, _ ->
			Left (DBus.ERR_FAILED, "immutable property",
			      [ DBus.String (Printf.sprintf "Property \"%s\" is immutable" pname) ]
			     )
		| Some g, _ ->
			Left (DBus.ERR_INVALID_ARGS, "invalid arg(s)",
			      [ DBus.String (
					Printf.sprintf "Invalid arg for property \"%s\": %s received, %s expected"
					  pname (DBus.string_of_ty dbus_ty) (setter_argtype_str g)
				)
			      ])
		| None, _ ->
			Left (DBus.ERR_INVALID_ARGS, "Unknown property",
				[ DBus.String (Printf.sprintf "Unknown property \"%s\"" pname) ])

	let dispatch_setall properties pargs =
		let results = (try List.map (fun (p, a) -> dispatch_set properties p a) pargs
			       with e -> [ Left (DBus.ERR_FAILED, "exception",
						 [DBus.String (Printexc.to_string e)]) ]) in
		let errors, rvals = separate_errors_rvals results
		in match errors, rvals with
		| _ :: _, _ -> Left errors
		| [], _     -> Right (List.concat rvals)
end

(* Server interface *)

(* The response of a method call can either be sent synchronously
   using Done, or later asynchronously using Pending.

   No state is kept in this module for Pending results; the
   implementation of the method is responsible to eventually send a
   response.  The message can be sent later either from the main
   thread using send_msg is usual, or from another thread using
   async_send_msg.  async_send_msg should _not_ be used from the main
   thread (this will cause a deadlock), and send_msg should _not_ be
   used from another thread (this is not thread-safe).
*)
type meth_result =
	| Done of DBus.message
	| Pending
type meth = DBus.message -> DBus.ty list -> meth_result

type interface =
{
	mutable methods    : ((* meth name *) string * meth) list;
}
type node =
{
	mutable interfaces : ((* interface name *) string * interface) list;
	mutable properties : ((* prop name *) string * Property.t) list;
}

let apis = ref ([] : ((* node name *) string * node) list)

let register_node name node =
	debug "Registering node %S ..." name;
	apis := (name, node) :: !apis

let remove_node name =
	(* Remove child nodes as well. *)
	debug "Removing node %S and its children..." name;
	apis := List.filter (fun (n, _) -> not (String.startswith name n)) !apis

let dispatch_getall req node props =
	match Property.dispatch_getall node.properties props with
	| Left ((err_name, err_msg, err_args) :: _) ->
		send_error req err_name ~err_args err_msg
	| Left [] ->
		(* We should always have at least one error in error cases. *)
		assert false
	| Right rvals ->
		send_response req rvals

let dispatch_setall req node prop_args =
	match Property.dispatch_setall node.properties prop_args with
	| Left ((err_name, err_msg, err_args) :: _) ->
		send_error req err_name ~err_args err_msg
	| Left [] ->
		(* We should always have at least one error in error cases. *)
		assert false
	| Right rvals ->
		send_response req rvals

let dispatch_property_interface req node m =
	let send_invalid_property_arg_error () =
		send_error req DBus.ERR_FAILED "Unexpected arg(s)" in
	let send_unknown_method () =
		send_error req DBus.ERR_FAILED m in
	let args = DBus.Message.get req in
	let to_prop_names args =
		let helper acc arg =
			match acc, arg with
			| Some pnames, DBus.String pname -> Some (pname :: pnames)
			| _, _    -> None
		in List.fold_left helper (Some []) args in
	let to_prop_name_arg_pairs args =
		let rec helper acc args =
			match acc, args with
			| Some pairs, ((DBus.String pname) :: pval :: rest) ->
				helper (Some ((pname, pval) :: pairs)) rest
			| _, [] -> acc
			| _, _  -> None
		in helper (Some []) args
	in match m with
	| "Get" ->
		(match to_prop_names args with
		 | Some [ pname ] ->
		 	dispatch_getall req node [ pname ]    
		 | _ ->
			send_invalid_property_arg_error ()
		)
	 | "Set" ->
		(match to_prop_name_arg_pairs args with
		 | Some [ pname, pval ] ->
		 	dispatch_setall req node [ pname, pval ]
		 | _ ->
			send_invalid_property_arg_error ()
		)
	 | "GetAll" ->
		(match to_prop_names args with
		 | Some pnames ->
		 	dispatch_getall req node (List.rev pnames)
		 | _ ->
			send_invalid_property_arg_error ()
		)
	 | "SetAll" ->
		(match to_prop_name_arg_pairs args with
		 | Some pargs ->
		 	dispatch_setall req node (List.rev pargs)
		 | _ ->
			send_invalid_property_arg_error ()
		)
	 | _ ->
		send_unknown_method ()

let dispatch_interface req node interface m =
	let send_unknown_method () =
		send_error req DBus.ERR_FAILED m in
	let meth = (try Some (List.assoc m interface.methods)
		    with Not_found -> None) in
	match meth with
	| None ->
		warn "Unknown method \"%s\" ...\n%!" m;
		send_unknown_method ()
	| Some m ->
		(match m req (DBus.Message.get req) with
		 | Done resp -> send_msg resp
		 | Pending   -> ()
		)

let dispatch_api req n i m =
	let send_unknown_node_error () =
		send_error req DBus.ERR_FAILED n in
	let send_invalid_interface_error () =
		send_error req DBus.ERR_FAILED i in
	let opt_node =
		try Some (List.assoc n !apis)
		with Not_found -> None in
	let opt_interface =
		try (match opt_node with None -> None | Some node -> Some (List.assoc i node.interfaces))
		with Not_found -> None
	in
	match opt_node, i, opt_interface with
	| None, _, _ ->
		warn "Unknown node %S ..." n;
		send_unknown_node_error ()
	| Some node, "org.freedesktop.DBus.Properties", _ ->
		dispatch_property_interface req node m
	| _, _, None ->
		warn "Unknown interface %S for node %S..." i n;
		send_invalid_interface_error ()
	| Some node, _, Some interface ->
		dispatch_interface req node interface m

let dispatch_request msg =
	let node = DBus.Message.get_path msg in
	let intf = DBus.Message.get_interface msg in
	let meth = DBus.Message.get_member msg in
	match node, intf, meth with
	|      _,      _,   None ->
		warn "Missing method";
		send_error msg DBus.ERR_INVALID_ARGS "Missing method"
	|      _,   None,      _ ->
		warn "Missing interface";
		send_error msg DBus.ERR_INVALID_ARGS "Missing interface"
	|   None,      _,      _ ->
		warn "Missing object";
		send_error msg DBus.ERR_INVALID_ARGS "Missing object"
	| Some n, Some i, Some m ->
		dispatch_api msg n i m

(* Incoming callbacks from connection *)

let error_callback conn err =
	warn "Received error."

let msg_received_callback conn m =
	debug "Received %s:" (DBus.Message.string_of_message_ty (DBus.Message.get_type m));
	dump_msg m;
	(match DBus.Message.get_type m with
	 | DBus.Message.Invalid       -> ()
	 | DBus.Message.Method_call   -> dispatch_request m
	 | DBus.Message.Signal        -> dispatch_signal m

	 (* Use same handler to process responses as well as errors
	    (which could be responses).
	 *)
	 | DBus.Message.Error
	 | DBus.Message.Method_return -> dispatch_response m
	)

let init_connection ?use_session_bus:(session=false) el =
	let callbacks =
	{
		Dbus_conn.msg_received_callback = msg_received_callback;
		Dbus_conn.error_callback = error_callback;
	} in
	let bus = DBus.Bus.get (if session then DBus.Bus.Session else DBus.Bus.System) in
	let conn = Dbus_conn.attach bus el callbacks in
	set_connection conn;
	Dbus_conn.enable_recv conn;
	conn

(* To allow threads to send messages (usually responses)
   asynchronously on the DBus connection, without sharing the
   connection across all the possible threads, we use an event
   channel.

   Threads will need to call 'async_send_msg msg' to post (in a blocking
   manner) the message to the event channel.  The eventloop will
   monitor the event channel in a non-blocking manner using
   Event.poll.  If any message is retrieved, then it is sent as is on
   the underlying shared DBus connection.
*)

let async_message_channel = (Event.new_channel () : DBus.message Event.channel)

(* This blocks the calling thread.  Do _not_ use this from the main
   thread; this will cause deadlock. *)
let async_send_msg msg =
	Event.sync (Event.send async_message_channel msg)

let dispatch () =
	(* Dispatch a completely received message (if any) in the
	   libdbus incoming message queue.  If there is more than one
	   such message, then we will dispatch them in subsequent
	   calls.  We cannot dispatch them all here since the
	   libdbus API does not provide a way to distinguish a
	   completely received message (which can be dispatched) and
	   an incompletely received message (which cannot be
	   dispatched).
	*)
	Dbus_conn.dispatch (Opt.unbox !dbus_conn_ref);

	(* Now, check if we have any messages posted from other
	   threads that we can send now. *)
	let rec sender () =
		match Event.poll (Event.receive async_message_channel) with
		| Some msg ->
			send_msg msg;
			(* Check if we have any more to send. *)
			sender ()
		| None ->
			(* We've emptied out the queue, and we're done. *)
			()
	in sender ()
