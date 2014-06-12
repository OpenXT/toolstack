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
open Pervasiveext

exception Unix_error of int

external _exit : int -> unit = "unix_exit"

(** remove a file, but doesn't raise an exception if the file is already removed *)
let unlink_safe file =
	try Unix.unlink file with (* Unix.Unix_error (Unix.ENOENT, _ , _)*) _ -> ()

(** create a directory but doesn't raise an exception if the directory already exist *)
let mkdir_safe dir perm =
	try Unix.mkdir dir perm with Unix.Unix_error (Unix.EEXIST, _, _) -> ()

(** create a directory, and create parent if doesn't exist *)
let mkdir_rec dir perm =
	let rec p_mkdir dir =
		let p_name = Filename.dirname dir in
		if p_name <> "/" && p_name <> "." 
		then p_mkdir p_name;
		mkdir_safe dir perm in
	p_mkdir dir

(** write a pidfile file *)
let pidfile_write filename =
	let fd = Unix.openfile filename
	                       [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC; ]
			       0o640 in
	finally
	(fun () ->
		let pid = Unix.getpid () in
		let buf = string_of_int pid ^ "\n" in
		let len = String.length buf in
		if Unix.write fd buf 0 len <> len 
		then failwith "pidfile_write failed";
	)
	(fun () -> Unix.close fd)

(** read a pidfile file, return either Some pid or None *)
let pidfile_read filename =
	let fd = Unix.openfile filename [ Unix.O_RDONLY ] 0o640 in
	finally
	(fun () ->
		try
			let buf = String.create 80  in
			let rd = Unix.read fd buf 0 (String.length buf) in
			if rd = 0 then
				failwith "pidfile_read failed";
			Scanf.sscanf (String.sub buf 0 rd) "%d" (fun i -> Some i)
		with exn -> None)
	(fun () -> Unix.close fd)

(** daemonize a process *)
(* !! Must call this before spawning any threads !! *)
let daemonize () =
	match Unix.fork () with
	| 0 ->
		if Unix.setsid () == -1 then
			failwith "Unix.setsid failed";

		begin match Unix.fork () with
		| 0 ->
			let nullfd = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0 in
			begin try
				Unix.close Unix.stdin;
				Unix.dup2 nullfd Unix.stdout;
				Unix.dup2 nullfd Unix.stderr;
			with exn -> Unix.close nullfd; raise exn
			end;
			Unix.close nullfd
		| _ -> exit 0
		end
	| _ -> exit 0

(** Run a function over every line in a file *)
let readfile_line fn fname =
	let fin = open_in fname in
	try
		while true do
			let line = input_line fin in
			fn line
		done;
		close_in fin;
	with
	| End_of_file -> close_in fin
	| exn -> close_in fin; raise exn

(** open a file, and make sure the close is always done *)
let with_file file mode perms f =
	let fd = Unix.openfile file mode perms in
	let r =
		try f fd
		with exn -> Unix.close fd; raise exn
		in
	Unix.close fd;
	r

let with_directory dir f =
	let dh = Unix.opendir dir in
	let r =
		try f dh
		with exn -> Unix.closedir dh; raise exn
		in
	Unix.closedir dh;
	r

(** Read whole file from specified fd *)
let read_whole_file size_hint block_size fd =
  let filebuf = Buffer.create size_hint in
  let blockbuf = String.create block_size in
  let rec do_read() =
    let nread = Unix.read fd blockbuf 0 block_size in
      if nread=0 then
	Buffer.contents filebuf
      else
	begin
	  Buffer.add_substring filebuf blockbuf 0 nread;
	  do_read()
	end in
    do_read()

(** Read whole file into string *)
let read_whole_file_to_string fname =
	with_file fname [ Unix.O_RDONLY ] 0o0 (read_whole_file 1024 1024)

(** Atomically write a string to a file *)
let write_string_to_file fname s =
  let tmp = Filenameext.temp_file_in_dir fname in
  Pervasiveext.finally 
    (fun () ->
       let fd = Unix.openfile tmp [Unix.O_WRONLY; Unix.O_CREAT] 0o644 in
       Pervasiveext.finally 
	 (fun () -> 
	   let len = String.length s in
	   let written = Unix.write fd s 0 len in
	   if written <> len then (failwith "Short write occured!"))
	 (fun () -> Unix.close fd);
       Unix.rename tmp fname)
    (fun () -> unlink_safe tmp)

let execv_get_output cmd args =
	let (pipe_exit, pipe_entrance) = Unix.pipe () in
	let r = try Unix.set_close_on_exec pipe_exit; true with _ -> false in
	match Unix.fork () with
	| 0 ->
		Unix.dup2 pipe_entrance Unix.stdout;
		Unix.close pipe_entrance;
		if not r then
			Unix.close pipe_exit;
		begin try Unix.execv cmd args with _ -> exit 127 end
	| pid ->
		Unix.close pipe_entrance;
		pid, pipe_exit

(** Copy all data from an in_channel to an out_channel,
 * returning the total number of bytes *)
let copy_file ?limit ifd ofd =
	let buffer = String.make 65536 '\000' in
	let buffer_len = Int64.of_int (String.length buffer) in
	let finished = ref false in
	let total_bytes = ref 0L in
	let limit = ref limit in
	while not(!finished) do
		let requested = min (Opt.default buffer_len !limit) buffer_len in
		let num = Unix.read ifd buffer 0 (Int64.to_int requested) in
		let num64 = Int64.of_int num in

		limit := Opt.map (fun x -> Int64.sub x num64) !limit;
		let wnum = Unix.write ofd buffer 0 num in
		total_bytes := Int64.add !total_bytes num64;
		finished := wnum = 0 || !limit = Some 0L;
	done;
	!total_bytes

(** Create a new file descriptor, connect it to host:port and return it *)
exception Host_not_found of string
let open_connection_fd host port =
	let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
	try 
	  let he =
	    try
	      Unix.gethostbyname host
	    with
		Not_found -> raise (Host_not_found host) in
	  if Array.length he.Unix.h_addr_list = 0
	  then failwith (Printf.sprintf "Couldn't resolve hostname: %s" host);
	  let ip = he.Unix.h_addr_list.(0) in
	  let addr = Unix.ADDR_INET(ip, port) in
	  Unix.connect s addr;
	  s
	with e -> Unix.close s; raise e


let open_connection_unix_fd filename =
	let s = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
	try
	  let addr = Unix.ADDR_UNIX(filename) in
	  Unix.connect s addr;
	  s
	with e -> Unix.close s; raise e

type endpoint = { fd: Unix.file_descr; mutable buffer: string; mutable buffer_len: int }

let make_endpoint fd = {
	fd = fd;
	buffer = String.make 4096 '\000';
	buffer_len = 0
}

exception Process_still_alive

let kill_and_wait ?(signal = Sys.sigterm) ?(timeout=10.) pid =
	let proc_entry_exists pid =
		try Unix.access (Printf.sprintf "/proc/%d" pid) [ Unix.F_OK ]; true
		with _ -> false
		in
	if pid > 0 && proc_entry_exists pid then (
		let loop_time_waiting = 0.03 in
		let left = ref timeout in
		let readcmdline pid =
			try read_whole_file_to_string (Printf.sprintf "/proc/%d/cmdline" pid)
			with _ -> ""
			in
		let reference = readcmdline pid and quit = ref false in
		Unix.kill pid signal;

		(* We cannot do a waitpid here, since we might not be parent of
		   the process, so instead we are waiting for the /proc/%d to go
		   away. Also we verify that the cmdline stay the same if it's still here
		   to prevent the very very unlikely event that the pid get reused before
		   we notice it's gone *)
		while proc_entry_exists pid && not !quit && !left > 0.
		do
			let cmdline = readcmdline pid in
			if cmdline = reference then (
				(* still up, let's sleep a bit *)
				ignore (Unix.select [] [] [] loop_time_waiting);
				left := !left -. loop_time_waiting
			) else (
				(* not the same, it's gone ! *)
				quit := true
			)
		done;
		if !left <= 0. then
			raise Process_still_alive;
	)

let proxy (a: Unix.file_descr) (b: Unix.file_descr) =
	let a' = make_endpoint a and b' = make_endpoint b in
	Unix.set_nonblock a;
	Unix.set_nonblock b;

	let can_read x =
		x.buffer_len < (String.length x.buffer - 1) in
	let can_write x =
		x.buffer_len > 0 in
	let write_from x fd =
		let written = Unix.single_write fd x.buffer 0 x.buffer_len in
		String.blit x.buffer written x.buffer 0 (x.buffer_len - written);
		x.buffer_len <- x.buffer_len - written in
	let read_into x =
		let read = Unix.read x.fd x.buffer x.buffer_len (String.length x.buffer - x.buffer_len) in
		if read = 0 then raise End_of_file;
		x.buffer_len <- x.buffer_len + read in

	try
	while true do
		let r = (if can_read a' then [ a ] else []) @ (if can_read b' then [ b ] else []) in
		let w = (if can_write a' then [ b ] else []) @ (if can_write b' then [ a ] else []) in

		let r, w, _ = Unix.select r w [] (-1.0) in
		(* Do the writing before the reading *)
		List.iter (fun fd -> if a = fd then write_from b' a else write_from a' b) w;
		List.iter (fun fd -> if a = fd then read_into a' else read_into b') r
	done
	with _ ->
		(try Unix.clear_nonblock a with _ -> ());
		(try Unix.clear_nonblock b with _ -> ());
		(try Unix.close a with _ -> ());
		(try Unix.close b with _ -> ())

let rec really_read fd string off n =
  if n=0 then () else
    let m = Unix.read fd string off n in
    if m = 0 then raise End_of_file;
    really_read fd string (off+m) (n-m)

let really_write fd string off n =
	let written = ref 0 in
	while !written < n
	do
		let wr = Unix.write fd string (off + !written) (n - !written) in
		written := wr + !written
	done

let spawnvp ?(pid_callback=(fun _ -> ())) cmd args =
	match Unix.fork () with
	| 0 ->
		Unix.execvp cmd args
	| pid ->
		begin try pid_callback pid with _ -> () end;
		snd (Unix.waitpid [] pid)

let double_fork f =
	match Unix.fork () with
	| 0 ->
		begin match Unix.fork () with
		  (* NB: use _exit (calls C lib _exit directly) to avoid
		     calling at_exit handlers and flushing output channels
		     which wouild cause intermittent deadlocks if we
		     forked from a threaded program *)
		| 0 -> (try f () with _ -> ()); _exit 0
		| _ -> _exit 0
		end
	| pid -> ignore(Unix.waitpid [] pid)

external set_tcp_nodelay : Unix.file_descr -> bool -> unit = "stub_unixext_set_tcp_nodelay"

external fsync : Unix.file_descr -> unit = "stub_unixext_fsync"

external get_max_fd : unit -> int = "stub_unixext_get_max_fd"

let int_of_file_descr (x: Unix.file_descr) : int = Obj.magic x
let file_descr_of_int (x: int) : Unix.file_descr = Obj.magic x

(** Forcibly closes all open file descriptors except those explicitly passed in as arguments.
    Useful to avoid accidentally passing a file descriptor opened in another thread to a
    process being concurrently fork()ed (there's a race between open/set_close_on_exec).
    NB this assumes that 'type Unix.file_descr = int' 
*)
let close_all_fds_except (fds: Unix.file_descr list) =
  (* get at the file descriptor within *)
  let fds' = List.map int_of_file_descr fds in
  let close' (x: int) = 
    try Unix.close(file_descr_of_int x) with _ -> () in

  let highest_to_keep = List.fold_left max (-1) fds' in
  (* close all the fds higher than the one we want to keep *)
  for i = highest_to_keep + 1 to get_max_fd () do close' i done;
  (* close all the rest *)
  for i = 0 to highest_to_keep - 1 do
    if not(List.mem i fds') then close' i
  done

exception Process_output_error of string
let get_process_output ?(handler) cmd : string =
	let inchan = Unix.open_process_in cmd in

	let buffer = Buffer.create 1024
	and buf = String.make 1024 '\000' in
	
	let rec read_until_eof () =
		let rd = input inchan buf 0 1024 in
		if rd = 0 then
			()
		else (
			Buffer.add_substring buffer buf 0 rd;
			read_until_eof ()
		) in
	(* Make sure an exception doesn't prevent us from waiting for the child process *)
	(try read_until_eof () with _ -> ());
	match (Unix.close_process_in inchan), handler with
	| Unix.WEXITED 0, _ -> Buffer.contents buffer
	| Unix.WEXITED n, Some handler -> handler cmd n
	| _ -> raise (Process_output_error cmd)

(** Remove "." and ".." from paths (NB doesn't attempt to resolve symlinks) *)
let resolve_dot_and_dotdot (path: string) : string = 
  let of_string (x: string): string list = 
    let rec rev_split path = 
      let basename = Filename.basename path 
      and dirname = Filename.dirname path in
      let rest = if Filename.dirname dirname = dirname then [] else rev_split dirname in
      basename :: rest in
    let abs_path path = 
      if Filename.is_relative path 
      then Filename.concat "/" path (* no notion of a cwd *)
      else path in
    rev_split (abs_path x) in
  
  let to_string (x: string list) = List.fold_left Filename.concat "/" (List.rev x) in
  
  (* Process all "." and ".." references *)
  let rec remove_dots (n: int) (x: string list) = 
    match x, n with
    | [], _ -> []
    | "." :: rest, _ -> remove_dots n rest (* throw away ".", don't count as parent for ".." *)
    | ".." :: rest, _ -> remove_dots (n + 1) rest (* note the number of ".." *)
    | x :: rest, 0 -> x :: (remove_dots 0 rest)
    | x :: rest, n -> remove_dots (n - 1) rest (* munch *) in
  to_string (remove_dots 0 (of_string path))

external flock_sh : Unix.file_descr -> unit = "stub_unixext_flock_sh"
external flock_ex : Unix.file_descr -> unit = "stub_unixext_flock_ex"
external flock_un : Unix.file_descr -> unit = "stub_unixext_flock_un"

let with_flock_sh fd f =
	finally (fun () -> flock_sh fd; f ())
		(fun () -> flock_un fd)

let with_flock_ex fd f =
	finally (fun () -> flock_ex fd; f ())
		(fun () -> flock_un fd)

type statfs_t = {
	statfs_type: int64;
	statfs_bsize: int;
	statfs_blocks: int64;
	statfs_bfree: int64;
	statfs_bavail: int64;
	statfs_files: int64;
	statfs_ffree: int64;
	statfs_namelen: int;
}

external statfs: string -> statfs_t = "stub_unixext_statfs"

external get_major_minor : string -> int * int = "stub_unixext_get_major_minor"

external gettimeofday_monotonic : unit -> float = "stub_unixext_gettimeofday_monotonic"

module Fdset = struct
	type t
	external of_list : Unix.file_descr list -> t = "stub_fdset_of_list"
	let create () = of_list []
	external is_set : t -> Unix.file_descr -> bool = "stub_fdset_is_set"
	external set : t -> Unix.file_descr -> unit = "stub_fdset_set"
	external clear : t -> Unix.file_descr -> unit = "stub_fdset_clear"
	external _select : t -> t -> t -> float -> t * t * t = "stub_fdset_select"
	external _select_ro : t -> float -> t = "stub_fdset_select_ro"
	let select r w e t = _select r w e t
	let select_ro r t = _select_ro r t
end

let _ = Callback.register_exception "unixext.unix_error" (Unix_error (0))
