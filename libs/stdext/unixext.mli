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
external _exit : int -> unit = "unix_exit"
val unlink_safe : string -> unit
val mkdir_safe : string -> Unix.file_perm -> unit
val mkdir_rec : string -> Unix.file_perm -> unit
val pidfile_write : string -> unit
val pidfile_read : string -> int option
val daemonize : unit -> unit
val with_file : string -> Unix.open_flag list -> Unix.file_perm -> (Unix.file_descr -> 'a) -> 'a
val with_directory : string -> (Unix.dir_handle -> 'a) -> 'a
val readfile_line : (string -> 'a) -> string -> unit
val read_whole_file : int -> int -> Unix.file_descr -> string
val read_whole_file_to_string : string -> string
val write_string_to_file : string -> string -> unit
val execv_get_output : string -> string array -> int * Unix.file_descr
val copy_file : ?limit:int64 -> Unix.file_descr -> Unix.file_descr -> int64
exception Host_not_found of string
val open_connection_fd : string -> int -> Unix.file_descr
val open_connection_unix_fd : string -> Unix.file_descr
type endpoint = {
  fd : Unix.file_descr;
  mutable buffer : string;
  mutable buffer_len : int;
}
exception Process_still_alive
val kill_and_wait : ?signal:int -> ?timeout:float -> int -> unit
val make_endpoint : Unix.file_descr -> endpoint
val proxy : Unix.file_descr -> Unix.file_descr -> unit
val really_read : Unix.file_descr -> string -> int -> int -> unit
val really_write : Unix.file_descr -> string -> int -> int -> unit
val spawnvp :
  ?pid_callback:(int -> unit) ->
  string -> string array -> Unix.process_status
val double_fork : (unit -> unit) -> unit
external set_tcp_nodelay : Unix.file_descr -> bool -> unit
  = "stub_unixext_set_tcp_nodelay"
external fsync : Unix.file_descr -> unit = "stub_unixext_fsync"
external get_max_fd : unit -> int = "stub_unixext_get_max_fd"
val int_of_file_descr : Unix.file_descr -> int
val file_descr_of_int : int -> Unix.file_descr
val close_all_fds_except : Unix.file_descr list -> unit
val get_process_output : ?handler:(string -> int -> string) -> string -> string
val resolve_dot_and_dotdot : string -> string

val flock_sh : Unix.file_descr -> unit
val flock_ex : Unix.file_descr -> unit
val flock_un : Unix.file_descr -> unit

val with_flock_sh : Unix.file_descr -> ( unit -> 'a ) -> 'a
val with_flock_ex : Unix.file_descr -> ( unit -> 'a ) -> 'a

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

val statfs: string -> statfs_t
val get_major_minor : string -> int * int 
val gettimeofday_monotonic : unit -> float

module Fdset : sig
	type t
	val create : unit -> t
	external of_list : Unix.file_descr list -> t = "stub_fdset_of_list"
	external is_set : t -> Unix.file_descr -> bool = "stub_fdset_is_set"
	external set : t -> Unix.file_descr -> unit = "stub_fdset_set"
	external clear : t -> Unix.file_descr -> unit = "stub_fdset_clear"

	val select : t -> t -> t -> float -> t * t * t
	val select_ro : t -> float -> t
end
