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

val close_and_exec_prg : string
val close_and_exec_path : string
exception Close_and_exec_binary_missing of string
val close_and_exec_cmdline :
  ?withpath:bool ->
  Unix.file_descr list -> string -> string list -> string list
val fork_and_exec :
  ?withpath:bool ->
  ?pre_exec:(unit -> unit) -> ?env:string array -> string list -> int
type fd_operation =
    Dup2 of Unix.file_descr * Unix.file_descr
  | Close of Unix.file_descr
val do_fd_operation : fd_operation -> unit
val safe_close_and_exec :
  ?withpath:bool ->
  ?env:string array ->
  fd_operation list -> Unix.file_descr list -> string -> string list -> int
exception Subprocess_failed of int
exception Subprocess_killed of int
val waitpid : int -> unit
type 'a result = Success of string * 'a | Failure of string * exn
val with_logfile_fd : string -> (Unix.file_descr -> 'a) -> 'a result
val with_dev_null : (Unix.file_descr -> 'a) -> 'a
exception Spawn_internal_error of string * string * Unix.process_status
val execute_command_get_output :
  ?withpath:bool ->
  ?env:string array ->	
  ?cb_set:(int -> unit) ->
  ?cb_clear:(unit -> unit) -> string -> string list -> string * string
