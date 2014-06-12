(*
 * Copyright (C) 2006-2007 XenSource Ltd.
 * Copyright (C) 2008-2009 Citrix Ltd.
 * Author Vincent Hanquez <vincent.hanquez@eu.citrix.com>
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

(*
 * Copyright (c) 2011 Citrix Systems, Inc.
 *)


open Pervasiveext

let with_xal f = let xal = Xal.init () in finally (fun () -> f xal) (fun () -> Xal.close xal)

let create_ext3fs_on dev =
	let _ = Unixext.spawnvp "mkfs.ext3" [| "mkfs.ext3"; dev |] in
	let _ = Unixext.spawnvp "tune2fs" [| "tune2fs"; "-i"; "0"; "-c"; "0"; dev |] in
	()

let vhd_create_snapshot src dst =
	let _ = Unixext.spawnvp "td-util" [| "td-util"; "snapshot"; "vhd"; dst; src |] in
	()

let vhd_set_key p keyfile =
	let _ = Forkhelpers.execute_command_get_output
		~withpath:true
		"/usr/sbin/vhd-util"
		["key"; "-s"; "-n"; p; "-k"; keyfile] in
	()

let keybytes = 32

let vhd_gen_random_key () =
	let rec charlist_of s i =
		if i >= String.length s
		then []
		else int_of_char (String.get s i) :: charlist_of s (i+1)
	in
	let fd = Unix.openfile "/dev/urandom" [ Unix.O_RDONLY ] 0 in
	let buf = String.create keybytes in
	Unixext.really_read fd buf 0 keybytes;
	Unix.close fd;
	charlist_of buf 0

let vhd_write_key file key =
	let buf = String.create keybytes in
	let rec fill s src i =
		if i >= keybytes
		then ()
		else ( match src with
			| (x::xs) -> String.set s i (char_of_int x); fill s xs (i+1)
			| _ -> failwith "incorrect key"
		)
	in
	fill buf key 0;

	let fd = Unix.openfile file [ Unix.O_WRONLY;  Unix.O_CREAT ] 0o644 in
	Unixext.really_write fd buf 0 keybytes;
	Unix.close fd

let vhd_coalesce_snapshot src dst =
	let _ = Unixext.spawnvp "vhd-util" [| |] in
	()

let with_mounted_fs dev mntpoint f =
	let _ = Unixext.spawnvp "mount" [| "mount"; "-t"; "ext3"; dev; mntpoint |] in
	finally (fun () -> f ())
		(fun () -> let _ = Unixext.spawnvp "unmount" [| "unmount"; dev |]; in ())

