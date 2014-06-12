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

let write_string file mode s =
	let fn_write_string fd = Unixext.really_write fd s 0 (String.length s) in
	Unixext.with_file file (Unix.O_WRONLY :: mode) 0o640 fn_write_string

let write_fn file mode fn =
	let fn_write_fn fd =
		let quit = ref false in
		while not !quit
		do
			let s = fn () in
			if s = "" then
				quit := true
			else
				Unixext.really_write fd s 0 (String.length s)
		done
		in
	Unixext.with_file file (Unix.O_WRONLY :: mode) 0o640 fn_write_fn
