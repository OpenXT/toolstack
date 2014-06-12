(*
 * Copyright (C) 2009      Citrix Ltd.
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
exception Failed_assoc of string
exception Failed_revassoc

type 'a table = (string * 'a) list

let assoc (table: 'a table) x =
	try snd (List.find (fun (a, b) -> x = a) table)
	with Not_found -> raise (Failed_assoc x)

let rev_assoc (table: 'a table) y =
	try fst (List.find (fun (a, b) -> y = b) table)
	with Not_found -> raise Failed_revassoc 
