(*
 * Copyright (C) 2006-2007 XenSource Ltd.
 * Copyright (C) 2008      Citrix Ltd.
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

open Printf

let error str code =
	printf "error: %s\n" str; exit code

let parse cmd argv =
	let prefix = ref false and sock = ref false and tidy = ref false in

	let spec_prefix = ("-p", Arg.Unit (fun () -> prefix := true),
	                   "Use a prefix")
	and spec_sock = ("-s", Arg.Unit (fun () -> sock := true),
	                 "use xs socket instead of a xenbus")
	and spec_tidy = ("-t", Arg.Unit (fun () -> tidy := true),
	                 "remove all empty directory") in

	let cmdline_argv =
		[ ("read", ([ spec_prefix; spec_sock; ],
			"[-h] [-p] [-s] key [...]"));
		  ("write", ([ spec_prefix; spec_sock; ],
			"[-h] [-p] [-s] key value"));
		  ("rm", ([ spec_prefix; spec_sock; spec_tidy; ],
			"[-h] [-p] [-s] [-t] key [...]"));
		  ("list", ([ spec_prefix; spec_sock ],
			"[-h] [-p] [-s] key [...]"));
		  ("ls", ([ spec_sock ],
		        "[-h] [-s] [paths ...]"));
		  ("chmod", ([ spec_sock ],
		        "[-h] [-s] key <mode [modes...]>"));
		] in

	let speclist, usage = List.assoc cmd cmdline_argv in

	let args = ref [] in
	let anonfct s = (args := s :: !args) in

	Arg.parse_argv argv speclist anonfct usage;
	if List.length !args = 0 then
		error (sprintf "xenstore %s: %s" cmd usage) 3;
	(List.rev !args), !prefix, !sock, !tidy

let do_read prefix tidy argv buf t =
	List.iter (fun path ->
		let ret = t.Xst.read path in
		if prefix then bprintf buf "%s: " path;
		bprintf buf "%s\n" ret) argv

let rec do_write prefix tidy argv buf t =
	match argv with
	| path :: value :: l ->
		t.Xst.write path value;
		do_write prefix tidy l buf t
	| [] -> ()
	| _  -> error "not enough arguments" 2

let do_rm prefix tidy argv buf t =
	let do_tidy path =
		() in
	List.iter (fun path ->
		if tidy then
			do_tidy path
		else
			t.Xst.rm path) argv

let do_list prefix tidy argv buf t =
	List.iter (fun path ->
		let dirs = t.Xst.directory path in
		List.iter (fun dir ->
			if prefix then bprintf buf "%s/" path;
			bprintf buf "%s\n" dir) dirs) argv

let do_ls prefix tidy argv xs =
	let rec ls indent path =
		let entries = try xs.Xs.directory path
		              with _ -> [] in
		List.iter (fun entry -> if entry <> "" then (
			let v =
				try "\"" ^ xs.Xs.read entry ^ "\""
				with _ -> "<CANTREAD>" in
			let x = String.make (indent * 1) ' ' in
			printf "%s%s = %s\n" x entry v;
			ls (indent + 1) (path ^ "/" ^ entry)
			)
		) entries
		in
	let argv = if argv = [] then [ "/" ] else argv in
	List.iter (ls 0) argv

let do_chmod prefix tidy argv buf t =
	match argv with
	| path :: p0 :: l ->
		let perms = Xsraw.perms_of_string (String.concat "" (p0 :: l)) in
		t.Xst.setperms path perms
	| _  -> error "not enough arguments" 2

let process cmd argv fct =
	let args, prefix, sock, tidy = parse cmd argv in
	let buf = Buffer.create 1024 in
	let xsh = if sock then Xs.daemon_open () else Xs.domain_open () in

	let tloop t =
		Buffer.reset buf;
		fct prefix tidy args buf t;
		in
	ignore (Xs.transaction xsh tloop);
	Xs.close xsh;
	buf

let process_natomic cmd argv fct =
	let args, prefix, sock, tidy = parse cmd argv in
	let xsh = if sock then Xs.daemon_open () else Xs.domain_open () in
	fct prefix tidy args xsh;
	Xs.close xsh;
	(* return an empty buffer *)
	Buffer.create 1

let process_cmd () =
	if Array.length Sys.argv < 2 then
		error "not enough arguments" 3;
	let argv = Array.sub Sys.argv 1 (Array.length Sys.argv - 1) in
	match argv.(0) with
	| "read"  -> process "read" argv do_read
	| "write" -> process "write" argv do_write
	| "rm"    -> process "rm" argv do_rm
	| "list"  -> process "list" argv do_list
	| "ls"    -> process_natomic "ls" argv do_ls
	| "chmod" -> process "chmod" argv do_chmod
	| _       -> error "unknown cmd" 2

let _ =
	let prg = (Filename.basename Sys.argv.(0)) in
	let buf = match prg with
	| "xenstore-read"  -> process "read" Sys.argv do_read
	| "xenstore-write" -> process "write" Sys.argv do_write
	| "xenstore-rm"    -> process "rm" Sys.argv do_rm
	| "xenstore-list"  -> process "list" Sys.argv do_list
	| "xenstore-ls"    -> process_natomic "ls" Sys.argv do_ls
	| "xenstore-chmod" -> process "chmod" Sys.argv do_chmod
	| "xenstore"       -> process_cmd ()
	| _                -> error "Unknown program name" 3;

	in

	printf "%s" (Buffer.contents buf)
