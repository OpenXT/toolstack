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

module type BRAND = sig
  val name: string
end

let uid = ref None

module Debugger = functor(Brand: BRAND) -> struct
  let output (f:string -> ?extra:string -> ('a, unit, string, unit) format4 -> 'a) fmt =
    let extra =
      match !uid with
      | None -> Brand.name
      | Some uid -> Printf.sprintf "%s|%s" uid Brand.name
      in
    f Brand.name ~extra fmt

  let debug fmt = output Logs.debug fmt
  let warn fmt = output Logs.warn fmt
  let info fmt = output Logs.info fmt
  let error fmt = output Logs.error fmt
end
