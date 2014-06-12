(*
 * Copyright (c) 2012 Citrix Systems, Inc.
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

(* for the moment use Dm.info structure *)
(* dmpath is unused but it's to be compliant with Dm.start/restore prototype *)
val start : xc:Xc.handle -> xs:Xs.xsh -> dmpath:string
			-> ?timeout:float -> Dm.info -> Xc.domid
			-> (int option) * (int option)

(* Signal all dm-agent that domain will be destroy *)
val stop : xs:Xs.xsh -> Xc.domid -> unit
