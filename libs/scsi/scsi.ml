(*
 * Copyright (C) 2006-2007 XenSource Ltd.
 * Copyright (C) 2008      Citrix Ltd.
 * Author Ross Philipson <ross.philipson@citrix.com>
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

(* scsi_inq_standard: device -> atamods -> inquiry *) 
external scsi_inq_standard : string -> int -> string = "stub_scsi_inq_standard"
(* scsi_inq_vpd: device -> vpdnum -> inquiry *) 
external scsi_inq_vpd : string -> int -> string = "stub_scsi_inq_vpd"
