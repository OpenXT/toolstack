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
 * Copyright (c) 2012 Citrix Systems, Inc.
 *)


open Threadext
open Vmconfig

type vmlifestate =
	| VmCreatingDomain | VmCreatingDevices | VmCreated
	| VmShutdown | VmShutdowning
	| VmRebooted | VmRebooting
	| VmPaused | VmSuspending | VmSuspended | VmRestoring
	| VmRunning

let string_of_vmlifestate state =
	match state with
	| VmCreatingDomain  -> "creating-domain"
	| VmCreatingDevices -> "creating-devices"
	| VmCreated -> "created"
	| VmShutdown -> "shutdown"
	| VmShutdowning -> "shutdowning"
	| VmRebooting -> "rebooting"
	| VmRebooted -> "rebooted"
	| VmPaused -> "paused"
	| VmSuspending -> "suspending" | VmSuspended -> "suspended"
	| VmRestoring -> "restoring"
	| VmRunning -> "running"

type locks = {
	l_start: Mutex.t;
}

type monitor_state = {
	mutable monitor_use_dbus: bool;
	mutable monitor_use_dbus_session: bool;
	mutable monitor_use_json: bool;
	mutable monitor_json_quit: bool;
	mutable monitor_dbus_quit: bool;
}

type nic_state = {
        ns_id : int;
        mutable ns_netty: Netman.netty;
}

type vm_state = {
	vm_uuid: string;
	vm_monitors: monitor_state;
	quit_mutex: Mutex.t;
	quit_condition: Condition.t;
	mutable quit_requested: bool;
	mutable vm_config_path: string;
	mutable vm_arch: Domain.domarch;
	mutable vm_domid: int;
	mutable vm_stubdomid: int option;
	mutable vm_vnc_port: int option;
	mutable vm_lifestate: vmlifestate;
	mutable vm_tap2_disks: (Vmconfig.config_disk * string * string option) list;
	mutable vm_passthrough_mmios: (int64 * int64) list;
	mutable vm_passthrough_ios: (int * int) list;
        mutable vm_pcis: Vmconfig.config_pci list;
        mutable vm_nics: nic_state list;
	mutable vm_on_suspend_action: Vmconfig.action;
	mutable vm_cfg: Vmconfig.config;
	mutable vm_next_cfg: Vmconfig.config option;
	mutable vm_expect_shutdown: bool;
	mutable vm_power_state: int;
	locks: locks;
}

let state_init uuid config_path cfg =
	{
		vm_uuid = uuid;
		vm_monitors = {
			monitor_use_dbus = false;
			monitor_use_dbus_session = false;
			monitor_use_json = true;
			monitor_json_quit = false;
			monitor_dbus_quit = false;
		};
		quit_mutex = Mutex.create ();
		quit_condition = Condition.create ();
		quit_requested = false;
		vm_config_path = config_path;
		vm_arch = if cfg.hvm then Domain.Arch_HVM else Domain.Arch_native;
		vm_domid = (-1);
		vm_stubdomid = None;
		vm_lifestate = VmShutdown;
		vm_vnc_port = None;
		vm_tap2_disks = [];
		vm_passthrough_mmios = [];
		vm_passthrough_ios = [];
                vm_nics = [];
	        vm_pcis = [];
		vm_on_suspend_action = ActionSuspend;
		vm_cfg = cfg;
		vm_next_cfg = None;
		vm_expect_shutdown = false;
		vm_power_state = 5;
		locks = {
			l_start = Mutex.create ();
		}
	}

let with_create_lock state f =
	Mutex.execute state.locks.l_start f
