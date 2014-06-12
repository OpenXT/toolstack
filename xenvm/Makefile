#
# Copyright (c) 2013 Citrix Systems, Inc.
# 
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2.1 of the License, or (at your option) any later version.
# 
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
# 
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
#

TOPLEVEL=..
include $(TOPLEVEL)/common.make

DBUS_DIR = `ocamlfind query dbus`
CAMOMILE_DIR = `ocamlfind query camomile`
BIGARRAY_DIR = `ocamlfind query bigarray`

c_libs = -cclib -L$(XEN_DIST_ROOT)/usr/lib -cclib -lxenctrl -cclib -lxenguest -cclib -lxenhvm -cclib -lxenstore

OCAMLINCLUDE += \
	-I $(TOPLEVEL)/libs/log -I $(TOPLEVEL)/libs/xb -I $(TOPLEVEL)/libs/xs -I $(TOPLEVEL)/libs/udev \
	-I $(TOPLEVEL)/libs/uuid -I $(TOPLEVEL)/libs/mmap -I $(TOPLEVEL)/libs/base64 \
	-I $(TOPLEVEL)/libs/xc -I $(TOPLEVEL)/libs/xg -I $(TOPLEVEL)/libs/eventchn -I $(TOPLEVEL)/libs/scsi \
	-I $(TOPLEVEL)/libs/json -I $(TOPLEVEL)/libs/jsonrpc \
	-I $(TOPLEVEL)/libs/netdev -I $(TOPLEVEL)/libs/stdext \
	-I $(TOPLEVEL)/libs/common -I $(TOPLEVEL)/xenops \
	-I $(DBUS_DIR) \
	-I $(BIGARRAY_DIR) \
	-I $(CAMOMILE_DIR)

OCAMLOPTFLAGS += -thread

xenvm_OBJS = xenvmlib tasks misc vmconfig vmstate vmact xenvm
xenvm-cmd_OBJS = xenvmlib xenvm_cmd
xenops_OBJS = xenops

ALL_OCAML_OBJS = xenvmlib xenvm xenvm_cmd misc tasks vmconfig vmstate vmact xenops

xenvm_LIBS = unix.cmxa camomile.cmxa dBus.cmxa threads.cmxa $(TOPLEVEL)/libs/uuid/uuid.cmxa \
	$(c_libs) \
	-ccopt -L -ccopt $(BIGARRAY_DIR) $(BIGARRAY_DIR)/bigarray.cmxa \
	-ccopt -L -ccopt $(CAMOMILE_DIR) $(CAMOMILE_DIR)/camomile.cmxa \
	-ccopt -L -ccopt $(DBUS_DIR) $(DBUS_DIR)/dBus.cmxa \
	-ccopt -L -ccopt $(TOPLEVEL)/libs/mmap $(TOPLEVEL)/libs/mmap/mmap.cmxa \
	-ccopt -L -ccopt $(TOPLEVEL)/libs/log $(TOPLEVEL)/libs/log/log.cmxa \
	-ccopt -L -ccopt $(TOPLEVEL)/libs/eventchn $(TOPLEVEL)/libs/eventchn/eventchn.cmxa \
	-ccopt -L -ccopt $(TOPLEVEL)/libs/xc $(TOPLEVEL)/libs/xc/xc.cmxa \
	-ccopt -L -ccopt $(TOPLEVEL)/libs/xg $(TOPLEVEL)/libs/xg/xg.cmxa \
	-ccopt -L -ccopt $(TOPLEVEL)/libs/xb $(TOPLEVEL)/libs/xb/xb.cmxa \
	-ccopt -L -ccopt $(TOPLEVEL)/libs/xs $(TOPLEVEL)/libs/xs/xs.cmxa \
	-ccopt -L -ccopt $(TOPLEVEL)/libs/udev $(TOPLEVEL)/libs/udev/udev.cmxa \
	-ccopt -L -ccopt $(TOPLEVEL)/libs/stdext $(TOPLEVEL)/libs/stdext/stdext.cmxa \
	-ccopt -L -ccopt $(TOPLEVEL)/libs/netdev $(TOPLEVEL)/libs/netdev/netdev.cmxa \
	-ccopt -L -ccopt $(TOPLEVEL)/libs/json $(TOPLEVEL)/libs/json/json.cmxa \
	-ccopt -L -ccopt $(TOPLEVEL)/libs/jsonrpc $(TOPLEVEL)/libs/jsonrpc/jsonrpc.cmxa \
	-ccopt -L -ccopt $(TOPLEVEL)/libs/scsi $(TOPLEVEL)/libs/scsi/scsi.cmxa \
	-ccopt -L -ccopt $(TOPLEVEL)/libs/base64 $(TOPLEVEL)/libs/base64/base64.cmxa \
	-ccopt -L -ccopt $(TOPLEVEL)/libs/common $(TOPLEVEL)/libs/common/tscommon.cmxa \
	-ccopt -L -ccopt $(TOPLEVEL)/xenops $(TOPLEVEL)/xenops/xenops.cmxa

xenvm-cmd_LIBS = unix.cmxa camomile.cmxa threads.cmxa \
	-ccopt -L -ccopt $(BIGARRAY_DIR) $(BIGARRAY_DIR)/bigarray.cmxa \
	-ccopt -L -ccopt $(CAMOMILE_DIR) $(CAMOMILE_DIR)/camomile.cmxa \
	-ccopt -L -ccopt $(DBUS_DIR) $(DBUS_DIR)/dBus.cmxa \
	-ccopt -L -ccopt $(TOPLEVEL)/libs/json $(TOPLEVEL)/libs/json/json.cmxa \
	-ccopt -L -ccopt $(TOPLEVEL)/libs/jsonrpc $(TOPLEVEL)/libs/jsonrpc/jsonrpc.cmxa \
	-ccopt -L -ccopt $(TOPLEVEL)/libs/common $(TOPLEVEL)/libs/common/tscommon.cmxa \
	-ccopt -L -ccopt $(TOPLEVEL)/libs/stdext $(TOPLEVEL)/libs/stdext/stdext.cmxa

xenops_LIBS = unix.cmxa threads.cmxa \
	$(c_libs) \
	$(TOPLEVEL)/libs/uuid/uuid.cmxa \
	-ccopt -L -ccopt $(TOPLEVEL)/libs/mmap $(TOPLEVEL)/libs/mmap/mmap.cmxa \
	-ccopt -L -ccopt $(TOPLEVEL)/libs/log $(TOPLEVEL)/libs/log/log.cmxa \
	-ccopt -L -ccopt $(TOPLEVEL)/libs/eventchn $(TOPLEVEL)/libs/eventchn/eventchn.cmxa \
	-ccopt -L -ccopt $(TOPLEVEL)/libs/xc $(TOPLEVEL)/libs/xc/xc.cmxa \
	-ccopt -L -ccopt $(TOPLEVEL)/libs/xg $(TOPLEVEL)/libs/xg/xg.cmxa \
	-ccopt -L -ccopt $(TOPLEVEL)/libs/xb $(TOPLEVEL)/libs/xb/xb.cmxa \
	-ccopt -L -ccopt $(TOPLEVEL)/libs/xs $(TOPLEVEL)/libs/xs/xs.cmxa \
	-ccopt -L -ccopt $(TOPLEVEL)/libs/udev $(TOPLEVEL)/libs/udev/udev.cmxa \
	-ccopt -L -ccopt $(TOPLEVEL)/libs/stdext $(TOPLEVEL)/libs/stdext/stdext.cmxa \
	-ccopt -L -ccopt $(TOPLEVEL)/libs/netdev $(TOPLEVEL)/libs/netdev/netdev.cmxa \
	-ccopt -L -ccopt $(TOPLEVEL)/libs/scsi $(TOPLEVEL)/libs/scsi/scsi.cmxa \
	-ccopt -L -ccopt $(TOPLEVEL)/libs/base64 $(TOPLEVEL)/libs/base64/base64.cmxa \
	-ccopt -L -ccopt $(TOPLEVEL)/libs/common $(TOPLEVEL)/libs/common/tscommon.cmxa \
	-ccopt -L -ccopt $(TOPLEVEL)/xenops $(TOPLEVEL)/xenops/xenops.cmxa

PROGRAMS = xenvm xenvm-cmd xenops
OCAML_PROGRAM = xenvm xenvm-cmd xenops

all: $(INTF) $(PROGRAMS)

bins: $(PROGRAMS)

include $(TOPLEVEL)/Makefile.rules

