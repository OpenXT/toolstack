#
# Copyright (c) 2009 Citrix Systems, Inc.
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

TOPLEVEL=../..
include $(TOPLEVEL)/common.make

OCAMLINCLUDE += \
	-I $(TOPLEVEL)/libs/log -I $(TOPLEVEL)/libs/xb -I $(TOPLEVEL)/libs/xs \
	-I $(TOPLEVEL)/libs/uuid -I $(TOPLEVEL)/libs/mmap -I $(TOPLEVEL)/libs/base64 \
	-I $(TOPLEVEL)/libs/xc -I $(TOPLEVEL)/libs/eventchn -I $(TOPLEVEL)/libs/scsi \
	-I $(TOPLEVEL)/libs/json -I $(TOPLEVEL)/libs/jsonrpc \
	-I $(TOPLEVEL)/libs/stdext -I $(TOPLEVEL)/common -I $(TOPLEVEL)/xenops -I $(TOPLEVEL)/xenvm
OCAMLOPTFLAGS += -thread

PROGRAMS = runtest
OCAML_PROGRAM = runtest

runtest_LIBS = unix.cmxa dBus.cmxa threads.cmxa \
	$(TOPLEVEL)/libs/uuid/uuid.cmxa \
	-ccopt -L -ccopt $(TOPLEVEL)/libs/mmap $(TOPLEVEL)/libs/mmap/mmap.cmxa \
	-ccopt -L -ccopt $(TOPLEVEL)/libs/log $(TOPLEVEL)/libs/log/log.cmxa \
	-ccopt -L -ccopt $(TOPLEVEL)/libs/eventchn $(TOPLEVEL)/libs/eventchn/eventchn.cmxa \
	-ccopt -L -ccopt $(TOPLEVEL)/libs/xc $(TOPLEVEL)/libs/xc/xc.cmxa \
	-ccopt -L -ccopt $(TOPLEVEL)/libs/xb $(TOPLEVEL)/libs/xb/xb.cmxa \
	-ccopt -L -ccopt $(TOPLEVEL)/libs/xs $(TOPLEVEL)/libs/xs/xs.cmxa \
	-ccopt -L -ccopt $(TOPLEVEL)/libs/stdext $(TOPLEVEL)/libs/stdext/stdext.cmxa \
	-ccopt -L -ccopt $(TOPLEVEL)/libs/netdev $(TOPLEVEL)/libs/netdev/netdev.cmxa \
	-ccopt -L -ccopt $(TOPLEVEL)/libs/json $(TOPLEVEL)/libs/json/json.cmxa \
	-ccopt -L -ccopt $(TOPLEVEL)/libs/jsonrpc $(TOPLEVEL)/libs/jsonrpc/jsonrpc.cmxa \
	-ccopt -L -ccopt $(TOPLEVEL)/libs/scsi $(TOPLEVEL)/libs/scsi/scsi.cmxa \
	-ccopt -L -ccopt $(TOPLEVEL)/libs/base64 $(TOPLEVEL)/libs/base64/base64.cmxa \
	-ccopt -L -ccopt $(TOPLEVEL)/xenops $(TOPLEVEL)/xenops/xenops.cmxa


runtest_OBJS = $(TOPLEVEL)/xenvm/xenvmlib test runtest

all: $(PROGRAMS)

bins: $(PROGRAMS)

include $(TOPLEVEL)/Makefile.rules
