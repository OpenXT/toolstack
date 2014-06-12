#
# Copyright (c) 2012 Citrix Systems, Inc.
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

LIBS = xenops.cma xenops.cmxa

OCAMLINCLUDE += \
	-I $(TOPLEVEL)/libs/log -I $(TOPLEVEL)/libs/xb -I $(TOPLEVEL)/libs/xs -I $(TOPLEVEL)/libs/udev -I $(TOPLEVEL)/libs/uuid \
	-I $(TOPLEVEL)/libs/mmap -I $(TOPLEVEL)/libs/xc -I $(TOPLEVEL)/libs/xg -I $(TOPLEVEL)/libs/eventchn -I $(TOPLEVEL)/libs/scsi \
	-I $(TOPLEVEL)/libs/netdev -I $(TOPLEVEL)/libs/stdext -I $(TOPLEVEL)/libs/base64 -I $(TOPLEVEL)/libs/common
OCAMLOPTFLAGS += -thread

OBJS_XENOPS_COMMON = \
	io memory netman table stats watch \
	xenbus balloon device_common hotplug device domain_common dm dmagent xal \
	domain

all: $(LIBS)

xenops_OBJS = $(OBJS_XENOPS_COMMON)

ALL_OCAML_OBJS = $(OBJS_XENOPS_COMMON)

INTF = watch.cmi netman.cmi balloon.cmi device_common.cmi memory.cmi device.cmi xal.cmi domain.cmi dm.cmi \
	   dmagent.cmi

OCAML_NOC_LIBRARY = xenops

all: $(INTF) $(PROGRAMS)

bins: $(PROGRAMS)

include $(TOPLEVEL)/Makefile.rules
