#
# Copyright (c) 2011 Citrix Systems, Inc.
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

OCAMLINCLUDE += -I $(TOPLEVEL)/libs/log -I $(TOPLEVEL)/libs/xb -I $(TOPLEVEL)/libs/uuid \
	-I $(TOPLEVEL)/libs/mmap -I $(TOPLEVEL)/libs/xc -I $(TOPLEVEL)/libs/eventchn \
	-I $(TOPLEVEL)/libs/stdext -I $(TOPLEVEL)/libs/common

OBJS = define logging quota perms symbol utils store disk transaction \
       event domain domains connection connections \
       parse_arg process xenstored
INTF = symbol.cmi
XENSTOREDLIBS = unix.cmxa \
	$(TOPLEVEL)/libs/uuid/uuid.cmxa \
	-ccopt -lxenctrl -ccopt -lxenguest \
	-ccopt -L -ccopt $(TOPLEVEL)/libs/mmap $(TOPLEVEL)/libs/mmap/mmap.cmxa \
	-ccopt -L -ccopt $(TOPLEVEL)/libs/log $(TOPLEVEL)/libs/log/log.cmxa \
	-ccopt -L -ccopt $(TOPLEVEL)/libs/eventchn $(TOPLEVEL)/libs/eventchn/eventchn.cmxa \
	-ccopt -L -ccopt $(TOPLEVEL)/libs/xc $(TOPLEVEL)/libs/xc/xc.cmxa \
	-ccopt -L -ccopt $(TOPLEVEL)/libs/xb $(TOPLEVEL)/libs/xb/xb.cmxa \
	-ccopt -L -ccopt $(TOPLEVEL)/libs/stdext $(TOPLEVEL)/libs/stdext/stdext.cmxa \
	-ccopt -L -ccopt $(TOPLEVEL)/libs/stdext $(TOPLEVEL)/libs/common/tscommon.cmxa

PROGRAMS = xenstored

xenstored_LIBS = $(XENSTOREDLIBS)
xenstored_OBJS = $(OBJS)

OCAML_PROGRAM = xenstored

all: $(INTF) $(PROGRAMS)

bins: $(PROGRAMS)

include $(TOPLEVEL)/Makefile.rules
