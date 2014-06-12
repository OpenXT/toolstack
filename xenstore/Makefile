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

TOPLEVEL=..
include $(TOPLEVEL)/common.make

OCAMLINCLUDE += -I $(TOPLEVEL)/libs/log -I $(TOPLEVEL)/libs/xb -I $(TOPLEVEL)/libs/uuid \
	-I $(TOPLEVEL)/libs/mmap -I $(TOPLEVEL)/libs/xs -I $(TOPLEVEL)/libs/stdext

OBJS = xenstore
INTF =
xenstore_LIBS = unix.cmxa \
	$(TOPLEVEL)/libs/uuid/uuid.cmxa \
	-ccopt -L -ccopt $(TOPLEVEL)/libs/mmap $(TOPLEVEL)/libs/mmap/mmap.cmxa \
	-ccopt -L -ccopt $(TOPLEVEL)/libs/xb $(TOPLEVEL)/libs/xb/xb.cmxa \
	-ccopt -L -ccopt $(TOPLEVEL)/libs/xs $(TOPLEVEL)/libs/xs/xs.cmxa \
	-ccopt -L -ccopt $(TOPLEVEL)/libs/stdext $(TOPLEVEL)/libs/stdext/stdext.cmxa

xenstore_OBJS = $(OBJS)

PROGRAMS = xenstore
OCAML_PROGRAM = xenstore

all: $(INTF) $(PROGRAMS)

bins: $(PROGRAMS)

include $(TOPLEVEL)/Makefile.rules
