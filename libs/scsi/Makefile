#
# Copyright (c) 2010 Citrix Systems, Inc.
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

OBJS = scsi
INTF = $(foreach obj, $(OBJS),$(obj).cmi)
LIBS = scsi.cma scsi.cmxa

all: $(INTF) $(LIBS) $(PROGRAMS)

bins: $(PROGRAMS)

libs: $(LIBS)

scsi_OBJS = $(OBJS)
scsi_C_OBJS = scsi_inq_stubs
OCAML_LIBRARY = scsi

.PHONY: install uninstall stage

install: $(LIBS) META
	ocamlfind install -destdir $(DESTDIR)$(shell ocamlfind printconf destdir) -ldconf ignore scsi META $(INTF) $(LIBS) *.a *.so *.cmx

uninstall:
	ocamlfind remove scsi

stage: libs META
	ocamlfind remove scsi
	ocamlfind install scsi -ldconf ignore META $(INTF) $(LIBS) *.a *.so *.cmx

include $(TOPLEVEL)/Makefile.rules

