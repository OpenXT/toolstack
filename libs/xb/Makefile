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

CFLAGS += -I../mmap
OCAMLINCLUDE += -I ../mmap

.NOTPARALLEL:
# Ocaml is such a PITA!

PREINTF = op.cmi partial.cmi packet.cmi
PREOBJS = op partial packet xs_ring
PRELIBS = $(foreach obj, $(PREOBJS),$(obj).cmo) $(foreach obj,$(PREOJBS),$(obj).cmx)
OBJS = op partial packet xs_ring xb
INTF = op.cmi packet.cmi xb.cmi
LIBS = xb.cma xb.cmxa

ALL_OCAML_OBJS = $(OBJS) $(PREOJBS)

all: $(PREINTF) $(PRELIBS) $(INTF) $(LIBS) $(PROGRAMS)

bins: $(PROGRAMS)

libs: $(LIBS)

xb_OBJS = $(OBJS)
xb_C_OBJS = xs_ring_stubs xb_stubs
OCAML_LIBRARY = xb

%.mli: %.ml
	$(E) " MLI       $@"
	$(Q)$(OCAMLC) -i $< $o

.PHONY: install uninstall stage

install: $(LIBS) META
	ocamlfind install -destdir $(DESTDIR)$(shell ocamlfind printconf destdir) -ldconf ignore xb META $(INTF) $(LIBS) *.a *.so *.cmx

uninstall:
	ocamlfind remove xb

stage: libs META
	ocamlfind remove xb
	ocamlfind install xb META -ldconf ignore $(INTF) $(LIBS) *.a *.so *.cmx

include $(TOPLEVEL)/Makefile.rules
