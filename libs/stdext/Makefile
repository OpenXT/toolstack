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

TOPLEVEL=../..


include $(TOPLEVEL)/common.make

LDFLAGS += -lrt

OCAMLINCLUDE += -I ../uuid

OCAML_TEST_INC = -I $(shell ocamlfind query oUnit)
OCAML_TEST_LIB = $(shell ocamlfind query oUnit)/oUnit.cmxa

OBJS = filenameext stringext hashtblext listext pervasiveext threadext ring qring trie opt unixext bigbuffer vIO varmap eventloop
INTF = $(foreach obj, $(OBJS),$(obj).cmi)
LIBS = stdext.cma stdext.cmxa

all: $(INTF) $(LIBS) $(PROGRAMS)

bins: $(PROGRAMS)

libs: $(LIBS)

stdext_OBJS = $(OBJS)
stdext_C_OBJS = unixext_stubs

OCAML_LIBRARY = stdext

## OBJS
threadext.cmo: threadext.ml
	echo $LDFLAGS
	$(call quiet-command, $(OCAMLC) $(OCAMLCFLAGS) -thread -c -o $@ $<,MLC,$@)

threadext.cmi: threadext.mli
	$(call quiet-command, $(OCAMLC) $(OCAMLCFLAGS) -thread -c -o $@ $<,MLI,$@)

threadext.cmx: threadext.ml
	$(call quiet-command, $(OCAMLOPT) $(OCAMLOPTFLAGS) -thread -c -o $@ $<,MLOPT,$@)

.PHONY: install uninstall stage

install: $(LIBS) META
	ocamlfind install -destdir $(DESTDIR)$(shell ocamlfind printconf destdir) -ldconf ignore stdext META $(INTF) $(LIBS) *.a *.so *.cmx

uninstall:
	ocamlfind remove stdext

stage: libs META
	ocamlfind remove stdext
	ocamlfind install stdext -ldconf ignore META $(INTF) $(LIBS) *.a *.so *.cmx


include $(TOPLEVEL)/Makefile.rules

