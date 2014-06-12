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

PACKAGES = camomile
OCAMLC   = ocamlfind ocamlc -package $(PACKAGES)
OCAMLOPT = ocamlfind ocamlopt -package $(PACKAGES)

OBJS = json json_parse json_conv
INTF = $(foreach obj, $(OBJS),$(obj).cmi)
LIBS = json.cma json.cmxa

all: $(INTF) $(LIBS) $(PROGRAMS)

bins: $(PROGRAMS)

libs: $(LIBS)

json_OBJS = $(OBJS)
OCAML_NOC_LIBRARY = json

.PHONY: install uninstall stage

install: libs META
	ocamlfind install -destdir $(DESTDIR)$(shell ocamlfind printconf destdir) -ldconf ignore json META $(INTF) $(LIBS) *.a *.cmx

uninstall:
	ocamlfind remove json

stage: libs META
	ocamlfind remove json
	ocamlfind install json -ldconf ignore META $(INTF) $(LIBS) *.a *.cmx

include $(TOPLEVEL)/Makefile.rules

