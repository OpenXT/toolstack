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

OCAMLINCLUDE += -I ../stdext

OBJS = base64
INTF = 
LIBS = base64.cma base64.cmxa

all: $(INTF) $(LIBS) $(PROGRAMS)

bins: $(PROGRAMS)

libs: $(LIBS)

base64_OBJS = $(OBJS)
OCAML_NOC_LIBRARY = base64

.PHONY: install uninstall stage

install: libs META
	ocamlfind install -destdir $(DESTDIR)$(shell ocamlfind printconf destdir) -ldconf ignore base64 META $(INTF) $(LIBS) *.a *.cmx

uninstall:
	ocamlfind remove base64

stage: libs META
	ocamlfind remove base64
	ocamlfind install -ldconf ignore base64 META $(INTF) $(LIBS) *.a *.cmx

include $(TOPLEVEL)/Makefile.rules

