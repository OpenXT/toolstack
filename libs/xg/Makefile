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

TOPLEVEL=../..
include $(TOPLEVEL)/common.make

CFLAGS +=
OCAMLINCLUDE +=

OBJS = xg
INTF =
LIBS = xg.cma xg.cmxa

xg_OBJS = $(OBJS)
xg_C_OBJS = acpi_decode smbios_decode xg_utils xg_stubs

OCAML_LIBRARY = xg

all: $(INTF) $(LIBS)

libs: $(LIBS)

.PHONY: install uninstall stage

install: $(LIBS) META
	ocamlfind install -destdir $(DESTDIR)$(shell ocamlfind printconf destdir) -ldconf ignore $(OCAML_LIBRARY) META $(INTF) $(LIBS) *.a *.so *.cmx

uninstall:
	ocamlfind remove $(OCAML_LIBRARY)

stage: libs META
	ocamlfind remove $(OCAML_LIBRARY)
	ocamlfind install $(OCAML_LIBRARY) -ldconf ignore META $(INTF) $(LIBS) *.a *.so *.cmx

include $(TOPLEVEL)/Makefile.rules
