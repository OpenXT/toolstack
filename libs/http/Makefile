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

OBJS = httputils uri http
INTF = $(foreach obj, $(OBJS),$(obj).cmi)
LIBS = http.cma http.cmxa

all: $(INTF) $(LIBS) $(PROGRAMS)

bins: $(PROGRAMS)

libs: $(LIBS)

http_OBJS = $(OBJS)
OCAML_NOC_LIBRARY = http

.PHONY: install uninstall stage

install: libs META
	ocamlfind install -destdir $(DESTDIR)$(shell ocamlfind printconf destdir) -ldconf ignore http META $(INTF) $(LIBS) *.a *.cmx

uninstall:
	ocamlfind remove http

stage: libs META
	ocamlfind remove http
	ocamlfind install http -ldconf ignore META $(INTF) $(LIBS) *.a *.cmx

include $(TOPLEVEL)/Makefile.rules

