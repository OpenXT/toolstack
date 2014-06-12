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

OCAMLINCLUDE += -I ../xb/

.NOTPARALLEL:
# Ocaml is such a PITA!

PREINTF = xsraw.cmi xst.cmi
PREOBJS = queueop xsraw xst
PRELIBS = $(foreach obj, $(PREOBJS),$(obj).cmo) $(foreach obj,$(PREOJBS),$(obj).cmx)
OBJS = queueop xsraw xst xs
INTF = xsraw.cmi xst.cmi xs.cmi
LIBS = xs.cma xs.cmxa

all: $(PREINTF) $(PRELIBS) $(INTF) $(LIBS) $(PROGRAMS)

bins: $(PROGRAMS)

libs: $(LIBS)

xs_OBJS = $(OBJS)
OCAML_NOC_LIBRARY = xs

.PHONY: install uninstall stage

install: $(LIBS) META
	ocamlfind install -destdir $(DESTDIR)$(shell ocamlfind printconf destdir) -ldconf ignore xs META $(INTF) xs.mli xst.mli xsraw.mli $(LIBS) *.a *.cmx

uninstall:
	ocamlfind remove xs

stage: libs META
	ocamlfind remove xs
	ocamlfind install xs -ldconf ignore META $(INTF) $(LIBS) *.a *.cmx

include $(TOPLEVEL)/Makefile.rules

