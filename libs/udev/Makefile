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

OBJS = udev
INTF = udev.cmi
LIBS = udev.cma udev.cmxa

udev_OBJS = $(OBJS)
udev_C_OBJS = udev_stubs

OCAML_LIBRARY = udev

all: $(INTF) $(LIBS)

libs: $(LIBS)

.PHONY: install uninstall stage

install: $(LIBS) META
	ocamlfind install -destdir $(DESTDIR)$(shell ocamlfind printconf destdir) -ldconf ignore udev META $(INTF) $(LIBS) *.a *.so *.cmx

uninstall:
	ocamlfind remove udev

stage: libs META
	ocamlfind remove udev
	ocamlfind install udev -ldconf ignore META $(INTF) $(LIBS) *.a *.so *.cmx

include $(TOPLEVEL)/Makefile.rules
