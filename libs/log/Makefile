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

OCAMLINCLUDE += -I ../stdext

OBJS = syslog log logs
INTF = log.cmi logs.cmi syslog.cmi
LIBS = log.cma log.cmxa

all: $(INTF) $(LIBS) $(PROGRAMS)

bins: $(PROGRAMS)

libs: $(LIBS)

log.cmxa: libsyslog_stubs.a $(foreach obj,$(OBJS),$(obj).cmx)
	$(call mk-caml-lib-native, $@, -cclib -lsyslog_stubs, $(foreach obj,$(OBJS),$(obj).cmx))

log.cma: $(foreach obj,$(OBJS),$(obj).cmo)
	$(call mk-caml-lib-bytecode, $@, -dllib dllsyslog_stubs.so -cclib -lsyslog_stubs, $(foreach obj,$(OBJS),$(obj).cmo))

syslog_stubs.a: syslog_stubs.o
	$(call mk-caml-stubs, $@, $+)

libsyslog_stubs.a: syslog_stubs.o
	$(call mk-caml-lib-stubs, $@, $+)

logs.mli : logs.ml
	$(OCAMLC) -i $(OCAMLCFLAGS) $< > $@

syslog.mli : syslog.ml
	$(OCAMLC) -i $< > $@

.PHONY: install uninstall stage
install: $(LIBS) META
	ocamlfind install -destdir $(DESTDIR)$(shell ocamlfind printconf destdir) -ldconf ignore log META $(INTF) $(LIBS) *.a *.so *.cmx

uninstall:
	ocamlfind remove log

stage: libs META
	ocamlfind remove log
	ocamlfind install log -ldconf ignore META $(INTF) $(LIBS) *.a *.so *.cmx

include $(TOPLEVEL)/Makefile.rules

