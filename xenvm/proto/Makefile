#
# Copyright (c) 2009 Citrix Systems, Inc.
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

OCAMLC = ocamlc
OCAMLOPT = ocamlopt
OCAMLDEP = ocamldep

GEN_JSON_CONV = $(TOPLEVEL)/gen/json_conv/gen_json_conv
GEN_RPC = $(TOPLEVEL)/gen/rpc/gen_rpc

INCLUDES = -I $(TOPLEVEL)/libs/json -I $(TOPLEVEL)/libs/jsonrpc
OFLAGS = -g -dtypes $(INCLUDES)

GENERATED = xenvm_proto_json xenvm_notify_client xenvm_notify_server xenvm_control_client xenvm_control_server
GENERATED_SRC = $(addsuffix .ml, $(GENERATED))

FILES = xenvm_proto $(GENERATED)
SRC = $(addsuffix .ml, $(FILES))
OBJ = $(addsuffix .o, $(FILES))

all: $(OBJ)

xenvm_proto_json.ml: xenvm_proto.ml
	$(GEN_JSON_CONV) -i $< -o $@

xenvm_notify_client.ml xenvm_notify_server.ml: xenvm_notify.json
	$(GEN_RPC) -i $< -c xenvm_notify_client.ml -s xenvm_notify_server.ml

xenvm_control_client.ml xenvm_control_server.ml: xenvm_control.json
	$(GEN_RPC) -i $< -c xenvm_control_client.ml -s xenvm_control_server.ml

.SUFFIXES: .mli .ml .cmi .cmo .cmx .mll .mly .tex .dvi .ps .html

.mli.cmi:
	$(OCAMLC) -c $(OFLAGS) $<

.ml.cmo:
	$(OCAMLC) -c $(OFLAGS) $<

.ml.o:
	$(OCAMLOPT) -c $(OFLAGS) $<

.ml.cmx:
	$(OCAMLOPT) -c $(OFLAGS) $<


.PHONY: clean realclean depend

clean:
	rm -f *.cmi *.cmo *.annot *.o *.cmxa *.cmx

realclean: clean
	rm -f $(GENERATED_SRC)

.depend depend: $(GENERATED_SRC)
	rm -f .depend
	$(OCAMLDEP) $(INCLUDES) *.ml *.mli > .depend

include .depend
