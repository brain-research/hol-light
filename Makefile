OCAML_VERSION=$(shell ocamlc -version | cut -f1-2 -d.)
CAMLP5_VERSION=$(shell camlp5 -v 2>&1 | cut -f3 -d' ' | cut -f1 -d.)

ifneq "${OCAML_VERSION}" "4.03"
$(error Expected ocaml version 4.03 but got ${OCAML_VERSION})
endif
ifneq "${CAMLP5_VERSION}" "6"
$(error Expected camlp5 major version 6 but got ${CAMLP5_VERSION})
endif

OCAML_LIB=$(shell ocamlc -where)
CAMLP5_LIB=$(shell camlp5 -where)
OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLFLAGS=-g -w -3-8-52 -safe-string -I Library -I Multivariate
CAMLP5_REVISED=pa_r.cmo pa_rp.cmo pr_dump.cmo pa_lexer.cmo pa_extend.cmo q_MLast.cmo pa_reloc.cmo pa_macro.cmo

CXXFLAGS+= -std=c++11
CPPFLAGS+= `pkg-config --cflags protobuf grpc`
LDFLAGS+= -static
LDFLAGS+= -Wl,--whole-archive -lgrpc++_reflection -Wl,--no-whole-archive
LDFLAGS+= -L/usr/local/lib `pkg-config --libs protobuf grpc++ grpc`
LDFLAGS+= -lstdc++
PROTOC=protoc
GRPC_CPP_PLUGIN=`which grpc_cpp_plugin`

all: core main

pa_j_tweak.cmo: pa_j_tweak.ml
	$(OCAMLC) -c -pp "camlp5r $(CAMLP5_REVISED)" -I $(CAMLP5_LIB) $<

system.cmo: system.ml pa_j_tweak.cmo
	$(OCAMLC) -c -pp "camlp5r $(CAMLP5_REVISED) ./pa_j_tweak.cmo" -I $(CAMLP5_LIB) $<

PP=-pp "camlp5r $(CAMLP5_REVISED) ./pa_j_tweak.cmo $(OCAML_LIB)/nums.cma ./system.cmo"

%.cmx: %.ml system.cmo pa_j_tweak.cmo
	$(OCAMLOPT) $(OCAMLFLAGS) $(PP) -I $(CAMLP5_LIB) -c $@ $<

%.grpc.pb.cc %.grpc.pb.h: %.proto
	$(PROTOC) --grpc_out=. --plugin=protoc-gen-grpc=$(GRPC_CPP_PLUGIN) $<

%.pb.cc %.pb.h: %.proto
	$(PROTOC) --cpp_out=. $<

CORE_SRCS=system hol_native lib fusion basics nets printer theorem_fingerprint pb_printer preterm parser equal bool drule log tactics itab replay simp theorems ind_defs class trivia canon meson metis quot impconv pair nums recursion arith wf calc_num normalizer grobner ind_types lists realax calc_int realarith reals calc_rat ints sets iterate cart define comms parse_tactic sandboxee
CORE_OBJS=str_list_fingerprint comms_wrapper subprocess
CORE_LIBS=nums.cmxa str.cmxa quotation.cmx
core: $(addsuffix .cmx, $(CORE_SRCS)) $(addsuffix .o, $(CORE_OBJS))
	$(OCAMLOPT) -o $@ $(OCAMLFLAGS) -I $(CAMLP5_LIB)\
		$(CORE_LIBS) $^ -cclib "$(LDFLAGS) -lfarmhash"

main.o: main.cc hol_light.grpc.pb.h hol_light.o

hol_light.o: hol_light.cc hol_light.h hol_light.pb.h subprocess.o

subprocess.o: subprocess.cc subprocess.h

MAIN_OBJS=main hol_light subprocess hol_light.grpc.pb hol_light.pb
main: $(addsuffix .o, $(MAIN_OBJS))
	$(CXX) $^ $(LDFLAGS) -o $@

clean:
	rm -f core main *.cmx *.cmi *.cmo *.o *.pb.*
