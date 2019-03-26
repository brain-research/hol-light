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
CXXFLAGS+= -DNAMESPACE_FOR_HASH_FUNCTIONS=farmhash
CPPFLAGS+= `pkg-config --cflags protobuf grpc`
LDFLAGS+= -static
LDFLAGS+= -pthread
LDFLAGS+= -Wl,--whole-archive -lpthread -Wl,--no-whole-archive
LDFLAGS+= -Wl,--whole-archive -lgrpc++_reflection -Wl,--no-whole-archive
LDFLAGS+= -L/usr/local/lib `pkg-config --libs protobuf grpc++ grpc`
LDFLAGS+= -lfarmhash -lstdc++
PROTOC=protoc
GRPC_CPP_PLUGIN=`which grpc_cpp_plugin`

all: main

%.grpc.pb.cc %.grpc.pb.h: %.proto
	$(PROTOC) --grpc_out=. --plugin=protoc-gen-grpc=$(GRPC_CPP_PLUGIN) $<

%.pb.cc %.pb.h: %.proto
	$(PROTOC) --cpp_out=. $<

main.o: main.cc proof_assistant.grpc.pb.h hol_light_prover.o theorem_fingerprint.o

compatibility.o: compatibility.cc compatibility.h

hol_light_prover.o: hol_light_prover.cc hol_light_prover.h proof_assistant.pb.h

subprocess.o: ../subprocess.cc ../subprocess.h

subprocess_comm.o: subprocess_comm.cc subprocess_comm.h comm.h

theorem_fingerprint.o: theorem_fingerprint.cc theorem_fingerprint.h proof_assistant.pb.h farmhash_compatibility.h

MAIN_OBJS= main compatibility hol_light_prover ../subprocess subprocess_comm proof_assistant.grpc.pb proof_assistant.pb theorem_fingerprint

main: $(addsuffix .o, $(MAIN_OBJS))
	$(CXX) $^ $(LDFLAGS) -o $@

clean:
	rm -f main *.cmx *.cmi *.cmo *.o *.pb.*
