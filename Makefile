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
OCAMLOPT=ocamlopt.opt
OCAMLFLAGS=-g -w -3-8-52 -safe-string -I Library -I Multivariate
CAMLP5_REVISED=pa_r.cmo pa_rp.cmo pr_dump.cmo pa_lexer.cmo pa_extend.cmo q_MLast.cmo pa_reloc.cmo pa_macro.cmo

CXXFLAGS+= -std=c++11
CXXFLAGS+= -DNAMESPACE_FOR_HASH_FUNCTIONS=farmhash
LDFLAGS+= -static
LDFLAGS+= -pthread
LDFLAGS+= -Wl,--whole-archive -lpthread -Wl,--no-whole-archive
LDFLAGS+= -Wl,--whole-archive -lgrpc++_reflection -Wl,--no-whole-archive
LDFLAGS+= -L/usr/local/lib `pkg-config --libs protobuf grpc++ grpc`
LDFLAGS+= -lfarmhash -lstdc++

all: core

pa_j_tweak.cmo: pa_j_tweak.ml
	$(OCAMLC) -c -pp "camlp5r $(CAMLP5_REVISED)" -I $(CAMLP5_LIB) $<

system.cmo: system.ml pa_j_tweak.cmo
	$(OCAMLC) -c -pp "camlp5r $(CAMLP5_REVISED) ./pa_j_tweak.cmo" -I $(CAMLP5_LIB) $<

PP=-pp "camlp5r $(CAMLP5_REVISED) ./pa_j_tweak.cmo $(OCAML_LIB)/nums.cma ./system.cmo"

%.cmx: %.ml system.cmo pa_j_tweak.cmo
	$(OCAMLOPT) $(OCAMLFLAGS) $(PP) -I $(CAMLP5_LIB) -c $@ $<

CORE_SRCS=system hol_native lib fusion basics nets printer theorem_fingerprint preterm parser equal normalize pb_printer debug_mode bool drule log import_proofs tactics itab replay simp theorems ind_defs class trivia canon meson metis quot impconv pair nums recursion arith wf calc_num normalizer grobner ind_types lists realax calc_int realarith reals calc_rat ints sets iterate cart define parse_tactic

SANDBOXEE_SRCS=$(CORE_SRCS) comms sandboxee
CORE_OBJS=theorem_fingerprint_c comms_wrapper subprocess
CORE_LIBS=nums.cmxa str.cmxa quotation.cmx
hol_light_sandboxee: $(addsuffix .cmx, $(SANDBOXEE_SRCS)) $(addsuffix .o, $(CORE_OBJS))
	$(OCAMLOPT) -o $@ $(OCAMLFLAGS) -I $(CAMLP5_LIB)\
		$(CORE_LIBS) $^ -cclib "$(LDFLAGS)"

COMPLEX_SRCS=Library/wo Library/binary Library/card Library/permutations Library/products Library/floor Multivariate/misc Library/iter Multivariate/metric Multivariate/vectors Multivariate/determinants Multivariate/topology Multivariate/convex Multivariate/paths Multivariate/polytope Multivariate/degree Multivariate/derivatives Multivariate/clifford Multivariate/integration Multivariate/measure Library/binomial Multivariate/complexes Multivariate/canal Multivariate/transcendentals Multivariate/realanalysis Multivariate/moretop Multivariate/cauchy

complex: $(addsuffix .cmx, $(CORE_SRCS) $(COMPLEX_SRCS)) $(addsuffix .o, $(CORE_OBJS))
	$(OCAMLOPT) -o $@ $(OCAMLFLAGS) -I $(CAMLP5_LIB) $(CORE_LIBS) $^ -cclib "$(LDFLAGS)"

CHECK_PROOFS_SRCS=$(CORE_SRCS) synthetic_proofs $(COMPLEX_SRCS) import_proofs_summary

check_proofs: $(addsuffix .cmx, $(CHECK_PROOFS_SRCS)) $(addsuffix .o, $(CORE_OBJS))
	$(OCAMLOPT) -o $@ $(OCAMLFLAGS) -I $(CAMLP5_LIB) $(CORE_LIBS) $^ -cclib "$(LDFLAGS)"

clean:
	rm -f hol_light_sandboxee complex check_proofs *.cmx *.cmi *.cmo *.o
