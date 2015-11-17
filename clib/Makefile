OCAMLOPT_BIN=ocamlopt 

OCAMLOPT_LIBS=str.cmxa
OCAMLOPT_ARGS= -I ../util -I ../hunpos -output-obj

SOURCES_UTIL = amap.ml parse.ml mfhash.ml vocab.mli vocab.ml
SOURCES_HUNPOS= io.ml lexicon.ml  linear_interpolated_lm.ml   morphtable.ml  ngram.ml  special_tokens.ml  suffix_guesser.ml  viterbi.ml hmm_tagger.ml 

SOURCE_WRAPPER_ML=hunposcaml.ml
SOURCE_WRAPPER_C=hunposwrap.c

SOURCES=$(addprefix ../util/,${SOURCES_UTIL}) $(addprefix ../hunpos/,${SOURCES_HUNPOS}) ${SOURCE_WRAPPER_ML} 
OCAMLC_WHERE=`ocamlc -where`

all: test

libhunpos.so: ${SOURCES}
	ocamlmklib ${OCAMLOPT_ARGS} ${SOURCES}  -oc hunpos

hunpos.o: ${SOURCES}
	${OCAMLOPT_BIN} ${OCAMLOPT_ARGS} ${OCAMLOPT_LIBS}  ${SOURCES} -o hunpos.o

test: hunpos.o hunposwrap.c test.c
	cc -o test -I. -I${OCAMLC_WHERE} -L${OCAMLC_WHERE} ${SOURCE_WRAPPER_C}  hunpos.o  test.c  -lunix -lasmrun -lm -ldl -lcamlstr


TO_CLEAN:=$(filter %.ml,$(SOURCES))
TO_CLEAN:=$(TO_CLEAN:.ml=.cmx) $(TO_CLEAN:.ml=.cmi) $(TO_CLEAN:.ml=.cmo) $(TO_CLEAN:.ml=.o) a.a a.cma a.cmxa

clean:
	$(RM) $(TO_CLEAN)
