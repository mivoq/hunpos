OCAMLOPT=ocamlopt 
OCAMLMKLIB=ocamlmklib

SOURCES :=                                  \
	../util/amap.ml			    \
	../util/parse.ml		    \
	../util/mfhash.ml		    \
	../util/vocab.mli		    \
	../util/vocab.ml		    \
					    \
	../hunpos/io.ml			    \
	../hunpos/lexicon.ml		    \
	../hunpos/linear_interpolated_lm.ml \
	../hunpos/morphtable.ml		    \
	../hunpos/ngram.ml		    \
	../hunpos/special_tokens.ml	    \
	../hunpos/suffix_guesser.ml	    \
	../hunpos/viterbi.ml		    \
	../hunpos/hmm_tagger.ml		    \
					    \
	hunposcaml.ml			    \
					    \
	hunposwrap.c			    \
					    \
	hunpos.h

SOURCES_C=$(filter %.c,$(SOURCES))
SOURCES_ML=$(filter %.ml,$(SOURCES))
SOURCES_OCAML=$(filter-out %.c %.h,$(SOURCES))

LIBS_OCAML=str.cmxa

OBJECTS=$(SOURCES_C:.c=.o)
TO_CLEAN:=$(SOURCES_ML:.ml=.cmx) $(SOURCES_ML:.ml=.cmi) $(SOURCES_ML:.ml=.cmo) $(SOURCES_ML:.ml=.o) hunpos.cma hunpos.cmxa hunpos.a dllhunpos.so libhunpos.a
TO_CLEAN+=$(OBJECTS) test

OCAMLC_WHERE:=$(shell ocamlc -where)

all: test

%.o: %.c
	$(CC) -Wall -c $(<) -fPIC -o $(@)

libhunpos.a: $(SOURCES) $(OBJECTS) hunpos.o
	ar rvs $(@) $(OBJECTS) hunpos.o

hunpos.o: $(SOURCES)
	$(OCAMLOPT) -I ../util -I ../hunpos -output-obj $(LIBS_OCAML) $(SOURCES_OCAML) -o hunpos.o

test: test.c $(SOURCES) $(OBJECTS) hunpos.o libhunpos.a
	cc -Wall $(<) -o $(@) -I. libhunpos.a -L$(OCAMLC_WHERE) -lcamlstr -lunix -lasmrun_pic libhunpos.a -L$(OCAMLC_WHERE) -lcamlstr -lunix -lasmrun_pic -lm -ldl

clean:
	$(RM) $(TO_CLEAN) hunpos.o test

.DELETE_ON_ERROR:
