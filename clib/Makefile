OCAMLOPT=ocamlopt 

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
TO_CLEAN:=$(SOURCES_ML:.ml=.cmx) $(SOURCES_ML:.ml=.cmi) $(SOURCES_ML:.ml=.cmo) $(SOURCES_ML:.ml=.o) hunpos.cma hunpos.cmxa hunpos.a hunpos.o
TO_CLEAN+=$(OBJECTS) test test2 libhunpos.a libhunpos.so

OCAMLC_WHERE:=$(shell $(OCAMLOPT) -where)

ADDITIONAL_CFLAGS ?= -g

all: test test2 libhunpos.a libhunpos.so

%.o: %.c
	$(CC) $(ADDITIONAL_CFLAGS) -Wall -I "$(OCAMLC_WHERE)" -c "$(<)" -fPIC -o "$(@)"

libhunpos.a: $(SOURCES) $(OBJECTS) hunpos.o
	ar rvs "$(@)" $(OBJECTS) hunpos.o

libhunpos.so: $(SOURCES) $(OBJECTS) hunpos.o
	gcc -o "$(@)" -shared -lm -ldl $(OBJECTS) hunpos.o -L"$(OCAMLC_WHERE)" -lcamlstr -lunix -lasmrun_pic

hunpos.o: $(SOURCES)
	$(OCAMLOPT) $(ADDITIONAL_CFLAGS) -I ../util -I ../hunpos -output-obj $(LIBS_OCAML) $(SOURCES_OCAML) -o hunpos.o

test2: test.c $(SOURCES) $(OBJECTS) hunpos.o libhunpos.so
	$(CC) $(ADDITIONAL_CFLAGS) -Wall "$(<)" -o "$(@)" -I. libhunpos.so

test: test.c $(SOURCES) $(OBJECTS) hunpos.o libhunpos.a
	$(CC) $(ADDITIONAL_CFLAGS) -Wall "$(<)" -o "$(@)" -I. libhunpos.a -L"$(OCAMLC_WHERE)" -lcamlstr -lunix -lasmrun_pic libhunpos.a -L"$(OCAMLC_WHERE)" -lcamlstr -lunix -lasmrun_pic -lm -ldl

clean:
	$(RM) $(TO_CLEAN)

.DELETE_ON_ERROR:
