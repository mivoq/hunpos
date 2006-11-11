OCAMLMAKEFILE = ../OCamlMakefile

SOURCES = ../src/hunpos/io.ml ../src/hunpos/ngram.ml  ../src/hunpos/viterbi.ml chinese_decoder.ml


RESULT = decoder

#ocamorph  maxent
export OCAMLNCFLAGS := -unsafe -inline 100 
all: native-code
#all: profiling-byte-code

include $(OCAMLMAKEFILE)

