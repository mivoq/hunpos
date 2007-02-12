OCAMLMAKEFILE = ../../OCamlMakefile

SOURCES = amap.ml ocamap.ml mfhash.ml trie.ml  parse.ml vocab.mli vocab.ml 

RESULT = ocautil
LIBINSTALL_FILES = *.cmi *.cma *.cmo *.cmx *.cmxa *.a *.o
export OCAMLNCFLAGS := -unsafe -inline 100 
all: native-code-library byte-code-library


include $(OCAMLMAKEFILE)
