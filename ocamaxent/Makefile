OCAMLMAKEFILE = ../../OCamlMakefile

SOURCES = stub.cc maxent.ml
RESULT  = maxent
INCDIRS=/sw/include/boost-1_33_1 /usr/local/lib/ocaml  /usr/local/include/
CLIBS=maxent z stdc++ 
LDFLAGS = -fexceptions 
LIBINSTALL_FILES = *.cmi *.cma *.cmo *.cmx *.cmxa *.a *.o
all: debug-code-library native-code-library byte-code-library
include $(OCAMLMAKEFILE)