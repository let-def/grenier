all: byte-code-library native-code-library

SOURCES = doubledouble.mli doubledouble.ml
RESULT = doubledouble

OCAMLMAKEFILE=../OCamlMakefile
-include $(OCAMLMAKEFILE)

test: all
	ocaml test_basic.ml
	ocaml test_io.ml
	ocaml test_compute.ml
