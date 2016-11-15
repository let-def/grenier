all: byte-code-library native-code-library

SOURCES = distwit.mli distwit.ml

RESULT = distwit

test: all
	ocamlc -o test_distwit.byte distwit.cma test_distwit.ml
	ocamlopt -o test_distwit.native distwit.cmxa test_distwit.ml
	./test_distwit.byte
	./test_distwit.native

OCAMLMAKEFILE=../OCamlMakefile
-include $(OCAMLMAKEFILE)
