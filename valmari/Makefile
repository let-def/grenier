all: byte-code-library native-code-library

SOURCES = finite.mli finite.ml partition.mli partition.ml valmari.mli valmari.ml
RESULT = valmari

OCAMLMAKEFILE=../OCamlMakefile
-include $(OCAMLMAKEFILE)

test.exe: all
	ocamlopt -o $@ valmari.cmxa test/test.ml

test: test.exe
	./test.exe test/Sample.dfa > test/Sample.output
	./test.exe test/Lex.dfa > test/Lex.output
	diff test/Sample.output test/Sample.reference
	diff test/Lex.output test/Lex.reference

.PHONY: test
