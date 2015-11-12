all: byte-code-library native-code-library

SOURCES = trope.ml trope.mli
RESULT = trope

-include OCamlMakefile

OCAMLFLAGS += -I ../baltree -I ../orderme

OCamlMakefile:
	ln -s ../$@ $@

.PHONY: test

test:
	ocamlfind opt -linkpkg -package grenier.trope -o test.exe \
		test/reference.mli test/reference.ml test/test.ml
	./test.exe 30000
