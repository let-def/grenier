all: byte-code-library native-code-library

SOURCES = trope.ml trope.mli
RESULT = trope

OCAMLFLAGS += -I ../baltree -I ../orderme

.PHONY: test.exe test

test.exe:
	ocamlfind opt -linkpkg -o test.exe -I ../orderme -I test \
		../orderme/orderme.cmxa ../baltree/baltree.cmxa trope.cmxa \
		test/reference.mli test/reference.ml test/test.ml

test: test.exe
	./test.exe 30000

test-long: test.exe
	./test.exe 3000000

OCAMLMAKEFILE=../OCamlMakefile
-include $(OCAMLMAKEFILE)
