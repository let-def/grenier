all: byte-code-library native-code-library

SOURCES = order_list.mli order_list.ml \
					order_indir.mli order_indir.ml \
					order_managed.mli order_managed.ml

RESULT = orderme

-include OCamlMakefile

OCamlMakefile:
	ln -s ../$@ $@

test:
	ocamlopt -o test_order.native unix.cmxa -cclib -L. orderme.cmxa test_order.ml
	./test_order.native 100

bench:
	ocamlopt -o bench_order.native unix.cmxa -cclib -L. orderme.cmxa bench_order.ml
	./bench_order.native 5_000_000
