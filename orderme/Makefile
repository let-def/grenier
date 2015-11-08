all: byte-code-library native-code-library

SOURCES = order_list.mli order_list.ml \
					order_indir.mli order_indir.ml \
					order_managed.mli order_managed.ml

RESULT = orderme

-include OCamlMakefile

OCamlMakefile:
	ln -s ../$@ $@

