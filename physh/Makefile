all: physh.cma physh.cmxa

%.o: %.c
	ocamlc -c -ccopt "-O2 -std=gnu99" $<

dll_physh_stubs.so lib_physh_stubs.a: ml_physh_set.o ml_physh_map.o
	ocamlmklib \
	    -o _physh_stubs $^ \
	    -ccopt -O2 -ccopt -std=gnu99

physh.cmi: physh.mli
	ocamlc -c $<

physh.cmo: physh.ml physh.cmi
	ocamlc -c $<

physh.cma: physh.cmo dll_physh_stubs.so
	ocamlc -a -custom -o $@ $< \
	       -dllib dll_physh_stubs.so \
	       -cclib -l_physh_stubs

physh.cmx: physh.ml physh.cmi
	ocamlopt -c $<

physh.cmxa physh.a: physh.cmx dll_physh_stubs.so
	ocamlopt -a -o $@ $< \
	      -cclib -l_physh_stubs \
	  		-ccopt -O2 -ccopt -std=gnu99

.PHONY: clean

clean:
	rm -f *.[oa] *.so *.cm[ixoa] *.cmxa
