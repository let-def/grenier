
SUB = hll jmphash pcg orderme doubledouble physh baltree trope

all::
	for i in $(SUB); do $(MAKE) -C $$i $@; done

doc::
	rm -rf doc;
	mkdir doc
	for i in $(SUB); do $(MAKE) -C $$i htdoc; mv $$i/doc/$$i doc/; done

clean::
	for i in $(SUB); do $(MAKE) -C $$i $@; done
	for i in $(SUB); do rm -f $$i/OCamlMakefile; done

RESULT = grenier

LIBINSTALL_FILES =              \
	hll/hll.mli                   \
	hll/hll.cmi                   \
	hll/hll_consts.cmi            \
	hll/hll.a                     \
	hll/hll.cma                   \
	hll/hll.cmxa                  \
	jmphash/jmphash.mli           \
	jmphash/jmphash.cmi           \
	jmphash/jmphash.a             \
	jmphash/jmphash.cma           \
	jmphash/jmphash.cmxa          \
	pcg/pcg.mli                   \
	pcg/pcg.cmi                   \
	pcg/pcg.a                     \
	pcg/pcg.cma                   \
	pcg/pcg.cmxa                  \
	orderme/order_list.mli        \
	orderme/order_indir.mli       \
	orderme/order_managed.mli     \
	orderme/order_list.cmi        \
	orderme/order_indir.cmi       \
	orderme/order_managed.cmi     \
	orderme/orderme.a             \
	orderme/orderme.cma           \
	orderme/orderme.cmxa          \
	doubledouble/doubledouble.mli \
	doubledouble/doubledouble.cmi \
	doubledouble/doubledouble.cmo \
	doubledouble/doubledouble.cmx \
	physh/physh.mli               \
	physh/physh.cmi               \
	physh/physh.a                 \
	physh/physh.cma               \
	physh/physh.cmxa              \
	physh/lib_physh_stubs.a       \
	physh/dll_physh_stubs.so      \
	baltree/baltree.a             \
	baltree/baltree.cma           \
	baltree/baltree.cmxa          \
	baltree/bt1.mli               \
	baltree/bt1.cmi               \
	baltree/bt2.mli               \
	baltree/bt2.cmi               \
	baltree/mbt.mli               \
	baltree/mbt.cmi               \
	trope/trope.a   	          	\
	trope/trope.cma 	          	\
	trope/trope.cmxa	          	\
	trope/trope.mli               \
	trope/trope.cmi


-include OCamlMakefile

install: libinstall

uninstall: libuninstall

reinstall:
	-$(MAKE) uninstall
	$(MAKE) install

OCAMLFLAGS += -g
OCAMLLDFLAGS += -g
