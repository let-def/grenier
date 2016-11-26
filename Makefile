
SUB = hll jmphash pcg orderme doubledouble physh baltree trope binpacking
SUB_TEST = hll orderme doubledouble trope binpacking

all::
	for i in $(SUB); do $(MAKE) -C $$i $@; done

doc::
	mkdir -p doc
	for i in $(SUB); do \
			rm -rf doc/$$i; \
			$(MAKE) -C $$i htdoc; \
	 		mv $$i/doc/$$i doc/; \
	 		mv doc/$$i/html doc/$$i/doc; \
	 		rmdir $$i/doc; \
		done

clean::
	for i in $(SUB); do $(MAKE) -C $$i $@; done
	for i in $(SUB); do rm -f $$i/OCamlMakefile; done

RESULT = grenier

LIBINSTALL_FILES =              \
	hll/hll.mli                   \
	hll/hll.cmi                   \
	hll/hll.cmx                   \
	hll/hll_consts.cmi            \
	hll/hll_consts.cmx            \
	hll/hll.a                     \
	hll/hll.cma                   \
	hll/hll.cmxa                  \
	jmphash/jmphash.mli           \
	jmphash/jmphash.cmi           \
	jmphash/jmphash.cmx           \
	jmphash/jmphash.a             \
	jmphash/jmphash.cma           \
	jmphash/jmphash.cmxa          \
	pcg/pcg.mli                   \
	pcg/pcg.cmi                   \
	pcg/pcg.cmx                   \
	pcg/pcg.a                     \
	pcg/pcg.cma                   \
	pcg/pcg.cmxa                  \
	orderme/order_list.mli        \
	orderme/order_indir.mli       \
	orderme/order_managed.mli     \
	orderme/order_list.cmi        \
	orderme/order_list.cmx        \
	orderme/order_indir.cmi       \
	orderme/order_indir.cmx       \
	orderme/order_managed.cmi     \
	orderme/order_managed.cmx     \
	orderme/orderme.a             \
	orderme/orderme.cma           \
	orderme/orderme.cmxa          \
	doubledouble/doubledouble.mli \
	doubledouble/doubledouble.cmi \
	doubledouble/doubledouble.cmo \
	doubledouble/doubledouble.cmx \
	physh/physh.mli               \
	physh/physh.cmi               \
	physh/physh.cmx               \
	physh/physh.a                 \
	physh/physh.cma               \
	physh/physh.cmxa              \
	physh/lib_physh_stubs.a       \
	physh/dll_physh_stubs.so      \
	baltree/bt1.mli               \
	baltree/bt1.cmi               \
	baltree/bt1.cmx               \
	baltree/bt2.mli               \
	baltree/bt2.cmi               \
	baltree/bt2.cmx               \
	baltree/mbt.mli               \
	baltree/mbt.cmi               \
	baltree/mbt.cmx               \
	baltree/baltree.a             \
	baltree/baltree.cma           \
	baltree/baltree.cmxa          \
	binpacking/maxrects.mli       \
	binpacking/maxrects.cmi       \
	binpacking/maxrects.cmx       \
	binpacking/binpacking.a       \
	binpacking/binpacking.cma     \
	binpacking/binpacking.cmxa    \
	trope/trope.mli               \
	trope/trope.cmi               \
	trope/trope.cmx               \
	trope/trope.a   	          	\
	trope/trope.cma 	          	\
	trope/trope.cmxa

-include OCamlMakefile

install: libinstall

uninstall: libuninstall

reinstall:
	-$(MAKE) uninstall
	$(MAKE) install

OCAMLFLAGS += -g
OCAMLLDFLAGS += -g

test:
	for i in $(SUB_TEST); do $(MAKE) -C $$i test; done
