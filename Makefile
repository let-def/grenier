
SUB = hll jmphash pcg orderme doubledouble physh baltree

all::
	for i in $(SUB); do $(MAKE) -C $$i $@; done

clean::
	for i in $(SUB); do $(MAKE) -C $$i $@; done
	for i in $(SUB); do rm -f $$i/OCamlMakefile; done

RESULT = grenier

LIBINSTALL_FILES =     		 \
	hll/hll.mli          		 \
	hll/hll.cmi          		 \
	hll/hll_consts.cmi   		 \
	hll/hll.a            		 \
	hll/hll.cma          		 \
	hll/hll.cmxa         		 \
	jmphash/jmphash.mli  		 \
	jmphash/jmphash.cmi  		 \
	jmphash/jmphash.a    		 \
	jmphash/jmphash.cma  		 \
	jmphash/jmphash.cmxa 		 \
	pcg/pcg.mli          		 \
	pcg/pcg.cmi          		 \
	pcg/pcg.a            		 \
	pcg/pcg.cma          		 \
	pcg/pcg.cmxa         		 \
	orderme/orderList.mli    \
	orderme/orderManaged.mli \
	orderme/orderList.cmi    \
	orderme/orderManaged.cmi \
	orderme/orderme.a        \
	orderme/orderme.cma      \
	orderme/orderme.cmxa     \
	doubledouble/doubledouble.mli \
	doubledouble/doubledouble.cmi \
	doubledouble/doubledouble.cmo \
	doubledouble/doubledouble.cmx \
	physh/physh.mli          \
	physh/physh.cmi          \
	physh/physh.a            \
	physh/physh.cma          \
	physh/physh.cmxa         \
	physh/lib_physh_stubs.a  \
	physh/dll_physh_stubs.so \
	baltree/baltree.a        \
	baltree/baltree.cma      \
	baltree/baltree.cmxa     \
	baltree/bt1.cmi          \
	baltree/bt2.cmi          \
	baltree/mbt.cmi

-include OCamlMakefile

install: libinstall

uninstall: libuninstall

reinstall:
	-$(MAKE) uninstall
	$(MAKE) install

OCAMLFLAGS += -g
OCAMLLDFLAGS += -g
