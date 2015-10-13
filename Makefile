
SUB = hll jmphash pcg orderme doubledouble

all::
	for i in $(SUB); do $(MAKE) -C $$i $@; done

clean::
	for i in $(SUB); do $(MAKE) -C $$i $@; done
	for i in $(SUB); do rm -f $$i/OCamlMakefile; done

RESULT = grenier

LIBINSTALL_FILES =     		 \
	hll/hll.mli          		 \
	hll/hll.a            		 \
	hll/hll.cma          		 \
	hll/hll.cmi          		 \
	hll/hll.cmxa         		 \
	hll/hll_consts.cmi   		 \
	jmphash/jmphash.mli  		 \
	jmphash/jmphash.a    		 \
	jmphash/jmphash.cma  		 \
	jmphash/jmphash.cmi  		 \
	jmphash/jmphash.cmxa 		 \
	pcg/pcg.mli          		 \
	pcg/pcg.a            		 \
	pcg/pcg.cma          		 \
	pcg/pcg.cmi          		 \
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
	doubledouble/doubledouble.cmx

-include OCamlMakefile

install: libinstall

uninstall: libuninstall

reinstall:
	-$(MAKE) uninstall
	$(MAKE) install
