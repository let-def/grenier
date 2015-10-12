
SUB = hll jmphash pcg orderme

all clean::
	for i in $(SUB); do $(MAKE) -C $$i $@; done

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
	orderme/orderme.cmxa


-include OCamlMakefile

install: libinstall

uninstall: libuninstall

reinstall:
	-$(MAKE) uninstall
	$(MAKE) install
