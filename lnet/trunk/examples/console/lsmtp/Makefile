PP=fpc
BASEARGS=-Fu../../../lib -Fi../../../lib/sys -Sm 
ARGS=$(BASEARGS) $(OPT) -O2 -XX -Xs

all:
	$(PP) $(ARGS) lsmtpclient

argless:
	$(PP) $(BASEARGS) $(OPT) lsmtpclient

clean:
	delp .
	rm -f lsmtpclient
	rm -f *~

clear:
	$(MAKE) -C . clean
	$(MAKE) -C ../../../ clean
