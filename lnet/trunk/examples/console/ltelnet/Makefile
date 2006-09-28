PP=fpc
BASEARGS=-Fu../../../lib -Fi../../../lib/sys -Sm 
ARGS=$(BASEARGS) $(OPT) -O2 -XX -Xs

all:
	$(PP) $(ARGS) ltclient.pas

argless:
	$(PP) $(BASEARGS) $(OPT) ltclient.pas	

clean:
	delp .
	rm -f ltclient
	rm -f *~

clear:
	$(MAKE) -C . clean
	$(MAKE) -C ../../../ clean
