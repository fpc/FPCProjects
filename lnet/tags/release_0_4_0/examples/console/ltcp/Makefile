PP=fpc
BASEARGS=-Fu../../../lib -Fi../../../lib/sys -Sm 
ARGS=$(BASEARGS) $(OPT) -O2 -XX -Xs

all:
	$(PP) $(ARGS) lserver.pas
	$(PP) $(ARGS) lclient.pas

argless:
	$(PP) $(BASEARGS) $(OPT) lserver.pas	
	$(PP) $(BASEARGS) $(OPT) lclient.pas	

clean:
	delp .
	rm -f lserver
	rm -f lclient
	rm -f *~

clear:
	$(MAKE) -C . clean
	$(MAKE) -C ../../../ clean
