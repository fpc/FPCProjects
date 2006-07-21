PP=fpc
BASEARGS=-Fu../../../lib -Fi../../../lib/sys -Sm 
ARGS=$(BASEARGS) $(OPT) -O2 -XX -Xs

all:
	$(PP) $(ARGS) fphttpd.pas

argless:
	$(PP) $(BASEARGS) $(OPT) fphttpd.pas	

clean:
	delp .
	rm -f fphttpd
	rm -f *~

clear:
	$(MAKE) -C . clean
	$(MAKE) -C ../../../ clean
