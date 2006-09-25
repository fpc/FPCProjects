PP=fpc
BASEARGS=-Fu../../../lib -Fi../../../lib/sys -Sm 
ARGS=$(BASEARGS) $(OPT) -O2 -XX -Xs

all:
	$(PP) $(ARGS) fphttpd.pas
	$(PP) $(ARGS) fpget.pas
	$(PP) $(ARGS) fpfcgi.pas

argless:
	$(PP) $(BASEARGS) $(OPT) fphttpd.pas	
	$(PP) $(BASEARGS) $(OPT) fpget.pas
	$(PP) $(BASEARGS) $(OPT) fpfcgi.pas

clean:
	delp .
	rm -f fphttpd
	rm -f *~
	rm -f fpget
	rm -f fpfcgi

clear:
	$(MAKE) -C . clean
	$(MAKE) -C ../../../ clean
