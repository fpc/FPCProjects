PP=fpc
BASEARGS=-Fu../../../lib -Fi../../../lib/sys -Sm 
ARGS=$(BASEARGS) $(OPT) -O2 -XX -Xs

all:
	$(PP) $(ARGS) ludp.pas

argless:
	$(PP) $(BASEARGS) $(OPT) ludp.pas	

clean:
	delp .
	rm -f ludp
	rm -f *~

clear:
	$(MAKE) -C . clean
	$(MAKE) -C ../../../ clean
