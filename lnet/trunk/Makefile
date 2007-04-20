
all:
	$(MAKE) -C examples OPT=$(OPT)

argless:
	$(MAKE) -C examples argless OPT=$(OPT)

clean:
	delp .
	delp lib
	delp lib/sys
	delp units
	$(MAKE) -C examples clean
	rm -f *~
