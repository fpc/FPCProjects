PP=fpc
OPT=
BASEARGS=-Fulib -Fulib/sys -Filib -Filib/sys -Sm 
ARGS=$(BASEARGS) $(OPT) -O2 -XX -Xs
TLIB=examples/console/lnet/lnet.pas
TTCPS=examples/console/ltcp/lserver.pas
TTCPC=examples/console/ltcp/lclient.pas
TUDP=examples/console/ludp/ludp.pas
TTELNET=examples/console/ltelnet/ltclient.pas
TFTP=examples/console/lftp/lftpclient.pas
THTTP=examples/console/lhttp/fphttpd.pas
TSMTP=examples/console/lsmtp/lsmtpclient.pas

all:
	$(PP) $(ARGS) $(TUDP)
	$(PP) $(ARGS) $(TTCPC)
	$(PP) $(ARGS) $(TTCPS)
	$(PP) $(ARGS) $(TTELNET)
	$(PP) $(ARGS) $(TFTP)
	$(PP) $(ARGS) $(TSMTP)

clean:
	delp .
	delp lib
	delp lib/sys
	delp examples/console/ludp
	delp examples/console/ltcp
	delp examples/console/ltelnet
	delp examples/console/lftp
	delp examples/console/lhttp
	delp examples/console/lsmtp

debug:
	$(PP) $(BASEARGS) -g $(TLIB)

memcheck:
	$(PP) $(BASEARGS) -ghl $(TLIB)

http:
	$(PP) $(ARGS) $(THTTP)
