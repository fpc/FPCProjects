PP=fpc
OPT=
BASEARGS=-Fulnet -Filnet
ARGS=$(BASEARGS) $(OPT) -O2 -XX -Xs
TLIB=lnet/lnet.pas
TTCPS=ltcp/lserver.pas
TTCPC=ltcp/lclient.pas
TUDP=ludp/ludp.pas
TTELNET=ltelnet/ltclient.pas
TFTP=lftp/lftpclient.pas

all:
	$(PP) $(ARGS) $(TUDP)
	$(PP) $(ARGS) $(TTCPC)
	$(PP) $(ARGS) $(TTCPS)
	$(PP) $(ARGS) $(TTELNET)
	$(PP) $(ARGS) $(TFTP)

clean:
	delp .
	delp ltcp
	delp ludp
	delp lnet
	delp ltelnet
	delp lftp

debug:
	$(PP) $(BASEARGS) -g $(TLIB)

memcheck:
	$(PP) $(BASEARGS) -ghl $(TLIB)
