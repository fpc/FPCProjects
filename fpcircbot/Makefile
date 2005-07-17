PP=fpc
BASEARGS=-Fulnet -Filnet/include -Fucgi -Fubot -Fibot/include
ARGS=$(BASEARGS) -O2p2 -XX -Xs
LBOT=bot/lfpcbot.pas
LCGI=cgi/cgifpcbot.pas

all:
	$(PP) $(ARGS) $(LBOT)
	$(PP) $(ARGS) $(LCGI)

clean:
	delp bot
	delp cgi
	delp lnet

debug:
	$(PP) $(BASEARGS) -g $(LBOT)
	$(PP) $(BASEARGS) -g $(LCGI)
