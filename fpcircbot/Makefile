PP=fpc
BASEARGS=-Fulnet -Filnet/include -Fucgi -Fubot -Fibot/include -Fiinclude -Fucgi/psp
ARGS=$(BASEARGS) -O2p2 -XX -Xs
LBOT=bot/lfpcbot.pas
LCGI=cgi/cgifpcbot.pas
INSTSRC=botinst.pas
INSTBIN1=./botinst
INSTBIN2=./botinst inst

all:
	$(PP) $(ARGS) $(INSTSRC)
	$(INSTBIN1)
	$(PP) $(ARGS) $(LBOT)
	$(PP) $(ARGS) $(LCGI)
	echo "Run make install as root."

manual:
	$(PP) $(ARGS) $(LBOT)
	$(PP) $(ARGS) $(LCGI)

install:
	$(INSTBIN2)

clean:
	delp bot
	delp cgi
	delp lnet
	delp cgi/psp
	delp .

debug:
	$(PP) $(BASEARGS) -g $(LBOT)
	$(PP) $(BASEARGS) -g $(LCGI)
