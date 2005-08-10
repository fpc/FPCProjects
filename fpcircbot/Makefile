PP=fpc
BASEARGS=-Fulnet -Fubot/chatext -Filnet/include -Fucgi -Fubot -Fibot/include -Fiinclude -Fucgi/psp
ARGS=$(BASEARGS) -O2p2 -XX -Xs
LBOT=bot/lfpcbot.pas
LCGI=cgi/cgifpcbot.pas
INSTSRC=botinst.pas
INSTBIN1=./botinst
INSTBIN2=./botinst inst

all:
	$(PP) $(ARGS) $(INSTSRC)
	$(INSTBIN1)
	echo "Run make compile."

compile:
	$(PP) $(ARGS) $(LBOT)
	$(PP) $(ARGS) $(LCGI)
	echo "Run make install as root."

install:
	$(INSTBIN2)

clean:
	delp bot
	delp bot/chatext
	delp cgi
	delp lnet
	delp cgi/psp
	delp .
	rm -f cgibin_path.ipt
	rm -f makedb.sh
	rm -f dbscr.sql

debug:
	$(PP) $(BASEARGS) -g $(LBOT)
	$(PP) $(BASEARGS) -g $(LCGI)
