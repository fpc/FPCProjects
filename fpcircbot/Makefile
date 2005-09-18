PP=fpc
BASEARGS=-Fucommon -Fulnet -Fubot/pasdoc -Fubot/markov -Filnet/include -Fucgi -Fubot -Fibot/include -Fiinclude -Fucgi/psp
ARGS=$(BASEARGS) -O2p2 -XX -Xs
LBOT=bot/lfpcbot.pas
LCGI=cgi/cgifpcbot.pas
LDUMP=utils/dbdump.pas
LFILL=utils/fillmarkov.pas
INSTSRC=botinst.pas
INSTBIN1=./botinst
INSTBIN2=./botinst inst

all:
	$(PP) $(ARGS) $(INSTSRC)
	rm -f include/baseinc.inc
	touch include/baseinc.inc
	$(INSTBIN1)
	echo "Run make compile."

compile:
	$(PP) $(ARGS) $(LBOT)
	$(PP) $(ARGS) $(LCGI)
	$(PP) $(ARGS) $(LDUMP)
	$(PP) $(ARGS) $(LFILL)
	echo "Run make inst as root."

inst:
	$(INSTBIN2)

clean:
	delp bot
	delp bot/markov
	delp bot/pasdoc
	delp cgi
	delp lnet
	delp cgi/psp
	delp common
	delp utils
	delp .
	rm -f cgibin_path.ipt
	rm -f makedb.sh
	rm -f dbscr.sql

debug:
	$(PP) $(BASEARGS) -g $(LBOT)
	$(PP) $(BASEARGS) -g $(LCGI)
	$(PP) $(BASEARGS) -g $(LDUMP)
	$(PP) $(BASEARGS) -g $(LFILL)
