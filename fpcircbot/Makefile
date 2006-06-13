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
ECHO=echo
RM=rm -f
TOUCH=touch

all:
	$(PP) $(ARGS) $(INSTSRC)
	$(RM) include/baseinc.inc
	$(TOUCH) include/baseinc.inc
	$(INSTBIN1)
	@$(ECHO) "Run make compile."

compile:
	$(PP) $(ARGS) $(LBOT)
	$(PP) $(ARGS) $(LCGI)
	$(PP) $(ARGS) $(LDUMP)
	$(PP) $(ARGS) $(LFILL)
	@$(ECHO) "Run make inst as root."

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
	$(RM) cgibin_path.ipt
	$(RM) makedb.sh
	$(RM) dbscr.sql

clear:
	$(MAKE) clean
	$(RM) include/baseinc.inc
	$(RM) include/hiddeninc.inc
	$(RM) include/baseinc.inc

debug:
	$(PP) $(BASEARGS) -g $(LBOT)
	$(PP) $(BASEARGS) -g $(LCGI)
	$(PP) $(BASEARGS) -g $(LDUMP)
	$(PP) $(BASEARGS) -g $(LFILL)
