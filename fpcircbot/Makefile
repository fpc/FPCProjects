PP=fpc
BASEARGS=-Fucommon -Fulnet -Fubot/pasdoc -Fubot/markov -Filnet/include -Fucgi/hiliter -Ficgi/hiliter/inc -Fucgi -Fubot -Fibot/include -Fiinclude -Fucgi/psp
ARGS=$(BASEARGS) -O2 -XX -Xs
LBOT=bot/lfpcbot.pas
LCGI1=cgi/cgifpcbot.pas
LCGI2=cgi/cgipastebin.pas
LDUMP=utils/dbdump.pas
LFILL=utils/fillmarkov.pas
INSTSRC=botinst.pas
INSTBIN1=./botinst
INSTBIN2=./botinst inst
TOUCH=touch

all:
	$(PP) $(ARGS) $(INSTSRC)
	$(RM) include/baseinc.inc
	$(TOUCH) include/baseinc.inc
	$(INSTBIN1)
	@echo "Run make compile."

compile:
	$(PP) $(ARGS) $(LBOT)
	$(PP) $(ARGS) $(LCGI1)
	$(PP) $(ARGS) $(LCGI2)
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

clear:
	$(MAKE) clean
	$(RM) include/baseinc.inc
	$(RM) include/hiddeninc.inc
	$(RM) include/baseinc.inc
	$(RM) cgibin_path.ipt
	$(RM) makedb.sh
	$(RM) dbscr.sql

debug:
	$(PP) $(BASEARGS) -g $(LBOT)
	$(PP) $(BASEARGS) -g $(LCGI1)
	$(PP) $(BASEARGS) -g $(LCGI2)
	$(PP) $(BASEARGS) -g $(LDUMP)
	$(PP) $(BASEARGS) -g $(LFILL)
