@echo off
del *.o *.ppu
ppc386 -va %1 >> testlog.log

