@echo off
ppc386 %1 > testlog.log
if not "%errorlevel%"=="0" goto :error
goto :end
:error
type testlog.log >>longlog.log
echo . >>longlog.log
echo . >>longlog.log
:end

