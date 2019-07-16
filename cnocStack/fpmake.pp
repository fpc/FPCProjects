{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;

Var
  T : TTarget;
  P : TPackage;
begin
  With Installer do
    begin
{$endif ALLPACKAGES}

    P:=AddPackage('cnocStack');

    P.Version:='0.5.1';
    P.Author := 'Joost van der Sluis / CNOC';
    P.License := 'LGPL with modification, ';
    P.HomepageURL := 'www.lazarussupport.com';
    P.Email := 'joost@cnoc.nl';
    P.Description := 'Simple queue based bus, with libraries to communicate with it';
    P.OSes:=AllOSes;

    P.Dependencies.Add('fcl-net');
    P.Dependencies.Add('log4fpc');
    P.Dependencies.Add('rtl-generics');
    P.Dependencies.Add('fcl-json');
    P.Dependencies.Add('cnocThreadingTools');

    T:=P.Targets.AddUnit('cnocstackbinaryclient.pp');
    T:=P.Targets.AddUnit('cnocstackerrorcodes.pp');
    T:=P.Targets.AddUnit('cnocstackhandlerthread.pp');
    T:=P.Targets.AddUnit('cnocstackjsonhandlerthread.pp');
    T:=P.Targets.AddUnit('cnocstackbinaryreaderthread.pp');
    T:=P.Targets.AddUnit('cnocstackmessagetypes.pp');
    T:=P.Targets.AddUnit('cnocstackbinarylistener.pp');
    T:=P.Targets.AddUnit('cnocstackinternalstack.pp');
    T:=P.Targets.AddUnit('cnocstackinternalsenderhandler.pp');

    P.Targets.AddExampleUnit('tests/connectiontests.pp');
    P.Targets.AddExampleUnit('tests/testmessageformat.pp');
    P.Targets.AddExampleProgram('tests/cnocconsoletestrunner.pp');

    P.Targets.AddExampleProgram('examples/cnocstack.pp');
{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}

