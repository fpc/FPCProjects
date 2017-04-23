{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit, sysutils;

Var
  P : TPackage;
  T : TTarget;

begin
  With Installer do
    begin
    P:=AddPackage('lazmkunit');

    P.Version:='0.9.5-1';
    P.OSes:=AllUnixOSes+[Win32,Win64];
    P.Author := 'Joost van der Sluis';
    P.License := 'LGPL with modification';
    P.HomepageURL := 'http://www.lazarussupport.nl/';
    P.Email := 'joost@cnoc.nl';
    P.Description := 'Tools to use fppkg with Lazarus';
    P.Options.add('-Sm');
    P.IsFPMakeAddIn:=True;

    P.Dependencies.Add('fpmkunit');
    // These dependency is a dependency and should not be added here, but because
    // of a bug in fpcmake which doesn't detect the dependencies right, this
    // has to be added temporarily
    P.Dependencies.Add('fcl-process');
    P.Dependencies.Add('paszlib');
    P.Dependencies.Add('hash');
    P.Dependencies.Add('lazutils');

    T:=P.Targets.AddUnit('lazmkunit.pp');
    T:=P.Targets.AddUnit('lazfpmakeplugin.pp');
    T.IsFPMakePlugin := True;

    Run;
    end;
end.

