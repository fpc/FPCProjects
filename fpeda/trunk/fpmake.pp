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

    P:=AddPackage('fp-eda');
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='0.0.1';
    P.Dependencies.Add('fcl-base');

    P.Author := 'Mazen NEIFER of the Free Pascal development team';
    P.License := 'LGPL with modification, ';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'Electronic Design Automation basic support for FPC.';
    P.NeedLibC:= false;
    P.OSes:=AllOSes-[embedded];

    P.SourcePath.Add('src');

    T:=P.Targets.AddUnit('fpvcdwriter.pas');

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}

