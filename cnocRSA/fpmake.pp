{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;

Var
  P : TPackage;
  T : TTarget;
begin
  With Installer do
    begin
    P:=AddPackage('cnocRSA');
    P.ShortName:='RSA';
    P.Description := 'A RSA-SHA256 signature library.';
    P.Version:='0.9.0';
    P.License := 'LGPL with static linking exception';
    P.Author := 'Joost van der Sluis (CNOC)';
    P.Email := 'joost@cnoc.nl';
    P.Dependencies.Add('fcl-base');
    P.Dependencies.Add('cnocASN1');
    P.Dependencies.Add('openssl');
    P.Dependencies.Add('fcl-web');

    T:=P.Targets.AddUnit('cnocrsa.pas');

    P.ExamplePath.Add('examples');

    P.Targets.AddExampleProgram('rsaexample.pp');
    P.Sources.AddDoc('readme.md');
    P.Sources.AddDoc('COPYING.txt');

    Run;
    end;
end.
