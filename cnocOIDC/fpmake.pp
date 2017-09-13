{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;

Var
  P : TPackage;
  T : TTarget;
begin
  With Installer do
    begin
    P:=AddPackage('cnocOIDC');
    P.ShortName:='OIDC';
    P.Description := 'A library to authenticate against an OpenID Provider.';
    P.Version:='0.9.0';
    P.License := 'LGPL with static linking exception';
    P.Author := 'Joost van der Sluis (CNOC)';
    P.Email := 'joost@cnoc.nl';
    P.Dependencies.Add('fcl-base');
    P.Dependencies.Add('cnocRSA');
    P.Dependencies.Add('fcl-web');

    T:=P.Targets.AddUnit('cnocopenidconnect.pas');
    T:=P.Targets.AddUnit('cnocoidcidtoken.pas');

    P.Sources.AddDoc('readme.md');
    P.Sources.AddDoc('COPYING.txt');

    Run;
    end;
end.
