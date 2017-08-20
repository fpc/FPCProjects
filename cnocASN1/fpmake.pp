{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;

Var
  P : TPackage;
  T : TTarget;
begin
  With Installer do
    begin
    P:=AddPackage('cnocASN1');
    P.ShortName:='ASN1';
    P.Description := 'A BER/DER encoding and decoding library. It is not a full-blown ASN.1 library.';
    P.Version:='0.9.0';
    P.License := 'LGPL with static linking exception';
    P.Author := 'Joost van der Sluis (CNOC)';
    P.Email := 'joost@cnoc.nl';
    P.Dependencies.Add('fcl-base');

    T:=P.Targets.AddUnit('cnocasn1.pas');
    T:=P.Targets.AddUnit('cnocasn1decoder.pas');
      T.Dependencies.AddUnit('cnocasn1');
    T:=P.Targets.AddUnit('cnocasn1encoder.pas');
      T.Dependencies.AddUnit('cnocasn1');
      T.Dependencies.AddUnit('cnocasn1tagfactory');

    T:=P.Targets.AddUnit('cnocasn1tagfactory.pas');
      T.Dependencies.AddUnit('cnocasn1');
    T:=P.Targets.AddUnit('cnocasn1element.pas');
      T.Dependencies.AddUnit('cnocasn1');
      T.Dependencies.AddUnit('cnocasn1tagfactory');
      T.Dependencies.AddUnit('cnocasn1decoder');
      T.Dependencies.AddUnit('cnocasn1encoder');

    P.ExamplePath.Add('examples');

    P.Targets.AddExampleProgram('asn1parser.pas');
    P.Sources.AddDoc('readme.md');
    P.Sources.AddDoc('COPYING.txt');

    Run;
    end;
end.
