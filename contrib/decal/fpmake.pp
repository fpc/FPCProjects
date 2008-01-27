{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;

Var
  P : TPackage;
  T : TTarget;
begin
  With Installer do
    begin
{$endif ALLPACKAGES}

    P:=AddPackage('decal');
{$ifdef ALLPACKAGES}
    P.Directory:='decal';
{$endif ALLPACKAGES}
    P.Version:='2.0.0';
    P.SourcePath.Add('src');
    P.IncludePath.add('src');
//    P.Dependencies.Add('x11');

    T:=P.Targets.AddUnit('decalio.pas');
      with T.Dependencies do
        begin
          AddInclude('decalh.inc');
          AddUnit('decal');
          AddUnit('superstream');
        end;
    T:=P.Targets.AddUnit('decal.pas');
      with T.Dependencies do
        begin
          AddInclude('decalh.inc');
          AddUnit('mwfixedrecsort');
        end;
    T:=P.Targets.AddUnit('mwfixedrecsort.pas');
      with T.Dependencies do
        begin
          AddInclude('decalh.inc');
        end;
    T:=P.Targets.AddUnit('superstream.pas');
      with T.Dependencies do
        begin
          AddInclude('decalh.inc');
        end;

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
