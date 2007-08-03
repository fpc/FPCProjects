program htest;

{$mode objfpc}{$H+}

uses
  SysUtils, PasHiliter;
  
const
  TEST = 'program test; // oh crap uses Classes; begin Writeln(''Oh wow''); @shit(); a[55] := $f1; end.';

begin
  Writeln(PasStrToHtmStr(TEST));
end.

