program test_005;

uses
  Classes;

begin
  {$IFDEF Win32}
    Writeln('Bil');
  {$ELSE}
    Writeln('Linus');
  {$ENDIF}
end.
