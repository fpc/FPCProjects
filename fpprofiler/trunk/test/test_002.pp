program test_002;

uses
  Classes;

var
  i: integer;

begin
  i := 1;

  case i of
    0: Writeln('Hello');
    1: Writeln('World!');
  end;
end.

