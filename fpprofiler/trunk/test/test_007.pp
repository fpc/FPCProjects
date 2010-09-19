program test_007;

uses
  Classes;
  
type
  TMyArray = record
    case boolean of
      True: (byte1: array [0..255] of byte);
      False: (byte4: array [0 .. 63] of dword);
  end;


begin

end.
