program test_006;

uses
  Classes;
  
  {$ASMMODE intel}
  
  function TestAsm: Boolean; register; assembler;
  asm
    MOV     AL,True            
  end;


begin

end.
