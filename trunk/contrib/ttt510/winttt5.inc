Procedure MoveFromScreen(Source: longint;var Dest;Length:Word);
{ assumes screen segment = hi(source) and screen offset = lo(source) }
begin
  { removed retrace waiting (only necessary for CGA cards) }
  dosmemget(source shr 16,source and $ffff,dest,length*2);
end;

procedure MoveToScreen(var source; Dest: longint; Length : Word);
begin
  dosmemput(dest shr 16,dest and $ffff,source,length*2);
end;