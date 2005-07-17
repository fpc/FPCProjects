// TLCommand body file
// Automaticly created by listmaker
constructor TLCommandList.Create(Owns: Boolean = True);
begin
  inherited Create(Owns);
end;
 
destructor TLCommandList.Destroy;
begin
  inherited Destroy;
end;
 
function TLCommandList.Add(AItem: TLCommand): Integer; {$ifdef HASINLINE} inline; {$endif}
begin
  result:=inherited Add(AItem);
end;
 function TLCommandList.Remove(AItem: TLCommand): Integer;
begin
  result:=inherited Remove(AItem);
end;
 
function TLCommandList.IndexOf(AItem: TLCommand): Integer;
begin
  result:=inherited IndexOf(AItem);
end;
 
function TLCommandList.GetItem(Index: Integer): TLCommand; {$ifdef HASINLINE} inline; {$endif}
begin
  result:=TLCommand(inherited GetItem(Index));
end;
 
function TLCommandList.First: TLCommand;
begin
  result:=TLCommand(inherited First);
end;
 
function TLCommandList.Last: TLCommand;
begin
  result:=TLCommand(inherited Last);
end;
 
procedure TLCommandList.SetItem(Index: Integer; AItem: TLCommand); {$ifdef HASINLINE} inline; {$endif}
begin
  inherited SetItem(Index, AItem);
end;
