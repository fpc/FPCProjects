// TStringList body file
// Automaticly created by listmaker
constructor TStringListList.Create(Owns: Boolean = True);
begin
  inherited Create(Owns);
end;
 
destructor TStringListList.Destroy;
begin
  inherited Destroy;
end;
 
function TStringListList.Add(AItem: TStringList): Integer; {$ifdef HASINLINE} inline; {$endif}
begin
  result:=inherited Add(AItem);
end;
 function TStringListList.Remove(AItem: TStringList): Integer;
begin
  result:=inherited Remove(AItem);
end;
 
function TStringListList.IndexOf(AItem: TStringList): Integer;
begin
  result:=inherited IndexOf(AItem);
end;
 
function TStringListList.GetItem(Index: Integer): TStringList; {$ifdef HASINLINE} inline; {$endif}
begin
  result:=TStringList(inherited GetItem(Index));
end;
 
function TStringListList.First: TStringList;
begin
  result:=TStringList(inherited First);
end;
 
function TStringListList.Last: TStringList;
begin
  result:=TStringList(inherited Last);
end;
 
procedure TStringListList.SetItem(Index: Integer; AItem: TStringList); {$ifdef HASINLINE} inline; {$endif}
begin
  inherited SetItem(Index, AItem);
end;
