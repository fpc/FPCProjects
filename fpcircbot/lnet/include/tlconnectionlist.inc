{%MainUnit lnet.pas}
// TLConnection body file
// Automaticly created by listmaker
 constructor TLConnectionList.Create(const Owns: boolean = true);
 begin
   inherited Create(Owns);
 end;
 
 destructor TLConnectionList.Destroy;
 begin
   inherited Destroy;
 end;
 
 function TLConnectionList.Add(AItem: TLConnection): Integer;
 begin
   result:=inherited Add(AItem);
 end;
  function TLConnectionList.Remove(AItem: TLConnection): Integer;
 begin
   result:=inherited Remove(AItem);
 end;
 
 function TLConnectionList.IndexOf(AItem: TLConnection): Integer;
 begin
   result:=inherited IndexOf(AItem);
 end;
 
 function TLConnectionList.GetItem(Index: Integer): TLConnection;
 begin
   result:=TLConnection(inherited GetItem(Index));
 end;
 
 function TLConnectionList.First: TLConnection;
 begin
   result:=TLConnection(inherited First);
 end;
 
 function TLConnectionList.Last: TLConnection;
 begin
   result:=TLConnection(inherited Last);
 end;
 
 procedure TLConnectionList.SetItem(Index: Integer; AItem: TLConnection);
 begin
   inherited SetItem(Index, AItem);
 end;
