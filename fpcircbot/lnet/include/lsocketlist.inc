
 constructor TLSocketList.Create;
 begin
   inherited Create(true);
 end;

 destructor TLSocketList.Destroy;
 begin
   inherited Destroy;
 end;

 function TLSocketList.Add(ASocket: TLSocket): Integer;
 begin
   result:=inherited Add(ASocket);
 end;
 
 function TLSocketList.Remove(ASocket: TLSocket): Integer;
 begin
   result:=inherited Remove(ASocket);
 end;
 
 function TLSocketList.IndexOf(ASocket: TLSocket): Integer;
 begin
   result:=inherited IndexOf(ASocket);
 end;

 function TLSocketList.GetItem(Index: Integer): TLSocket;
 begin
   result:=TLSocket(inherited GetItem(Index));
 end;

 function TLSocketList.First: TLSocket;
 begin
   result:=TLSocket(inherited First);
 end;
 
 function TLSocketList.Last: TLSocket;
 begin
   result:=TLSocket(inherited Last);
 end;

 procedure TLSocketList.SetItem(Index: Integer; ASocket: TLSocket);
 begin
   inherited SetItem(Index, ASocket);
 end;

