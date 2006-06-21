
{ included by LCLNet.pas }

uses
 WinSock2;
  
const
  IOLCL_FLAGS = FD_ACCEPT or FD_READ or FD_CLOSE or FD_CONNECT or FD_WRITE;

//****************************TLCLSocket***************************************

constructor TLCLSocket.Create;
begin
  inherited Create;
  FEventHandle:=nil;
end;

function TLCLSocket.CanSend: Boolean;
begin
  Result:=inherited CanSend;
end;

function TLCLSocket.CanReceive: Boolean;
begin
  Result:=inherited CanReceive;
end;

procedure TLCLSocket.SetNonBlock;
begin
  inherited SetNonBlock;
  RegisterHandler(IOLCL_FLAGS);
end;

procedure TLCLSocket.RegisterHandler(const aFlags: DWord);
begin
  FWinObject:=WSACreateEvent;
  if WSAEventSelect(FHandle, FWinObject, IOLCL_FLAGS) <> 0 then
    Bail('EventSelect error', WSAGetLastError);
  FEventHandle:=AddEventHandler(FWinObject, aFlags, @HandleEvents, PtrInt(Self));
  if not Assigned(FEventHandle) then
    Bail('Error on handler', -1);
end;

procedure TLCLSocket.HandleEvents(aData: PtrInt; aFlags: DWord);
var
  Events: TWSANetworkEvents;
begin
  if not FReferenced then begin
    FReferenced:=True;
    if WSAEnumNetworkEvents(FHandle, FWinObject, @Events) = 0 then begin
      if Events.lNetworkEvents and FD_CONNECT = FD_CONNECT then
        if Assigned(FOnWrite) then
          FOnWrite(Self);

      if Events.lNetworkEvents and FD_READ  = FD_READ then
        if Assigned(FOnRead) then
          FOnRead(Self);

      if Events.lNetworkEvents and FD_CLOSE = FD_CLOSE then
        if Assigned(FOnRead) then
          FOnRead(Self);

      if Events.lNetworkEvents and FD_ACCEPT = FD_ACCEPT then
        if Assigned(FOnRead) then
          FOnRead(Self);

      if Events.lNetworkEvents and FD_WRITE  = FD_WRITE then
        if Assigned(FOnWrite) then
          FOnWrite(Self);

      FReferenced:=False;
      if FDispose then
        Free;
    end else Bail('EventEnum error', WSAGetLastError);
  end;
end;

procedure TLCLSocket.Disconnect;
begin
  RemoveEventHandler(FEventHandle);
  FEventHandle:=nil;
  WSACloseEvent(FWinObject);
  inherited Disconnect;
end;

