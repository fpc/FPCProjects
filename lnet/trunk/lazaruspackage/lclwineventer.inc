
uses
  lCommon, WinSock2;

const
  ALL_FLAGS = FD_ACCEPT or FD_READ or FD_CLOSE or FD_CONNECT or FD_WRITE;

procedure TLCLEventer.HandleIgnoreError(aHandle: TLHandle);
begin
  // don't do anything
end;

procedure TLCLEventer.HandleIgnoreWrite(aHandle: TLHandle);
begin
  // don't do anything
end;

procedure TLCLEventer.HandleIgnoreRead(aHandle: TLHandle);
begin
  // don't do anything
end;

procedure TLCLEventer.HandleEvents(aData: PtrInt; aFlags: DWord);
var
  Events: TWSANetworkEvents;
  LHI: PLCLHandleInfo;
  Temp: TLHandle;
begin
  LHI:=PLCLHandleInfo(aData);
  Temp:=LHI^.Handle;
  
  if not FInLoop then begin
    FInLoop:=True;
    if WSAEnumNetworkEvents(Temp.Handle, LHI^.WinObject, @Events) = 0 then begin
      if Events.lNetworkEvents and FD_CONNECT = FD_CONNECT then
        if not Temp.Dispose and Assigned(Temp.OnWrite) then
          Temp.OnWrite(Temp);

      if Events.lNetworkEvents and FD_READ  = FD_READ then
        if not Temp.Dispose and Assigned(Temp.OnRead) then
          Temp.OnRead(Temp);

      if Events.lNetworkEvents and FD_CLOSE = FD_CLOSE then
        if not Temp.Dispose and Assigned(Temp.OnRead) then
          Temp.OnRead(Temp);

      if Events.lNetworkEvents and FD_ACCEPT = FD_ACCEPT then
        if not Temp.Dispose and Assigned(Temp.OnRead) then
          Temp.OnRead(Temp);

      if Events.lNetworkEvents and FD_WRITE  = FD_WRITE then
        if not Temp.Dispose and Assigned(Temp.OnWrite) then
          Temp.OnWrite(Temp);

      if Temp.Dispose then
        AddForFree(Temp);
    end else Bail('EventEnum error', WSAGetLastError);
    FInLoop:=False;

    if Assigned(FFreeRoot) then
      FreeHandles
    else if not Assigned(LHI^.EventHandle) then
      LHI^.EventHandle:=AddEventHandler(LHI^.WinObject, ALL_FLAGS,
                                        @HandleEvents, PtrInt(LHI));
  end else if Assigned(LHI^.EventHandle) then begin
    RemoveEventHandler(LHI^.EventHandle);
    LHI^.EventHandle:=nil;
  end;
end;

function TLCLEventer.AddHandle(aHandle: TLHandle): Boolean;
var
  LHI: PLCLHandleInfo;
begin
  Result := True;
  SetHandleEventer(aHandle);
  LHI:=GetMem(SizeOf(TLCLHandleInfo));
  LHI^.Handle:=aHandle;
  LHI^.WinObject:=WSACreateEvent;
  SetInternalData(aHandle, LHI);
  
  if WSAEventSelect(aHandle.Handle, LHI^.WinObject, ALL_FLAGS) <> 0 then
    Exit(Bail('EventSelect error', WSAGetLastError));

  LHI^.EventHandle:=AddEventHandler(LHI^.WinObject, ALL_FLAGS,
                                    @HandleEvents, PtrInt(LHI));
  LHI^.Flags:=ALL_FLAGS;
  if not Assigned(LHI^.EventHandle) then
    Exit(Bail('Error on handler', -1));
end;

procedure TLCLEventer.UnplugHandle(aHandle: TLHandle);
var
  LHI: PLCLHandleInfo;
begin
  LHI:=GetInternalData(aHandle);
  RemoveEventHandler(LHI^.EventHandle);
  WSACloseEvent(LHI^.WinObject);
  FreeMem(LHI);
  inherited UnplugHandle(aHandle);
end;


