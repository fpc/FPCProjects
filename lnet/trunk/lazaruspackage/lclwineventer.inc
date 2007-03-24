
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
//  Events: TWSANetworkEvents;
  LHI: PLCLHandleInfo;
  Temp: TLHandle;
begin
  LHI := PLCLHandleInfo(aData);
  Temp := LHI^.Handle;
  
  if not FInLoop then begin
    FInLoop := True;

    if aFlags and FD_CONNECT = FD_CONNECT then
      if not Temp.Dispose and Assigned(Temp.OnWrite) then
        Temp.OnWrite(Temp);

    if aFlags and FD_READ  = FD_READ then
      if not Temp.Dispose and Assigned(Temp.OnRead) then
        Temp.OnRead(Temp);

    if aFlags and FD_CLOSE = FD_CLOSE then
      if not Temp.Dispose and Assigned(Temp.OnRead) then
        Temp.OnRead(Temp);

    if aFlags and FD_ACCEPT = FD_ACCEPT then
      if not Temp.Dispose and Assigned(Temp.OnRead) then
        Temp.OnRead(Temp);

    if aFlags and FD_WRITE  = FD_WRITE then
      if not Temp.Dispose and Assigned(Temp.OnWrite) then
        Temp.OnWrite(Temp);

    if Temp.Dispose then
      AddForFree(Temp);

    FInLoop := False;

    if Assigned(FFreeRoot) then
      FreeHandles
    else if not Assigned(LHI^.EventHandle) then
      LHI^.EventHandle := AddSocketEventHandler(Temp.Handle, ALL_FLAGS,
                                        @HandleEvents, PtrInt(LHI));
  end else if Assigned(LHI^.EventHandle) then begin
    RemoveEventHandler(LHI^.EventHandle);
    LHI^.EventHandle := nil;
  end;
end;

function TLCLEventer.AddHandle(aHandle: TLHandle): Boolean;
var
  LHI: PLCLHandleInfo;
begin
  {$ERROR TODO: finish in windows you lazy ass}
  TWin32WidgetSet(WidgetSet).OnAsyncSocketMsg := @HandleEvents;

  Result := True;
  SetHandleEventer(aHandle);
  LHI := GetMem(SizeOf(TLCLHandleInfo));
  LHI^.Handle := aHandle;
//  LHI^.WinObject := WSACreateEvent;
  SetInternalData(aHandle, LHI);
  
{  if WSAEventSelect(aHandle.Handle, LHI^.WinObject, ALL_FLAGS) <> 0 then
    Exit(Bail('EventSelect error', WSAGetLastError));}

  LHI^.EventHandle := AddSocketEventHandler(aHandle.Handle, ALL_FLAGS,
                                    @HandleEvents, PtrInt(LHI));
  LHI^.Flags := ALL_FLAGS;
  if not Assigned(LHI^.EventHandle) then
    Exit(Bail('Error on handler', -1));
end;

procedure TLCLEventer.UnplugHandle(aHandle: TLHandle);
var
  LHI: PLCLHandleInfo;
begin
  LHI := GetInternalData(aHandle);
  RemoveSocketEventHandler(LHI^.EventHandle);
//  WSACloseEvent(LHI^.WinObject);
  FreeMem(LHI);
  inherited UnplugHandle(aHandle);
end;


