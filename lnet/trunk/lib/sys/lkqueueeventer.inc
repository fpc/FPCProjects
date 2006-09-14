{% lkqueueeventer.inc included by levents.pas }

{$ifdef BSD}

{ TLKQueueEventer }

constructor TLKQueueEventer.Create;
begin
  inherited Create;
  Inflate;
  FFreeSlot:=0;
  FTimeout.tv_sec:=0;
  FTimeout.tv_nsec:=0;
  FQueue:=KQueue;
  if FQueue < 0 then
    raise Exception.Create('Unable to create kqueue');
end;

destructor TLKQueueEventer.Destroy;
begin
  fpClose(FQueue);
  inherited Destroy;
end;

function TLKQueueEventer.GetTimeout: DWord;
begin
  Result:=FTimeout.tv_sec + FTimeout.tv_nsec * 1000 * 1000;
end;

procedure TLKQueueEventer.SetTimeout(const Value: DWord);
begin
  FTimeout.tv_sec:=Value div 1000;
  FTimeout.tv_nsec:=(Value mod 1000) * 1000;
end;

procedure TLKQueueEventer.HandleIgnoreWrite(aHandle: TLHandle);
const
  OUBOOL: array[Boolean] of Integer = (EV_ENABLE, EV_DISABLE);
begin
  if not Assigned(aHandle.HandleData[0]) then begin
    aHandle.HandleData[0]:=@FChanges[FFreeSlot];
    Inc(FFreeSlot);
    if FFreeSlot > Length(FChanges) then
      Inflate;
  end;

  EV_SET(aHandle.HandleData[0], aHandle.FHandle, EVFILT_WRITE,
         OUBOOL[aHandle.IgnoreWrite], 0, 0, Pointer(aHandle));

  if KEvent(FQueue, aHandle.HandleData[0], 1, nil, 0, nil) < 0 then
    Bail('Error on KEvent/setignore', LSocketError);
end;

procedure TLKQueueEventer.HandleIgnoreRead(aHandle: TLHandle);
const
  INBOOL: array[Boolean] of Integer = (EV_ENABLE, EV_DISABLE);
begin
  if not Assigned(aHandle.HandleData[1]) then begin
    aHandle.HandleData[1]:=@FChanges[FFreeSlot];
    Inc(FFreeSlot);
    if FFreeSlot > Length(FChanges) then
      Inflate;
  end;

  EV_SET(aHandle.HandleData[1], aHandle.FHandle, EVFILT_READ,
         INBOOL[aHandle.IgnoreRead], 0, 0, Pointer(aHandle));

  if KEvent(FQueue, aHandle.HandleData[1], 1, nil, 0, nil) < 0 then
    Bail('Error on KEvent/setignore', LSocketError);
end;

procedure TLKQueueEventer.Inflate;
const
  BASE_SIZE = 100;
var
  OldLength: Integer;
begin
  OldLength:=Length(FChanges);
  if OldLength > 1 then begin
    SetLength(FChanges, Sqr(OldLength));
    SetLength(FEvents, Sqr(OldLength));
  end else begin
    SetLength(FChanges, BASE_SIZE);
    SetLength(FEvents, BASE_SIZE);
  end;
end;

function TLKQueueEventer.AddHandle(aHandle: TLHandle): Boolean;
begin
  Result:=inherited AddHandle(aHandle);

  SetLength(aHandle.HandleData, 2);
  aHandle.HandleData[0]:=nil;
  aHandle.HandleData[1]:=nil;

  if FFreeSlot > Length(FChanges) then
    Inflate;
  if not aHandle.FIgnoreWrite then begin
    EV_SET(@FChanges[FFreeSlot], aHandle.FHandle, EVFILT_WRITE,
           EV_ADD or EV_CLEAR, 0, 0, Pointer(aHandle));
    aHandle.HandleData[0]:=@FChanges[FFreeSlot];
    Inc(FFreeSlot);
  end;
  if FFreeSlot > Length(FChanges) then
    Inflate;
  if not aHandle.FIgnoreRead then begin
    EV_SET(@FChanges[FFreeSlot], aHandle.FHandle, EVFILT_READ,
           EV_ADD, 0, 0, Pointer(aHandle));
    aHandle.HandleData[1]:=@FChanges[FFreeSlot];
    Inc(FFreeSlot);
  end;
end;

function TLKQueueEventer.CallAction: Boolean;
var
  i, n: Integer;
  Temp: TLHandle;
begin
  n:=KEvent(FQueue, @FChanges[0], FFreeSlot,
            @FEvents[0], Length(FEvents), @FTimeout);
  FFreeSlot:=0;
  if n < 0 then
    Bail('Error on kqueue: ', LSocketError);
  Result:=n > 0;
  if Result then begin
    FInLoop:=True;
    for i:=0 to n-1 do begin
      Temp:=TLHandle(FEvents[i].uData);
      
      if  (not Temp.FDispose)
      and (FEvents[i].Filter = EVFILT_WRITE) then
        if Assigned(Temp.FOnWrite) and not Temp.IgnoreWrite then
          Temp.FOnWrite(Temp);

      if  (not Temp.FDispose)
      and (FEvents[i].Filter = EVFILT_READ) then
        if Assigned(Temp.FOnRead) and not Temp.IgnoreRead then
          Temp.FOnRead(Temp);
      
      if  (not Temp.FDispose)
      and ((FEvents[i].Flags and EV_ERROR) > 0) then
        if Assigned(Temp.FOnError) and not Temp.IgnoreError then
          Temp.FOnError(Temp, 'Handle error' + LStrError(LSocketError));

      if Temp.FDispose then
        AddForFree(Temp);
    end;
    FInLoop:=False;
    if Assigned(FFreeRoot) then
      FreeHandles;
  end;
end;

function BestEventerClass: TLEventerClass;
begin
  Result:=TLKQueueEventer;
end;

{$endif} // BSD

