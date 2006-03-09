{% lkqueueeventer.inc included by levents.pas }

{$ifdef BSD}

{ TLKQueueEventer }

constructor TLKQueueEventer.Create;
begin
  inherited Create;
  FFreeRoot:=nil;
  FFreeIter:=nil;
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
  if Assigned(FFreeRoot) then
    FreeHandles;
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

procedure TLKQueueEventer.AddForFree(aHandle: TLHandle);
begin
  aHandle.FDestroyed:=True;
  if not Assigned(FFreeIter) then begin
    FFreeIter:=aHandle;
    FFreeRoot:=aHandle;
  end else begin
    FFreeIter.FreeNext:=aHandle;
    FFreeIter:=aHandle;
  end;
end;

procedure TLKQueueEventer.FreeHandles;
var
  Temp, Temp2: TLHandle;
begin
  Temp:=FFreeRoot;
  while Assigned(Temp) do begin
    Temp2:=Temp.FreeNext;
    Temp.Free;
    Temp:=Temp2;
  end;
  FFreeRoot:=nil;
  FFreeIter:=nil;
end;

function TLKQueueEventer.AddHandle(aHandle: TLHandle): Boolean;
begin
  Result:=inherited AddHandle(aHandle);
  if FFreeSlot > Length(FChanges) then
    Inflate;
  if not aHandle.FIgnoreWrite then begin
    EV_SET(@FChanges[FFreeSlot], aHandle.FHandle, EVFILT_WRITE,
           EV_ADD or EV_CLEAR, 0, 0, Pointer(aHandle));
    Inc(FFreeSlot);
  end;
  if FFreeSlot > Length(FChanges) then
    Inflate;
  if not aHandle.FIgnoreRead then begin
    EV_SET(@FChanges[FFreeSlot], aHandle.FHandle, EVFILT_READ,
           EV_ADD, 0, 0, Pointer(aHandle));
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
    Bail('Error on kqueue: ', SocketError);
  Result:=n > 0;
  if Result then begin
    for i:=0 to n-1 do begin
      Temp:=TLHandle(FEvents[i].uData);
      Temp.FReferenced:=True;
      
      if (FEvents[i].Flags and EV_ERROR) > 0 then
        if Assigned(Temp.FOnError) then
          Temp.FOnError(Temp);

      if FEvents[i].Filter = EVFILT_WRITE then
        if Assigned(Temp.FOnWrite) then
          Temp.FOnWrite(Temp);

      if FEvents[i].Filter = EVFILT_READ then
        if Assigned(Temp.FOnRead) then
          Temp.FOnRead(Temp);
          
      Temp.FReferenced:=False;
      
      if Temp.FDispose and not Temp.FDestroyed then
        AddForFree(Temp);
    end;
    
    if Assigned(FFreeRoot) then
      FreeHandles;
  end;
end;

function BestEventerClass: TLEventerClass;
begin
  Result:=TLKQueueEventer;
end;

{$endif}

