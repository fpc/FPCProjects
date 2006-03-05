{% lkqueueeventer.inc included by levents.pas }

{$ifdef BSD}

{ TLKQueueEventer }

constructor TLKQueueEventer.Create;
begin
  inherited Create;
//  FFreeList:=TFPObjectList.Create(True);
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
//  FFreeList.Free;
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
begin
  n:=KEvent(FQueue, @FChanges[0], FFreeSlot,
            @FEvents[0], Length(FEvents), @FTimeout);
  FFreeSlot:=0;
  if n < 0 then
    Bail('Error on kqueue: ', SocketError);
  Result:=n > 0;
  if Result then begin
    for i:=0 to n-1 do with TLHandle(FEvents[i].uData) do begin
      FReferenced:=True;
      if (FEvents[i].Flags and EV_ERROR) > 0 then
        if Assigned(FOnError) then
          FOnError(TLHandle(FEvents[i].uData));

      if FEvents[i].Filter = EVFILT_WRITE then
        if Assigned(FOnWrite) then
          FOnWrite(TLHandle(FEvents[i].uData));

      if FEvents[i].Filter = EVFILT_READ then
        if Assigned(FOnRead) then
          FOnRead(TLHandle(FEvents[i].uData));
      // TODO: figure out a better way, this is hell slow
{      if Dispose then
        if FFreeList.IndexOf(TObject(FEvents[i].uData)) < 0 then
          FFreeList.Add(TObject(FEvents[i].uData));}
      FReferenced:=False;
      if FDispose then
        if FDestroyed then
          FreeInstance
        else
          Free;
    end;
{    if FFreeList.Count > 0 then
      FFreeList.Clear;}
  end;
end;

function BestEventerClass: TLEventerClass;
begin
  Result:=TLKQueueEventer;
end;

{$endif}

