{% lepolleventer.inc included by levents.pas }

{$ifdef Linux}

{ TLEpollEventer }

const
  BASE_SIZE = 100;

constructor TLEpollEventer.Create;
begin
  inherited Create;
  FFreeList:=TFPObjectList.Create;
  Inflate;
  FTimeout:=0;
  FEpollFD:=epoll_create(BASE_SIZE);
  if FEPollFD < 0 then
    raise Exception.Create('Unable to create kqueue');
end;

destructor TLEpollEventer.Destroy;
begin
  fpClose(FEpollFD);
  FFreeList.Free;
  inherited Destroy;
end;

function TLEpollEventer.GetTimeout: DWord;
begin
  Result:=DWord(FTimeout);
end;

procedure TLEpollEventer.SetTimeout(const Value: DWord);
begin
  FTimeout:=cInt(Value);
end;

procedure TLEpollEventer.Inflate;
var
  OldLength: Integer;
begin
  OldLength:=Length(FEvents);
  if OldLength > 1 then
    SetLength(FEvents, Sqr(OldLength))
  else
    SetLength(FEvents, BASE_SIZE);
end;

function TLEpollEventer.AddHandle(aHandle: TLHandle): Boolean;
var
  lEvent: TEpollEvent;
begin
  Result:=inherited AddHandle(aHandle);
  if Result then begin
    Result:=False;
    lEvent.events:=EPOLLIN or EPOLLOUT or EPOLLERR or EPOLLHUP or EPOLLPRI or EPOLLET;
    lEvent.data.ptr:=aHandle;
    if epoll_ctl(FEpollFD, EPOLL_CTL_ADD, aHandle.FHandle, @lEvent) < 0 then
      raise Exception.Create('Error adding handle to epoll')
    else begin
      Inc(FCount);
      Result:=True;
    end;
    if FCount > High(FEvents) then
      Inflate;
  end;
end;

function TLEpollEventer.CallAction: Boolean;
var
  i, n: Integer;
  Temp: TLHandle;
begin
  n:=epoll_wait(FEpollFD, @FEvents[0], FCount, FTimeout);
  if n < 0 then
    Bail('Error on kqueue: ', SocketError);
  Result:=n > 0;
  if Result then begin
    for i:=0 to n-1 do begin
      Temp:=TLHandle(FEvents[i].data.ptr);
      Temp.FReferenced:=True;
      if (FEvents[i].events and EPOLLERR = EPOLLERR)
      or (FEvents[i].events and EPOLLHUP = EPOLLHUP) then
        if Assigned(Temp.FOnError) then
          Temp.FOnError(Temp);
          
      if (FEvents[i].events and EPOLLOUT = EPOLLOUT)
      or (FEvents[i].events and EPOLLPRI = EPOLLPRI) then
        if Assigned(Temp.FOnWrite) then
          Temp.FOnWrite(Temp);
          
      if FEvents[i].events and EPOLLIN = EPOLLIN then
        if Assigned(Temp.FOnRead) then
          Temp.FOnRead(Temp);
      Temp.FReferenced:=False;
      if Temp.FDispose then
        if Temp.FDestroyed then
          Temp.FreeInstance
        else
          Temp.Free;
    end;
  end;
end;

function BestEventerClass: TLEventerClass;
begin
  Result:=TLEpollEventer;
end;

{$endif}

