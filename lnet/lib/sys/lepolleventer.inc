{% lepolleventer.inc included by levents.pas }

{$ifdef Linux}

{ TLEpollEventer }

const
  BASE_SIZE = 100;

constructor TLEpollEventer.Create;
begin
  inherited Create;
  Inflate;
  FTimeout:=0;
  FEpollFD:=epoll_create(BASE_SIZE);
  if FEPollFD < 0 then
    raise Exception.Create('Unable to create kqueue');
end;

destructor TLEpollEventer.Destroy;
begin
  fpClose(FEpollFD);
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
  inherited AddHandle(aHandle);
  lEvent.events:=EPOLLIN or EPOLLOUT or EPOLLERR;
  lEvent.data.ptr:=aHandle;
  if epoll_ctl(FEpollFD, EPOLL_CTL_ADD, aHandle.FHandle, @lEvent) < 0 then
    raise Exception.Create('Error adding handle to epoll')
  else
    Inc(FCount);
  if FCount > High(FEvents) then
    Inflate;
end;

function TLEpollEventer.CallAction: Boolean;
var
  i, n: Integer;
begin
  n:=epoll_wait(FEpollFD, @FEvents[0], FCount, FTimeout);
  if n < 0 then
    Bail('Error on kqueue: ', SocketError);
  Result:=n > 0;
  if Result then
    for i:=0 to n-1 do with TLHandle(FEvents[i].data.ptr) do begin
      FReferenced:=True;
      if FEvents[i].events and EPOLLERR = EPOLLERR then
        if Assigned(FOnError) then
          FOnError(TLHandle(FEvents[i].data.ptr));
          
      if FEvents[i].events and EPOLLOUT = EPOLLOUT then
        if Assigned(FOnWrite) then
          FOnWrite(TLHandle(FEvents[i].data.ptr));
          
      if FEvents[i].events and EPOLLIN = EPOLLIN then
        if Assigned(FOnRead) then
          FOnRead(TLHandle(FEvents[i].data.ptr));
      FReferenced:=False;
      if FDispose then
        Free;
    end;
end;

function BestEventerClass: TLEventerClass;
begin
  Result:=TLEpollEventer;
end;

{$endif}

