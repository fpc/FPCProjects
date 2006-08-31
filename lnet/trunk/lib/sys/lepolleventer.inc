{% lepolleventer.inc included by levents.pas }

{$ifdef Linux}

{ TLEpollEventer }

const
  BASE_SIZE = 100;
  // bug in fpc 2.0.4-
  EPOLL_CTL_ADD = 1;
  EPOLL_CTL_DEL = 2;
  EPOLL_CTL_MOD = 3;


constructor TLEpollEventer.Create;
var
  lEvent: TEpollEvent;
begin
  inherited Create;
  FFreeList:=TFPObjectList.Create;
  Inflate;
  FTimeout:=0;
  FEpollFD:=epoll_create(BASE_SIZE);
  FEpollReadFD:=epoll_create(BASE_SIZE);
  FEpollMasterFD:=epoll_create(2);
  if (FEPollFD < 0) or (FEpollReadFD < 0) or (FEpollMasterFD < 0) then
    raise Exception.Create('Unable to create epoll');
  lEvent.events:=EPOLLIN or EPOLLOUT or EPOLLERR or EPOLLHUP or EPOLLPRI or EPOLLET;
  lEvent.data.fd:=FEpollFD;
  if epoll_ctl(FEpollMasterFD, EPOLL_CTL_ADD, FEpollFD, @lEvent) < 0 then
    raise Exception.Create('Unable to add FDs to master epoll FD');
  lEvent.data.fd:=FEpollReadFD;
  if epoll_ctl(FEpollMasterFD, EPOLL_CTL_ADD, FEpollReadFD, @lEvent) < 0 then
    raise Exception.Create('Unable to add FDs to master epoll FD');
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

procedure TLEpollEventer.HandleIgnoreError(aHandle: TLHandle);
const
  BASE_BOOL = EPOLLHUP or EPOLLET;
  ERBOOL: array[Boolean] of Integer = (EPOLLERR, 0);
  OUBOOL: array[Boolean] of Integer = (EPOLLOUT or EPOLLPRI, 0);
var
  lEvent: TEpollEvent;
begin
  lEvent.data.ptr:=aHandle;
  lEvent.events:=BASE_BOOL or ERBOOL[aHandle.IgnoreError] or OUBOOL[aHandle.IgnoreWrite];
  if epoll_ctl(FEpollFD, EPOLL_CTL_MOD, aHandle.FHandle, @lEvent) < 0 then
    Bail('Error modifying handle for errors/writes', LSocketError);
end;

procedure TLEpollEventer.HandleIgnoreWrite(aHandle: TLHandle);
begin
  HandleIgnoreError(aHandle);
end;

procedure TLEpollEventer.HandleIgnoreRead(aHandle: TLHandle);
const
  INBOOL: array[Boolean] of Integer = (EPOLLIN, 0);
var
  lEvent: TEpollEvent;
begin
  lEvent.data.ptr:=aHandle;
  lEvent.events:=INBOOL[aHandle.IgnoreRead];
  if epoll_ctl(FEpollReadFD, EPOLL_CTL_MOD, aHandle.FHandle, @lEvent) < 0 then
    Bail('Error modifying handle for reads', LSocketError);
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
  SetLength(FEventsRead, Length(FEvents));
end;

function TLEpollEventer.AddHandle(aHandle: TLHandle): Boolean;
const
  BASE_BOOL = EPOLLHUP or EPOLLPRI or EPOLLET;
  INBOOL: array[Boolean] of Integer = (EPOLLIN, 0);
  ERBOOL: array[Boolean] of Integer = (EPOLLERR, 0);
  OUBOOL: array[Boolean] of Integer = (EPOLLOUT, 0);
var
  lEvent: TEpollEvent;
begin
  Result:=inherited AddHandle(aHandle);
  if Result then begin
    Result:=False;
    lEvent.events:=BASE_BOOL or ERBOOL[aHandle.IgnoreError] or OUBOOL[aHandle.IgnoreWrite];
    lEvent.data.ptr:=aHandle;
    if epoll_ctl(FEpollFD, EPOLL_CTL_ADD, aHandle.FHandle, @lEvent) < 0 then
      Bail('Error adding handle to epoll', LSocketError);
    lEvent.events:=INBOOL[aHandle.IgnoreRead];
    if epoll_ctl(FEpollReadFD, EPOLL_CTL_ADD, aHandle.FHandle, @lEvent) < 0 then
      Bail('Error adding handle to epoll', LSocketError);
    if FCount > High(FEvents) then
      Inflate;
  end;
end;

function Max(const a, b: Integer): Integer; inline;
begin
  if a > b then
    Result:=a
  else
    Result:=b;
end;

function TLEpollEventer.CallAction: Boolean;
var
  i, MasterChanges, Changes, ReadChanges: Integer;
  Temp, TempRead: TLHandle;
  MasterEvents: array[0..1] of TEpollEvent;
begin
  Result:=False;
  Changes:=0;
  ReadChanges:=0;
  MasterChanges:=epoll_wait(FEpollMasterFD, @MasterEvents[0], 2, FTimeout);
  if MasterChanges > 0 then begin
    for i:=0 to MasterChanges-1 do
      if MasterEvents[i].Data.fd = FEpollFD then
        Changes:=epoll_wait(FEpollFD, @FEvents[0], FCount, 0)
      else
        ReadChanges:=epoll_wait(FEpollReadFD, @FEventsRead[0], FCount, 0);
    if (Changes < 0) or (ReadChanges < 0) then
      Bail('Error on epoll: ', LSocketError)
    else
      Result:=Changes + ReadChanges > 0;
    if Result then begin
      FInLoop:=True;
      for i:=0 to Max(Changes, ReadChanges)-1 do begin
        if i < Changes then begin
          Temp:=TLHandle(FEvents[i].data.ptr);
          if (FEvents[i].events and EPOLLERR = EPOLLERR)
          or (FEvents[i].events and EPOLLHUP = EPOLLHUP) then
            if Assigned(Temp.FOnError) then
              Temp.FOnError(Temp, 'Handle error' + LStrError(LSocketError));
              
          if (FEvents[i].events and EPOLLOUT = EPOLLOUT)
          or (FEvents[i].events and EPOLLPRI = EPOLLPRI) then
            if Assigned(Temp.FOnWrite) then
              Temp.FOnWrite(Temp);
          if Temp.FDispose then
            AddForFree(Temp);
        end; // normals
        
        if i < ReadChanges then begin
          TempRead:=TLHandle(FEventsRead[i].data.ptr);

          if FEventsRead[i].events and EPOLLIN = EPOLLIN then
            if Assigned(TempRead.FOnRead) then
              TempRead.FOnRead(TempRead);
          if TempRead.FDispose then
            AddForFree(TempRead);
        end; // reads
      end;
      FInLoop:=False;
      if Assigned(FFreeRoot) then
        FreeHandles;
    end;
  end;
end;

function BestEventerClass: TLEventerClass;

  function GetVersion(s: string): Integer;
  const
    Numbers = ['0'..'9'];
  var
    i: Integer;
  begin
    s:=StringReplace(s, '.', '', [rfReplaceAll]);
    i:=1;
    while (i <= Length(s)) and (s[i] in Numbers) do
      Inc(i);
    s:=Copy(s, 1, i - 1);
    if Length(s) < 4 then // varies OS to OS
      Insert('0', s, 3); // in linux, last part can be > 10
    Result:=StrToInt(s);
  end;

var
  u: TUTSName;
begin
  Result:=TLSelectEventer;
  if fpUname(u) = 0 then   // check for 2.6+
    if GetVersion(u.release) >= 2600 then
      Result:=TLEpollEventer;
end;

{$endif} // Linux
