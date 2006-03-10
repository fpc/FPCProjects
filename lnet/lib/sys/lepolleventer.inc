{% lepolleventer.inc included by levents.pas }

{$ifdef Linux}

{ TLEpollEventer }

const
  BASE_SIZE = 100;

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
var
  lEvent: TEpollEvent;
begin
  Result:=inherited AddHandle(aHandle);
  if Result then begin
    Result:=False;
    lEvent.events:=EPOLLOUT or EPOLLERR or EPOLLHUP or EPOLLPRI or EPOLLET;
    lEvent.data.ptr:=aHandle;
    if epoll_ctl(FEpollFD, EPOLL_CTL_ADD, aHandle.FHandle, @lEvent) < 0 then
      Bail('Error adding handle to epoll', LSocketError);
    lEvent.events:=EPOLLIN;
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
  MasterChanges:=epoll_wait(FEpollMasterFD, @MasterEvents, 2, FTimeout);
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
      for i:=0 to Max(Changes, ReadChanges)-1 do begin
        if i < Changes then begin
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
        end; // normals
        if i < ReadChanges then begin
          TempRead:=TLHandle(FEventsRead[i].data.ptr);
          TempRead.FReferenced:=True;

          if FEventsRead[i].events and EPOLLIN = EPOLLIN then
            if Assigned(TempRead.FOnRead) then
              TempRead.FOnRead(TempRead);
        end; // reads

        if i < Changes then begin
          Temp.FReferenced:=False;
          if Temp.FDispose and (Temp <> TempRead) then
            if Temp.FDestroyed then
              Temp.FreeInstance
            else
              Temp.Free;
        end;

        if i < ReadChanges then begin
          TempRead.FReferenced:=False;
          if TempRead.FDispose then
            if TempRead.FDestroyed then
              TempRead.FreeInstance
            else
              TempRead.Free;
        end;
      end;
    end;
  end;
end;

function BestEventerClass: TLEventerClass;
begin
  Result:=TLEpollEventer;
end;

{$endif}

