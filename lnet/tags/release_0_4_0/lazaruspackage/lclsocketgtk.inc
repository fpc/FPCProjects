
{ included by LCLNet.pas }

uses
  Sockets, lCommon, Forms;

const
  G_IO_IN     = 1;
  G_IO_OUT    = 4;
  G_IO_PRI    = 2;
  G_IO_ERR    = 8;
  G_IO_HUP    = 16;
  G_IO_NVAL   = 32;
  
  IOLCL_FLAGS = G_IO_ERR or G_IO_NVAL or G_IO_HUP or G_IO_PRI;

//****************************TLCLSocket***************************************

constructor TLCLSocket.Create;
begin
  inherited Create;
  FEventHandle:=nil;
  FFlags:=IOLCL_FLAGS or G_IO_OUT or G_IO_IN;
end;

function TLCLSocket.CanSend: Boolean;
begin
  Result:=inherited CanSend;
  FFlags:=FFlags or G_IO_OUT;
  SetEventHandlerFlags(FEventHandle, FFlags);
end;

function TLCLSocket.CanReceive: Boolean;
begin
  Result:=inherited CanReceive;
  FFlags:=FFlags or G_IO_IN;
  SetEventHandlerFlags(FEventHandle, FFlags);
end;

procedure TLCLSocket.SetNonBlock;
begin
  inherited SetNonBlock;
  RegisterHandler(IOLCL_FLAGS or G_IO_OUT or G_IO_IN);
end;

procedure TLCLSocket.RegisterHandler(const aFlags: DWord);
begin
  FEventHandle:=AddEventHandler(FHandle, aFlags, @HandleEvents, PtrInt(Self));
  FFlags:=aFlags;
  if not Assigned(FEventHandle) then
    Bail('Error on handler', -1);
end;

procedure TLCLSocket.HandleEvents(aData: PtrInt; aFlags: DWord);
{
   HUGE WARNING: due to a certain hack, ControlSock must be the LAST THING called.
                 No instance variable or method must be accessed after this call.
}
begin
  FReferenced:=True;
  SetEventHandlerFlags(FEventHandle, 0);
  if (aFlags and G_IO_ERR = G_IO_ERR)
  or (aFlags and G_IO_NVAL = G_IO_NVAL)
  or (aFlags and G_IO_HUP = G_IO_HUP) then
    if Assigned(FOnError) then
      FOnError(Self, 'Handle error' + LStrError(LSocketError));

  if aFlags and G_IO_OUT = G_IO_OUT then begin
    if not Connecting then begin
      FFlags:=FFlags and (not G_IO_OUT);
      FCanSend:=True;
    end;
    if Assigned(FOnWrite) then
      FOnWrite(Self);
  end;

  if aFlags and G_IO_IN = G_IO_IN then begin
//    {$warning Ensure this can be truly commented out}
//    FFlags:=FFlags and (not G_IO_IN); // Make sure this is OK to be out
//    SetEventHandlerFlags(FEventHandle, FFlags);
    FCanReceive:=True;
    if Assigned(FOnRead) then
      FOnRead(Self);
  end;

  SetEventHandlerFlags(FEventHandle, FFlags);

  FReferenced:=False;
  if FDispose then
    Free;
end;

procedure TLCLSocket.Disconnect;
begin
  RemoveEventHandler(FEventHandle);
  FEventHandle:=nil;
  inherited Disconnect;
end;
