{
    $Id$

    KCL  -  Kassandra Component Library
    Copyright (C) 1999  by the KCL team
      see file AUTHORS in base directory of this distribution

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


// ===================================================================
// Common methods for clipboard implementation
//
// 	NOTE: Define the symbol "KCL_USEINTERNALCLIPBOARD" to use the
//	generic clipboard implementation. This can be very handy for
//	new KCL targets to have something to start with...
// ===================================================================


// -------------------------------------------------------------------
//   Generic implementation
// -------------------------------------------------------------------

{$IFDEF KCL_USEINTERNALCLIPBOARD}

const
  mimeTextPlain = 'text/plain';


function TClipboard.GetFormatCount: Integer;
begin
  if not priv.IsOpen then
    Result := 0
  else
    Result := priv.NumFormats;
end;

function TClipboard.GetFormat(Index: Integer): String;
var
  entry: PClipboardEntry;
  i: Integer;
begin
  if (not priv.IsOpen) or (Index < 0) or (Index >= priv.NumFormats) then
    Result := ''
  else begin
    entry := priv.FirstEntry;
    for i := 1 to Index do entry := entry^.Next;
    Result := entry^.Format;
  end;
end;

procedure TClipboard.Open;
begin
  priv.IsOpen := True;
end;

procedure TClipboard.Close;
begin
  priv.IsOpen := False;
end;

procedure TClipboard.Clear;
var
  entry, next: PClipboardEntry;
begin
  if not priv.IsOpen then exit;
  entry := priv.FirstEntry;
  while Assigned(entry) do begin
    FreeMem(entry^.Data);
    next := entry^.Next;
    StrDispose(entry^.Format);
    Dispose(entry);
    entry := next;
  end;
  priv.FirstEntry := nil;
  priv.LastEntry := nil;
end;

procedure TClipboard.SetData(MimeFormat: String; const Data; Size: LongWord);
var
  entry: PClipboardEntry;
begin
  if not priv.IsOpen then exit;

  entry := priv.FirstEntry;
  while Assigned(entry) do begin
    if entry^.Format = MimeFormat then begin
      FreeMem(entry^.Data);
      break;
    end;
    entry := entry^.Next;
  end;

  if not Assigned(entry) then begin
    New(Entry);
    if Assigned(priv.LastEntry) then begin
      priv.LastEntry^.Next := Entry;
      priv.LastEntry := Entry;
    end else begin
      priv.FirstEntry := Entry;
      priv.LastEntry := Entry;
    end;
  end;

  entry^.Next := nil;
  entry^.Format := StrNew(PChar(MimeFormat));
  entry^.DataSize := Size;
  GetMem(entry^.Data, Size);
  Move(Data, entry^.Data^, Size);
end;

function TClipboard.GetDataSize(MimeFormat: String): LongWord;
var
  entry: PClipboardEntry;
begin
  Result := 0;
  if not priv.IsOpen then exit;

  entry := priv.FirstEntry;
  while Assigned(entry) do begin
    if entry^.Format = MimeFormat then
      exit(entry^.DataSize);
    entry := entry^.Next;
  end;
end;

procedure TClipboard.GetData(MimeFormat: String; var Data; MaxSize: LongWord);
var
  entry: PClipboardEntry;
begin
  if not priv.IsOpen then exit;

  entry := priv.FirstEntry;
  while Assigned(entry) do begin
    if entry^.Format = MimeFormat then begin
      if MaxSize > entry^.DataSize then
        MaxSize := entry^.DataSize;
      Move(entry^.Data^, Data, MaxSize);
      exit;
    end;
    entry := entry^.Next;
  end;
end;

function TClipboard.GetAsText: String;
var
  size: LongWord;
begin
  Result := '';
  if not priv.IsOpen then exit;

  if HasFormat(mimeTextPlain) then begin
    size := GetDataSize(mimeTextPlain);
    SetLength(Result, size);
    GetData(mimeTextPlain, Result[1], size);
    exit;
  end;
end;

procedure TClipboard.SetAsText(AText: String);
begin
  SetData(mimeTextPlain, AText[1], Length(AText));
end;

procedure TClipboard.SetTextBuf(Buffer: PChar);
begin
  if not priv.IsOpen then exit;
  SetData(mimeTextPlain, Buffer[0], StrLen(Buffer));
end;

{$ENDIF  KCL_USEINTERNALCLIPBOARD}


// -------------------------------------------------------------------
//   Common method implementations, shared among all targets
// -------------------------------------------------------------------

procedure TClipboard.SetComponent(Component: TComponent);
begin
  if not priv.IsOpen then exit;
end;

function TClipboard.GetComponent(AOwner, AParent: TComponent): TComponent;
begin
  Result := nil;
  if not priv.IsOpen then exit;
end;

function TClipboard.HasFormat(MimeFormat: String): Boolean;
begin
  Result := False;
  if not priv.IsOpen then exit;

  if MimeFormat = mimeTextPlain then Result := True;
end;


// -------------------------------------------------------------------
//   Global functions, shared among all targets
// -------------------------------------------------------------------

var
  GlobalClipboard: TClipboard;

function Clipboard: TClipboard;
begin
  if not Assigned(GlobalClipboard) then
    GlobalClipboard := TClipboard.Create;
  Result := GlobalClipboard;
end;

// function SetClipboard(NewClipboard: TClipboard): TClipboard;
// (somewhat pointless at the moment)


{
  $Log$
  Revision 1.1  1999/12/30 21:33:07  sg
  Initial revision

}
