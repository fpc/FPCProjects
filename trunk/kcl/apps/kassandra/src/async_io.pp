{
    $Id$

    Async_IO: Mananging class for asynchronous input/output
    Copyright (C) 2000 by Sebastian Guenther (sg@freepascal.org)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{$MODE objfpc}
{$H+}

unit Async_IO;

interface

uses Linux, Classes;

// *** Begin Linux specific
const
  MaxHandle = SizeOf(TFDSet) * 8 - 1;
// *** End Linux specific

type

// -------------------------------------------------------------------
//   TAsyncIOManager
// -------------------------------------------------------------------

  TAsyncIOManager = class;

  TAsyncIONotify = procedure(UserData: TObject) of object;

  TAsyncIONotifyInfo = record
    Method: TAsyncIONotify;
    UserData: TObject;
  end;

  TAsyncIOManager = class
  protected
    DoBreak: Boolean;
// *** Begin Linux specific
    ReadMap, WriteMap: TFDSet;
    ReadNotifies, WriteNotifies: array[0..MaxHandle] of TAsyncIONotifyInfo;
    HighestHandle: Integer;
    procedure CalcHighestHandle(max: Integer);
// *** End Linux specific
    FTimeout: Integer;
    TimeoutNotify: TAsyncIONotifyInfo;
    procedure ExecuteNotify(const Notify: TAsyncIONotifyInfo);
    function  GetHandleAsync(AHandle: Integer): Boolean;
    procedure SetHandleAsync(AHandle: Integer; AValue: Boolean);
  public
    constructor Create;
    procedure Run;
    procedure BreakRun;
    procedure SetReadHandler(AHandle: Integer; AMethod: TAsyncIONotify; AUserData: TObject);
    procedure ClearReadHandler(AHandle: Integer);
    procedure SetWriteHandler(AHandle: Integer; AMethod: TAsyncIONotify; AUserData: TObject);
    procedure ClearWriteHandler(AHandle: Integer);
    procedure SetTimeoutHandler(AMethod: TAsyncIONotify; AUserData: TObject);
    procedure ClearTimeoutHandler;
    property  Timeout: Integer read FTimeout write FTimeout;
    property  HandleAsync[AHandle: Integer]: Boolean read GetHandleAsync write SetHandleAsync;
  end;


// -------------------------------------------------------------------
//   Line reader classes
// -------------------------------------------------------------------

  TLineNotify = procedure(const line: String) of object;

  TGenericLineReader = class
  protected
    RealBuffer, FBuffer: PChar;
    FBytesInBuffer: Integer;
    FOnLine: TLineNotify;

    function  Read(var ABuffer; count: Integer): Integer; virtual; abstract;
    procedure NoData; virtual; abstract;

  public
    destructor Destroy; override;
    procedure Run;		// Process as many lines as possible

    property Buffer: PChar read FBuffer;
    property BytesInBuffer: Integer read FBytesInBuffer;
    property OnLine: TLineNotify read FOnLine write FOnLine;
  end;


  TAsyncStreamLineReader = class(TGenericLineReader)
  protected
    FManager: TAsyncIOManager;
    FDataStream: TStream;
    FBlockingStream: THandleStream;
    FOnEOF: TNotifyEvent;

    function  Read(var ABuffer; count: Integer): Integer; override;
    procedure NoData; override;
    procedure StreamDataAvailable(UserData: TObject);
  public
    constructor Create(AManager: TAsyncIOManager; AStream: THandleStream);
    constructor Create(AManager: TAsyncIOManager;
      ADataStream: TStream; ABlockingStream: THandleStream);
    destructor Destroy; override;

    property DataStream: TStream read FDataStream;
    property BlockingStream: THandleStream read FBlockingStream;
    property OnEOF: TNotifyEvent read FOnEOF write FOnEOF;
  end;


// -------------------------------------------------------------------
//   TWriteBuffer
// -------------------------------------------------------------------

  TWriteBuffer = class(TStream)
  protected
    FBuffer: PChar;
    FBytesInBuffer: Integer;
    FOnBufferEmpty: TNotifyEvent;

    function  Seek(Offset: LongInt; Origin: Word): LongInt; override;
    function  Write(const ABuffer; Count: LongInt): LongInt; override;
    function  DoRealWrite(const ABuffer; Count: Integer): Integer; virtual; abstract;
    procedure WritingFailed; virtual; abstract;
    procedure WantWrite; virtual; abstract;
    procedure BufferEmpty; virtual;

    constructor Create;

  public
    EndOfLineMarker: String;

    destructor Destroy; override;
    procedure WriteLine(const line: String);
    procedure Run;		// Write as many data as possible

    property BytesInBuffer: Integer read FBytesInBuffer;
    property OnBufferEmpty: TNotifyEvent read FOnBufferEmpty write FOnBufferEmpty;
  end;


  TAsyncWriteStream = class(TWriteBuffer)
  protected
    FManager: TAsyncIOManager;
    FDataStream: TStream;
    FBlockingStream: THandleStream;

    function  DoRealWrite(const ABuffer; Count: Integer): Integer; override;
    procedure WritingFailed; override;
    procedure WantWrite; override;
    procedure BufferEmpty; override;
    procedure CanWrite(UserData: TObject);
  public
    constructor Create(AManager: TAsyncIOManager; AStream: THandleStream);
    constructor Create(AManager: TAsyncIOManager;
      ADataStream: TStream; ABlockingStream: THandleStream);
    destructor Destroy; override;

    property DataStream: TStream read FDataStream;
    property BlockingStream: THandleStream read FBlockingStream;
  end;



// ===================================================================
// ===================================================================

implementation

// -------------------------------------------------------------------
//   TAsyncIOManager
// -------------------------------------------------------------------

procedure TAsyncIOManager.ExecuteNotify(const Notify: TAsyncIONotifyInfo);
begin
  if Assigned(Notify.Method) then
    Notify.Method(Notify.UserData);
end;

// *** Begin Linux specific

procedure TAsyncIOManager.CalcHighestHandle(max: Integer);
var
  i: Integer;
begin
  HighestHandle := -1;
  for i := max downto 0 do
    if FD_IsSet(i, ReadMap) or FD_IsSet(i, WriteMap) then begin
      HighestHandle := i;
      break;
    end;
end;

function TAsyncIOManager.GetHandleAsync(AHandle: Integer): Boolean;
begin
  Result := (fcntl(AHandle, F_GetFl) and Open_NonBlock) <> 0;
end;

procedure TAsyncIOManager.SetHandleAsync(AHandle: Integer; AValue: Boolean);
var
  SavedBits: Integer;
begin
  SavedBits := fcntl(AHandle, F_GetFl) and not Open_NonBlock;
  if AValue then
    fcntl(AHandle, F_SetFl, SavedBits or Open_NonBlock)
  else
    fcntl(AHandle, F_SetFl, SavedBits);
end;

constructor TAsyncIOManager.Create;
begin
  inherited Create;
  FD_Zero(ReadMap);
  FD_Zero(WriteMap);
  HighestHandle := -1;
end;

procedure TAsyncIOManager.Run;
var
  ThisReadMap, ThisWriteMap: TFDSet;
  i, res: Integer;
begin
  DoBreak := False;
  while (not DoBreak) and ((HighestHandle >= 0) or (FTimeout > 0)) do begin
    ThisReadMap := ReadMap;
    ThisWriteMap := WriteMap;

    if FTimeout > 0 then
      res := Select(HighestHandle + 1, @ThisReadMap, @ThisWriteMap, nil, FTimeout)
    else
      res := Select(HighestHandle + 1, @ThisReadMap, @ThisWriteMap, nil, nil);
    if res < 0 then
      break;

    if res = 0 then
      ExecuteNotify(TimeoutNotify)
    else
      for i := 0 to HighestHandle do begin
        if FD_IsSet(i, ThisReadMap) and FD_IsSet(i, ReadMap)then
          ExecuteNotify(ReadNotifies[i]);
        if FD_IsSet(i, ThisWriteMap) and FD_IsSet(i, WriteMap) then
          ExecuteNotify(WriteNotifies[i]);
      end;
  end;
end;

procedure TAsyncIOManager.BreakRun;
begin
  DoBreak := True;
end;

procedure TAsyncIOManager.SetReadHandler(AHandle: Integer;
  AMethod: TAsyncIONotify; AUserData: TObject);
begin
  ASSERT((AHandle >= 0) and (AHandle <= MaxHandle) and Assigned(AMethod));
  if (AHandle < 0) or (AHandle > MaxHandle) then
    exit;

  if AHandle > HighestHandle then
    HighestHandle := AHandle;
  FD_Set(AHandle, ReadMap);
  ReadNotifies[AHandle].Method := AMethod;
  ReadNotifies[AHandle].UserData := AUserData;
end;

procedure TAsyncIOManager.ClearReadHandler(AHandle: Integer);
begin
  ASSERT((AHandle >= 0) and (AHandle <= MaxHandle));
  if (AHandle >= 0) and (AHandle <= MaxHandle) then begin
    FD_Clr(AHandle, ReadMap);
    if AHandle = HighestHandle then
      CalcHighestHandle(AHandle);
  end;
end;

procedure TAsyncIOManager.SetWriteHandler(AHandle: Integer;
  AMethod: TAsyncIONotify; AUserData: TObject);
begin
  ASSERT((AHandle >= 0) and (AHandle <= MaxHandle) and Assigned(AMethod));
  if (AHandle < 0) or (AHandle > MaxHandle) then
    exit;

  if AHandle > HighestHandle then
    HighestHandle := AHandle;
  FD_Set(AHandle, WriteMap);
  WriteNotifies[AHandle].Method := AMethod;
  WriteNotifies[AHandle].UserData := AUserData;
end;

procedure TAsyncIOManager.ClearWriteHandler(AHandle: Integer);
begin
  ASSERT((AHandle >= 0) and (AHandle <= MaxHandle));
  if (AHandle >= 0) and (AHandle <= MaxHandle) then begin
    FD_Clr(AHandle, WriteMap);
    if AHandle = HighestHandle then
      CalcHighestHandle(AHandle);
  end;
end;

// *** End Linux specific

procedure TAsyncIOManager.SetTimeoutHandler(AMethod: TAsyncIONotify; AUserData: TObject);
begin
  TimeoutNotify.Method := AMethod;
  TimeoutNotify.UserData := AUserData;
end;

procedure TAsyncIOManager.ClearTimeoutHandler;
begin
  TimeoutNotify.Method := nil;
end;


// -------------------------------------------------------------------
//   TGenericLineReader
// -------------------------------------------------------------------

destructor TGenericLineReader.Destroy;
begin
  if Assigned(RealBuffer) then begin
    FreeMem(RealBuffer);
    RealBuffer := nil;
  end;
  inherited Destroy;
end;

procedure TGenericLineReader.Run;
var
  NewData: array[0..1023] of Byte;
  p: PChar;
  BytesRead, OldBufSize, CurBytesInBuffer, LastEndOfLine, i, LineLength: Integer;
  line: String;
  FirstRun: Boolean;
begin
  FirstRun := True;
  while True do begin
    BytesRead := Read(NewData, SizeOf(NewData));
    //WriteLn('Linereader: ', BytesRead, ' bytes read');
    if BytesRead <= 0 then begin
      if FirstRun then
        NoData;
      break;
    end;
    FirstRun := False;
    OldBufSize := FBytesInBuffer;

    // Append the new received data to the read buffer
    Inc(FBytesInBuffer, BytesRead);
    ReallocMem(RealBuffer, FBytesInBuffer);
    Move(NewData, RealBuffer[OldBufSize], BytesRead);

    {Process all potential lines in the current buffer. Attention: FBuffer and
     FBytesInBuffer MUST be updated for each line, as they can be accessed from
     within the FOnLine handler!}
    LastEndOfLine := 0;
    if OldBufSize > 0 then
      i := OldBufSize - 1
    else
      i := 0;

    CurBytesInBuffer := FBytesInBuffer;

    while i <= CurBytesInBuffer - 2 do begin
      if (RealBuffer[i] = #13) or (RealBuffer[i] = #10) then begin
        LineLength := i - LastEndOfLine;
	SetLength(line, LineLength);
	if LineLength > 0 then
	  Move(RealBuffer[LastEndOfLine], line[1], LineLength);

	if ((RealBuffer[i] = #13) and (RealBuffer[i + 1] = #10)) or
	   ((RealBuffer[i] = #10) and (RealBuffer[i + 1] = #13)) then
	  Inc(i);
	LastEndOfLine := i + 1;

	if Assigned(FOnLine) then begin
	  FBuffer := RealBuffer + LastEndOfLine;
	  FBytesInBuffer := CurBytesInBuffer - LastEndOfLine;
	  FOnLine(line);
	  // Check if <this> has been destroyed by FOnLine:
	  if not Assigned(FBuffer) then exit;
	end;
      end;
      Inc(i);
    end;

    FBytesInBuffer := CurBytesInBuffer;

    if LastEndOfLine > 0 then begin
      // Remove all processed lines from the buffer
      Dec(FBytesInBuffer, LastEndOfLine);
      GetMem(p, FBytesInBuffer);
      Move(RealBuffer[LastEndOfLine], p^, FBytesInBuffer);
      FreeMem(RealBuffer);
      RealBuffer := p;
    end;
    FBuffer := RealBuffer;
  end;
end;


// -------------------------------------------------------------------
//   TAsyncStreamLineReader
// -------------------------------------------------------------------

function TAsyncStreamLineReader.Read(var ABuffer; count: Integer): Integer;
begin
  Result := FDataStream.Read(ABuffer, count);
end;

procedure TAsyncStreamLineReader.NoData;
var
  s: String;
begin
  if (FDataStream = FBlockingStream) or (FDataStream.Position = FDataStream.Size) then begin

    if (FBytesInBuffer > 0) and Assigned(FOnLine) then begin
      if FBuffer[FBytesInBuffer - 1] in [#13, #10] then
        Dec(FBytesInBuffer);
      SetLength(s, FBytesInBuffer);
      Move(FBuffer^, s[1], FBytesInBuffer);
      FOnLine(s);
    end;

    FManager.ClearReadHandler(FBlockingStream.Handle);
    if Assigned(FOnEOF) then
      FOnEOF(Self);
  end;
end;

procedure TAsyncStreamLineReader.StreamDataAvailable(UserData: TObject);
begin
  Run;
end;

constructor TAsyncStreamLineReader.Create(AManager: TAsyncIOManager; AStream: THandleStream);
begin
  Self.Create(AManager, AStream, AStream);
end;

constructor TAsyncStreamLineReader.Create(AManager: TAsyncIOManager;
  ADataStream: TStream; ABlockingStream: THandleStream);
begin
  ASSERT(Assigned(ADataStream) and Assigned(ABlockingStream));

  inherited Create;
  FManager := AManager;
  FDataStream := ADataStream;
  FBlockingStream := ABlockingStream;
  AManager.SetReadHandler(FBlockingStream.Handle, @StreamDataAvailable, nil);
  AManager.HandleAsync[FBlockingStream.Handle] := True;
end;

destructor TAsyncStreamLineReader.Destroy;
begin
  FManager.ClearReadHandler(FBlockingStream.Handle);
  inherited Destroy;
end;


// -------------------------------------------------------------------
//   TWriteBuffer
// -------------------------------------------------------------------

procedure TWriteBuffer.BufferEmpty;
begin
  if Assigned(FOnBufferEmpty) then
    FOnBufferEmpty(Self);
end;

constructor TWriteBuffer.Create;
begin
  inherited Create;

  FBuffer := nil;
  FBytesInBuffer := 0;
  EndOfLineMarker := #10;
end;

destructor TWriteBuffer.Destroy;
begin
  if Assigned(FBuffer) then
    FreeMem(FBuffer);
  inherited Destroy;
end;

function TWriteBuffer.Seek(Offset: LongInt; Origin: Word): LongInt;
begin
  if ((Offset = 0) and ((Origin = soFromCurrent) or (Origin = soFromEnd))) or
     ((Offset = FBytesInBuffer) and (Origin = soFromBeginning)) then
    Result := FBytesInBuffer
  else
    raise EStreamError.Create('Invalid stream operation');
end;

function TWriteBuffer.Write(const ABuffer; Count: LongInt): LongInt;
begin
  ReallocMem(FBuffer, FBytesInBuffer + Count);
  Move(ABuffer, FBuffer[FBytesInBuffer], Count);
  Inc(FBytesInBuffer, Count);
  WantWrite;
  Result := Count;
end;

procedure TWriteBuffer.WriteLine(const line: String);
var
  s: String;
begin
  s := line + EndOfLineMarker;
  WriteBuffer(s[1], Length(s));
end;

procedure TWriteBuffer.Run;
var
  CurStart, written: Integer;
  NewBuf: PChar;
  failed: Boolean;
begin
  CurStart := 0;
  failed := True;
  repeat
    if FBytesInBuffer = 0 then begin
      BufferEmpty;
      exit;
    end;

    written := DoRealWrite(FBuffer[CurStart], FBytesInBuffer - CurStart);
    if written > 0 then begin
      Inc(CurStart, written);
      failed := False;
      GetMem(NewBuf, FBytesInBuffer - CurStart);
      Move(FBuffer[CurStart], NewBuf[0], FBytesInBuffer - CurStart);
      FreeMem(FBuffer);
      FBuffer := NewBuf;
      Dec(FBytesInBuffer, CurStart);
    end;
  until written <= 0;

  if failed then
    WritingFailed;
end;


// -------------------------------------------------------------------
//   TAsyncWriteStream
// -------------------------------------------------------------------

function TAsyncWriteStream.DoRealWrite(const ABuffer; Count: Integer): Integer;
begin
  Result := FDataStream.Write(ABuffer, count);
end;

procedure TAsyncWriteStream.WritingFailed;
begin
  if FDataStream <> FBlockingStream then
    FManager.ClearWriteHandler(FBlockingStream.Handle);
end;

procedure TAsyncWriteStream.WantWrite;
begin
  FManager.SetWriteHandler(FBlockingStream.Handle, @CanWrite, nil);
end;

procedure TAsyncWriteStream.BufferEmpty;
begin
  FManager.ClearWriteHandler(FBlockingStream.Handle);
  inherited BufferEmpty;
end;

procedure TAsyncWriteStream.CanWrite(UserData: TObject);
begin
  Run;
end;

constructor TAsyncWriteStream.Create(AManager: TAsyncIOManager; AStream: THandleStream);
begin
  Self.Create(AManager, AStream, AStream);
end;

constructor TAsyncWriteStream.Create(AManager: TAsyncIOManager;
  ADataStream: TStream; ABlockingStream: THandleStream);
begin
  ASSERT(Assigned(ADataStream) and Assigned(ABlockingStream));

  inherited Create;
  FManager := AManager;
  FDataStream := ADataStream;
  FBlockingStream := ABlockingStream;
  AManager.HandleAsync[FBlockingStream.Handle] := True;
end;

destructor TAsyncWriteStream.Destroy;
begin
  FManager.ClearWriteHandler(FBlockingStream.Handle);
  inherited Destroy;
end;


end.


{
  $Log$
  Revision 1.1  2000/02/17 22:40:05  sg
  * First version. This unit should go into FCL soon...

}
