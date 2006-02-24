{ HTTP server component

  Copyright (C) 2006 Micha Nelissen

  This library is Free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is diStributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; withOut even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a Copy of the GNU Library General Public License
  along with This library; if not, Write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
  
  This license has been modified. See file LICENSE.ADDON for more information.
  Should you find these sources without a LICENSE File, please contact
  me at ales@chello.sk
}

unit lhttpserver;

{$mode objfpc}{$h+}

interface

uses
  classes, sysutils, strutils, lnet, lhttputil, lstrbuffer;

type
  TLHTTPRequest = (hrHead, hrGet, hrPost, hrUnknown);
  TLHTTPRequestParameter = (hpConnection, hpContentLength, hpContentType,
    hpAccept, hpAcceptCharset, hpAcceptEncoding, hpAcceptLanguage, hpHost,
    hpFrom, hpReferer, hpUserAgent, hpRange, hpTransferEncoding,
    hpIfModifiedSince, hpIfUnmodifiedSince, hpCookie);
  TLHTTPStatus = (hsOK, hsMovedPermanently, hsNotModified, hsBadRequest, 
    hsForbidden, hsNotFound, hsPreconditionFailed, hsRequestTooLong,
    hsInternalError, hsNotImplemented, hsNotAllowed);
  TLHTTPTransferEncoding = (teIdentity, teChunked);

const
  HTTPRequestStrings: array[TLHTTPRequest] of string =
    ('HEAD', 'GET', 'POST', '');
  HTTPRequestParameterStrings: array[TLHTTPRequestParameter] of string =
    ('CONNECTION', 'CONTENT-LENGTH', 'CONTENT-TYPE', 'ACCEPT', 
     'ACCEPT-CHARSET', 'ACCEPT-ENCODING', 'ACCEPT-LANGUAGE', 'HOST',
     'FROM', 'REFERER', 'USER-AGENT', 'RANGE', 'TRANSFER-ENCODING',
     'IF-MODIFIED-SINCE', 'IF-UNMODIFIED-SINCE', 'COOKIE');
  HTTPStatusCodes: array[TLHTTPStatus] of dword =
    (200, 301, 304, 400, 403, 404, 412, 414, 500, 501, 504);
  HTTPTexts: array[TLHTTPStatus] of string = 
    ('OK', 'Moved Permanently', 'Not Modified', 'Bad Request', 'Forbidden', 
     'Not Found', 'Precondition Failed', 'Request Too Long', 'Internal Error',
     'Method Not Implemented', 'Method Not Allowed');
  HTTPDescriptions: array[TLHTTPStatus] of string = (
      { hsOK }
    '',
      { hsMovedPermanently }
    '',
      { hsNotModified }
    '',
      { hsBadRequest }
    '<html><head><title>400 Bad Request</title></head><body>'+#10+
    '<h1>Bad Request</h1>'+#10+
    '<p>Your browser did a request this server did not understand.</p>'+#10+
    '</body></html>'+#10,
      { hsForbidden }
    '<html><head><title>403 Forbidden</title></head><body>'+#10+
    '<h1>Forbidden</h1>'+#10+
    '<p>You do not have permission to access this resource.</p>'+#10+
    '</body></html>'+#10,
      { hsNotFound }
    '<html><head><title>404 Not Found</title></head><body>'+#10+
    '<h1>Not Found</h1>'+#10+
    '<p>The requested URL was not found on this server.</p>'+#10+
    '</body></html>'+#10,
      { hsPreconditionFailed }
    '<html><head><title>412 Precondition Failed</title></head><body>'+#10+
    '<h1>Precondition Failed</h1>'+#10+
    '<p>The precondition on the request evaluated to false.</p>'+#10+
    '</body></html>'+#10,
      { hsRequestTooLong }
    '<html><head><title>414 Request Too Long</title></head><body>'+#10+
    '<h1>Bad Request</h1>'+#10+
    '<p>Your browser did a request that was too long for this server to parse.</p>'+#10+
    '</body></html>'+#10,
      { hsInternalError }
    '<html><head><title>500 Internal Error</title></head><body>'+#10+
    '<h1>Internal Error</h1>'+#10+
    '<p>An error occurred while generating the content for this request.</p>'+#10+
    '</body></html>'+#10,
      { hsNotImplemented }
    '<html><head><title>501 Method Not Implemented</title></head><body>'+#10+
    '<h1>Method Not Implemented</h1>'+#10+
    '<p>The method used in the request is invalid.</p>'+#10+
    '</body></html>'+#10,
      { hsNotAllowed }
    '<html><head><title>504 Method Not Allowed</title></head><body>'+#10+
    '<h1>Method Not Allowed</h1>'+#10+
    '<p>The method used in the request is not allowed on the resource specified in the URL.</p>'+#10+
    '</body></html>'+#10);


type
  TLHTTPSocket = class;
  TLHTTPServer = class;
  
  PRequestInfo = ^TRequestInfo;
  TRequestInfo = record
    { request }
    RequestType: TLHTTPRequest;
    DateTime: TDateTime;
    Method: pchar;
    Argument: pchar;
    Status: TLHTTPStatus;
    QueryParams: pchar;
    VersionStr: pchar;
    Version: dword;
    Parameters: array[TLHTTPRequestParameter] of pchar;

    { response }
    ContentType: string;
    ContentCharset: string;
    ContentLength: integer;
    TransferEncoding: TLHTTPTransferEncoding;
    LastModified: TDateTime;
    ExtraHeaders: string;
  end;

  TWriteBlockMethod = function: boolean of object;

  TOutputItem = class(TObject)
  protected
    FBuffer: pchar;
    FBufferPos: integer;
    FBufferSize: integer;
    FBufferOffset: integer;
    FOutputPending: boolean;
    FEof: boolean;
    FNext: TOutputItem;
    FSocket: TLHTTPSocket;
    FWriteBlock: TWriteBlockMethod;
  protected
    function WriteData: boolean; virtual;
  public
    constructor Create;

    function  HandleInput(ABuffer: pchar; ASize: dword): dword; virtual;
    procedure LogAccess(const AMessage: string);
    procedure LogError(const AMessage: string);
    function  WriteBlock: boolean; virtual;

    property Socket: TLHTTPSocket read FSocket write FSocket;
  end;

  TBufferOutput = class(TOutputItem)
  protected
    procedure PrepareChunk;
    procedure PrepareBuffer;
  public
    constructor Create;
    destructor Destroy; override;

    function WriteChunk: boolean;
    function WriteBuffer: boolean;
    function WritePlain: boolean;
    function WriteBlock: boolean; override;
  end;

  TMemoryOutput = class(TOutputItem)
  protected
    FFreeBuffer: boolean;
  public
    constructor Create(ABuffer: pointer; ABufferOffset, 
      ABufferSize: integer; AFreeBuffer: boolean);
    destructor Destroy; override;
  end;

  TChunkState = (csInitial, csData, csDataEnd, csTrailer, csFinished);
  
  TParseBufferMethod = function: boolean of object;

  TLHTTPSocket = class(TLSocket)
  protected
    FBuffer: pchar;
    FBufferPos: pchar;
    FBufferEnd: pchar;
    FBufferSize: dword;
    FRequestBuffer: pchar;
    FRequestPos: pchar;
    FPendingData: boolean;
    FRequestInputDone: boolean;
    FRequestHeaderDone: boolean;
    FInputRemaining: dword;
    FLogMessage: TStringBuffer;
    FChunkState: TChunkState;
    FCurrentInput: TOutputItem;
    FCurrentOutput: TOutputItem;
    FLastOutput: TOutputItem;
    FKeepAlive: boolean;
    FRequestInfo: TRequestInfo;
    FParseBuffer: TParseBufferMethod;

    procedure AddToOutput(AOutputItem: TOutputItem);
    procedure LogMessage;
    procedure FlushRequest;
    function  GetServer: TLHTTPServer;
    function  HandleURI: TOutputItem; virtual;
    procedure ResetDefaults;
    procedure ProcessRequest;
    procedure PackRequestBuffer;
    procedure PackInputBuffer;
    function  ParseRequest: boolean;
    function  ParseEntityPlain: boolean;
    function  ParseEntityChunked: boolean;
    procedure ParseLine(pLineEnd: pchar);
    procedure ParseRequestLine(pLineEnd: pchar);
    procedure ParseParameterLine(pLineEnd: pchar);
    procedure WriteError(AStatus: TLHTTPStatus);
    procedure WriteHeaders(AHeaderResponse, ADataResponse: TOutputItem);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure HandleReceive;
    procedure LogAccess(const AMessage: string);
    procedure ParseBuffer;
    procedure StartResponse(AOutputItem: TBufferOutput);
    procedure WriteBlock;

    property Server: TLHTTPServer read GetServer;
    property RequestInfo: TRequestInfo read FRequestInfo;
  end;
  
  { TLHTTPServer }
  
  TURIHandler = class(TObject)
  private
    FNext: TURIHandler;
  public
    function HandleURI(ASocket: TLHTTPSocket): TOutputItem; virtual; abstract;
  end;

  TLHTTPServer = class(TLTcp)
  protected
    FHandlerList: TURIHandler;
    FDelayFreeItems: TOutputItem;
  
    procedure FreeDelayFreeItems;
    function HandleURI(ASocket: TLHTTPSocket): TOutputItem;
  public
    constructor Create(AOwner: TComponent); override;

    procedure DelayFree(AOutputItem: TOutputItem);
    procedure HandleReceive(aSocket: TLSocket);
    procedure HandleSend(aSocket: TLSocket);
    procedure LogAccess(const AMessage: string); virtual;

    procedure RegisterHandler(AHandler: TURIHandler);
  end;

implementation

const
  PackBufferSize = 512;     { if less data than this, pack buffer }
  RequestBufferSize = 1024;
  DataBufferSize = 64*1024;

constructor TOutputItem.Create;
begin
  inherited;
end;

function TOutputItem.HandleInput(ABuffer: pchar; ASize: dword): dword;
begin
  { discard input }
  Result := ASize;
end;

procedure TOutputItem.LogAccess(const AMessage: string);
begin
  FSocket.LogAccess(AMessage);
end;

procedure TOutputItem.LogError(const AMessage: string);
begin
  FSocket.LogError(AMessage, 0);
end;

function TOutputItem.WriteData: boolean;
var
  lWritten: integer;
begin
  if FOutputPending then
  begin
    lWritten := FSocket.Send(FBuffer[FBufferPos], FBufferSize-FBufferPos);
    Inc(FBufferPos, lWritten);
    Result := FBufferPos = FBufferSize;
    FOutputPending := not Result;
  end else
    Result := FEof;
end;

function TOutputItem.WriteBlock: boolean;
begin
  Result := WriteData;
end;

const
  ReserveChunkBytes = 12;

constructor TBufferOutput.Create;
begin
  inherited;
  GetMem(FBuffer, DataBufferSize);
  FWriteBlock := @WritePlain;
  { prepare for "worst case" data needed }
  PrepareChunk;
end;

destructor TBufferOutput.Destroy;
begin
  inherited;
  FreeMem(FBuffer);
end;

procedure TBufferOutput.PrepareChunk;
begin
  { 12 bytes for starting space, 7 bytes to end: <CR><LF>0<CR><LF><CR><LF> }
  FBufferPos := ReserveChunkBytes;
  FBufferOffset := FBufferPos;
  FBufferSize := DataBufferSize-7;
end;

procedure TBufferOutput.PrepareBuffer;
  { also for "plain" encoding }
begin
  FBufferPos := 0;
  FBufferOffset := 0;
  FBufferSize := DataBufferSize;
end;

const
   HexDigits: array[0..15] of char = '0123456789ABCDEF';

function HexReverse(AValue: dword; ABuffer: pchar): integer;
begin
  Result := 0;
  repeat
    ABuffer^ := HexDigits[AValue and $F];
    AValue := AValue shr 4;
    Dec(ABuffer);
    Inc(Result);
  until AValue = 0;
end;

function TBufferOutput.WriteChunk: boolean;
var
  lOffset: integer;
begin
  if not FOutputPending and not FEof then
  begin
    FEof := WriteData;
    if FBufferPos - FBufferOffset > 0 then
    begin
      FOutputPending := true;
      lOffset := HexReverse(FBufferPos-FBufferOffset, FBuffer+FBufferOffset-3);
      FBuffer[FBufferOffset-2] := #13;
      FBuffer[FBufferOffset-1] := #10;
      FBuffer[FBufferPos] := #13;
      FBuffer[FBufferPos+1] := #10;
      FBufferSize := FBufferPos+2;
      FBufferPos := FBufferOffset-lOffset-2;
    end else begin
      FBufferSize := 0;
      FBufferPos := 0;
    end;
    if FEof then
    begin
      FOutputPending := true;
      FBuffer[FBufferSize] := '0';
      FBuffer[FBufferSize+1] := #13;
      FBuffer[FBufferSize+2] := #10;
      { no trailer }
      FBuffer[FBufferSize+3] := #13;
      FBuffer[FBufferSize+4] := #10;
      inc(FBufferSize, 5);
    end;
  end;
  Result := inherited WriteData;
  if Result then
  begin
    Result := FEof;
    if not Result then
      PrepareChunk;
  end;
end;
  
function TBufferOutput.WriteBuffer: boolean;
begin
  if not FOutputPending then
  begin
    FEof := WriteData;
    FOutputPending := FEof;
    if FEof or (FBufferPos = FBufferSize) then
    begin
      if FBufferPos > FBufferOffset then
      begin
        FSocket.AddToOutput(TMemoryOutput.Create(FBuffer, FBufferOffset,
          FBufferPos, true));
        Inc(FSocket.RequestInfo.ContentLength, FBufferPos-FBufferOffset);
      end;
      if not FEof then
      begin
        FBuffer := GetMem(DataBufferSize);
        PrepareBuffer;
      end else begin
        FBuffer := nil;
        FBufferPos := 0;
        FBufferSize := 0;
        FSocket.WriteHeaders(Self, nil);
      end;
    end;
  end;
  Result := FEof;
  if Result then
    Result := inherited WriteData;
end;

function TBufferOutput.WritePlain: boolean;
begin
  if not FOutputPending then
  begin
    FEof := WriteData;
    if FBufferPos > FBufferOffset then
    begin
      FOutputPending := true;
      FBufferSize := FBufferPos;
      FBufferPos := FBufferOffset;
    end else begin
      FBufferSize := 0;
      FBufferPos := 0;
    end;
  end;
  Result := inherited WriteData;
  if Result then
  begin
    Result := FEof;
    if not Result then
      PrepareBuffer;
  end;
end;

function TBufferOutput.WriteBlock: boolean;
begin
  Result := FWriteBlock();
end;

constructor TMemoryOutput.Create(ABuffer: pointer; ABufferOffset, 
  ABufferSize: integer; AFreeBuffer: boolean);
begin
  inherited Create;

  FBuffer := ABuffer;
  FBufferPos := ABufferOffset;
  FBufferSize := ABufferSize;
  FFreeBuffer := AFreeBuffer;
  FOutputPending := true;
end;

destructor TMemoryOutput.Destroy;
begin
  inherited;
  if FFreeBuffer then
    FreeMem(FBuffer);
end;

{ TLHTTPSocket }

constructor TLHTTPSocket.Create;
begin
  inherited;

  FRequestInfo.RequestType := hrUnknown;
  FLogMessage := InitStringBuffer(256);
  FBuffer := GetMem(RequestBufferSize);
  FBufferSize := RequestBufferSize;
  FBufferPos := FBuffer;
  FBufferEnd := FBufferPos;
  FBuffer[0] := #0;
  FKeepAlive := true;
  ResetDefaults;
end;

destructor TLHTTPSocket.Destroy;
var
  lOutput: TOutputItem;
begin
  while FCurrentOutput <> nil do
  begin
    lOutput := FCurrentOutput;
    FCurrentOutput := FCurrentOutput.FNext;
    if lOutput = FCurrentInput then
      FCurrentInput := nil;
    lOutput.Free;
  end;
  if FCurrentInput <> nil then
    FreeAndNil(FCurrentInput);
  
  inherited;
  FreeMem(FBuffer);
  FreeMem(FLogMessage.Memory);
end;

function TLHTTPSocket.GetServer: TLHTTPServer;
begin
  Result := TLHTTPServer(FParent);
end;

procedure TLHTTPSocket.ResetDefaults;
begin
  FRequestInfo.ContentType := 'application/octet-stream';
  FParseBuffer := @ParseRequest;
end;

procedure TLHTTPSocket.FlushRequest;
begin
  with FRequestInfo do
  begin
    { request }
    Status := hsOK;
    RequestType := hrUnknown;
    Argument := nil;
    QueryParams := nil;
    Version := 0;
    FillDword(Parameters, sizeof(Parameters) div 4, 0);
    { response }
    ContentLength := 0;
    ContentCharset := '';
    LastModified := 0.0;
    TransferEncoding := teIdentity;
    ExtraHeaders := '';
  end;
  ResetDefaults;
end;

procedure TLHTTPSocket.LogAccess(const AMessage: string);
begin
  Server.LogAccess(AMessage);
end;

procedure TLHTTPSocket.LogMessage;
begin
  { log a message about this request, 
    '<StatusCode> <Length> "<Referer>" "<User-Agent>"' }
  AppendString(FLogMessage, IntToStr(HTTPStatusCodes[FRequestInfo.Status]));
  AppendChar(FLogMessage, ' ');
  AppendString(FLogMessage, IntToStr(FRequestInfo.ContentLength));
  AppendString(FLogMessage, ' "');
  AppendString(FLogMessage, FRequestInfo.Parameters[hpReferer]);
  AppendString(FLogMessage, '" "');
  AppendString(FLogMessage, FRequestInfo.Parameters[hpUserAgent]);
  AppendChar(FLogMessage, '"');
  AppendChar(FLogMessage, #0);
  LogAccess(FLogMessage.Memory);
end;

procedure TLHTTPSocket.HandleReceive;
var
  lRead: integer;
begin
  if FRequestInputDone then 
  begin
    FPendingData := true;
    exit;
  end;
  
  if ptruint(FBufferEnd-FBuffer) = FBufferSize then
  begin
    WriteError(hsRequestTooLong);
    exit;
  end;

  FPendingData := false;
  lRead := Get(FBufferEnd^, FBufferSize-PtrUInt(FBufferEnd-FBuffer)-1);
  if lRead = 0 then exit;
  Inc(FBufferEnd, lRead);
  FBufferEnd^ := #0;
  FParseBuffer();

  if FIgnoreWrite then
    WriteBlock;
end;

procedure TLHTTPSocket.PackRequestBuffer;
var
  lBytesLeft: dword;
  lFreeBuffer: pchar;
begin
  if (FRequestBuffer <> nil) and (FBufferEnd-FBufferPos <= RequestBufferSize) then
  begin
    { switch back to normal size buffer }
    lFreeBuffer := FBuffer;
    FBuffer := FRequestBuffer;
    FBufferSize := RequestBufferSize;
    FRequestBuffer := nil;
  end else
    lFreeBuffer := nil;
  if FRequestPos <> nil then
  begin
    lBytesLeft := FBufferEnd-FRequestPos;
    FBufferEnd := FBuffer+lBytesLeft;
    FBufferPos := FBuffer;
    if lBytesLeft > 0 then
    begin
      Move(FRequestPos^, FBuffer^, lBytesLeft);
      { restart parsing of request }
      FlushRequest;
    end;
    FRequestPos := nil;
  end;
  if lFreeBuffer <> nil then
    FreeMem(lFreeBuffer);
end;

procedure TLHTTPSocket.PackInputBuffer;
var
  lBytesLeft: dword;
begin
  { use bigger buffer for more speed }
  if FRequestBuffer = nil then
  begin
    FRequestBuffer := FBuffer;
    FBuffer := GetMem(DataBufferSize);
    FBufferSize := DataBufferSize;
    FRequestPos := nil;
  end;
  lBytesLeft := FBufferEnd-FBufferPos;
  Move(FBufferPos^, FBuffer^, lBytesLeft);
  FBufferEnd := FBuffer+lBytesLeft;
  FBufferPos := FBuffer;
end;

function TLHTTPSocket.ParseEntityPlain: boolean;
var
  lNumBytes: dword;
begin
  lNumBytes := FBufferEnd - FBufferPos;
  if lNumBytes > FInputRemaining then
    lNumBytes := FInputRemaining;
  { if no output item to feed into, discard }
  if FCurrentInput <> nil then
    lNumBytes := FCurrentInput.HandleInput(FBufferPos, lNumBytes);
  inc(FBufferPos, lNumBytes);
  dec(FInputRemaining, lNumBytes);
  Result := FInputRemaining > 0;
  { prepare for more data, if more data coming }
  if Result and ((FBufferEnd-FBufferPos) < PackBufferSize) then
    PackInputBuffer;
end;

function TLHTTPSocket.ParseEntityChunked: boolean;
var
  lLineEnd, lNextLine: pchar;
  lCode: integer;
begin
  repeat
    if FChunkState = csFinished then
      exit(false);
    if FChunkState = csData then
      if ParseEntityPlain then 
        exit(true)
      else
        FChunkState := csDataEnd;
    
    lLineEnd := StrScan(FBufferPos, #10);
    if lLineEnd = nil then
      exit(true);
    
    lNextLine := lLineEnd+1;
    if (lLineEnd > FBufferPos) and ((lLineEnd-1)^ = #13) then
      dec(lLineEnd);
    case FChunkState of 
      csInitial:
      begin
        lLineEnd^ := #0;
        Val(FBufferPos^, FInputRemaining, lCode);
        if lCode <> 0 then
        begin
          FChunkState := csFinished;
          Disconnect;
          exit(false);
        end;
        if FInputRemaining = 0 then
          FChunkState := csTrailer
        else
          FChunkState := csData;
      end;
      csDataEnd:
      begin
        { skip empty line }
        FChunkState := csInitial;
      end;
      csTrailer:
      begin
        { trailer is optional, empty line indicates end }
        if lLineEnd = FBufferPos then
          FChunkState := csFinished
        else
          ParseParameterLine(lLineEnd);
      end;
    end;
    FBufferPos := lNextLine;
  until false;
end;

function TLHTTPSocket.ParseRequest: boolean;
var
  pNextLine, pLineEnd: pchar;
begin
  if FRequestHeaderDone then exit(false);
  repeat
    pLineEnd := StrScan(FBufferPos, #10);
    if pLineEnd = nil then
    begin
      PackRequestBuffer;
      exit(true);
    end;
  
    pNextLine := pLineEnd+1;
    if (pLineEnd > FBufferPos) and ((pLineEnd-1)^ = #13) then
      dec(pLineEnd);
    pLineEnd^ := #0;
    ParseLine(pLineEnd);
    FBufferPos := pNextLine;
    if FRequestHeaderDone then
      exit(false);
  until false;
end;

function TrySingleDigit(ADigit: char; out OutDigit: byte): boolean;
begin
  Result := (ord(ADigit) >= ord('0')) and (ord(ADigit) <= ord('9'));
  if not Result then exit;
  OutDigit := ord(ADigit) - ord('0');
end;

procedure TLHTTPSocket.ParseRequestLine(pLineEnd: pchar);
var
  lPos: pchar;
  lMajorVersion, lMinorVersion: byte;
  I: TLHTTPRequest;
begin
  { make a timestamp for this request }
  FRequestInfo.DateTime := LocalTimeToGMT(Now);
  { begin log message }
  FLogMessage.Pos := FLogMessage.Memory;
  AppendString(FLogMessage, PeerAddress);
  AppendString(FLogMessage, ' - [');
  AppendString(FLogMessage, FormatDateTime('dd/mmm/yyyy:hh:nn:ss', FRequestInfo.DateTime));
  AppendString(FLogMessage, ' GMT] "');
  AppendString(FLogMessage, FBufferPos, pLineEnd-FBufferPos);
  AppendString(FLogMessage, '" ');

  { decode version }
  lPos := pLineEnd;
  repeat
    if lPos^ = ' ' then break;
    dec(lPos);
    if lPos < FBufferPos then
    begin
      WriteError(hsBadRequest);
      exit;
    end;
  until false;

  lPos^ := #0;
  inc(lPos);
  { lPos = version string }
  if ((pLineEnd-lPos) <> 8) 
    or not CompareMem(lPos, pchar('HTTP/'), 5)
    or not TrySingleDigit(lPos[5], lMajorVersion) 
    or (lPos[6] <> '.')
    or not TrySingleDigit(lPos[7], lMinorVersion) then
  begin
    WriteError(hsBadRequest);
    exit;
  end;
  FRequestInfo.VersionStr := lPos;
  FRequestInfo.Version := lMajorVersion * 10 + lMinorVersion;
  
  { trim spaces at end of URI }
  dec(lPos);
  repeat
    if lPos = FBufferPos then break;
    dec(lPos);
    if lPos^ <> ' ' then break;
    lPos^ := #0;
  until false;

  { decode method }
  FRequestInfo.Method := FBufferPos;
  lPos := StrScan(FBufferPos, ' ');
  if lPos = nil then
  begin
    WriteError(hsBadRequest);
    exit;
  end;

  lPos^ := #0;
  for I := Low(TLHTTPRequest) to High(TLHTTPRequest) do
  begin
    if I = hrUnknown then
    begin
      WriteError(hsNotImplemented);
      exit;
    end;
    
    if ((lPos-FBufferPos) = Length(HTTPRequestStrings[I]))
    and CompareMem(FBufferPos, PChar(HTTPRequestStrings[I]), lPos-FBufferPos) then
    begin
      repeat
        inc(lPos);
      until lPos^ <> ' ';
      FRequestInfo.Argument := lPos;
      FRequestInfo.RequestType := I;
      break;
    end;
  end;

  if ((pLineEnd-FRequestInfo.Argument) > 7) and (StrIComp(FRequestInfo.Argument, 'http://') = 0) then
  begin
    { absolute URI }
    lPos := FRequestInfo.Argument+7;
    while (lPos^ = '/') do 
      Inc(lPos);
    FRequestInfo.Parameters[hpHost] := lPos;
    lPos := StrScan(lPos, '/');
    FRequestInfo.Argument := lPos;
  end;
  { FRequestInfo.Argument now points to an "abs_path" }
  if FRequestInfo.Argument[0] <> '/' then
  begin
    WriteError(hsBadRequest);
    exit;
  end;
  repeat
    Inc(FRequestInfo.Argument);
  until FRequestInfo.Argument[0] <> '/';
end;

procedure TLHTTPSocket.ParseParameterLine(pLineEnd: pchar);
var
  lPos: pchar;
  I: TLHTTPRequestParameter;
  lLen: integer;
begin
  lPos := StrScan(FBufferPos, ' ');
  if (lPos = nil) or (lPos = FBufferPos) or ((lPos-1)^ <> ':') then
  begin
    WriteError(hsBadRequest);
    exit;
  end;

  { null-terminate at colon }
  (lPos-1)^ := #0;
  StrUpper(FBufferPos);
  lLen := lPos-FBufferPos-1;
  for I := Low(TLHTTPRequestParameter) to High(TLHTTPRequestParameter) do
    if (Length(HTTPRequestParameterStrings[I]) = lLen)
    and CompareMem(FBufferPos, PChar(HTTPRequestParameterStrings[I]), lLen) then
    begin
      repeat
        inc(lPos);
      until lPos^ <> ' ';
      FRequestInfo.Parameters[I] := lPos;
      break;
    end;
end;

procedure TLHTTPSocket.ParseLine(pLineEnd: pchar);
begin
  if FRequestInfo.RequestType = hrUnknown then
  begin
    ParseRequestLine(pLineEnd);
    exit;
  end;

  if FBufferPos[0] = #0 then
  begin
    ProcessRequest;
    FRequestHeaderDone := true;
  end else
    ParseParameterLine(pLineEnd);
end;
        
function HexToNum(AChar: char): byte;
begin
  if ('0' <= AChar) and (AChar <= '9') then
    Result := ord(AChar) - ord('0')
  else if ('A' <= AChar) and (AChar <= 'F') then
    Result := ord(AChar) - (ord('A') - 10)
  else if ('a' <= AChar) and (AChar <= 'f') then
    Result := ord(AChar) - (ord('a') - 10)
  else
    Result := 0;
end;

procedure DecodeUrl(AStr: pchar);
var
  lPos, lNext, lDest, lEnd: pchar;
begin
  lDest := AStr;
  lEnd := AStr + StrLen(AStr);
  repeat
    lPos := StrScan(AStr, '%');
    if (lPos <> nil) and (lPos[1] <> #0) and (lPos[2] <> #0) then
    begin
      lPos^ := char(HexToNum(lPos[1])*16 + HexToNum(lPos[2]));
      Inc(lPos);
      lNext := lPos+2;
    end else begin
      lPos := lEnd;
      lNext := nil;
    end;
    if lDest <> AStr then
      Move(AStr^, lDest^, lPos-AStr);
    Inc(lDest, lPos-AStr);
    AStr := lNext;
  until lNext = nil;
end;

function CheckPermission(const ADocument: pchar): boolean;
var
  lPos: pchar;
begin
  lPos := ADocument;
  repeat
    lPos := StrScan(lPos, '/');
    if lPos = nil then exit(true);
    if (lPos[1] = '.') and (lPos[2] = '.') and ((lPos[3] = '/') or (lPos[3] = #0)) then
      exit(false);
    inc(lPos);
  until false;
end;

procedure TLHTTPSocket.ProcessRequest;
var
  lPos: pchar;
  lCode: integer;
begin
  { do HTTP/1.1 Host-field present check }
  if (FRequestInfo.Version > 10) and (FRequestInfo.Parameters[hpHost] = nil) then
  begin
    WriteError(hsBadRequest);
    exit;
  end;
      
  lPos := StrScan(FRequestInfo.Argument, '?');
  if lPos <> nil then
  begin
    lPos^ := #0;
    FRequestInfo.QueryParams := lPos+1;
  end;

  FKeepAlive := FRequestInfo.Version > 10;
  if FRequestInfo.Parameters[hpConnection] <> nil then
  begin
    if StrIComp(FRequestInfo.Parameters[hpConnection], 'keep-alive') = 0 then
      FKeepAlive := true
    else
    if StrIComp(FRequestInfo.Parameters[hpConnection], 'close') = 0 then
      FKeepAlive := false;
  end;
  
  DecodeUrl(FRequestInfo.Argument);
  if not CheckPermission(FRequestInfo.Argument) then
  begin
    WriteError(hsForbidden);
  end else begin
    if FRequestInfo.Parameters[hpContentLength] <> nil then
    begin
      FParseBuffer := @ParseEntityPlain;
      Val(FRequestInfo.Parameters[hpContentLength], FInputRemaining, lCode);
      if lCode <> 0 then
      begin
        WriteError(hsBadRequest);
        exit;
      end;
    end else 
    if FRequestInfo.Parameters[hpTransferEncoding] <> nil then
    begin
      if (StrIComp(FRequestInfo.Parameters[hpTransferEncoding], 'chunked') = 0) then
      begin
        FParseBuffer := @ParseEntityChunked;
        FChunkState := csInitial;
      end else begin
        WriteError(hsNotImplemented);
        exit;
      end;
    end else begin
      FRequestInputDone := true;
    end;
   
    FCurrentInput := HandleURI;
    { if we have a valid outputitem, wait until it is ready 
      to produce its response }
    if FCurrentInput = nil then
    begin
      if FRequestInfo.Status = hsOK then
        WriteError(hsNotFound)
      else
        WriteError(FRequestInfo.Status);
    end;
  end;
end;

procedure TLHTTPSocket.StartResponse(AOutputItem: TBufferOutput);
var
  lDateTime: TDateTime;
begin
  { check modification date }
  if FRequestInfo.Status = hsOK then
  begin
    if (FRequestInfo.Parameters[hpIfModifiedSince] <> nil) 
      and (FRequestInfo.LastModified <> 0.0) then
    begin
      if TryHTTPDateStrToDateTime(FRequestInfo.Parameters[hpIfModifiedSince], lDateTime) then
      begin
        if lDateTime > FRequestInfo.DateTime then
          FRequestInfo.Status := hsBadRequest
        else
        if FRequestInfo.LastModified <= lDateTime then
          FRequestInfo.Status := hsNotModified;
      end;
    end else
    if (FRequestInfo.Parameters[hpIfUnmodifiedSince] <> nil) then
    begin
      if TryHTTPDateStrToDateTime(FRequestInfo.Parameters[hpIfUnmodifiedSince], lDateTime) then
      begin
        if (FRequestInfo.LastModified = 0.0) 
          or (lDateTime < FRequestInfo.LastModified) then
          FRequestInfo.Status := hsPreconditionFailed;
      end;
    end;
  end;

  if FRequestInfo.Status = hsOK then
  begin
    if FRequestInfo.ContentLength = 0 then
    begin
      if FRequestInfo.Version >= 11 then
      begin
        { we can use chunked encoding }
        FRequestInfo.TransferEncoding := teChunked;
        AOutputItem.FWriteBlock := @AOutputItem.WriteChunk;
      end else begin
        { we need to buffer the response to find its length }
        FRequestInfo.TransferEncoding := teIdentity;
        AOutputItem.FWriteBlock := @AOutputItem.WriteBuffer;
        { need to accumulate data before starting header output }
        AddToOutput(AOutputItem);
        exit;
      end;
    end else begin
      FRequestInfo.TransferEncoding := teIdentity;
      AOutputItem.FWriteBlock := @AOutputItem.WritePlain;
    end;
    WriteHeaders(nil, AOutputItem);
  end else begin
    WriteError(FRequestInfo.Status);
    Server.DelayFree(AOutputItem);
    FCurrentInput := nil;
  end;
end;

function TLHTTPSocket.HandleURI: TOutputItem;
begin
  Result := Server.HandleURI(Self);
end;

procedure TLHTTPSocket.ParseBuffer; inline;
begin
  FParseBuffer();
end;

procedure TLHTTPSocket.WriteBlock;
var
  lFreeOutput: TOutputItem;
begin
  if FCurrentOutput <> nil then
  begin
    if FCurrentOutput.WriteBlock then
    begin
      lFreeOutput := FCurrentOutput;
      FCurrentOutput := FCurrentOutput.FNext;
      if lFreeOutput = FLastOutput then
        FLastOutput := nil;
      if lFreeOutput = FCurrentInput then
        FCurrentInput := nil;
      lFreeOutput.Free;
    end;
    { nothing left to write, request was busy and now completed }
    if FCurrentOutput = nil then
    begin
      LogMessage;
      if not FKeepAlive then
      begin
        Disconnect;
      end else begin
        FRequestInputDone := false;
        FRequestHeaderDone := false;
        FRequestPos := FBufferPos;
        FlushRequest;
        { rewind buffer pointers if at end of buffer anyway }
        if FBufferPos = FBufferEnd then
          PackRequestBuffer;
        if FParseBuffer() and FPendingData then 
        begin
          { end of input buffer reached, try reading more }
          HandleReceive;
        end;
      end;
    end;
  end;
end;

procedure TLHTTPSocket.WriteError(AStatus: TLHTTPStatus);
var
  lMessage: string;
begin
  FKeepAlive := false;
  lMessage := HTTPDescriptions[AStatus];
  FRequestInfo.ContentType := 'text/html';
  FRequestInfo.Status := AStatus;
  FRequestInfo.ContentLength := Length(lMessage);
  FRequestInfo.TransferEncoding := teIdentity;
  WriteHeaders(nil, TMemoryOutput.Create(PChar(lMessage), 0, Length(lMessage), false));
end;

procedure TLHTTPSocket.WriteHeaders(AHeaderResponse, ADataResponse: TOutputItem);
var
  lTemp: shortstring;
  lMessage: TStringBuffer;
begin
  lMessage := InitStringBuffer(504);
  
  AppendString(lMessage, 'HTTP/1.1 ');
  Str(HTTPStatusCodes[FRequestInfo.Status], lTemp);
  AppendString(lMessage, lTemp);
  AppendChar(lMessage, ' ');
  AppendString(lMessage, HTTPTexts[FRequestInfo.Status]);
  AppendString(lMessage, #13#10+'Date: ');
  AppendString(lMessage, FormatDateTime(HTTPDateFormat, FRequestInfo.DateTime));
  AppendString(lMessage, ' GMT'+#13#10);
  if FRequestInfo.ContentLength <> 0 then
  begin
    AppendString(lMessage, 'Content-Length: ');
    Str(FRequestInfo.ContentLength, lTemp);
    AppendString(lMessage, lTemp);
    AppendString(lMessage, #13#10);
  end;
  if FRequestInfo.TransferEncoding <> teIdentity then
  begin
    { only other possibility: teChunked }
    AppendString(lMessage, 'Transfer-Encoding: chunked'+#13#10);
  end;
  if FRequestInfo.LastModified <> 0.0 then
  begin
    AppendString(lMessage, 'Last-Modified: ');
    AppendString(lMessage, FormatDateTime(HTTPDateFormat, FRequestInfo.LastModified));
    AppendString(lMessage, ' GMT'+#13#10);
  end;
  AppendString(lMessage, 'Connection: ');
  if FKeepAlive then
    AppendString(lMessage, 'keep-alive')
  else
    AppendString(lMessage, 'close');
  AppendString(lMessage, #13#10+'Content-Type: ');
  AppendString(lMessage, FRequestInfo.ContentType);
  if Length(FRequestInfo.ContentCharset) > 0 then
  begin
    AppendString(lMessage, '; charset=');
    AppendString(lMessage, FRequestInfo.ContentCharset);
  end;
  AppendString(lMessage, #13#10);
  AppendString(lMessage, FRequestInfo.ExtraHeaders);
  AppendString(lMessage, #13#10);
  if AHeaderResponse <> nil then
  begin
    AHeaderResponse.FBuffer := lMessage.Memory;
    AHeaderResponse.FBufferSize := lMessage.Pos-lMessage.Memory;
  end else
    AddToOutput(TMemoryOutput.Create(lMessage.Memory, 0,
      lMessage.Pos-lMessage.Memory, true));

  if ADataResponse <> nil then
  begin
    if FRequestInfo.RequestType = hrHead then
      Server.DelayFree(ADataResponse)
    else
      AddToOutput(ADataResponse);
  end;
end;

procedure TLHTTPSocket.AddToOutput(AOutputItem: TOutputItem);
begin
  if FLastOutput <> nil then
  begin
    FLastOutput.FNext := AOutputItem;
  end else begin
    FCurrentOutput := AOutputItem;
  end;
  FLastOutput := AOutputItem;
  AOutputItem.Socket := Self;
end;

{ TLHTTPServer }

constructor TLHTTPServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SocketClass := TLHTTPSocket;
  OnCanSend := @HandleSend;
  OnReceive := @HandleReceive;
end;

procedure TLHTTPServer.FreeDelayFreeItems;
var
  lItem: TOutputItem;
begin
  while FDelayFreeItems <> nil do
  begin
    lItem := FDelayFreeItems;
    FDelayFreeItems := FDelayFreeItems.FNext;
    lItem.Free;
  end;
end;

procedure TLHTTPServer.DelayFree(AOutputItem: TOutputItem);
begin
  AOutputItem.FNext := FDelayFreeItems;
  FDelayFreeItems := AOutputItem;
end;

procedure TLHTTPServer.HandleReceive(aSocket: TLSocket);
begin
  TLHTTPSocket(aSocket).HandleReceive;
  FreeDelayFreeItems;
end;

procedure TLHTTPServer.HandleSend(aSocket: TLSocket);
begin
  TLHTTPSocket(aSocket).WriteBlock;
  FreeDelayFreeItems;
end;

function TLHTTPServer.HandleURI(ASocket: TLHTTPSocket): TOutputItem;
var
  lHandler: TURIHandler;
begin
  Result := nil;
  lHandler := FHandlerList;
  while lHandler <> nil do
  begin
    Result := lHandler.HandleURI(ASocket);
    if ASocket.RequestInfo.Status <> hsOK then break;
    if Result <> nil then break;
    lHandler := lHandler.FNext;
  end;
end;

procedure TLHTTPServer.LogAccess(const AMessage: string);
begin
end;

procedure TLHTTPServer.RegisterHandler(AHandler: TURIHandler);
begin
  if AHandler = nil then exit;
  AHandler.FNext := FHandlerList;
  FHandlerList := AHandler;
end;

end.

