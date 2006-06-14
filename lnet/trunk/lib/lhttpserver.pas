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
  TLHTTPMethod = (hmHead, hmGet, hmPost, hmUnknown);
  TLHTTPParameter = (hpConnection, hpContentLength, hpContentType,
    hpAccept, hpAcceptCharset, hpAcceptEncoding, hpAcceptLanguage, hpHost,
    hpFrom, hpReferer, hpUserAgent, hpRange, hpTransferEncoding,
    hpIfModifiedSince, hpIfUnmodifiedSince, hpCookie);
  TLHTTPStatus = (hsOK, hsMovedPermanently, hsNotModified, hsBadRequest, 
    hsForbidden, hsNotFound, hsPreconditionFailed, hsRequestTooLong,
    hsInternalError, hsNotImplemented, hsNotAllowed);
  TLHTTPTransferEncoding = (teIdentity, teChunked);

const
  HTTPMethodStrings: array[TLHTTPMethod] of string =
    ('HEAD', 'GET', 'POST', '');
  HTTPParameterStrings: array[TLHTTPParameter] of string =
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
  TLHTTPConnection = class;
  
  PRequestInfo = ^TRequestInfo;
  TRequestInfo = record
    { request }
    RequestType: TLHTTPMethod;
    DateTime: TDateTime;
    Method: pchar;
    Argument: pchar;
    Status: TLHTTPStatus;
    QueryParams: pchar;
    VersionStr: pchar;
    Version: dword;

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
    FPrevDelayFree: TOutputItem;
    FNextDelayFree: TOutputItem;
    FSocket: TLHTTPSocket;
    FWriteBlock: TWriteBlockMethod;

    procedure DoneInput; virtual;
    function WriteData: boolean; virtual;
  public
    constructor Create(ASocket: TLHTTPSocket);
    destructor Destroy; override;

    function  HandleInput(ABuffer: pchar; ASize: dword): dword; virtual;
    procedure LogAccess(const AMessage: string);
    procedure LogError(const AMessage: string);
    function  WriteBlock: boolean; virtual;

    property Socket: TLHTTPSocket read FSocket;
  end;

  TBufferOutput = class(TOutputItem)
  protected
    procedure PrepareChunk;
    procedure PrepareBuffer;
  public
    constructor Create(ASocket: TLHTTPSocket);
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
    constructor Create(ASocket: TLHTTPSocket; ABuffer: pointer; 
      ABufferOffset, ABufferSize: integer; AFreeBuffer: boolean);
    destructor Destroy; override;
  end;

  TChunkState = (csInitial, csData, csDataEnd, csTrailer, csFinished);
  TLHTTPParameterArray = array[TLHTTPParameter] of pchar;
  
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
    FOutputDone: boolean;
    FInputRemaining: dword;
    FChunkState: TChunkState;
    FCurrentInput: TOutputItem;
    FCurrentOutput: TOutputItem;
    FLastOutput: TOutputItem;
    FKeepAlive: boolean;
    FConnection: TLHTTPConnection;
    FParseBuffer: TParseBufferMethod;
    FParameters: TLHTTPParameterArray;
   
    procedure AddContentLength(ALength: integer); virtual;
    procedure DoneBuffer(AOutput: TBufferOutput); virtual;
    procedure LogMessage; virtual;
    procedure FlushRequest; virtual;
    procedure PackRequestBuffer;
    procedure PackInputBuffer;
    function  ParseRequest: boolean;
    function  ParseEntityPlain: boolean;
    function  ParseEntityChunked: boolean;
    procedure ParseLine(pLineEnd: pchar); virtual;
    procedure ParseParameterLine(pLineEnd: pchar);
    procedure ProcessHeaders; virtual; abstract;
    procedure ResetDefaults; virtual;
    procedure WriteError(AStatus: TLHTTPStatus); virtual;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure AddToOutput(AOutputItem: TOutputItem);
    procedure HandleReceive;
    procedure LogAccess(const AMessage: string); virtual;
    function  ParseBuffer: boolean;
    procedure WriteBlock;
    
    property Connection: TLHTTPConnection read FConnection;
    property Parameters: TLHTTPParameterArray read FParameters;
  end;

  { TLHTTPClientSocket }

  TLHTTPClientSocket = class(TLHTTPSocket)
  protected
    FMethod: TLHTTPMethod;
    
    procedure ParseLine(pLineEnd: pchar); override;
    procedure ProcessHeaders; override;
    procedure ResetDefaults; override;
  public
    constructor Create; override;

    property Method: TLHTTPMethod read FMethod write FMethod;
    
  end;

  TLHTTPServerSocket = class(TLHTTPSocket)
  protected
    FLogMessage: TStringBuffer;
    FRequestInfo: TRequestInfo;

    procedure AddContentLength(ALength: integer); override;
    procedure DoneBuffer(AOutput: TBufferOutput); override;
    procedure FlushRequest; override;
    function  HandleURI: TOutputItem; virtual;
    procedure LogMessage; override;
    procedure ResetDefaults; override;
    procedure ParseLine(pLineEnd: pchar); override;
    procedure ParseRequestLine(pLineEnd: pchar);
    procedure ProcessHeaders; override;
    procedure WriteError(AStatus: TLHTTPStatus); override;
    procedure WriteHeaders(AHeaderResponse, ADataResponse: TOutputItem);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure LogAccess(const AMessage: string); override;
    procedure StartResponse(AOutputItem: TBufferOutput);

    property RequestInfo: TRequestInfo read FRequestInfo;
  end;
  
  { TLHTTPServer }
  
  TURIHandler = class(TObject)
  private
    FNext: TURIHandler;
  public
    function HandleURI(ASocket: TLHTTPServerSocket): TOutputItem; virtual; abstract;
  end;

  TLHTTPConnection = class(TLTcp)
  protected
    FDelayFreeItems: TOutputItem;

    procedure FreeDelayFreeItems;
  public
    procedure DelayFree(AOutputItem: TOutputItem);
    procedure LogAccess(const AMessage: string); virtual;
    procedure HandleReceive(aSocket: TLSocket);
    procedure HandleSend(aSocket: TLSocket);
  end;

  TLHTTPServer = class(TLHTTPConnection)
  protected
    FHandlerList: TURIHandler;
  
    function InitSocket(aSocket: TLSocket): TLSocket; override;
    function HandleURI(ASocket: TLHTTPServerSocket): TOutputItem;
  public
    constructor Create(AOwner: TComponent); override;

    procedure RegisterHandler(AHandler: TURIHandler);
  end;

implementation

const
  PackBufferSize = 512;     { if less data than this, pack buffer }
  RequestBufferSize = 1024;
  DataBufferSize = 16*1024;

constructor TOutputItem.Create(ASocket: TLHTTPSocket);
begin
  FSocket := ASocket;
  inherited Create;
end;

destructor TOutputItem.Destroy;
begin
  if FSocket.FCurrentInput = Self then
    FSocket.FCurrentInput := nil;
    
  if FPrevDelayFree = nil then
    FSocket.FConnection.FDelayFreeItems := FNextDelayFree
  else
    FPrevDelayFree.FNextDelayFree := FNextDelayFree;
  if FNextDelayFree <> nil then
    FNextDelayFree.FPrevDelayFree := FPrevDelayFree;

  inherited;
end;

procedure TOutputItem.DoneInput;
begin
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

constructor TBufferOutput.Create(ASocket: TLHTTPSocket);
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
    FOutputPending := FBufferPos > FBufferOffset;
    if FOutputPending then
    begin
      lOffset := HexReverse(FBufferPos-FBufferOffset, FBuffer+FBufferOffset-3);
      FBuffer[FBufferOffset-2] := #13;
      FBuffer[FBufferOffset-1] := #10;
      FBuffer[FBufferPos] := #13;
      FBuffer[FBufferPos+1] := #10;
      FBufferSize := FBufferPos+2;
      FBufferPos := FBufferOffset-lOffset-2;
    end else begin
      FBufferPos := 0;
      FBufferSize := 0;
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
  if FOutputPending then
  begin
    Result := inherited WriteData;
    if not Result then exit;
  end;

  Result := FEof;
  if not Result then
    PrepareChunk;
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
        FSocket.AddToOutput(TMemoryOutput.Create(FSocket, FBuffer, FBufferOffset,
          FBufferPos, true));
        FSocket.AddContentLength(FBufferPos-FBufferOffset);
      end;
      if not FEof then
      begin
        FBuffer := GetMem(DataBufferSize);
        PrepareBuffer;
      end else begin
        FBuffer := nil;
        FBufferPos := 0;
        FBufferSize := 0;
        FSocket.DoneBuffer(Self);
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

constructor TMemoryOutput.Create(ASocket: TLHTTPSocket; ABuffer: pointer; 
  ABufferOffset, ABufferSize: integer; AFreeBuffer: boolean);
begin
  inherited Create(ASocket);

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

  FBuffer := GetMem(RequestBufferSize);
  FBufferSize := RequestBufferSize;
  FBufferPos := FBuffer;
  FBufferEnd := FBufferPos;
  FBuffer[0] := #0;
  FKeepAlive := true;
end;

destructor TLHTTPSocket.Destroy;
var
  lOutput: TOutputItem;
begin
  while FCurrentOutput <> nil do
  begin
    lOutput := FCurrentOutput;
    FCurrentOutput := FCurrentOutput.FNext;
    lOutput.Free;
  end;
  if FCurrentInput <> nil then
    FreeAndNil(FCurrentInput);
  
  inherited;
  FreeMem(FBuffer);
end;

procedure TLHTTPSocket.AddContentLength(ALength: integer);
begin
end;

procedure TLHTTPSocket.DoneBuffer(AOutput: TBufferOutput);
begin
end;

procedure TLHTTPSocket.LogMessage;
begin
end;

procedure TLHTTPSocket.LogAccess(const AMessage: string);
begin
end;

procedure TLHTTPSocket.WriteError(AStatus: TLHTTPStatus);
begin
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
end;

procedure TLHTTPSocket.ResetDefaults;
begin
  FParseBuffer := @ParseRequest;
end;

procedure TLHTTPSocket.FlushRequest;
begin
  FillDWord(FParameters, sizeof(FParameters) div 4, 0);
  ResetDefaults;
end;

procedure TLHTTPSocket.HandleReceive;
var
  lRead: integer;
begin
  if FRequestInputDone then 
  begin
    FPendingData := true;
    FIgnoreRead := true;
    exit;
  end;
  
  if ptruint(FBufferEnd-FBuffer) = FBufferSize-1 then
  begin
    WriteError(hsRequestTooLong);
    exit;
  end;

  FPendingData := false;
  lRead := Get(FBufferEnd^, FBufferSize-PtrUInt(FBufferEnd-FBuffer)-1);
  if lRead <= 0 then exit;
  Inc(FBufferEnd, lRead);
  FBufferEnd^ := #0;
  ParseBuffer;

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
  if FRequestHeaderDone then exit(not FRequestInputDone);
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
      exit(not FRequestInputDone);
  until false;
end;

function TrySingleDigit(ADigit: char; out OutDigit: byte): boolean;
begin
  Result := (ord(ADigit) >= ord('0')) and (ord(ADigit) <= ord('9'));
  if not Result then exit;
  OutDigit := ord(ADigit) - ord('0');
end;

procedure TLHTTPSocket.ParseParameterLine(pLineEnd: pchar);
var
  lPos: pchar;
  I: TLHTTPParameter;
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
  for I := Low(TLHTTPParameter) to High(TLHTTPParameter) do
    if (Length(HTTPParameterStrings[I]) = lLen)
    and CompareMem(FBufferPos, PChar(HTTPParameterStrings[I]), lLen) then
    begin
      repeat
        inc(lPos);
      until lPos^ <> ' ';
      FParameters[I] := lPos;
      break;
    end;
end;

procedure TLHTTPSocket.ParseLine(pLineEnd: pchar);
begin
  if FBufferPos[0] = #0 then
  begin
    FRequestHeaderDone := true;
    ProcessHeaders;
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

function TLHTTPSocket.ParseBuffer: boolean;
var
  lParseFunc: TParseBufferMethod;
begin
  repeat
    lParseFunc := FParseBuffer;
    Result := FParseBuffer();
    if not Result and not FRequestInputDone then
    begin
      FRequestInputDone := true;
      if FCurrentInput <> nil then
        FCurrentInput.DoneInput;
    end;
    { if parse func changed mid-run, then we should continue calling the new 
      one: header + data }
  until (lParseFunc = FParseBuffer) or not Result;
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
      lFreeOutput.Free;
    end;
    { nothing left to write, request was busy and now completed }
    if FCurrentOutput = nil then
    begin
      LogMessage;
      FOutputDone := true;
      if not FKeepAlive then
      begin
        Disconnect;
        exit;
      end;
    end;
  end;
  if FOutputDone and FRequestInputDone then
  begin
    if FRequestInputDone then
    begin
      { next request }
      FRequestInputDone := false;
      FRequestHeaderDone := false;
      FOutputDone := false;
      FRequestPos := FBufferPos;
      FlushRequest;
      { rewind buffer pointers if at end of buffer anyway }
      if FBufferPos = FBufferEnd then
        PackRequestBuffer;
    end;
    if ParseBuffer and FPendingData then 
    begin
      { end of input buffer reached, try reading more }
      HandleReceive;
    end;
  end;
end;

{ TLHTTPServerSocket }

constructor TLHTTPServerSocket.Create;
begin
  inherited;

  FLogMessage := InitStringBuffer(256);
  FRequestInfo.RequestType := hmUnknown;
  ResetDefaults;
end;

destructor TLHTTPServerSocket.Destroy;
begin
  FreeMem(FLogMessage.Memory);
  inherited;
end;

procedure TLHTTPServerSocket.AddContentLength(ALength: integer);
begin
  Inc(FRequestInfo.ContentLength, ALength);
end;

procedure TLHTTPServerSocket.DoneBuffer(AOutput: TBufferOutput);
begin
  WriteHeaders(AOutput, nil);
end;

procedure TLHTTPServerSocket.LogAccess(const AMessage: string);
begin
  FConnection.LogAccess(AMessage);
end;

procedure TLHTTPServerSocket.LogMessage;
begin
  { log a message about this request, 
    '<StatusCode> <Length> "<Referer>" "<User-Agent>"' }
  AppendString(FLogMessage, IntToStr(HTTPStatusCodes[FRequestInfo.Status]));
  AppendChar(FLogMessage, ' ');
  AppendString(FLogMessage, IntToStr(FRequestInfo.ContentLength));
  AppendString(FLogMessage, ' "');
  AppendString(FLogMessage, FParameters[hpReferer]);
  AppendString(FLogMessage, '" "');
  AppendString(FLogMessage, FParameters[hpUserAgent]);
  AppendChar(FLogMessage, '"');
  AppendChar(FLogMessage, #0);
  LogAccess(FLogMessage.Memory);
end;

procedure TLHTTPServerSocket.ResetDefaults;
begin
  inherited;
  FRequestInfo.ContentType := 'application/octet-stream';
end;

procedure TLHTTPServerSocket.FlushRequest;
begin
  with FRequestInfo do
  begin
    { request }
    Status := hsOK;
    RequestType := hmUnknown;
    Argument := nil;
    QueryParams := nil;
    Version := 0;
    { response }
    ContentLength := 0;
    ContentCharset := '';
    LastModified := 0.0;
    TransferEncoding := teIdentity;
    ExtraHeaders := '';
  end;
  inherited;
end;
  
procedure TLHTTPServerSocket.ParseLine(pLineEnd: pchar);
begin
  if FRequestInfo.RequestType = hmUnknown then
  begin
    ParseRequestLine(pLineEnd);
    exit;
  end;

  inherited;
end;

procedure TLHTTPServerSocket.ParseRequestLine(pLineEnd: pchar);
var
  lPos: pchar;
  lMajorVersion, lMinorVersion: byte;
  I: TLHTTPMethod;
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
  for I := Low(TLHTTPMethod) to High(TLHTTPMethod) do
  begin
    if I = hmUnknown then
    begin
      WriteError(hsNotImplemented);
      exit;
    end;
    
    if ((lPos-FBufferPos) = Length(HTTPMethodStrings[I]))
    and CompareMem(FBufferPos, PChar(HTTPMethodStrings[I]), lPos-FBufferPos) then
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
    FParameters[hpHost] := lPos;
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

procedure TLHTTPServerSocket.ProcessHeaders;
  { process request }
var
  lPos: pchar;
  lCode: integer;
begin
  { do HTTP/1.1 Host-field present check }
  if (FRequestInfo.Version > 10) and (FParameters[hpHost] = nil) then
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
  if FParameters[hpConnection] <> nil then
  begin
    if StrIComp(FParameters[hpConnection], 'keep-alive') = 0 then
      FKeepAlive := true
    else
    if StrIComp(FParameters[hpConnection], 'close') = 0 then
      FKeepAlive := false;
  end;
  
  DecodeUrl(FRequestInfo.Argument);
  if not CheckPermission(FRequestInfo.Argument) then
  begin
    WriteError(hsForbidden);
  end else begin
    if FParameters[hpContentLength] <> nil then
    begin
      FParseBuffer := @ParseEntityPlain;
      Val(FParameters[hpContentLength], FInputRemaining, lCode);
      if lCode <> 0 then
      begin
        WriteError(hsBadRequest);
        exit;
      end;
    end else 
    if FParameters[hpTransferEncoding] <> nil then
    begin
      if (StrIComp(FParameters[hpTransferEncoding], 'chunked') = 0) then
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
    end else if FRequestInputDone then
      FCurrentInput.DoneInput;
  end;
end;

procedure TLHTTPServerSocket.StartResponse(AOutputItem: TBufferOutput);
var
  lDateTime: TDateTime;
begin
  { check modification date }
  if FRequestInfo.Status = hsOK then
  begin
    if (FParameters[hpIfModifiedSince] <> nil) 
      and (FRequestInfo.LastModified <> 0.0) then
    begin
      if TryHTTPDateStrToDateTime(FParameters[hpIfModifiedSince], lDateTime) then
      begin
        if lDateTime > FRequestInfo.DateTime then
          FRequestInfo.Status := hsBadRequest
        else
        if FRequestInfo.LastModified <= lDateTime then
          FRequestInfo.Status := hsNotModified;
      end;
    end else
    if (FParameters[hpIfUnmodifiedSince] <> nil) then
    begin
      if TryHTTPDateStrToDateTime(FParameters[hpIfUnmodifiedSince], lDateTime) then
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
    FConnection.DelayFree(AOutputItem);
  end;
end;

function TLHTTPServerSocket.HandleURI: TOutputItem;
begin
  Result := TLHTTPServer(FConnection).HandleURI(Self);
end;

procedure TLHTTPServerSocket.WriteError(AStatus: TLHTTPStatus);
var
  lMessage: string;
  lMsgOutput: TMemoryOutput;
begin
  if AStatus >= hsBadRequest then
    FKeepAlive := false;
  lMessage := HTTPDescriptions[AStatus];
  FRequestInfo.Status := AStatus;
  FRequestInfo.ContentLength := Length(lMessage);
  FRequestInfo.TransferEncoding := teIdentity;
  if Length(lMessage) > 0 then
  begin
    FRequestInfo.ContentType := 'text/html';
    lMsgOutput := TMemoryOutput.Create(Self, PChar(lMessage), 0, Length(lMessage), false)
  end else begin
    FRequestInfo.ContentType := '';
    lMsgOutput := nil;
  end;
  WriteHeaders(nil, lMsgOutput);
end;

procedure TLHTTPServerSocket.WriteHeaders(AHeaderResponse, ADataResponse: TOutputItem);
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
  AppendString(lMessage, ' GMT');
  if Length(FRequestInfo.ContentType) > 0 then
  begin
    AppendString(lMessage, #13#10+'Content-Type: ');
    AppendString(lMessage, FRequestInfo.ContentType);
    if Length(FRequestInfo.ContentCharset) > 0 then
    begin
      AppendString(lMessage, '; charset=');
      AppendString(lMessage, FRequestInfo.ContentCharset);
    end;
  end;
  if FRequestInfo.ContentLength <> 0 then
  begin
    AppendString(lMessage, #13#10+'Content-Length: ');
    Str(FRequestInfo.ContentLength, lTemp);
    AppendString(lMessage, lTemp);
  end;
  if FRequestInfo.TransferEncoding <> teIdentity then
  begin
    { only other possibility: teChunked }
    AppendString(lMessage, #13#10+'Transfer-Encoding: chunked');
  end;
  if FRequestInfo.LastModified <> 0.0 then
  begin
    AppendString(lMessage, #13#10+'Last-Modified: ');
    AppendString(lMessage, FormatDateTime(HTTPDateFormat, FRequestInfo.LastModified));
    AppendString(lMessage, ' GMT');
  end;
  AppendString(lMessage, #13#10+'Connection: ');
  if FKeepAlive then
    AppendString(lMessage, 'keep-alive')
  else
    AppendString(lMessage, 'close');
  AppendString(lMessage, #13#10);
  AppendString(lMessage, FRequestInfo.ExtraHeaders);
  AppendString(lMessage, #13#10);
  if AHeaderResponse <> nil then
  begin
    AHeaderResponse.FBuffer := lMessage.Memory;
    AHeaderResponse.FBufferSize := lMessage.Pos-lMessage.Memory;
  end else
    AddToOutput(TMemoryOutput.Create(Self, lMessage.Memory, 0,
      lMessage.Pos-lMessage.Memory, true));

  if ADataResponse <> nil then
  begin
    if FRequestInfo.RequestType = hmHead then
      FConnection.DelayFree(ADataResponse)
    else
      AddToOutput(ADataResponse);
  end;
end;

{ TLHTTPConnection }

procedure TLHTTPConnection.FreeDelayFreeItems;
var
  lItem: TOutputItem;
begin
  while FDelayFreeItems <> nil do
  begin
    lItem := FDelayFreeItems;
    FDelayFreeItems := FDelayFreeItems.FNextDelayFree;
    lItem.Free;
  end;
end;

procedure TLHTTPConnection.DelayFree(AOutputItem: TOutputItem);
begin
  if FDelayFreeItems <> nil then
    FDelayFreeItems.FPrevDelayFree := AOutputItem;
  AOutputItem.FNextDelayFree := FDelayFreeItems;
  FDelayFreeItems := AOutputItem;
end;

procedure TLHTTPConnection.LogAccess(const AMessage: string);
begin
end;

procedure TLHTTPConnection.HandleReceive(aSocket: TLSocket);
begin
  TLHTTPSocket(aSocket).HandleReceive;
  FreeDelayFreeItems;
end;

procedure TLHTTPConnection.HandleSend(aSocket: TLSocket);
begin
  TLHTTPSocket(aSocket).WriteBlock;
  FreeDelayFreeItems;
end;

{ TLHTTPServer }

constructor TLHTTPServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SocketClass := TLHTTPServerSocket;
  OnCanSend := @HandleSend;
  OnReceive := @HandleReceive;
end;

function TLHTTPServer.InitSocket(aSocket: TLSocket): TLSocket;
begin
  Result := inherited InitSocket(aSocket);
  TLHTTPSocket(aSocket).FConnection := Self;
end;

function TLHTTPServer.HandleURI(ASocket: TLHTTPServerSocket): TOutputItem;
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

procedure TLHTTPServer.RegisterHandler(AHandler: TURIHandler);
begin
  if AHandler = nil then exit;
  AHandler.FNext := FHandlerList;
  FHandlerList := AHandler;
end;

{ TLHTTPClientSocket }

constructor TLHTTPClientSocket.Create;
begin
  // TODO
  inherited Create;
end;

procedure TLHTTPClientSocket.ParseLine(pLineEnd: pchar);
begin
  // TODO
end;

procedure TLHTTPClientSocket.ProcessHeaders;
begin
  // TODO
end;

procedure TLHTTPClientSocket.ResetDefaults;
begin
  inherited;
  // TODO
end;

end.

