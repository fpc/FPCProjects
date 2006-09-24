{ HTTP server and client components

  Copyright (C) 2006 Micha Nelissen

  This library is Free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
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

unit lhttp;

{$mode objfpc}{$h+}
{$inline on}

interface

uses
  classes, sysutils, strutils, lnet, levents, lhttputil, lstrbuffer;

type
  TLHTTPMethod = (hmHead, hmGet, hmPost, hmUnknown);
  TLHTTPParameter = (hpConnection, hpContentLength, hpContentType,
    hpAccept, hpAcceptCharset, hpAcceptEncoding, hpAcceptLanguage, hpHost,
    hpFrom, hpReferer, hpUserAgent, hpRange, hpTransferEncoding,
    hpIfModifiedSince, hpIfUnmodifiedSince, hpCookie);
  TLHTTPStatus = (hsUnknown, hsOK, hsMovedPermanently, hsFound, hsNotModified, 
    hsBadRequest, hsForbidden, hsNotFound, hsPreconditionFailed, hsRequestTooLong,
    hsInternalError, hsNotImplemented, hsNotAllowed);
  TLHTTPTransferEncoding = (teIdentity, teChunked);
  TLHTTPClientError = (ceNone, ceMalformedStatusLine, ceVersionNotSupported,
    ceUnsupportedEncoding);

const
  HTTPDisconnectStatuses = [hsBadRequest, hsRequestTooLong, hsForbidden, 
    hsInternalError, hsNotAllowed];
  HTTPMethodStrings: array[TLHTTPMethod] of string =
    ('HEAD', 'GET', 'POST', '');
  HTTPParameterStrings: array[TLHTTPParameter] of string =
    ('CONNECTION', 'CONTENT-LENGTH', 'CONTENT-TYPE', 'ACCEPT', 
     'ACCEPT-CHARSET', 'ACCEPT-ENCODING', 'ACCEPT-LANGUAGE', 'HOST',
     'FROM', 'REFERER', 'USER-AGENT', 'RANGE', 'TRANSFER-ENCODING',
     'IF-MODIFIED-SINCE', 'IF-UNMODIFIED-SINCE', 'COOKIE');
  HTTPStatusCodes: array[TLHTTPStatus] of dword =
    (0, 200, 301, 302, 304, 400, 403, 404, 412, 414, 500, 501, 504);
  HTTPTexts: array[TLHTTPStatus] of string = 
    ('', 'OK', 'Moved Permanently', 'Found', 'Not Modified', 'Bad Request', 'Forbidden', 
     'Not Found', 'Precondition Failed', 'Request Too Long', 'Internal Error',
     'Method Not Implemented', 'Method Not Allowed');
  HTTPDescriptions: array[TLHTTPStatus] of string = (
      { hsUnknown }
    '',
      { hsOK }
    '',
      { hsMovedPermanently }
    '',
      { hsFound }
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
  TLHTTPClientSocket = class;
  
  PRequestInfo = ^TRequestInfo;
  TRequestInfo = record
    RequestType: TLHTTPMethod;
    DateTime: TDateTime;
    Method: pchar;
    Argument: pchar;
    QueryParams: pchar;
    VersionStr: pchar;
    Version: dword;
  end;

  PClientRequest = ^TClientRequest;
  TClientRequest = record
    Method: TLHTTPMethod;
    URI: string;
    QueryParams: string;
  end;

  PHeaderOutInfo = ^THeaderOutInfo;
  THeaderOutInfo = record
    ContentLength: integer;
    TransferEncoding: TLHTTPTransferEncoding;
    ExtraHeaders: string;
    Version: dword;
  end;

  PResponseInfo = ^TResponseInfo;
  TResponseInfo = record
    Status: TLHTTPStatus;
    ContentType: string;
    ContentCharset: string;
    LastModified: TDateTime;
  end;

  TWriteBlockStatus = (wsPendingData, wsWaitingData, wsDone);
  TWriteBlockMethod = function: TWriteBlockStatus of object;

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
    function WriteData: TWriteBlockStatus; virtual;
  public
    constructor Create(ASocket: TLHTTPSocket);
    destructor Destroy; override;

    function  HandleInput(ABuffer: pchar; ASize: dword): dword; virtual;
    procedure LogError(const AMessage: string);
    function  WriteBlock: TWriteBlockStatus; virtual;

    property Socket: TLHTTPSocket read FSocket;
  end;

  TBufferOutput = class(TOutputItem)
  protected
    procedure PrepareChunk;
    procedure PrepareBuffer;
  public
    constructor Create(ASocket: TLHTTPSocket);
    destructor Destroy; override;

    function WriteChunk: TWriteBlockStatus;
    function WriteBuffer: TWriteBlockStatus;
    function WritePlain: TWriteBlockStatus;
    function WriteBlock: TWriteBlockStatus; override;
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
  TLInputEvent = function(ASocket: TLHTTPClientSocket; ABuffer: pchar; ASize: dword): dword of object;
  TLCanWriteEvent = procedure(ASocket: TLHTTPClientSocket; var OutputEof: TWriteBlockStatus) of object;
  TLHTTPClientProc = procedure(ASocket: TLHTTPClientSocket) of object;

  TLHTTPConnection = class(TLTcp)
  protected
    procedure ReceiveEvent(aSocket: TLHandle); override;
    procedure CanSendEvent(aSocket: TLHandle); override;
  public
    destructor Destroy; override;

    procedure LogAccess(const AMessage: string); virtual;
  end;

  TLHTTPSocket = class(TLSocket)
  protected
    FBuffer: pchar;
    FBufferPos: pchar;
    FBufferEnd: pchar;
    FBufferSize: dword;
    FRequestBuffer: pchar;
    FRequestPos: pchar;
    FRequestInputDone: boolean;
    FRequestHeaderDone: boolean;
    FOutputDone: boolean;
    FInputRemaining: dword;
    FChunkState: TChunkState;
    FCurrentInput: TOutputItem;
    FCurrentOutput: TOutputItem;
    FLastOutput: TOutputItem;
    FKeepAlive: boolean;
    FHeaderOut: THeaderOutInfo;
    FConnection: TLHTTPConnection;
    FParseBuffer: TParseBufferMethod;
    FParameters: TLHTTPParameterArray;
    FDelayFreeItems: TOutputItem;

    procedure AddContentLength(ALength: integer);
    function  CalcAvailableBufferSpace: integer;
    procedure DelayFree(AOutputItem: TOutputItem);
    procedure Disconnect; override;
    procedure DoneBuffer(AOutput: TBufferOutput); virtual;
    procedure FreeDelayFreeItems;
    procedure LogMessage; virtual;
    procedure FlushRequest; virtual;
    procedure PackRequestBuffer;
    procedure PackInputBuffer;
    function  ParseRequest: boolean;
    function  ParseEntityPlain: boolean;
    function  ParseEntityChunked: boolean;
    procedure ParseLine(pLineEnd: pchar); virtual;
    procedure ParseParameterLine(pLineEnd: pchar);
    function  ProcessEncoding: boolean;
    procedure ProcessHeaders; virtual; abstract;
    procedure RelocateVariable(var AVar: pchar);
    procedure RelocateVariables; virtual;
    procedure ResetDefaults; virtual;
    function  SetupEncoding(AOutputItem: TBufferOutput): boolean;
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
    property HeaderOut: THeaderOutInfo read FHeaderOut;
    property Parameters: TLHTTPParameterArray read FParameters;
  end;

  { http server }
  
  TLHTTPServerSocket = class(TLHTTPSocket)
  protected
    FLogMessage: TStringBuffer;
    FRequestInfo: TRequestInfo;
    FResponseInfo: TResponseInfo;

    procedure DoneBuffer(AOutput: TBufferOutput); override;
    procedure FlushRequest; override;
    function  HandleURI: TOutputItem; virtual;
    procedure LogMessage; override;
    procedure RelocateVariables; override;
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
    property ResponseInfo: TResponseInfo read FResponseInfo;
  end;
  
  TURIHandler = class(TObject)
  private
    FNext: TURIHandler;
  public
    function HandleURI(ASocket: TLHTTPServerSocket): TOutputItem; virtual; abstract;
  end;

  TLHTTPServer = class(TLHTTPConnection)
  protected
    FHandlerList: TURIHandler;
    FLogMessageTZString: string;

    function InitSocket(aSocket: TLSocket): TLSocket; override;
    function HandleURI(ASocket: TLHTTPServerSocket): TOutputItem;
  public
    constructor Create(AOwner: TComponent); override;

    procedure RegisterHandler(AHandler: TURIHandler);
  end;

  { http client }

  TLHTTPClientSocket = class(TLHTTPSocket)
  protected
    FRequest: PClientRequest;
    FResponseStatus: TLHTTPStatus;
    FResponseVersion: dword;
    FResponseReason: pchar;
    FError: TLHTTPClientError;
    
    procedure Cancel(AError: TLHTTPClientError);
    procedure ParseLine(pLineEnd: pchar); override;
    procedure ParseStatusLine(pLineEnd: pchar);
    procedure ProcessHeaders; override;
    procedure ResetDefaults; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure SendRequest;

    property Error: TLHTTPClientError read FError write FError;
    property ResponseStatus: TLHTTPStatus read FResponseStatus;
    property ResponseVersion: dword read FResponseVersion;
    property ResponseReason: pchar read FResponseReason;
  end;

  TLHTTPClient = class(TLHTTPConnection)
  protected
    FHost: string;
    FPort: integer;
    FRequest: TClientRequest;
    FOutputEof: boolean;
    FOnCanWrite: TLCanWriteEvent;
    FOnDoneInput: TLHTTPClientProc;
    FOnInput: TLInputEvent;
    FOnProcessHeaders: TLHTTPClientProc;
    
    procedure ConnectEvent(aSocket: TLHandle); override;
    procedure DoDoneInput(ASocket: TLHTTPClientSocket);
    function  DoHandleInput(ASocket: TLHTTPClientSocket; ABuffer: pchar; ASize: dword): dword;
    procedure DoProcessHeaders(ASocket: TLHTTPClientSocket);
    function  DoWriteBlock(ASocket: TLHTTPClientSocket): TWriteBlockStatus;
    function  InitSocket(aSocket: TLSocket): TLSocket; override;
    procedure InternalSendRequest;
  public
    constructor Create(AOwner: TComponent); override;

    procedure SendRequest;

    property Host: string read FHost write FHost;
    property Method: TLHTTPMethod read FRequest.Method write FRequest.Method;
    property Port: integer read FPort write FPort;
    property URI: string read FRequest.URI write FRequest.URI;
    property OnCanWrite: TLCanWriteEvent read FOnCanWrite write FOnCanWrite;
    property OnDoneInput: TLHTTPClientProc read FOnDoneInput write FOnDoneInput;
    property OnInput: TLInputEvent read FOnInput write FOnInput;
    property OnProcessHeaders: TLHTTPClientProc read FOnProcessHeaders write FOnProcessHEaders;
  end;

implementation

const
  PackBufferSize = 512;     { if less data than this, pack buffer }
  RequestBufferSize = 1024;
  DataBufferSize = 16*1024;

  BufferEmptyToWriteStatus: array[boolean] of TWriteBlockStatus =
    (wsPendingData, wsDone);
  EofToWriteStatus: array[boolean] of TWriteBlockStatus =
    (wsWaitingData, wsDone);

{ helper functions }

function TrySingleDigit(ADigit: char; out OutDigit: byte): boolean;
begin
  Result := (ord(ADigit) >= ord('0')) and (ord(ADigit) <= ord('9'));
  if not Result then exit;
  OutDigit := ord(ADigit) - ord('0');
end;

function HTTPVersionCheck(AStr, AStrEnd: pchar; out AVersion: dword): boolean;
var
  lMajorVersion, lMinorVersion: byte;
begin
  Result := ((AStrEnd-AStr) = 8) 
    and CompareMem(AStr, pchar('HTTP/'), 5)
    and TrySingleDigit(AStr[5], lMajorVersion) 
    and (AStr[6] = '.')
    and TrySingleDigit(AStr[7], lMinorVersion);
  AVersion := lMajorVersion * 10 + lMinorVersion;
end;

function CodeToHTTPStatus(ACode: dword): TLHTTPStatus;
begin
  for Result := Low(TLHTTPStatus) to High(TLHTTPStatus) do
    if HTTPStatusCodes[Result] = ACode then exit;
  Result := hsUnknown;
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

procedure HexToInt(ABuffer: pchar; out AValue: dword; out ACode: integer);
var
  Val, Incr: dword;
  Start: pchar;
begin
  Val := 0;
  Start := ABuffer;
  while ABuffer^ <> #0 do
  begin
    if (ABuffer^ >= '0') and (ABuffer^ <= '9') then
      Incr := ord(ABuffer^) - ord('0')
    else if (ABuffer^ >= 'A') and (ABuffer^ <= 'F') then
      Incr := ord(ABuffer^) - ord('A') + 10
    else if (ABuffer^ >= 'a') and (ABuffer^ <= 'f') then
      Incr := ord(ABuffer^) - ord('a') + 10
    else begin
      ACode := ABuffer - Start + 1;
      exit;
    end;
    Val := (Val * 16) + Incr;
    Inc(ABuffer);
  end;
  AValue := Val;
  ACode := 0;
end;

{ TOutputItem }

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
    FSocket.FDelayFreeItems := FNextDelayFree
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

procedure TOutputItem.LogError(const AMessage: string);
begin
  FSocket.LogError(AMessage, 0);
end;

function TOutputItem.WriteData: TWriteBlockStatus;
var
  lWritten: integer;
begin
  if FOutputPending then
  begin
    lWritten := FSocket.Send(FBuffer[FBufferPos], FBufferSize-FBufferPos);
    Inc(FBufferPos, lWritten);
    FOutputPending := FBufferPos < FBufferSize;
    Result := BufferEmptyToWriteStatus[not FOutputPending];
  end else
    Result := EofToWriteStatus[FEof];
end;

function TOutputItem.WriteBlock: TWriteBlockStatus;
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

function TBufferOutput.WriteChunk: TWriteBlockStatus;
var
  lOffset: integer;
begin
  if not FOutputPending and not FEof then
  begin
    Result := WriteData;
    FEof := Result = wsDone;
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
    end;
    if FEof then
    begin
      if not FOutputPending then
      begin
        { FBufferPos/Size still in "read mode" }
        FBufferSize := 0;
        FBufferPos := 0;
        FOutputPending := true;
      end;
      FBuffer[FBufferSize] := '0';
      FBuffer[FBufferSize+1] := #13;
      FBuffer[FBufferSize+2] := #10;
      { no trailer }
      FBuffer[FBufferSize+3] := #13;
      FBuffer[FBufferSize+4] := #10;
      inc(FBufferSize, 5);
    end;
  end else   
    Result := EofToWriteStatus[FEof];
  if FOutputPending then
  begin
    Result := inherited WriteData;
    if (Result = wsDone) and not FEof then
    begin
      Result := wsPendingData;
      PrepareChunk;
    end;
  end;
end;
  
function TBufferOutput.WriteBuffer: TWriteBlockStatus;
begin
  if not FOutputPending then
  begin
    Result := WriteData;
    FEof := Result = wsDone;
    FOutputPending := FEof;
    if FOutputPending or (FBufferPos = FBufferSize) then
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
  end else
    Result := EofToWriteStatus[FEof];
  if Result = wsDone then
    Result := inherited WriteData;
end;

function TBufferOutput.WritePlain: TWriteBlockStatus;
begin
  if not FOutputPending then
  begin
    Result := WriteData;
    FEof := Result = wsDone;
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
  if Result = wsWaitingData then
    PrepareBuffer;
end;

function TBufferOutput.WriteBlock: TWriteBlockStatus;
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
begin
  FreeDelayFreeItems;
  inherited;
  FreeMem(FBuffer);
end;

procedure TLHTTPSocket.Disconnect;
var
  lOutput: TOutputItem;
begin
  inherited Disconnect;
  while FCurrentOutput <> nil do
  begin
    lOutput := FCurrentOutput;
    FCurrentOutput := FCurrentOutput.FNext;
    lOutput.Free;
  end;
  if FCurrentInput <> nil then
    FreeAndNil(FCurrentInput);
end;

procedure TLHTTPSocket.FreeDelayFreeItems;
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

procedure TLHTTPSocket.DelayFree(AOutputItem: TOutputItem);
begin
  if AOutputItem = nil then exit;
  if FDelayFreeItems <> nil then
    FDelayFreeItems.FPrevDelayFree := AOutputItem;
  AOutputItem.FNextDelayFree := FDelayFreeItems;
  FDelayFreeItems := AOutputItem;
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

procedure TLHTTPSocket.AddContentLength(ALength: integer);
begin
  Inc(FHeaderOut.ContentLength, ALength);
end;

procedure TLHTTPSocket.ResetDefaults;
begin
  FParseBuffer := @ParseRequest;
end;

procedure TLHTTPSocket.FlushRequest;
begin
  FillDWord(FParameters, sizeof(FParameters) div 4, 0);
  with FHeaderOut do
  begin
    ContentLength := 0;
    TransferEncoding := teIdentity;
    ExtraHeaders := '';
    Version := 0;
  end;
  ResetDefaults;
end;

function TLHTTPSocket.CalcAvailableBufferSpace: integer;
begin
  Result := FBufferSize-PtrUInt(FBufferEnd-FBuffer)-1;
end;

procedure TLHTTPSocket.HandleReceive;
var
  lRead: integer;
begin
  if FRequestInputDone then 
  begin
    IgnoreRead := true;
    exit;
  end;

  lRead := CalcAvailableBufferSpace;
  { if buffer has filled up, keep ignoring and continue parsing requests }
  if lRead > 0 then
  begin
    IgnoreRead := false;
    lRead := Get(FBufferEnd^, lRead);
    if lRead = 0 then exit;
    Inc(FBufferEnd, lRead);
    FBufferEnd^ := #0;
  end;
  ParseBuffer;

  if FIgnoreWrite then
    WriteBlock;
end;

procedure TLHTTPSocket.RelocateVariable(var AVar: pchar);
begin
  if AVar = nil then exit;
  AVar := FBuffer + (AVar - FRequestPos);
end;

procedure TLHTTPSocket.RelocateVariables;
var
  I: TLHTTPParameter;
begin
  for I := Low(TLHTTPParameter) to High(TLHTTPParameter) do
    RelocateVariable(FParameters[I]);
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
    RelocateVariable(FBufferPos);
    RelocateVariables;
    { include null-terminator, where FBufferEnd is pointing at }
    Move(FRequestPos^, FBuffer^, lBytesLeft+1);
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
        HexToInt(FBufferPos, FInputRemaining, lCode);
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
      if (FRequestBuffer <> nil) or (FRequestPos <> nil) then
        PackRequestBuffer
      else if CalcAvailableBufferSpace = 0 then
        WriteError(hsRequestTooLong);
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

function TLHTTPSocket.ProcessEncoding: boolean;
var
  lCode: integer;
begin
  Result := true;
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
      Result := false;
    end;
  end else begin
    FRequestInputDone := true;
  end;
end;
  
function TLHTTPSocket.SetupEncoding(AOutputItem: TBufferOutput): boolean;
begin
  if FHeaderOut.ContentLength = 0 then
  begin
    if FHeaderOut.Version >= 11 then
    begin
      { we can use chunked encoding }
      FHeaderOut.TransferEncoding := teChunked;
      AOutputItem.FWriteBlock := @AOutputItem.WriteChunk;
    end else begin
      { we need to buffer the response to find its length }
      FHeaderOut.TransferEncoding := teIdentity;
      AOutputItem.FWriteBlock := @AOutputItem.WriteBuffer;
      { need to accumulate data before starting header output }
      AddToOutput(AOutputItem);
      exit(false);
    end;
  end else begin
    FHeaderOut.TransferEncoding := teIdentity;
    AOutputItem.FWriteBlock := @AOutputItem.WritePlain;
  end;
  Result := true;
end;

procedure TLHTTPSocket.WriteBlock;
begin
  if FCurrentOutput = nil then exit;
  while true do
  begin
    case FCurrentOutput.WriteBlock of
      wsDone:
      begin
        if FCurrentOutput = FLastOutput then
          FLastOutput := nil;
        { some output items may trigger this parse/write loop }
        DelayFree(FCurrentOutput);
        FCurrentOutput := FCurrentOutput.FNext;
      end;
      wsWaitingData:
      begin
        { wait for more data from external source }
        break;
      end;
    end;
    { nothing left to write, request was busy and now completed }
    if FCurrentOutput = nil then
    begin
      LogMessage;
      FOutputDone := true;
    end;
    { if not ignoring, then the send buffer is full }
    if not FIgnoreWrite or not FConnected then
      break;

    if FCurrentOutput = nil then
    begin
      if not FOutputDone or (not FRequestInputDone and FKeepAlive) then
        break;

      if not FKeepAlive then
      begin
        Disconnect;
        exit;
      end;

      { next request }
      FRequestInputDone := false;
      FRequestHeaderDone := false;
      FOutputDone := false;
      FRequestPos := FBufferPos;
      FlushRequest;
      { rewind buffer pointers if at end of buffer anyway }
      if FBufferPos = FBufferEnd then
        PackRequestBuffer;

      if ParseBuffer and IgnoreRead then 
      begin
        { end of input buffer reached, try reading more }
        HandleReceive;
      end;

      if FCurrentOutput = nil then 
        break;
    end;
  end;
end;

{ TLHTTPServerSocket }

constructor TLHTTPServerSocket.Create;
begin
  inherited;

  FLogMessage := InitStringBuffer(256);
  ResetDefaults;
end;

destructor TLHTTPServerSocket.Destroy;
begin
  FreeMem(FLogMessage.Memory);
  inherited;
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
  AppendString(FLogMessage, IntToStr(HTTPStatusCodes[FResponseInfo.Status]));
  AppendChar(FLogMessage, ' ');
  AppendString(FLogMessage, IntToStr(FHeaderOut.ContentLength));
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
  FRequestInfo.RequestType := hmUnknown;
  with FResponseInfo do
  begin
    Status := hsOK;
    ContentType := 'application/octet-stream';
    ContentCharset := '';
    LastModified := 0.0;
  end;
end;

procedure TLHTTPServerSocket.FlushRequest;
  { reset structure to zero, not called from constructor }
begin
  with FRequestInfo do
  begin
    { request }
    Argument := nil;
    QueryParams := nil;
    Version := 0;
  end;
  inherited;
end;
  
procedure TLHTTPServerSocket.RelocateVariables;
begin
  RelocateVariable(FRequestInfo.Method);
  RelocateVariable(FRequestInfo.Argument);
  RelocateVariable(FRequestInfo.QueryParams);
  RelocateVariable(FRequestInfo.VersionStr);
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
  I: TLHTTPMethod;
  NowLocal: TDateTime;
begin
  { make a timestamp for this request }
  NowLocal := Now;
  FRequestInfo.DateTime := LocalTimeToGMT(NowLocal);
  { begin log message }
  FLogMessage.Pos := FLogMessage.Memory;
  AppendString(FLogMessage, PeerAddress);
  AppendString(FLogMessage, ' - [');
  AppendString(FLogMessage, FormatDateTime('dd/mmm/yyyy:hh:nn:ss', NowLocal));
  AppendString(FLogMessage, TLHTTPServer(FConnection).FLogMessageTZString);
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
  if not HTTPVersionCheck(lPos, pLineEnd, FRequestInfo.Version) then
  begin
    WriteError(hsBadRequest);
    exit;
  end;
  FRequestInfo.VersionStr := lPos;
  FHeaderOut.Version := FRequestInfo.Version;
  
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
    if not ProcessEncoding then
    begin
      WriteError(hsNotImplemented);
      exit;
    end;
      
    FCurrentInput := HandleURI;
    { if we have a valid outputitem, wait until it is ready 
      to produce its response }
    if FCurrentInput = nil then
    begin
      if FResponseInfo.Status = hsOK then
        WriteError(hsNotFound)
      else
        WriteError(FResponseInfo.Status);
    end else if FRequestInputDone then
      FCurrentInput.DoneInput;
  end;
end;

procedure TLHTTPServerSocket.StartResponse(AOutputItem: TBufferOutput);
var
  lDateTime: TDateTime;
begin
  { check modification date }
  if FResponseInfo.Status = hsOK then
  begin
    if (FParameters[hpIfModifiedSince] <> nil) 
      and (FResponseInfo.LastModified <> 0.0) then
    begin
      if TryHTTPDateStrToDateTime(FParameters[hpIfModifiedSince], lDateTime) then
      begin
        if lDateTime > FRequestInfo.DateTime then
          FResponseInfo.Status := hsBadRequest
        else
        if FResponseInfo.LastModified <= lDateTime then
          FResponseInfo.Status := hsNotModified;
      end;
    end else
    if (FParameters[hpIfUnmodifiedSince] <> nil) then
    begin
      if TryHTTPDateStrToDateTime(FParameters[hpIfUnmodifiedSince], lDateTime) then
      begin
        if (FResponseInfo.LastModified = 0.0) 
          or (lDateTime < FResponseInfo.LastModified) then
          FResponseInfo.Status := hsPreconditionFailed;
      end;
    end;
  end;

  if FResponseInfo.Status = hsOK then
  begin
    if SetupEncoding(AOutputItem) then
      WriteHeaders(nil, AOutputItem);
  end else begin
    WriteError(FResponseInfo.Status);
    DelayFree(AOutputItem);
  end;
end;

function TLHTTPServerSocket.HandleURI: TOutputItem; {inline;} {<--- triggers IE}
begin
  Result := TLHTTPServer(FConnection).HandleURI(Self);
end;

procedure TLHTTPServerSocket.WriteError(AStatus: TLHTTPStatus);
var
  lMessage: string;
  lMsgOutput: TMemoryOutput;
begin
  if AStatus in HTTPDisconnectStatuses then
    FKeepAlive := false;
  lMessage := HTTPDescriptions[AStatus];
  FRequestHeaderDone := true;
  FResponseInfo.Status := AStatus;
  FHeaderOut.ContentLength := Length(lMessage);
  FHeaderOut.TransferEncoding := teIdentity;
  if Length(lMessage) > 0 then
  begin
    FResponseInfo.ContentType := 'text/html';
    lMsgOutput := TMemoryOutput.Create(Self, PChar(lMessage), 0, Length(lMessage), false)
  end else begin
    FResponseInfo.ContentType := '';
    lMsgOutput := nil;
  end;
  WriteHeaders(nil, lMsgOutput);
end;

procedure TLHTTPServerSocket.WriteHeaders(AHeaderResponse, ADataResponse: TOutputItem);
var
  lTemp: string[23];
  lMessage: TStringBuffer;
begin
  lMessage := InitStringBuffer(504);
  
  AppendString(lMessage, 'HTTP/1.1 ');
  Str(HTTPStatusCodes[FResponseInfo.Status], lTemp);
  AppendString(lMessage, lTemp);
  AppendChar(lMessage, ' ');
  AppendString(lMessage, HTTPTexts[FResponseInfo.Status]);
  AppendString(lMessage, #13#10+'Date: ');
  AppendString(lMessage, FormatDateTime(HTTPDateFormat, FRequestInfo.DateTime));
  AppendString(lMessage, ' GMT');
  if Length(FResponseInfo.ContentType) > 0 then
  begin
    AppendString(lMessage, #13#10+'Content-Type: ');
    AppendString(lMessage, FResponseInfo.ContentType);
    if Length(FResponseInfo.ContentCharset) > 0 then
    begin
      AppendString(lMessage, '; charset=');
      AppendString(lMessage, FResponseInfo.ContentCharset);
    end;
  end;
  if FHeaderOut.ContentLength <> 0 then
  begin
    AppendString(lMessage, #13#10+'Content-Length: ');
    Str(FHeaderOut.ContentLength, lTemp);
    AppendString(lMessage, lTemp);
  end;
  if FHeaderOut.TransferEncoding <> teIdentity then
  begin
    { only other possibility: teChunked }
    AppendString(lMessage, #13#10+'Transfer-Encoding: chunked');
  end;
  if FResponseInfo.LastModified <> 0.0 then
  begin
    AppendString(lMessage, #13#10+'Last-Modified: ');
    AppendString(lMessage, FormatDateTime(HTTPDateFormat, FResponseInfo.LastModified));
    AppendString(lMessage, ' GMT');
  end;
  AppendString(lMessage, #13#10+'Connection: ');
  if FKeepAlive then
    AppendString(lMessage, 'keep-alive')
  else
    AppendString(lMessage, 'close');
  AppendString(lMessage, #13#10);
  AppendString(lMessage, FHeaderOut.ExtraHeaders);
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
      DelayFree(ADataResponse)
    else
      AddToOutput(ADataResponse);
  end;
end;

{ TLHTTPConnection }

destructor TLHTTPConnection.Destroy;
begin
  inherited;
end;

procedure TLHTTPConnection.LogAccess(const AMessage: string);
begin
end;

procedure TLHTTPConnection.ReceiveEvent(aSocket: TLHandle);
begin
  TLHTTPSocket(aSocket).HandleReceive;
  TLHTTPSocket(aSocket).FreeDelayFreeItems;
end;

procedure TLHTTPConnection.CanSendEvent(aSocket: TLHandle);
begin
  TLHTTPSocket(aSocket).WriteBlock;
  TLHTTPSocket(aSocket).FreeDelayFreeItems;
end;

{ TLHTTPServer }

constructor TLHTTPServer.Create(AOwner: TComponent);
var
  TZSign: char;
  TZSecsAbs: integer;
begin
  inherited Create(AOwner);

  SocketClass := TLHTTPServerSocket;
  if TZSeconds >= 0 then
    TZSign := '+'
  else
    TZSign := '-';
  TZSecsAbs := Abs(TZSeconds);
  FLogMessageTZString := Format(' %s%.2d%.2d] "', 
    [TZSign, TZSecsAbs div 3600, (TZSecsAbs div 60) mod 60]);
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
    if ASocket.ResponseInfo.Status <> hsOK then break;
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

{ TClientInput }

type
  TClientOutput = class(TOutputItem)
  protected
    FPersistent: boolean;
    
    procedure DoneInput; override;
  public
    constructor Create(ASocket: TLHTTPClientSocket);
    destructor Destroy; override;
    procedure FreeInstance; override;

    function  HandleInput(ABuffer: pchar; ASize: dword): dword; override;
    function  WriteBlock: TWriteBlockStatus; override;
  end;

constructor TClientOutput.Create(ASocket: TLHTTPClientSocket);
begin
  inherited Create(ASocket);
  FPersistent := true;
end;

destructor TClientOutput.Destroy;
begin
  if FPersistent then exit; 
  inherited;
end;

procedure TClientOutput.FreeInstance;
begin
  if FPersistent then exit; 
  inherited;
end;

procedure TClientOutput.DoneInput;
begin
  TLHTTPClient(TLHTTPClientSocket(FSocket).FConnection).
    DoDoneInput(TLHTTPClientSocket(FSocket));
end;

function  TClientOutput.HandleInput(ABuffer: pchar; ASize: dword): dword;
begin
  Result := TLHTTPClient(TLHTTPClientSocket(FSocket).FConnection).
    DoHandleInput(TLHTTPClientSocket(FSocket), ABuffer, ASize);
end;

function  TClientOutput.WriteBlock: TWriteBlockStatus;
begin
  Result := TLHTTPClient(TLHTTPClientSocket(FSocket).FConnection).
    DoWriteBlock(TLHTTPClientSocket(FSocket));
end;

{ TLHTTPClientSocket }

constructor TLHTTPClientSocket.Create;
begin
  inherited Create;

  FCurrentInput := TClientOutput.Create(Self);
  ResetDefaults;
end;

destructor TLHTTPClientSocket.Destroy;
begin
  if Assigned(FCurrentInput) then begin
    TClientOutput(FCurrentInput).FPersistent := false;
    FreeAndNil(FCurrentInput);
  end;
  inherited;
end;

procedure TLHTTPClientSocket.Cancel(AError: TLHTTPClientError);
begin
  FError := AError;
  Disconnect;
end;

procedure TLHTTPClientSocket.SendRequest;
var
  lMessage: TStringBuffer;
  lTemp: string[23];
begin
  lMessage := InitStringBuffer(504);

  AppendString(lMessage, HTTPMethodStrings[FRequest^.Method]);
  AppendChar(lMessage, ' ');
  AppendString(lMessage, FRequest^.URI);
  AppendChar(lMessage, ' ');
  AppendString(lMessage, 'HTTP/1.1'+#13#10);
  AppendString(lMessage, 'Host: ');
  AppendString(lMessage, TLHTTPClient(FConnection).Host);
  if TLHTTPClient(FConnection).Port <> 80 then
  begin
    AppendChar(lMessage, ':');
    Str(TLHTTPClient(FConnection).Port, lTemp);
    AppendString(lMessage, lTemp);
  end;
  AppendString(lMessage, #13#10);
  AppendString(lMessage, FHeaderOut.ExtraHeaders);
  AppendString(lMessage, #13#10);
  AddToOutput(TMemoryOutput.Create(Self, lMessage.Memory, 0,
    lMessage.Pos-lMessage.Memory, true));
  AddToOutput(FCurrentInput);
  
  WriteBlock;
end;

procedure TLHTTPClientSocket.ParseLine(pLineEnd: pchar);
begin
  if FError <> ceNone then
    exit;

  if FResponseStatus = hsUnknown then
  begin
    ParseStatusLine(pLineEnd);
    exit;
  end;

  inherited;
end;

procedure TLHTTPClientSocket.ParseStatusLine(pLineEnd: pchar);
var
  lPos: pchar;
begin
  lPos := FBufferPos;
  repeat
    if lPos >= pLineEnd then
    begin
      Cancel(ceMalformedStatusLine);
      exit;
    end;
    if lPos^ = ' ' then
      break;
    Inc(lPos);
  until false;
  if not HTTPVersionCheck(FBufferPos, lPos, FResponseVersion) then
  begin
    Cancel(ceMalformedStatusLine);
    exit;
  end;

  if (FResponseVersion > 11) then
  begin
    Cancel(ceVersionNotSupported);
    exit;
  end;

  { status code }
  Inc(lPos);
  if (lPos+3 >= pLineEnd) or (lPos[3] <> ' ') then
  begin
    Cancel(ceMalformedStatusLine);
    exit;
  end;
  FResponseStatus := CodeToHTTPStatus((ord(lPos[0])-ord('0'))*100
    + (ord(lPos[1])-ord('0'))*10 + (ord(lPos[2])-ord('0')));
  if FResponseStatus = hsUnknown then
  begin
    Cancel(ceMalformedStatusLine);
    exit;
  end;

  Inc(lPos, 4);
  if lPos < pLineEnd then
    FResponseReason := lPos;
end;

procedure TLHTTPClientSocket.ProcessHeaders;
begin
  if not ProcessEncoding then
    Cancel(ceUnsupportedEncoding);

  TLHTTPClient(FConnection).DoProcessHeaders(Self);
end;

procedure TLHTTPClientSocket.ResetDefaults;
begin
  inherited;

  FError := ceNone;
end;

{ TLHTTPClient }

constructor TLHTTPClient.Create(AOwner: TComponent);
begin
  FPort:=80;
  inherited;

  SocketClass := TLHTTPClientSocket;
end;

procedure TLHTTPClient.DoDoneInput(ASocket: TLHTTPClientSocket);
begin
  if Assigned(FOnDoneInput) then
    FOnDoneInput(ASocket);
end;

function  TLHTTPClient.DoHandleInput(ASocket: TLHTTPClientSocket; ABuffer: pchar; ASize: dword): dword;
begin
  if Assigned(FOnInput) then
    Result := FOnInput(ASocket, ABuffer, ASize)
  else
    Result := ASize;
end;

procedure TLHTTPClient.DoProcessHeaders(ASocket: TLHTTPClientSocket);
begin
  if Assigned(FOnProcessHeaders) then
    FOnProcessHeaders(ASocket);
end;

function  TLHTTPClient.DoWriteBlock(ASocket: TLHTTPClientSocket): TWriteBlockStatus;
begin
  Result := wsDone;
  if not FOutputEof then
    if Assigned(FOnCanWrite) then
      FOnCanWrite(ASocket, Result)
end;

function  TLHTTPClient.InitSocket(aSocket: TLSocket): TLSocket;
begin
  Result := inherited;
  TLHTTPClientSocket(aSocket).FConnection := Self;
  TLHTTPClientSocket(aSocket).FRequest := @FRequest;
end;

procedure TLHTTPClient.InternalSendRequest;
begin
  FOutputEof := false;
  TLHTTPClientSocket(FIterator).SendRequest;
end;

procedure TLHTTPClient.ConnectEvent(aSocket: TLHandle);
begin
  inherited;
  InternalSendRequest;
end;

procedure TLHTTPClient.SendRequest;
begin
  if not Connected then
    Connect(FHost, FPort)
  else
    InternalSendRequest;
end;

end.

