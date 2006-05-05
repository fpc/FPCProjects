unit httpsocket;

{$mode objfpc}{$h+}

interface

uses
  sysutils, strutils, lnet, mimetypes;

const
  HTTPBufferSize = 65535;
  FileBufferSize = 65535;

type
  THTTPRequest = (hrHead, hrGet, hrPost, hrUnknown);
  THTTPRequestParameter = (hpConnection, hpContentLength, hpAccept, 
    hpAcceptCharset, hpAcceptEncoding, hpAcceptLanguage, hpHost,
    hpFrom, hpReferer, hpUserAgent, hpRange);

const
  HTTPDateFormat: string = 'ddd, d mmm yyyy hh:nn:ss';
  HTTPRequestStrings: array[THTTPRequest] of string =
    ('HEAD', 'GET', 'POST', '');
  HTTPRequestParameterStrings: array[THTTPRequestParameter] of string =
    ('CONNECTION', 'CONTENT-LENGTH', 'ACCEPT', 
     'ACCEPT-CHARSET', 'ACCEPT-ENCODING', 'ACCEPT-LANGUAGE', 'HOST',
     'FROM', 'REFERER', 'USER-AGENT', 'RANGE');
  HTTPBadRequest: string =
    '<html><head><title>400 Bad Request</title></head><body>'+#10+
    '<h1>Bad Request</h1>'+#10+
    '<p>Your browser did a request this server did not understand.</p>'+#10+
    '</body></html>'+#10;
  HTTPForbidden: string =
    '<html><head><title>403 Forbidden</title></head><body>'+#10+
    '<h1>Forbidden</h1>'+#10+
    '<p>You do not have permission to access this resource.</p>'+#10+
    '</body></html>'+#10;
  HTTPNotFound: string =
    '<html><head><title>404 Not Found</title></head><body>'+#10+
    '<h1>Not Found</h1>'+#10+
    '<p>The requested URL was not found on this server.</p>'+#10+
    '</body></html>'+#10;

type
  THTTPBuffer = array[0..HTTPBufferSize-1] of char;
  TFileBuffer = array[0..FileBufferSize-1] of byte;

  TOutputItem = class(TObject)
  protected
    FBuffer: PByte;
    FBufferPos: integer;
    FBufferSize: integer;
    FNext: TOutputItem;
  public
    constructor Create;

    function WriteBlock(ASocket: TLSocket): boolean; virtual;

    property Next: TOutputItem read FNext write FNext;
  end;

  TFileOutput = class(TOutputItem)
  protected
    FFile: file;
    FOpen: boolean;
    FFileBuffer: TFileBuffer;

    function GetSize: integer;
  public
    constructor Create;
    destructor Destroy; override;

    function  Open(const AFileName: string): boolean;
    function  WriteBlock(ASocket: TLSocket): boolean; override;

    property Size: integer read GetSize;
  end;

  TMemoryOutput = class(TOutputItem)
  protected
    FFreeBuffer: boolean;
  public
    constructor Create(ABuffer: pointer; ABufferSize: integer;
      AFreeBuffer: boolean);
    destructor Destroy; override;
  end;

  THTTPSocket = class(TLSocket)
  protected
    FBuffer: THTTPBuffer;
    FBufferPos: pchar;
    FBufferEnd: pchar;
    FPendingData: boolean;
    FPendingOutput: boolean;
    FCurrentOutput: TOutputItem;
    FLastOutput: TOutputItem;
    FKeepAlive: boolean;

    { request }
    FRequestType: THTTPRequest;
    FArgument: pchar;
    FDocument: string;
    FStatusCode: integer;
    FQueryParams: pchar;
    FVersion: pchar;
    FRequestParameters: array[THTTPRequestParameter] of pchar;

    { response }
    FContentType: string;
    FContentCharset: string;
    FContentLength: integer;
    FLastModified: TDateTime;

    procedure AddToOutput(AOutputItem: TOutputItem);
    procedure AddStaticMsgToOutput(const AMessage: string);
    procedure AddDynamicMsgToOutput(const AMessage: pchar; ALength: integer);
    procedure LogMessage(const AMessage: string);
    procedure FlushRequest;
    procedure ResetDefaults;
    procedure HandleHeadRequest;
    procedure HandleGetRequest;
    procedure HandlePostRequest;
    function  ParseBuffer: boolean;
    procedure ParseLine;
    procedure ParseRequestLine;
    procedure ParseParameterLine;
    procedure WriteError(AStatusCode: integer);
    procedure WriteHeaders;
    procedure WriteStringResponse(const AMessage: string);
    procedure WriteBufferResponse(const AMessage: pchar; ALength: integer);
  public
    constructor Create(const sType, Protocol: Byte; Owner: TLConnection;
                           const aSocketNumber: Integer); override;
    destructor Destroy; override;

    procedure HandleReceive;
    procedure WriteBlock;
  end;
  
implementation

constructor TOutputItem.Create;
begin
  inherited;
end;

function TOutputItem.WriteBlock(ASocket: TLSocket): boolean;
var
  lRead: integer;
begin
  lRead := ASocket.Send(FBuffer[FBufferPos], FBufferSize - FBufferPos);
  Inc(FBufferPos, lRead);
  Result := FBufferPos = FBufferSize;
end;

constructor TFileOutput.Create;
begin
  inherited;
  FBuffer := @FFileBuffer[0];
  FBufferSize:=FileBufferSize;
end;

destructor TFileOutput.Destroy;
begin
  inherited;
  
  if FOpen then
    close(FFile);
end;

function TFileOutput.Open(const AFileName: string): boolean;
begin
  {$I-}
  Assign(FFile, AFileName);
  Reset(FFile,1);
  {$I+}
  Result := IOResult = 0;
  FBufferPos := 0;
  if Result then
    FOpen := true;
end;

function TFileOutput.GetSize: integer; inline;
begin
  Result := Integer(FileSize(FFile));
end;

function TFileOutput.WriteBlock(ASocket: TLSocket): boolean;
begin
  if FBufferPos = 0 then
  begin
    BlockRead(FFile, FFileBuffer, sizeof(FFileBuffer), FBufferSize);
    if FBufferSize = 0 then
    begin
      { EOF reached }
      Close(FFile);
      Result := true;
      FOpen := false;
      exit;
    end;
  end;
  
  Result := inherited;
  if Result then
  begin
    FBufferPos := 0;
    Result := false;
  end;
end;

constructor TMemoryOutput.Create(ABuffer: pointer; ABufferSize: integer;
      AFreeBuffer: boolean);
begin
  inherited Create;
  FBuffer := ABuffer;
  FBufferSize := ABufferSize;
  FFreeBuffer := AFreeBuffer;
end;

destructor TMemoryOutput.Destroy;
begin
  inherited;
  if FFreeBuffer then
    FreeMem(FBuffer);
end;

{ THTTPSocket }

constructor THTTPSocket.Create(const sType, Protocol: Byte; 
  Owner: TLConnection; const aSocketNumber: Integer);
begin
  inherited;

  FRequestType := hrUnknown;
  FBufferPos := @FBuffer[0];
  FBufferEnd := FBufferPos;
  FBuffer[0] := #0;
  FKeepAlive := true;
  ResetDefaults;
end;

destructor THTTPSocket.Destroy;
begin
  inherited;
end;

procedure THTTPSocket.ResetDefaults;
begin
  FContentType := 'application/octet-stream';
end;

procedure THTTPSocket.FlushRequest;
begin
  { request }
  FStatusCode := 0;
  FRequestType := hrUnknown;
  FArgument := nil;
  FQueryParams := nil;
  FVersion := nil;
  FillChar(FRequestParameters, sizeof(FRequestParameters), #0);
  { response }
  FContentLength := 0;
  FLastModified := 0.0;
  FContentCharset := '';
  ResetDefaults;
end;

procedure THTTPSocket.LogMessage(const AMessage: string);
begin
  writeln(PeerAddress, ' - - [', FormatDateTime('dd/mmm/yyyy:hh:nn:ss +0100', now), '] ', AMessage);
end;

procedure THTTPSocket.HandleReceive;
var
  lRead: integer;
begin
  if FBufferPos = @FBuffer[HTTPBufferSize] then
  begin
    Disconnect;
    exit;
  end;

  if FPendingOutput then 
  begin
    FPendingData := true;
    exit;
  end;
  
  FPendingData := false;
  lRead := Get(FBufferEnd^, @FBuffer[HTTPBufferSize]-FBufferEnd);
  if lRead = 0 then exit;
  Inc(FBufferEnd, lRead);
  FBufferEnd^ := #0;
  ParseBuffer;
end;

function THTTPSocket.ParseBuffer: boolean;
var
  pLineEnd: pchar;
begin
  if FPendingOutput then exit;
  repeat
    pLineEnd := StrScan(FBufferPos, #13);
    if pLineEnd = nil then
    begin
      if FBufferPos > @FBuffer[0] then
      begin
        { also copy null-terminator }
        Move(FBufferPos^, FBuffer, FBufferEnd-FBufferPos+1);
        FBufferEnd := @FBuffer[FBufferEnd-FBufferPos];
        FBufferPos := @FBuffer[0];
      end;
      Result := true;
      exit;
    end;
  
    pLineEnd^ := #0;
    ParseLine;
    FBufferPos := pLineEnd+1;
    if FBufferPos^ = #10 then 
      Inc(FBufferPos);
    if FPendingOutput then
    begin
      Result := false;
      exit;
    end;
  until false;
end;

procedure THTTPSocket.ParseRequestLine;
var
  lPos: pchar;
  I: THTTPRequest;
begin
  lPos := StrRScan(FBufferPos, ' ');
  if lPos = nil then
  begin
    Disconnect;
    exit;
  end;

  lPos^ := #0;
  FVersion := lPos+1;

  lPos := StrScan(FBufferPos, ' ');
  if lPos = nil then
  begin
    Disconnect;
    exit;
  end;

  lPos^ := #0;
  for I := Low(THTTPRequest) to High(THTTPRequest) do
  begin
    if I = hrUnknown then
    begin
      Disconnect;
      FRequestType := I;
      exit;
    end;
    
    if ((lPos-FBufferPos) = Length(HTTPRequestStrings[I]))
    and CompareMem(FBufferPos, PChar(HTTPRequestStrings[I]), lPos-FBufferPos) then
    begin
      FArgument := lPos+1;
      FRequestType := I;
      break;
    end;
  end;
end;

procedure THTTPSocket.ParseParameterLine;
var
  lPos: pchar;
  I: THTTPRequestParameter;
  lLen: integer;
begin
  lPos := StrScan(FBufferPos, ' ');
  if (lPos = nil) or (lPos = FBufferPos) or ((lPos-1)^ <> ':') then
  begin
    Disconnect;
    exit;
  end;

  { null-terminate at colon }
  (lPos-1)^ := #0;
  StrUpper(FBufferPos);
  lLen := lPos-FBufferPos-1;
  for I := Low(THTTPRequestParameter) to High(THTTPRequestParameter) do
    if (Length(HTTPRequestParameterStrings[I]) = lLen)
    and CompareMem(FBufferPos, PChar(HTTPRequestParameterStrings[I]), lLen) then
    begin
      FRequestParameters[I] := lPos+1;
      break;
    end;
end;

procedure THTTPSocket.ParseLine;
begin
  if FRequestType = hrUnknown then
  begin
    ParseRequestLine;
    exit;
  end;

  if FBufferPos[0] = #0 then
  begin
    case FRequestType of
      hrHead: HandleHeadRequest;
      hrGet: HandleGetRequest;
      hrPost: HandlePostRequest;
    end;
    FlushRequest;
  end else
    ParseParameterLine;
end;
        
procedure THTTPSocket.HandleHeadRequest;
begin
  
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

function DecodeUrl(AStr: pchar): string;
var
  lPos, lNext, lDest, lEnd: pchar;
begin
  lEnd := AStr + StrLen(AStr);
  SetLength(Result, lEnd-AStr);
  lDest := pchar(Result);
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
    Move(AStr^, lDest^, lPos-AStr);
    Inc(lDest, lPos-AStr);
    AStr := lNext;
  until lNext = nil;
  SetLength(Result, lDest-pchar(Result));
end;

function CheckPermission(const ADocument: pchar): boolean;
var
  lPos, lSepPos: pchar;
  lLevel: integer;
begin
  lPos := ADocument;
  lLevel := 0;
  Result := true;
  repeat
    lSepPos := StrScan(lPos, '/');
    if lSepPos = nil then break;
    if (((lSepPos-ADocument >= 3) and ((lSepPos-3)^ = '/')) or (lSepPos-ADocument = 3))
    and ((lSepPos-2)^ = '.') and ((lSepPos-1)^ = '.') then
    begin
      if lLevel = 0 then
      begin
        Result := false;
        exit;
      end else
        Dec(lLevel);
    end else
      Inc(lLevel);
    lPos := lSepPos+1;
  until false;
end;

procedure THTTPSocket.HandleGetRequest;
var
  lPos: pchar;
  lFileOutput: TFileOutput;
  lIndex: integer;
  lMessage: string;
begin
  lMessage := '"GET '+FArgument+' '+FVersion+'" ';
      
  lPos := StrScan(FArgument, '?');
  if lPos <> nil then
  begin
    lPos^ := #0;
    FQueryParams := lPos+1;
  end;

  if FArgument[0] = '/' then
    Inc(FArgument);
  if not CheckPermission(FArgument) then
  begin
    WriteError(403);
  end else begin
    FDocument := ExpandFileName(DecodeUrl(FArgument));
    lFileOutput := TFileOutput.Create;
    if lFileOutput.Open(FDocument) then
    begin
      FStatusCode := 200;
      FContentLength := lFileOutput.Size;
      FLastModified := FileDateToDateTime(FileAge(FDocument));
      lIndex := MimeList.IndexOf(Copy(ExtractFileExt(FDocument), 2, 255));
      if lIndex >= 0 then
        FContentType := TStringObject(MimeList.Objects[lIndex]).Str;
      WriteHeaders;
      AddToOutput(lFileOutput);
    end else begin
      WriteError(404);
      lFileOutput.Free;
    end;
  end;

  LogMessage(lMessage+IntToStr(FStatusCode)+' '+IntToStr(FContentLength)
    +' "'+FRequestParameters[hpReferer]+'" "'+FRequestParameters[hpUserAgent]+'"');
end;

procedure THTTPSocket.HandlePostRequest;
begin
  
end;

procedure THTTPSocket.WriteBlock;
var
  lFreeOutput: TOutputItem;
begin
  if FCurrentOutput <> nil then
  begin
    if FCurrentOutput.WriteBlock(Self) then
    begin
      lFreeOutput := FCurrentOutput;
      FCurrentOutput := FCurrentOutput.Next;
      if lFreeOutput = FLastOutput then
        FLastOutput := nil;
      lFreeOutput.Free;
      if FCurrentOutput = nil then
        FPendingOutput := false;
    end;
  end else 
  if ParseBuffer and FPendingData then 
  begin
    { end of input buffer reached, try reading more }
    HandleReceive;  
  end else 
  if not FKeepAlive then
    Disconnect;
end;

procedure THTTPSocket.WriteError(AStatusCode: integer);
begin
  FStatusCode := AStatusCode;
  FKeepAlive := false;
  FContentType := 'text/html';
  case AStatusCode of
    403: WriteStringResponse(HTTPForbidden);
    404: WriteStringResponse(HTTPNotFound);
  end;
end;

function StatusCodeToText(StatusCode: integer): pchar;
begin
  case StatusCode of
    200: Result := 'OK';
    400: Result := 'Bad Request';
    403: Result := 'Forbidden';
    404: Result := 'Not Found';
  else
    Result := '';
  end;
end;
      
procedure THTTPSocket.WriteHeaders;
var
  lTemp: string;
  lMessage, lStr: pchar;
begin
  GetMem(lMessage, 500);
  
  lStr := StrECopy(lMessage, 'HTTP/1.0 ');
  lTemp := IntToStr(FStatusCode);
  lStr := StrECopy(lStr, PChar(lTemp));
  lStr^ := ' '; Inc(lStr);
  lStr := StrECopy(lStr, StatusCodeToText(FStatusCode));
  lStr := StrECopy(lStr, #13#10+'Date: ');
  lTemp := FormatDateTime(HTTPDateFormat, now);
  lStr := StrECopy(lStr, PChar(lTemp));
  lStr := StrECopy(lStr, ' CET'+#13#10+'Content-Length: ');
  lTemp := IntToStr(FContentLength);
  lStr := StrECopy(lStr, PChar(lTemp));
  if FLastModified <> 0.0 then
  begin
    lTemp := FormatDateTime(HTTPDateFormat, FLastModified);
    lStr := StrECopy(lStr, #13#10+'Last-Modified: ');
    lStr := StrECopy(lStr, PChar(lTemp));
  end;
  lStr := StrECopy(lStr, #13#10+'Connection: ');
  if FKeepAlive then
    lStr := StrECopy(lStr, 'keep-alive')
  else
    lStr := StrECopy(lStr, 'close');
  lStr := StrECopy(lStr, #13#10+'Content-Type: ');
  lStr := StrECopy(lStr, PChar(FContentType));
  if Length(FContentCharset) > 0 then
  begin
    lStr := StrECopy(lStr, '; charset=');
    lStr := StrECopy(lStr, PChar(FContentCharset));
  end;
  lStr := StrECopy(lStr, #13#10#13#10);
  
  AddDynamicMsgToOutput(lMessage, lStr-lMessage);
end;

procedure THTTPSocket.AddStaticMsgToOutput(const AMessage: string);
var
  lMemOutput: TMemoryOutput;
begin
  lMemOutput := TMemoryOutput.Create(PChar(AMessage), Length(AMessage), false);
  AddToOutput(lMemOutput);
end;

procedure THTTPSocket.AddDynamicMsgToOutput(const AMessage: pchar; ALength: integer);
var
  lMemOutput: TMemoryOutput;
begin
  lMemOutput := TMemoryOutput.Create(AMessage, ALength, true);
  AddToOutput(lMemOutput);
end;

procedure THTTPSocket.WriteStringResponse(const AMessage: string);
begin
  FContentLength := Length(AMessage);
  WriteHeaders;
  AddStaticMsgToOutput(AMessage);
end;

procedure THTTPSocket.WriteBufferResponse(const AMessage: pchar; ALength: integer);
begin
  FContentLength := ALength;
  WriteHeaders;
  AddDynamicMsgToOutput(AMessage, ALength);
end;
      
procedure THTTPSocket.AddToOutput(AOutputItem: TOutputItem);
begin
  if FLastOutput <> nil then
  begin
    FLastOutput.Next := AOutputItem;
  end else begin
    FCurrentOutput := AOutputItem;
  end;
  FLastOutput := AOutputItem;
  FPendingOutput := true;
  if FCurrentOutput = AOutputItem then
    WriteBlock;
end;

end.

