{ Web server component, built on the HTTP server component

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

unit lwebserver;

{$mode objfpc}{$h+}
{$inline on}

interface

uses
  sysutils, classes, lnet, lhttp, lhttputil, lmimetypes, levents, 
  lprocess, process, lfastcgi, fastcgi, lHTTPSettings;

type
  TFileOutput = class(TBufferOutput)
  protected
    FFile: file;

    function GetSize: integer;
    function WriteData: TWriteBlockStatus; override;
  public
    constructor Create(ASocket: TLHTTPSocket);
    destructor Destroy; override;

    function  Open(const AFileName: string): boolean;

    property Size: integer read GetSize;
  end;

  TCGIOutput = class(TBufferOutput)
  protected
    FParsePos: pchar;
    FReadPos: integer;
    FParsingHeaders: boolean;
    FExtraPath: string;
    FScriptFileName: string;
    FScriptName: string;
   
    procedure AddEnvironment(const AName, AValue: string); virtual; abstract;
    procedure AddHTTPParam(const AName: string; AParam: TLHTTPParameter);
    function  ParseHeaders: boolean;
    procedure CGIOutputError; virtual; abstract;
    procedure WriteCGIBlock;
    function  WriteCGIData: TWriteBlockStatus; virtual; abstract;
  public
    constructor Create(ASocket: TLHTTPSocket);
    destructor Destroy; override;

    function  WriteData: TWriteBlockStatus; override;
    procedure StartRequest; virtual;
    
    property ExtraPath: string read FExtraPath write FExtraPath;
    property ScriptFileName: string read FScriptFileName write FScriptFileName;
    property ScriptName: string read FScriptName write FScriptName;
  end;

  TSimpleCGIOutput = class(TCGIOutput)
  protected
    FProcess: TLProcess;

    procedure AddEnvironment(const AName, AValue: string); override;
    procedure CGIProcNeedInput(AHandle: TLHandle);
    procedure CGIProcHasOutput(AHandle: TLHandle);
    procedure CGIProcHasStderr(AHandle: TLHandle);
    procedure DoneInput; override;
    function  HandleInput(ABuffer: pchar; ASize: dword): dword; override;
    procedure CGIOutputError; override;
    function  WriteCGIData: TWriteBlockStatus; override;
  public
    constructor Create(ASocket: TLHTTPSocket);
    destructor Destroy; override;

    procedure  StartRequest; override;

    property Process: TLProcess read FProcess;
  end;

  TFastCGIOutput = class(TCGIOutput)
  protected
    FRequest: TLFastCGIRequest;

    procedure AddEnvironment(const AName, AValue: string); override;
    procedure CGIOutputError; override;
    procedure DoneInput; override;
    procedure RequestEnd(ARequest: TLFastCGIRequest);
    procedure RequestNeedInput(ARequest: TLFastCGIRequest);
    procedure RequestHasOutput(ARequest: TLFastCGIRequest);
    procedure RequestHasStderr(ARequest: TLFastCGIRequest);
    function  HandleInput(ABuffer: pchar; ASize: dword): dword; override;
    function  WriteCGIData: TWriteBlockStatus; override;
    function  WriteBlock: TWriteBlockStatus; override;
  public
    constructor Create(ASocket: TLHTTPSocket);
    destructor Destroy; override;

    procedure StartRequest; override;

    property Request: TLFastCGIRequest read FRequest write FRequest;
  end;

  TCGIHandler = class(TURIHandler)
  public
    function HandleURI(ASocket: TLHTTPServerSocket): TOutputItem; override;
  end;

  TDocumentRequest = record
    Socket: TLHTTPServerSocket;
    Document: string;
    URIPath: string;
    ExtraPath: string;
  end;

  TDocumentHandler = class(TObject)
  private
    FNext: TDocumentHandler;
  public
    function HandleDocument(const ARequest: TDocumentRequest): TOutputItem; virtual; abstract;
  end;

  TFileHandler = class(TURIHandler)
  protected
    FDocHandlerList: TDocumentHandler;
    FDirIndexList: TStrings;
  public
    constructor Create;
    destructor Destroy; override;
    
    function HandleFile(const ARequest: TDocumentRequest): TOutputItem;
    function HandleURI(ASocket: TLHTTPServerSocket): TOutputItem; override;
    procedure RegisterHandler(AHandler: TDocumentHandler);

    property DirIndexList: TStrings read FDirIndexList;
  end;

  TPHPCGIHandler = class(TDocumentHandler)
  public
    function HandleDocument(const ARequest: TDocumentRequest): TOutputItem; override;
  end;

  TPHPFastCGIHandler = class(TDocumentHandler)
  protected
    FPool: TLFastCGIPool;
  public
    constructor Create;
    destructor Destroy; override;

    function HandleDocument(const ARequest: TDocumentRequest): TOutputItem; override;

    property Pool: TLFastCGIPool read FPool;
  end;

  TLWebServer = class(TLHTTPServer)
  protected
    FFileHandler: TFileHandler;
    FCGIHandler: TCGIHandler;
    FPHPCGIHandler: TPHPFastCGIHandler;
    FPHPCGIAppName: string;
    FPHPCGIHost: string;
    FPHPCGIPort: integer;

    procedure RegisterWithEventer; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LogAccess(const AMessage: string); override;
    procedure LogError(const AMessage: string; ASocket: TLSocket);

    property PHPCGIAppName: string read FPHPCGIAppName write FPHPCGIAppName;
    property PHPCGIHost: string read FPHPCGIHost write FPHPCGIHost;
    property PHPCGIPort: integer read FPHPCGIPort write FPHPCGIPort;
    property PHPCGIHandler: TPHPFastCGIHandler read FPHPCGIHandler;
  end;

var
  EnableWriteln: Boolean = True;

implementation

{ Example handlers }

const
  ServerSoftware = 'fpHTTPd/0.2';

  InputBufferEmptyToWriteStatus: array[boolean] of TWriteBlockStatus =
    (wsPendingData, wsWaitingData);
  
var
  ScriptPathPrefix: string;
  DocumentRoot: string;
  CGIPath: string;
  CGIRoot: string;
  PHPCGIBinary: string;
  
procedure InternalWrite(const s: string);
begin
  if EnableWriteln then
    Writeln(s);
end;

function TCGIHandler.HandleURI(ASocket: TLHTTPServerSocket): TOutputItem;
var
  lOutput: TSimpleCGIOutput;
  lExecPath: string;
begin
  if StrLComp(ASocket.RequestInfo.Argument, PChar(ScriptPathPrefix),
      Length(ScriptPathPrefix)) = 0 then
  begin
    lOutput := TSimpleCGIOutput.Create(ASocket);
    lOutput.Process.CurrentDirectory := CGIRoot;
    lExecPath := CGIRoot+(ASocket.RequestInfo.Argument+Length(ScriptPathPrefix));
    if SeparatePath(lExecPath, lOutput.ExtraPath) then
    begin
      lOutput.Process.CommandLine := lExecPath;
      lOutput.ScriptFileName := lExecPath;
      lOutput.ScriptName := Copy(lExecPath, Length(CGIRoot), 
        Length(lExecPath)-Length(CGIRoot)+1);
      lOutput.StartRequest;
    end else
      ASocket.ResponseInfo.Status := hsNotFound;
    Result := lOutput;
  end else
    Result := nil;
end;

constructor TFileHandler.Create;
begin
  inherited;

  FDirIndexList := TStringList.Create;
end;

destructor TFileHandler.Destroy;
begin
  FreeAndNil(FDirIndexList);

  inherited;
end;

function TFileHandler.HandleFile(const ARequest: TDocumentRequest): TOutputItem;
var
  lFileOutput: TFileOutput;
  lReqInfo: PRequestInfo;
  lRespInfo: PResponseInfo;
  lHeaderOut: PHeaderOutInfo;
  lIndex: integer;
  lInfo: TSearchRec;
begin
  if Length(ARequest.ExtraPath) = 0 then
  begin
    lReqInfo := @ARequest.Socket.RequestInfo;
    lRespInfo := @ARequest.Socket.ResponseInfo;
    lHeaderOut := @ARequest.Socket.HeaderOut;
    if not (lReqInfo^.RequestType in [hmHead, hmGet]) then
    begin
      lRespInfo^.Status := hsNotAllowed;
    end else begin
      lFileOutput := TFileOutput.Create(ARequest.Socket);
      FindFirst(ARequest.Document, 0, lInfo);
      if lFileOutput.Open(ARequest.Document) then
      begin
        lRespInfo^.Status := hsOK;
        lHeaderOut^.ContentLength := lInfo.Size;
        lRespInfo^.LastModified := LocalTimeToGMT(FileDateToDateTime(lInfo.Time));
        lIndex := MimeList.IndexOf(ExtractFileExt(ARequest.Document));
        if lIndex >= 0 then
          lRespInfo^.ContentType := TStringObject(MimeList.Objects[lIndex]).Str;
        Result := lFileOutput;
        ARequest.Socket.StartResponse(lFileOutput);
      end else
        lFileOutput.Free;
      FindClose(lInfo);
    end;
  end;
end;

function TFileHandler.HandleURI(ASocket: TLHTTPServerSocket): TOutputItem;
var
  lDocRequest: TDocumentRequest;
  lHandler: TDocumentHandler;
  lTempDoc: string;
  lDirIndexFound: boolean;
  I: integer;
begin
  Result := nil;
  lDocRequest.Socket := ASocket;
  lDocRequest.URIPath := ASocket.RequestInfo.Argument;
  lDocRequest.Document := DocumentRoot+lDocRequest.URIPath;
  if DirectoryExists(lDocRequest.Document) then
  begin
    lDocRequest.Document := IncludeTrailingPathDelimiter(lDocRequest.Document);
    lDirIndexFound := false;
    for I := 0 to FDirIndexList.Count - 1 do
    begin
      lTempDoc := lDocRequest.Document + FDirIndexList.Strings[I];
      if FileExists(lTempDoc) then
      begin
        lDocRequest.Document := lTempDoc;
        lDirIndexFound := true;
        break;
      end;
    end;
    { requested a directory, but no source to show }
    if not lDirIndexFound then exit;
  end else
  if not SeparatePath(lDocRequest.Document, lDocRequest.ExtraPath) then exit;
  lHandler := FDocHandlerList;
  while lHandler <> nil do
  begin
    Result := lHandler.HandleDocument(lDocRequest);
    if Result <> nil then exit;
    if ASocket.ResponseInfo.Status <> hsOK then exit;
    lHandler := lHandler.FNext;
  end;

  { no dynamic handler, see if it's a plain file }
  Result := HandleFile(lDocRequest);
end;

procedure TFileHandler.RegisterHandler(AHandler: TDocumentHandler);
begin
  if AHandler = nil then exit;
  AHandler.FNext := FDocHandlerList;
  FDocHandlerList := AHandler;
end;

function TPHPCGIHandler.HandleDocument(const ARequest: TDocumentRequest): TOutputItem;
var
  lOutput: TSimpleCGIOutput;
begin
  if ExtractFileExt(ARequest.Document) = '.php' then
  begin
    lOutput := TSimpleCGIOutput.Create(ARequest.Socket);
    lOutput.Process.CommandLine := PHPCGIBinary;
    lOutput.ScriptName := ARequest.URIPath;
    lOutput.ScriptFileName := ARequest.Document;
    lOutput.ExtraPath := ARequest.ExtraPath;
    lOutput.StartRequest;
    Result := lOutput;
  end else
    Result := nil;
end;

constructor TPHPFastCGIHandler.Create;
begin
  inherited;
  FPool := TLFastCGIPool.Create;
end;

destructor TPHPFastCGIHandler.Destroy;
begin
  inherited;
  FPool.Free;
end;

function TPHPFastCGIHandler.HandleDocument(const ARequest: TDocumentRequest): TOutputItem;
var
  lOutput: TFastCGIOutput;
  fcgiRequest: TLFastCGIRequest;
begin
  if ExtractFileExt(ARequest.Document) = '.php' then
  begin
    fcgiRequest := FPool.BeginRequest(FCGI_RESPONDER);
    if fcgiRequest <> nil then
    begin
      lOutput := TFastCGIOutput.Create(ARequest.Socket);
      lOutput.ScriptName := ARequest.URIPath;
      lOutput.ScriptFileName := ARequest.Document;
      lOutput.ExtraPath := ARequest.ExtraPath;
      lOutput.Request := fcgiRequest;
      lOutput.StartRequest;
      Result := lOutput;
    end else begin
      ARequest.Socket.ResponseInfo.Status := hsInternalError;
      ARequest.Socket.StartResponse(nil);
      Result := nil;
    end;
  end else
    Result := nil;
end;

{ Output Items }

constructor TFileOutput.Create(ASocket: TLHTTPSocket);
begin
  inherited;
  FEof := true;
end;

destructor TFileOutput.Destroy;
begin
  inherited;
  
  if not FEof then
    Close(FFile);
end;

function TFileOutput.Open(const AFileName: string): boolean;
begin
  {$I-}
  FileMode := 0;
  Assign(FFile, AFileName);
  Reset(FFile,1);
  {$I+}
  Result := IOResult = 0;
  FEof := false;
end;

function TFileOutput.GetSize: integer; inline;
begin
  Result := FileSize(FFile);
end;

function TFileOutput.WriteData: TWriteBlockStatus;
var
  lRead: integer;
begin
  if FEof then 
    exit(wsDone);
  BlockRead(FFile, FBuffer[FBufferPos], FBufferSize-FBufferPos, lRead);
  Inc(FBufferPos, lRead);
  if lRead = 0 then
  begin
    { EOF reached }
    Close(FFile);
    exit(wsDone);
  end;
  exit(wsPendingData);
end;

constructor TCGIOutput.Create(ASocket: TLHTTPSocket);
begin
  inherited;
end;

destructor TCGIOutput.Destroy;
begin
  inherited;
end;

procedure TCGIOutput.AddHTTPParam(const AName: string; AParam: TLHTTPParameter);
var
  lValue: pchar;
begin
  lValue := FSocket.Parameters[AParam];
  if lValue = nil then exit;
  AddEnvironment(AName, lValue);
end;

procedure TCGIOutput.StartRequest;
var
  lServerSocket: TLHTTPServerSocket absolute FSocket;
begin
{
  FProcess.Environment.Add('SERVER_ADDR=');
  FProcess.Environment.Add('SERVER_ADMIN=');
  FProcess.Environment.Add('SERVER_NAME=');
  FProcess.Environment.Add('SERVER_PORT=');
}
  AddEnvironment('SERVER_SOFTWARE', ServerSoftware);

  AddEnvironment('GATEWAY_INTERFACE', 'CGI/1.1'); 
  AddEnvironment('SERVER_PROTOCOL', lServerSocket.RequestInfo.VersionStr);
  AddEnvironment('REQUEST_METHOD', lServerSocket.RequestInfo.Method);
  AddEnvironment('REQUEST_URI', '/'+lServerSocket.RequestInfo.Argument);

  if Length(FExtraPath) > 0 then
  begin
    AddEnvironment('PATH_INFO', FExtraPath);
    { do not set PATH_TRANSLATED: bug in PHP }
//    AddEnvironment('PATH_TRANSLATED', DocumentRoot+FExtraPath);
  end;

  AddEnvironment('SCRIPT_NAME', FScriptName);
  AddEnvironment('SCRIPT_FILENAME', FScriptFileName);
  
  AddEnvironment('QUERY_STRING', lServerSocket.RequestInfo.QueryParams);
  AddHTTPParam('CONTENT_TYPE', hpContentType);
  AddHTTPParam('CONTENT_LENGTH', hpContentLength);

  AddEnvironment('REMOTE_ADDR', FSocket.PeerAddress);
  AddEnvironment('REMOTE_PORT', IntToStr(FSocket.LocalPort));

  { used when user has authenticated in some way to server }
//  AddEnvironment('AUTH_TYPE='+...);
//  AddEnvironment('REMOTE_USER='+...);
  
  AddEnvironment('DOCUMENT_ROOT', DocumentRoot);
  AddEnvironment('REDIRECT_STATUS', '200');
  AddHTTPParam('HTTP_HOST', hpHost);
  AddHTTPParam('HTTP_COOKIE', hpCookie);
  AddHTTPParam('HTTP_CONNECTION', hpConnection);
  AddHTTPParam('HTTP_REFERER', hpReferer);
  AddHTTPParam('HTTP_USER_AGENT', hpUserAgent);
  AddHTTPParam('HTTP_ACCEPT', hpAccept);
  AddEnvironment('PATH', CGIPath);

  FParsingHeaders := true;
  FReadPos := FBufferPos;
  FParsePos := FBuffer+FReadPos;
end;

function  TCGIOutput.ParseHeaders: boolean;
var
  lHttpStatus: TLHTTPStatus;
  iEnd, lCode: integer;
  lStatus, lLength: dword;
  pLineEnd, pNextLine, pValue: pchar;
  lServerSocket: TLHTTPServerSocket absolute FSocket;

  procedure AddExtraHeader;
  begin
    lServerSocket.HeaderOut.ExtraHeaders := lServerSocket.HeaderOut.ExtraHeaders +
      FParsePos + ': ' + pValue + #13#10;
  end;

begin
  repeat
    iEnd := IndexByte(FParsePos^, @FBuffer[FReadPos]-FParsePos, 10);
    if iEnd = -1 then exit(false);
    pNextLine := FParsePos+iEnd+1;
    if (iEnd > 0) and (FParsePos[iEnd-1] = #13) then
      dec(iEnd);
    pLineEnd := FParsePos+iEnd;
    pLineEnd^ := #0;
    if pLineEnd = FParsePos then
    begin
      { empty line signals end of headers }
      FParsingHeaders := false;
      FBufferOffset := pNextLine-FBuffer;
      FBufferPos := FReadPos;
      FReadPos := 0;
      lServerSocket.StartResponse(Self);
      exit(false);
    end;
    iEnd := IndexByte(FParsePos^, iEnd, ord(':'));
    if (iEnd = -1) or (FParsePos[iEnd+1] <> ' ') then
      break;
    FParsePos[iEnd] := #0;
    pValue := FParsePos+iEnd+2;
    if StrIComp(FParsePos, 'Content-type') = 0 then
    begin
      lServerSocket.ResponseInfo.ContentType := pValue;
    end else 
    if StrIComp(FParsePos, 'Location') = 0 then
    begin
      if StrLIComp(pValue, 'http://', 7) = 0 then
      begin
        lServerSocket.ResponseInfo.Status := hsMovedPermanently;
        { add location header as-is to response }
        AddExtraHeader;
      end else
        InternalWrite('WARNING: unimplemented ''Location'' response received from CGI script');
    end else 
    if StrIComp(FParsePos, 'Status') = 0 then
    begin
      Val(pValue, lStatus, lCode);
      if lCode <> 0 then
        break;
      for lHttpStatus := Low(TLHTTPStatus) to High(TLHTTPStatus) do
        if HTTPStatusCodes[lHttpStatus] = lStatus then
          lServerSocket.ResponseInfo.Status := lHttpStatus;
    end else
    if StrIComp(FParsePos, 'Content-Length') = 0 then
    begin
      Val(pValue, lLength, lCode);
      if lCode <> 0 then
        break;
      lServerSocket.HeaderOut.ContentLength := lLength;
    end else
    if StrIComp(FParsePos, 'Last-Modified') = 0 then
    begin
      if not TryHTTPDateStrToDateTime(pValue, 
          lServerSocket.ResponseInfo.LastModified) then
        InternalWrite('WARNING: unable to parse last-modified string from CGI script: ' + pValue);
    end else
      AddExtraHeader;
    FParsePos := pNextLine;
  until false;

  { error happened }
  lServerSocket.ResponseInfo.Status := hsInternalError;
  exit(true);
end;

function TCGIOutput.WriteData: TWriteBlockStatus;
begin
  if not FParsingHeaders then
    FReadPos := FBufferPos;
  Result := WriteCGIData;
  if FParsingHeaders then
  begin
    if ParseHeaders then
    begin
      { error while parsing }
      FEof := true;
      exit(wsDone);
    end;
  end else
    FBufferPos := FReadPos;
end;

procedure TCGIOutput.WriteCGIBlock;
begin
  { CGI process has output pending, we can write a block to socket }
  if FParsingHeaders then
  begin
    if (WriteData = wsDone) and FParsingHeaders then
    begin
      { still parsing headers ? something's wrong }
      FParsingHeaders := false;
      CGIOutputError;
      TLHTTPServerSocket(FSocket).StartResponse(Self);
    end;
  end;
  if not FParsingHeaders then
    FSocket.WriteBlock;
end;

{ TSimpleCGIOutput }

constructor TSimpleCGIOutput.Create(ASocket: TLHTTPSocket);
begin
  inherited;
  FProcess := TLProcess.Create(nil);
  FProcess.Options := FProcess.Options + [poUsePipes];
  FProcess.OnNeedInput := @CGIProcNeedInput;
  FProcess.OnHasOutput := @CGIProcHasOutput;
  FProcess.OnHasStderr := @CGIProcHasStderr;
end;

destructor TSimpleCGIOutput.Destroy;
begin
  inherited;
  FProcess.Free;
end;

function TSimpleCGIOutput.WriteCGIData: TWriteBlockStatus;
var
  lRead: integer;
begin
  lRead := FProcess.Output.Read(FBuffer[FReadPos], FBufferSize-FReadPos);
  if lRead = 0 then exit(wsDone);
  Inc(FReadPos, lRead);
  Result := InputBufferEmptyToWriteStatus[lRead = 0];
end;

procedure TSimpleCGIOutput.AddEnvironment(const AName, AValue: string);
begin
  FProcess.Environment.Add(AName+'='+AValue);
end;

procedure TSimpleCGIOutput.DoneInput;
begin
  FProcess.CloseInput;
end;

function TSimpleCGIOutput.HandleInput(ABuffer: pchar; ASize: dword): dword;
begin
  if ASize > 0 then
    Result := FProcess.Input.Write(ABuffer^, ASize)
  else
    Result := 0;
  FProcess.InputEvent.IgnoreWrite := ASize = 0;
end;

procedure TSimpleCGIOutput.StartRequest;
begin
  inherited;
  
  FProcess.Eventer := FSocket.Connection.Eventer;
  FProcess.Execute;
end;

procedure TSimpleCGIOutput.CGIOutputError;
var
  ServerSocket: TLHTTPServerSocket absolute FSocket;
begin
  if FProcess.ExitStatus = 127 then
    ServerSocket.ResponseInfo.Status := hsNotFound
  else
    ServerSocket.ResponseInfo.Status := hsInternalError;
end;

procedure TSimpleCGIOutput.CGIProcNeedInput(AHandle: TLHandle);
begin
  FProcess.InputEvent.IgnoreWrite := true;
  FSocket.ParseBuffer;
end;

procedure TSimpleCGIOutput.CGIProcHasOutput(AHandle: TLHandle);
begin
  WriteCGIBlock;
end;

procedure TSimpleCGIOutput.CGIProcHasStderr(AHandle: TLHandle);
var
  lBuf: array[0..1023] of char;
  lRead: integer;
begin
  lRead := FProcess.Stderr.Read(lBuf, sizeof(lBuf)-1);
  lBuf[lRead] := #0;
  write(pchar(@lBuf[0]));
end;

{ TFastCGIOutput }

constructor TFastCGIOutput.Create(ASocket: TLHTTPSocket);
begin
  inherited;
end;

destructor TFastCGIOutput.Destroy;
begin
  if FRequest <> nil then
  begin
    FRequest.OnInput := nil;
    FRequest.OnOutput := nil;
    FRequest.OnStderr := nil;
    FRequest.OnEndRequest := nil;
    FRequest.AbortRequest;
  end;
  inherited;
end;

procedure TFastCGIOutput.AddEnvironment(const AName, AValue: string);
begin
  FRequest.SendParam(AName, AValue);
end;

procedure TFastCGIOutput.CGIOutputError;
begin
  TLHTTPServerSocket(FSocket).ResponseInfo.Status := hsInternalError;
end;

procedure TFastCGIOutput.DoneInput;
begin
  if FRequest <> nil then
    FRequest.DoneInput;
end;

procedure TFastCGIOutput.RequestEnd(ARequest: TLFastCGIRequest);
begin
  FRequest.OnEndRequest := nil;
  FRequest.OnInput := nil;
  FRequest.OnOutput := nil;
  FRequest := nil;
  { trigger final write, to flush output to socket }
  WriteCGIBlock;
end;

procedure TFastCGIOutput.RequestNeedInput(ARequest: TLFastCGIRequest);
begin
  FSocket.ParseBuffer;
end;

procedure TFastCGIOutput.RequestHasOutput(ARequest: TLFastCGIRequest);
begin
  WriteCGIBlock;
end;

procedure TFastCGIOutput.RequestHasStderr(ARequest: TLFastCGIRequest);
var
  lBuf: array[0..1023] of char;
  lRead: integer;
begin
  lRead := ARequest.Get(lBuf, sizeof(lBuf)-1);
  lBuf[lRead] := #0;
  write(pchar(@lBuf[0]));
end;

function  TFastCGIOutput.HandleInput(ABuffer: pchar; ASize: dword): dword;
begin
  Result := FRequest.SendInput(ABuffer, ASize);
end;

function  TFastCGIOutput.WriteCGIData: TWriteBlockStatus;
var
  lRead: integer;
begin
  if FRequest = nil then exit(wsDone);
  if FRequest.OutputDone then exit(wsDone);
  lRead := FRequest.Get(@FBuffer[FReadPos], FBufferSize-FReadPos);
  Inc(FReadPos, lRead);
  Result := InputBufferEmptyToWriteStatus[lRead = 0];
end;

function  TFastCGIOutput.WriteBlock: TWriteBlockStatus;
begin
  if (FRequest <> nil) and FRequest.OutputPending then
  begin
    FRequest.ParseClientBuffer;
    Result := wsWaitingData;
  end else
    Result := inherited;
end;

procedure TFastCGIOutput.StartRequest;
begin
  FRequest.OnEndRequest := @RequestEnd;
  FRequest.OnInput := @RequestNeedInput;
  FRequest.OnOutput := @RequestHasOutput;
  FRequest.OnStderr := @RequestHasStderr;
  inherited;
  FRequest.DoneParams;
end;

{ TLWebServer }

constructor TLWebServer.Create(AOwner: TComponent);
begin
  inherited;

  FPHPCGIHost := 'localhost';
  FPHPCGIPort := GetPHPCGIPort;
  FPHPCGIAppName := PHPCGIBinary;

  FFileHandler := TFileHandler.Create;
  FCGIHandler := TCGIHandler.Create;
  FPHPCGIHandler := TPHPFastCGIHandler.Create;
  FOnError := @LogError;

  RegisterHandler(FFileHandler);
  RegisterHandler(FCGIHandler);
  FFileHandler.DirIndexList.Add('index.html');
  FFileHandler.DirIndexList.Add('index.htm');
  FFileHandler.DirIndexList.Add('index.php');
  FFileHandler.DirIndexList.Add('index.cgi');
  FFileHandler.RegisterHandler(FPHPCGIHandler);
end;

destructor TLWebServer.Destroy;
begin
  inherited;

  FFileHandler.Free;
  FCGIHandler.Free;
  FPHPCGIHandler.Free;
end;

procedure TLWebServer.RegisterWithEventer;
begin
  inherited;
  FPHPCGIHandler.Pool.Eventer := Eventer;
  FPHPCGIHandler.Pool.AppName := FPHPCGIAppName;
  FPHPCGIHandler.Pool.Host := FPHPCGIHost;
  FPHPCGIHandler.Pool.Port := FPHPCGIPort;
end;

procedure TLWebServer.LogAccess(const AMessage: string);
begin
  InternalWrite(AMessage);
end;

procedure TLWebServer.LogError(const AMessage: string; ASocket: TLSocket);
begin
  InternalWrite(AMessage);
end;

initialization
  ScriptPathPrefix := GetScriptPathPrefix;
  DocumentRoot := GetHTTPPath;
  CGIPath := GetCGIPath;
  CGIRoot := GetCGIRoot;
  PHPCGIBinary := GetPHPCGIBinary;

end.
