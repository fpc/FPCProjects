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

interface

uses
  sysutils, classes, lnet, lhttpserver, lhttputil, lmimetypes, levents, 
  lprocess, process;

type
  TFileOutput = class(TBufferOutput)
  protected
    FFile: file;

    function GetSize: integer;
    function WriteData: boolean; override;
  public
    constructor Create;
    destructor Destroy; override;

    function  Open(const AFileName: string): boolean;

    property Size: integer read GetSize;
  end;

  TCGIOutput = class(TBufferOutput)
  protected
    FProcess: TLProcess;
    FParsePos: pchar;
    FReadPos: integer;
    FParsingHeaders: boolean;
    FExtraPath: string;
    FScriptFileName: string;
    FScriptName: string;
    
    procedure AddHTTPParam(const AName: string; AParam: TLHTTPRequestParameter);
    procedure CGIProcNeedInput(AHandle: TLHandle);
    procedure CGIProcHasOutput(AHandle: TLHandle);
    procedure CGIProcHasStderr(AHandle: TLHandle);
    function  ParseHeaders: boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function  HandleInput(ABuffer: pchar; ASize: dword): dword; override;
    function  WriteData: boolean; override;
    procedure ExecuteProcess;
    
    property ExtraPath: string read FExtraPath write FExtraPath;
    property Process: TLProcess read FProcess;
    property ScriptFileName: string read FScriptFileName write FScriptFileName;
    property ScriptName: string read FScriptName write FScriptName;
  end;

  TCGIHandler = class(TURIHandler)
  public
    function HandleURI(ASocket: TLHTTPSocket): TOutputItem; override;
  end;

  TDocumentRequest = record
    Socket: TLHTTPSocket;
    Document: string;
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
  public
    function HandleFile(const ARequest: TDocumentRequest): TOutputItem;
    function HandleURI(ASocket: TLHTTPSocket): TOutputItem; override;
    procedure RegisterHandler(AHandler: TDocumentHandler);
  end;

  TPHPCGIHandler = class(TDocumentHandler)
  public
    function HandleDocument(const ARequest: TDocumentRequest): TOutputItem; override;
  end;

  TLWebServer = class(TLHTTPServer)
  protected
    FFileHandler: TFileHandler;
    FCGIHandler: TCGIHandler;
    FPHPCGIHandler: TPHPCGIHandler;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LogAccess(const AMessage: string); override;
    procedure LogError(const AMessage: string; ASocket: TLSocket);
  end;

implementation

{ Example handlers }

const
  ServerSoftware = 'fpHTTPd/0.2';
  ScriptPathPrefix = 'cgi-bin/';
  DocumentRoot = '/var/www';
  CGIPath = '/usr/local/bin:/usr/bin:/bin';
  CGIRoot = '/usr/lib/cgi-bin/';
  PHPCGIBinary = '/usr/lib/cgi-bin/php';

function TCGIHandler.HandleURI(ASocket: TLHTTPSocket): TOutputItem;
var
  lOutput: TCGIOutput;
  lExecPath: string;
begin
  if StrLComp(ASocket.RequestInfo.Argument, ScriptPathPrefix, 
      Length(ScriptPathPrefix)) = 0 then
  begin
    lOutput := TCGIOutput.Create;
    lOutput.Socket := ASocket;
    lOutput.Process.CurrentDirectory := CGIRoot;
    lExecPath := CGIRoot+(ASocket.RequestInfo.Argument+Length(ScriptPathPrefix));
    if SeparatePath(lExecPath, lOutput.ExtraPath) then
    begin
      lOutput.Process.CommandLine := lExecPath;
      lOutput.ScriptFileName := lExecPath;
      lOutput.ScriptName := Copy(lExecPath, Length(CGIRoot), 
        Length(lExecPath)-Length(CGIRoot)+1);
      lOutput.ExecuteProcess;
    end else
      ASocket.RequestInfo.Status := hsNotFound;
    Result := lOutput;
  end else
    Result := nil;
end;

function TFileHandler.HandleFile(const ARequest: TDocumentRequest): TOutputItem;
var
  lFileOutput: TFileOutput;
  lReqInfo: PRequestInfo;
  lIndex: integer;
  lInfo: TSearchRec;
begin
  if Length(ARequest.ExtraPath) = 0 then
  begin
    lReqInfo := @ARequest.Socket.RequestInfo;
    if not (lReqInfo^.RequestType in [hrHead, hrGet]) then
    begin
      lReqInfo^.Status := hsNotAllowed;
    end else begin
      lFileOutput := TFileOutput.Create;
      FindFirst(ARequest.Document, 0, lInfo);
      if lFileOutput.Open(ARequest.Document) then
      begin
        lReqInfo^.Status := hsOK;
        lReqInfo^.ContentLength := lInfo.Size;
        lReqInfo^.LastModified := LocalTimeToGMT(FileDateToDateTime(lInfo.Time));
        lIndex := MimeList.IndexOf(ExtractFileExt(lReqInfo^.Argument));
        if lIndex >= 0 then
          lReqInfo^.ContentType := TStringObject(MimeList.Objects[lIndex]).Str;
        Result := lFileOutput;
        ARequest.Socket.StartResponse(lFileOutput);
      end else
        lFileOutput.Free;
      FindClose(lInfo);
    end;
  end;
end;

function TFileHandler.HandleURI(ASocket: TLHTTPSocket): TOutputItem;
var
  lDocRequest: TDocumentRequest;
  lHandler: TDocumentHandler;
begin
  Result := nil;
  lDocRequest.Socket := ASocket;
  lDocRequest.Document := ASocket.RequestInfo.Argument;
  if not SeparatePath(lDocRequest.Document, lDocRequest.ExtraPath) then exit;
  
  lHandler := FDocHandlerList;
  while lHandler <> nil do
  begin
    Result := lHandler.HandleDocument(lDocRequest);
    if Result <> nil then exit;
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
  lOutput: TCGIOutput;
begin
  if ExtractFileExt(ARequest.Document) = '.php' then
  begin
    lOutput := TCGIOutput.Create;
    lOutput.Socket := ARequest.Socket;
    lOutput.Process.CommandLine := PHPCGIBinary;
    lOutput.ScriptName := '/'+ARequest.Document;
    lOutput.ScriptFileName := DocumentRoot+lOutput.ScriptName;
    lOutput.ExtraPath := ARequest.ExtraPath;
    lOutput.ExecuteProcess;
    Result := lOutput;
  end else
    Result := nil;
end;

{ Output Items }

constructor TFileOutput.Create;
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

function TFileOutput.WriteData: boolean;
var
  lRead: integer;
begin
  if FEof then 
    exit(true);
  BlockRead(FFile, FBuffer[FBufferPos], FBufferSize-FBufferPos, lRead);
  Inc(FBufferPos, lRead);
  if lRead = 0 then
  begin
    { EOF reached }
    Close(FFile);
    exit(true);
  end;
  exit(false);
end;

constructor TCGIOutput.Create;
begin
  inherited;
  FProcess := TLProcess.Create(nil);
  FProcess.Options := FProcess.Options + [poUsePipes];
  FProcess.OnNeedInput := @CGIProcNeedInput;
  FProcess.OnHasOutput := @CGIProcHasOutput;
  FProcess.OnHasStderr := @CGIProcHasStderr;
end;

destructor TCGIOutput.Destroy;
begin
  inherited;
  FProcess.Free;
end;

procedure TCGIOutput.CGIProcNeedInput(AHandle: TLHandle);
begin
  FProcess.InputEvent.IgnoreWrite := true;
  FSocket.ParseBuffer;
end;

procedure TCGIOutput.CGIProcHasOutput(AHandle: TLHandle);
begin
  { CGI process has output pending, we can write a block to socket }
  if FParsingHeaders then
  begin
    if WriteData and FParsingHeaders then
    begin
      { still parsing headers ? something's wrong }
      FParsingHeaders := false;
      if FProcess.ExitStatus = 127 then
        FSocket.RequestInfo.Status := hsNotFound
      else
        FSocket.RequestInfo.Status := hsInternalError;
      FSocket.StartResponse(Self);
      exit;
    end;
  end;
  if not FParsingHeaders then
    FSocket.WriteBlock;
end;

procedure TCGIOutput.AddHTTPParam(const AName: string; AParam: TLHTTPRequestParameter);
var
  lValue: pchar;
begin
  lValue := FSocket.RequestInfo.Parameters[AParam];
  if lValue = nil then exit;
  FProcess.Environment.Add(AName+'='+lValue);
end;

procedure TCGIOutput.CGIProcHasStderr(AHandle: TLHandle);
var
  lBuf: array[0..1023] of char;
  lRead: integer;
begin
  lRead := FProcess.Stderr.Read(lBuf, sizeof(lBuf)-1);
  lBuf[lRead] := #0;
  write(pchar(@lBuf[0]));
end;

procedure TCGIOutput.ExecuteProcess;
begin
  FProcess.Environment.Clear;
{
  FProcess.Environment.Add('SERVER_ADDR=');
  FProcess.Environment.Add('SERVER_ADMIN=');
  FProcess.Environment.Add('SERVER_NAME=');
  FProcess.Environment.Add('SERVER_PORT=');
}
  FProcess.Environment.Add('SERVER_SOFTWARE='+ServerSoftware);

  FProcess.Environment.Add('GATEWAY_INTERFACE=CGI/1.1'); 
  FProcess.Environment.Add('SERVER_PROTOCOL='+FSocket.RequestInfo.VersionStr);
  FProcess.Environment.Add('REQUEST_METHOD='+FSocket.RequestInfo.Method);
  FProcess.Environment.Add('REQUEST_URI=/'+FSocket.RequestInfo.Argument);

  if Length(FExtraPath) > 0 then
  begin
    FProcess.Environment.Add('PATH_INFO='+FExtraPath);
    { do not set PATH_TRANSLATED: bug in PHP }
//    FProcess.Environment.Add('PATH_TRANSLATED='+DocumentRoot+FExtraPath);
  end;

  FProcess.Environment.Add('SCRIPT_NAME='+FScriptName);
  FProcess.Environment.Add('SCRIPT_FILENAME='+FScriptFileName);
  
  case FSocket.RequestInfo.RequestType of
    hrGet:
    begin
      FProcess.Environment.Add('QUERY_STRING='+FSocket.RequestInfo.QueryParams);
    end;
    hrPost:
    begin
      AddHTTPParam('CONTENT_TYPE', hpContentType);
      AddHTTPParam('CONTENT_LENGTH', hpContentLength);
    end;
  end;
  FProcess.Environment.Add('REMOTE_ADDR='+FSocket.PeerAddress);
  FProcess.Environment.Add('REMOTE_PORT='+IntToStr(FSocket.Port));

  { used when user has authenticated in some way to server }
//  FProcess.Environment.Add('AUTH_TYPE='+...);
//  FProcess.Environment.Add('REMOTE_USER='+...);
  
  FProcess.Environment.Add('DOCUMENT_ROOT='+DocumentRoot);
  FProcess.Environment.Add('REDIRECT_STATUS=200');
  AddHTTPParam('HTTP_HOST', hpHost);
  AddHTTPParam('HTTP_COOKIE', hpCookie);
  AddHTTPParam('HTTP_CONNECTION', hpConnection);
  AddHTTPParam('HTTP_REFERER', hpReferer);
  AddHTTPParam('HTTP_USER_AGENT', hpUserAgent);
  AddHTTPParam('HTTP_ACCEPT', hpAccept);
  FProcess.Environment.Add('PATH='+CGIPath);

  FProcess.Eventer := FSocket.Parent.Eventer;
  FProcess.Execute;

  FParsingHeaders := true;
  FReadPos := FBufferPos;
  FParsePos := FBuffer+FReadPos;
end;

function TCGIOutput.HandleInput(ABuffer: pchar; ASize: dword): dword;
begin
  Result := FProcess.Input.Write(ABuffer^, ASize);
  FProcess.InputEvent.IgnoreWrite := false;
end;

function  TCGIOutput.ParseHeaders: boolean;
var
  lHttpStatus: TLHTTPStatus;
  iEnd, lCode: integer;
  lStatus, lLength: dword;
  pLineEnd, pNextLine, pValue: pchar;

  procedure AddExtraHeader;
  begin
    FSocket.RequestInfo.ExtraHeaders := FSocket.RequestInfo.ExtraHeaders +
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
      FSocket.StartResponse(Self);
      exit(false);
    end;
    iEnd := IndexByte(FParsePos^, iEnd, ord(':'));
    if (iEnd = -1) or (FParsePos[iEnd+1] <> ' ') then
      break;
    FParsePos[iEnd] := #0;
    pValue := FParsePos+iEnd+2;
    if StrIComp(FParsePos, 'Content-type') = 0 then
    begin
      FSocket.RequestInfo.ContentType := pValue;
    end else 
    if StrIComp(FParsePos, 'Location') = 0 then
    begin
      if StrLIComp(pValue, 'http://', 7) = 0 then
      begin
        FSocket.RequestInfo.Status := hsMovedPermanently;
        { add location header as-is to response }
        AddExtraHeader;
      end else
        writeln('WARNING: unimplemented ''Location'' response received from CGI script');
      //FSocket.RequestInfo.Status := hsRedirect;
    end else 
    if StrIComp(FParsePos, 'Status') = 0 then
    begin
      Val(pValue, lStatus, lCode);
      if lCode <> 0 then
        break;
      for lHttpStatus := Low(TLHTTPStatus) to High(TLHTTPStatus) do
        if HTTPStatusCodes[lHttpStatus] = lStatus then
          FSocket.RequestInfo.Status := lHttpStatus;
    end else
    if StrIComp(FParsePos, 'Content-Length') = 0 then
    begin
      Val(pValue, lLength, lCode);
      if lCode <> 0 then
        break;
      FSocket.RequestInfo.ContentLength := lLength;
    end else
    if StrIComp(FParsePos, 'Last-Modified') = 0 then
    begin
      if not TryHTTPDateStrToDateTime(pValue, 
          FSocket.RequestInfo.LastModified) then
        writeln('WARNING: unable to parse last-modified string from CGI script: ', pValue);
    end else
      AddExtraHeader;
    FParsePos := pNextLine;
  until false;

  { error happened }
  FSocket.RequestInfo.Status := hsInternalError;
  exit(true);
end;

function TCGIOutput.WriteData: boolean;
var
  lRead: integer;
begin
  if not FParsingHeaders then
    FReadPos := FBufferPos;
  lRead := FProcess.Output.Read(FBuffer[FReadPos], FBufferSize-FReadPos);
  if lRead = 0 then exit(true);
  Inc(FReadPos, lRead);
  if FParsingHeaders then
  begin
    if ParseHeaders then
    begin
      { error while parsing }
      FEof := true;
      exit(true);
    end;
  end else
    FBufferPos := FReadPos;
  exit(false);
end;

{ TLWebServer }

constructor TLWebServer.Create(AOwner: TComponent);
begin
  inherited;

  FFileHandler := TFileHandler.Create;
  FCGIHandler := TCGIHandler.Create;
  FPHPCGIHandler := TPHPCGIHandler.Create;
  FOnError := @LogError;

  RegisterHandler(FFileHandler);
  RegisterHandler(FCGIHandler);
  FFileHandler.RegisterHandler(FPHPCGIHandler);
end;

destructor TLWebServer.Destroy;
begin
  inherited;

  FFileHandler.Free;
  FCGIHandler.Free;
  FPHPCGIHandler.Free;
end;

procedure TLWebServer.LogAccess(const AMessage: string);
begin
  writeln(AMessage);
end;

procedure TLWebServer.LogError(const AMessage: string; ASocket: TLSocket);
begin
  writeln(AMessage);
end;

end.
