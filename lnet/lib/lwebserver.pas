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

  TScriptOutput = class(TBufferOutput)
  public
    constructor Create;

    procedure UpdateHeaders; virtual;
  end;

  TCGIOutput = class(TScriptOutput)
  protected
    FProcess: TLProcess;
    FParsePos: pchar;
    FReadPos: integer;
    FParsingHeaders: boolean;
    FExecPath: string;
    FExtraPath: string;
    
    procedure AddHTTPParam(const AName: string; AParam: TLHTTPRequestParameter);
    procedure CGIProcNeedInput(AHandle: TLHandle);
    procedure CGIProcHasOutput(AHandle: TLHandle);
    function  ParseHeaders: boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function  HandleInput(ABuffer: pchar; ASize: dword): dword; override;
    function  WriteData: boolean; override;
    procedure UpdateHeaders; override;
    
    property ExecPath: string read FExecPath write FExecPath;
    property ExtraPath: string read FExtraPath write FExtraPath;
  end;

  TCGIHandler = class(TDocumentHandler)
  public
    function HandleDocument(ASocket: TLHTTPSocket): TOutputItem; override;
  end;

  TPHPCGIHandler = class(TDocumentHandler)
  public
    function HandleDocument(ASocket: TLHTTPSocket): TOutputItem; override;
  end;

  TFileHandler = class(TDocumentHandler)
  public
    function HandleDocument(ASocket: TLHTTPSocket): TOutputItem; override;
  end;

  TLWebServer = class(TLHTTPServer)
  protected
    FFileHandler: TFileHandler;
    FCGIHandler: TCGIHandler;
    FPHPCGIHandler: TPHPCGIHandler;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{ Example handlers }

function TPHPCGIHandler.HandleDocument(ASocket: TLHTTPSocket): TOutputItem;
begin
  Result := nil;
end;

const
  ServerSoftware = 'fpHTTPd/0.2';
  ScriptPathPrefix = 'cgi-bin/';
  DocumentRoot = '/var/www';
  CGIPath = '/usr/local/bin:/usr/bin:/bin';
  CGIRoot = '/usr/lib/cgi-bin/';

function TCGIHandler.HandleDocument(ASocket: TLHTTPSocket): TOutputItem;
var
  lOutput: TCGIOutput;
begin
  if StrLComp(ASocket.RequestInfo.Argument, ScriptPathPrefix, 
      Length(ScriptPathPrefix)) = 0 then
  begin
    lOutput := TCGIOutput.Create;
    lOutput.Socket := ASocket;
    lOutput.ExecPath := CGIRoot+(ASocket.RequestInfo.Argument+Length(ScriptPathPrefix));
    if SeparatePath(lOutput.ExecPath, lOutput.ExtraPath) then
      lOutput.UpdateHeaders
    else
      ASocket.RequestInfo.Status := hsNotFound;
    Result := lOutput;
  end else
    Result := nil;
end;

function TFileHandler.HandleDocument(ASocket: TLHTTPSocket): TOutputItem;
var
  lFileOutput: TFileOutput;
  lInfo: TSearchRec;
  lIndex: integer;
begin
  Result := nil;
  if FindFirst(ASocket.RequestInfo.Argument, 0, lInfo) = 0 then
  begin
    if not (ASocket.RequestInfo.RequestType in [hrHead, hrGet]) then
    begin
      ASocket.RequestInfo.Status := hsNotAllowed;
    end else begin
      lFileOutput := TFileOutput.Create;
      if lFileOutput.Open(ASocket.RequestInfo.Argument) then
      begin
        ASocket.RequestInfo.Status := hsOK;
        ASocket.RequestInfo.ContentLength := lInfo.Size;
        ASocket.RequestInfo.LastModified := LocalTimeToGMT(FileDateToDateTime(lInfo.Time));
        lIndex := MimeList.IndexOf(ExtractFileExt(ASocket.RequestInfo.Argument));
        if lIndex >= 0 then
          ASocket.RequestInfo.ContentType := TStringObject(MimeList.Objects[lIndex]).Str;
        Result := lFileOutput;
        ASocket.StartResponse(lFileOutput);
      end else
        lFileOutput.Free;
    end;
  end;
  FindClose(lInfo);
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
  FileMode := fmInput;
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

constructor TScriptOutput.Create;
begin
  inherited;
end;

procedure TScriptOutput.UpdateHeaders;
begin
end;

constructor TCGIOutput.Create;
begin
  inherited;
  FProcess := TLProcess.Create(nil);
  FProcess.Options := FProcess.Options + [poStderrToOutput, poUsePipes];
  FProcess.OnNeedInput := @CGIProcNeedInput;
  FProcess.OnHasOutput := @CGIProcHasOutput;
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

procedure TCGIOutput.UpdateHeaders;
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
    FProcess.Environment.Add('PATH_TRANSLATED='+DocumentRoot+FExtraPath);
  end;

  FProcess.Environment.Add('SCRIPT_NAME=/'+Copy(FExecPath, Length(CGIRoot)-7, 1024));
  FProcess.Environment.Add('SCRIPT_FILENAME='+FExecPath);
  
  case FSocket.RequestInfo.RequestType of
    hrGet:
    begin
      FProcess.Environment.Add('QUERY_STRING='+FSocket.RequestInfo.QueryParams);
    end;
    hrPost:
    begin
      FProcess.Environment.Add('CONTENT_TYPE='+FSocket.RequestInfo.Parameters[hpContentType]);
      FProcess.Environment.Add('CONTENT_LENGTH='+FSocket.RequestInfo.Parameters[hpContentLength]);
    end;
  end;
  FProcess.Environment.Add('REMOTE_ADDR='+FSocket.PeerAddress);
  FProcess.Environment.Add('REMOTE_PORT='+IntToStr(FSocket.Port));

  { used when user has authenticated in some way to server }
//  FProcess.Environment.Add('AUTH_TYPE='+...);
//  FProcess.Environment.Add('REMOTE_USER='+...);
  
  FProcess.Environment.Add('DOCUMENT_ROOT='+DocumentRoot);
  AddHTTPParam('HTTP_HOST', hpHost);
  AddHTTPParam('HTTP_COOKIE', hpCookie);
  AddHTTPParam('HTTP_CONNECTION', hpConnection);
  AddHTTPParam('HTTP_REFERER', hpReferer);
  AddHTTPParam('HTTP_USER_AGENT', hpUserAgent);
  AddHTTPParam('HTTP_ACCEPT', hpAccept);
  FProcess.Environment.Add('PATH='+CGIPath);

  FProcess.Environment.Add('UNIQUE_ID=Q-eLbVoAAAQAAEkoEF8');

  FProcess.CommandLine := FExecPath;
  FProcess.CurrentDirectory := CGIRoot;
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
      {$message warn TODO: CGI redirect location}
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
      begin
        {$message warn TODO: log a warning message, unhandled datetime?};
      end;
    end else
      {$message warn TODO: log a warning message, unhandled header?};
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

  RegisterHandler(FFileHandler);
  RegisterHandler(FCGIHandler);
  RegisterHandler(FPHPCGIHandler);
end;

destructor TLWebServer.Destroy;
begin
  inherited;

  FFileHandler.Free;
  FCGIHandler.Free;
  FPHPCGIHandler.Free;
end;

end.
