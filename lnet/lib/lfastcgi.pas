{ FastCGI requester support for lNet

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

unit lfastcgi;

{$mode objfpc}{$h+}

interface

uses
  classes, sysutils, fastcgi, lnet, levents, lstrbuffer;

type
  TLFastCGIClient = class;
  TLFastCGIRequest = class;
  TLFastCGIPool = class;

  TLFastCGIRequestEvent = procedure(ARequest: TLFastCGIRequest) of object;

  PLFastCGIRequest = ^TLFastCGIRequest;
  TLFastCGIRequest = class(TObject)
  protected
    FID: integer;
    FClient: TLFastCGIClient;
    FBuffer: TStringBuffer;
    FBufferSendPos: integer;
    FHeader: FCGI_Header;
    FInputBuffer: pchar;
    FInputSize: integer;
    FNextFree: TLFastCGIRequest;
    FNextSend: TLFastCGIRequest;
    FOnEndRequest: TLFastCGIRequestEvent;
    FOnInput: TLFastCGIRequestEvent;
    FOnOutput: TLFastCGIRequestEvent;
    FOnStderr: TLFastCGIRequestEvent;

    procedure HandleReceive;
    function  HandleSend: boolean;
    procedure DoOutput;
    procedure DoStderr;
    procedure EndRequest;
    procedure SetContentLength(NewLength: integer);
    procedure SendEmptyRec(AType: integer);
    procedure SendGetValues;
    procedure SetID(const NewID: integer);
  public
    constructor Create;
    
    procedure AbortRequest;
    function  Get(ABuffer: pchar; ASize: integer): integer;
    function  SendBuffer: integer;
    function  SendPrivateBuffer: boolean;
    procedure SendBeginRequest(AType: integer);
    procedure SendParam(const AName, AValue: string; AReqType: integer = FCGI_PARAMS);
    function  SendInput(const ABuffer: pchar; ASize: integer): integer;
    procedure DoneParams;
    procedure DoneInput;

    property ID: integer read FID write SetID;
    property OnEndRequest: TLFastCGIRequestEvent read FOnEndRequest write FOnEndRequest;
    property OnInput: TLFastCGIRequestEvent read FOnInput write FOnInput;
    property OnOutput: TLFastCGIRequestEvent read FOnOutput write FOnOutput;
    property OnStderr: TLFastCGIRequestEvent read FOnStderr write FOnStderr;
  end;

  TFastCGIParseState = (fsHeader, fsData, fsFlush);
  
  PLFastCGIClient = ^TLFastCGIClient;
  TLFastCGIClient = class(TLTcp)
  protected
    FRequests: PLFastCGIRequest;
    FRequestsCount: integer;
    FNextRequestID: integer;
    FFreeRequest: TLFastCGIRequest;
    FSendRequest: TLFastCGIRequest;
    FRequest: TLFastCGIRequest;
    FState: TFastCGIParseState;
    FNextFree: TLFastCGIClient;
    FPool: TLFastCGIPool;
    FBuffer: pchar;
    FBufferEnd: pchar;
    FBufferPos: pchar;
    FBufferSize: dword;
    FReqType: byte;
    FContentLength: integer;
    FPaddingLength: integer;

    procedure ConnectEvent(ASocket: TLHandle); override;
    function  CreateRequester: TLFastCGIRequest;
    procedure HandleGetValuesResult;
    procedure HandleReceive(ASocket: TLSocket);
    procedure HandleSend(ASocket: TLSocket);
    procedure ParseBuffer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AddToSendQueue(ARequest: TLFastCGIRequest);
    function  BeginRequest(AType: integer): TLFastCGIRequest;
    procedure EndRequest(ARequest: TLFastCGIRequest);
    procedure Flush;
    function  GetBuffer(ABuffer: pchar; ASize: integer): integer;

    property ReqType: byte read FReqType;
  end;

  TLFastCGIPool = class(TObject)
  protected
    FClients: PLFastCGIClient;
    FClientsCount: integer;
    FClientsAvail: integer;
    FFreeClient: TLFastCGIClient;
    FEventer: TLEventer;
    FHost: string;
    FPort: integer;
    
    procedure AddToFreeClients(AClient: TLFastCGIClient);
    function  CreateClient: TLFastCGIClient;
  public
    constructor Create;
    destructor Destroy; override;

    function  BeginRequest(AType: integer): TLFastCGIRequest;
    procedure EndRequest(AClient: TLFastCGIClient);

    property Eventer: TLEventer read FEventer write FEventer;
    property Host: string read FHost write FHost;
    property Port: integer read FPort write FPort;
  end;

implementation

{ TLFastCGIRequest }

constructor TLFastCGIRequest.Create;
begin
  inherited;

  FBuffer := InitStringBuffer(504);
  FHeader.Version := FCGI_VERSION_1;
end;

procedure TLFastCGIRequest.HandleReceive;
begin
  case FClient.ReqType of
    FCGI_STDOUT: DoOutput;
    FCGI_STDERR: DoStderr;
    FCGI_END_REQUEST: EndRequest;
    FCGI_GET_VALUES_RESULT: FClient.HandleGetValuesResult;
  else
    FClient.Flush;
  end;
end;

function TLFastCGIRequest.HandleSend: boolean;
begin
  if FOnInput <> nil then
    FOnInput(Self);
  Result := FInputBuffer = nil;
end;

procedure TLFastCGIRequest.DoOutput;
begin
  if FOnOutput <> nil then
    FOnOutput(Self);
end;

procedure TLFastCGIRequest.DoStderr;
begin
  if FOnStderr <> nil then
    FOnStderr(Self);
end;

procedure TLFastCGIRequest.EndRequest;
begin
  if FOnEndRequest <> nil then
    FOnEndRequest(Self);
  FClient.EndRequest(Self);
  {$message warning TODO: do something useful with end request data }
  FClient.Flush;
end;

function TLFastCGIRequest.Get(ABuffer: pchar; ASize: integer): integer;
begin
  Result := FClient.GetBuffer(ABuffer, ASize);
end;

procedure TLFastCGIRequest.SetID(const NewID: integer);
begin
  FID := NewID;
  FHeader.RequestIDB0 := byte(NewID and $FF);
  FHeader.RequestIDB1 := byte((NewID shr 8) and $FF);
end;

procedure TLFastCGIRequest.SetContentLength(NewLength: integer);
begin
  FHeader.ContentLengthB0 := byte(NewLength and $FF);
  FHeader.ContentLengthB1 := byte((NewLength shr 8) and $FF);
  FHeader.PaddingLength := byte(7-((NewLength+7) and 7));
end;

const
  PaddingBuffer: array[0..7] of char = (#0, #0, #0, #0, #0, #0, #0, #0);
type
  TLFastCGIStringSize = record
    Size: integer;
    SizeBuf: array[0..3] of char;
  end;

function GetFastCGIStringSize(ABufferPos: pbyte; var ASize: integer): integer;
begin
  ASize := ABufferPos[0];
  if ASize >= 128 then
  begin
    ASize := ((ABufferPos[0] shl 24) and $7f) or (ABufferPos[1] shl 16)
      or (ABufferPos[2] shl 8) or ABufferPos[3];
    Result := 4;
  end else
    Result := 1;
end;
  
procedure FillFastCGIStringSize(const AStr: string; var AFastCGIStr: TLFastCGIStringSize);
var
  lLen: dword;
begin
  lLen := dword(Length(AStr));
  if lLen > 127 then
  begin
    AFastCGIStr.Size := 4;
    AFastCGIStr.SizeBuf[0] := char($80 + ((lLen shr 24) and $ff));
    AFastCGIStr.SizeBuf[1] := char((lLen shr 16) and $ff);
    AFastCGIStr.SizeBuf[2] := char((lLen shr 8) and $ff);
    AFastCGIStr.SizeBuf[3] := char(lLen and $ff);
  end else begin
    AFastCGIStr.Size := 1;
    AFastCGIStr.SizeBuf[0] := char(lLen);
  end;
end;

procedure TLFastCGIRequest.SendBeginRequest(AType: integer);
var
  lBody: FCGI_BeginRequestBody;
begin
  lBody.roleB1 := byte((AType shr 8) and $ff);
  lBody.roleB0 := byte(AType and $ff);
  lBody.flags := FCGI_KEEP_CONN;
  FHeader.ReqType := FCGI_BEGIN_REQUEST;
  SetContentLength(sizeof(lBody));
  AppendString(FBuffer, @FHeader, sizeof(FHeader));
  AppendString(FBuffer, @lBody, sizeof(lBody));
end;

procedure TLFastCGIRequest.SendParam(const AName, AValue: string; AReqType: integer = FCGI_PARAMS);
var
  lNameLen: TLFastCGIStringSize;
  lValueLen: TLFastCGIStringSize;
begin
  FillFastCGIStringSize(AName, lNameLen);
  FillFastCGIStringSize(AValue, lValueLen);
  FHeader.ReqType := AReqType;
  SetContentLength(lNameLen.Size+lValueLen.Size+Length(AName)+Length(AValue));
  AppendString(FBuffer, @FHeader, sizeof(FHeader));
  AppendString(FBuffer, @lNameLen.SizeBuf[0], lNameLen.Size);
  AppendString(FBuffer, @lValueLen.SizeBuf[0], lValueLen.Size);
  AppendString(FBuffer, AName);
  AppendString(FBuffer, AValue);
  AppendString(FBuffer, @PaddingBuffer[0], FHeader.PaddingLength);
end;

procedure TLFastCGIRequest.SendGetValues;
var
  lRequestID: integer;
begin
  { management record type has request id 0 }
  lRequestID := ID;
  ID := 0;
  SendParam('FCGI_MAX_REQS', '', FCGI_GET_VALUES);
  ID := lRequestID;
  SendPrivateBuffer;
end;

function  TLFastCGIRequest.SendInput(const ABuffer: pchar; ASize: integer): integer;
begin
  { first send current buffer if any }
  if FInputBuffer <> nil then
  begin
    Result := SendBuffer;
    if FInputBuffer <> nil then exit;
  end else Result := 0;
  if Result >= ASize then exit;
  if FInputBuffer = nil then
  begin
    FInputBuffer := ABuffer+Result;
    FInputSize := ASize-Result;
    FHeader.ReqType := FCGI_STDIN;
    SetContentLength(FInputSize);
    AppendString(FBuffer, @FHeader, sizeof(FHeader));
  end;
  Inc(Result, SendBuffer);
end;

function TLFastCGIRequest.SendPrivateBuffer: boolean;
var
  lWritten: integer;
begin
  { already a queue and we are not first in line ? no use in trying to send then }
  if (FClient.FSendRequest <> nil) and (FClient.FSendRequest <> Self) then 
    exit(false);

  lWritten := FClient.Send(FBuffer.Memory[FBufferSendPos], 
    FBuffer.Pos-FBuffer.Memory-FBufferSendPos);
  Inc(FBufferSendPos, lWritten);
  Result := FBufferSendPos = FBuffer.Pos-FBuffer.Memory;
  if Result then
  begin
    { all of headers written }
    FBufferSendPos := 0;
    { rewind stringbuffer }
    FBuffer.Pos := FBuffer.Memory;
  end else
    FClient.AddToSendQueue(Self);
end;

function TLFastCGIRequest.SendBuffer: integer;
var
  lWritten: integer;
begin
  { already a queue and we are not first in line ? no use in trying to send then }
  if (FClient.FSendRequest <> nil) and (FClient.FSendRequest <> Self) then 
    exit(0);

  { non-empty header to be sent? }
  if FBuffer.Pos <> FBuffer.Memory then
    if not SendPrivateBuffer then exit(0);

  if FInputBuffer = nil then exit(0);

  lWritten := FClient.Send(FInputBuffer^, FInputSize);
  Inc(FInputBuffer, lWritten);
  Dec(FInputSize, lWritten);
  if FInputSize = 0 then
  begin
    FInputBuffer := nil;
    AppendString(FBuffer, @PaddingBuffer[0], FHeader.PaddingLength);
  end else
    FClient.AddToSendQueue(Self);
  Result := lWritten;
end;

procedure TLFastCGIRequest.SendEmptyRec(AType: integer);
begin
  FHeader.ReqType := AType;
  SetContentLength(0);
  AppendString(FBuffer, @FHeader, sizeof(FHeader));
  { no padding needed for empty string }
end;

procedure TLFastCGIRequest.DoneParams;
begin
  SendEmptyRec(FCGI_PARAMS);
  SendPrivateBuffer;
end;

procedure TLFastCGIRequest.DoneInput;
begin
  SendEmptyRec(FCGI_STDIN);
  SendPrivateBuffer;
end;

procedure TLFastCGIRequest.AbortRequest;
begin
  FHeader.ReqType := FCGI_ABORT_REQUEST;
  SetContentLength(0);
  AppendString(FBuffer, @FHeader, sizeof(FHeader));
  SendPrivateBuffer;
end;

{ TLFastCGIClient }

const
  DataBufferSize = 64*1024-1;

constructor TLFastCGIClient.Create(AOwner: TComponent);
begin
  inherited;

  FBuffer := GetMem(DataBufferSize+1);
  FBufferPos := FBuffer;
  FBufferEnd := FBuffer;
  FRequests := AllocMem(sizeof(TLFastCGIRequest));
  FRequestsCount := 1;
  FFreeRequest := CreateRequester;
  OnReceive := @HandleReceive;
  OnCanSend := @HandleSend;
end;

destructor TLFastCGIClient.Destroy;
var
  I: integer;
begin
  for I := 0 to FNextRequestID-1 do
    FRequests[I].Free;
  FreeMem(FRequests);
  inherited;
end;

function TLFastCGIClient.GetBuffer(ABuffer: pchar; ASize: integer): integer;
begin
  Result := FBufferEnd - FBufferPos;
  if Result > FContentLength then 
    Result := FContentLength;
  if Result > ASize then
    Result := ASize;
  Move(FBufferPos^, ABuffer^, Result);
  Inc(FBufferPos, Result);
  Dec(FContentLength, Result);
  { buffer empty? reset }
  if FBufferPos = FBufferEnd then
  begin
    FBufferPos := FBuffer;
    FBufferEnd := FBuffer;
  end;
end;

procedure TLFastCGIClient.ConnectEvent(ASocket: TLHandle);
begin
  FRequest.SendGetValues;
  if FPool <> nil then
    FPool.AddToFreeClients(Self);

  inherited;
end;

procedure TLFastCGIClient.HandleGetValuesResult;
var
  lNameLen, lValueLen, lMaxReqs, lCode: integer;
  lBufferPtr: pchar;
  lPrevChar: char;
begin
  lBufferPtr := FBufferPos;
  Inc(lBufferPtr, GetFastCGIStringSize(PByte(lBufferPtr), lNameLen));
  Inc(lBufferPtr, GetFastCGIStringSize(PByte(lBufferPtr), lValueLen));
  if lBufferPtr + lNameLen + lValueLen > FBufferEnd then exit;
  if StrLComp(lBufferPtr, 'FCGI_MAX_REQS', lNameLen) = 0 then
  begin
    lPrevChar := lBufferPtr[lNameLen+lValueLen];
    lBufferPtr[lNameLen+lValueLen] := #0;
    Val(lBufferPtr+lNameLen, lMaxReqs, lCode);
    lBufferPtr[lNameLen+lValueLen] := lPrevChar;
    if (lCode = 0) and (FRequestsCount <> lMaxReqs) then
    begin
      FRequestsCount := lMaxReqs;
      ReallocMem(FRequests, sizeof(TLFastCGIRequest)*lMaxReqs);
    end;
  end;
  Inc(lBufferPtr, lNameLen+lValueLen);
  Dec(FContentLength, lBufferPtr-FBufferPos);
  FBufferPos := lBufferPtr;
end;

procedure TLFastCGIClient.HandleReceive(ASocket: TLSocket);
var
  lRead: integer;
begin
  lRead := Get(FBufferEnd^, DataBufferSize-PtrUInt(FBufferEnd-FBuffer));
  if lRead = 0 then exit;
  Inc(FBufferEnd, lRead);
  ParseBuffer;
end;

procedure TLFastCGIClient.HandleSend(ASocket: TLSocket);
var
  lRequest: TLFastCGIRequest;
begin
  if FSendRequest = nil then exit;
  lRequest := FSendRequest.FNextSend;
  repeat
    if not lRequest.SendPrivateBuffer or not lRequest.HandleSend then
      exit;
    { only this one left in list ? }
    if FSendRequest = lRequest then
    begin
      FSendRequest := nil;
      exit;
    end else begin
      lRequest.FNextSend := nil;
      lRequest := lRequest.FNextSend;
      FSendRequest.FNextSend := lRequest;
    end;
  until false;
end;

procedure TLFastCGIClient.AddToSendQueue(ARequest: TLFastCGIRequest);
begin
  if ARequest.FNextSend <> nil then exit;

  if FSendRequest = nil then
    FSendRequest := ARequest
  else
    ARequest.FNextSend := FSendRequest.FNextSend;
  FSendRequest.FNextSend := ARequest;
  FSendRequest := ARequest;
end;

procedure TLFastCGIClient.ParseBuffer;
var
  lHeader: PFCGI_Header;
  lReqIndex: integer;
begin
  repeat
    case FState of
      fsHeader:
      begin
        if FBufferEnd-FBufferPos < sizeof(FCGI_Header) then
          exit;
        lHeader := PFCGI_Header(FBufferPos);
        FReqType := lHeader^.ReqType;
        lReqIndex := (lHeader^.RequestIDB1 shl 8) or lHeader^.RequestIDB0;
        FContentLength := (lHeader^.ContentLengthB1 shl 8) or lHeader^.ContentLengthB0;
        FPaddingLength := lHeader^.PaddingLength;
        Inc(FBufferPos, sizeof(lHeader^));
        if lReqIndex > 0 then
          Dec(lReqIndex);
        if (lReqIndex < FRequestsCount) and (FRequests[lReqIndex] <> nil) then
        begin
          FRequest := FRequests[lReqIndex];
          FState := fsData;
        end else
          Flush;
      end;
      fsData: 
      begin
        FRequest.HandleReceive;
        if FContentLength = 0 then 
          Flush
        else
          exit;
      end;
      fsFlush: Flush;
    end;
  until FBufferPos = FBufferEnd;
end;

procedure TLFastCGIClient.Flush;

  function FlushSize(var ANumBytes: integer): boolean;
  var
    lFlushBytes: integer;
  begin
    lFlushBytes := ANumBytes;
    if lFlushBytes > FBufferEnd - FBufferPos then
      lFlushBytes := FBufferEnd - FBufferPos;
    Dec(ANumBytes, lFlushBytes);
    Inc(FBufferPos, lFlushBytes);
    Result := ANumBytes = 0;
  end;

begin
  FState := fsFlush;
  if FlushSize(FContentLength) and FlushSize(FPaddingLength) then
  begin
    FState := fsHeader;
    FRequest := nil;
  end;
end;

function TLFastCGIClient.CreateRequester: TLFastCGIRequest;
begin
  if FRequests[FNextRequestID] = nil then
    FRequests[FNextRequestID] := TLFastCGIRequest.Create;
  Result := FRequests[FNextRequestID];
  Inc(FNextRequestID);
  Result.FClient := Self;
  Result.ID := FNextRequestID;  { request ids start at 1 }
end;

function TLFastCGIClient.BeginRequest(AType: integer): TLFastCGIRequest;
begin
  if FRootSock = nil then
  begin
    Connect(FPool.Host, FPool.Port);
    FRequest := FRequests[0];
  end;

  if FFreeRequest <> nil then
  begin
    Result := FFreeRequest;
    FFreeRequest := FFreeRequest.FNextFree;
  end else
  if FNextRequestID = FRequestsCount then
    exit(nil)
  else begin
    Result := CreateRequester;
  end;

  Result.SendBeginRequest(AType);
end;

procedure TLFastCGIClient.EndRequest(ARequest: TLFastCGIRequest);
begin
  ARequest.FNextFree := FFreeRequest;
  FFreeRequest := ARequest;
  if FPool <> nil then
    FPool.EndRequest(Self);
end;
   
{ TLFastCGIPool }

constructor TLFastCGIPool.Create;
begin
  inherited;
end;

destructor TLFastCGIPool.Destroy;
var
  I: integer;
begin
  for I := 0 to FClientsAvail-1 do
    FClients[I].Free;
  FreeMem(FClients);
  inherited;
end;

function  TLFastCGIPool.CreateClient: TLFastCGIClient;
begin
  if FClientsAvail = FClientsCount then
  begin
    Inc(FClientsCount, 64);
    ReallocMem(FClients, FClientsCount*sizeof(TLFastCGIRequest));
  end;
  Result := TLFastCGIClient.Create(nil);
  Result.FPool := Self;
  Result.FEventer := FEventer;
  FClients[FClientsAvail] := Result;
  Inc(FClientsAvail);
end;

function  TLFastCGIPool.BeginRequest(AType: integer): TLFastCGIRequest;
var
  lTempClient: TLFastCGIClient;
begin
  Result := nil;
  while FFreeClient <> nil do
  begin
    Result := FFreeClient.BeginRequest(AType);
    lTempClient := FFreeClient;
    if FFreeClient.FNextFree = FFreeClient then
      FFreeClient := nil
    else
      FFreeClient := FFreeClient.FNextFree;
    if Result <> nil then break;
    lTempClient.FNextFree := nil;
  end;

  { all clients busy }
  if Result = nil then
    Result := CreateClient.BeginRequest(AType);
end;

procedure TLFastCGIPool.EndRequest(AClient: TLFastCGIClient);
begin
  AddToFreeClients(AClient);
end;

procedure TLFastCGIPool.AddToFreeClients(AClient: TLFastCGIClient);
begin
  if AClient.FNextFree <> nil then exit;
  
  if FFreeClient = nil then
    FFreeClient := AClient
  else
    AClient.FNextFree := FFreeClient.FNextFree;
  FFreeClient.FNextFree := AClient;
  FFreeClient := AClient;
end;
    
end.
