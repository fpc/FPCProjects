unit DCSHTTPRestServer;

{ Distributed Command Server

  This unit contains a simple (REST) HTTP-server which translates GET-requests
  into thread-commands, waits for the response and sends the result to the HTTP-
  client.

  Copyright (C) 2016 Joost van der Sluis joost@cnoc.nl

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

{$mode objfpc}{$H+}
{$interfaces corba}

interface

uses
  Classes,
  SysUtils,
  strutils,
  typinfo,
  variants,
  varutils,
  fgl,
  BaseUnix,
  sockets,
  ssockets,
  math,
  lazCollections,
  dcsHandler,
  dcsThreadCommandFactory,
  dcsInOutputProcessor,
  dcsListenerThread;

type
  TDCSRestCallThreadList = class(TThreadList);
  TDCSThreadedQueueString = specialize TLazThreadedQueue<string>;
  TDCSRestCallThread = class;

  IDCSHTTPJSonResultEvent = interface ['{467B1B2A-4F31-467B-9D20-3BC4223B70D4}']
    function GetAsJSonString: string;
  end;

  { TDCSHTTPRestServer }

  TDCSHTTPRestServer = class(TThread)
  private
    FInOutputProcessor: TDCSCustomInOutputProcessor;
    FPort: integer;
    FTCPConnection: TInetServer;
    FRestThreadList: TDCSRestCallThreadList;
    FDistributor: TDCSDistributor;
    FInitializationFinished: PRTLEvent;
    FListenerThread: TDCSListenerThread;

    FListenerStr: string;
    FCallNr: integer;
    function CreateInetServer: TInetServer;
    procedure FTCPConnectionConnect(Sender: TObject; Data: TSocketStream);
    procedure FTCPConnectionAcceptError(Sender: TObject; ASocket: Longint; E: Exception; var ErrorAction: TAcceptErrorAction);
    procedure FTCPConnectionConnectQuery(Sender: TObject; ASocket: Longint; var Allow: Boolean);
    function GetListenerID: integer;
  protected
    procedure Execute; override;
    procedure OnListenerEvent(Event: TDCSEvent);
  public
    constructor create(ADistributor: TDCSDistributor; APort: integer);
    procedure RemoveRestThread(ARestThread: TDCSRestCallThread);
    procedure ForceTerminate;
    destructor Destroy; override;

    property ListenerID: integer read GetListenerID;
    property InOutputProcessor: TDCSCustomInOutputProcessor read FInOutputProcessor;
  end;

  { TDcsRestCallThread }


  TDCSRestCallThread = class(TThread)
  private
    FDistributor: TDCSDistributor;
    FData: TSocketStream;
    FRequestLineParsed: Boolean;
    FRequest: string;
    FRestServer: TDCSHTTPRestServer;
    FUID: string;
    FUri: string;
    FResultEvent: TDCSNotificationEvent;
    FResultReceived: PRTLEvent;
    procedure SendResult(StatusCode: integer; StatusText: string; Content: UTF8String);
    function ParseHeaderEnd(HeaderString: string): Boolean;
    procedure ComposeAndSendCommand();
    procedure WriteData(AData: UTF8String);
  protected
    procedure Execute; override;
  public
    constructor Create(Distributor: TDCSDistributor; RestServer: TDCSHTTPRestServer; UID: string; Data: TSocketStream);
    destructor Destroy; override;
    procedure SetResultEvent(AnEvent: TDCSNotificationEvent);
    property UID: string read FUID;
  end;

implementation

type

  { TDCSHTTPInOutputProcessor }

  TDCSHTTPInOutputProcessor = class(TDCSJSonInOutputProcessor)
  public
    function TextToCommand(const ACommandText: string): TDCSThreadCommand; override;
    function EventToText(AnEvent: TDCSEvent): string; override;
    function TextToCommandUID(const ACommandText: string; UID: variant): TDCSThreadCommand;
  end;

{ TDCSHTTPInOutputProcessor }

function TDCSHTTPInOutputProcessor.TextToCommandUID(const ACommandText: string; UID: variant): TDCSThreadCommand;
var
  ACommandClass: TDCSThreadCommandClass;
  i,j: integer;
  APropCount: integer;
  APropList: PPropList;
  APropName: string;
  Params: TStrings;
  CommandName: string;
begin
  result := nil;

  // Remove leading slash
  if ACommandText[1]<>'/' then
    i := 1
  else
    i := 2;

  j := pos('?', ACommandText, i);
  if j = 0 then
    j := length(ACommandText)+1;
  CommandName := copy(ACommandText,i,j-i);

  if CommandName = '' then
    begin
    FDistributor.SendNotification(FLisId, ntInvalidCommand, NULL, 'Received empty request', ACommandText, []);
    Exit;
    end;

  ACommandClass := TDCSThreadCommandFactory.GetCommandClassByName(CommandName);
  if not assigned(ACommandClass) then
    begin
    FDistributor.SendNotification(FLisId, ntInvalidCommand, NULL, 'Command "%s" does not exist.', CommandName, [CommandName]);
    exit;
    end;

  result := ACommandClass.Create(FLisId, UID, FDistributor);

  Params := TStringList.Create;
  try
    Params.Delimiter := '&';
    Params.DelimitedText := copy(ACommandText, j+1, length(ACommandText));

    APropCount := GetPropList(result, APropList);
    try
      for i := 0 to APropCount-1 do
        begin
        APropName := APropList^[i]^.Name;
        j := Params.IndexOfName(LowerCase(APropName));

        if j > -1 then
          begin
          case APropList^[i]^.PropType^.Kind of
            tkAString, tkString, tkUString:
              SetStrProp(result, APropList^[i], Params.ValueFromIndex[j]);
            tkInteger:
              SetOrdProp(result, APropList^[i], StrToIntDef(Params.ValueFromIndex[j], 0));
          end;
          end;
        end;
    finally
      Freemem(APropList);
    end;
  finally
    Params.Free;
  end;
end;

function TDCSHTTPInOutputProcessor.TextToCommand(const ACommandText: string): TDCSThreadCommand;
begin
  result := TextToCommandUID(ACommandText, null);
end;

function TDCSHTTPInOutputProcessor.EventToText(AnEvent: TDCSEvent): string;
var
  HTTPJsonResult: IDCSHTTPJSonResultEvent;
begin
  if Supports(AnEvent, IDCSHTTPJSonResultEvent, HTTPJsonResult) then
    Result := HTTPJsonResult.GetAsJSonString
  else
    Result := inherited;
end;


{ TDcsRestCallThread }

procedure TDCSRestCallThread.SendResult(StatusCode: integer; StatusText: string; Content: UTF8String);
var
  s: string;
begin
  s := Format('HTTP/1.1 %.3d %s'#13#10#13#10,[StatusCode, StatusText])+Content;
  WriteData(s);
  Terminate;
end;

function TDCSRestCallThread.ParseHeaderEnd(HeaderString: string): Boolean;
var
  s: string;
begin
  result := (HeaderString = '');
  if not FRequestLineParsed then
    begin
    s := ExtractDelimited(3,HeaderString,[' ']);
    if copy(s,1,5) <> 'HTTP/' then
      begin
      SendResult(400,'Bad Request','Unrecognized protocol');
      Result := True;
      end
    else
      begin
      FRequest := ExtractDelimited(1,HeaderString,[' ']);
      FUri := ExtractDelimited(2,HeaderString,[' ']);
      FRequestLineParsed := True;
      end;
    end;
end;

procedure TDCSRestCallThread.ComposeAndSendCommand;
var
  ACommand: TDCSThreadCommand;
begin
  ACommand := TDCSHTTPInOutputProcessor(FRestServer.InOutputProcessor).TextToCommandUID(FUri, FUID);
  if not assigned(ACommand) then
    begin
    SendResult(404,'Not Found','Page not found.');
    end
  else
    FDistributor.QueueCommand(ACommand);
end;

procedure TDCSRestCallThread.WriteData(AData: UTF8String);
var
  timeout: integer;
  i: integer;
  written: integer;
  total: integer;
begin
  total := length(AData);
  written := 0;
  timeout := 0;

  repeat
    i := FData.Write(AData[written+1], min(total-written, 65536));

    if i < 0 then
      begin
      if FData.LastError=ESysEAGAIN then
        begin
        if timeout>100 then
          begin
          FDistributor.SendNotification(FRestServer.ListenerId, ntConnectionProblem, null, 'Error during write. Timeout.', '');
          Terminate;
          end;
        sleep(10);
        inc(timeout);
        end
      else
        begin
        if FData.LastError=ESysEPIPE then
          begin
          // Lost connection
          end
        else
          FDistributor.SendNotification(FRestServer.ListenerId, ntConnectionProblem, null, 'Error during write. Socket-error: %d', '', [FData.LastError]);
        Terminate;
        end;
      end
    else
      begin
      inc(written, i);
      timeout := 0;
      end;
  until terminated or (written >= total);
end;


procedure TDCSRestCallThread.Execute;
const
  InputBufferSize = 512;
var
  s: string;
  i: integer;
  InputBuffer: array[0..InputBufferSize-1] of ansichar;
  InputStr: string;
  HeadersComplete: Boolean;
begin
  InputStr := '';
  try
    FData.IOTimeout := 5000;
    HeadersComplete := False;
    // Read headers
    while not Terminated and not HeadersComplete do
      begin
      i := FData.Read(InputBuffer[0], InputBufferSize);
      if i > 0 then
        begin
        setlength(s,i);
        move(InputBuffer[0],s[1],i);
        InputStr:=InputStr+s;
        i := pos(#13#10, InputStr);
        while i > 0 do
          begin
          s := copy(InputStr, 1, i-1);
          delete(InputStr,1,i+1);
          if ParseHeaderEnd(S) then
            begin
            HeadersComplete := True;
            Break;
            end;
          i := pos(#13#10, InputStr);
          end;
        end
      else if i < 0 then
        begin
        if FData.LastError <> ESysEAGAIN then
          begin
          FDistributor.Log(Format('Error during read on HTTP-connection (%s): Socket-error: %d', [HostAddrToStr(FData.RemoteAddress.sin_addr), FData.LastError]), etWarning, Null);
          Terminate;
          end
        else
          begin
          SendResult(408,'Request Timeout','Timeout while waiting for full headers');
          end;
        end
      else if i = 0 then
        begin
        // Zero-count -> Connection closed
        Terminate;
        end;
      end;

    if not Terminated then
      // Compose and send command
      ComposeAndSendCommand();

    if not Terminated then
      begin
      // Wait for result
      RTLeventWaitFor(FResultReceived, 10*MSecsPerSec);
      if not assigned(FResultEvent) then
        begin
        SendResult(500,'Internal Server Error','Timeout');
        end;
      end;

    if not Terminated then
      begin
      if FResultEvent.NotificationType=ntFailedCommand then
        SendResult(500,'Internal Server Error',FResultEvent.Message)
      else
        SendResult(200,'OK',FRestServer.InOutputProcessor.EventToText(FResultEvent));
      end;
  except
    on E: Exception do
      begin
      FDistributor.Log(Format('Exception raised by HTTP-connection (%s): %s', [HostAddrToStr(FData.RemoteAddress.sin_addr), E.Message]), etWarning, Null);
      end;
  end;
end;

constructor TDCSRestCallThread.Create(Distributor: TDCSDistributor; RestServer: TDCSHTTPRestServer;
  UID: string; Data: TSocketStream);
begin
  inherited Create(False);
  FRestServer := RestServer;
  FData := Data;
  FUID := UID;
  FDistributor := Distributor;
  FResultReceived := RTLEventCreate;
end;

destructor TDCSRestCallThread.Destroy;
begin
  RTLeventdestroy(FResultReceived);
  FRestServer.RemoveRestThread(self);
  FData.Free;
  if Assigned(FResultEvent) then
    FResultEvent.Release;
  inherited Destroy;
end;

procedure TDCSRestCallThread.SetResultEvent(AnEvent: TDCSNotificationEvent);
begin
  FResultEvent := AnEvent;
  FResultEvent.AddRef;
  RTLeventSetEvent(FResultReceived);
end;

{ TDCSHTTPRestServer }

function TDCSHTTPRestServer.CreateInetServer: TInetServer;
var
  InetServer: TInetServer;
begin
  result := nil;

  InetServer := TInetServer.Create(FPort);
  try
    InetServer.Listen;
    result := InetServer;
  except
    on E: Exception do
      begin
      InetServer.Free;
      FDistributor.SendNotification(-1, ntListenerMessage, null, 'Listening for incoming TCP-connections on port %d', '', [FPort]);
      if (E is ESocketError) and (ESocketError(E).Code=seBindFailed) then
        begin
        FDistributor.SendNotification(-1, ntConnectionProblem, null, 'Failed to start listening for incoming TCP-connections: %s', '', [e.Message])
        end
      else
        Raise;
      end;
  end;
end;

procedure TDCSHTTPRestServer.FTCPConnectionConnect(Sender: TObject; Data: TSocketStream);
var
  RestCallThread: TDCSRestCallThread;
begin
  // Without this, on Linux at least, Data.Write can generate a SIGPIPE exception
  // when the other side disconnects unexpectedly. We do not want this and use the
  // return value to detect the lost connection.
  Data.WriteFlags := $4000; // MSG_NOSIGNAL: do not generate SIGPIPE on connection end

  Inc(FCallNr);
  if FListenerStr='' then
    FListenerStr := format('%.5d_',[GetListenerID]);

  RestCallThread:=TDCSRestCallThread.create(FDistributor, Self, FListenerStr+IntToStr(FCallNr), data);
  RestCallThread.FreeOnTerminate:=true;
  FRestThreadList.Add(RestCallThread);
end;

procedure TDCSHTTPRestServer.FTCPConnectionAcceptError(Sender: TObject; ASocket: Longint;
  E: Exception; var ErrorAction: TAcceptErrorAction);
begin
  if E is ESocketError and (ESocketError(E).Code=seAcceptFailed) and (socketerror=53) {ECONNABORTED} then
    begin
    // The socket has stopped listening. The TCP-server is shutting down...
    ErrorAction:=aeaStop;
    end
  else
    FDistributor.Log(Format('Error on accept of new http-request: %s', [E.Message]), etError, null);
end;

procedure TDCSHTTPRestServer.FTCPConnectionConnectQuery(Sender: TObject; ASocket: Longint; var Allow: Boolean);
begin
  Allow:=true;
end;

procedure TDCSHTTPRestServer.Execute;
var
  AConnection: TInetServer;
begin
  try
    FTCPConnection := CreateInetServer;
    RTLeventSetEvent(FInitializationFinished);
    if assigned(FTCPConnection) then
      begin
      try
        FTCPConnection.OnConnect:=@FTCPConnectionConnect;
        FTCPConnection.OnConnectQuery:=@FTCPConnectionConnectQuery;
        FTCPConnection.OnAcceptError:=@FTCPConnectionAcceptError;
        FTCPConnection.StartAccepting;
      finally
        AConnection:=FTCPConnection;
        FTCPConnection := nil;
        AConnection.Free;
      end;
      end
  Except
    on E: Exception do
      begin
      FDistributor.Log(Format('Exception while waiting for incoming HTTP-requests: %s', [E.Message]), etError, null);
      end;
  end;
end;

procedure TDCSHTTPRestServer.OnListenerEvent(Event: TDCSEvent);
var
  List: TList;
  Thread: TDCSRestCallThread;
  NotificationEvent: TDCSNotificationEvent;
  i: Integer;
  s: string;
begin
  if Event.EventType <> dcsetNotification then
    Exit;

  NotificationEvent := Event as TDCSNotificationEvent;

  if not (NotificationEvent.NotificationType in [ntExecutedCommand, ntFailedCommand]) then
    Exit;

  s := VarToStr(Event.UID);
  if (FListenerStr='') or (copy(s,1,6)<>FListenerStr) then
    Exit;

  List := FRestThreadList.LockList;
  try
    for i := 0 to List.Count-1 do
      begin
      Thread := TDCSRestCallThread(List.Items[i]);
      if Thread.UID = s then
        begin
        Thread.SetResultEvent(NotificationEvent);
        Break;
        end;
      end;
  finally
    FRestThreadList.UnlockList;
  end;
end;

constructor TDCSHTTPRestServer.create(ADistributor: TDCSDistributor; APort: integer);
begin
  FPort:=APort;
  FDistributor:=ADistributor;
  FRestThreadList:=TDCSRestCallThreadList.Create;
  FInOutputProcessor := TDCSHTTPInOutputProcessor.create(-1, FDistributor);
  FInitializationFinished:=RTLEventCreate;
  FListenerThread := TDCSListenerThread.Create(FDistributor, 15*MSecsPerSec, 'HTTP Rest server', nil, @OnListenerEvent);
  inherited Create(false);
end;

procedure TDCSHTTPRestServer.RemoveRestThread(ARestThread: TDCSRestCallThread);
begin
  FRestThreadList.Remove(ARestThread);
end;

procedure TDCSHTTPRestServer.ForceTerminate;
begin
  Terminate;
  if assigned(FTCPConnection) then
    FTCPConnection.StopAccepting(true);
end;

destructor TDCSHTTPRestServer.Destroy;
begin
  RTLeventdestroy(FInitializationFinished);
  FInOutputProcessor.Free;
  inherited Destroy;
end;

function TDCSHTTPRestServer.GetListenerID: integer;
begin
  Result := FListenerThread.GetListenerId;
end;

end.

