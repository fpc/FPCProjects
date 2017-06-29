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
  dateutils,
  typinfo,
  syncobjs,
  HTTPDefs,
  httpprotocol,
  fphttpserver,
  fphttpstatus,
  lazCollections,
  dcsHandler,
  dcsThreadCommandFactory,
  dcsInOutputProcessor,
  dcsListenerThread;

type
  TDCSRestCallThreadList = class(TThreadList);

  IDCSHTTPJSonResultEvent = interface ['{467B1B2A-4F31-467B-9D20-3BC4223B70D4}']
    function GetAsJSonString: string;
  end;

  { TDCSHTTPRestServer }

  TDCSHTTPRestServer = class(TThread)
  private
    FPort: integer;
    FDistributor: TDCSDistributor;
    FServ: TFPHttpServer;

    Procedure DoOnAllowConnect(Sender: TObject; ASocket: Longint; Var Allow: Boolean);
    Procedure DoOnRequest(Sender: TObject; Var ARequest: TFPHTTPConnectionRequest; Var AResponse: TFPHTTPConnectionResponse);
    Procedure DoOnRequestError(Sender: TObject; E: Exception);
    function StringToEventTypes(AString: string): TEventTypes;
    function StringToEventType(AString: string): TEventType;
  protected
    procedure Execute; override;
  public
    constructor create(ADistributor: TDCSDistributor; APort: integer);
    procedure ForceTerminate;
  end;

implementation

type

  { TDCSHTTPRestListener }

  TDCSHTTPRestListener = class(TObject, IDCSListener)
  private
    FEventQueue: TDCSEventQueue;
    FOrigin: string;
    FListenerId: Integer;
  public
    constructor Create(AnOrigin: string; AnEventQueue: TDCSEventQueue);

    function GetListenerId: Integer;
    procedure InitListener(AListenerId: Integer);
    procedure SendEvent(AnEvent: TDCSEvent);
    function GetOrigin: string;
    property ListenerId: Integer read GetListenerId;
  end;

  { TDCSHTTPInOutputProcessor }

  TDCSHTTPInOutputProcessor = class(TDCSJSonInOutputProcessor)
  public
    function TextToCommand(const ACommandText: string): TDCSThreadCommand; override;
    function EventToText(AnEvent: TDCSEvent): string; override;
    function TextToCommandUID(const ACommandText: string; UID: variant): TDCSThreadCommand;
  end;

  { TDCSContinousHTTPConnectionResponse }

  TDCSContinousHTTPConnectionResponse = class(TFPHTTPConnectionResponse)
  private
    FUseChunkedTransfer: Boolean;
    Procedure DoSendChunkedContent(ChunkLength: Integer);
  protected
    Procedure DoSendContent; override;
  public
    procedure SendIntermediateContent();
  end;

  { TDCSContinousHttpServer }

  TDCSContinousHttpServer = class(TFPHttpServer)
    Function CreateResponse(ARequest : TFPHTTPConnectionRequest) : TFPHTTPConnectionResponse; override;
  end;

resourcestring
  SErrHeadersAlreadySent = 'Can not send chunked (intermediate) response when the headers are already sent.';

{ TDCSContinousHTTPConnectionResponse }

Procedure TDCSContinousHTTPConnectionResponse.DoSendChunkedContent(ChunkLength: Integer);
var
  s: string;
begin
  s := IntToHex(ChunkLength, 8);
  Connection.Socket.Write(s[1], length(s));
  Connection.Socket.WriteByte(13);
  Connection.Socket.WriteByte(10);
  inherited DoSendContent;
  Connection.Socket.WriteByte(13);
  Connection.Socket.WriteByte(10);
end;

Procedure TDCSContinousHTTPConnectionResponse.DoSendContent;
begin
  if FUseChunkedTransfer then
    begin
    DoSendChunkedContent(ContentLength);
    // Send last chunk
    Connection.Socket.WriteByte(ord('0'));
    Connection.Socket.WriteByte(13);
    Connection.Socket.WriteByte(10);
    end
  else
    inherited DoSendContent;
end;

procedure TDCSContinousHTTPConnectionResponse.SendIntermediateContent;
var
  ChunkLength: Integer;
begin
  if not FUseChunkedTransfer and HeadersSent then
    Raise Exception.Create(SErrHeadersAlreadySent);
  FUseChunkedTransfer := True;
  SetHeader(hhTransferEncoding, 'chunked');
  ChunkLength := ContentLength;
  if not HeadersSent then
    begin
    SetHeader(hhContentLength, '');
    SendHeaders;
    end;
  DoSendChunkedContent(ChunkLength);
end;

{ TDCSContinousHttpServer }

Function TDCSContinousHttpServer.CreateResponse(ARequest: TFPHTTPConnectionRequest): TFPHTTPConnectionResponse;
begin
  Result:=TDCSContinousHTTPConnectionResponse.Create(ARequest);
end;

{ TDCSHTTPRestListener }

constructor TDCSHTTPRestListener.Create(AnOrigin: string; AnEventQueue: TDCSEventQueue);
begin
  FOrigin := AnOrigin;
  FEventQueue := AnEventQueue;
end;

function TDCSHTTPRestListener.GetListenerId: Integer;
begin
  Result := FListenerId;
end;

procedure TDCSHTTPRestListener.InitListener(AListenerId: Integer);
begin
  FListenerId := AListenerId;
end;

procedure TDCSHTTPRestListener.SendEvent(AnEvent: TDCSEvent);
begin
  AnEvent.AddRef;
  FEventQueue.PushItem(AnEvent);
end;

function TDCSHTTPRestListener.GetOrigin: string;
begin
  Result := FOrigin;
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
  D: Double;
  DT: TDateTime;
  b: Boolean;
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
            tkBool:
              SetOrdProp(result, APropList^[i], Ord(StrToBool(Params.ValueFromIndex[j])));
            tkFloat:
              begin
              b := false;
              try
                DT := ScanDateTime('yyyymmdd', Params.ValueFromIndex[j]);
                b := true;
              except
                // ignore
              end;
              if b then
                begin
                SetFloatProp(result, APropList^[i], DT);
                end
              else if TryStrToDate(Params.ValueFromIndex[J], DT) then
                begin
                SetFloatProp(result, APropList^[i], DT);
                end
              else if TryStrToDateTime(Params.ValueFromIndex[J], DT) then
                begin
                SetFloatProp(result, APropList^[i], DT);
                end
              else if TryStrToFloat(Params.ValueFromIndex[J], D) then
                begin
                SetFloatProp(result, APropList^[i], D);
                end
              else
                raise Exception.CreateFmt('Invalid float/datetime value for parameter %s', [APropName]);
              end;
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

{ TDCSHTTPRestServer }

Procedure TDCSHTTPRestServer.DoOnAllowConnect(Sender: TObject; ASocket: Longint; Var Allow: Boolean);
begin
  Allow := True;
end;

Procedure TDCSHTTPRestServer.DoOnRequest(Sender: TObject; Var ARequest: TFPHTTPConnectionRequest; Var AResponse: TFPHTTPConnectionResponse);
var
  ACommand: TDCSThreadCommand;
  EventQueue: TDCSEventQueue;
  Listener: TDCSHTTPRestListener;
  Event: TDCSEvent;
  NotificationEvent: TDCSNotificationEvent;
  InOutputProcessor: TDCSHTTPInOutputProcessor;
begin
  try
    EventQueue := TDCSEventQueue.create(100, INFINITE, 200);
    try
      Listener := TDCSHTTPRestListener.Create('HTTP Request from ' + ARequest.RemoteAddr, EventQueue);
      try
        FDistributor.AddListener(Listener);
        try
          FDistributor.SetEventsForListener(Listener, reOwnOnly);
          if ARequest.QueryFields.IndexOfName('loglevel') > -1 then
            FDistributor.SetLogEventsForListener(Listener, reOwnOnly, StringToEventTypes(ARequest.QueryFields.Values['loglevel']))
          else
            FDistributor.SetLogEventsForListener(Listener, reOwnOnly, [etCustom, etError, etInfo, etWarning]);
          FDistributor.SetNotificationEventsForListener(Listener, reOwnOnly, cAllNotificationTypes);

          InOutputProcessor := TDCSHTTPInOutputProcessor.create(Listener.ListenerId, FDistributor);

          ACommand := InOutputProcessor.TextToCommandUID(ARequest.URI, null);
          if not assigned(ACommand) then
            begin
            AResponse.Code := 404;
            AResponse.CodeText := 'Not Found';
            AResponse.Content := 'Page not found.';
            end
          else
            begin
            FDistributor.QueueCommand(ACommand);

            AResponse.Code := 200;
            AResponse.CodeText := 'Ok';

            while not terminated do
              begin
              if (EventQueue.PopItem(Event) = wrSignaled) then
                begin
                try
                  if Event.EventType = dcsetLog then
                    begin
                    AResponse.Content := InOutputProcessor.EventToText(Event);
                    (AResponse as TDCSContinousHTTPConnectionResponse).SendIntermediateContent;
                    end
                  else if Event.EventType = dcsetNotification then
                    begin
                    NotificationEvent := Event as TDCSNotificationEvent;

                    if (NotificationEvent.NotificationType in [ntExecutedCommand, ntFailedCommand]) then
                      begin
                      AResponse.Content := InOutputProcessor.EventToText(Event);;
                      Break;
                      end;
                    end;

                finally
                  Event.Release;
                end;
                end
              end;
            end;
        finally
          FDistributor.RemoveListener(Listener);
        end;
      finally
        Listener.Free;
      end;
    finally
      EventQueue.Free;
    end;
  except
    on E: Exception do
      begin
      if E is EHTTP then
        begin
        AResponse.Code := EHTTP(E).StatusCode;
        AResponse.CodeText := GetStatusCode(AResponse.Code);
        AResponse.Content := E.Message;
        end
      else
        begin
        AResponse.Code := 500;
        AResponse.CodeText := GetStatusCode(AResponse.Code);
        AResponse.Content := E.Message;
        end;
      end;
  end;
end;

Procedure TDCSHTTPRestServer.DoOnRequestError(Sender: TObject; E: Exception);
begin
  FDistributor.Log(Format('On-request exception: %s', [E.Message]), etError, null);
end;

function TDCSHTTPRestServer.StringToEventTypes(AString: string): TEventTypes;
var
  SL: TStringList;
  i: Integer;
begin
  Result := [];
  SL := TStringList.Create;
  try
    SL.CommaText := AString;
    for i := 0 to SL.Count -1 do
      begin
      Include(Result, StringToEventType(SL[i]));
      end;
  finally
    SL.Free;
  end;
end;

function TDCSHTTPRestServer.StringToEventType(AString: string): TEventType;
begin
  for Result := Low(TEventType) to high(TEventType) do
    begin
    if SameText(DCSLogLevelNames[Result], AString) then
      Exit;
    end;
  raise EHTTP.CreateFmtHelp('[%s] is not a valid loglevel.', [AString], 400);
end;

procedure TDCSHTTPRestServer.Execute;
begin
  try
    FServ:=TDCSContinousHttpServer.Create(Nil);
    try
      FServ.Threaded:=True;
      FServ.Port:=FPort;
      FServ.AcceptIdleTimeout:=1000;
      FServ.OnRequest := @DoOnRequest;
      FServ.OnRequestError := @DoOnRequestError;
      FServ.OnAllowConnect := @DoOnAllowConnect;
      FServ.Active:=True;
    finally
      FreeAndNil(FServ);
    end;
  Except
    on E: Exception do
      begin
      FDistributor.Log(Format('Exception while waiting for incoming HTTP-requests: %s', [E.Message]), etError, null);
      end;
  end;
end;

constructor TDCSHTTPRestServer.create(ADistributor: TDCSDistributor; APort: integer);
begin
  FPort:=APort;
  FDistributor:=ADistributor;
  inherited Create(false);
end;

procedure TDCSHTTPRestServer.ForceTerminate;
begin
  Terminate;
  if assigned(FServ) then
    FServ.Active := False;
end;

end.

