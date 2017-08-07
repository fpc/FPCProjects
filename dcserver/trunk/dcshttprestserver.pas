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
  fgl,
  syncobjs,
  fpjson,
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

  IDCSHTTPCommand = interface ['{13EE3526-2327-4828-BC6D-5E0F4E0BED50}']
    procedure FillCommandBasedOnRequest(ARequest: TRequest);
  end;

  TDCSHTTPRestServerFlag = (
    dcsRestServerFlagAllowDifferentOutputFormat
  );
  TDCSHTTPRestServerFlags = set of TDCSHTTPRestServerFlag;

  { TDCSHTTPCorsEntry }

  TDCSHTTPCorsEntry = class
  private
    FCredentials: Boolean;
    FHeaders: string;
    FMethods: string;
    FOrigin: string;
  public
    // If the origin does not match, by default the server will respond with 403
    property Origin: string read FOrigin write FOrigin;
    // If the method does not match, by default the server will respond with 405
    property Methods: string read FMethods write FMethods;
    // The headers are returned in the response when requested, but are not checked
    // by the server. If Headers is empty, the requested headers are echo'ed
    property Headers: string read FHeaders write FHeaders;
    property Credentials: Boolean read FCredentials write FCredentials;
  end;
  TDCSHTTPCorsEntryList = specialize TFPGObjectList<TDCSHTTPCorsEntry>;

  { TDCSHTTPRestServer }

  TDCSHTTPRestServer = class(TThread)
  private
    FCorsOriginList: TDCSHTTPCorsEntryList;
    FDefaultInOutputProcessorName: string;
    FFlags: TDCSHTTPRestServerFlags;
    FPort: integer;
    FDistributor: TDCSDistributor;
    FServ: TFPHttpServer;

    Procedure DoOnAllowConnect(Sender: TObject; ASocket: Longint; Var Allow: Boolean);
    Procedure DoOnRequest(Sender: TObject; Var ARequest: TFPHTTPConnectionRequest; Var AResponse: TFPHTTPConnectionResponse);
    Procedure DoOnRequestError(Sender: TObject; E: Exception);
    function StringToEventTypes(AString: string): TEventTypes;
    function StringToEventType(AString: string): TEventType;
  protected
    procedure HandleCors(ARequest: TFPHTTPConnectionRequest; AResponse: TFPHTTPConnectionResponse; out StopProcessing: Boolean); virtual;
    procedure Execute; override;
  public
    constructor create(ADistributor: TDCSDistributor; APort: integer);
    destructor Destroy; override;
    procedure ForceTerminate;
    procedure AddCorsOrigin(Origin, Methods, Headers: string; Credentials: Boolean);
    property DefaultInOutputProcessorName: string read FDefaultInOutputProcessorName write FDefaultInOutputProcessorName;
    property Flags: TDCSHTTPRestServerFlags read FFlags write FFlags;
    property CorsOriginList: TDCSHTTPCorsEntryList read FCorsOriginList;
  end;

  { TDCSHTTPInOutputProcessor }

  TDCSHTTPJsonInOutputProcessor = class(TDCSJSonInOutputProcessor)
  public
    function TextToCommand(const ACommandText: string): TDCSThreadCommand; override;
    function EventToText(AnEvent: TDCSEvent): string; override;
  end;

  { TDCSHTTPCustomInOutputProcessor }

  TDCSHTTPCustomInOutputProcessor = class(TDCSHTTPJsonInOutputProcessor)
  protected
    procedure GetBasicMsgTypeAndMessage(AnEvent: TDCSEvent; out MsgType, Msg: string);
  end;

  { TDCSHTTPBasicJSonInOutputProcessor }

  TDCSHTTPBasicJSonInOutputProcessor = class(TDCSHTTPCustomInOutputProcessor)
  protected
    function EventToJSOn(AnEvent: TDCSEvent): TJSONObject; override;
  end;

  { TDCSHTTPTextInOutputProcessor }

  TDCSHTTPTextInOutputProcessor = class(TDCSHTTPCustomInOutputProcessor)
  public
    function EventToText(AnEvent: TDCSEvent): string; override;
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

{ TDCSHTTPCustomInOutputProcessor }

procedure TDCSHTTPCustomInOutputProcessor.GetBasicMsgTypeAndMessage(AnEvent: TDCSEvent; out MsgType, Msg: string);
begin
  case AnEvent.EventType of
    dcsetEvent:
      begin
      MsgType := 'Event';
      Msg := '';
      end;
    dcsetLog:
      begin
      MsgType := DCSLogLevelNames[TDCSLogEvent(AnEvent).LogLevel];
      Msg := TDCSLogEvent(AnEvent).Message;
      end;
    dcsetNotification:
      begin
      case TDCSNotificationEvent(AnEvent).NotificationType of
        ntNewConnection     : MsgType := 'New conn';
        ntLostConnection    : MsgType := 'Lost conn';
        ntInvalidCommand    : MsgType := 'Invalid';
        ntConnectionProblem : MsgType := 'Conn failure';
        ntListenerMessage   : MsgType := 'Listener';
        ntReceivedCommand   : MsgType := 'Start';
        ntExecutedCommand   : MsgType := 'Done';
        ntFailedCommand     : MsgType := 'Failed';
      end;
      Msg := TDCSNotificationEvent(AnEvent).Message;
      end;
  end;
end;

{ TDCSHTTPTextInOutputProcessor }

function TDCSHTTPTextInOutputProcessor.EventToText(AnEvent: TDCSEvent): string;
var
  MsgType,Msg: string;
begin
  GetBasicMsgTypeAndMessage(AnEvent, MsgType, Msg);
  Result := Format('%s [%13s] %s', [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now), MsgType, Msg]);
end;

{ TDCSHTTPBasicJSonInOutputProcessor }

function TDCSHTTPBasicJSonInOutputProcessor.EventToJSOn(AnEvent: TDCSEvent): TJSONObject;
var
  MsgType,Msg: string;
begin
  Result := TJSONObject.Create;
  try
    GetBasicMsgTypeAndMessage(AnEvent, MsgType, Msg);
    Result.Add('timestamp', FormatDateTime('yyyy-mm-dd''T''hh:nn:ss', Now));
    Result.Add('type', MsgType);
    Result.Add('message', Msg);
  except
    on E: Exception do
      begin
      Result.Free;
      Raise E;
      end;
  end;
end;

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

{ TDCSHTTPJsonInOutputProcessor }

function TDCSHTTPJsonInOutputProcessor.TextToCommand(const ACommandText: string): TDCSThreadCommand;
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

  result := ACommandClass.Create(FLisId, Null, FDistributor);

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

function TDCSHTTPJsonInOutputProcessor.EventToText(AnEvent: TDCSEvent): string;
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
  InOutputProcessorClass: TDCSInOutputProcessorClass;
  InOutputProcessor: TDCSCustomInOutputProcessor;
  InOutputProcessorName: string;
  HTTPCommand: IDCSHTTPCommand;
  StopProcessing: Boolean;
begin
  try
    HandleCors(ARequest, AResponse, StopProcessing);
    if StopProcessing then
      Exit;
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

          if (dcsRestServerFlagAllowDifferentOutputFormat in Flags) and (ARequest.QueryFields.IndexOfName('outputformat') > -1) then
            InOutputProcessorName := ARequest.QueryFields.Values['outputformat']
          else
            InOutputProcessorName := DefaultInOutputProcessorName;

          InOutputProcessorClass := TDCSInOutputProcessorFactory.GetInOutputProcessorByName(InOutputProcessorName);
          if not Assigned(InOutputProcessorClass) then
            raise EHTTP.CreateFmtHelp('In-output processor class [%s] is not known.', [InOutputProcessorName], 400);
          InOutputProcessor := InOutputProcessorClass.create(Listener.ListenerId, FDistributor);

          ACommand := InOutputProcessor.TextToCommand(ARequest.URI);
          if not assigned(ACommand) then
            begin
            AResponse.Code := 404;
            AResponse.CodeText := 'Not Found';
            AResponse.Content := 'Page not found.';
            end
          else
            begin
            if supports(ACommand, IDCSHTTPCommand, HTTPCommand) then
              HTTPCommand.FillCommandBasedOnRequest(ARequest);

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

procedure TDCSHTTPRestServer.HandleCors(ARequest: TFPHTTPConnectionRequest; AResponse: TFPHTTPConnectionResponse; out StopProcessing: Boolean);
var
  i, j: Integer;
  CorsEntry: TDCSHTTPCorsEntry;
  Origin: string;
  RefuseMethod: Boolean;
  s: string;
begin
  StopProcessing := False;
  i := ARequest.CustomHeaders.IndexOfName('Origin');
  if i > -1 then
    begin
    CorsEntry := nil;
    RefuseMethod := False;
    Origin := ARequest.CustomHeaders.ValueFromIndex[i];
    for j := 0 to FCorsOriginList.Count-1 do
      begin
      if (FCorsOriginList.Items[j].Origin = '*') or (FCorsOriginList.Items[j].Origin = Origin) then
        begin
        CorsEntry := FCorsOriginList.Items[j];
        RefuseMethod := False;
        if ARequest.Method='OPTIONS' then
          begin
          s := ARequest.CustomHeaders.Values['Access-Control-Request-Method'];
          if s <> '' then
            begin
            if Pos(s, CorsEntry.Methods) = 0 then
              RefuseMethod := True
            else
              Break;
            end
          else
            Break;
          end
        else
          begin
          if Pos(ARequest.Method, CorsEntry.Methods) = 0 then
            RefuseMethod := True
          else
            Break;
          end;
        end;
      end;

    if RefuseMethod then
      begin
      AResponse.Code := 405;
      AResponse.CodeText := GetStatusCode(AResponse.Code);
      StopProcessing := True;
      end
    else if Assigned(CorsEntry) then
      begin
      AResponse.CustomHeaders.Values['Access-Control-Allow-Origin'] := CorsEntry.Origin;
      if CorsEntry.Credentials then
        AResponse.CustomHeaders.Values['Access-Control-Allow-Credentials'] := 'true';
      if ARequest.Method='OPTIONS' then
        begin
        AResponse.CustomHeaders.Values['Access-Control-Allow-Methods'] := CorsEntry.Methods;
        if ARequest.CustomHeaders.IndexOfName('Access-Control-Request-Headers') > -1 then
          begin
          if CorsEntry.Headers <> '' then
            AResponse.CustomHeaders.Values['Access-Control-Allow-Headers'] := CorsEntry.Headers
          else
            AResponse.CustomHeaders.Values['Access-Control-Allow-Headers'] := ARequest.CustomHeaders.Values['Access-Control-Request-Headers'];
          end;
        StopProcessing := true;
        end;
      end
    else
      begin
      AResponse.Code := 403;
      AResponse.CodeText := GetStatusCode(AResponse.Code);
      StopProcessing := true;
      end;

    end;
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
  FDefaultInOutputProcessorName := 'http-json-basic';
  FCorsOriginList := TDCSHTTPCorsEntryList.Create;
  inherited Create(false);
end;

destructor TDCSHTTPRestServer.Destroy;
begin
  FCorsOriginList.Free;
  inherited Destroy;
end;

procedure TDCSHTTPRestServer.ForceTerminate;
begin
  Terminate;
  if assigned(FServ) then
    FServ.Active := False;
end;

procedure TDCSHTTPRestServer.AddCorsOrigin(Origin, Methods, Headers: string; Credentials: Boolean);
var
  CorsEntry: TDCSHTTPCorsEntry;
begin
  try
    CorsEntry := TDCSHTTPCorsEntry.Create;
    CorsEntry.Origin := Origin;
    CorsEntry.Methods := Methods;
    CorsEntry.Headers := Headers;
    CorsEntry.Credentials := Credentials;
    FCorsOriginList.Add(CorsEntry);
  except
    CorsEntry.Free;
  end;
end;

initialization
  TDCSInOutputProcessorFactory.RegisterCommandClass('http-json', TDCSHTTPJsonInOutputProcessor);
  TDCSInOutputProcessorFactory.RegisterCommandClass('http-json-basic', TDCSHTTPBasicJSonInOutputProcessor);
  TDCSInOutputProcessorFactory.RegisterCommandClass('http-text', TDCSHTTPTextInOutputProcessor);
end.

