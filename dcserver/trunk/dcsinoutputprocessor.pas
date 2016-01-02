unit dcsInOutputProcessor;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpjson,
  fpjsonrtti,
  DCSHandler,
  dcsThreadCommandFactory,
  typinfo,
  variants,
  jsonparser;

type

  { TDCSCustomInOutputProcessor }

  TDCSCustomInOutputProcessor = class
  private
    FLisId: integer;
  protected
    FDistributor: TDCSDistributor;
  public
    constructor create(ALisId: integer; ADistributor: TDCSDistributor); virtual;
    function TextToCommand(const ACommandText: string): TDCSThreadCommand; virtual; abstract;
    function EventToText(AnEvent: TDCSEvent): string; virtual; abstract;
  end;

  { TDCSJSonInOutputProcessor }

  TDCSJSonInOutputProcessor = class(TDCSCustomInOutputProcessor)
  public
    function TextToCommand(const ACommandText: string): TDCSThreadCommand; override;
    function EventToText(AnEvent: TDCSEvent): string; override;
  end;

implementation

{ TDCSCustomInOutputProcessor }

constructor TDCSCustomInOutputProcessor.create(ALisId: integer; ADistributor: TDCSDistributor);
begin
  FLisId:=ALisId;
  FDistributor:=ADistributor;
end;

{ TDCSJSonInOutputProcessor }

function TDCSJSonInOutputProcessor.TextToCommand(const ACommandText: string): TDCSThreadCommand;
var
  AJSonCommand: TJSONData;
  AJSonProp: TJSONData;
  AJSonUID: TJSONData;
  AnUID: variant;
  ACommandClass: TDCSThreadCommandClass;
  s: string;
  i: integer;
  APropCount: integer;
  APropList: PPropList;
  APropName: string;
begin
  result := nil;
  try
    AJSonCommand := GetJSON(ACommandText);
  except
    on E: Exception do
      begin
      FDistributor.SendNotification(FLisId, ntInvalidCommand, NULL, 'Command "%s" is not a valid JSON string: %s', ACommandText, [ACommandText, e.Message]);
      Exit;
      end;
  end;
  if not assigned(AJSonCommand) then
    begin
    FDistributor.SendNotification(FLisId, ntInvalidCommand, NULL, 'Command "%s" is not a valid JSON string.', ACommandText, [ACommandText]);
    exit;
    end;

  try
    if AJSonCommand.JSONType<>jtObject then
      begin
      FDistributor.SendNotification(FLisId, ntInvalidCommand, NULL, 'Command "%s" is not a JSON-object.', ACommandText, [ACommandText]);
      exit;
      end;
    s := TJSONObject(AJSonCommand).Get('command', '');
    if s = '' then
      begin
      FDistributor.SendNotification(FLisId, ntInvalidCommand, NULL, 'Command "%s" does not contain a "command" entry.', ACommandText,[ACommandText]);
      exit;
      end;
    ACommandClass := TDCSThreadCommandFactory.GetCommandClassByName(s);
    if not assigned(ACommandClass) then
      begin
      FDistributor.SendNotification(FLisId, ntInvalidCommand, NULL, 'Command "%s" does not exist.', s, [S]);
      exit;
      end;

    AJSonUID := TJSONObject(AJSonCommand).find('uid');
    if assigned(AJSonUID) then
      AnUID := AJSonUID.Value
    else
      AnUID := null;

    result := ACommandClass.Create(FLisId, AnUID, FDistributor);
    APropCount := GetPropList(result, APropList);
    try
      for i := 0 to APropCount-1 do
        begin
        APropName := APropList^[i]^.Name;
        AJSonProp := TJSONObject(AJSonCommand).Find(LowerCase(APropName));

        if assigned(AJSonProp) then
          begin
          case APropList^[i]^.PropType^.Kind of
            tkAString, tkString, tkUString:
              SetStrProp(result, APropList^[i], AJSonProp.AsString);
            tkInteger:
              SetOrdProp(result, APropList^[i], AJSonProp.AsInteger);
          end;
          end;
        end;
    finally
      Freemem(APropList);
    end;
  finally
    AJSonCommand.Free;
  end;
end;

function TDCSJSonInOutputProcessor.EventToText(AnEvent: TDCSEvent): string;
var
  JSonEvent: TJSONObject;
  s: String;
  JSStreamer: TJSONStreamer;
begin
  JSStreamer := TJSONStreamer.Create(nil);
  try
    JSonEvent := JSStreamer.ObjectToJSON(AnEvent);
    try
      JSonEvent.Add('type',DCSEventTypeNames[AnEvent.EventType]);
      if not varisnull(AnEvent.UID) then
        begin
        if VarIsOrdinal(AnEvent.UID) then
          JSonEvent.Add('uid', integer(AnEvent.UID))
        else
          JSonEvent.Add('uid', VarToStr(AnEvent.UID));
        end;
      case AnEvent.EventType of
        dcsetLog:
          begin
          JSonEvent.Add('logLevel',DCSLogLevelNames[TDCSLogEvent(AnEvent).LogLevel]);
          JSonEvent.Add('message',TDCSLogEvent(AnEvent).Message);
          end;
        dcsetNotification:
          begin
          JSonEvent.Add('notificationType', DCSNotificationTypeNames[TDCSNotificationEvent(AnEvent).NotificationType]);
          JSonEvent.Add('message', TDCSNotificationEvent(AnEvent).Message);
          s := TDCSNotificationEvent(AnEvent).Command;
          if s <> '' then
            JSonEvent.Add('command', s);
          JSonEvent.Add('lisId', TDCSNotificationEvent(AnEvent).LisId);
          end;
        end;
      result := JSonEvent.AsJSON;
    finally
      JSonEvent.Free;
    end;
  finally
    JSStreamer.Free;
  end;
end;

end.

