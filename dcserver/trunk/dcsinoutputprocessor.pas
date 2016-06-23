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
  protected
    FDistributor: TDCSDistributor;
    FLisId: integer;
    function StringToEnum(TypeInfo : PTypeInfo;Value : String): integer; virtual;
  public
    constructor create(ALisId: integer; ADistributor: TDCSDistributor); virtual;
    function TextToCommand(const ACommandText: string): TDCSThreadCommand; virtual; abstract;
    function EventToText(AnEvent: TDCSEvent): string; virtual; abstract;
  end;
  TDCSInOutputProcessorClass = class of TDCSCustomInOutputProcessor;

  { TDCSJSonInOutputProcessor }

  TDCSJSonInOutputProcessor = class(TDCSCustomInOutputProcessor)
  protected
    function JSONArrayToSetOrdinal(AnArray: TJSONArray; PropInfo: PPropInfo): Int64;
  public
    function TextToCommand(const ACommandText: string): TDCSThreadCommand; override;
    function EventToText(AnEvent: TDCSEvent): string; override;
  end;

  { TDCSInOutputProcessorFactory }

  TDCSInOutputProcessorFactory = class
  private
    class var FStringList: TStringList;
  public
    class constructor Create();
    class destructor Destroy;
    class function GetInOutputProcessorByName(ATextName: string): TDCSInOutputProcessorClass;
    class procedure RegisterCommandClass(ATextName: string; AnInOutputProcessor: TDCSInOutputProcessorClass);
  end;


implementation

{ TDCSInOutputProcessorFactory }

class constructor TDCSInOutputProcessorFactory.Create;
begin
  FStringList := TStringList.Create;
end;

class destructor TDCSInOutputProcessorFactory.Destroy;
begin
  FStringList.Destroy;
end;

class function TDCSInOutputProcessorFactory.GetInOutputProcessorByName(ATextName: string): TDCSInOutputProcessorClass;
var
  i: Integer;
begin
  result := nil;
  for i := 0 to FStringList.Count -1 do
    begin
    if FStringList.Strings[i]=ATextName then
      result := TDCSInOutputProcessorClass(FStringList.Objects[i]);
    end;
end;

class procedure TDCSInOutputProcessorFactory.RegisterCommandClass(ATextName: string;
  AnInOutputProcessor: TDCSInOutputProcessorClass);
begin
  FStringList.AddObject(ATextName, TObject(AnInOutputProcessor));
end;

{ TDCSCustomInOutputProcessor }

function TDCSCustomInOutputProcessor.StringToEnum(TypeInfo: PTypeInfo; Value: String): integer;
begin
  Result := GetEnumValue(TypeInfo,Value);
end;

constructor TDCSCustomInOutputProcessor.create(ALisId: integer; ADistributor: TDCSDistributor);
begin
  FLisId:=ALisId;
  FDistributor:=ADistributor;
end;

{ TDCSJSonInOutputProcessor }

function TDCSJSonInOutputProcessor.JSONArrayToSetOrdinal(AnArray: TJSONArray; PropInfo: PPropInfo): Int64;
var
  i: Integer;
  PTI: PTypeInfo;
  EnumInd: Integer;
begin
  Result := 0;
  PTI:=GetTypeData(PropInfo^.PropType)^.Comptype;
  for i := 0 to AnArray.Count -1 do
    begin
    EnumInd := StringToEnum(PTI, AnArray.Items[i].AsString);
    if EnumInd > -1 then
      Result := Result or (1 shl EnumInd)
    else
      raise Exception.CreateFmt('Invalid set value ''%s''',[AnArray.Items[i].AsString]);
    end;
end;

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
            tkSet:
              if AJSonProp.JSONType=jtString then
                SetSetProp(result, APropList^[i], AJSonProp.AsString)
              else
                SetOrdProp(result, APropList^[i], JSONArrayToSetOrdinal(AJSonProp as TJSONArray, APropList^[i]));
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

