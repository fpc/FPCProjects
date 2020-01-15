unit fprWebHandler;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  TypInfo,
  fgl,
  HTTPDefs,
  fpjson,
  fpjsonrtti,
  jsonparser,
  DCSHTTPRestServer,
  dcsGlobalSettings,
  fphttpclient,
  fphttpserver,
  fprJSONRTTI,
  fprLog,
  fprWebModule,
  fprErrorHandling,
  cnocStackMessageTypes;

type

  { TfprWebHandler }

  TfprWebHandler = class
  private
    FCorsOriginList: TDCSHTTPCorsEntryList;

    FDeStreamer: TfprJSONDeStreamer;
    FStreamer: TJSONStreamer;
    function ListToJSon(AList: TFPSList): TJSONArray;
    Procedure DoRestoreProperty(Sender: TObject; AObject: TObject; Info: PPropInfo;
      AValue: TJSONData; Var Handled: Boolean);
    Procedure FStreamerStreamProperty(Sender: TObject; AObject: TObject; Info: PPropInfo;
      var Res: TJSONData);
  protected
    procedure JSONContentToObject(AContent: TJSONObject; AnObject: TObject);
    function ObjectToJSON(AnObject: TObject): TJSONData;
    function JSONObjectRestRequest(AnURL: string; AccessToken: string; AnObject: TObject; Method: string = 'GET'; Content: TStream = nil): Boolean;
    function RetrieveBuildAgentURL(FPCVersion: string; AccessToken: string): string;
    procedure JSONContentStringToObject(AContentString: string; AnObject: TObject);
    function DoHandleRequest(ARequest : TRequest; JSONContent: TJSONData): TJSONData; virtual;
    procedure HandleCors(ARequest: TRequest; AResponse: TResponse; out StopProcessing: Boolean); virtual;
    procedure RespondToJSONMessage(const IncomingMessage: PcnocStackMessage; const JSONData: TJSONObject; out AResponse: TJSONData; var Handled: Boolean); virtual;
    procedure DoRespondToJSONMessage(const IncomingMessage: PcnocStackMessage; const JSONData: TJSONObject; out AResponse: TJSONData; var Handled: Boolean); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure HandleRequest(ARequest : TRequest; AResponse : TResponse); virtual;
    procedure AddCorsOrigin(Origin, Methods, Headers: string; Credentials: Boolean);
  end;

implementation

{ TfprWebHandler }

function TfprWebHandler.ObjectToJSON(AnObject: TObject): TJSONData;
var
  JSO: TJSONData;
begin
  if AnObject is TFPSList then
    JSO := ListToJSon(TFPSList(AnObject))
  else if AnObject is TCollection then
    JSO := FStreamer.StreamCollection(TCollection(AnObject))
  else
    JSO := FStreamer.ObjectToJSON(AnObject);
  try
    Result := JSO;
    JSO := Nil;
  finally
    JSO.Free;
  end;
end;

function TfprWebHandler.ListToJSon(AList: TFPSList): TJSONArray;
var
  JA: TJSONArray;
  i : Integer;
begin
  JA:=TJSONArray.Create;
  try
    For I:=0 to AList.Count-1 do
      JA.Add(FStreamer.ObjectToJSON(TObject(AList.Items[i]^)));
    Result := JA;
    JA := nil;
  finally
    JA.Free;
  end;
end;

procedure TfprWebHandler.JSONContentToObject(AContent: TJSONObject; AnObject: TObject);
begin
  try
    FDeStreamer.JSONToObject(AContent, AnObject);
  except
    on E: Exception do
      raise EJsonWebException.CreateFmt('Failed to parse content-data: %s', [E.Message]);
  end;
end;

procedure TfprWebHandler.HandleRequest(ARequest: TRequest; AResponse: TResponse);
var
  JSONData: TJSONData;
  JSONContent: TJSONData;
  StopProcessing: boolean;
begin
  TfprLog.LogTrace('Received [' +ARequest.Method+ '] request.', ARequest.PathInfo + ':' + ARequest.Query, ARequest);

  HandleCors(ARequest, AResponse, StopProcessing);
  if not StopProcessing then
    begin
    JSONContent:=nil;
    if ARequest.Content <> '' then
      begin
      With TJSONParser.Create(ARequest.Content) do
        try
          JSONContent:=Parse;
        finally
          Free;
        end;
      end;

    JSONData := DoHandleRequest(ARequest, JSONContent);
    try
      if not Assigned(JSONData) then
        begin
        AResponse.Content := '{"error":{"msg":"Request did not return any data"}}';
        AResponse.Code := 500;
        end
      else
        begin
        AResponse.Content := JSONData.AsJSON;
        AResponse.Code := 200;
        end;
      AResponse.CodeText := GetStatusCode(AResponse.Code);
      AResponse.ContentType := 'application/json; charset=utf-8';
    finally
      JSONData.Free;
    end;
    end;
  TfprLog.LogTrace('Respond with [' + IntToStr(AResponse.Code) + ':' + AResponse.CodeText + ']', AResponse.Content);
end;

function TfprWebHandler.DoHandleRequest(ARequest: TRequest; JSONContent: TJSONData): TJSONData;
begin
  Result := nil;
end;

function TfprWebHandler.JSONObjectRestRequest(AnURL: string; AccessToken: string; AnObject: TObject; Method: string; Content: TStream): Boolean;
var
  JSonData: TJSONData;
begin
  JSonData := TfprWebModule.ObtainJSONRestRequest(AnURL, AccessToken, Method, Content );
  try
    Result := Assigned(JSonData);
    if Result then
      begin
      if (JSonData.JSONType = jtObject) then
        FDeStreamer.JSONToObject(TJSONObject(JSonData), AnObject)
      else
        begin
        Assert(AnObject is TCollection);
        FDeStreamer.JSONToCollection(TJSONArray(JSonData), TCollection(AnObject));
        end;
      end;
  finally
    JSonData.Free;
  end;
end;

function TfprWebHandler.RetrieveBuildAgentURL(FPCVersion: string; AccessToken: string): string;
var
  BuildManagerURL, URL: String;
  BuildAgentList: TJSONArray;
  I: Integer;
begin
  BuildManagerURL := IncludeHTTPPathDelimiter(TDCSGlobalSettings.GetInstance.GetSettingAsString('buildmanagerurl'));
  BuildAgentList := TfprWebModule.ObtainJSONRestRequest(BuildManagerURL+'agent/list/'+FPCVersion, AccessToken) as TJSONArray;
  try
    if BuildAgentList.Count = 0 then
      raise Exception.Create('No buildagents available');
    I := Random(BuildAgentList.Count);
    URL := IncludeHTTPPathDelimiter((BuildAgentList.Items[I] as TJSONObject).Get('url', ''));
    if URL = '' then
      raise Exception.Create('Failed to find URL for buildagent');
  finally
    BuildAgentList.Free;
  end;
  Result := URL;
end;

procedure TfprWebHandler.JSONContentStringToObject(AContentString: string; AnObject: TObject);
begin
  try
    FDeStreamer.JSONToObject(AContentString, AnObject);
  except
    on E: Exception do
      raise EJsonWebException.CreateFmt('Failed to parse content-data: %s', [E.Message]);
  end;
end;

constructor TfprWebHandler.Create;
begin
  FDeStreamer := TfprJSONDeStreamer.Create(nil);
  FDeStreamer.Options := [jdoCaseInsensitive];
  FDeStreamer.OnRestoreProperty := @DoRestoreProperty;

  FStreamer := TJSONStreamer.Create(nil);
  FStreamer.Options := [jsoLowerPropertyNames];
  FStreamer.OnStreamProperty := @FStreamerStreamProperty;

  FCorsOriginList := TDCSHTTPCorsEntryList.Create;
end;

destructor TfprWebHandler.Destroy;
begin
  FDeStreamer.Free;
  FStreamer.Free;

  FCorsOriginList.Free;

  inherited Destroy;
end;

procedure TfprWebHandler.DoRestoreProperty(Sender: TObject; AObject: TObject; Info: PPropInfo; AValue: TJSONData; var Handled: Boolean);
var
  EnumName: String;
  i, j: Integer;
begin
  if AValue.JSONType = jtNull then
    begin
    Handled := True;
    Exit;
    end;

  if Info^.PropType^.Kind = tkEnumeration then
    begin
    if AValue.JSONType = jtString then
      begin
      for i := 0 to GetEnumNameCount(Info^.PropType) -1 do
        begin
        EnumName := GetEnumName(Info^.PropType, i);
        for j := 1 to length(EnumName) do
          begin
          if upcase(EnumName[j])=EnumName[j] then
            begin
            EnumName := copy(EnumName, j, Length(EnumName));
            Break;
            end;
          end;
        if SameText(EnumName, AValue.AsString) then
          begin
          SetOrdProp(AObject, Info, i);
          Handled := True;
          end;
        end;
      end;
    end;
end;

procedure TfprWebHandler.FStreamerStreamProperty(Sender: TObject; AObject: TObject; Info: PPropInfo; var Res: TJSONData);
var
  PropObject: TObject;
  i: Integer;
  s: string;
begin
  case Info^.PropType^.Kind of
    tkClass:
      begin
      PropObject := GetObjectProp(AObject, Info);
      if PropObject is TFPSList then
        begin
        Res := ListToJSon(TFPSList(PropObject));
        end;
      end;
    tkEnumeration:
      begin
      if Res.JSONType = jtString then
        begin
        s := TJSONString(Res).AsString;
        for i := 1 to High(s) do
          begin
          if upcase(s[i]) = s[i] then
            begin
            if i > 1 then
              TJSONString(Res).AsString := Copy(s, i, length(s)-i+1);
            Break;
            end;
          end;
        end;
      end;
  end;
end;

procedure TfprWebHandler.HandleCors(ARequest: TRequest; AResponse: TResponse; out StopProcessing: Boolean);
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
    else if ARequest.Method='OPTIONS' then
      begin
      AResponse.Code := 403;
      AResponse.CodeText := GetStatusCode(AResponse.Code);
      StopProcessing := true;
      end;
    end;
end;

procedure TfprWebHandler.AddCorsOrigin(Origin, Methods, Headers: string; Credentials: Boolean);
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

procedure TfprWebHandler.RespondToJSONMessage(const IncomingMessage: PcnocStackMessage;
  const JSONData: TJSONObject; out AResponse: TJSONData; var Handled: Boolean);
begin
  try
    DoRespondToJSONMessage(IncomingMessage, JSONData, AResponse, Handled);
  except
    on E: Exception do
      begin
      AResponse := TJSONObject.Create();
      TJSONObject(AResponse).Add('error', TJSONObject.Create(['message', E.Message]));
      Handled := True;
      end;
  end;
end;

procedure TfprWebHandler.DoRespondToJSONMessage(const IncomingMessage: PcnocStackMessage;
  const JSONData: TJSONObject; out AResponse: TJSONData; var Handled: Boolean);
begin
  // Do nothing
  AResponse := nil;
end;

end.

