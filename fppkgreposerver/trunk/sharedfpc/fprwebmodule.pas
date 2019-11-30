unit fprWebModule;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fgl,
  typinfo,
  HTTPDefs,
  fphttpclient,
  fphttpserver,
  fpjson,
  fpjsonrtti,
  jsonparser,
  cnocOpenIDConnect,
  cnocOIDCIDToken,
  dcsGlobalSettings,
  DCSHTTPRestServer,
  fpWeb,
  fprErrorHandling,
  fprLog,
  fprJSONRTTI;

type

  { TfprWebModule }

  TfprWebModule = class(TFPWebModule)
  private
    FCorsOriginList: TDCSHTTPCorsEntryList;
    FDeStreamer: TfprJSONDeStreamer;
    FStreamer: TJSONStreamer;
    FOIDC: TcnocOpenIDConnect;
    FAuthError: string;
    Procedure DoRestoreProperty(Sender: TObject; AObject: TObject; Info: PPropInfo;
      AValue: TJSONData; Var Handled: Boolean);
    Procedure FStreamerStreamProperty(Sender: TObject; AObject: TObject; Info: PPropInfo;
      var Res: TJSONData);
    function ListToJSon(AList: TFPSList): TJSONArray;
  protected
    FSubjectId: string;
    FAccessToken: string;
    procedure DoneSession; override;
    Procedure DoBeforeRequest(ARequest: TRequest); override;
    procedure DoAfterResponse(AResponse: TResponse); override;
    procedure DoOnRequest(ARequest: TRequest; AResponse: TResponse;
      var AHandled: boolean); override;
    procedure HandleCors(ARequest: TRequest; AResponse: TResponse; out StopProcessing: Boolean); virtual;
    function ObtainJSONRestRequest(AnURL: string; IncludeAccessToken: boolean; Method: string = 'GET';
      Content: TStream = nil): TJSONData;
    function RetrieveBuildAgentURL(FPCVersion: string): string;
    function JSONObjectRestRequest(AnURL: string; IncludeAccessToken: boolean; AnObject: TObject; Method: string = 'GET';
      Content: TStream = nil): Boolean;
    procedure JSONContentStringToObject(AContentString: string; AnObject: TObject);
    function ObjectToJSONContentString(AnObject: TObject): string;
    function GetUserRole: string;
    function RequireAuthentication(ARequest: TRequest): Boolean; virtual;
    function PerformAuthentication(ARequest: TRequest): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
    procedure AddCorsOrigin(Origin, Methods, Headers: string; Credentials: Boolean);
    class function ObtainJSONRestRequest(AnURL: string; AccessToken: String; Method: string = 'GET'; Content: TStream = nil): TJSONData;
  end;

implementation

{ TfprWebModule }

procedure TfprWebModule.DoOnRequest(ARequest: TRequest; AResponse: TResponse; var AHandled: boolean);
var
  StopProcessing: boolean;
begin
  HandleCors(ARequest, AResponse, StopProcessing);
  if StopProcessing then
    begin
    AHandled := True;
    Exit;
    end;

  if not Assigned(FOIDC) and RequireAuthentication(ARequest) then
  begin
    AResponse.Code := 401;
    AResponse.CodeText := GetStatusCode(AResponse.Code);
    AResponse.Content := Format('Authentication failed (%s).', [FAuthError]);
    AHandled := True;
    Exit;
  end;

  inherited DoOnRequest(ARequest, AResponse, AHandled);
end;

function TfprWebModule.ListToJSon(AList: TFPSList): TJSONArray;
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

procedure TfprWebModule.DoneSession;
begin
  inherited DoneSession;
  FreeAndNil(FOIDC);
  FAccessToken := '';
  FSubjectId := '';
  FAuthError := '';
end;

Procedure TfprWebModule.DoBeforeRequest(ARequest: TRequest);
var
  AuthorizationToken, s: string;
  OIDC: TcnocOpenIDConnect;
  AllowRequest: boolean;
  JWT: TcnocOIDCIDTokenJWT;
begin
  inherited DoBeforeRequest(ARequest);

  TfprLog.LogTrace('Received [' +ARequest.Method+ '] request.', ARequest.PathInfo + ':' + ARequest.Query, ARequest);

  FSubjectId := '';
  FAccessToken := '';
  FreeAndNil(FOIDC);
  FAuthError := '';

  if PerformAuthentication(ARequest) then
  begin
    OIDC := TcnocOpenIDConnect.Create;
    try
      OIDC.OpenIDProvider := TDCSGlobalSettings.GetInstance.GetSettingAsString(
        'OpenIDProviderURL');

      AuthorizationToken := ARequest.Authorization;

      if copy(AuthorizationToken, 1, 7) = 'Bearer ' then
      begin
        s := copy(AuthorizationToken, 8, length(AuthorizationToken) - 7);
        AllowRequest := OIDC.VerifyJWT(s);
        if not AllowRequest then
          s := OIDC.GetLatestError
        else
        begin
          JWT := TcnocOIDCIDTokenJWT.Create;
          try
            JWT.AsEncodedString := s;
            FSubjectId := JWT.Claims.sub;
            FAccessToken := s;
            FOIDC := OIDC;
            OIDC :=nil;
         finally
            JWT.Free;
          end;
        end;
      end
      else
      begin
        AllowRequest := False;
        s := 'No authorization information';
      end;

      if not AllowRequest then
      begin
        FAuthError := Format('Authentication failed (%s).', [s]);
      end;

    finally
      FreeAndNil(OIDC);
    end;
  end;
end;

Procedure TfprWebModule.FStreamerStreamProperty(Sender: TObject; AObject: TObject; Info: PPropInfo; var Res: TJSONData);
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

Procedure TfprWebModule.DoRestoreProperty(Sender: TObject; AObject: TObject; Info: PPropInfo;
  AValue: TJSONData; Var Handled: Boolean);
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

procedure TfprWebModule.HandleCors(ARequest: TRequest; AResponse: TResponse; out
  StopProcessing: Boolean);
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

class function TfprWebModule.ObtainJSONRestRequest(AnURL: string; AccessToken: String; Method: string = 'GET'; Content: TStream = nil): TJSONData;
var
  HTTPClient: TFPHTTPClient;
  MemStream: TMemoryStream;
  JSONParser: TJSONParser;
  s: string;
begin
  HTTPClient := TFPHTTPClient.Create(nil);
  try
    if AccessToken <> '' then
      HTTPClient.RequestHeaders.Values['authorization'] := 'Bearer ' + AccessToken;
    MemStream := TMemoryStream.Create;
    try
      HTTPClient.RequestBody := Content;
      HTTPClient.HTTPMethod(Method, AnURL, MemStream, [200]);
      MemStream.Seek(0, soBeginning);

      SetLength(s, MemStream.Size);
      MemStream.Read(s[1], MemStream.Size);
      MemStream.Seek(0, soBeginning);

      JSONParser := TJSONParser.Create(MemStream, []);
      try
        Result := JSONParser.Parse;
      finally
        JSONParser.Free;
      end;
    finally
      MemStream.Free;
    end;
  finally
    HTTPClient.Free;
  end;
end;

function TfprWebModule.ObtainJSONRestRequest(AnURL: string;
  IncludeAccessToken: boolean; Method: string = 'GET'; Content: TStream = nil): TJSONData;
begin
  if IncludeAccessToken then
    Result := ObtainJSONRestRequest(AnURL, FAccessToken, Method, Content)
  else
    Result := ObtainJSONRestRequest(AnURL, '', Method, Content);
end;

function TfprWebModule.RetrieveBuildAgentURL(FPCVersion: string): string;
var
  BuildManagerURL, URL: String;
  BuildAgentList: TJSONArray;
  I: Integer;
begin
  BuildManagerURL := IncludeHTTPPathDelimiter(TDCSGlobalSettings.GetInstance.GetSettingAsString('buildmanagerurl'));
  BuildAgentList := ObtainJSONRestRequest(BuildManagerURL+'agent/list/'+FPCVersion, False) as TJSONArray;
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

function TfprWebModule.JSONObjectRestRequest(AnURL: string; IncludeAccessToken: boolean;
  AnObject: TObject; Method: string; Content: TStream): Boolean;
var
  JSonData: TJSONData;
begin
  JSonData := ObtainJSONRestRequest(AnURL, IncludeAccessToken, Method, Content );
  try
    Result := Assigned(JSonData) and (JSonData.JSONType in [jtObject, jtArray]);
    if Result then
      FDeStreamer.JSONToObject(TJSONObject(JSonData), AnObject);
  finally
    JSonData.Free;
  end;
end;

function TfprWebModule.ObjectToJSONContentString(AnObject: TObject): string;
var
  JSO: TJSONData;
begin
  try
    if AnObject is TFPSList then
      JSO := ListToJSon(TFPSList(AnObject))
    else if AnObject is TCollection then
      JSO := FStreamer.StreamCollection(TCollection(AnObject))
    else
      JSO := FStreamer.ObjectToJSON(AnObject);
    try
      Result := JSO.AsJSON;
    finally
      JSO.Free;
    end;
  finally
  end;
end;

function TfprWebModule.GetUserRole: string;
var
  JSonData: TJSONData;
begin
  if not Assigned(FOIDC) then
    raise Exception.Create('OpenIDConnect provider not assigned');

  Result := '';
  JSonData := ObtainJSONRestRequest(FOIDC.UserinfoEndpoint, True);
  try
    if JSonData.JSONType = jtObject then
      begin
      Result := (JSonData as TJSONObject).Get('role', '');
      end;
  finally
    JSonData.Free;
  end;
end;

function TfprWebModule.RequireAuthentication(ARequest: TRequest): Boolean;
begin
  Result := True;
end;

function TfprWebModule.PerformAuthentication(ARequest: TRequest): Boolean;
begin
  Result := RequireAuthentication(ARequest);
end;

procedure TfprWebModule.JSONContentStringToObject(AContentString: string; AnObject: TObject);
begin
  try
    FDeStreamer.JSONToObject(AContentString, AnObject);
  except
    on E: Exception do
      raise EJsonWebException.CreateFmt('Failed to parse content-data: %s', [E.Message]);
  end;
end;

constructor TfprWebModule.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDeStreamer := TfprJSONDeStreamer.Create(nil);
  FDeStreamer.Options := [jdoCaseInsensitive];
  FDeStreamer.OnRestoreProperty := @DoRestoreProperty;

  FStreamer := TJSONStreamer.Create(nil);
  FStreamer.Options := [jsoLowerPropertyNames];
  FStreamer.OnStreamProperty := @FStreamerStreamProperty;

  FCorsOriginList := TDCSHTTPCorsEntryList.Create;
end;

Destructor TfprWebModule.Destroy;
begin
  FDeStreamer.Free;
  FStreamer.Free;

  FCorsOriginList.Free;

  inherited Destroy;
end;

procedure TfprWebModule.AddCorsOrigin(Origin, Methods, Headers: string; Credentials: Boolean);
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

procedure TfprWebModule.DoAfterResponse(AResponse: TResponse);
begin
  inherited DoAfterResponse(AResponse);
  TfprLog.LogTrace('Respond with [' + IntToStr(AResponse.Code) + ':' + AResponse.CodeText + ']', AResponse.Content);
end;

end.
