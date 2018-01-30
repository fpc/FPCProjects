unit fprWebModule;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
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
  fprErrorHandling;

type

  { TfprWebModule }

  TfprWebModule = class(TFPWebModule)
  private
    FCorsOriginList: TDCSHTTPCorsEntryList;
    FDeStreamer: TJSONDeStreamer;
    FStreamer: TJSONStreamer;
    FOIDC: TcnocOpenIDConnect;
  protected
    FSubjectId: string;
    FAccessToken: string;
    procedure DoOnRequest(ARequest: TRequest; AResponse: TResponse;
      var AHandled: boolean); override;
    procedure HandleCors(ARequest: TRequest; AResponse: TResponse; out StopProcessing: Boolean); virtual;
    function ObtainJSONRestRequest(AnURL: string; IncludeAccessToken: boolean; Method: string = 'GET';
      Content: TStream = nil): TJSONData;
    procedure JSONContentStringToObject(AContentString: string; AnObject: TObject);
    function ObjectToJSONContentString(AnObject: TObject): string;
    function GetUserRole: string;
  public
    constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
    procedure AddCorsOrigin(Origin, Methods, Headers: string; Credentials: Boolean);
  end;

implementation

{ TfprWebModule }

procedure TfprWebModule.DoOnRequest(ARequest: TRequest; AResponse: TResponse;
  var AHandled: boolean);
var
  AuthorizationToken, s: string;
  AllowRequest, StopProcessing: boolean;
  JWT: TcnocOIDCIDTokenJWT;
begin
  HandleCors(ARequest, AResponse, StopProcessing);
  if StopProcessing then
    begin
    AHandled := True;
    Exit;
    end;

  FOIDC := TcnocOpenIDConnect.Create;
  try
    FOIDC.OpenIDProvider := TDCSGlobalSettings.GetInstance.GetSettingAsString(
      'OpenIDProviderURL');

    AuthorizationToken := ARequest.Authorization;

    if copy(AuthorizationToken, 1, 7) = 'Bearer ' then
    begin
      s := copy(AuthorizationToken, 8, length(AuthorizationToken) - 7);
      AllowRequest := FOIDC.VerifyJWT(s);
      if not AllowRequest then
        s := FOIDC.GetLatestError
      else
      begin
        JWT := TcnocOIDCIDTokenJWT.Create;
        try
          JWT.AsEncodedString := s;
          FSubjectId := JWT.Claims.sub;
          FAccessToken := s;
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
      AResponse.Code := 401;
      AResponse.CodeText := GetStatusCode(AResponse.Code);
      AResponse.Content := Format('Authentication failed (%s).', [s]);
      AHandled := True;
      Exit;
    end;

    inherited DoOnRequest(ARequest, AResponse, AHandled);
  finally
    FreeAndNil(FOIDC);
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
    else
      begin
      AResponse.Code := 403;
      AResponse.CodeText := GetStatusCode(AResponse.Code);
      StopProcessing := true;
      end;

    end;
end;

function TfprWebModule.ObtainJSONRestRequest(AnURL: string;
  IncludeAccessToken: boolean; Method: string = 'GET'; Content: TStream = nil): TJSONData;
var
  HTTPClient: TFPHTTPClient;
  MemStream: TMemoryStream;
  JSONParser: TJSONParser;
  s: string;
begin
  HTTPClient := TFPHTTPClient.Create(nil);
  try
    HTTPClient.RequestHeaders.Values['authorization'] := 'Bearer ' + FAccessToken;
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

function TfprWebModule.ObjectToJSONContentString(AnObject: TObject): string;
var
  JSO: TJSONObject;
begin
  try
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
  FDeStreamer := TJSONDeStreamer.Create(Self);
  FDeStreamer.Options := [jdoCaseInsensitive];

  FStreamer := TJSONStreamer.Create(Self);
  FStreamer.Options := [jsoLowerPropertyNames];

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

end.
