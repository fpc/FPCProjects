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
  fpWeb,
  fprErrorHandling;

type

  { TfprWebModule }

  TfprWebModule = class(TFPWebModule)
  private
    FDeStreamer: TJSONDeStreamer;
    FStreamer: TJSONStreamer;
  protected
    FSubjectId: string;
    FAccessToken: string;
    procedure DoOnRequest(ARequest: TRequest; AResponse: TResponse;
      var AHandled: boolean); override;
    function ObtainJSONRestRequest(AnURL: string; IncludeAccessToken: boolean; Method: string = 'GET';
      Content: TStream = nil): TJSONData;
    procedure JSONContentStringToObject(AContentString: string; AnObject: TObject);
    function ObjectToJSONContentString(AnObject: TObject): string;
  public
    constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
  end;

implementation

{ TfprWebModule }

procedure TfprWebModule.DoOnRequest(ARequest: TRequest; AResponse: TResponse;
  var AHandled: boolean);
var
  OIDC: TcnocOpenIDConnect;
  AuthorizationToken, s: string;
  AllowRequest: boolean;
  JWT: TcnocOIDCIDTokenJWT;
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
  finally
    OIDC.Free;
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
end;

Destructor TfprWebModule.Destroy;
begin
  FDeStreamer.Free;
  FStreamer.Free;
  inherited Destroy;
end;

end.
