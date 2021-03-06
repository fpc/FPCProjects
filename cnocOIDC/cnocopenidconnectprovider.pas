unit cnocOpenIDConnectProvider;

{$mode objfpc}{$H+}
{$interfaces corba}

interface

uses
  Classes,
  SysUtils,
  contnrs,
  DateUtils,
  fgl,
  FmtBCD,
  md5,
  dcpsha256,
  base64,
  Math,
  HTTPDefs,
  fphttpserver,
  fpjson,
  fpjsonrtti,
  fpTemplate,
  URIParser,
  fpjwt,
  cnocRSA,
  TLoggerUnit,
  cnocOIDCIDToken;

type
  TcnocOpenIDConnectKty = (oidckEC, oidcRSA, oidckOct);
  TcnocOpenIDConnectUse = (oidcuSig, oidcuEnc, oidcuOther);

  { TcnocOpenIDConnectJWK }

  TcnocOpenIDConnectJWK = class(TCollectionItem)
  private
    FE: string;
    FN: string;
    FKid: string;
    FKTy: TcnocOpenIDConnectKty;
    FUse: TcnocOpenIDConnectUse;
    FUseString: string;

    function GetKty: string;
    procedure SetKty(AValue: string);

    function GetUse: string;
    procedure SetUse(AValue: string); overload;
    procedure SetUse(AValue: TcnocOpenIDConnectUse); overload;
  public
    property KTyEnum: TcnocOpenIDConnectKty read FKTy write FKTy;
    property UseEnum: TcnocOpenIDConnectUse read FUse write SetUse;
  published
    property Kty: string read GetKty write SetKty;
    property Use: string read GetUse write SetUse;
    property Kid: string read FKid write FKid;

    property E: string read FE write FE;
    property N: string read FN write FN;
  end;

  TcnocOpenIDConnectJWKCollection = TCollection;

  TcnocOpenIDSession = class
  public
    Subject: string;
    AuthenticationTime: TDateTime;
    ExpirationTime: TDateTime;
    IdToken: string;
  end;
  TcnocOpenIDSessionMap = specialize TFPGMapObject<string, TcnocOpenIDSession>;

  TcnocOpenIDLogoutInfo = class
  public
    ExpirationTime: TDateTime;
    PostLogoutRedirectUri: string;
    IdToken: string;
  end;
  TcnocOpenIDLogoutInfoMap = specialize TFPGMapObject<string, TcnocOpenIDLogoutInfo>;

  TcnocOpenIDRevokedToken = class
  public
    ExpirationTime: TDateTime;
  end;
  TcnocOpenIDRevokedTokenMap = specialize TFPGMapObject<string, TcnocOpenIDRevokedToken>;

  { TcnocOpenIDConnectConfiguration }

  TcnocOpenIDConnectConfiguration = class
  private
    FStringValues: array[0..5] of string;
    FBaseURL: string;
    procedure SetBaseURL(AValue: string);

    function GetFullValue(AIndex: Integer): string;
    function GetRelValue(AIndex: Integer): string;
    procedure SetRelValue(AIndex: Integer; AValue: string);
  public
    constructor Create;
    property BaseURL: string read FBaseURL write SetBaseURL;

    property RelIssuer: string index 0 read GetRelValue write SetRelValue;
    property RelAuthorization_Endpoint: string index 1 read GetRelValue write SetRelValue;
    property RelJwks_URI: string index 2 read GetRelValue write SetRelValue;
    property RelUserinfo_Endpoint: string index 3 read GetRelValue write SetRelValue;
    property RelCheck_Session_Iframe: string index 4 read GetRelValue write SetRelValue;
    property RelEnd_Session_Endpoint: string index 5 read GetRelValue write SetRelValue;
  published
    property Issuer: string index 0 read GetFullValue;
    property Authorization_Endpoint: string index 1 read GetFullValue;
    property Jwks_URI: string index 2 read GetFullValue;
    property Userinfo_Endpoint: string index 3 read GetFullValue;
    property Check_Session_Iframe: string index 4 read GetFullValue;
    property End_Session_Endpoint: string index 5 read GetFullValue;
  end;

  { TcnocOpenIDConnectProvider }

  TcnocOpenIDConnectProvider = class
  private
    FSessionExpiration: Integer;
    FTokenExpiration: Integer;
  type
    TcnocRSAPrivateKeyMap = specialize TFPGMapObject<string, TcnocRSAPrivateKey>;
  class var{threadvar -- Lazarus can not handle this}
    FReturnUrl: string;
    FRedirectUrl: string;
    FFailureMessage: string;
  var
    FBaseURL: string;
    FConfiguration: TcnocOpenIDConnectConfiguration;
    FJSONStreamer: TJSONStreamer;
    FLoginTemplate: TFPCustomTemplate;
    FLogoutTemplate: TFPCustomTemplate;
    FProtocol: string;
    FKeyList: TcnocRSAPrivateKeyMap;
    FContentProviderList: TFPObjectList;
    FAuthenticatedSessionMap: TcnocOpenIDSessionMap;
    FLogoutInfoMap: TcnocOpenIDLogoutInfoMap;
    FRevokedTokenMap: TcnocOpenIDRevokedTokenMap;

    Procedure GetLoginTemplateParams(Sender: TObject; Const ParamName: String; Out AValue: String);
    Procedure GetLogoutTemplateParams(Sender: TObject; Const ParamName: String; Out AValue: String);
    procedure SetBaseURL(AValue: string);
    procedure InitDefaultTemplates;
    procedure AddCorsHeaders(ARequest: TRequest; AResponse: TResponse);
    function SHA256Hash(const AString: string): ansistring;

    function SetupAuthenticatedSession(Subject: string): string;

    function VerifyJWT(const AJWTAsString: string; out AJWT: TcnocOIDCIDTokenJWT): Boolean;
    function ObtainJWKJsonArray: TJSONArray;
    procedure RevokeToken(Token: string);
  public
    constructor Create(ABaseURL: string);
    destructor Destroy; override;
    procedure HandleRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse; Var Handled: Boolean);
    procedure HandleWellKnown(Sender: TObject; ARequest: TRequest; AResponse: TResponse; Var Handled: Boolean);
    procedure HandleAuthorizationEndpoint(Sender: TObject; ARequest: TRequest; AResponse: TResponse; Var Handled: Boolean);
    procedure HandleAuthorizationEndpointLogin(Sender: TObject; ARequest: TRequest; AResponse: TResponse; Var Handled: Boolean);
    procedure HandleAuthorizationEndpointLogout(Sender: TObject; ARequest: TRequest; AResponse: TResponse; Var Handled: Boolean);
    procedure HandleUserInfoEndpoint(Sender: TObject; ARequest: TRequest; AResponse: TResponse; Var Handled: Boolean);
    procedure HandleEndSessionEndpoint(Sender: TObject; ARequest: TRequest; AResponse: TResponse; Var Handled: Boolean);
    procedure HandleAccount(Sender: TObject; ARequest: TRequest; AResponse: TResponse; Var Handled: Boolean);
    procedure SignJWT(AJwt: TJWT; AKey: TcnocRSAPrivateKey);

    procedure GenerateNewKey(Bits: Integer = 2048; Exp: Integer = 65537);

    procedure AddContentProvider(AnObject: TObject);
    procedure RemoveContentProvider(AnObject: TObject);

    property Configuration: TcnocOpenIDConnectConfiguration read FConfiguration;
    property BaseURL: string read FBaseURL;
    property Protocol: string read FPRotocol write FProtocol;
    property LoginTemplate: TFPCustomTemplate read FLoginTemplate;
    property LogoutTemplate: TFPCustomTemplate read FLogoutTemplate;
    property SessionExpiration: Integer read FSessionExpiration write FSessionExpiration;
    property TokenExpiration: Integer read FTokenExpiration write FTokenExpiration;
  end;

  IcnocOpenIDConnectLoginCheck = Interface['{D40636C3-BB4B-4F2F-82FE-FEB5567A7BF4}']
    procedure IsValidLogin(const Provider: TcnocOpenIDConnectProvider; const AUserName, APassword: string; const ARequest: TRequest; var Handled: Boolean; var Subject: string; out FailureMessage: string);
  end;

  IcnocOpenIDConnectUsedEndpointJSON = Interface['{9F1F9D1A-9A02-4E67-A795-298C83F6E798}']
    function ObtainUserInfoEndpoint(const Provider: TcnocOpenIDConnectProvider; const Subject: string): TJSONObject;
  end;

const
  cnocOpenIDConnectKtyStr: array[TcnocOpenIDConnectKty] of string = ('EC', 'RSA', 'oct');
  cnocOpenIDConnectUseStr: array[TcnocOpenIDConnectUse] of string = ('sig', 'enc', '');


implementation

uses
  cnocOpenIDConnect;

{ TcnocOpenIDConnectJWK }

function TcnocOpenIDConnectJWK.GetKty: string;
begin
  Result := cnocOpenIDConnectKtyStr[FKTy];
end;

function TcnocOpenIDConnectJWK.GetUse: string;
begin
  if FUse<>oidcuOther then
    Result := cnocOpenIDConnectUseStr[FUse]
  else
    Result := FUseString;
end;

procedure TcnocOpenIDConnectJWK.SetKty(AValue: string);
var
  i: TcnocOpenIDConnectKty;
begin
  for i := Low(i) to High(i) do
    if AValue=cnocOpenIDConnectKtyStr[i] then
      begin
      FKTy := i;
      Break;
      end;
  raise Exception.CreateFmt('Invalid value [%s] for kty.', [AValue]);
end;

procedure TcnocOpenIDConnectJWK.SetUse(AValue: string);
var
  i: TcnocOpenIDConnectUse;
begin
  FUseString := '';
  for i := Low(i) to High(i) do
    if AValue=cnocOpenIDConnectUseStr[i] then
      begin
      FUse := i;
      Break;
      end;
  FUse := oidcuOther;
  FUseString := AValue;
end;

procedure TcnocOpenIDConnectJWK.SetUse(AValue: TcnocOpenIDConnectUse);
begin
  if FUse = AValue then Exit;
  if FUse=oidcuOther then
    raise Exception.Create('For usage of custom use''s, set the property as string');
  FUse := AValue;
end;

{ TcnocOpenIDConnectConfiguration }

procedure TcnocOpenIDConnectConfiguration.SetBaseURL(AValue: string);
begin
  if FBaseURL = AValue then Exit;
  FBaseURL := IncludeHTTPPathDelimiter(AValue);
end;

procedure TcnocOpenIDConnectConfiguration.SetRelValue(AIndex: Integer; AValue: string);
begin
  FStringValues[AIndex] := AValue;
end;

constructor TcnocOpenIDConnectConfiguration.Create;
begin
  RelAuthorization_Endpoint := 'connect/authorize';
  RelJwks_URI := '.well-known/openid-configuration/jwks';
  RelUserinfo_Endpoint := 'connect/userinfo';
  RelCheck_Session_Iframe := 'connect/checksession';
  RelEnd_Session_Endpoint := 'endsession';
  RelIssuer := '';
end;

function TcnocOpenIDConnectConfiguration.GetFullValue(AIndex: Integer): string;
begin
  if (FStringValues[AIndex] = '') and (RightStr(FBaseURL,1) = '/') then
    begin
    Result := Copy(FBaseURL, 1, length(FBaseURL) -1);
    end
  else
    Result := BaseURL + FStringValues[AIndex];
end;

function TcnocOpenIDConnectConfiguration.GetRelValue(AIndex: Integer): string;
begin
  result := FStringValues[AIndex];
end;

{ TcnocOpenIDConnect }

procedure TcnocOpenIDConnectProvider.SetBaseURL(AValue: string);
begin
  if FBaseURL = AValue then Exit;
  FBaseURL := IncludeHTTPPathDelimiter(AValue);
end;

Procedure TcnocOpenIDConnectProvider.GetLoginTemplateParams(Sender: TObject; Const ParamName: String; Out AValue: String);
begin
  if SameText(ParamName, 'returnurl') then
    AValue := FReturnUrl
  else if SameText(ParamName, 'failuremessage') then
    AValue := FFailureMessage
  else
    AValue := '';
end;

Procedure TcnocOpenIDConnectProvider.GetLogoutTemplateParams(Sender: TObject; Const ParamName: String; Out AValue: String);
begin
  if SameText(ParamName, 'redirecturl') then
    AValue := FRedirectUrl
  else
    AValue := '';
end;

procedure TcnocOpenIDConnectProvider.InitDefaultTemplates;
begin
  FLoginTemplate.Template :=
    '<!DOCTYPE html><html>' +

    '<head><meta charset="utf-8"/><meta http-equiv="X-UA-Compatible" content="IE=edge"/><meta name="viewport" content="width=device-width, initial-scale=1.0"/><title>Login</title></head>' +
    '<body><form action="./login" method="post">' +

    '<input type="hidden" id="ReturnUrl" name="ReturnUrl" value="{returnurl}" />' +
    '<p>{failuremessage}</p>' +
    '<label for="Username">Username</label>' +
    '<input class="form-control" placeholder="Username" autofocus type="text" data-val="true" data-val-required="The Username field is required." id="Username" name="Username" value="">' +

    '<label for="Password">Password</label>' +
    '<input type="password" class="form-control" placeholder="Password" autocomplete="off" data-val="true" data-val-required="The Password field is required." id="Password" name="Password">' +


    '<label for="RememberLogin">' +
    '<input data-val="true" data-val-required="The RememberLogin field is required." id="RememberLogin" name="RememberLogin" type="checkbox" value="true">Remember My Login' +
    '</label>' +

    '<div class="form-group"><button class="btn btn-primary">Login</button></div>' +

    '</form></body>' +
    '</html>';

  FLogoutTemplate.Template :=
    '<!DOCTYPE html><html>' +

    '<head><meta charset="utf-8"/><meta http-equiv="X-UA-Compatible" content="IE=edge"/><meta name="viewport" content="width=device-width, initial-scale=1.0"/><title>Logged out</title></head>' +
    '<body><h2>Logout</h2><p>You have succesfully logged out. Click <a href="{redirecturl}">here</a> to continue.</body>' +
    '</html>';
end;

procedure TcnocOpenIDConnectProvider.AddCorsHeaders(ARequest: TRequest; AResponse: TResponse);
var
  Origin: String;
begin
  Origin := ARequest.CustomHeaders.Values['Origin'];
  if Origin<>'' then
    begin
    // CORS
    AResponse.CustomHeaders.Values['Access-Control-Allow-Origin'] := Origin;
    AResponse.CustomHeaders.Values['Access-Control-Allow-Headers'] := 'content-type,authorization';
    end;
end;

function TcnocOpenIDConnectProvider.SHA256Hash(const AString: string): ansistring;
var
  HashEngine: TDCP_sha256;
  Buf: TBytes;
  i: Integer;
begin
  HashEngine := TDCP_sha256.Create(nil);
  try
    HashEngine.Init;
    HashEngine.UpdateStr(AString);
    SetLength(Buf, HashEngine.GetHashSize div 8);
    HashEngine.Final(Buf[0]);
  finally
    HashEngine.Free;
  end;
  SetLength(Result, length(Buf));
  move(Buf[0], Result[1], length(buf));
end;

function TcnocOpenIDConnectProvider.SetupAuthenticatedSession(Subject: string): string;
var
  Session: TcnocOpenIDSession;
  CookieStr: string;
  i: Integer;
begin
  Session := TcnocOpenIDSession.Create;
  try
    CookieStr := '';
    for i := 0 to 15 do
      CookieStr := CookieStr + IntToHex(Random(MaxInt), 8);
    Session.AuthenticationTime := LocalTimeToUniversal(Now);
    Session.ExpirationTime := IncSecond(Session.AuthenticationTime, FSessionExpiration);
    Session.Subject := Subject;
    FAuthenticatedSessionMap.Add(CookieStr, Session);
    Session := nil;
    Result := CookieStr;
  finally
    Session.Free
  end;

end;

function TcnocOpenIDConnectProvider.VerifyJWT(const AJWTAsString: string; out AJWT: TcnocOIDCIDTokenJWT): Boolean;
var
  OIDCHandler: TcnocOpenIDConnect;
  JWT: TcnocOIDCIDTokenJWT;
begin
  OIDCHandler := TcnocOpenIDConnect.Create;
  try
    OIDCHandler.Issuer := FConfiguration.Issuer;
    OIDCHandler.JWKsUri := FConfiguration.Jwks_URI;
    OIDCHandler.JWKJsonArray := ObtainJWKJsonArray;

    JWT := TcnocOIDCIDTokenJWT.Create;
    try
      JWT.AsEncodedString := AJWTAsString;
      Result := OIDCHandler.VerifyJWT(JWT, AJWTAsString);
      if not Result then
        TLogger.GetInstance('cnocOIDC').Debug('Token not valid: ' + OIDCHandler.GetLatestError);
      AJWT := JWT;
      JWT := nil;
    finally
      JWT.Free;
    end;
  finally
    OIDCHandler.Free;
  end;
end;

function TcnocOpenIDConnectProvider.ObtainJWKJsonArray: TJSONArray;
var
  JWKCollection: TcnocOpenIDConnectJWKCollection;
  i: Integer;
  RsaKey: TcnocRSAPrivateKey;
  JWK: TcnocOpenIDConnectJWK;
  JSONObj: TJSONObject;
begin
  JWKCollection := TcnocOpenIDConnectJWKCollection.Create(TcnocOpenIDConnectJWK);
  try
    for i := 0 to FKeyList.Count -1 do
      begin
      RsaKey := FKeyList.Data[i];
      JWK := JWKCollection.Add as TcnocOpenIDConnectJWK;
      JWK.E := RsaKey.e.AsBase64UrlEncoded;
      JWK.N := RsaKey.n.AsBase64UrlEncoded;
      JWK.KTyEnum := oidcRSA;
      JWK.UseEnum := oidcuSig;
      JWK.Kid := FKeyList.Keys[i];
      end;

    Result := FJSONStreamer.StreamCollection(JWKCollection);
  finally
    JWKCollection.Free;
  end;
end;

procedure TcnocOpenIDConnectProvider.RevokeToken(Token: string);
var
  RevokeInfo: TcnocOpenIDRevokedToken;
begin
  RevokeInfo := TcnocOpenIDRevokedToken.Create;
  try
    RevokeInfo.ExpirationTime := DateTimeToUnix(IncSecond(LocalTimeToUniversal(Now), FTokenExpiration));
    FRevokedTokenMap.Add(Token, RevokeInfo);
    RevokeInfo := nil;
  finally
    RevokeInfo.Free;
  end;
end;

constructor TcnocOpenIDConnectProvider.Create(ABaseURL: string);
begin
  SetBaseURL(ABaseURL);
  FJSONStreamer := TJSONStreamer.Create(nil);
  FJSONStreamer.Options := FJSONStreamer.Options + [jsoLowerPropertyNames];
  FConfiguration := TcnocOpenIDConnectConfiguration.Create;
  FConfiguration.BaseURL := ABaseURL;
  FProtocol := 'https://';
  FKeyList := TcnocRSAPrivateKeyMap.Create(True);
  FSessionExpiration := SecsPerDay * 30;
  FTokenExpiration := 3600;

  FLoginTemplate := TFPCustomTemplate.Create;
  FLoginTemplate.OnGetParam := @GetLoginTemplateParams;
  FLogoutTemplate := TFPCustomTemplate.Create;
  FLogoutTemplate.OnGetParam := @GetLogoutTemplateParams;
  FContentProviderList := TFPObjectList.Create(False);
  FAuthenticatedSessionMap := TcnocOpenIDSessionMap.Create(True);
  FAuthenticatedSessionMap.Sorted := True;
  FRevokedTokenMap := TcnocOpenIDRevokedTokenMap.Create;
  FRevokedTokenMap.Sorted := True;
  FLogoutInfoMap := TcnocOpenIDLogoutInfoMap.Create(True);
  FLogoutInfoMap.Sorted := True;
  InitDefaultTemplates;
  if RandSeed=0 then
    Randomize;
end;

destructor TcnocOpenIDConnectProvider.Destroy;
begin
  FContentProviderList.Free;
  FAuthenticatedSessionMap.Free;
  FLogoutInfoMap.Free;
  FRevokedTokenMap.Free;
  FLoginTemplate.Free;
  FLogoutTemplate.Free;
  FJSONStreamer.Free;
  FKeyList.Free;
  inherited Destroy;
end;

procedure TcnocOpenIDConnectProvider.HandleRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse; Var Handled: Boolean);
var
  Command: String;
  RelPath: String;
begin
  if ARequest.Method='OPTIONS' then
    begin
    AddCorsHeaders(ARequest, AResponse);
    AResponse.Code := 200;
    AResponse.CodeText := GetStatusCode(AResponse.Code);
    Handled := True;
    Exit;
    end;

  Command := ARequest.GetNextPathInfo;
  case Command of
    '.well-known': HandleWellKnown(Sender, ARequest, AResponse, Handled);
    'account':     HandleAccount(Sender, ARequest, AResponse, Handled);
  else
    begin
    RelPath := Command;
    Command := ARequest.GetNextPathInfo;
    while Command <> '' do
      begin
      RelPath := IncludeHTTPPathDelimiter(RelPath) + Command;
      Command := ARequest.GetNextPathInfo;
      end;
    if RelPath=FConfiguration.RelAuthorization_Endpoint then
      HandleAuthorizationEndpoint(Sender, ARequest, AResponse, Handled)
    else if RelPath=IncludeHTTPPathDelimiter(FConfiguration.RelAuthorization_Endpoint)+'login' then
      HandleAuthorizationEndpointLogin(Sender, ARequest, AResponse, Handled)
    else if RelPath=IncludeHTTPPathDelimiter(FConfiguration.RelAuthorization_Endpoint)+'logout' then
      HandleAuthorizationEndpointLogout(Sender, ARequest, AResponse, Handled)
    else if RelPath=FConfiguration.RelUserinfo_Endpoint then
      HandleUserInfoEndpoint(Sender, ARequest, AResponse, Handled)
    else if RelPath=FConfiguration.RelEnd_Session_Endpoint then
      HandleEndSessionEndpoint(Sender, ARequest, AResponse, Handled);
    end;
  end;

  if Handled then
    begin
    AddCorsHeaders(ARequest, AResponse);
    end;
end;


procedure TcnocOpenIDConnectProvider.HandleAuthorizationEndpoint(Sender: TObject; ARequest: TRequest; AResponse: TResponse; Var Handled: Boolean);
var
  client_id: string;
  redirect_uri: string;
  nonce: string;
  response_type: string;
  scope: string;
  state: string;

  ReturnUrl: string;
begin
  //No checks whatsoever...
  client_id := ARequest.QueryFields.Values['client_id'];
  redirect_uri := ARequest.QueryFields.Values['redirect_uri'];
  nonce := ARequest.QueryFields.Values['nonce'];
  response_type := ARequest.QueryFields.Values['response_type'];
  scope := ARequest.QueryFields.Values['scope'];
  state := ARequest.QueryFields.Values['state'];

  ReturnUrl := IncludeHTTPPathDelimiter(FConfiguration.Authorization_Endpoint)+Format('login?response_type=%s&client_id=%s&redirect_uri=%s&scope=%s&state=%s&nonce=%s',
    [response_type, client_id, redirect_uri, scope, state, nonce]);

  AResponse.Code := 302;
  AResponse.CodeText := GetStatusCode(AResponse.Code);
  AResponse.Location := FBaseURL + Format('account/login?returnurl=%s',[HTTPEncode(ReturnUrl)]);
  Handled := True;
end;

procedure TcnocOpenIDConnectProvider.HandleAuthorizationEndpointLogin(Sender: TObject; ARequest: TRequest; AResponse: TResponse; Var Handled: Boolean);
var
  redirect_uri: string;
  access_token: ansistring;
  id_token: ansistring;
  state: string;
  expires_in: integer;
  AccesJwt: TcnocOIDCIDTokenJWT;
  IdJwt: TcnocOIDCIDTokenJWT;
  CurrDateTime: TDateTime;
  OIDCClaim: TcnocOIDCIDTokenClaim;
  Key: TcnocRSAPrivateKey;
  KeyIndex, SessionIdx: Integer;
  Hash, AuthCookie: string;
  AuthSession: TcnocOpenIDSession;
begin
  Handled := true;

  AuthCookie := ARequest.CookieFields.Values['Authentication'];
  if not FAuthenticatedSessionMap.Find(AuthCookie, SessionIdx) then
    begin
    TLogger.GetInstance('cnocOIDC').Debug('Authorization-endpoint login refused, missing or invalid authentication cookie');
    AResponse.Code := 403;
    AResponse.CodeText := GetStatusCode(AResponse.Code);
    Exit;
    end
  else
    AuthSession := FAuthenticatedSessionMap.Data[SessionIdx];

  if CompareDateTime(LocalTimeToUniversal(Now), AuthSession.ExpirationTime) = GreaterThanValue then
    begin
    TLogger.GetInstance('cnocOIDC').Debug('Authorization-endpoint login refused, session expired');
    FAuthenticatedSessionMap.Delete(SessionIdx);
    AResponse.Code := 403;
    AResponse.CodeText := GetStatusCode(AResponse.Code);
    Exit;
    end;

  // See Openid-Connect-Core 1.0 specs, section 3.1.3.3
  AResponse.Pragma := 'no-cache';
  AResponse.CacheControl := 'no-store';

  CurrDateTime := LocalTimeToUniversal(Now);

  if FKeyList.Count = 0 then
    raise Exception.Create('No key available to sign the tokens.');
  KeyIndex := FKeyList.Count -1;
  Key := FKeyList.Data[KeyIndex];

  AccesJwt := TcnocOIDCIDTokenJWT.Create;
  try
    AccesJwt.JOSE.alg := 'RS256';
    AccesJwt.JOSE.typ := 'JWT';
    AccesJwt.JOSE.kid := FKeyList.Keys[KeyIndex];

    OIDCClaim := AccesJwt.Claims as TcnocOIDCIDTokenClaim;
    OIDCClaim.nbf := DateTimeToUnix(CurrDateTime);
    OIDCClaim.exp := DateTimeToUnix(min(IncSecond(CurrDateTime, FTokenExpiration), AuthSession.ExpirationTime));
    OIDCClaim.iss := Configuration.Issuer;
    OIDCClaim.client_id := ARequest.QueryFields.Values['client_id'];
    OIDCClaim.auth_time := DateTimeToUnix(AuthSession.AuthenticationTime);
    OIDCClaim.aud := Configuration.Authorization_Endpoint; // ToDo: resources endpoint
    OIDCClaim.sub := AuthSession.Subject;
    // todo: role, idp, scope, amr?

    SignJWT(AccesJwt, Key);

    access_token := AccesJwt.AsEncodedString;
  finally
    AccesJwt.Free;
  end;

  IdJwt := TcnocOIDCIDTokenJWT.Create;
  try
    IdJwt.JOSE.alg := 'RS256';
    IdJwt.JOSE.typ := 'JWT';

    OIDCClaim := IdJwt.Claims as TcnocOIDCIDTokenClaim;
    OIDCClaim.aud := ARequest.QueryFields.Values['client_id'];
    OIDCClaim.nonce := ARequest.QueryFields.Values['nonce'];
    OIDCClaim.iss := Configuration.Issuer;
    OIDCClaim.nbf := DateTimeToUnix(CurrDateTime);
    OIDCClaim.iat := OIDCClaim.nbf;
    OIDCClaim.auth_time := OIDCClaim.iat; // ToDo: when a user authenticated earlier, return the time of the authentication
    OIDCClaim.exp := DateTimeToUnix(min(IncSecond(CurrDateTime, FTokenExpiration), AuthSession.ExpirationTime));
    OIDCClaim.sub := AuthSession.Subject;

    Hash := SHA256Hash(access_token);
    Hash := LeftStr(Hash, Length(Hash) div 2);
    OIDCClaim.at_hash := TBaseJWT.Base64ToBase64URL(EncodeStringBase64(Hash));

    SignJWT(IdJwt, Key);

    id_token := IdJwt.AsEncodedString;
    AuthSession.IdToken := id_token;
  finally
    IdJwt.Free;
  end;

  // See Openid-Connect-Core 1.0 specs, section 3.2.2.5
  // ToDo: only add access_token when response-type <> id_token
  state := ARequest.QueryFields.Values['state'];
  redirect_uri := ARequest.QueryFields.Values['redirect_uri'];

  AResponse.Location := redirect_uri + Format('#id_token=%s&access_token=%s&token_type=Bearer&expires_in=%d&state=%s', [id_token, access_token, expires_in, state]);

  AResponse.Code := 302;
  AResponse.CodeText := GetStatusCode(AResponse.Code);
  AResponse.Expires := '-1';

end;

procedure TcnocOpenIDConnectProvider.HandleAuthorizationEndpointLogout(Sender: TObject; ARequest: TRequest; AResponse: TResponse; Var Handled: Boolean);

  procedure SendLogoutConfirmation(LogoutRedirectURI: string);
  var
    Cookie: TCookie;
    URI: TURI;
  begin
    FRedirectUrl := LogoutRedirectURI;
    AResponse.Contents.Text := FLogoutTemplate.GetContent;
    FRedirectUrl := '';

    AResponse.Code := 200;
    AResponse.CodeText := GetStatusCode(AResponse.Code);
    AResponse.ContentType := 'text/html';
    AResponse.CustomHeaders.Values['X-Content-Type-Security-Policy'] := 'default-src ''self''';
    AResponse.CustomHeaders.Values['X-Content-Type-Options'] := 'nosniff';
    AResponse.CustomHeaders.Values['X-Frame-Options'] := 'SAMEORIGIN';
    AResponse.Pragma := 'no-cache';
    AResponse.CacheControl := 'no-cache';

    Cookie := AResponse.Cookies.Add;
    Cookie.HttpOnly := True;
    URI := ParseURI(IncludeHTTPPathDelimiter(FConfiguration.Authorization_Endpoint)+'login');
    Cookie.Path := IncludeHTTPPathDelimiter(URI.Path);
    Cookie.Name := 'Authentication';
    Cookie.Value := '';
    Cookie.Expire;
  end;

var
  LogoutId, AuthCookie: String;
  SessionIdx: Integer;
  LogoutInfo: TcnocOpenIDLogoutInfo;
  AuthSession: TcnocOpenIDSession;
begin
  Handled := True;
  LogoutId := ARequest.QueryFields.Values['logoutId'];
  if not FLogoutInfoMap.Find(LogoutId, SessionIdx) then
    begin
    TLogger.GetInstance('cnocOIDC').Debug('Authorization-endpoint logout refused, invalid logoutId');
    AResponse.Code := 403;
    AResponse.CodeText := GetStatusCode(AResponse.Code);
    Exit;
    end
  else
    LogoutInfo := FLogoutInfoMap.Data[SessionIdx];

  if CompareDateTime(LocalTimeToUniversal(Now), LogoutInfo.ExpirationTime) = GreaterThanValue then
    begin
    TLogger.GetInstance('cnocOIDC').Debug('Authorization-endpoint logout refused, logoutId expired');
    FLogoutInfoMap.Delete(SessionIdx);
    AResponse.Code := 403;
    AResponse.CodeText := GetStatusCode(AResponse.Code);
    Exit;
    end;

  AuthCookie := ARequest.CookieFields.Values['Authentication'];
  if not FAuthenticatedSessionMap.Find(AuthCookie, SessionIdx) then
    begin
    TLogger.GetInstance('cnocOIDC').Debug('Authorization-endpoint logout refused, missing or invalid authentication cookie');
    AResponse.Code := 403;
    AResponse.CodeText := GetStatusCode(AResponse.Code);
    Exit;
    end
  else
    AuthSession := FAuthenticatedSessionMap.Data[SessionIdx];

  if LogoutInfo.IdToken <> AuthSession.IdToken then
    begin
    TLogger.GetInstance('cnocOIDC').Debug('Authorization-endpoint logout refused, trying to logout for a token the user is currently not logged in with');
    AResponse.Code := 403;
    AResponse.CodeText := GetStatusCode(AResponse.Code);
    Exit;
    end;

  SendLogoutConfirmation(LogoutInfo.PostLogoutRedirectUri);

  FAuthenticatedSessionMap.Delete(SessionIdx);

  RevokeToken(LogoutInfo.IdToken);
end;

procedure TcnocOpenIDConnectProvider.HandleUserInfoEndpoint(Sender: TObject; ARequest: TRequest; AResponse: TResponse; Var Handled: Boolean);
var
  AuthorizationToken, s: String;
  JWT: TcnocOIDCIDTokenJWT;
  AllowRequest: Boolean;
  UserJson: TJSONObject;
  i: Integer;
  LoginCheckIntf: IcnocOpenIDConnectUsedEndpointJSON;
begin
  Handled := True;
  AllowRequest := False;

  AuthorizationToken := ARequest.Authorization;
  if copy(AuthorizationToken, 1, 7) = 'Bearer ' then
    begin
    s := copy(AuthorizationToken, 8, length(AuthorizationToken) - 7);
    AllowRequest := VerifyJWT(s, JWT);

    i := FRevokedTokenMap.Count;
    if FRevokedTokenMap.Find(s, i) then
      begin
      TLogger.GetInstance('cnocOIDC').Debug('Access to user-info endpoint refused, token is revoked');
      AllowRequest := False;
      end;

    if AllowRequest then
      begin
      try
        for i := 0 to FContentProviderList.Count -1 do
          begin
          if Supports(FContentProviderList.Items[i], IcnocOpenIDConnectUsedEndpointJSON, LoginCheckIntf) then
            begin
            UserJson := LoginCheckIntf.ObtainUserInfoEndpoint(Self, JWT.Claims.sub);
            try
              if Assigned(UserJson) then
                begin
                AResponse.Content := UserJson.AsJSON;
                Break;
                end;
            finally
              UserJson.Free;
            end;
            end;
          end;
        AResponse.Code := 200;
      finally
        JWT.Free;
      end;
      AResponse.Pragma := 'no-cache';
      AResponse.CacheControl := 'no-store, no-cache, max-age=0';
      end;
    end
  else
    TLogger.GetInstance('cnocOIDC').Debug('Access to user-info endpoint refused, bearer-token is missing');

  if not AllowRequest then
    begin
    AResponse.Code := 403;
    AResponse.Content := 'Access denied';
    end;
  AResponse.CodeText := GetStatusCode(AResponse.Code);
end;

procedure TcnocOpenIDConnectProvider.HandleEndSessionEndpoint(Sender: TObject; ARequest: TRequest; AResponse: TResponse; Var Handled: Boolean);
var
  LogoutInfo: TcnocOpenIDLogoutInfo;
  LogoutId: string;
  i: Integer;
begin
  LogoutId := '';
  for i := 0 to 8 do
    LogoutId := LogoutId + IntToHex(Random(MaxInt), 8);

  LogoutInfo := TcnocOpenIDLogoutInfo.Create;
  try
    LogoutInfo.IdToken := ARequest.QueryFields.Values['id_token_hint'];
    LogoutInfo.PostLogoutRedirectUri := ARequest.QueryFields.Values['post_logout_redirect_uri'];

    LogoutInfo.ExpirationTime := IncSecond(LocalTimeToUniversal(Now), 60);
    FLogoutInfoMap.Add(LogoutId, LogoutInfo);
    LogoutInfo := nil;
  finally
    LogoutInfo.Free;
  end;

  AResponse.Code := 302;
  AResponse.CodeText := GetStatusCode(AResponse.Code);
  AResponse.Location := FBaseURL + IncludeHTTPPathDelimiter(FConfiguration.RelAuthorization_Endpoint) + Format('logout?logoutId=%s', [LogoutId]);
  Handled := True;
end;

procedure TcnocOpenIDConnectProvider.HandleAccount(Sender: TObject; ARequest: TRequest; AResponse: TResponse; Var Handled: Boolean);

  procedure SendLoginForm(FailedMessage: string);
  begin
    FFailureMessage := FailedMessage;
    FReturnUrl := ARequest.QueryFields.Values['returnurl'];
    if FReturnUrl='' then
      FReturnUrl := ARequest.ContentFields.Values['returnurl'];
    AResponse.Contents.Text := FLoginTemplate.GetContent;
    FReturnUrl := '';
    FFailureMessage := '';

    AResponse.Code := 200;
    AResponse.CodeText := GetStatusCode(AResponse.Code);
    AResponse.ContentType := 'text/html';
    AResponse.CustomHeaders.Values['X-Content-Type-Security-Policy'] := 'default-src ''self''';
    AResponse.CustomHeaders.Values['X-Content-Type-Options'] := 'nosniff';
    AResponse.CustomHeaders.Values['X-Frame-Options'] := 'SAMEORIGIN';
    AResponse.Pragma := 'no-cache';
    AResponse.CacheControl := 'no-cache';
  end;

var
  Command: string;
  ReturnUrl: string;
  UserName, Password: string;
  i: Integer;
  LoginCheckIntf: IcnocOpenIDConnectLoginCheck;
  LoginHandled: Boolean;
  Subject: string;
  Err: string;
  FailureMessage: string;
  Cookie: TCookie;
  RememberLogin: Boolean;
  URI: TURI;
begin
  Command := ARequest.GetNextPathInfo;
  if Command = 'login' then
    begin
    if ARequest.Method='POST' then
      begin
      ReturnUrl := ARequest.ContentFields.Values['returnUrl'];

      UserName := ARequest.ContentFields.Values['Username'];
      Password := ARequest.ContentFields.Values['Password'];
      if not TryStrToBool(ARequest.ContentFields.Values['RememberLogin'], RememberLogin) then
        RememberLogin := False;
      Subject := '';
      LoginHandled := False;
      FailureMessage := '';;

      for i := 0 to FContentProviderList.Count -1 do
        begin
        if Supports(FContentProviderList.Items[i], IcnocOpenIDConnectLoginCheck, LoginCheckIntf) then
          begin
          LoginCheckIntf.IsValidLogin(Self, UserName, Password, ARequest, LoginHandled, Subject, Err);
          FailureMessage := FailureMessage + Err;
          end;
        end;

      if LoginHandled and (Subject<>'') then
        begin
        Cookie := AResponse.Cookies.Add;
        if RememberLogin then
          Cookie.Expires := IncSecond(LocalTimeToUniversal(Now), FSessionExpiration);
        Cookie.HttpOnly := True;
        URI := ParseURI(IncludeHTTPPathDelimiter(FConfiguration.Authorization_Endpoint)+'login');
        Cookie.Path := IncludeHTTPPathDelimiter(URI.Path);
        Cookie.Name := 'Authentication';
        Cookie.Value := SetupAuthenticatedSession(Subject);

        AResponse.Code := 302;
        AResponse.CodeText := GetStatusCode(AResponse.Code);
        AResponse.Location := ReturnUrl;
        end
      else if LoginHandled then
        begin
        SendLoginForm(FailureMessage);
        end
      else
        begin
        SendLoginForm('No login method available');
        end;
      Handled := True;
      end
    else if ARequest.Method='GET' then
      begin
      SendLoginForm('');
      Handled := True;
      end;
    end;
end;

procedure TcnocOpenIDConnectProvider.SignJWT(AJwt: TJWT; AKey: TcnocRSAPrivateKey);
var
  BaseStr: ansistring;
  BaseBytes: TBytes;
  SignBytes: TBytes;
  SignStr: String;
begin
  BaseStr := AJwt.AsEncodedString;
  SetLength(BaseBytes, Length(BaseStr));
  Move(BaseStr[1], BaseBytes[0], length(BaseStr));
  SignBytes := AKey.Sign(BaseBytes);
  SetLength(SignStr, Length(SignBytes));
  move(SignBytes[0], SignStr[1], Length(SignBytes));
  AJwt.Signature := AJwt.Base64ToBase64URL(EncodeStringBase64(SignStr));
end;

procedure TcnocOpenIDConnectProvider.GenerateNewKey(Bits: Integer; Exp: Integer);
var
  Rsa: TcnocRSAPrivateKey;
begin
  Rsa := TcnocRSAPrivateKey.Create;
  try
    rsa.Generate(Bits, Exp);
    FKeyList.Add(MD5Print(MD5String(rsa.n.AsBase64Encoded)), Rsa);
    Rsa := nil;
  finally
    Rsa.Free;
  end;
end;

procedure TcnocOpenIDConnectProvider.AddContentProvider(AnObject: TObject);
begin
  FContentProviderList.Add(AnObject);
end;

procedure TcnocOpenIDConnectProvider.RemoveContentProvider(AnObject: TObject);
begin
  FContentProviderList.Remove(AnObject);
end;

procedure TcnocOpenIDConnectProvider.HandleWellKnown(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; Var Handled: Boolean);
var
  JSONObj: TJSONObject;
begin
  if ARequest.GetNextPathInfo = 'openid-configuration' then
    begin
    if ARequest.GetNextPathInfo = 'jwks' then
      begin
      JSONObj := TJSONObject.Create();
      try
        JSONObj.Add('keys', ObtainJWKJsonArray);
        AResponse.Content := JSONObj.AsJSON;
        AResponse.Code := 200;
        AResponse.CodeText := GetStatusCode(AResponse.Code);
        AResponse.ContentType := 'application/json';
      finally
        JSONObj.Free;
      end;
      end
    else
      begin
      JSONObj := FJSONStreamer.ObjectToJSON(FConfiguration);
      try
        AResponse.Content := JSONObj.AsJSON;
        AResponse.Code := 200;
        AResponse.CodeText := GetStatusCode(AResponse.Code);
        AResponse.ContentType := 'application/json';
      finally
        JSONObj.Free;
      end;
      end;
    Handled := True;
    end;
end;

end.

