unit cnocOpenIDConnect;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  strutils,
  base64,
  fphttpclient,
  dateutils,
  math,
  fpjson,
  fpjwt,
  cnocRSA,
  cnocOIDCIDToken;

type
  { TcnocOpenIDConnectt }

  TcnocOpenIDConnect = class
  private
    FLatestError: string;
    FAuthorizationEndpoint: string;
    FCheckSessionIFrame: string;
    FEndSessionEndpoint: string;
    FIntrospectionEndpoint: string;
    FIssuer: string;
    FJWKsUri: string;
    FOpenIDProvider: string;
    FRevocationEndpoint: string;
    FTokenEndpoint: string;
    FUserinfoEndpoint: string;

    FJWKJsonArray: TJSONArray;
    procedure SetOpenIDProvider(AValue: string);
    function ConcatUri(const Uri1, Uri2: string): string;
  public
    function RetrieveJWKs: Boolean;
    function RetrieveEndpoints: Boolean;

    function ValidateSignature(AJWTAsString: String; AJWK: TJSONObject): Boolean;
    function VerifyJWT(AJWT: TcnocOIDCIDTokenJWT; AJWTAsString: String): Boolean; overload;
    function VerifyJWT(AJWTString: string): Boolean; overload;

    function GetLatestError: string;

    property OpenIDProvider: string read FOpenIDProvider write SetOpenIDProvider;
    property JWKsUri: string read FJWKsUri write FJWKsUri;
    property AuthorizationEndpoint: string read FAuthorizationEndpoint write FAuthorizationEndpoint;
    property TokenEndpoint: string read FTokenEndpoint write FTokenEndpoint;
    property UserinfoEndpoint: string read FUserinfoEndpoint write FUserinfoEndpoint;
    property EndSessionEndpoint: string read FEndSessionEndpoint write FEndSessionEndpoint;
    property CheckSessionIFrame: string read FCheckSessionIFrame write FCheckSessionIFrame;
    property RevocationEndpoint: string read FRevocationEndpoint write FRevocationEndpoint;
    property IntrospectionEndpoint: string read FIntrospectionEndpoint write FIntrospectionEndpoint;
    property Issuer: string read FIssuer write FIssuer;
    // ToDo: this property should not be here, especially not writable.
    property JWKJsonArray: TJSONArray read FJWKJsonArray write FJWKJsonArray;
  end;

implementation

{ TcnocOpenIDConnect }

procedure TcnocOpenIDConnect.SetOpenIDProvider(AValue: string);
begin
  if FOpenIDProvider = AValue then Exit;
  FOpenIDProvider := AValue;
end;

function TcnocOpenIDConnect.ConcatUri(const Uri1, Uri2: string): string;
begin
  Result := trim(Uri1);
  if RightStr(Uri1, 1) <> '/' then
    Result := Result + '/';
  Result := Result + Uri2;
end;

function TcnocOpenIDConnect.RetrieveJWKs: Boolean;
var
  JWKsUriContent: string;
  JSONData: TJSONData;
begin
  Result := False;
  FLatestError := '';
  if FJWKsUri = '' then
    begin
    if not RetrieveEndpoints then
      Exit;
    end;
  if FJWKsUri = '' then
    begin
    FLatestError := 'It is not possible to retrieve an JWK without a JWKsUri.';
    Exit;
    end;

  JWKsUriContent :=  TFPHTTPClient.SimpleGet(FJWKsUri);
  if JWKsUriContent = '' then
    begin
    FLatestError := Format('Failed to retrieve OpenID Provider''s JWK from %s', [FJWKsUri]);
    Exit;
    end;
  JSONData := GetJSON(JWKsUriContent);
  if JSONData.JSONType <> jtObject then
    begin
    FLatestError := Format('Received invalid JWK from url %s. Expected an object but recieved something else: %s', [FJWKsUri, JWKsUriContent]);
    Exit;
    end;

  FJWKJsonArray := TJSONObject(JSONData).Get('keys', TJSONArray(nil));
  if not Assigned(FJWKJsonArray) then
    begin
    FLatestError := Format('Received invalid JWK from url %s: %s', [FJWKsUri, JWKsUriContent]);
    Exit;
    end;
  Result := True;
end;

function TcnocOpenIDConnect.RetrieveEndpoints: Boolean;
var
  ConfigurationURL: string;
  ConfigurationContent: string;
  JSONData: TJSONData;
  Configuration: TJSONObject;
begin
  FLatestError := '';
  Result := False;
  if OpenIDProvider = '' then
    begin
    FLatestError := 'Can not retrieve endpoints when there is no OpenID Provider specified.';
    Exit
    end;

  ConfigurationURL := ConcatUri(OpenIDProvider, '.well-known/openid-configuration');
  try
    ConfigurationContent := TFPHTTPClient.SimpleGet(ConfigurationURL);
  except
    on E: Exception do
      begin
      FLatestError := Format('Failed to retrieve OpenID Provider''s configuration from [%s]: %s', [ConfigurationURL, E.Message]);
      Exit;
      end;
  end;
  if ConfigurationContent='' then
    begin
    FLatestError := Format('Failed to retrieve OpenID Provider''s configuration from [%s]', [ConfigurationURL]);
    Exit;
    end;
  JSONData := GetJSON(ConfigurationContent);
  if JSONData.JSONType <> jtObject then
    begin
    FLatestError := Format('Received invalid configuration-data from url "%s": %s', [ConfigurationURL, ConfigurationContent]);
    Exit;
    end;

  Configuration := JSONData as TJSONObject;
  FAuthorizationEndpoint := Configuration.Get('authorization_endpoint', '');
  FJWKsUri := Configuration.Get('jwks_uri', '');
  FTokenEndpoint := Configuration.Get('token_endpoint', '');
  FUserinfoEndpoint := Configuration.Get('userinfo_endpoint', '');
  FEndSessionEndpoint := Configuration.Get('end_session_endpoint', '');
  FCheckSessionIFrame := Configuration.Get('check_session_iframe', '');
  FRevocationEndpoint := Configuration.Get('revocation_endpoint', '');
  FIntrospectionEndpoint := Configuration.Get('instrospect_endpoint', '');
  FIssuer := Configuration.Get('issuer', '');
  Result := True;
end;

function TcnocOpenIDConnect.ValidateSignature(AJWTAsString: String; AJWK: TJSONObject): Boolean;
var
  RSAKey: TcnocRSAPublicKey;
  ContentString: UTF8String;
  ContentBytes: TBytes;
  SignatureBin: UTF8String;
  Signature: TBytes;
begin
  RSAKey := TcnocRSAPublicKey.Create;
  try
    RSAKey.e.AsBase64UrlEncoded := AJWK.Get('e', '');
    RSAKey.n.AsBase64UrlEncoded := AJWK.Get('n', '');

    ContentString := ExtractWord(1,AJWTAsString,['.']) + '.' + ExtractWord(2,AJWTAsString,['.']);
    SetLength(ContentBytes, Length(ContentString));
    Move(ContentString[1], ContentBytes[0], Length(ContentString));

    try
      SignatureBin := DecodeStringBase64(TBaseJWT.Base64URLToBase64(ExtractWord(3,AJWTAsString,['.'])), True);
    except
      on E: Exception do
      begin
        FLatestError := 'Invalid JWT. ' + E.Message;
        Result := False;
        Exit;
      end;
    end;
    SetLength(Signature, Length(SignatureBin));
    move(SignatureBin[1], Signature[0], Length(SignatureBin));
    try
      Result := RSAKey.Verify(ContentBytes, Signature);
      if not Result then
        FLatestError := 'Invalid signature';
    except
      on E: Exception do
      begin
        FLatestError := 'Problem during signature validation: ' + E.Message;
        Result := False;
      end;
    end;
  finally
    RSAKey.Free;
  end;
end;

function TcnocOpenIDConnect.VerifyJWT(AJWT: TcnocOIDCIDTokenJWT; AJWTAsString: String): Boolean;
var
  AmountMatchingKeys : Integer;
  JSONJwk: TJSONObject;
  i: Integer;
  CurrTimeUTC: TDateTime;
begin
  FLatestError := '';
  Result := False;
  if AJWT.Claims.iss <> Issuer then
    begin
    FLatestError := Format('Issuer of the JWT does not match. Received: %s, expected: %s', [AJWT.Claims.iss, Issuer]);
    Exit;
    end;

  CurrTimeUTC := LocalTimeToUniversal(Now);
  i := AJWT.Claims.nbf;
  if i > 0 then
    begin
    if CompareDateTime(UnixToDateTime(i), CurrTimeUTC) = GreaterThanValue then
      begin
      FLatestError := 'JWT is not valid yet';
      exit;
      end;
    end;

  i := AJWT.Claims.exp;
  if i > 0 then
    begin
    if CompareDateTime(CurrTimeUTC, UnixToDateTime(i)) = GreaterThanValue then
      begin
      FLatestError := 'JWT is expired';
      Exit;
      end;
    end;

  if AJWT.JOSE.alg <> 'RS256' then
    begin
    FLatestError := 'Failed to verify AJWT. Only RS256 supported.';
    Exit;
    end;

  if AJWT.JOSE.kid = '' then
    begin
    AmountMatchingKeys := 0;
    for i := 0 to FJWKJsonArray.Count -1 do
      begin
      JSONJwk := FJWKJsonArray.Items[i] as TJSONObject;
      if (JSONJwk.Get('kty', '') = 'RSA') and (JSONJwk.Get('use', '') = 'sig') then
        Inc(AmountMatchingKeys);
      end;

    if AmountMatchingKeys = 0 then
      begin
      FLatestError := 'Failed to verify JWT, no keys found, incorrect signature.';
      Exit;
      end;
    if AmountMatchingKeys > 1 then
      begin
      FLatestError := 'Failed to verify JWT, no ID Token kid clams in JOSE header and multiple keys supplied in jwks_uri.';
      Exit
      end;
    end
  else
    begin
    for i := 0 to FJWKJsonArray.Count -1 do
      begin
      JSONJwk := FJWKJsonArray.Items[i] as TJSONObject;

      AmountMatchingKeys := 0;
      if (JSONJwk.Get('kid', '') = AJWT.JOSE.kid) and (JSONJwk.Get('kty', '') = 'RSA') and (JSONJwk.Get('use', '') = 'sig') then
        inc(AmountMatchingKeys);

      if AmountMatchingKeys = 0 then
        begin
        FLatestError := 'Failed to verify JWT, no matching key found.';
        Exit
        end;
      if AmountMatchingKeys > 1 then
        begin
        FLatestError := 'Failed to verify JWT, multiple matching key found.';
        Exit;
        end;
      end;
    end;

  if not ValidateSignature(AJWTAsString, JSONJwk) then
    begin
    FLatestError := 'Failed to validate JWT: ' + FLatestError;
    Exit;
    end;

  result := True;
end;

function TcnocOpenIDConnect.VerifyJWT(AJWTString: string): Boolean;
var
  JWT: TcnocOIDCIDTokenJWT;
begin
  Result := False;
  FLatestError := '';
  if not Assigned(FJWKJsonArray) then
    if not RetrieveJWKs then
      Exit;

  JWT := TcnocOIDCIDTokenJWT.Create;
  try
    JWT.AsEncodedString := AJWTString;
    Result := VerifyJWT(JWT, AJWTString);
  finally
    JWT.Free;
  end;
end;

function TcnocOpenIDConnect.GetLatestError: string;
begin
  Result := FLatestError;
end;


end.

