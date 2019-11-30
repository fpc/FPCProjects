unit fprAuthenticationHandler;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpjson,
  cnocOpenIDConnect,
  dcsGlobalSettings,
  fprWebModule;

type

  { TfprAuthenticationHandler }

  TfprAuthenticationHandler = class
  private
    FOIDCProvider: TcnocOpenIDConnect;

    class var FInstance: TfprAuthenticationHandler;
  public
    constructor create;
    destructor Destroy; override;
    class function GetInstance: TfprAuthenticationHandler;
    function GetUserRole(const AccessToken: string): string;
    function VerifyAccessToken(const AccessToken: string; out ErrMessage: string): Boolean;
  end;

implementation

uses
  fprErrorHandling;

{ TfprAuthenticationHandler }

class function TfprAuthenticationHandler.GetInstance: TfprAuthenticationHandler;
begin
  if not Assigned(FInstance) then
    begin
    FInstance := TfprAuthenticationHandler.Create;
    end;
  Result := FInstance;
end;

function TfprAuthenticationHandler.GetUserRole(const AccessToken: string): string;
var
  JSonData: TJSONData;
begin
  if AccessToken='' then
    raise EJsonWebException.CreateHelp('Authentication token is missing', 403);

  if FOIDCProvider.UserinfoEndpoint='' then
    FOIDCProvider.RetrieveEndpoints;

  try
    Result := '';
    JSonData := TfprWebModule.ObtainJSONRestRequest(FOIDCProvider.UserinfoEndpoint, AccessToken);
    try
      if JSonData.JSONType = jtObject then
        begin
        Result := (JSonData as TJSONObject).Get('role', '');
        end;
    finally
      JSonData.Free;
    end;
  except
    on E: Exception do
      begin
      raise EJsonWebException.CreateFmtHelp('Problem occured while contacting the authentication server: [%s]', [E.Message], 500);
      end;
  end;
end;

constructor TfprAuthenticationHandler.create;
begin
  FOIDCProvider := TcnocOpenIDConnect.Create;
  FOIDCProvider.OpenIDProvider := TDCSGlobalSettings.GetInstance.GetSettingAsString('OpenIDProviderURL');

end;

destructor TfprAuthenticationHandler.Destroy;
begin
  FOIDCProvider.Free;
  inherited Destroy;
end;

function TfprAuthenticationHandler.VerifyAccessToken(const AccessToken: string; out ErrMessage: string): Boolean;
begin
  Result := FOIDCProvider.VerifyJWT(AccessToken);
  if not Result then
    ErrMessage := FOIDCProvider.GetLatestError
  else
    ErrMessage := '';
end;

end.

