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
    function GetUserRole(AccessToken: string): string;

  end;

implementation

{ TfprAuthenticationHandler }

class function TfprAuthenticationHandler.GetInstance: TfprAuthenticationHandler;
begin
  if not Assigned(FInstance) then
    begin
    FInstance := TfprAuthenticationHandler.Create;
    end;
  Result := FInstance;
end;

function TfprAuthenticationHandler.GetUserRole(AccessToken: string): string;
var
  JSonData: TJSONData;
begin
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

end.

