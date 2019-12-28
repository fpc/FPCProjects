unit pmFPCVersionWebModule;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  Classes,
  httpdefs,
  fpHTTP,
  fphttpserver,
  dcsGlobalSettings,
  fprFPCVersion,
  fprWebModule;

type

  { TpmFPCVersionWM }

  TpmFPCVersionWM = class(TfprWebModule)
    procedure DataModuleCreate(Sender: TObject);
    Procedure DataModuleRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse; Var Handled: Boolean);
  protected
    function RequireAuthentication(ARequest: TRequest): Boolean; override;
  end;

var
  pmFPCVersionWM: TpmFPCVersionWM;

implementation

{$R *.lfm}

{ TpmFPCVersionWM }

procedure TpmFPCVersionWM.DataModuleCreate(Sender: TObject);
var
  GlobalSettings: TDCSGlobalSettings;
begin
  GlobalSettings := TDCSGlobalSettings.GetInstance;
  if GlobalSettings.GetSettingAsString('AllowCorsOrigin') <> '' then
    AddCorsOrigin(GlobalSettings.GetSettingAsString('AllowCorsOrigin'), 'POST, GET, PUT', '', True);
end;

Procedure TpmFPCVersionWM.DataModuleRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse; Var Handled: Boolean);
begin
  Handled := True;
  if ARequest.Method = 'GET' then
    begin
    AResponse.Content := ObjectToJSONContentString(TfprFPCVersionCollection.Instance);
    AResponse.Code := 200;
    AResponse.CodeText := GetStatusCode(AResponse.Code);
    end
  else
    raise EHTTP.CreateHelp('FPC-versions are immutable', 405);
end;

function TpmFPCVersionWM.RequireAuthentication(ARequest: TRequest): Boolean;
begin
  if ARequest.Method<>'GET' then
    Result := inherited
  else
    Result := False;
end;

initialization
  RegisterHTTPModule('fpcversion', TpmFPCVersionWM);
end.

