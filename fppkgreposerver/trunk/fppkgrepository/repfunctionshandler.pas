unit repFunctionsHandler;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  httproute,
  HTTPDefs,
  fpjson,
  dcsGlobalSettings,
  cnocStackHandlerThread,
  cnocStackMessageTypes,
  cnocStackJSONHandlerThread,
  fprWebHandler,
  fprAuthenticationHandler,
  repPackage;

type

  { TrepFunctionsHander }

  TrepFunctionsHander = class(TfprWebHandler, IRouteInterface, IcnocStackHandler, IcnocStackJSONRespondToMessage)
  private
  protected
    function DoHandleRequest(ARequest: TRequest; JSONContent: TJSONData): TJSONData; override;
    function HandleFunctionRequest(const AFunction, AccessToken: string): TJSONData;
    procedure DoRespondToJSONMessage(const IncomingMessage: PcnocStackMessage; const JSONData: TJSONObject; out AResponse: TJSONData; var Handled: Boolean); override;
  public
    constructor Create; override;
  end;


implementation

{ TrepFunctionsHander }

constructor TrepFunctionsHander.Create;
var
  GlobalSettings: TDCSGlobalSettings;
begin
  inherited Create;
  GlobalSettings := TDCSGlobalSettings.GetInstance;
  if GlobalSettings.GetSettingAsString('AllowCorsOrigin') <> '' then
    AddCorsOrigin(GlobalSettings.GetSettingAsString('AllowCorsOrigin'), 'POST, GET, PUT', '', True);
end;

function TrepFunctionsHander.DoHandleRequest(ARequest: TRequest; JSONContent: TJSONData): TJSONData;
var
  AuthorizationToken, AccessToken: String;
begin
  AuthorizationToken := ARequest.Authorization;
  if copy(AuthorizationToken, 1, 7) = 'Bearer ' then
    AccessToken := copy(AuthorizationToken, 8, length(AuthorizationToken) - 7)
  else
    AccessToken := '';

  Result := HandleFunctionRequest(ARequest.RouteParams['function'], AccessToken);
end;

procedure TrepFunctionsHander.DoRespondToJSONMessage(const IncomingMessage: PcnocStackMessage;
  const JSONData: TJSONObject; out AResponse: TJSONData; var Handled: Boolean);
begin
  AResponse := HandleFunctionRequest(JSONData.Get('function',''), IncomingMessage^.GetExtAccessKey(0));
end;

function TrepFunctionsHander.HandleFunctionRequest(const AFunction, AccessToken: string): TJSONData;
var
  VersionList: TrepFPCVersionList;
  RepositoryList: TrepRepositoryList;
  i, j: Integer;
begin
  if TfprAuthenticationHandler.GetInstance.GetUserRole(AccessToken) <> 'admin' then
    raise EHTTP.CreateHelp('You need admin rights to be able to clear the repository-data', 403);

  VersionList := TrepFPCVersionList.Instance;
  for i := 0 to VersionList.Count -1 do
    begin
    RepositoryList := VersionList.Items[i].RepositoryList;
    for j := 0 to RepositoryList.Count -1 do
      begin
      RepositoryList.Items[j].PackageList.Clear;
      end;
    end;
  Result := ObjectToJSON(VersionList);
end;

end.

