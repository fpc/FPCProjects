unit repPackageWebmodule;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  Classes,
  httpdefs,
  httproute,
  fpHTTP,
  fphttpclient,
  fpjson,
  jsonparser,
  dcsGlobalSettings,
  cnocStackMessageTypes,
  cnocStackJSONHandlerThread,
  cnocStackHandlerThread,
  fprErrorHandling,
  fprWebHandler,
  fprWebModule,
  fprAuthenticationHandler,
  repPackage;

type

  { TrepPackageHander }

  TrepPackageHander = class(TfprWebHandler, IRouteInterface, IcnocStackHandler, IcnocStackJSONRespondToMessage)
  protected
    function DoHandleRequest(ARequest : TRequest; JSONContent: TJSONData): TJSONData; override;
  private
    function HandlePackageRequest(FPCVersionStr, RepName, PackageName, Method: string; const JSONContent: TJSONData; AccessToken: string): TJSONData;
  protected
    function HandleNewPackageRequest(ARepository: TrepRepository; AJSONContent: TJSONData;
      AccessToken: string): TJSONData;
    function HandleGetPackageRequest(APackageName: string; ARepository: TrepRepository): TJSONData;
    function HandleUpdatePackageRequest(APackageName: string; AJSONContent: TJSONData; ARepository: TrepRepository): TJSONData;
  public
    constructor Create; override;
    procedure DoRespondToJSONMessage(const IncomingMessage: PcnocStackMessage; const JSONData: TJSONObject; out AResponse: TJSONData; var Handled: Boolean); override;
  end;

implementation

{ TrepPackageHander }

procedure TrepPackageHander.DoRespondToJSONMessage(const IncomingMessage: PcnocStackMessage; const JSONData: TJSONObject; out AResponse: TJSONData; var Handled: Boolean);
begin
  AResponse := HandlePackageRequest(JSONData.Get('fpcversion',''), JSONData.Get('repository',''), JSONData.get('package', ''), JSONData.get('method', ''), JSONData.FindPath('data'), IncomingMessage^.GetExtAccessKey(0));
  Handled := Assigned(AResponse);
end;

constructor TrepPackageHander.Create;
var
  GlobalSettings: TDCSGlobalSettings;
begin
  inherited Create;
  GlobalSettings := TDCSGlobalSettings.GetInstance;
  if GlobalSettings.GetSettingAsString('AllowCorsOrigin') <> '' then
    AddCorsOrigin(GlobalSettings.GetSettingAsString('AllowCorsOrigin'), 'POST, GET, PUT', '', True);
end;

function TrepPackageHander.DoHandleRequest(ARequest: TRequest; JSONContent: TJSONData): TJSONData;
var
  AuthorizationToken, AccessToken: String;
begin
  AuthorizationToken := ARequest.Authorization;
  if copy(AuthorizationToken, 1, 7) = 'Bearer ' then
    AccessToken := copy(AuthorizationToken, 8, length(AuthorizationToken) - 7)
  else
    AccessToken := '';

  Result := HandlePackageRequest(ARequest.RouteParams['fpcversion'], ARequest.RouteParams['repository'], ARequest.RouteParams['package'], ARequest.Method, JSONContent, AccessToken);
end;

function TrepPackageHander.HandlePackageRequest(FPCVersionStr, RepName, PackageName, Method: string;
  const JSONContent: TJSONData; AccessToken: string): TJSONData;
var
  FPCVersion: TrepFPCVersion;
  Repository: TrepRepository;
begin
  Result := nil;
  Repository := nil;
  FPCVersion := nil;

  if FPCVersionStr <> '' then
    begin
    FPCVersion := TrepFPCVersionList.Instance.FindFPCVersion(FPCVersionStr);
    if not Assigned(FPCVersion) then
      raise EJsonWebException.CreateFmtHelp('FPC-Version %s is not available', [FPCVersionStr], 404);

    if RepName <> '' then
      begin
      Repository := FPCVersion.RepositoryList.FindRepository(RepName);
      if not Assigned(Repository) then
        raise EJsonWebException.CreateFmtHelp('Repository %s does not exist', [RepName] , 404);
      end;
    end;

  if Method = 'GET' then
    Result := HandleGetPackageRequest(PackageName, Repository)
  else
    begin
    if FPCVersionStr = '' then
      raise EJsonWebException.CreateHelp('Missing FPC-Version', 404);
    if RepName = '' then
      raise EJsonWebException.CreateHelp('Missing repository name', 404);

    if Method = 'POST' then
      Result := HandleNewPackageRequest(Repository, JSONContent, AccessToken)
    else if Method = 'PUT' then
      Result := HandleUpdatePackageRequest(PackageName, JSONContent, Repository)
    end;
end;

function TrepPackageHander.HandleNewPackageRequest(ARepository: TrepRepository; AJSONContent: TJSONData; AccessToken: string): TJSONData;
var
  Package: TrepPackage;
  PackageURL: String;
begin
  Result := nil;
  if ARepository.NeedAdminRights then
    if TfprAuthenticationHandler.GetInstance.GetUserRole(AccessToken) <> 'admin' then
      raise EHTTPClient.CreateHelp('Only admins can add packages to this repository.', 403);

  Package := TrepPackage.Create(nil);
  try
    if AJSONContent.JSONType <> jtObject then
      raise Exception.Create('Content is not a valid json-object');
    JSONContentToObject(TJSONObject(AJSONContent), Package);
    if Package.Name = '' then
      raise Exception.Create('Missing package-name');

    if Package.Tag = '' then
      raise Exception.Create('Missing package-tag');

    if Assigned(ARepository.PackageList.FindPackage(Package.Name)) then
      raise Exception.CreateFmt('Package %s does already exist', [Package.Name]);

    PackageURL := TDCSGlobalSettings.GetInstance.GetSettingAsString('packagemanagerurl') + '/package/'+Package.Name;

    try
      TfprWebModule.ObtainJSONRestRequest(PackageURL, AccessToken);
    Except
      on E: Exception do
        begin
        if E is EHTTPClient then
          begin
          // Probably.... 404
          raise Exception.CreateFmt('Package %s does not exist', [Package.Name]);
          Exit;
          end
        else
          Raise E;
        end;
    end;

    Result := ObjectToJSON( Package );

    Package.Collection := ARepository.PackageList;
    Package := nil;
  finally
    Package.Free;
  end;
  if ARepository.StorageFile <> '' then
    ARepository.SaveToFile;
end;

function TrepPackageHander.HandleGetPackageRequest(APackageName: string; ARepository: TrepRepository): TJSONData;

  function ProcessRepository(ARepo: TrepRepository):TJSONData;
  var
    Package: TrepPackage;
  begin
    Result := nil;
    if APackageName='' then
      begin
      Result := ObjectToJSON(ARepo.PackageList);
      end
    else
      begin
      Package := ARepo.PackageList.FindPackage(APackageName);
      if Assigned(Package) then
        Result := ObjectToJSON(Package)
      else if ARepository <> nil then
        raise EHTTPClient.CreateFmtHelp('Package %s does not exist in this repository', [APackageName], 404);
      end;
  end;

var
  FPCVersion: TrepFPCVersion;
  Repository: TrepRepository;
  VersionList: TrepFPCVersionList;
  JSONData: TJSONData;
  JSONArr: TJSONArray;
  i, j, k: integer;
begin
  if not Assigned(ARepository) then
    begin
    JSONArr := TJSONArray.Create;
    try
      VersionList := TrepFPCVersionList.Instance;
      for i := 0 to VersionList.Count -1 do
        begin
        FPCVersion := VersionList.Items[i];
        for j := 0 to FPCVersion.RepositoryList.Count -1 do
          begin
          Repository := FPCVersion.RepositoryList.Items[j];
          JSONData := ProcessRepository(Repository);
          if Assigned(JSONData) then
            begin
            if JSONData.JSONType = jtArray then
              begin
              for k := 0 to TJSONArray(JSONData).Count -1 do
                begin
                JSONArr.Add(TJSONArray(JSONData).Items[k]);
                end;
              end
            else
              begin
              JSONArr.Add(JSONData.Clone);
              end;
            end;
          end;
        end;
      Result := JSONArr;
      JSONArr := nil;
    finally
      JSONArr.Free;
    end;
    end
  else
    Result := ProcessRepository(ARepository);
end;

function TrepPackageHander.HandleUpdatePackageRequest(APackageName: string; AJSONContent: TJSONData; ARepository: TrepRepository): TJSONData;
var
  Package, IntPackage: TrepPackage;
begin
  Result := nil;
  if APackageName='' then
    raise EJsonWebException.CreateHelp('Missing package name', 404);

  IntPackage := ARepository.PackageList.FindPackage(APackageName);
  if not Assigned(IntPackage) then
    raise EJsonWebException.CreateFmtHelp('Package %s is not available in this repository', [APackageName], 404);

  Package := TrepPackage.Create(nil);
  try
    if AJSONContent.JSONType<>jtObject then
      raise Exception.Create('Content is not a valid json-object');
    JSONContentToObject(TJSONObject(AJSONContent), Package);
    if Package.Name = '' then
      raise Exception.Create('Missing package-name');

    if Package.Name <> APackageName then
      raise Exception.Create('Inconsistent package name');

    if Package.Tag = '' then
      raise Exception.Create('Missing package-tag');

    IntPackage.Tag := Package.Tag;

    Result := ObjectToJSON( IntPackage );
  finally
    Package.Free;
  end;
  if ARepository.StorageFile <> '' then
    ARepository.SaveToFile;
end;

end.

