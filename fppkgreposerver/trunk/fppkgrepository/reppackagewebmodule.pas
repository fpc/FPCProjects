unit repPackageWebmodule;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  Classes,
  httpdefs,
  fpHTTP,
  fphttpserver,
  fphttpclient,
  fpWeb,
  fpjson,
  dcsGlobalSettings,
  fprWebModule,
  fprErrorHandling,
  repPackage;

type

  { TrepPackageWM }

  TrepPackageWM = class(TfprWebModule)
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    Procedure DataModuleRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse; Var Handled: Boolean);
  protected
    procedure HandleNewPackageRequest(ARepository: TrepRepository; ARequest: TRequest; AResponse: TResponse);
    procedure HandleGetPackageRequest(ARepository: TrepRepository; ARequest: TRequest; AResponse: TResponse);
    procedure HandleUpdatePackageRequest(ARepository: TrepRepository; ARequest: TRequest; AResponse: TResponse);
  public

  end;

var
  repPackageWM: TrepPackageWM;

implementation

{$R *.lfm}

{ TrepPackageWM }

procedure TrepPackageWM.DataModuleCreate(Sender: TObject);
begin
end;

procedure TrepPackageWM.DataModuleDestroy(Sender: TObject);
begin
end;

Procedure TrepPackageWM.DataModuleRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse; Var Handled: Boolean);
var
  FPCVersionStr, RepName: String;
  FPCVersion: TrepFPCVersion;
  Repository: TrepRepository;
begin
  FPCVersionStr := ARequest.GetNextPathInfo;

  if FPCVersionStr = '' then
    raise EJsonWebException.CreateHelp('Missing FPC-Version', 404);

  FPCVersion := TrepFPCVersionList.Instance.FindFPCVersion(FPCVersionStr);
  if not Assigned(FPCVersion) then
    raise EJsonWebException.CreateFmtHelp('FPC-Version %s is not available', [FPCVersionStr], 404);

  RepName := ARequest.GetNextPathInfo;
  if RepName = '' then
    raise EJsonWebException.CreateHelp('Missing repository name', 404);

  Repository := FPCVersion.RepositoryList.FindRepository(RepName);
  if not Assigned(Repository) then
    raise EJsonWebException.CreateFmtHelp('Repository %s does not exist', [RepName] , 404);

  if ARequest.Method = 'POST' then
    HandleNewPackageRequest(Repository, ARequest, AResponse)
  else if ARequest.Method = 'PUT' then
    HandleUpdatePackageRequest(Repository, ARequest, AResponse)
  else
    begin
    HandleGetPackageRequest(Repository, ARequest, AResponse);
    end;
  Handled := True;
end;

procedure TrepPackageWM.HandleNewPackageRequest(ARepository: TrepRepository; ARequest: TRequest; AResponse: TResponse);
var
  Package: TrepPackage;
  PackageURL: String;
begin
  if ARepository.NeedAdminRights then
    if GetUserRole <> 'admin' then
      raise EHTTPClient.CreateHelp('Only admins can add packages to this repository.', 403);

  Package := TrepPackage.Create(nil);
  try
    JSONContentStringToObject(ARequest.Content, Package);
    if Package.Name = '' then
      raise Exception.Create('Missing package-name');

    if Package.Tag = '' then
      raise Exception.Create('Missing package-tag');

    if Assigned(ARepository.PackageList.FindPackage(Package.Name)) then
      raise Exception.CreateFmt('Package %s does already exist', [Package.Name]);

    PackageURL := TDCSGlobalSettings.GetInstance.GetSettingAsString('packagemanagerurl') + '/package/'+Package.Name;

    try
      ObtainJSONRestRequest(PackageURL, True);
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

    AResponse.Content := ObjectToJSONContentString( Package );
    AResponse.Code := 200;
    AResponse.CodeText := GetStatusCode(AResponse.Code);

    Package.Collection := ARepository.PackageList;
    Package := nil;
  finally
    Package.Free;
  end;
end;

procedure TrepPackageWM.HandleGetPackageRequest(ARepository: TrepRepository; ARequest: TRequest; AResponse: TResponse);
var
  PackageName: string;
  Package: TrepPackage;
begin
  PackageName := ARequest.GetNextPathInfo;

  if PackageName='' then
    begin
    AResponse.Content := ObjectToJSONContentString(ARepository.PackageList);
    AResponse.Code := 200;
    AResponse.CodeText := GetStatusCode(AResponse.Code);
    end
  else
    begin
    Package := ARepository.PackageList.FindPackage(PackageName);
    if Assigned(Package) then
      AResponse.Content := ObjectToJSONContentString(Package)
    else
      raise EHTTPClient.CreateFmtHelp('Package %s does not exist in this repository', [PackageName], 404);
    end;
  AResponse.Code := 200;
  AResponse.CodeText := GetStatusCode(AResponse.Code);
end;

procedure TrepPackageWM.HandleUpdatePackageRequest(ARepository: TrepRepository; ARequest: TRequest; AResponse: TResponse);
var
  PackageName: String;
  Package, IntPackage: TrepPackage;
begin
  PackageName := ARequest.GetNextPathInfo;

  if PackageName='' then
    raise EJsonWebException.CreateHelp('Missing package name', 404);

  IntPackage := ARepository.PackageList.FindPackage(PackageName);
  if not Assigned(IntPackage) then
    raise EJsonWebException.CreateFmtHelp('Package %s is not available in this repository', [PackageName], 404);

  Package := TrepPackage.Create(nil);
  try
    JSONContentStringToObject(ARequest.Content, Package);
    if Package.Name = '' then
      raise Exception.Create('Missing package-name');

    if Package.Name <> PackageName then
      raise Exception.Create('Inconsistent package name');

    if Package.Tag = '' then
      raise Exception.Create('Missing package-tag');

    IntPackage.Tag := Package.Tag;

    AResponse.Content := ObjectToJSONContentString( IntPackage );
    AResponse.Code := 200;
    AResponse.CodeText := GetStatusCode(AResponse.Code);
  finally
    Package.Free;
  end;
end;

initialization
  RegisterHTTPModule('package', TrepPackageWM);
end.

