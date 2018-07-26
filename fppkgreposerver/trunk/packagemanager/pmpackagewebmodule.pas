unit pmPackageWebModule;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  Classes,
  httpdefs,
  fpjson,
  fpHTTP,
  fphttpserver,
  fprWebModule,
  fprErrorHandling,
  fprFPCVersion,
  dcsGlobalSettings,
  pmPackage,
  pmPackageJSonStreaming;

type

  { TpmPackageWM }

  TpmPackageWM = class(TfprWebModule)
    Procedure DataModuleRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse; Var Handled: Boolean);
  private
    FPackageStreamer: TpmPackageJSonStreaming;
    Procedure HandlePackageVersion(PackageName: string; ARequest: TRequest; AResponse: TResponse);
    Procedure HandlePackage(PackageName: string; ARequest: TRequest; AResponse: TResponse);
    Procedure HandlePackageApprove(PackageName: string; ARequest: TRequest; AResponse: TResponse);
    procedure SavePackageList;
  public
    constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
  end;

var
  pmPackageWM: TpmPackageWM;

implementation

{$R *.lfm}

{ TpmPackageWM }

Procedure TpmPackageWM.DataModuleRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse; Var Handled: Boolean);
var
  PackageName: string;
  SubObject: string;
begin
  PackageName := ARequest.GetNextPathInfo;
  if (PackageName = '') and (ARequest.Method = 'GET') then
    begin
    // Return list of all packages
    AResponse.Content := FPackageStreamer.PackageCollectionToJSon(TpmPackageCollection.Instance);
    AResponse.Code := 200;
    end
  else
    begin
    SubObject := ARequest.GetNextPathInfo;
    case SubObject of
      '':
        begin
        HandlePackage(PackageName, ARequest, AResponse);
        AResponse.Code := 200;
        end;
      'version':
        begin
        HandlePackageVersion(PackageName, ARequest, AResponse);
        AResponse.Code := 200;
        end;
      'approve':
        begin
        HandlePackageApprove(PackageName, ARequest, AResponse);
        AResponse.Code := 200;
        end
    else
      begin
      AResponse.Code := 404;
      end
    end; { case }
    end;


  AResponse.CodeText := GetStatusCode(AResponse.Code);
  Handled := True;
end;

Procedure TpmPackageWM.HandlePackageVersion(PackageName: string; ARequest: TRequest;
  AResponse: TResponse);
var
  Package: TpmPackage;
  PackageVersion: TpmPackageVersion;
begin
  if ARequest.Method = 'POST' then
    begin
    PackageVersion := TpmPackageVersion.Create(nil);
    try
      JSONContentStringToObject(ARequest.Content, PackageVersion);
      Package := TpmPackageCollection.Instance.FindPackageByName(PackageName);
      if not Assigned(Package) then
        raise EHTTP.CreateFmtHelp('Package %s does not exist', [PackageName], 404);
      if Assigned(Package.PackageVersionList.FindVersionByTag(PackageVersion.FPCVersion, PackageVersion.Tag)) then
        raise Exception.Create('Package-version already exists');

      AResponse.Content := ObjectToJSONContentString(PackageVersion);

      PackageVersion.Collection := Package.PackageVersionList;
      PackageVersion := nil;
    finally
      PackageVersion.Free;
    end;
    SavePackageList;
    if Package.PackageState = pmpsInitial then
      Package.PackageState := pmpsAcceptance;
    end
  else
    raise Exception.Create('Get package version not implemented');
end;

Procedure TpmPackageWM.HandlePackage(PackageName: string; ARequest: TRequest; AResponse: TResponse);
var
  Package: TpmPackage;
  ErrStr: string;
begin
  if ARequest.Method = 'POST' then
    begin
    Package := TpmPackage.Create(nil);
    try
      FPackageStreamer.JSonToPackage(ARequest.Content, Package);
      Package.OwnerId := FSubjectId;
      Package.PackageState := pmpsInitial;

      if not Package.Validate(ErrStr) then
        Raise EJsonWebException.Create(ErrStr);

      if Assigned(TpmPackageCollection.Instance.FindPackageByName(Package.Name)) then
        Raise EJsonWebException.CreateFmt('Package with the name %s does already exist', [Package.Name]);

      AResponse.Content := FPackageStreamer.PackageToJSon(Package);

      Package.Collection := TpmPackageCollection.Instance;
      Package := Nil;
    finally
      Package.Free;
    end;
    SavePackageList;
    end
  else if ARequest.Method = 'GET' then
    begin
    Package := TpmPackageCollection.Instance.FindPackageByName(PackageName);
    if not Assigned(Package) then
      raise EHTTP.CreateFmtHelp('Package %s not found', [PackageName], 404);

    AResponse.Content := FPackageStreamer.PackageToJSon(Package);
    end
  else
    raise EHTTP.CreateFmtHelp('Method %s not supported', [ARequest.Method], 405);
  AResponse.Code := 200;
  AResponse.CodeText := GetStatusCode(AResponse.Code);
end;

Procedure TpmPackageWM.HandlePackageApprove(PackageName: string; ARequest: TRequest;
  AResponse: TResponse);
var
  Package: TpmPackage;
begin
  if GetUserRole <> 'admin' then
    raise EHTTP.CreateFmtHelp('Approve is denied', [PackageName], 403);

  if ARequest.Method = 'PUT' then
    begin
    Package := TpmPackageCollection.Instance.FindPackageByName(PackageName);
    if not Assigned(Package) then
      raise EHTTP.CreateFmtHelp('Package %s not found', [PackageName], 404);

    if Package.PackageState <> pmpsAcceptance then
      raise Exception.Create('Only packages in Acceptance state can be approved.');

    Package.PackageState := pmpsApproved;

    SavePackageList;

    AResponse.Content := FPackageStreamer.PackageToJSon(Package);
    end
  else
    raise EHTTP.CreateFmtHelp('Method %s not supported', [ARequest.Method], 405);
end;

procedure TpmPackageWM.SavePackageList;
var
  PackageListFile: String;
begin
  PackageListFile := TDCSGlobalSettings.GetInstance.GetSettingAsString('PackageListFile');
  if (PackageListFile <> '') then
    TpmPackageCollection.Instance.SaveToFile(PackageListFile);
end;

constructor TpmPackageWM.Create(AOwner: TComponent);
var
  GlobalSettings: TDCSGlobalSettings;
begin
  inherited Create(AOwner);
  FPackageStreamer := TpmPackageJSonStreaming.Create;

  GlobalSettings := TDCSGlobalSettings.GetInstance;
  if GlobalSettings.GetSettingAsString('AllowCorsOrigin') <> '' then
    AddCorsOrigin(GlobalSettings.GetSettingAsString('AllowCorsOrigin'), 'POST, GET, PUT', '', True);
end;

Destructor TpmPackageWM.Destroy;
begin
  FPackageStreamer.Free;
  inherited Destroy;
end;

initialization
  RegisterHTTPModule('package', TpmPackageWM);
end.

