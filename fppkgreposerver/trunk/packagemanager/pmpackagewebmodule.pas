unit pmPackageWebModule;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  Classes,
  httpdefs,
  fpjson,
  fpHTTP,
  fprLog,
  fphttpserver,
  fprWebModule,
  fprErrorHandling,
  fprFPCVersion,
  fprJSONFileStreaming,
  fprStackClient,
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
    procedure HandlePatchPackage(PackageName: string; ARequest: TRequest; AResponse: TResponse);
    procedure SavePackageList;
  protected
    function RequireAuthentication(ARequest: TRequest): Boolean; override;
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
    FPackageStreamer.FilterOutOldVersions := True;
    AResponse.Content := FPackageStreamer.PackageCollectionToJSon(TpmPackageCollection.Instance, True);
    AResponse.Code := 200;
    end
  else
    begin
    FPackageStreamer.FilterOutOldVersions := False;
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
    TfprLog.Log('Received a new Package-version request for package ' + PackageName, ARequest);
    PackageVersion := TpmPackageVersion.Create(nil);
    try
      JSONContentStringToObject(ARequest.Content, PackageVersion);
      Package := TpmPackageCollection.Instance.FindPackageByName(PackageName);
      if not Assigned(Package) then
        raise EHTTP.CreateFmtHelp('Package %s does not exist', [PackageName], 404);
      if Assigned(Package.PackageVersionList.FindVersionByTag(PackageVersion.FPCVersion, PackageVersion.Tag)) then
        raise Exception.Create('Package-version already exists');
      if PackageVersion.FPCVersion = '' then
        raise Exception.Create('The obligatory FPC-version-field is missing');
      if PackageVersion.Version.Empty then
        raise Exception.Create('The obligatory version-field is missing');

      AResponse.Content := ObjectToJSONContentString(PackageVersion);

      PackageVersion.Collection := Package.PackageVersionList;
      PackageVersion := nil;
    finally
      PackageVersion.Free;
    end;
    if Package.PackageState = pmpsInitial then
      begin
      TfprLog.Log('Received new package-version for package [' + PackageName + '] which is in the initial state. Update the state to Acceptance.', ARequest);
      Package.PackageState := pmpsAcceptance;
      end;
    SavePackageList;
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
  else if ARequest.Method = 'PATCH' then
    begin
    HandlePatchPackage(PackageName, ARequest, AResponse);
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
    FPackageStreamer.SavePackageCollectionToFile(TpmPackageCollection.Instance, PackageListFile);
end;

constructor TpmPackageWM.Create(AOwner: TComponent);
var
  GlobalSettings: TDCSGlobalSettings;
begin
  inherited Create(AOwner);
  GlobalSettings := TDCSGlobalSettings.GetInstance;

  FPackageStreamer := TpmPackageJSonStreaming.Create;
  FPackageStreamer.StackClient := TfprStackClientSingleton.Instance.Client;

  GlobalSettings := TDCSGlobalSettings.GetInstance;
  if GlobalSettings.GetSettingAsString('AllowCorsOrigin') <> '' then
    AddCorsOrigin(GlobalSettings.GetSettingAsString('AllowCorsOrigin'), 'POST, GET, PUT, PATCH', '', True);
end;

Destructor TpmPackageWM.Destroy;
begin
  FPackageStreamer.Free;
  inherited Destroy;
end;

procedure TpmPackageWM.HandlePatchPackage(PackageName: string; ARequest: TRequest; AResponse: TResponse);
var
  Package: TpmPackage;
  Patch: TpmPatchPackage;
begin
  Patch := TpmPatchPackage.Create;
  try
    FPackageStreamer.JSonToPatchPackage(ARequest.Content, Patch);

    Package := TpmPackageCollection.Instance.FindPackageByName(PackageName);
    if not Assigned(Package) then
      raise EHTTP.CreateFmtHelp('Package %s not found', [PackageName], 404);

    if Patch.CategoryId<>0 then
      Package.CategoryId := Patch.CategoryId;

    if length(Patch.KeywordIds)>0 then
      Package.KeywordIds := Patch.KeywordIds;

    SavePackageList;
  finally
    Patch.Free;
  end;
  AResponse.Content := FPackageStreamer.PackageToJSon(Package);
end;

function TpmPackageWM.RequireAuthentication(ARequest: TRequest): Boolean;
begin
  if ARequest.Method<>'GET' then
    Result := inherited
  else
    Result := False;
end;

initialization
  RegisterHTTPModule('package', TpmPackageWM);
end.

