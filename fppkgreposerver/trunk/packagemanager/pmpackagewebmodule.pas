unit pmPackageWebModule;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  Classes,
  httpdefs,
  fpjson,
  fpHTTP,
  httproute,
  cnocStackMessageTypes,
  cnocStackHandlerThread,
  cnocStackJSONHandlerThread,
  fprLog,
  fprErrorHandling,
  fprFPCVersion,
  fprAuthenticationHandler,
  fprStackClient,
  fprWebHandler,
  fprInterfaceClasses,
  dcsGlobalSettings,
  pmPackage,
  pmPackageJSonStreaming;

type

  { TpmPackageWM }

  TpmPackageWM = class(TfprWebHandler, IRouteInterface, IcnocStackHandler, IcnocStackJSONRespondToMessage)
  protected
    function DoHandleRequest(ARequest : TRequest; JSONContent: TJSONData): TJSONData; override;
    procedure DoRespondToJSONMessage(const IncomingMessage: PcnocStackMessage; const JSONData: TJSONObject; out AResponse: TJSONData; var Handled: Boolean); override;

    function HandlePackageRequest(const PackageName, SubObject, Method, Data, AccessToken: string): TJSONData;
  private
    FPackageStreamer: TpmPackageJSonStreaming;
    function HandlePackageVersion(const PackageName, Method, Data, AccessToken: string): TJSONData;
    function HandlePackage(PackageName, Method, Data, AccessToken: string): TJSONData;
    function HandlePackageApprove(const PackageName, Method, AccessToken: string): TJSONData;
    function HandlePatchPackage(const PackageName, Data, AccessToken: string): TJSONData;
    procedure SavePackageList;
  protected
    procedure EnsureLoggedIn(const AccessToken: string);
    procedure EnsureAdmin(const AccessToken: string);
    procedure EnsureMayEditPackage(const AccessToken: string; const Package: TfprPackage);
  public
    constructor Create(); override;
    Destructor Destroy; override;
  end;

var
  pmPackageWM: TpmPackageWM;

implementation

{ TpmPackageWM }

function TpmPackageWM.HandlePackageRequest(const PackageName, SubObject, Method, Data, AccessToken: string): TJSONData;
begin
  if (PackageName = '') and (Method = 'GET') then
    begin
    // Return list of all packages
    FPackageStreamer.FilterOutOldVersions := True;
    Result := FPackageStreamer.PackageCollectionToJSon(TfprPackageCollection.Instance, True);
    end
  else
    begin
    FPackageStreamer.FilterOutOldVersions := False;
    case SubObject of
      '':
        begin
        Result := HandlePackage(PackageName, Method, Data, AccessToken);
        end;
      'version':
        begin
        Result := HandlePackageVersion(PackageName, Method, Data, AccessToken);
        end;
      'approve':
        begin
        Result := HandlePackageApprove(PackageName, Method, AccessToken);
        end
    else
      begin
      raise EJsonWebException.CreateFmtHelp('Invalid subobject [%s], valid values are: version and approve', [SubObject], 404);
      end
    end; { case }
    end;
end;

function TpmPackageWM.HandlePackageVersion(const PackageName, Method, Data, AccessToken: string): TJSONData;
var
  Package: TfprPackage;
  PackageVersion: TfprPackageVersion;
begin
  if Method = 'POST' then
    begin
    TfprLog.Log('Received a new Package-version request for package ' + PackageName);
    EnsureLoggedIn(AccessToken);
    PackageVersion := TfprPackageVersion.Create(nil);
    try
      JSONContentStringToObject(Data, PackageVersion);
      Package := TfprPackageCollection.Instance.FindPackageByName(PackageName);
      if not Assigned(Package) then
        raise EHTTP.CreateFmtHelp('Package %s does not exist', [PackageName], 404);
      if Assigned(Package.PackageVersionList.FindVersionByTag(PackageVersion.FPCVersion, PackageVersion.Tag)) then
        raise Exception.Create('Package-version already exists');
      if PackageVersion.FPCVersion = '' then
        raise Exception.Create('The obligatory FPC-version-field is missing');
      if PackageVersion.Version.Empty then
        raise Exception.Create('The obligatory version-field is missing');

      Result := FPackageStreamer.PackageVersionToJSon(PackageVersion);

      PackageVersion.Collection := Package.PackageVersionList;
      PackageVersion := nil;
    finally
      PackageVersion.Free;
    end;
    if Package.PackageState = prspsInitial then
      begin
      TfprLog.Log('Received new package-version for package [' + PackageName + '] which is in the initial state. Update the state to Acceptance.');
      Package.PackageState := prspsAcceptance;
      end;
    SavePackageList;
    end
  else
    raise EHTTP.CreateHelp('Only POST is allowed on a package-version', 405);
end;

function TpmPackageWM.HandlePackage(PackageName, Method, Data, AccessToken: string): TJSONData;
var
  Package: TfprPackage;
  ErrStr: string;
begin
  if Method = 'POST' then
    begin
    EnsureLoggedIn(AccessToken);
    Package := TfprPackage.Create(nil);
    try
      FPackageStreamer.JSonToPackage(Data, Package);
      Package.OwnerId := TfprAuthenticationHandler.GetInstance.GetSubject(AccessToken);
      Package.PackageState := prspsInitial;
      Package.Support := 'Community';

      if not Package.Validate(ErrStr) then
        Raise EJsonWebException.Create(ErrStr);

      if Assigned(TfprPackageCollection.Instance.FindPackageByName(Package.Name)) then
        Raise EJsonWebException.CreateFmt('Package with the name %s does already exist', [Package.Name]);

      Result := FPackageStreamer.PackageToJSon(Package);

      Package.Collection := TfprPackageCollection.Instance;
      Package := Nil;
    finally
      Package.Free;
    end;
    SavePackageList;
    end
  else if Method = 'GET' then
    begin
    Package := TfprPackageCollection.Instance.FindPackageByName(PackageName);
    if not Assigned(Package) then
      raise EHTTP.CreateFmtHelp('Package %s not found', [PackageName], 404);

    Result := FPackageStreamer.PackageToJSon(Package);
    end
  else if Method = 'PATCH' then
    begin
    Result := HandlePatchPackage(PackageName, Data, AccessToken);
    end
  else
    raise EHTTP.CreateFmtHelp('Method %s not supported', [Method], 405);
end;

function TpmPackageWM.HandlePackageApprove(const PackageName, Method, AccessToken: string): TJSONData;
var
  Package: TfprPackage;
begin
  EnsureAdmin(AccessToken);

  if Method = 'PUT' then
    begin
    Package := TfprPackageCollection.Instance.FindPackageByName(PackageName);
    if not Assigned(Package) then
      raise EHTTP.CreateFmtHelp('Package %s not found', [PackageName], 404);

    if Package.PackageState <> prspsAcceptance then
      raise Exception.Create('Only packages in Acceptance state can be approved.');

    Package.PackageState := prspsApproved;

    SavePackageList;

    Result := FPackageStreamer.PackageToJSon(Package);
    end
  else
    raise EHTTP.CreateFmtHelp('Method %s not supported', [Method], 405);
end;

procedure TpmPackageWM.SavePackageList;
var
  PackageListFile: String;
begin
  PackageListFile := TDCSGlobalSettings.GetInstance.GetSettingAsString('PackageListFile');
  if (PackageListFile <> '') then
    FPackageStreamer.SavePackageCollectionToFile(TfprPackageCollection.Instance, PackageListFile);
end;

constructor TpmPackageWM.Create();
var
  GlobalSettings: TDCSGlobalSettings;
begin
  inherited Create();
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

function TpmPackageWM.HandlePatchPackage(const PackageName, Data, AccessToken: string): TJSONData;
var
  Package: TfprPackage;
  Patch: TpmPatchPackage;
begin
  EnsureLoggedIn(AccessToken);
  Patch := TpmPatchPackage.Create;
  try
    FPackageStreamer.JSonToPatchPackage(Data, Patch);

    Package := TfprPackageCollection.Instance.FindPackageByName(PackageName);
    if not Assigned(Package) then
      raise EHTTP.CreateFmtHelp('Package %s not found', [PackageName], 404);

    EnsureMayEditPackage(AccessToken, Package);

    if Patch.CategoryId<>0 then
      Package.CategoryId := Patch.CategoryId;

    if Patch.Support<>'' then
      begin
      EnsureAdmin(AccessToken);
      Package.Support := Patch.Support;
      end;

    if length(Patch.KeywordIds)>0 then
      Package.KeywordIds := Patch.KeywordIds;

    SavePackageList;
  finally
    Patch.Free;
  end;
  Result := FPackageStreamer.PackageToJSon(Package);
end;

function TpmPackageWM.DoHandleRequest(ARequest: TRequest; JSONContent: TJSONData): TJSONData;
var
  AuthorizationToken, AccessToken: String;
begin
  AuthorizationToken := ARequest.Authorization;
  if copy(AuthorizationToken, 1, 7) = 'Bearer ' then
    AccessToken := copy(AuthorizationToken, 8, length(AuthorizationToken) - 7)
  else
    AccessToken := '';

  Result := HandlePackageRequest(ARequest.RouteParams['packagename'], ARequest.RouteParams['subobject'], ARequest.Method, ARequest.Content, AccessToken);
end;

procedure TpmPackageWM.DoRespondToJSONMessage(const IncomingMessage: PcnocStackMessage;
  const JSONData: TJSONObject; out AResponse: TJSONData; var Handled: Boolean);
var
  AccessToken: String;
  JSData: TJSONData;
  Data: string;
begin
  AccessToken := IncomingMessage^.GetExtAccessKey(0);
  JSData := JSONData.FindPath('data');
  if Assigned(JSData) then
    Data := JSData.AsJson
  else
    Data := '';
  AResponse := HandlePackageRequest(JSONData.Get('package',''), JSONData.Get('subobject',''), JSONData.get('method', ''), Data, AccessToken);
  Handled := Assigned(AResponse);
end;

procedure TpmPackageWM.EnsureAdmin(const AccessToken: string);
begin
  EnsureLoggedIn(AccessToken);
  if TfprAuthenticationHandler.GetInstance.GetUserRole(AccessToken) <> 'admin' then
    raise EHTTP.CreateHelp('You have to be admin to perform this task', 403);
end;

procedure TpmPackageWM.EnsureLoggedIn(const AccessToken: string);
var
  ErrMessage: string;
begin
  if AccessToken='' then
    raise EHTTP.CreateHelp('Authentication failed (no accesstoken (bearer) provided)', 403);
  if not TfprAuthenticationHandler.GetInstance.VerifyAccessToken(AccessToken, ErrMessage) then
    raise EHTTP.CreateFmtHelp('Authentication failed (%s)', [ErrMessage], 403);
end;

procedure TpmPackageWM.EnsureMayEditPackage(const AccessToken: string; const Package: TfprPackage);
begin
  EnsureLoggedIn(AccessToken);
  if (TfprAuthenticationHandler.GetInstance.GetUserRole(AccessToken) <> 'admin') and
    (TfprAuthenticationHandler.GetInstance.GetSubject(AccessToken) <> Package.OwnerId) then
    raise EHTTP.CreateFmtHelp('You have no rights to edit package [%s]', [Package.Name], 403);
end;

end.

