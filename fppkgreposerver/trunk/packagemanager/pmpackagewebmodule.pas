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
    AResponse.Content := FPackageStreamer.PackageListToJSon(TpmPackageList.Instance);
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
    PackageVersion := TpmPackageVersion.Create;
    try
      JSONContentStringToObject(ARequest.Content, PackageVersion);
      Package := TpmPackageList.Instance.FindPackageByName(PackageName);
      if not Assigned(Package) then
        raise EHTTP.CreateFmtHelp('Package %s does not exist', [PackageName], 404);
      if Assigned(Package.PackageVersionList.FindVersionByTag(PackageVersion.Tag)) then
        raise Exception.Create('Packagename of URL does not match with the packagename of the contents');

      AResponse.Content := ObjectToJSONContentString(PackageVersion);

      Package.PackageVersionList.Add(PackageVersion);
      PackageVersion := nil;

      if Package.PackageState = pmpsInitial then
        Package.PackageState := pmpsAcceptance;
    finally
      PackageVersion.Free;
    end;
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
    Package := TpmPackage.Create;
    try
      FPackageStreamer.JSonToPackage(ARequest.Content, Package);
      Package.OwnerId := FSubjectId;
      Package.PackageState := pmpsInitial;

      if not Package.Validate(ErrStr) then
        Raise EJsonWebException.Create(ErrStr);

      if Assigned(TpmPackageList.Instance.FindPackageByName(Package.Name)) then
        Raise EJsonWebException.CreateFmt('Package with the name %s does already exist', [Package.Name]);

      AResponse.Content := FPackageStreamer.PackageToJSon(Package);

      TpmPackageList.Instance.Add(Package);
      Package := Nil;
    finally
      Package.Free;
    end;
    end
  else if ARequest.Method = 'GET' then
    begin
    Package := TpmPackageList.Instance.FindPackageByName(PackageName);
    if not Assigned(Package) then
      raise EHTTP.CreateFmtHelp('Package %s not found', [PackageName], 404);

    AResponse.Content := FPackageStreamer.PackageToJSon(Package);
    end
  else
    raise EHTTP.CreateFmtHelp('Method %s not supported', [ARequest.Method], 405);
  AResponse.Code := 200;
  AResponse.CodeText := GetStatusCode(AResponse.Code);
end;

constructor TpmPackageWM.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPackageStreamer := TpmPackageJSonStreaming.Create;
end;

Destructor TpmPackageWM.Destroy;
begin
  FPackageStreamer.Free;
  inherited Destroy;
end;

initialization
  RegisterHTTPModule('package', TpmPackageWM);
end.

