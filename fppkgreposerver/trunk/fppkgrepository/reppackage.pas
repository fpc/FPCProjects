unit repPackage;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fgl,
  dcsGlobalSettings,
  fprGCollection;

type

  { TrepPackage }

  TrepPackage = class(TCollectionItem)
  private
    FName: string;
    FTag: string;
  published
    property Name: string read FName write FName;
    property Tag: string read FTag write FTag;
  end;

  TrepCustomPackageList = specialize TcnocGCollection<TrepPackage>;

  { TrepPackageList }

  TrepPackageList = class(TrepCustomPackageList)
  public
    function FindPackage(APackageName: string): TrepPackage;
  end;

  { TrepRepository }

  TrepRepository = class
  private
    FBaseURL: string;
    FContact: string;
    FFPCVersion: string;
    FMasterRepositoryName: string;
    FName: string;
    FNeedAdminRights: Boolean;
    FPackageList: TrepPackageList;
    FPath: string;
  public
    constructor Create;
    destructor Destroy; override;
    property PackageList: TrepPackageList read FPackageList;
    property Name: string read FName write FName;
    property MasterRepositoryName: string read FMasterRepositoryName write FMasterRepositoryName;
    property NeedAdminRights: Boolean read FNeedAdminRights write FNeedAdminRights;
    property Path: string read FPath write FPath;
    property BaseURL: string read FBaseURL write FBaseURL;
    property Contact: string read FContact write FContact;
    property FPCVersion: string read FFPCVersion write FFPCVersion;
  end;

  TrepCustomRepositoryList = specialize TFPGObjectList<TrepRepository>;

  { TrepRepositoryList }

  TrepRepositoryList = class(TrepCustomRepositoryList)
  public
    function FindRepository(AName: string): TrepRepository;
  end;


  { TrepFPCVersion }

  TrepFPCVersion = class
  private
    FFPCVersion: string;
    FRepositoryList: TrepRepositoryList;
  public
    constructor Create;
    destructor Destroy; override;
    property FPCVersion: string read FFPCVersion write FFPCVersion;
    property RepositoryList: TrepRepositoryList read FRepositoryList;
  end;

  TrepCustomFPCVersionList = specialize TFPGObjectList<TrepFPCVersion>;

  { TrepFPCVersionList }

  TrepFPCVersionList = class(TrepCustomFPCVersionList)
  private
    class var FInstance: TrepFPCVersionList;
    class destructor Destroy;
    class function GetInstance: TrepFPCVersionList; static;
  public
    class property Instance: TrepFPCVersionList read GetInstance;
    function FindFPCVersion(FPCVersion: string): TrepFPCVersion;
    procedure InitFromSettings;
  end;

implementation

{ TrepPackageList }

function TrepPackageList.FindPackage(APackageName: string): TrepPackage;
var
  i: Integer;
begin
  result := nil;
  for i := 0 to Count -1 do
    if Items[i].Name = APackageName then
      begin
      Result := Items[i];
      Break;
      end;
end;

{ TrepRepositoryList }

function TrepRepositoryList.FindRepository(AName: string): TrepRepository;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count -1 do
    if SameText(Items[i].Name, AName) then
      begin
      Result := Items[i];
      Break;
      end;
end;

{ TrepFPCVersionList }

class destructor TrepFPCVersionList.Destroy;
begin
  FInstance.Free;
end;

class function TrepFPCVersionList.GetInstance: TrepFPCVersionList;
begin
  if not Assigned(FInstance) then
    FInstance := TrepFPCVersionList.Create(True);
  Result := FInstance;
end;

function TrepFPCVersionList.FindFPCVersion(FPCVersion: string): TrepFPCVersion;
var
  i: Integer;
begin
  result := nil;
  for i := 0 to Count -1 do
    if Items[i].FPCVersion = FPCVersion then
      begin
      Result := Items[i];
      break;
      end;
end;

procedure TrepFPCVersionList.InitFromSettings;
var
  Settings: TDCSGlobalSettings;
  SettingTemplate: TDCSSettingTemplate;
  FPCVersionStr, RepoName: String;
  FPCVersion: TrepFPCVersion;
  i: Integer;
  Repo: TrepRepository;
begin
  Settings := TDCSGlobalSettings.GetInstance;

  SettingTemplate := Settings.SettingTemplateList.FindSettingTemplate('Repository-');
  for i := 0 to SettingTemplate.Values.Count -1 do
    begin
    FPCVersionStr := Settings.GetSettingAsString('FPCVersion-'+SettingTemplate.Values[i]);
    FPCVersion := FindFPCVersion(FPCVersionStr);
    if not Assigned(FPCVersion) then
      begin
      FPCVersion := TrepFPCVersion.Create;
      FPCVersion.FPCVersion := FPCVersionStr;
      Add(FPCVersion);
      end;

    RepoName := Settings.GetSettingAsString('Name-'+SettingTemplate.Values[i]);

    Repo := FPCVersion.RepositoryList.FindRepository(RepoName);
    if not Assigned(Repo) then
      begin
      Repo := TrepRepository.Create;
      Repo.Name := RepoName;
      FPCVersion.RepositoryList.Add(Repo);
      end;

    Repo.MasterRepositoryName := Settings.GetSettingAsString('MasterRepository-'+SettingTemplate.Values[i]);
    Repo.NeedAdminRights := Settings.GetSettingAsBoolean('NeedAdminRights-'+SettingTemplate.Values[i]);
    Repo.Path := Settings.GetSettingAsString('Path-'+SettingTemplate.Values[i]);
    Repo.BaseURL := Settings.GetSettingAsString('BaseURL-'+SettingTemplate.Values[i]);
    Repo.Contact := Settings.GetSettingAsString('Contact-'+SettingTemplate.Values[i]);
    Repo.FPCVersion := FPCVersion.FPCVersion;
    end;
end;

{ TrepFPCVersion }

constructor TrepFPCVersion.Create;
begin
  FRepositoryList := TrepRepositoryList.Create;
end;

destructor TrepFPCVersion.Destroy;
begin
  FRepositoryList.Free;
  inherited Destroy;
end;

{ TrepRepository }

constructor TrepRepository.Create;
begin
  FPackageList := TrepPackageList.Create();
end;

destructor TrepRepository.Destroy;
begin
  FPackageList.Free;
  inherited Destroy;
end;

end.

