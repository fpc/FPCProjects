unit pmPackage;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fprGCollection,
  fprFPCVersion,
  fpjsonrtti;

type
  TpmPackageState = (pmpsInitial, pmpsAcceptance, pmpsApproved, pmpsPublished, pmpsRevoked);

  { TpmPackageVersion }

  TpmPackageVersion = class(TCollectionItem)
  private
    FAuthor: string;
    FFilename: string;
    FFPCVersion: string;
    FTag: string;
    procedure SetFPCVersion(AValue: string);
  published
    property Tag: string read FTag write FTag;
    property Filename: string read FFilename write FFilename;
    property Author: string read FAuthor write FAuthor;
    property FPCVersion: string read FFPCVersion write SetFPCVersion;
  end;

  TpmGenPackageVersionCollection = specialize TcnocGCollection<TpmPackageVersion>;

  { TpmPackageVersionCollection }

  TpmPackageVersionCollection = class(TpmGenPackageVersionCollection)
  public
    function FindVersionByTag(AFPCVersion, ATag: string): TpmPackageVersion;
  end;

  { TpmPackage }

  TpmPackage = class(TCollectionItem)
  private
    FName: string;
    FOwnerId: string;
    FPackageState: TpmPackageState;
    FPackageVersionList: TpmPackageVersionCollection;
    procedure SetPackageState(AValue: TpmPackageState);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    function Validate(out AnErrStr: string): Boolean;
  published
    property Name: string read FName write FName;
    property OwnerId: string read FOwnerId write FOwnerId;
    property PackageState: TpmPackageState read FPackageState write SetPackageState;
    property PackageVersionList: TpmPackageVersionCollection read FPackageVersionList;
  end;

  TpmGenPackageCollection = specialize TcnocGCollection<TpmPackage>;

  { TpmPackageCollection }

  TpmPackageCollection = class(TpmGenPackageCollection)
  private
    class var FPackageList: TpmPackageCollection;
    class function GetInstance: TpmPackageCollection; static;
  public
    class constructor Create;
    class destructor Destroy;
    class property Instance: TpmPackageCollection read GetInstance;
    function FindPackageByName(AName: string): TpmPackage;
    procedure LoadFromFile(AFileName: string);
    procedure SaveToFile(AFileName: string);
  end;

  { TpmManifest }

  TpmManifest = class
  private
    FAuthor: string;
    FFilename: string;
  published
    property Filename: string read FFilename write FFilename;
    property Author: string read FAuthor write FAuthor;
  end;

const
  CpmPackageStateString: array[TpmPackageState] of string = ('new', 'acceptance', 'approved', 'published', 'revoked');

implementation

{ TpmPackageVersion }

procedure TpmPackageVersion.SetFPCVersion(AValue: string);
begin
  if FFPCVersion<>AValue then
    begin
    if not Assigned(TfprFPCVersionCollection.Instance.FindVersion(AValue)) then
      raise Exception.CreateFmt('Unknown FPC-Version [%s]', [AValue]);
    FFPCVersion := AValue;
    end;
end;

{ TpmPackageVersionCollection }

function TpmPackageVersionCollection.FindVersionByTag(AFPCVersion, ATag: string): TpmPackageVersion;
var
  i: Integer;
  DefaultVersion: string;
begin
  DefaultVersion := TfprFPCVersionCollection.Instance.DefaultVersion.Name;
  for i := 0 to Count -1 do
    begin
    if (Items[i].Tag = ATag) and ((Items[i].FPCVersion = AFPCVersion) or ((AFPCVersion='') and (Items[i].FPCVersion=DefaultVersion))) then
      begin
      Result := Items[I];
      Exit;
      end;
    end;
  Result := Nil;
end;

{ TpmPackage }

procedure TpmPackage.SetPackageState(AValue: TpmPackageState);
begin
  if FPackageState = AValue then Exit;
  FPackageState := AValue;
end;

constructor TpmPackage.Create(ACollection: TCollection);
begin
  inherited;
  FPackageVersionList := TpmPackageVersionCollection.Create();
end;

destructor TpmPackage.Destroy;
begin
  FPackageVersionList.Free;
  inherited Destroy;
end;

function TpmPackage.Validate(out AnErrStr: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  if Name='' then
    AnErrStr := 'Missing package name'
  else if Length(Name)<3 then
    AnErrStr := 'Package name too short'
  else if Length(Name)>80 then
    AnErrStr := 'Package name too long'
  else if OwnerId='' then
    AnErrStr := 'No OwnerId'
  else
    begin
    for i := 0 to Length(AnErrStr) -1 do
      begin
      if not (AnErrStr[i] in ['a'..'z']) then
        AnErrStr := Format('Package name does contain an invalid character (%s).', [Name[i]])
      end;
    Result := True;
    end;
end;

{ TpmPackageCollection }

class function TpmPackageCollection.GetInstance: TpmPackageCollection; static;
begin
  Result := FPackageList;
end;

class constructor TpmPackageCollection.Create;
begin
  FPackageList := TpmPackageCollection.Create();
end;

class destructor TpmPackageCollection.Destroy;
begin
  FPackageList.Free;
end;

function TpmPackageCollection.FindPackageByName(AName: string): TpmPackage;
var
  i: Integer;
begin
  for i := 0 to Count -1 do
    begin
    if Items[i].Name = AName then
      begin
      Result := Items[I];
      Exit;
      end;
    end;
  Result := Nil;
end;

procedure TpmPackageCollection.LoadFromFile(AFileName: string);
var
  DeStreamer: TJSONDeStreamer;
  FS: TStringStream;
begin
  DeStreamer := TJSONDeStreamer.Create(nil);
  try
    FS := TStringStream.Create();
    try
      FS.LoadFromFile(AFileName);
      FS.Seek(0, soFromBeginning);
      DeStreamer.JSONToCollection(FS.DataString, Self);
    finally
      FS.Free;
    end;
  finally
    DeStreamer.Free;
  end;
end;

procedure TpmPackageCollection.SaveToFile(AFileName: string);
var
  Streamer: TJSONStreamer;
  FS: TStringStream;
begin
  Streamer := TJSONStreamer.Create(nil);
  try
    FS := TStringStream.Create(Streamer.ObjectToJSONString(Self));
    try
      FS.SaveToFile(AFileName);
    finally
      FS.Free;
    end;
  finally
    Streamer.Free;
  end;
end;

end.

