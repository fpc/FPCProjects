unit pmPackage;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpmkunit,
  fpjsonrtti,
  fprGCollection,
  fprFPCVersion,
  fprModel;

type
  TpmPackageState = (pmpsInitial, pmpsAcceptance, pmpsApproved, pmpsPublished, pmpsRevoked);
  TpmArrayOfString = array of string;
  TpmArrayOfInteger = array of integer;

  { TpmPackageVersion }

  TpmPackageVersion = class(TCollectionItem)
  private
    FAuthor: string;
    FFilename: string;
    FFPCVersion: string;
    FTag: string;
    FDescription: String;
    FEmail: String;
    FLicense: String;
    FHomepageurl: String;
    FVersion: TFPVersion;
    procedure SetFPCVersion(AValue: string);
    procedure SetVersion(AValue: TFPVersion);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  published
    property Version: TFPVersion read FVersion write SetVersion;
    property Tag: string read FTag write FTag;
    property Filename: string read FFilename write FFilename;
    property Author: string read FAuthor write FAuthor;
    property FPCVersion: string read FFPCVersion write SetFPCVersion;
    property Description: string read FDescription write FDescription;
    property Email: string read FEmail write FEmail;
    property License: string read FLicense write FLicense;
    property HomepageURL: string read FHomepageURL write FHomepageURL;
  end;

  TpmGenPackageVersionCollection = specialize TcnocGCollection<TpmPackageVersion>;

  { TpmPackageVersionCollection }

  TpmPackageVersionCollection = class(TpmGenPackageVersionCollection)
  public
    function FindVersionByTag(AFPCVersion, ATag: string): TpmPackageVersion;
  end;

  { TpmPackage }
//      G8D03E94  G26CEBFC  G0A2EF1F
  TpmPackage = class(TCollectionItem)
  private
    FName: string;
    FOwnerId: string;
    FPackageState: TpmPackageState;
    FPackageVersionList: TpmPackageVersionCollection;
    FCategoryId: Integer;
    FKeywordIds: TpmArrayOfInteger;
    procedure SetPackageState(AValue: TpmPackageState);
    function GetCategory: string;
    function GetKeywords: TpmArrayOfString;
    function GetKeywordIds: TpmArrayOfInteger;
    procedure SetKeywordsIds(AValue: TpmArrayOfInteger);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    function Validate(out AnErrStr: string): Boolean;
  published
    property Name: string read FName write FName;
    property OwnerId: string read FOwnerId write FOwnerId;
    property PackageState: TpmPackageState read FPackageState write SetPackageState;
    property PackageVersionList: TpmPackageVersionCollection read FPackageVersionList;
    property Category: string read GetCategory;
    property CategoryId: Integer read FCategoryId write FCategoryId;
    property Keywords: TpmArrayOfString read GetKeywords;
    property KeywordIds: TpmArrayOfInteger read GetKeywordIds write SetKeywordsIds;
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

procedure TpmPackageVersion.SetVersion(AValue: TFPVersion);
begin
  FVersion.Assign(AValue);
end;

constructor TpmPackageVersion.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FVersion := TFPVersion.Create;
end;

destructor TpmPackageVersion.Destroy;
begin
  FVersion.Free;
  inherited Destroy;
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

function TpmPackage.GetCategory: string;
var
  ACategory: TfprPackageCategory;
begin
  Result := '';
  if FCategoryId > 0 then
    begin
    ACategory := TfprPackageCategoryCollectionStackSingleton.GetAutoReadInstance('Category').FindObjectById(FCategoryId);
    if Assigned(ACategory) then
      Result := ACategory.Name;
    end;
end;

function TpmPackage.GetKeywordIds: TpmArrayOfInteger;
begin
  Result := FKeywordIds;
end;

function TpmPackage.GetKeywords: TpmArrayOfString;
var
  AKeyword: TfprPackageKeyword;
  i: Integer;
  t: array of string;
begin
  Result := [];
  for i := 0 to Length(FKeywordIds) -1 do
    begin
    AKeyword := TfprPackageKeywordCollectionStackSingleton.GetAutoReadInstance('Keyword').FindObjectById(FKeywordIds[i]);
    if not Assigned(AKeyword) then
      Result := Concat(Result, ['(deleted)'])
    else
      Result := Concat(Result, [AKeyword.Name]);
    end;
end;

procedure TpmPackage.SetKeywordsIds(AValue: TpmArrayOfInteger);
begin
  FKeywordIds := AValue;
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

end.

