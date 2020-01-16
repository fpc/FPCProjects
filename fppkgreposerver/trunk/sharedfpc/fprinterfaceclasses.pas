unit fprInterfaceClasses;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpmkunit,
  fprGCollection,
  fprFPCVersion,
  fprModel;

type
  TfprPackageState = (prspsInitial, prspsAcceptance, prspsApproved, prspsPublished, prspsRevoked);
  TfprArrayOfString = array of string;
  TfprArrayOfInteger = array of integer;

  { TfprPackageVersion }

  TfprPackageVersion = class(TCollectionItem)
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

  TfprGenPackageVersionCollection = specialize TcnocGOwnedCollection<TfprPackageVersion>;

  { TfprPackageVersionCollection }

  TfprPackageVersionCollection = class(TfprGenPackageVersionCollection)
  public
    function FindVersionByTag(AFPCVersion, ATag: string): TfprPackageVersion;
  end;

  { TfprPackage }

  TfprPackage = class(TCollectionItem)
  private
    FName: string;
    FOwnerId: string;
    FPackageState: TfprPackageState;
    FPackageVersionList: TfprPackageVersionCollection;
    FCategoryId: Integer;
    FKeywordIds: TfprArrayOfInteger;
    FSupport: string;
    procedure SetPackageState(AValue: TfprPackageState);
    function GetCategory: string;
    function GetKeywords: TfprArrayOfString;
    function GetKeywordIds: TfprArrayOfInteger;
    procedure SetKeywordsIds(AValue: TfprArrayOfInteger);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    function Validate(out AnErrStr: string): Boolean;
  published
    property Name: string read FName write FName;
    property OwnerId: string read FOwnerId write FOwnerId;
    property PackageState: TfprPackageState read FPackageState write SetPackageState;
    property PackageVersionList: TfprPackageVersionCollection read FPackageVersionList;
    property Category: string read GetCategory;
    property CategoryId: Integer read FCategoryId write FCategoryId;
    property Keywords: TfprArrayOfString read GetKeywords;
    property KeywordIds: TfprArrayOfInteger read GetKeywordIds write SetKeywordsIds;
    property Support: string read FSupport write FSupport;
  end;

  TfprGenPackageCollection = specialize TcnocGCollection<TfprPackage>;

  { TfprPackageCollection }

  TfprPackageCollection = class(TfprGenPackageCollection)
  private
    class var FPackageList: TfprPackageCollection;
    class function GetInstance: TfprPackageCollection; static;
  public
    class constructor Create;
    class destructor Destroy;
    class property Instance: TfprPackageCollection read GetInstance;
    function FindPackageByName(AName: string): TfprPackage;
  end;

const
  CfprPackageStateString: array[TfprPackageState] of string = ('new', 'acceptance', 'approved', 'published', 'revoked');

implementation

{ TfprPackageVersion }

procedure TfprPackageVersion.SetFPCVersion(AValue: string);
begin
  if FFPCVersion<>AValue then
    begin
    if not Assigned(TfprFPCVersionCollection.Instance.FindVersion(AValue)) then
      raise Exception.CreateFmt('Unknown FPC-Version [%s]', [AValue]);
    FFPCVersion := AValue;
    end;
end;

procedure TfprPackageVersion.SetVersion(AValue: TFPVersion);
begin
  FVersion.Assign(AValue);
end;

constructor TfprPackageVersion.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FVersion := TFPVersion.Create;
end;

destructor TfprPackageVersion.Destroy;
begin
  FVersion.Free;
  inherited Destroy;
end;

{ TfprPackageVersionCollection }

function TfprPackageVersionCollection.FindVersionByTag(AFPCVersion, ATag: string): TfprPackageVersion;
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

{ TfprPackage }

procedure TfprPackage.SetPackageState(AValue: TfprPackageState);
begin
  if FPackageState = AValue then Exit;
  FPackageState := AValue;
end;

constructor TfprPackage.Create(ACollection: TCollection);
begin
  inherited;
  FPackageVersionList := TfprPackageVersionCollection.Create(Self);
end;

destructor TfprPackage.Destroy;
begin
  FPackageVersionList.Free;
  inherited Destroy;
end;

function TfprPackage.Validate(out AnErrStr: string): Boolean;
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

function TfprPackage.GetCategory: string;
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

function TfprPackage.GetKeywordIds: TfprArrayOfInteger;
begin
  Result := FKeywordIds;
end;

function TfprPackage.GetKeywords: TfprArrayOfString;
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

procedure TfprPackage.SetKeywordsIds(AValue: TfprArrayOfInteger);
begin
  FKeywordIds := AValue;
end;

{ TfprPackageCollection }

class function TfprPackageCollection.GetInstance: TfprPackageCollection; static;
begin
  Result := FPackageList;
end;

class constructor TfprPackageCollection.Create;
begin
  FPackageList := TfprPackageCollection.Create();
end;

class destructor TfprPackageCollection.Destroy;
begin
  FPackageList.Free;
end;

function TfprPackageCollection.FindPackageByName(AName: string): TfprPackage;
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

