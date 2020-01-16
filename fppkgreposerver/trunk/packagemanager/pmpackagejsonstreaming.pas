unit pmPackageJSonStreaming;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpjsonrtti,
  fpjson,
  typinfo,
  cnocStackBinaryClient,
  cnocStackMessageTypes,
  cnocStackErrorCodes,
  csJSONRttiStreamHelper,
  csModel,
  fprErrorHandling,
  fprGCollection,
  fprInterfaceClasses,
  pmPackage;

type

  { TpmPackageJSonStreaming }

  TpmPackageJSonStreaming = class
  private
    FFilterOutOldVersions: Boolean;
    FStackClient: TcnocStackBinaryClient;

    FSerializer: TJSONRttiStreamClass;

    function SetPackageStateAsString(AnInstance: TObject; ADescription: TcsStreamDescription; const AValue: string): boolean;
    function GetPackageStateAsString(AnInstance: TObject; ADescription: TcsStreamDescription; out AValue: string): boolean;
    function DoFilterPackageVersions(AnInstance: TObject; ADescription: TcsStreamDescription): boolean;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function PackageToJSon(APackage: TfprPackage): TJSONData;
    function PackageVersionToJSon(APackageVersion: TfprPackageVersion): TJSONData;
    procedure JSonToPackage(AJSonStr: String; APackage: TfprPackage);
    procedure JSonToPatchPackage(AJSonStr: String; APatchPackage: TpmPatchPackage);

    function PackageCollectionToJSon(APackageList: TfprPackageCollection; ReleasedVersionInformationOnly: Boolean): TJSONData;
    function PackageCollectionToJSonString(APackageList: TfprPackageCollection; ReleasedVersionInformationOnly: Boolean): string;
    procedure SavePackageCollectionToFile(APackageList: TfprPackageCollection; Filename: string);
    procedure JSonToPackageCollection(AJSonStr: string; APackageList: TfprPackageCollection);
    property StackClient: TcnocStackBinaryClient read FStackClient write FStackClient;
    property FilterOutOldVersions: Boolean read FFilterOutOldVersions write FFilterOutOldVersions;
  end;

implementation

type

  { TpmRepositoryPackage }

  TpmRepositoryPackage = class(TCollectionItem)
  private
    FName: string;
    FTag: string;
  published
    property Name: string read FName write FName;
    property Tag: string read FTag write FTag;
  end;
  TpmCustomRepositoryPackageCollection = specialize TcnocGCollection<TpmRepositoryPackage>;

  { TpmRepositoryPackageCollection }

  TpmRepositoryPackageCollection = class(TpmCustomRepositoryPackageCollection)
  public
    function FindRepositoryPackageByTag(const Tag: string): TpmRepositoryPackage;
  end;

{ TpmRepositoryPackageCollection }

function TpmRepositoryPackageCollection.FindRepositoryPackageByTag(const Tag: string): TpmRepositoryPackage;
var
  i: Integer;
begin
  for i := 0 to Count -1 do
    if Items[i].Tag = Tag then
      begin
      Result := Items[i];
      Exit
      end;
  Result := nil;
end;

{ TpmPackageJSonStreaming }

function TpmPackageJSonStreaming.PackageToJSon(APackage: TfprPackage): TJSONData;
begin
  Result := FSerializer.ObjectToJSON(APackage);
end;

procedure TpmPackageJSonStreaming.JSonToPackage(AJSonStr: String; APackage: TfprPackage);
begin
  try
    FSerializer.JSONStringToObject(AJSonStr, APackage);
  except
    on E: Exception do
      raise EJsonWebException.CreateFmt('Failed to parse package-data: %s', [E.Message]);
  end;
end;

function TpmPackageJSonStreaming.PackageCollectionToJSonString(APackageList: TfprPackageCollection; ReleasedVersionInformationOnly: Boolean): string;
var
  JSONData: TJSONData;
begin
  JSONData := PackageCollectionToJSon(APackageList, ReleasedVersionInformationOnly);
  try
    Result := JSONData.AsJSON;
  finally
    JSONData.Free;
  end;
end;

function TpmPackageJSonStreaming.PackageCollectionToJSon(APackageList: TfprPackageCollection; ReleasedVersionInformationOnly: Boolean): TJSONData;
var
  DescriptionTag: string;
begin
  if ReleasedVersionInformationOnly then
    DescriptionTag := 'ReleasedVersionInformationOnly'
  else
    DescriptionTag := '';
  Result := FSerializer.ObjectToJSON(APackageList, DescriptionTag) as TJSONArray;
end;

constructor TpmPackageJSonStreaming.Create;
var
  PackageDescription: TcsStreamDescription;
  PackageCollDescription: TcsStreamDescription;
  FilteredPackageDescription: TcsStreamDescription;
  FilteredPackageCollDescription: TcsStreamDescription;
begin
  inherited Create;
  FSerializer := TJSONRttiStreamClass.Create;
  FSerializer.DescriptionStore.Describer.DefaultExportNameStyle := tcsensLowerCase;
  FSerializer.DescriptionStore.Describer.DefaultImportNameStyle := tcsinsLowerCase;
  FSerializer.DescriptionStore.Describer.Flags := [tcsdfCollectionAsList];

  PackageDescription := FSerializer.DescriptionStore.GetDescription(TfprPackage);
  PackageDescription.Properties.FindByPropertyName('PackageState').OnSetValueAsString := @SetPackageStateAsString;
  PackageDescription.Properties.FindByPropertyName('PackageState').OnGetValueAsString := @GetPackageStateAsString;

  PackageDescription.Properties.FindByPropertyName('PackageVersionList').ListDescription.DefaultSubObjectDescription := FSerializer.DescriptionStore.GetDescription(TfprPackageVersion){.Clone};

  PackageCollDescription := FSerializer.DescriptionStore.GetDescription(TfprPackageCollection);

  FilteredPackageDescription := FSerializer.DescriptionStore.CloneDescription(TfprPackage, '', 'ReleasedVersionInformationOnly');
  FilteredPackageCollDescription := FSerializer.DescriptionStore.CloneDescription(TfprPackageCollection, '', 'ReleasedVersionInformationOnly');
  FilteredPackageDescription.Properties.FindByPropertyName('PackageVersionList').ListDescription.OnFilter := @DoFilterPackageVersions;

  PackageCollDescription.ListDescription.DefaultSubObjectDescription := PackageDescription;
  FilteredPackageCollDescription.ListDescription.DefaultSubObjectDescription := FilteredPackageDescription;
end;

destructor TpmPackageJSonStreaming.Destroy;
begin
  FSerializer.Free;
  inherited Destroy;
end;

function TpmPackageJSonStreaming.SetPackageStateAsString(AnInstance: TObject; ADescription: TcsStreamDescription; const AValue: string): boolean;
var
  State: TfprPackageState;
begin
  Assert(AnInstance is TfprPackage);
  Result:=False;
  for State := low(TfprPackageState) to high (TfprPackageState) do
    begin
    if SameText(CpmPackageStateString[State], AValue) then
      begin
      TfprPackage(AnInstance).PackageState := State;
      Result := True;
      Exit;
      end;
    end;
end;

function TpmPackageJSonStreaming.GetPackageStateAsString(AnInstance: TObject; ADescription: TcsStreamDescription; out AValue: string): boolean;
begin
  Assert(AnInstance is TfprPackage);
  AValue := CpmPackageStateString[TfprPackage(AnInstance).PackageState];
  Result := True;
end;

procedure TpmPackageJSonStreaming.JSonToPackageCollection(AJSonStr: string; APackageList: TfprPackageCollection);
begin
  FSerializer.JSONStringToObject(AJSonStr, APackageList);
end;

procedure TpmPackageJSonStreaming.SavePackageCollectionToFile(APackageList: TfprPackageCollection; Filename: string);
var
  FS: TStringStream;
begin
  FS := TStringStream.Create(PackageCollectionToJSonString(APackageList, False));
  try
    FS.SaveToFile(Filename);
  finally
    FS.Free;
  end;
end;

function TpmPackageJSonStreaming.DoFilterPackageVersions(AnInstance: TObject; ADescription: TcsStreamDescription): boolean;
var
  Version: TfprPackageVersion;
  JSONObject: TJSONObject;
  Package: TfprPackage;
  ResponseStr: string;
  StackResult: TcnocStackErrorCodes;
  DeStreamer: TJSONDeStreamer;
  RepPackageCol: TpmRepositoryPackageCollection;
begin
  Result := False;
  Package := (AnInstance as TCollection).Owner as TfprPackage;
  Version := (AnInstance as TCollection).Items[ADescription.Index] as TfprPackageVersion;
  if Assigned(FStackClient) then
    begin
    JSONObject := TJSONObject.Create(['name', 'package', 'method', 'GET', 'package', Package.Name]);
    try
      StackResult := FStackClient.SendMessage('Repository', JSONObject.AsJSON, ResponseStr);
      DeStreamer := TJSONDeStreamer.Create(nil);
      try
        DeStreamer.Options := [jdoCaseInsensitive];
        RepPackageCol := TpmRepositoryPackageCollection.Create;
        DeStreamer.JSONToCollection(ResponseStr, RepPackageCol);
        Result := Assigned(RepPackageCol.FindRepositoryPackageByTag(Version.Tag));
      finally
        DeStreamer.Free;
      end;
    finally
      JSONObject.Free;
    end;
    end;
end;

procedure TpmPackageJSonStreaming.JSonToPatchPackage(AJSonStr: String; APatchPackage: TpmPatchPackage);
begin
  try
    FSerializer.JSONStringToObject(AJSonStr, APatchPackage);
  except
    on E: Exception do
      raise EJsonWebException.CreateFmt('Failed to parse package-data: %s', [E.Message]);
  end;
end;

function TpmPackageJSonStreaming.PackageVersionToJSon(APackageVersion: TfprPackageVersion): TJSONData;
begin
  Result := FSerializer.ObjectToJSON(APackageVersion);
end;

end.

