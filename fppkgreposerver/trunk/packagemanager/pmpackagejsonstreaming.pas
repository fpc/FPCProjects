unit pmPackageJSonStreaming;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fgl,
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
  pmPackage;

type

  { TpmPackageJSonStreaming }

  TpmPackageJSonStreaming = class
  private
    FFilterOutOldVersions: Boolean;
    FStackClient: TcnocStackBinaryClient;

    FSerializer: TJSONRttiStreamClass;

    function CollectionToJSon(AList: TCollection): TJSONArray;
    function PackageVersionCollectionToJSonFiltered(APackage: TpmPackage; AVersionCollection: TpmPackageVersionCollection): TJSONArray;

    Procedure StreamerStreamProperty(Sender: TObject; AObject: TObject; Info: PPropInfo; var Res: TJSONData);
    function SetPackageStateAsString(AnInstance: TObject; ADescription: TcsStreamDescription; const AValue: string): boolean;
    function GetPackageStateAsString(AnInstance: TObject; ADescription: TcsStreamDescription; out AValue: string): boolean;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function PackageToJSon(APackage: TpmPackage): string;
    procedure JSonToPackage(AJSonStr: String; APackage: TpmPackage);

    function PackageCollectionToJSon(APackageList: TpmPackageCollection): string;
    function PackageVersionCollectionToJSon(APackageVersionList: TpmPackageVersionCollection): string;
    procedure SavePackageCollectionToFile(APackageList: TpmPackageCollection; Filename: string);
    procedure JSonToPackageCollection(AJSonStr: string; APackageList: TpmPackageCollection);
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

function TpmPackageJSonStreaming.CollectionToJSon(AList: TCollection): TJSONArray;
var
  Streamer: TJSONStreamer;
begin
  Streamer := TJSONStreamer.Create(nil);
  try
    Streamer.OnStreamProperty := @StreamerStreamProperty;
    Streamer.Options := Streamer.Options + [jsoLowerPropertyNames];
    Result := Streamer.StreamCollection(AList);
  finally
    Streamer.Free;
  end;
end;

Procedure TpmPackageJSonStreaming.StreamerStreamProperty(Sender: TObject; AObject: TObject; Info: PPropInfo; var Res: TJSONData);
var
  PackageState: TpmPackageState;
  AnObject: TObject;
begin
  if Info^.Name='PackageState' then
    begin
    PackageState := TpmPackageState(GetOrdProp(AObject, Info));
    Res := TJSONString.Create(CpmPackageStateString[PackageState]);
    end
  else if Info^.PropType^.Kind = tkClass then
    begin
    AnObject := GetObjectProp(AObject, Info);
    if AnObject is TpmPackageVersionCollection then
      begin
      if AObject is TpmPackage then
        Res := PackageVersionCollectionToJSonFiltered(TpmPackage(AObject), TpmPackageVersionCollection(AnObject))
      else
        Res := PackageVersionCollectionToJSonFiltered(nil, TpmPackageVersionCollection(AnObject));
      end;
    end;
end;

function TpmPackageJSonStreaming.PackageToJSon(APackage: TpmPackage): string;
begin
  Result := FSerializer.ObjectToJSONString(APackage);
end;

procedure TpmPackageJSonStreaming.JSonToPackage(AJSonStr: String; APackage: TpmPackage);
begin
  try
    FSerializer.JSONStringToObject(AJSonStr, APackage);
  except
    on E: Exception do
      raise EJsonWebException.CreateFmt('Failed to parse package-data: %s', [E.Message]);
  end;
end;

function TpmPackageJSonStreaming.PackageCollectionToJSon(APackageList: TpmPackageCollection): string;
var
  JSONArr: TJSONArray;
begin
  JSONArr := FSerializer.ObjectToJSON(APackageList) as TJSONArray;
  try
    Result := JSONArr.AsJSON;
  finally
    JSONArr.Free;
  end;
end;

function TpmPackageJSonStreaming.PackageVersionCollectionToJSon(APackageVersionList: TpmPackageVersionCollection): string;
var
  JSONArr: TJSONArray;
begin
  JSONArr := CollectionToJSon(APackageVersionList);
  try
    Result := JSONArr.AsJSON;
  finally
    JSONArr.Free;
  end;
end;

function TpmPackageJSonStreaming.PackageVersionCollectionToJSonFiltered(APackage: TpmPackage;
  AVersionCollection: TpmPackageVersionCollection): TJSONArray;
var
  Streamer: TJSONStreamer;
  I: Integer;
  JSONObject: TJSONObject;
  ResponseStr: string;

  RepPackageCol: TpmRepositoryPackageCollection;

  StackResult: TcnocStackErrorCodes;
  DeStreamer: TJSONDeStreamer;
begin
  RepPackageCol := nil;
  Streamer := TJSONStreamer.Create(nil);
  try
    Streamer.OnStreamProperty := @StreamerStreamProperty;
    Streamer.Options := Streamer.Options + [jsoLowerPropertyNames];

    if Assigned(APackage) and Assigned(FStackClient) and (FilterOutOldVersions) then
      begin
      JSONObject := TJSONObject.Create(['name', 'package', 'method', 'GET', 'package', APackage.Name]);
      try
        StackResult := FStackClient.SendMessage('Repository', JSONObject.AsJSON, ResponseStr);
        DeStreamer := TJSONDeStreamer.Create(nil);
        try
          DeStreamer.Options := [jdoCaseInsensitive];
          RepPackageCol := TpmRepositoryPackageCollection.Create;
          DeStreamer.JSONToCollection(ResponseStr, RepPackageCol);
        finally
          DeStreamer.Free;
        end;
      finally
        JSONObject.Free;
      end;
      end;

    Result:=TJSONArray.Create;
    try
      for I:=0 to AVersionCollection.Count-1 do
        begin
        if not assigned(RepPackageCol) or Assigned(RepPackageCol.FindRepositoryPackageByTag(AVersionCollection.Items[i].Tag)) then
          Result.Add(Streamer.ObjectToJSON(AVersionCollection.Items[i]));
        end;
    except
      FreeAndNil(Result);
      Raise;
    end;

  finally
    Streamer.Free;
    RepPackageCol.Free;
  end;
end;

constructor TpmPackageJSonStreaming.Create;
begin
  inherited Create;
  FSerializer := TJSONRttiStreamClass.Create;
  FSerializer.DescriptionStore.Describer.DefaultExportNameStyle := tcsensLowerCase;
  FSerializer.DescriptionStore.Describer.DefaultImportNameStyle := tcsinsLowerCase;
  FSerializer.DescriptionStore.Describer.Flags := [tcsdfCollectionAsList];

  FSerializer.DescriptionStore.GetDescription(TpmPackageCollection).ListDescription.DefaultSubObjectDescription :=
    FSerializer.DescriptionStore.GetDescription(TpmPackage);

  FSerializer.DescriptionStore.GetDescription(TpmPackage).Properties.FindByPropertyName('PackageState').OnSetValueAsString := @SetPackageStateAsString;
  FSerializer.DescriptionStore.GetDescription(TpmPackage).Properties.FindByPropertyName('PackageState').OnGetValueAsString := @GetPackageStateAsString;

  FSerializer.DescriptionStore.GetDescription(TpmPackage).Properties.FindByPropertyName('PackageVersionList').ListDescription.DefaultSubObjectDescription := FSerializer.DescriptionStore.GetDescription(TpmPackageVersion).Clone;
end;

destructor TpmPackageJSonStreaming.Destroy;
begin
  FSerializer.Free;
  inherited Destroy;
end;

function TpmPackageJSonStreaming.SetPackageStateAsString(AnInstance: TObject; ADescription: TcsStreamDescription; const AValue: string): boolean;
var
  State: TpmPackageState;
begin
  Assert(AnInstance is TpmPackage);
  Result:=False;
  for State := low(TpmPackageState) to high (TpmPackageState) do
    begin
    if SameText(CpmPackageStateString[State], AValue) then
      begin
      TpmPackage(AnInstance).PackageState := State;
      Result := True;
      Exit;
      end;
    end;
end;

function TpmPackageJSonStreaming.GetPackageStateAsString(AnInstance: TObject; ADescription: TcsStreamDescription; out AValue: string): boolean;
begin
  Assert(AnInstance is TpmPackage);
  AValue := CpmPackageStateString[TpmPackage(AnInstance).PackageState];
  Result := True;
end;

procedure TpmPackageJSonStreaming.JSonToPackageCollection(AJSonStr: string; APackageList: TpmPackageCollection);
begin
  FSerializer.JSONStringToObject(AJSonStr, APackageList);
end;

procedure TpmPackageJSonStreaming.SavePackageCollectionToFile(APackageList: TpmPackageCollection; Filename: string);
var
  FS: TStringStream;
begin
  FS := TStringStream.Create(PackageCollectionToJSon(APackageList));
  try
    FS.SaveToFile(Filename);
  finally
    FS.Free;
  end;
end;

end.

