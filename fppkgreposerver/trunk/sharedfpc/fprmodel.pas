unit fprModel;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpjson,
  fpjsonrtti,
  dcsGlobalSettings,
  cnocStackHandlerThread,
  cnocStackBinaryClient,
  cnocStackMessageTypes,
  cnocStackErrorCodes,
  fprGCollection,
  fprInterfacedCollection,
  fprJSONRTTI,
  fprStackClient;

type

  { TfprPackageCategory }

  TfprPackageCategory = class(TfprInterfacedCollectionItem)
  private
    FName: string;
    FCategoryId: integer;
  published
    property CategoryId: integer read FCategoryId write FCategoryId;
    property Name: string read FName write FName;
  end;

  TfprGenPackageCategoryCollection = specialize TcnocGCollection<TfprPackageCategory>;

  { TfprPackageCategoryCollection }

  TfprPackageCategoryCollection = class(TfprGenPackageCategoryCollection)
  public
    function FindCategoryByName(AName: string): TfprPackageCategory;
    function FindCategoryById(ACategoryId: integer): TfprPackageCategory;
  end;

  TfprPackageCategoryCollectionSingleton = specialize TcnocSingleton<TfprPackageCategoryCollection>;

  { TfprPackageCategoryCollectionStackSingleton }

  TfprPackageCategoryCollectionStackSingleton = class(TfprPackageCategoryCollectionSingleton)
  private
    class var FVersion: Integer;
    class var FCachedVersion: Integer;
    class procedure LoadCategoryCollectionFromStack(Collection: TfprPackageCategoryCollection);
  public
    class procedure IncVersion;
    class function GetAutoReadInstance: TfprPackageCategoryCollection;
  end;

  { TfprPackageCategoryMonitor }

  TfprPackageCategoryMonitor = class(IcnocStackHandleMessage)
  private
    procedure HandleMessage(const BinaryClient: TcnocStackBinaryClient; const IncomingMessage: PcnocStackMessage; var Handled: Boolean);
  end;

  TfprPackageCategoryMonitorSingleton = specialize TcnocSingleton<TfprPackageCategoryMonitor>;

implementation

{ TfprPackageCategoryCollectionStackSingleton }

class function TfprPackageCategoryCollectionStackSingleton.GetAutoReadInstance: TfprPackageCategoryCollection;
var
  AVersion: Integer;
begin
  Result := Instance;
  AVersion := InterlockedCompareExchange(FVersion, 0, 0);
  if AVersion >= FCachedVersion then
    begin
    FCachedVersion := AVersion + 1;
    LoadCategoryCollectionFromStack(Result);
    end;
end;

class procedure TfprPackageCategoryCollectionStackSingleton.IncVersion;
begin
  InterlockedIncrement(FVersion);
end;

class procedure TfprPackageCategoryCollectionStackSingleton.LoadCategoryCollectionFromStack(Collection: TfprPackageCategoryCollection);
var
  Response: string;
  DeStreamer: TfprJSONDeStreamer;
begin
  if TfprStackClientSingleton.Instance.Client.SendMessage('Category', '{"name":"crud","action":"get"}', Response) = TcnocStackErrorCodes.ecNone then
    begin
    DeStreamer := TfprJSONDeStreamer.Create(nil);
    try
      DeStreamer.Options := DeStreamer.Options + [jdoCaseInsensitive];
      TfprPackageCategoryCollectionSingleton.Instance.Clear;
      DeStreamer.JSONToCollection(Response, Collection);
    finally
      DeStreamer.Free;
    end;
    end;
end;

{ TfprPackageCategoryMonitor }

procedure TfprPackageCategoryMonitor.HandleMessage(const BinaryClient: TcnocStackBinaryClient; const IncomingMessage: PcnocStackMessage; var Handled: Boolean);
var
  msg: String;
begin
  msg := IncomingMessage^.GetContentsAsAnsiString;
  TfprPackageCategoryCollectionStackSingleton.IncVersion;
  Handled := true;
end;

{ TfprPackageCategoryCollection }

function TfprPackageCategoryCollection.FindCategoryByName(AName: string): TfprPackageCategory;
var
  i: Integer;
begin
  result := Nil;;
  for i := 0 to Count -1 do
    if sametext(AName, Items[i].Name) then
      begin
      Result := Items[i];
      Break;
      end;
end;

function TfprPackageCategoryCollection.FindCategoryById(ACategoryId: integer): TfprPackageCategory;
var
  i,j: Integer;  s: string;
begin
  result := Nil;;
  for i := 0 to Count -1 do
    begin
    j := Items[i].Categoryid;
    s := Items[i].Name;
    if ACategoryId = Items[i].Categoryid then
      begin
      Result := Items[i];
      Break;
      end;

    end;
end;

end.

