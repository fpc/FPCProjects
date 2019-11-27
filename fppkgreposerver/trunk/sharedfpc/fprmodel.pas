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
  fprErrorHandling,
  fprGCollection,
  fprInterfacedCollection,
  fprJSONRTTI,
  fprStackClient;

type

  { TfprCrudCollectionItem }

  TfprCrudCollectionItem = class(TfprInterfacedCollectionItem)
  private
    FName: string;
  protected
    function GetId: Integer; virtual; abstract;
  published
    function CheckValidity: Boolean; virtual;
    property Name: string read FName write FName;
    property Id: Integer read GetId;
  end;

  { TcnocGCrudCollection }

  generic TcnocGCrudCollection<T: TfprCrudCollectionItem> = class(specialize TcnocGCollection<T>)
  public
    function FindObjectByName(AName: string): T;
    function FindObjectById(AnId: integer): T;
  end;

  { TfprCollectionStackSingleton }

  generic TfprCollectionStackSingleton<T: TCollection> = class(specialize TcnocSingleton<T>)
  private
    class var FVersion: Integer;
    class var FCachedVersion: Integer;
    class procedure LoadCollectionFromStack(StackName: string; Collection: T);
  public
    class procedure IncVersion;
    class function GetAutoReadInstance(const StackName: string): T;
  end;



  { TfprPackageCategory }

  TfprPackageCategory = class(TfprCrudCollectionItem)
  private
    FCategoryId: integer;
  protected
    function GetId: Integer; override;
  published
    property CategoryId: integer read FCategoryId write FCategoryId;
  end;

  TfprPackageCategoryCollection = specialize TcnocGCrudCollection<TfprPackageCategory>;

  TfprPackageCategoryCollectionSingleton = specialize TcnocSingleton<TfprPackageCategoryCollection>;

  TfprPackageCategoryCollectionStackSingleton = specialize TfprCollectionStackSingleton<TfprPackageCategoryCollection>;

  { TfprPackageCategoryMonitor }

  TfprPackageCategoryMonitor = class(IcnocStackHandleMessage)
  private
    procedure HandleMessage(const BinaryClient: TcnocStackBinaryClient; const IncomingMessage: PcnocStackMessage; var Handled: Boolean);
  end;

  TfprPackageCategoryMonitorSingleton = specialize TcnocSingleton<TfprPackageCategoryMonitor>;

  { TfprPackageKeyword }

  TfprPackageKeyword = class(TfprCrudCollectionItem)
  private
    FKeywordId: integer;
  protected
    function GetId: Integer; override;
  public
    function CheckValidity: Boolean; override;
  published
    property KeywordId: Integer read FKeywordId write FKeywordId;
  end;

  TfprPackageKeywordCollection = specialize TcnocGCrudCollection<TfprPackageKeyword>;

  TfprPackageKeywordCollectionSingleton = specialize TcnocSingleton<TfprPackageKeywordCollection>;

  TfprPackageKeywordCollectionStackSingleton = specialize TfprCollectionStackSingleton<TfprPackageKeywordCollection>;

  { TfprPackageKeywordMonitor }

  TfprPackageKeywordMonitor = class(IcnocStackHandleMessage)
  private
    procedure HandleMessage(const BinaryClient: TcnocStackBinaryClient; const IncomingMessage: PcnocStackMessage; var Handled: Boolean);
  end;

  TfprPackageKeywordMonitorSingleton = specialize TcnocSingleton<TfprPackageKeywordMonitor>;

implementation

procedure TfprPackageKeywordMonitor.HandleMessage(const BinaryClient: TcnocStackBinaryClient; const IncomingMessage: PcnocStackMessage; var Handled: Boolean);
var
  msg: String;
begin
  msg := IncomingMessage^.GetContentsAsAnsiString;
  TfprPackageKeywordCollectionStackSingleton.IncVersion;
  Handled := true;
end;

function TfprPackageKeyword.CheckValidity: Boolean;
begin
  Result := inherited CheckValidity;

  if Name = '' then
    raise EJsonWebException.CreateFmtHelp('Keyword [%d] must have a name.', [FKeywordId], 422);

  if (length(Name) < 3) or (length(Name) > 15) then
    raise EJsonWebException.CreateFmtHelp('The keyword name [%s] must have between 3 and 15 characters.', [Name], 422);
end;

function TfprPackageKeyword.GetId: Integer;
begin
  Result := KeywordId;
end;

{ TfprCrudCollectionItem }

function TfprCrudCollectionItem.CheckValidity: Boolean;
begin
  Result := True;
end;

{ TfprPackageCategory }

function TfprPackageCategory.GetId: Integer;
begin
  Result := CategoryId;
end;

{ TfprCollectionStackSingleton }

class function TfprCollectionStackSingleton.GetAutoReadInstance(const StackName: string): T;
var
  AVersion: Integer;
begin
  Result := Instance;
  AVersion := InterlockedCompareExchange(FVersion, 0, 0);
  if AVersion >= FCachedVersion then
    begin
    FCachedVersion := AVersion + 1;
    LoadCollectionFromStack(StackName, Result);
    end;
end;

class procedure TfprCollectionStackSingleton.IncVersion;
begin
  InterlockedIncrement(FVersion);
end;

class procedure TfprCollectionStackSingleton.LoadCollectionFromStack(StackName: string; Collection: T);
var
  Response: string;
  DeStreamer: TfprJSONDeStreamer;
begin
  if TfprStackClientSingleton.Instance.Client.SendMessage(Stackname, '{"name":"crud","action":"get"}', Response) = TcnocStackErrorCodes.ecNone then
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

{ TcnocGCrudCollection }

function TcnocGCrudCollection.FindObjectById(AnId: integer): T;
var
  i: Integer;
begin
  result := Nil;;
  for i := 0 to Count -1 do
    if AnId = Items[i].Id then
      begin
      Result := Items[i];
      Break;
      end;
end;

function TcnocGCrudCollection.FindObjectByName(AName: string): T;
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

{ TfprPackageCategoryMonitor }

procedure TfprPackageCategoryMonitor.HandleMessage(const BinaryClient: TcnocStackBinaryClient; const IncomingMessage: PcnocStackMessage; var Handled: Boolean);
var
  msg: String;
begin
  msg := IncomingMessage^.GetContentsAsAnsiString;
  TfprPackageCategoryCollectionStackSingleton.IncVersion;
  Handled := true;
end;

end.

