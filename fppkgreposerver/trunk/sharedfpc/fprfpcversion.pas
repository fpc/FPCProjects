unit fprFPCVersion;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  dcsGlobalSettings,
  fprGCollection,
  fprInterfacedCollection;

type
  { TfprFPCVersion }

  TfprFPCVersion = class(TfprInterfacedCollectionItem)
  private
    FIsDefault: Boolean;
    FName: string;
    FURLPrefix: string;
  public
    function GetBranchName: string;
  published
    property Name: string read FName write FName;
    property URLPrefix: string read FURLPrefix write FURLPrefix;
    property IsDefault: Boolean read FIsDefault write FIsDefault;
  end;

  TfprGenFPCVersionCollection = specialize TcnocGCollection<TfprFPCVersion>;

  { TfprFPCVersionCollection }

  TfprFPCVersionCollection = class(TfprGenFPCVersionCollection)
  private
    class var FFPCVersionCollection: TfprFPCVersionCollection;
    function GetDefaultVersion: TfprFPCVersion;
    class function GetInstance: TfprFPCVersionCollection; static;
  public
    class constructor Create;
    class destructor Destroy;
    class property Instance: TfprFPCVersionCollection read GetInstance;
    procedure LoadFromSettings;
    function FindVersion(AName: string): TfprFPCVersion;
    property DefaultVersion: TfprFPCVersion read GetDefaultVersion;
  end;

implementation

{ TfprFPCVersion }

function TfprFPCVersion.GetBranchName: string;
begin
  Result := 'FPC' + StringReplace(FName, '.', '_', [rfReplaceAll]);
end;

{ TpmFPCVersionCollection }

class function TfprFPCVersionCollection.GetInstance: TfprFPCVersionCollection;
begin
  Result := FFPCVersionCollection;
end;

function TfprFPCVersionCollection.GetDefaultVersion: TfprFPCVersion;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count -1 do
    begin
    if Items[i].IsDefault then
      begin
      Result := Items[i];
      Exit;
      end;
    end;
end;

class constructor TfprFPCVersionCollection.Create;
begin
  FFPCVersionCollection := TfprFPCVersionCollection.Create();
end;

class destructor TfprFPCVersionCollection.Destroy;
begin
  FFPCVersionCollection.Free;
end;

procedure TfprFPCVersionCollection.LoadFromSettings;
var
  SettingTemplate: TDCSSettingTemplate;
  i: Integer;
  FPCVersion: TfprFPCVersion;
  Settings: TDCSGlobalSettings;
begin
  Settings := TDCSGlobalSettings.GetInstance;
  SettingTemplate := Settings.SettingTemplateList.FindSettingTemplate('FPCVersion_');
  for i := 0 to SettingTemplate.Values.Count -1 do
    begin
    FPCVersion := TfprFPCVersion.Create(Self);
    FPCVersion.Name := Settings.GetSettingAsString('FPCVersionName-'+SettingTemplate.Values[i]);;
    FPCVersion.URLPrefix := Settings.GetSettingAsString('FPCVersionURLPrefix-'+SettingTemplate.Values[i]);;
    FPCVersion.IsDefault := Settings.GetSettingAsBoolean('FPCVersionIsDefault-'+SettingTemplate.Values[i]);;
    end;
end;

function TfprFPCVersionCollection.FindVersion(AName: string): TfprFPCVersion;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count -1 do
    begin
    if Items[i].Name = AName then
      begin
      Result := Items[i];
      Exit;
      end;
    end;
end;

end.

