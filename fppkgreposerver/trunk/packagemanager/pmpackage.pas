unit pmPackage;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fgl,
  fpjsonrtti;

type
  TpmPackageState = (pmpsInitial);

  { TpmPackage }

  TpmPackage = class
  private
    FName: string;
    FOwnerId: string;
    FPackageState: TpmPackageState;
    procedure SetPackageState(AValue: TpmPackageState);
  public
    function Validate(out AnErrStr: string): Boolean;
  published
    property Name: string read FName write FName;
    property OwnerId: string read FOwnerId write FOwnerId;
    property PackageState: TpmPackageState read FPackageState write SetPackageState;

  end;

  TpmGenPackageList = specialize TFPGObjectList<TpmPackage>;

  { TpmPackageList }

  TpmPackageList = class(TpmGenPackageList)
  private
    class var FPackageList: TpmPackageList;
    class function GetInstance: TpmPackageList; static;
  public
    class constructor Create;
    class destructor Destroy;
    class property Instance: TpmPackageList read GetInstance;
    function FindPackageByName(AName: string): TpmPackage;
  end;

const
  CpmPackageStateString: array[TpmPackageState] of string = ('new');

implementation

{ TpmPackage }

procedure TpmPackage.SetPackageState(AValue: TpmPackageState);
begin
  if FPackageState = AValue then Exit;
  FPackageState := AValue;
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

{ TpmPackageList }

class function TpmPackageList.GetInstance: TpmPackageList; static;
begin
  Result := FPackageList;
end;

class constructor TpmPackageList.Create;
begin
  FPackageList := TpmPackageList.Create(True);
end;

class destructor TpmPackageList.Destroy;
begin
  FPackageList.Free;
end;

function TpmPackageList.FindPackageByName(AName: string): TpmPackage;
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

