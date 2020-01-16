unit fprSerializer;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  csModel,
  csJSONRttiStreamHelper,
  fprInterfaceClasses,
  fprGCollection;

type

  { TfprSerializer }

  TfprSerializer = class(TJSONRttiStreamClass)
  private
    function SetPackageStateAsString(AnInstance: TObject; ADescription: TcsStreamDescription; const AValue: string): boolean;
    function GetPackageStateAsString(AnInstance: TObject; ADescription: TcsStreamDescription; out AValue: string): boolean;
  public
    constructor Create;
  end;

  TfprSerializerSingleton = specialize TcnocSingleton<TfprSerializer>;

implementation

{ TfprSerializer }

constructor TfprSerializer.Create;
var
  PackageDescription: TcsStreamDescription;
begin
  Inherited;
  DescriptionStore.Describer.DefaultExportNameStyle := tcsensLowerCase;
  DescriptionStore.Describer.DefaultImportNameStyle := tcsinsLowerCase;
  DescriptionStore.Describer.Flags := [tcsdfCollectionAsList];

  PackageDescription := DescriptionStore.GetDescription(TfprPackage);
  PackageDescription.Properties.FindByPropertyName('PackageState').OnSetValueAsString := @SetPackageStateAsString;
  PackageDescription.Properties.FindByPropertyName('PackageState').OnGetValueAsString := @GetPackageStateAsString;
end;

function TfprSerializer.GetPackageStateAsString(AnInstance: TObject; ADescription: TcsStreamDescription; out AValue: string): boolean;
begin
  Assert(AnInstance is TfprPackage);
  AValue := CfprPackageStateString[TfprPackage(AnInstance).PackageState];
  Result := True;
end;

function TfprSerializer.SetPackageStateAsString(AnInstance: TObject; ADescription: TcsStreamDescription; const AValue: string): boolean;
var
  State: TfprPackageState;
begin
  Assert(AnInstance is TfprPackage);
  Result:=False;
  for State := low(TfprPackageState) to high (TfprPackageState) do
    begin
    if SameText(CfprPackageStateString[State], AValue) then
      begin
      TfprPackage(AnInstance).PackageState := State;
      Result := True;
      Exit;
      end;
    end;
end;

end.

