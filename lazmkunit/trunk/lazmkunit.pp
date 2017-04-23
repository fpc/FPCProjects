unit lazmkunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpmkunit;

type

  { TLazPackageFile }

  TLazType = (ltFile, ltPackage, ltPackageTemplate);

  TLazPackageFile = Class(TNamedItem)
  private
    FInstallSourcePath : string;
    FLazType: TLazType;
    function GetInstallSourcePath: string;
  public
    procedure GetInstallFiles(List : TStrings); virtual;
    property InstallSourcePath : string read GetInstallSourcePath;
    property LazType: TLazType read FLazType write FLazType;
  end;

  { TLazPackageFiles }

  TLazPackageFiles = Class(TNamedCollection)
  private
    FPackage: TPackage;
    function GetPackageFileItem(Index : Integer): TLazPackageFile;
    procedure SetPackageFileItem(Index : Integer; const AValue: TLazPackageFile);
  public
    function AddLazFile(const AFiles : String) : TLazPackageFile;
    function AddLazFile(const AFiles : String; AInstallSourcePath : String) : TLazPackageFile;
    function AddLazPackageFile(const AFiles : String) : TLazPackageFile;
    function AddLazPackageFile(const AFiles : String; AInstallSourcePath : String) : TLazPackageFile;
    procedure AddLazFiles(const AFileMask: string; Recursive: boolean = False; AInstallSourcePath : String = '');
    function AddLazPackageTemplate(const AFiles : String) : TLazPackageFile;
    function AddLazPackageTemplate(const AFiles : String; AInstallSourcePath : String) : TLazPackageFile;
    property LazPackageFileItem[Index : Integer] : TLazPackageFile Read GetPackageFileItem Write SetPackageFileItem;default;
    property Package: TPackage read FPackage;
  end;

  { TLazPackage }

  TLazPackage = Class(TPackage)
  private
    FLazPackageFiles: TLazPackageFiles;
  protected
    procedure SaveUnitConfigToStringList(const AStringList: TStrings; ACPU: TCPU; AOS: TOS); override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure GetInstallSourceFiles(List: TStrings; SourceTypes : TSourceTypes; TargetTypes : TTargetTypes); override;
    procedure GetArchiveSourceFiles(List : TStrings); override;
    procedure RestorePackageVariantsAsMacro();
    property LazPackageFiles : TLazPackageFiles Read FLazPackageFiles;
  end;

  { TLazPackages }

  TLazPackages = Class(TPackages)
  private
    function GetPackage(const AName : String): TLazPackage;
    function GetPackageItem(AIndex : Integer): TLazPackage;
    procedure SetPackageItem(AIndex : Integer; const AValue: TLazPackage);
  Public
    function AddPackage(Const AName : String) : TLazPackage;
    property Packages[AName : String] : TLazPackage Read GetPackage ; Default;
    property PackageItems[AIndex : Integer] : TLazPackage Read GetPackageItem Write SetPackageItem;
  end;

  { TLazInstaller }

  TLazInstaller = class(TCustomInstaller)
  private
    FPackages: TLazPackages;
  protected
    procedure CreatePackages; override;
    procedure FreePackages; override;
    function GetPackages: TPackages; override;
    procedure AdaptPackageTemplate(APackage: TLazPackage; AFileName: string);
  public
    procedure DoRegisterLazarusPackages(Sender: TObject);
    constructor Create(AOwner: TComponent); override;
  end;

resourcestring
  SErrAlreadyInitialized  = 'Installer can only be initialized once.';
  SWarnLazarusDirNotFound = 'Lazarus directory ''%s'' does not exist. Lazarus-package registration skipped.';
  SWarnLazarusDirInvalid  = 'Lazarus directory ''%s'' does not contain a valid Lazarus installation. Lazarus-package registration skipped.';

implementation

uses
  Laz2_XMLCfg;

procedure TLazPackage.SaveUnitConfigToStringList(const AStringList: TStrings; ACPU: TCPU; AOS: TOS);
var
  i: Integer;
  L: TStrings;
begin
  inherited SaveUnitConfigToStringList(AStringList, ACPU, AOS);
  L := TStringList.Create;
  try
    for i := 0 to LazPackageFiles.Count-1 do
      begin
        if LazPackageFiles.LazPackageFileItem[i].FLazType in [ltPackage,ltPackageTemplate] then
          L.Add(IncludeTrailingPathDelimiter(LazPackageFiles.LazPackageFileItem[i].InstallSourcePath)+ChangeFileExt(ExtractFileName(LazPackageFiles.LazPackageFileItem[i].Name),'.lpk'));
      end;
    if L.Count>0 then
      AStringList.Values['LazarusPackageFiles']:=L.CommaText;
  finally
    L.Free;
  end;
  AStringList.Values[''];
end;

constructor TLazPackage.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FLazPackageFiles := TLazPackageFiles.Create(TLazPackageFile);
  FLazPackageFiles.FPackage := self;
end;

destructor TLazPackage.Destroy;
begin
  FreeAndNil(FLazPackageFiles);
  inherited Destroy;
end;

procedure TLazPackage.GetInstallSourceFiles(List: TStrings; SourceTypes : TSourceTypes; TargetTypes : TTargetTypes);
Var
  I : Integer;
begin
  inherited GetInstallSourceFiles(List, SourceTypes, TargetTypes);
  if stDoc in SourceTypes then
    For I:=0 to FLazPackageFiles.Count-1 do
      FLazPackageFiles.LazPackageFileItem[I].GetInstallFiles(List);
end;

procedure TLazPackage.GetArchiveSourceFiles(List: TStrings);
var
  i : integer;
begin
  inherited GetArchiveSourceFiles(List);
  for i := 0 to LazPackageFiles.Count-1 do
    List.Add(LazPackageFiles[i].Name);
end;

procedure TLazPackage.RestorePackageVariantsAsMacro;
var
  PackageVariants: TPackageVariants;
  i: Integer;
begin
  for i := 0 to PackageVariantsList.Count -1 do
    begin
      PackageVariants := TPackageVariants(PackageVariantsList.Items[i]);
      Dictionary.AddVariable(PackageVariants.Name,'%('+PackageVariants.Name+')');
    end;
end;

{ TLazPackageFiles }

function TLazPackageFiles.GetPackageFileItem(Index : Integer): TLazPackageFile;
begin
  Result:=TLazPackageFile(Items[Index]);
end;

procedure TLazPackageFiles.SetPackageFileItem(Index : Integer; const AValue: TLazPackageFile);
begin
  Items[Index]:=AValue;
end;

function TLazPackageFiles.AddLazFile(const AFiles: String): TLazPackageFile;
begin
  Result:=Add as TLazPackageFile;
  Result.Name:=AFiles;
end;

function TLazPackageFiles.AddLazFile(const AFiles: String; AInstallSourcePath: String): TLazPackageFile;
begin
  Result:=Add as TLazPackageFile;
  Result.Name:=AFiles;
  Result.FInstallSourcePath:=AInstallSourcePath;
end;

function TLazPackageFiles.AddLazPackageFile(const AFiles: String): TLazPackageFile;
begin
  result := AddLazFile(AFiles);
  result.FLazType:=ltPackage;
end;

function TLazPackageFiles.AddLazPackageFile(const AFiles: String; AInstallSourcePath: String): TLazPackageFile;
begin
  result := AddLazFile(AFiles, AInstallSourcePath);
  result.FLazType:=ltPackage;
end;

procedure TLazPackageFiles.AddLazFiles(const AFileMask: string; Recursive: boolean; AInstallSourcePath: String);
var
  List : TStrings;
  i: integer;
begin
  List := TStringList.Create;
  SearchFiles(AFileMask, '', Recursive, List);
  for i:= 0 to Pred(List.Count) do
    AddLazFile(List[i], AInstallSourcePath);
  List.Free;
end;

function TLazPackageFiles.AddLazPackageTemplate(const AFiles: String): TLazPackageFile;
begin
  result := AddLazFile(AFiles);
  result.FLazType:=ltPackageTemplate;
end;

function TLazPackageFiles.AddLazPackageTemplate(const AFiles: String; AInstallSourcePath: String): TLazPackageFile;
begin
  result := AddLazFile(AFiles, AInstallSourcePath);
  result.FLazType:=ltPackageTemplate;
end;

{ TLazPackageFile }

function TLazPackageFile.GetInstallSourcePath: string;
begin
  if (FInstallSourcePath<>'') and not IsRelativePath(FInstallSourcePath) then
    result := FInstallSourcePath
  else If Defaults.UnixPaths then
    result:=Defaults.Prefix+'share'+PathDelim+'fpc-'+IncludeTrailingPathDelimiter((Collection as TLazPackageFiles).Package.Name)+FInstallSourcePath
  else
    result:=Defaults.BaseInstallDir+PathDelim+'fpc-'+IncludeTrailingPathDelimiter((Collection as TLazPackageFiles).Package.Name)+FInstallSourcePath;
end;

procedure TLazPackageFile.GetInstallFiles(List: TStrings);
begin
  list.Values[name] := (IncludeTrailingPathDelimiter(InstallSourcePath)+ExtractFileName(Name))
end;

{ TLazPackages }

function TLazPackages.GetPackage(const AName: String): TLazPackage;
begin
  Result:=TLazPackage(ItemByName(AName))
end;

function TLazPackages.GetPackageItem(AIndex: Integer): TLazPackage;
begin
  Result:=TLazPackage(Items[AIndex]);
end;

procedure TLazPackages.SetPackageItem(AIndex: Integer; const AValue: TLazPackage);
begin
  Items[AIndex]:=AValue;
end;

function TLazPackages.AddPackage(const AName: String): TLazPackage;
begin
  Result:=Add as TLazPackage;
  Result.Name:=AName;
end;

{ TLazInstaller }

procedure TLazInstaller.CreatePackages;
begin
  FPackages:=TLazPackages.Create(TLazPackage);
end;

procedure TLazInstaller.FreePackages;
begin
  FreeAndNil(FPackages);
end;

function TLazInstaller.GetPackages: TPackages;
begin
  Result:=FPackages;
end;

procedure TLazInstaller.AdaptPackageTemplate(APackage: TLazPackage; AFileName: string);
var
  LazPackageConfig: TXMLConfig;
  UnitDir: string;
begin
  LazPackageConfig := TXMLConfig.Create(AFileName);
   try
    // We want to keep the package-variants (like the widgetset) as a Lazarus-macro
    APackage.RestorePackageVariantsAsMacro();
    UnitDir := GlobalDictionary.ReplaceStrings('$(unitinstalldir)'+PathDelim+APackage.GetPackageUnitInstallDir(Defaults.CPU, Defaults.OS));
    UnitDir := StringReplace(UnitDir, '%(', '$(', [rfReplaceAll]);
    LazPackageConfig.SetValue('Package/CompilerOptions/SearchPaths/UnitOutputDirectory/Value', UnitDir);
    LazPackageConfig.SetValue('Package/UsageOptions/UnitPath/Value', UnitDir);
    LazPackageConfig.Flush;
  finally
    LazPackageConfig.Free;
  end;

end;

procedure TLazInstaller.DoRegisterLazarusPackages(Sender: TObject);
Var
  LazarusDir : string;
  LazPackagerFile : Text;
  APackage: TLazPackage;
  i: integer;
  PackageFileName: string;
  LazCompiledFileName: string;
  LazGlobalLinksDir: string;

begin
  LazarusDir := ExpandFileName(GetCustomFpmakeCommandlineOptionValue('lazarusdir'));
  if LazarusDir <> '' then
    begin
    APackage := sender as TLazPackage;
    LazGlobalLinksDir:=IncludeTrailingPathDelimiter(LazarusDir)+'packager'+PathDelim+'globallinks'+PathDelim;
    for i := 0 to APackage.LazPackageFiles.Count-1 do with APackage.LazPackageFiles.LazPackageFileItem[i] do
      if LazType in [ltPackage, ltPackageTemplate] then
        begin
        if not DirectoryExists(LazarusDir) then
          log(vlWarning,Format(SWarnLazarusDirNotFound,[LazarusDir]))
        else if not DirectoryExists(LazGlobalLinksDir) then
          log(vlWarning,Format(SWarnLazarusDirInvalid,[LazarusDir]))
        else
          begin
          PackageFileName := IncludeTrailingPathDelimiter(InstallSourcePath)+ExtractFileName(Name);
          if LazType = ltPackageTemplate then
            begin
            BuildEngine.CmdRenameFile(PackageFileName,ChangeFileExt(PackageFileName,'.lpk'));
            AdaptPackageTemplate(APackage, ChangeFileExt(PackageFileName,'.lpk'));
            end;
          System.assign(LazPackagerFile,LazGlobalLinksDir+ChangeFileExt(ExtractFileName(Name),'')+'-0.lpl');
          System.Rewrite(LazPackagerFile);
          System.WriteLn(LazPackagerFile,ChangeFileExt(PackageFileName,'.lpk'));
          System.close(LazPackagerFile);

          // If the package is already compiled by Lazarus, clear the .compiled file, to force a recompile by Lazarus

          // This only works if the package UnitOutputDirectory is set to the default "lib/$(TargetCPU)-$(TargetOS)"
          // Better would be to parse the .lpk for the Compileroptions.UnitOutputDirecory and then resolve the used
          // macros. For now this is an easy hack which will work in most cases.
          LazCompiledFileName := IncludeTrailingPathDelimiter(InstallSourcePath)+'lib' + PathDelim + CurrentCPU + '-' + CurrentOS + PathDelim + ExtractFileName(Name);
          LazCompiledFileName := ChangeFileExt(LazCompiledFileName,'.compiled');
          if FileExists(LazCompiledFileName) then
            DeleteFile(LazCompiledFileName);
          end;
        end;
    end;
end;

constructor TLazInstaller.Create(AOwner: TComponent);
begin
  AddCustomFpmakeCommandlineOption('lazarusdir','Location of a Lazarus installation.');
  if assigned(Defaults) then
    Error(SErrAlreadyInitialized);
  Defaults:=TFPCDefaults.Create;
  inherited Create(AOwner);
end;

end.

