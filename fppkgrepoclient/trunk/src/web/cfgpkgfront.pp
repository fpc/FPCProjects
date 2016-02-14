unit cfgpkgfront;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, inifiles, dcserverclient;

Const
  DefPackageMaxSize = 2048; // In Kb, so 2 MB.

Type

  { TSettings }

  TSettings = Class(TComponent)
  private
    FBaseURL: String;
    FCommandTimeOut: Integer;
    FFPPKgServerHost: String;
    FFPPKgServerPort: Word;
    FLocalPackageDir: String;
    FLocalURL: String;
    FMantisDB: String;
    FMantisDBHost: String;
    FMantisDBPassword: String;
    FMantisDBUser: String;
    FMantisURL: String;
    FMantisUseSOAP: Boolean;
    FMasterPassword: String;
    FMasterUser: String;
    FPackageMaxSize: Integer;
    FTemplateDir: String;
    FUploadDir: String;
  Protected
    Class var Globals : TSettings;
  Public
    Class Function Global : TSettings;
    Procedure LoadFromFile(Const AFileName : String);
    Procedure SaveToFile(Const AFileName : String);
    Procedure LoadFromIni(AIni : TCustomIniFile); virtual;
    Procedure SaveToIni(AIni : TCustomIniFile); virtual;
    Procedure CheckConfig;
  Published
    Property BaseURL : String Read FBaseURL  Write FBaseURL;
    Property PackageDir : String Read FLocalPackageDir Write FLocalPackageDir;
    Property PackageURL : String Read FLocalURL Write FLocalURL;
    Property PackageMaxSize : Integer Read FPackageMaxSize Write FPackageMaxSize;
    Property CommandTimeOut : Integer Read FCommandTimeOut Write FCommandTimeOut;
    Property TemplateDir : String Read FTemplateDir Write FTemplateDir;
    Property UploadDir : String Read FUploadDir Write FUploadDir;
    Property FPPKgServerHost : String Read FFPPKgServerHost Write FFPPKgServerHost;
    Property FPPKgServerPort : Word Read FFPPKgServerPort Write FFPPKgServerPort;
    Property MantisDB : String Read FMantisDB Write FMantisDB;
    Property MantisDBUser : String Read FMantisDBUser Write FMantisDBUser;
    Property MantisDBPassword : String Read FMantisDBPassword Write FMantisDBPassword;
    Property MantisDBHost : String Read FMantisDBHost Write FMantisDBHost;
    Property MantisURL : String Read FMantisURL Write FMantisURL;
    Property MantisUseSOAP : Boolean Read FMantisUseSOAP Write FMantisUseSOAP;
    Property MasterUser  : String Read FMasterUser Write FMasterUser;
    Property MasterPassword  : String Read FMasterPassword Write FMasterPassword;
  end;


implementation

Const
  // Local section
  SLocal            = 'Local';
  KeyBaseURL        = 'BaseURL';
  KeyPackageDir     = 'PackageDir';
  KeyUploadDir      = 'UploadDir';
  KeyTemplateDir    = 'TemplateDir';
  KeyPackageURL     = 'PackageURL';
  KeyMasterUser     = 'MasterUser';
  KeyMasterPassword = 'MasterPassword';
  KeyPackageMaxSize = 'PackageMaxSize';

  SMantis       = 'Mantis';
  KeyDB         = 'DB';
  KeyDBHost     = 'DBHost';
  KeyDBUser     = 'DBUser';
  KeyDBPassword = 'DBPassword';
  KeySoapURL    = 'SOAPURL';
  KeyUseSOAP    = 'UseSOAP';

  // Server section
  SServer       = 'Server';
  KeyHost       = 'Host';
  KeyPort       = 'Port';
  KeyCommandTimeout = 'CommandTimeout';


{ TSettings }

Class Function TSettings.Global: TSettings;
begin
  If Globals=Nil then
    Globals:=TSettings.Create(Nil);
  Result:=Globals;
end;

Procedure TSettings.LoadFromFile(Const AFileName: String);

Var
  M : TMemIniFile;

begin
  M:=TMemIniFile.Create(AFileName);
  try
    LoadFromIni(M);
  finally
    M.Free;
  end;
end;

Procedure TSettings.SaveToFile(Const AFileName: String);

Var
  M : TMemIniFile;

begin
  M:=TMemIniFile.Create(AFileName);
  try
    SaveToIni(M);
    M.UpdateFile;
  finally
    M.Free;
  end;
end;

Procedure TSettings.LoadFromIni(AIni: TCustomIniFile);
begin
  With AIni do
    begin
    // Local
    FBaseURL:=ReadString(SLocal,KeyBaseURL,'');
    FLocalPackageDir:=ReadString(SLocal,KeyPackageDir,'');
    FLocalURL:=ReadString(SLocal,KeyPackageURL,'');
    FUploadDir:=ReadString(SLocal,KeyUploadDir,'');
    FMasterPassword:=ReadString(SLocal,KeyMasterPassword,'');
    FMasterUser:=ReadString(SLocal,KeyMasterUser,'');
    FPackageMaxSize:=ReadInteger(SLocal,KeyPackageMaxSize,DefPackageMaxSize);
    FTemplateDir:=ReadString(SLocal,KeyTemplateDir,'');
    // Server
    FFPPKgServerHost:=ReadString(SServer,KeyHost,'');
    FFPPKgServerPort:=ReadInteger(SServer,KeyPort,0);
    CommandTimeout:=ReadInteger(SServer,KeyCommandTimeout,0);
    // Mantis
    FMantisDB:=ReadString(SMantis,KeyDB,'');
    FMantisDBUser:=ReadString(SMantis,KeyDBUser,'');
    FMantisDBPassword:=ReadString(SMantis,KeyDBPassword,'');
    FMantisDBHost:=ReadString(SMantis,KeyDBHost,'');
    FMantisURL:=ReadString(SMantis,KeySOAPURL,'');
    FMantisUseSOAP:=ReadBool(SMantis,KeyUseSoap,True);
    end;
end;

Procedure TSettings.SaveToIni(AIni: TCustomIniFile);
begin
  With AIni do
    begin
    // Local
    WriteString(SLocal,KeyBaseURL,FBaseURL);
    WriteString(SLocal,KeyPackageDir,FLocalPackageDir);
    WriteString(SLocal,KeyPackageURL,FLocalURL);
    WriteString(SLocal,KeyUploadDir,FUploadDir);
    WriteString(SLocal,KeyTemplateDir,FTemplateDir);
    WriteString(SLocal,KeyMasterPassword,FMasterPassword);
    WriteString(SLocal,KeyMasterUser,FMasterUser);
    WriteInteger(SLocal,KeyPackageMaxSize,FPackageMaxSize);
    // Server
    WriteString(SServer,KeyHost,FFPPKgServerHost);
    WriteInteger(SServer,KeyPort,FFPPKgServerPort);
    WriteInteger(SServer,KeyCommandTimeout,CommandTimeout);
    // Mantis
    WriteString(SMantis,KeyDB,FMantisDB);
    WriteString(SMantis,KeyDBUser,FMantisDBUser);
    WriteString(SMantis,KeyDBPassword,FMantisDBPassword);
    WriteString(SMantis,KeyDBHost,FMantisDBHost);
    WriteString(SMantis,KeySOAPURL,FMantisURL);
    WriteBool(SMantis,KeyUseSoap,FMantisUseSOAP);
    end;
end;

Procedure TSettings.CheckConfig;

Var
  S : String;

  Procedure AddError(Name,Value : String) ;

  begin
    if Value='' then
      begin
      if (S<>'') then
        S:=S+sLineBreak;
      S:=S+Name+' is missing';
      end;
  end;

begin
  S:='';
//  AddError('BaseURL',FBaseURL);
  AddError('ServerHost',FPPKgServerHost);
  AddError('LocalPackageDir',PackageDir);
  AddError('LocalPackageURL',PackageURL);
  if Not FMantisUseSOAP then
    begin
    AddError('MantisDB',FMantisDB);
    AddError('MantisDBPassword',MantisDBPassword);
    AddError('MantisDBUser',MantisDBUser);
    end;
  if (S<>'') then
    Raise Exception.Create('Wrong configuration : '+S);
  PackageDir:=IncludeTrailingPathDelimiter(PackageDir);
  if (TemplateDir<>'') then
    TemplateDir:=IncludeTrailingPathDelimiter(TemplateDir);
  if FFPPKgServerPort=0 then
    FFPPKgServerPort:=DefServerPort;
end;

end.

