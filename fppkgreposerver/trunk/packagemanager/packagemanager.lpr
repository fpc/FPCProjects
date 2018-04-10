program packagemanager;

{$mode objfpc}{$H+}

uses
  classes,
  sysutils,
  fphttpapp,
  dcsGlobalSettings,
  fprErrorHandling,
  pmPackageWebModule,
  pmPackage,
  pmPackageJSonStreaming,
  pmFunctionsWM;

var
  ConfigFileStream: TFileStream;
  GlobalSettings: TDCSGlobalSettings;
  PackageListFile: String;
begin
  GlobalSettings := TDCSGlobalSettings.GetInstance;
  GlobalSettings.AddSetting('OpenIDProviderURL', 'OIDC', 'OpenIDProviderURL', '', #0, dcsPHasParameter);
  GlobalSettings.AddSetting('AllowCorsOrigin', 'HTTP', 'AllowCorsOrigin', '', #0, dcsPHasParameter);
  GlobalSettings.AddSetting('PackageListFile', 'Storage', 'PackageListFile', '', #0, dcsPHasParameter);

  ConfigFileStream := TFileStream.Create(ChangeFileExt(ParamStr(0), '.ini'), fmOpenRead);
  try
    GlobalSettings.LoadSettingsFromIniStream(ConfigFileStream);
  finally
    ConfigFileStream.Free;
  end;

  PackageListFile := GlobalSettings.GetSettingAsString('PackageListFile');
  if (PackageListFile <> '') and FileExists(PackageListFile) then
    TpmPackageCollection.Instance.LoadFromFile(PackageListFile);

  Application.Port:=8088;
  Application.OnShowRequestException := @fprOnShowRequestException;
  Application.Initialize;
  Application.Run;
end.

