program packagemanager;

{$mode objfpc}{$H+}

uses
  {$IFDEF Unix}
  cthreads,
  {$ENDIF}
  classes,
  sysutils,
  fphttpapp,
  dcsGlobalSettings,
  cnocStackJSONHandlerThread,
  fprErrorHandling,
  fprFPCVersion,
  fprJSONFileStreaming,
  fprStackClient,
  fprModel,
  pmPackageWebModule,
  pmPackage,
  pmPackageJSonStreaming,
  pmFunctionsWM,
  pmFPCVersionWebModule;

var
  ConfigFileStream: TFileStream;
  GlobalSettings: TDCSGlobalSettings;
  PackageListFile: String;
  ConfigFileName: string;
begin
  GlobalSettings := TDCSGlobalSettings.GetInstance;
  GlobalSettings.AddSetting('OpenIDProviderURL', 'OIDC', 'OpenIDProviderURL', '', #0, dcsPHasParameter);
  GlobalSettings.AddSetting('AllowCorsOrigin', 'HTTP', 'AllowCorsOrigin', '', #0, dcsPHasParameter);
  GlobalSettings.AddSetting('PackageListFile', 'Storage', 'PackageListFile', '', #0, dcsPHasParameter);

  GlobalSettings.AddSettingTemplate('FPCVersion_', 'Name', 'FPCVersionName-', '');
  GlobalSettings.AddSettingTemplate('FPCVersion_', 'IsDefault', 'FPCVersionIsDefault-', 'false');
  GlobalSettings.AddSettingTemplate('FPCVersion_', 'URLPrefix', 'FPCVersionURLPrefix-', '');

  GlobalSettings.AddSetting('StackHost', 'Stack', 'host', 'stackhost', 's', dcsPHasParameter);
  GlobalSettings.AddSetting('StackPort', 'Stack', 'port', 'stackport', #0, dcsPHasParameter);

  ConfigFileName := ChangeFileExt(ParamStr(0), '.ini');
  if FileExists(ConfigFileName) then
  begin
    ConfigFileStream := TFileStream.Create(ConfigFileName, fmOpenRead);
    try
      GlobalSettings.LoadSettingsFromIniStream(ConfigFileStream);
    finally
      ConfigFileStream.Free;
    end;
  end;

  if Application.HasOption('e') then
  begin
    GlobalSettings.LoadSettingsFromEnvironment();
  end;

  TfprFPCVersionCollection.Instance.LoadFromSettings;

  PackageListFile := GlobalSettings.GetSettingAsString('PackageListFile');
  if (PackageListFile <> '') and FileExists(PackageListFile) then
    LoadCollectionFromJSONFile(TpmPackageCollection.Instance, PackageListFile);

  TfprStackClientSingleton.Instance.InitHandler(['CategoryMonitor']);
  TfprStackClientSingleton.Instance.Handler.AddHandler('signal', TfprPackageCategoryMonitorSingleton.Instance);

  Application.Port:=8088;
  Application.OnShowRequestException := @fprOnShowRequestException;
  Application.Initialize;
  Application.Run;
end.

