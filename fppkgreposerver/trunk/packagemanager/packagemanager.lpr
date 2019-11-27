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
  csJSONRttiStreamHelper,
  csModel,
  fprErrorHandling,
  fprFPCVersion,
  fprJSONFileStreaming,
  fprSetupLogging,
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
  Serializer: TJSONRttiStreamClass;
  FS: TStringStream;
  Streamer: TpmPackageJSonStreaming;
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

  AddLoggingSettings;

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

  SetupLogging;

  TfprFPCVersionCollection.Instance.LoadFromSettings;

  PackageListFile := GlobalSettings.GetSettingAsString('PackageListFile');
  if (PackageListFile <> '') and FileExists(PackageListFile) then
    begin
    Streamer := TpmPackageJSonStreaming.Create;
    try
      FS := TStringStream.Create('');
      try
        FS.LoadFromFile(PackageListFile);
        Streamer.JSonToPackageCollection(FS.DataString, TpmPackageCollection.Instance);
      finally
        FS.Free;
      end;
    finally
      Streamer.Free;
    end;
    end;
  TfprStackClientSingleton.Instance.InitHandler(['CategoryMonitor', 'KeywordMonitor']);
  TfprStackClientSingleton.Instance.Handler.AddHandler('category_signal', TfprPackageCategoryMonitorSingleton.Instance);
  TfprStackClientSingleton.Instance.Handler.AddHandler('keyword_signal', TfprPackageKeywordMonitorSingleton.Instance);

  Application.Port:=8088;
  Application.OnShowRequestException := @fprOnShowRequestException;
  Application.Initialize;
  Application.Run;
end.

