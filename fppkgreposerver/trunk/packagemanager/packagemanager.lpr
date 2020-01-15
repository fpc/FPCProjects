program packagemanager;

{$mode objfpc}{$H+}

uses
  {$IFDEF Unix}
  cthreads,
  {$ENDIF}
  classes,
  sysutils,
  fphttpapp,
  httproute,
  dcsGlobalSettings,
  cnocStackJSONHandlerThread,
  csJSONRttiStreamHelper,
  csModel,
  TLoggerUnit,
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
  FS: TStringStream;
  Streamer: TpmPackageJSonStreaming;
  PackageHandler: TpmPackageWM;
  GStackClient: TcnocStackJSONHandlerThread;
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

  if TLogger.GetInstance().GetAllAppenders().Count > 0 then
    TLogger.GetInstance('cnocOIDC').AddAppender(TLogger.GetInstance().GetAllAppenders.Items[0]);

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

  GStackClient := TcnocStackJSONHandlerThread.Create(GlobalSettings.GetSettingAsString('StackHost'), StrToIntDef(GlobalSettings.GetSettingAsString('StackPort'), 0), ['Repository']);
  try
    PackageHandler := TpmPackageWM.Create();
    try
      HTTPRouter.RegisterRoute('/package/:packagename/:subobject', rmAll, PackageHandler);
      HTTPRouter.RegisterRoute('/package/:packagename', rmAll, PackageHandler);
      GStackClient.AddHandler('pmpackage', PackageHandler);

      Application.Port:=8088;
      Application.OnShowRequestException := @fprOnShowRequestException;
      Application.Initialize;
      Application.Run;

      GStackClient.Terminate;
      GStackClient.WaitFor;
    finally
      PackageHandler.Free;
    end;
  finally
    GStackClient.Free;
  end;
end.

