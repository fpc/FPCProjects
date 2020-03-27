program repository;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  classes,
  sysutils,
  HTTPDefs,
  httproute,
  fphttpapp,
  opensslsockets,
  jsonparser,
  TLoggerUnit,
  cnocStackJSONHandlerThread,
  csModel,
  fprErrorHandling,
  fprSetupLogging,
  dcsGlobalSettings,
  prPackageWebModule,
  prFunctionsWebModule;

var
  GlobalSettings: TDCSGlobalSettings;
  ConfigFileName: String;
  GStackClient: TcnocStackJSONHandlerThread;
  PackageHandler: TprPackageWM;

begin
  TcsDescriber.GlobalDefaultExportNameStyle := tcsensLowerCase;
  TcsDescriber.GloblaDefaultImportNameStyle := tcsinsCaseInsensitive;

  GlobalSettings := TDCSGlobalSettings.GetInstance;
  GlobalSettings.AddSetting('packagemanagerurl','Connections','PackageManagerURL','',#0, dcsPHasParameter);
  GlobalSettings.AddSetting('buildmanagerurl','Connections','BuildManagerURL','',#0, dcsPHasParameter);
  GlobalSettings.AddSetting('OpenIDProviderURL', 'OIDC', 'OpenIDProviderURL', '', #0, dcsPHasParameter);
  GlobalSettings.AddSetting('AllowCorsOrigin', 'HTTP', 'AllowCorsOrigin', '', #0, dcsPHasParameter);

  GlobalSettings.AddSetting('GITRepositoriesPath', 'GIT', 'RepositoriesPath', '', #0, dcsPHasParameter);
  GlobalSettings.AddSetting('GITUserName', 'GIT', 'UserName', '', #0, dcsPHasParameter);
  GlobalSettings.AddSetting('GITEmail', 'GIT', 'Email', '', #0, dcsPHasParameter);

  GlobalSettings.AddSetting('StackHost', 'Stack', 'host', 'stackhost', 's', dcsPHasParameter);
  GlobalSettings.AddSetting('StackPort', 'Stack', 'port', 'stackport', #0, dcsPHasParameter);

  AddLoggingSettings;

  if Application.HasOption('e') then
  begin
    GlobalSettings.LoadSettingsFromEnvironment();
  end
  else
  begin
    ConfigFileName := ChangeFileExt(ParamStr(0), '.ini');
    if FileExists(ConfigFileName) then
    begin
      GlobalSettings.LoadSettingsFromIniFile(ConfigFileName);
    end;
  end;

  SetupLogging;

  if TLogger.GetInstance().GetAllAppenders().Count > 0 then
    TLogger.GetInstance('cnocOIDC').AddAppender(TLogger.GetInstance().GetAllAppenders.Items[0]);

  GStackClient := TcnocStackJSONHandlerThread.Create(GlobalSettings.GetSettingAsString('StackHost'), StrToIntDef(GlobalSettings.GetSettingAsString('StackPort'), 0), ['PMPackage']);
  try
    PackageHandler := TprPackageWM.Create();
    try
      HTTPRouter.RegisterRoute('/package/:packagename/:command/:fpcversion', rmAll, PackageHandler);
      HTTPRouter.RegisterRoute('/package/:packagename/:command', rmAll, PackageHandler);
      HTTPRouter.RegisterRoute('/package/:packagename', rmAll, PackageHandler);
      HTTPRouter.RegisterRoute('/package', rmAll, PackageHandler);
      GStackClient.AddHandler('package', PackageHandler);

      Application.Port:=8089;
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

