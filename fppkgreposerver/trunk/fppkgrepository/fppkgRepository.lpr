program fppkgRepository;

{$mode objfpc}{$H+}

uses
  {$IFDEF Unix}
  cthreads,
  {$ENDIF}
  sysutils,
  classes,
  fphttpapp,
  fpjson,
  httproute,
  opensslsockets,
  dcsGlobalSettings,
  fprErrorHandling,
  fprSetupLogging,
  repPackageWebmodule,
  repPackage,
  Generics.Collections,
  reprepositorywebmodule,
  repFunctionsHandler,
  cnocStackJSONHandlerThread,
  cnocStackMessageTypes,
  cnocStackbinaryclient,
  cnocStackHandlerThread;

var
  GlobalSettings: TDCSGlobalSettings;
  configfilename: string;

  GStackClient: TcnocStackHandlerThread;

  RepositoryHandler: TrepRepositoryHander;
  PackageHandler: TrepPackageHander;
  FunctionsHandler: TrepFunctionsHander;

begin
  GlobalSettings := TDCSGlobalSettings.GetInstance;

  GlobalSettings.AddSetting('packagemanagerurl','Connections','PackageManagerURL','',#0, dcsPHasParameter);
  GlobalSettings.AddSetting('repositoryurl','Connections','RepositoryURL','',#0, dcsPHasParameter);
  GlobalSettings.AddSetting('buildmanagerurl','Connections','BuildManagerURL','',#0, dcsPHasParameter);
  GlobalSettings.AddSetting('OpenIDProviderURL', 'OIDC', 'OpenIDProviderURL', '', #0, dcsPHasParameter);
  GlobalSettings.AddSetting('AllowCorsOrigin', 'HTTP', 'AllowCorsOrigin', '', #0, dcsPHasParameter);

  GlobalSettings.AddSetting('StackHost', 'Stack', 'host', 'stackhost', 's', dcsPHasParameter);
  GlobalSettings.AddSetting('StackPort', 'Stack', 'port', 'stackport', #0, dcsPHasParameter);

  GlobalSettings.AddSettingTemplate('Repository_', 'FPCVersion', 'FPCVersion-', '');
  GlobalSettings.AddSettingTemplate('Repository_', 'MasterRepository', 'MasterRepository-', '');
  GlobalSettings.AddSettingTemplate('Repository_', 'Name', 'Name-', '');
  GlobalSettings.AddSettingTemplate('Repository_', 'NeedAdminRights', 'NeedAdminRights-', '');
  GlobalSettings.AddSettingTemplate('Repository_', 'Path', 'Path-', '');
  GlobalSettings.AddSettingTemplate('Repository_', 'BaseURL', 'BaseURL-', '');
  GlobalSettings.AddSettingTemplate('Repository_', 'Contact', 'Contact-', '');
  GlobalSettings.AddSettingTemplate('Repository_', 'StorageFile', 'StorageFile-', '');
  GlobalSettings.AddSettingTemplate('Repository_', 'ExportCategoryAndSupport', 'ExportCategoryAndSupport-', '');

  AddLoggingSettings;

  ConfigFileName := ChangeFileExt(ParamStr(0), '.ini');
  if FileExists(ConfigFileName) then
    GlobalSettings.LoadSettingsFromIniFile(configfilename);

  if Application.HasOption('e') then
    GlobalSettings.LoadSettingsFromEnvironment();

  GlobalSettings.CheckProgramParameters(Application);

  SetupLogging;

  TrepFPCVersionList.Instance.InitFromSettings;

  Application.OnShowRequestException := @fprOnShowRequestException;

  GStackClient := TcnocStackJSONHandlerThread.Create(GlobalSettings.GetSettingAsString('StackHost'), StrToIntDef(GlobalSettings.GetSettingAsString('StackPort'), 0), ['Repository']);
  try
    RepositoryHandler := TrepRepositoryHander.Create;
    FunctionsHandler := TrepFunctionsHander.Create;
    PackageHandler := TrepPackageHander.Create;
    try
      HTTPRouter.RegisterRoute('/repository/:fpcversion/:repository/:extra', rmAll, RepositoryHandler);
      HTTPRouter.RegisterRoute('/repository/:fpcversion/:repository', rmAll, RepositoryHandler);
      GStackClient.AddHandler('repository', RepositoryHandler);

      HTTPRouter.RegisterRoute('/package/:fpcversion/:repository/:package', PackageHandler);
      HTTPRouter.RegisterRoute('/package/*', PackageHandler);
      GStackClient.AddHandler('package', PackageHandler);

      HTTPRouter.RegisterRoute('/functions/:function', FunctionsHandler);
      GStackClient.AddHandler('functions', FunctionsHandler);

      Application.Port:=8282;
      Application.Initialize;
      Application.Run;
      GStackClient.Terminate;
      GStackClient.WaitFor;
    finally
      PackageHandler.Free;
      RepositoryHandler.Free;
      FunctionsHandler.Free;
    end;
  finally
    GStackClient.Free;
  end;
end.

