program category;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  sysutils,
  fphttpapp,
  httproute,
  opensslsockets,
  cnocStackJSONHandlerThread,
  dcsGlobalSettings,
  fprErrorHandling,
  fprModel,
  fprSetupLogging,
  fprStackClient,
  fprJSONFileStreaming,
  catCategoryWebHandler;

var
  GlobalSettings: TDCSGlobalSettings;
  ConfigFileName, CategoryFilename: String;

  CategoryHandler: TcatCategoryWebHandler;
  CategoryCollection: TfprPackageCategoryCollection;

begin
  GlobalSettings := TDCSGlobalSettings.GetInstance;
  GlobalSettings.AddSetting('OpenIDProviderURL', 'OIDC', 'OpenIDProviderURL', '', #0, dcsPHasParameter);
  GlobalSettings.AddSetting('AllowCorsOrigin', 'HTTP', 'AllowCorsOrigin', '', #0, dcsPHasParameter);

  GlobalSettings.AddSetting('StackHost', 'Stack', 'host', 'stackhost', 's', dcsPHasParameter);
  GlobalSettings.AddSetting('StackPort', 'Stack', 'port', 'stackport', #0, dcsPHasParameter);

  GlobalSettings.AddSetting('CategoryListFile', 'Storage', 'CategoryListFile', '', #0, dcsPHasParameter);

  AddLoggingSettings;

  ConfigFileName := ChangeFileExt(ParamStr(0), '.ini');
  if FileExists(ConfigFileName) then
    GlobalSettings.LoadSettingsFromIniFile(ConfigFileName);

  if Application.HasOption('e') then
    GlobalSettings.LoadSettingsFromEnvironment();

  GlobalSettings.CheckProgramParameters(Application);

  SetupLogging;

  Application.OnShowRequestException := @fprOnShowRequestException;

  CategoryCollection := TfprPackageCategoryCollectionSingleton.Instance;
  CategoryFilename := GlobalSettings.GetSettingAsString('CategoryListFile');
  if (CategoryFilename <> '') and FileExists(CategoryFilename) then
    LoadCollectionFromJSONFile(CategoryCollection, CategoryFilename);


  TfprStackClientSingleton.Instance.InitHandler(['Category']);
  CategoryHandler := TcatCategoryWebHandler.Create;
  try
    HTTPRouter.RegisterRoute('/category/:category', rmAll, CategoryHandler);
    TfprStackClientSingleton.Instance.Handler.AddHandler('crud', CategoryHandler);

    Application.Port:=8484;
    Application.Initialize;
    Application.Run;
    TfprStackClientSingleton.Instance.Handler.Terminate;
    TfprStackClientSingleton.Instance.Handler.WaitFor;
  finally
    CategoryHandler.Free;
  end;
end.

