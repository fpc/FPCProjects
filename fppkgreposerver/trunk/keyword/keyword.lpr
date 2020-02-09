program keyword;

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
  keyKeywordWebHandler;

var
  GlobalSettings: TDCSGlobalSettings;
  ConfigFileName, KeywordFilename: String;

  KeywordHandler: TkeyKeywordWebHandler;
  KeywordCollection: TfprPackageKeywordCollection;

begin
  GlobalSettings := TDCSGlobalSettings.GetInstance;
  GlobalSettings.AddSetting('OpenIDProviderURL', 'OIDC', 'OpenIDProviderURL', '', #0, dcsPHasParameter);
  GlobalSettings.AddSetting('AllowCorsOrigin', 'HTTP', 'AllowCorsOrigin', '', #0, dcsPHasParameter);

  GlobalSettings.AddSetting('StackHost', 'Stack', 'host', 'stackhost', 's', dcsPHasParameter);
  GlobalSettings.AddSetting('StackPort', 'Stack', 'port', 'stackport', #0, dcsPHasParameter);

  GlobalSettings.AddSetting('KeywordListFile', 'Storage', 'KeywordListFile', '', #0, dcsPHasParameter);

  AddLoggingSettings;

  ConfigFileName := ChangeFileExt(ParamStr(0), '.ini');
  if FileExists(ConfigFileName) then
    GlobalSettings.LoadSettingsFromIniFile(ConfigFileName);

  if Application.HasOption('e') then
    GlobalSettings.LoadSettingsFromEnvironment();

  GlobalSettings.CheckProgramParameters(Application);

  SetupLogging;

  Application.OnShowRequestException := @fprOnShowRequestException;

  KeywordCollection := TfprPackageKeywordCollectionSingleton.Instance;
  KeywordFilename := GlobalSettings.GetSettingAsString('KeywordListFile');
  if (KeywordFilename <> '') and FileExists(KeywordFilename) then
    LoadCollectionFromJSONFile(KeywordCollection, KeywordFilename);

  TfprStackClientSingleton.Instance.InitHandler(['Keyword']);
  KeywordHandler := TkeyKeywordWebHandler.Create;
  try
    KeywordHandler.Collection := KeywordCollection;
    KeywordHandler.SaveCollectionFilename := KeywordFilename;
    KeywordHandler.StackName := 'Keyword';
    KeywordHandler.UrlPath := 'keyword';
    HTTPRouter.RegisterRoute('/keyword/:keyword', rmAll, KeywordHandler);
    TfprStackClientSingleton.Instance.Handler.AddHandler('crud', KeywordHandler);

    Application.Port:=8585;
    Application.Initialize;
    Application.Run;
    TfprStackClientSingleton.Instance.Handler.Terminate;
    TfprStackClientSingleton.Instance.Handler.WaitFor;
  finally
    KeywordHandler.Free;
  end;
end.

