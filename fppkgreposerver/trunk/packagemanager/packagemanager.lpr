program packagemanager;

{$mode objfpc}{$H+}

uses
  classes,
  sysutils,
  fphttpapp,
  dcsGlobalSettings,
  pmPackageWebModule,
  pmPackage,
  pmErrorHandling, pmPackageJSonStreaming;

var
  ConfigFileStream: TFileStream;
  GlobalSettings: TDCSGlobalSettings;
begin
  GlobalSettings := TDCSGlobalSettings.GetInstance;
  GlobalSettings.AddSetting('OpenIDProviderURL', 'OIDC', 'OpenIDProviderURL', '', #0, dcsPHasParameter);


  ConfigFileStream := TFileStream.Create(ChangeFileExt(ParamStr(0), '.ini'), fmOpenRead);
  try
    GlobalSettings.LoadSettingsFromIniStream(ConfigFileStream);
  finally
    ConfigFileStream.Free;;
  end;


  Application.Title:='httpproject1';
  Application.Port:=8088;
  Application.OnShowRequestException := @pmOnShowRequestException;
  Application.Initialize;
  Application.Run;
end.

