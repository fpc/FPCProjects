program identityserver;

{$mode objfpc}{$H+}

uses
  cthreads,
  classes,
  sysutils,
  fphttp,
  httproute,
  fphttpapp,
  fpwebfile,
  idIdentityWebmodule,
  cnocOpenIDConnectProvider,
  dcsGlobalSettings,
  fprErrorHandling;

var
  ConfigFileStream: TFileStream;
  ConfigFilename: string;
  GlobalSettings: TDCSGlobalSettings;
begin
  GlobalSettings := TDCSGlobalSettings.GetInstance;
  GlobalSettings.AddSetting('AllowCorsOrigin', 'HTTP', 'AllowCorsOrigin', '', #0, dcsPHasParameter);
  GlobalSettings.AddSetting('wwwroot', 'HTTP', 'wwwroot', '', #0, dcsPHasParameter);
  GlobalSettings.AddSetting('BaseURL', 'HTTP', 'BaseURL', '', #0, dcsPHasParameter);

  if Application.HasOption('e') then
  begin
    GlobalSettings.LoadSettingsFromEnvironment();
  end;

  ConfigFilename := ChangeFileExt(ParamStr(0), '.ini');
  if FileExists(ConfigFilename) then
  begin
    ConfigFileStream := TFileStream.Create(ConfigFilename, fmOpenRead);
    try
      GlobalSettings.LoadSettingsFromIniStream(ConfigFileStream);
    finally
      ConfigFileStream.Free;
    end;
  end;

  MimeTypesFile := '/etc/mime.types';

  RegisterFileLocation('css',GlobalSettings.GetSettingAsString('wwwroot'));
  httprouter.RegisterRoute('/identityserver/*', ModuleFactory.FindModule('identityserver') as IRouteInterface, True);

  Application.Port:=5000;
  Application.Threaded:=True;
  Application.Initialize;
  Application.OnShowRequestException := @fprOnShowRequestException;
  Application.Run;
end.

