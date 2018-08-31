program repository;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  classes,
  sysutils,
  HTTPDefs,
  fphttpapp,
  jsonparser,
  fprErrorHandling,
  dcsGlobalSettings,
  prPackageWebModule,
  prFunctionsWebModule;

var
  GlobalSettings: TDCSGlobalSettings;
  ConfigFileStream: TFileStream;
  ConfigFileName: String;

begin
  GlobalSettings := TDCSGlobalSettings.GetInstance;
  GlobalSettings.AddSetting('packagemanagerurl','Connections','PackageManagerURL','',#0, dcsPHasParameter);
  GlobalSettings.AddSetting('buildmanagerurl','Connections','BuildManagerURL','',#0, dcsPHasParameter);
  GlobalSettings.AddSetting('OpenIDProviderURL', 'OIDC', 'OpenIDProviderURL', '', #0, dcsPHasParameter);
  GlobalSettings.AddSetting('AllowCorsOrigin', 'HTTP', 'AllowCorsOrigin', '', #0, dcsPHasParameter);

  GlobalSettings.AddSetting('GITRepositoriesPath', 'GIT', 'RepositoriesPath', '', #0, dcsPHasParameter);
  GlobalSettings.AddSetting('GITUserName', 'GIT', 'UserName', '', #0, dcsPHasParameter);
  GlobalSettings.AddSetting('GITEmail', 'GIT', 'Email', '', #0, dcsPHasParameter);

  if Application.HasOption('e') then
  begin
    GlobalSettings.LoadSettingsFromEnvironment();
  end
  else
  begin
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
  end;

  Application.OnShowRequestException := @fprOnShowRequestException;

  Application.Port:=8089;
  Application.Threaded := False;
  Application.Initialize;
  Application.Run;
end.

