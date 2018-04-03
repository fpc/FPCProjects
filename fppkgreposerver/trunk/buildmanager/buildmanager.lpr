program buildmanager;

{$mode objfpc}{$H+}

uses
  cthreads,
  classes,
  sysutils,
  fphttpapp,
  dcsGlobalSettings,
  fprErrorHandling,
  bmbuildagentwebmodule,
  bmBuildTaskWebmodule,
  bmHandleIdleEvents;

var
  ConfigFileStream: TFileStream;
  GlobalSettings: TDCSGlobalSettings;
  IdleHandler: TbmIdleEventsHander;
begin
  Randomize;

  GlobalSettings := TDCSGlobalSettings.GetInstance;
  GlobalSettings.AddSetting('OpenIDProviderURL', 'OIDC', 'OpenIDProviderURL', '', #0, dcsPHasParameter);
  GlobalSettings.AddSetting('AllowCorsOrigin', 'HTTP', 'AllowCorsOrigin', '', #0, dcsPHasParameter);
  GlobalSettings.AddSetting('packagemanagerurl','Connections','PackageManagerURL','',#0, dcsPHasParameter);
  GlobalSettings.AddSetting('repositoryurl','Connections','RepositoryURL','',#0, dcsPHasParameter);

  ConfigFileStream := TFileStream.Create(ChangeFileExt(ParamStr(0), '.ini'), fmOpenRead);
  try
    GlobalSettings.LoadSettingsFromIniStream(ConfigFileStream);
  finally
    ConfigFileStream.Free;;
  end;

  IdleHandler := TbmIdleEventsHander.Create;
  try

    Application.Port:=8181;
    Application.OnShowRequestException := @fprOnShowRequestException;
    Application.OnAcceptIdle := @IdleHandler.HandleOnIdleEvents;
    Application.AcceptIdleTimeout := 200;
    Application.Initialize;
    Application.Run;

  finally
    IdleHandler.Free;
  end;
end.

