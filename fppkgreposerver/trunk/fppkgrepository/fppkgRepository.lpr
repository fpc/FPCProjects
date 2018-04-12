program fppkgRepository;

{$mode objfpc}{$H+}

uses
  sysutils,
  classes,
  fphttpapp,
  dcsGlobalSettings,
  fprErrorHandling,
  repPackageWebmodule,
  repPackage,
  reprepositorywebmodule;

var
  GlobalSettings: TDCSGlobalSettings;
  ConfigFileStream: TFileStream;

begin
  GlobalSettings := TDCSGlobalSettings.GetInstance;

  GlobalSettings.AddSetting('packagemanagerurl','Connections','PackageManagerURL','',#0, dcsPHasParameter);
  GlobalSettings.AddSetting('repositoryurl','Connections','RepositoryURL','',#0, dcsPHasParameter);
  GlobalSettings.AddSetting('BuildAgentURL','Connections','BuildAgentURL','',#0, dcsPHasParameter);
  GlobalSettings.AddSetting('OpenIDProviderURL', 'OIDC', 'OpenIDProviderURL', '', #0, dcsPHasParameter);
  GlobalSettings.AddSetting('AllowCorsOrigin', 'HTTP', 'AllowCorsOrigin', '', #0, dcsPHasParameter);

  GlobalSettings.AddSettingTemplate('Repository-', 'FPCVersion', 'FPCVersion-', '');
  GlobalSettings.AddSettingTemplate('Repository-', 'MasterRepository', 'MasterRepository-', '');
  GlobalSettings.AddSettingTemplate('Repository-', 'Name', 'Name-', '');
  GlobalSettings.AddSettingTemplate('Repository-', 'NeedAdminRights', 'NeedAdminRights-', '');
  GlobalSettings.AddSettingTemplate('Repository-', 'Path', 'Path-', '');
  GlobalSettings.AddSettingTemplate('Repository-', 'BaseURL', 'BaseURL-', '');
  GlobalSettings.AddSettingTemplate('Repository-', 'Contact', 'Contact-', '');
  GlobalSettings.AddSettingTemplate('Repository-', 'StorageFile', 'StorageFile-', '');

  ConfigFileStream := TFileStream.Create(ChangeFileExt(ParamStr(0), '.ini'), fmOpenRead);
  try
    GlobalSettings.LoadSettingsFromIniStream(ConfigFileStream);
  finally
    ConfigFileStream.Free;;
  end;

  TrepFPCVersionList.Instance.InitFromSettings;

  Application.OnShowRequestException := @fprOnShowRequestException;

  Application.Port:=8282;
  Application.Initialize;
  Application.Run;
end.

