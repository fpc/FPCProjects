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
  configfilename: string;

begin
  GlobalSettings := TDCSGlobalSettings.GetInstance;

  GlobalSettings.AddSetting('packagemanagerurl','Connections','PackageManagerURL','',#0, dcsPHasParameter);
  GlobalSettings.AddSetting('repositoryurl','Connections','RepositoryURL','',#0, dcsPHasParameter);
  GlobalSettings.AddSetting('buildmanagerurl','Connections','BuildManagerURL','',#0, dcsPHasParameter);
  GlobalSettings.AddSetting('OpenIDProviderURL', 'OIDC', 'OpenIDProviderURL', '', #0, dcsPHasParameter);
  GlobalSettings.AddSetting('AllowCorsOrigin', 'HTTP', 'AllowCorsOrigin', '', #0, dcsPHasParameter);

  GlobalSettings.AddSettingTemplate('Repository_', 'FPCVersion', 'FPCVersion-', '');
  GlobalSettings.AddSettingTemplate('Repository_', 'MasterRepository', 'MasterRepository-', '');
  GlobalSettings.AddSettingTemplate('Repository_', 'Name', 'Name-', '');
  GlobalSettings.AddSettingTemplate('Repository_', 'NeedAdminRights', 'NeedAdminRights-', '');
  GlobalSettings.AddSettingTemplate('Repository_', 'Path', 'Path-', '');
  GlobalSettings.AddSettingTemplate('Repository_', 'BaseURL', 'BaseURL-', '');
  GlobalSettings.AddSettingTemplate('Repository_', 'Contact', 'Contact-', '');
  GlobalSettings.AddSettingTemplate('Repository_', 'StorageFile', 'StorageFile-', '');

  if Application.HasOption('e') then
    begin
    GlobalSettings.LoadSettingsFromEnvironment();
    end;

  ConfigFileName := ChangeFileExt(ParamStr(0), '.ini');
  if FileExists(ConfigFileName) then
    begin
    ConfigFileStream := TFileStream.Create(ChangeFileExt(ParamStr(0), '.ini'), fmOpenRead);
    try
      GlobalSettings.LoadSettingsFromIniStream(ConfigFileStream);
    finally
      ConfigFileStream.Free;;
    end;
  end;

  TrepFPCVersionList.Instance.InitFromSettings;

  Application.OnShowRequestException := @fprOnShowRequestException;

  Application.Port:=8282;
  Application.Initialize;
  Application.Run;
end.

