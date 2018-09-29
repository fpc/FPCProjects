program buildagent;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$if (defined(unix) and not defined(android)) or defined(windows)}
  pkgwget,
  pkgfphttp,
  {$endif}
  Classes,
  SysUtils,
  CustApp,
  DCSHandler,
  dcsConsoleServer,
  DCSTCPServer,
  dcsGlobalSettings,
  dcsInOutputProcessor,
  DCSHTTPRestServer,
  baBuildCommand,
  baBuildFPCEnvironment,
  baCommand,
  baRegisterWithBuildManager;

type

  { TFppkgRepoServer }

  TFppkgRepoServer = class(TCustomApplication)
  private
    FDistributor: TDCSDistributor;
    FRepositoryTestThread: TDCSHandlerThread;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TFppkgRepoServer }

procedure TFppkgRepoServer.DoRun;
var
  ErrorMsg, CommandStr, configfilename: String;
  ConsoleServer: TDCSConsoleServer;
  Port, SensePorts: Longint;
  TCPServerThread: TDCSTcpServer;
  GlobalSettings: TDCSGlobalSettings;
  ConfigFileStream: TFileStream;
  HTTPRestServer: TDCSHTTPRestServer;
begin
  GlobalSettings := TDCSGlobalSettings.GetInstance;

  GlobalSettings.AddSetting('help','','','help','h', dcsPNoParameter);
  GlobalSettings.AddSetting('port','','','port','p', dcsPHasParameter);
  GlobalSettings.AddSetting('tcp','','','tcp','t', dcsPNoParameter);
  GlobalSettings.AddSetting('autoport','','','autoport','a', dcsPOptionalParameter, '5');
  GlobalSettings.AddSetting('environment','','','environment','e', dcsPNoParameter);
  GlobalSettings.SetSettingAsString('autoport', '1');

  GlobalSettings.AddSetting('dbname','','','dbname',#0, dcsPHasParameter);
  GlobalSettings.AddSetting('dbuser','','','dbuser',#0, dcsPHasParameter);
  GlobalSettings.AddSetting('dbpassword','','','dbpassword',#0, dcsPHasParameter);
  GlobalSettings.SetSettingAsString('dbname', 'localhost:fppkg');
  GlobalSettings.SetSettingAsString('dbuser', 'sysdba');
  GlobalSettings.SetSettingAsString('dbpassword', 'masterkey');

  GlobalSettings.AddSetting('buildmanagerurl', 'Connections', 'BuildManagerURL', '', #0, dcsPHasParameter);
  GlobalSettings.AddSetting('AgentName', 'Agent', 'Name', '', #0, dcsPHasParameter);
  GlobalSettings.AddSetting('AgentURL', 'Agent', 'URL', '', #0, dcsPHasParameter);
  GlobalSettings.AddSetting('AgentFPCVersion', 'Agent', 'FPCVersion', '', #0, dcsPHasParameter);
  GlobalSettings.AddSetting('HTTPPort','HTTP','Port','HTTPPort', #0, dcsPHasParameter);

  GlobalSettings.AddSettingTemplate('TestEnv_', 'FPCSourcePath', 'TestEnvFPCSourcePath-', '');
  GlobalSettings.AddSettingTemplate('TestEnv_', 'FPCSVNUrl', 'TestEnvFPCSVNUrl-', '');
  GlobalSettings.AddSettingTemplate('TestEnv_', 'PristineEnvironmentPath', 'TestEnvPristineEnvironmentPath-', '');
  GlobalSettings.AddSettingTemplate('TestEnv_', 'StartCompiler', 'TestEnvStartCompiler-', '');
  GlobalSettings.AddSettingTemplate('TestEnv_', 'BuildPath', 'TestEnvBuildPath-', '');
  GlobalSettings.AddSettingTemplate('TestEnv_', 'AdditionalPackages', 'TestEnvAdditionalPackages-', '');
  GlobalSettings.AddSettingTemplate('TestEnv_', 'AbsoluteFilenamesBug', 'TestEnvAbsoluteFilenamesBug-', '');

  GlobalSettings.AddSettingTemplate('TestEnv_', 'FpcCfgTemplate', 'TestEnvFpcCfgTemplate-', '');
  GlobalSettings.AddSettingTemplate('TestEnv_', 'FppkgCfgTemplate', 'TestEnvFppkgCfgTemplate-', '');
  GlobalSettings.AddSettingTemplate('TestEnv_', 'FppkgDefaultTemplate', 'TestEnvFppkgDefaultTemplate-', '');

  GlobalSettings.AddSetting('BuildFilesLocation', 'BuildFiles', 'Location', '', #0, dcsPHasParameter, '');
  GlobalSettings.AddSetting('BuildFilesURL', 'BuildFiles', 'URL', '', #0, dcsPHasParameter, '');

  GlobalSettings.AddSetting('OpenIDProviderURL', 'OIDC', 'OpenIDProviderURL', '', #0, dcsPHasParameter);
  GlobalSettings.AddSetting('AllowCorsOrigin', 'OIDC', 'AllowCorsOrigin', '', #0, dcsPHasParameter);
  // quick check parameters
  ErrorMsg := GlobalSettings.CheckProgramParameters(Self);
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  GlobalSettings.EvaluateProgramParameters(Self);
  if GlobalSettings.GetSettingAsBoolean('help') then
    begin
    WriteHelp;
    Terminate;
    Exit;
    end;

  if GlobalSettings.GetSettingAsBoolean('environment') then
  begin
    GlobalSettings.LoadSettingsFromEnvironment();
  end
  else
  begin
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
  end;

  CommandStr := GlobalSettings.GetSettingAsString('port');
  if CommandStr<>'' then
    begin
    if not TryStrToInt(CommandStr, Port) then
      begin
      writeln('Invalid port number '''+CommandStr+'''');
      Terminate;
      Exit;
      end;
    end
  else
    Port := 9250;

  CommandStr := GlobalSettings.GetSettingAsString('autoport');
  if not TryStrToInt(CommandStr, SensePorts) then
    begin
    writeln('Autoport should be an integer number. Invalid autoport value '''+CommandStr+'''');
    Terminate;
    Exit;
    end;

  FDistributor := TDCSDistributor.Create;
  try
    FRepositoryTestThread := TDCSHandlerThread.Create(FDistributor, TDCSCustomController);
    try
      FDistributor.AddHandlerThread(FRepositoryTestThread);
    except
      FRepositoryTestThread.Free;
      raise;
    end;

    if GlobalSettings.GetSettingAsBoolean('tcp') then
      TCPServerThread := TDCSTcpServer.Create(FDistributor, Port, SensePorts)
    else
      TCPServerThread := nil;

    CommandStr := GlobalSettings.GetSettingAsString('HTTPPort');
    if CommandStr<>'' then
      begin
      if not TryStrToInt(CommandStr, Port) then
        begin
        writeln('Invalid port number '''+CommandStr+'''');
        Terminate;
        Exit;
        end;
      end
    else
      Port := 8080;

    HTTPRestServer := TDCSHTTPRestServer.create(FDistributor, Port);
    HTTPRestServer.Flags := HTTPRestServer.Flags + [dcsRestServerFlagAllowDifferentOutputFormat, dcsRestServerFlagAllowDifferentChunkedSetting];
    if GlobalSettings.GetSettingAsString('AllowCorsOrigin') <> '' then
      HTTPRestServer.AddCorsOrigin(GlobalSettings.GetSettingAsString('AllowCorsOrigin'), 'POST, GET', '', True);

    CheckSynchronize(100);

    FDistributor.QueueCommand(TbaRegisterWithBuildManagerCommand.Create(-1, null, FDistributor));

    ConsoleServer := TDCSConsoleServer.Create(FDistributor);
    try
      while not Terminated do
       begin
       try
         CheckSynchronize(100);
       except
         on e: exception do
           writeln(StdErr, 'Exception: '+e.Message);
       end;
       end;

    finally
      if Assigned(TCPServerThread) then
        begin
        TCPServerThread.Terminate;
        end;
      if Assigned(HTTPRestServer) then
        begin
        HTTPRestServer.ForceTerminate;
        end;

      ConsoleServer.Terminate;
      ConsoleServer.WaitFor;
      ConsoleServer.Free;

      if Assigned(TCPServerThread) then
        begin
        TCPServerThread.WaitFor;
        TCPServerThread.Free;
        end;

    end;

  finally
    FDistributor.Free;
  end;

  // stop program loop
  Terminate;
end;

constructor TFppkgRepoServer.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  CustomApplication := Self;
  StopOnException:=True;
end;

destructor TFppkgRepoServer.Destroy;
begin
  inherited Destroy;
end;

procedure TFppkgRepoServer.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' --help --tcp --dbstorage --port <port> --autoport <count> --dbuser <dbuser> --dbpassword <dbpassword> --dbname <dbname>');
end;

var
  Application: TFppkgRepoServer;
begin
  Application:=TFppkgRepoServer.Create(nil);
  application.title := 'Fppkg build agent';
  Application.Run;
  Application.Free;
end.

