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
  DBConnector,
  InOutputProcessor,
  baBuildCommand,
  baBuildFPCEnvironment,
  baCommand;

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
  ErrorMsg, CommandStr: String;
  ConsoleServer: TDCSConsoleServer;
  Port, SensePorts: Longint;
  TCPServerThread: TDCSTcpServer;
  ADbConnector: TDCSHandlerThread;
  GlobalSettings: TDCSGlobalSettings;
  ConfigFileStream: TFileStream;
  HTTPRestServer: TDCSHTTPRestServer;
begin
  GlobalSettings := TDCSGlobalSettings.GetInstance;

  GlobalSettings.AddSetting('help','','','help','h', dcsPNoParameter);
  GlobalSettings.AddSetting('port','','','port','p', dcsPHasParameter);
  GlobalSettings.AddSetting('tcp','','','tcp','t', dcsPNoParameter);
  GlobalSettings.AddSetting('autoport','','','autoport','a', dcsPOptionalParameter, '5');
  GlobalSettings.SetSettingAsString('autoport', '1');

  GlobalSettings.AddSetting('dbname','','','dbname',#0, dcsPHasParameter);
  GlobalSettings.AddSetting('dbuser','','','dbuser',#0, dcsPHasParameter);
  GlobalSettings.AddSetting('dbpassword','','','dbpassword',#0, dcsPHasParameter);
  GlobalSettings.AddSetting('dbstorage','','','dbstorage','d', dcsPNoParameter);
  GlobalSettings.SetSettingAsString('dbname', 'localhost:fppkg');
  GlobalSettings.SetSettingAsString('dbuser', 'sysdba');
  GlobalSettings.SetSettingAsString('dbpassword', 'masterkey');

  GlobalSettings.AddSettingTemplate('TestEnv-', 'FPCSourcePath', 'TestEnvFPCSourcePath-', '');
  GlobalSettings.AddSettingTemplate('TestEnv-', 'PristineEnvironmentPath', 'TestEnvPristineEnvironmentPath-', '');
  GlobalSettings.AddSettingTemplate('TestEnv-', 'StartCompiler', 'TestEnvStartCompiler-', '');
  GlobalSettings.AddSettingTemplate('TestEnv-', 'BuildPath', 'TestEnvBuildPath-', '');

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

  ConfigFileStream := TFileStream.Create(ChangeFileExt(ParamStr(0), '.ini'), fmOpenRead);
  try
    GlobalSettings.LoadSettingsFromIniStream(ConfigFileStream);
  finally
    ConfigFileStream.Free;;
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

  TDCSInOutputProcessorFactory.RegisterCommandClass('json', TJSONInOutputProcessor);

  FDistributor := TDCSDistributor.Create;
  try
    FRepositoryTestThread := TDCSHandlerThread.Create(FDistributor, TDCSCustomController);
    try
      FDistributor.AddHandlerThread(FRepositoryTestThread);
    except
      FRepositoryTestThread.Free;
      raise;
    end;

    if GlobalSettings.GetSettingAsBoolean('dbstorage') then
      begin
      ADbConnector := TDCSHandlerThread.Create(FDistributor, TDBController);
      end
    else
      ADbConnector := nil;

    if GlobalSettings.GetSettingAsBoolean('tcp') then
      TCPServerThread := TDCSTcpServer.Create(FDistributor, Port, SensePorts)
    else
      TCPServerThread := nil;

    HTTPRestServer := TDCSHTTPRestServer.create(FDistributor, 8080);
    HTTPRestServer.Flags := HTTPRestServer.Flags + [dcsRestServerFlagAllowDifferentOutputFormat, dcsRestServerFlagAllowDifferentChunkedSetting];
    if GlobalSettings.GetSettingAsString('AllowCorsOrigin') <> '' then
      HTTPRestServer.AddCorsOrigin(GlobalSettings.GetSettingAsString('AllowCorsOrigin'), 'POST, GET', '', True);

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
      if Assigned(ADbConnector) then
        begin
        ADbConnector.Terminate;
        end;

      ConsoleServer.Terminate;
      ConsoleServer.WaitFor;
      ConsoleServer.Free;

      if Assigned(TCPServerThread) then
        begin
        TCPServerThread.WaitFor;
        TCPServerThread.Free;
        end;

      if Assigned(ADbConnector) then
        begin
        ADbConnector.WaitFor;
        ADbConnector.Free;
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
  Application.Title:='Fppkg build server';
  Application.Run;
  Application.Free;
end.

