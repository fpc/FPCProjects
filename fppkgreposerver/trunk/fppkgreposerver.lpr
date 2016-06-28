program fppkgreposerver;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$if (defined(unix) and not defined(android)) or defined(windows)}
  pkgwget,
  pkglnet,
  {$endif}
  Classes,
  SysUtils,
  CustApp,
  DCSHandler,
  dcsConsoleServer,
  DCSTCPServer,
  dcsGlobalSettings,
  dcsInOutputProcessor,
  RepoTestCommand,
  RepoController,
  repoinitializecommand,
  dbconnector,
  RepoSvnCommands,
  InOutputProcessor;

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
begin
  GlobalSettings := TDCSGlobalSettings.GetInstance;

  GlobalSettings.AddSetting('help','','','help','h', dcsPNoParameter);
  GlobalSettings.AddSetting('port','','','port','p', dcsPHasParameter);
  GlobalSettings.AddSetting('dbname','','','dbname',#0, dcsPHasParameter);
  GlobalSettings.AddSetting('dbuser','','','dbuser',#0, dcsPHasParameter);
  GlobalSettings.AddSetting('dbpassword','','','dbpassword',#0, dcsPHasParameter);
  GlobalSettings.AddSetting('dbstorage','','','dbstorage','d', dcsPNoParameter);
  GlobalSettings.AddSetting('tcp','','','tcp','t', dcsPNoParameter);
  GlobalSettings.SetSettingAsString('dbname', 'localhost:fppkg');
  GlobalSettings.SetSettingAsString('dbuser', 'sysdba');
  GlobalSettings.SetSettingAsString('dbpassword', 'masterkey');

  GlobalSettings.AddSetting('autoport','','','autoport','a', dcsPOptionalParameter, '5');
  GlobalSettings.SetSettingAsString('autoport', '1');

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
    FRepositoryTestThread := TDCSHandlerThread.Create(FDistributor, TRepoController);
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
  Application.Title:='Fppkg repository server';
  Application.Run;
  Application.Free;
end.

