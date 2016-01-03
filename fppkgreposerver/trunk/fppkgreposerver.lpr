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
  RepoTestCommand,
  RepoController,
  repoinitializecommand,
  dbconnector;

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
  ADbConnector: TDbConnectorHandlerThread;
  DBName: string;
  DBUser: string;
  DBPassword: string;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('htp:a::d', ['help','tcp','port:','autoport::','dbstorage','dbname:','dbuser:','dbpassword:']);
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  CommandStr := GetOptionValue('p','port');
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

  DBName := GetOptionValue('dbname');
  if DBName='' then
    DBName := 'localhost:fppkg';

  DBUser := GetOptionValue('dbuser');
  if DBUser='' then
    DBUser := 'sysdba';

  DBPassword := GetOptionValue('dbpassword');
  if DBPassword='' then
    DBPassword := 'masterkey';

  if HasOption('a','autoport') then
    begin
    CommandStr := GetOptionValue('a','autoport');
    if CommandStr<>'' then
      begin
      if not TryStrToInt(CommandStr, SensePorts) then
        begin
        writeln('Autoport should be an integer number. Invalid autoport value '''+CommandStr+'''');
        Terminate;
        Exit;
        end;
      end
    else
      SensePorts:=5
    end
  else
    SensePorts:=1;


  FDistributor := TDCSDistributor.Create;
  try
    FRepositoryTestThread := TDCSHandlerThread.Create(FDistributor, TRepoController);
    try
      FDistributor.AddHandlerThread(FRepositoryTestThread);
    except
      FRepositoryTestThread.Free;
      raise;
    end;

    if HasOption('d','dbstorage') then
      begin
      ADbConnector := TDbConnectorHandlerThread.Create(FDistributor,DBName,DBUser,DBPassword);
      FDistributor.AddHandlerThread(ADbConnector);
      end
    else
      ADbConnector := nil;

    if HasOption('t','tcp') then
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

