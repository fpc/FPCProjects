unit DCSTCPServer;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  ssockets,
  BaseUnix,
  math,
  sockets,
  syncobjs,
  lazCollections,
  fpjson,
  fgl,
  dcsInOutputProcessor,
  dcsHandler;

type

  { TDCSTcpServer }

  TDCSTcpConnectionThread = class;
  TThreadedQueueString = specialize TLazThreadedQueue<string>;
  TConnectionList = specialize TFPGObjectList<TDCSTcpConnectionThread>;

  TDCSTcpServer = class(TThread)
  private
    FPort: integer;
    FSensePorts: integer;
    FTCPConnection: TInetServer;
    FConnectionList: TConnectionList;
    FDistributor: TDCSDistributor;
    FInitializationFinished: PRTLEvent;
    function CreateInetServer: TInetServer;
    procedure FTCPConnectionConnect(Sender: TObject; Data: TSocketStream);
    procedure FTCPConnectionAcceptError(Sender: TObject; ASocket: Longint; E: Exception; var ErrorAction: TAcceptErrorAction);
    procedure FTCPConnectionConnectQuery(Sender: TObject; ASocket: Longint; var Allow: Boolean);
  protected
    procedure Execute; override;
  public
    procedure WaitForInitialization(out Port: integer);
    procedure StopListening;
    constructor create(ADistributor: TDCSDistributor; APort, ASensePorts: integer);
    procedure RemoveConnection(ADebugTcpConnectionThread: TDCSTcpConnectionThread);
    destructor Destroy; override;
  end;

  { TDCSTcpConnectionThread }

  TDCSTcpConnectionThread = class(tthread, IDCSListener)
  private
    FData: TSocketStream;
    FDistributor: TDCSDistributor;
    FResponseQueue: TThreadedQueueString;
    FDebugTcpServer: TDCSTcpServer;
    FListenerId: integer;
    FInOutputProcessor: TDCSCustomInOutputProcessor;
  protected
    procedure Execute; override;
    procedure SendCommand(ACommandStr: string);
    function GetListenerId: Integer;
    procedure InitListener(AListenerId: Integer);
  public
    procedure SendEvent(AnEvent: TDCSEvent);
    function GetOrigin: string;
    constructor create(ADistributor: TDCSDistributor; ADebugTcpServer: TDCSTcpServer; Data: TSocketStream);
    destructor Destroy; override;
  end;

implementation

{ TDCSTcpConnectionThread }

function StrToStr(AStr: string): string;
var i : integer;
begin
  result := '';
  for i := 1 to length(AStr) do
    if ord(AStr[i])<20 then
      result := result +'#'+inttostr(ord(AStr[i]))
    else
      result := result + Astr[i];
end;

procedure TDCSTcpConnectionThread.Execute;

  procedure WriteString(AStr: string);
  var
    timeout: integer;
    i: integer;
    written: integer;
    total: integer;
  begin
    AStr := AStr + #10;
    total := length(AStr);
    written := 0;
    timeout := 0;

    repeat
      i := FData.Write(AStr[written+1], min(total-written, 65536));

      if i < 0 then
        begin
        if FData.LastError=ESysEAGAIN then
          begin
          if timeout>100 then
            begin
            FDistributor.SendNotification(FListenerId, ntConnectionProblem, null, 'Error during write. Timeout.', '');
            Terminate;
            end;
          sleep(10);
          inc(timeout);
          end
        else
          begin
          if FData.LastError=ESysEPIPE then
            begin
            // Lost connection
            end
          else
            FDistributor.SendNotification(FListenerId, ntConnectionProblem, null, 'Error during write. Socket-error: %d', '', [FData.LastError]);
          Terminate;
          end;
        end
      else
        begin
        inc(written, i);
        timeout := 0;
        end;
    until terminated or (written >= total);
  end;

const
  InputBufferSize = 1024;
var
  s: string;
  i: integer;
  InputBuffer: array[0..InputBufferSize-1] of char;
  InputStr: string;
begin
  try
    WriteString('Welcome to FPDebug-server.');
    if not Terminated then
      WriteString('Your connection-idenfifier is '+IntToStr(FListenerId)+'.');
    if not Terminated then
      WriteString('Send "help<enter>" for more information.');
    while not terminated do
      begin
      i := FData.Read(InputBuffer[0], InputBufferSize);
      if i > 0 then
        begin
        setlength(s,i);
        move(InputBuffer[0],s[1],i);
        s := StringReplace(s,#13#10,#10,[rfReplaceAll]);
        InputStr:=InputStr+s;
        i := pos(#10, InputStr);
        while i > 0 do
          begin
          s := copy(InputStr, 1, i-1);
          delete(InputStr,1,i);
          SendCommand(S);
          i := pos(#10, InputStr);
          end;
        end
      else if i < 0 then
        begin
        if FData.LastError<> ESysEAGAIN then
          begin
          writeln('Error during write. Socket-error: '+inttostr(FData.LastError));
          Terminate;
          end;
        end
      else if i = 0 then
        begin
        // Zero-count -> Connection closed
        Terminate;
        end;

      if not terminated and (FResponseQueue.PopItem(s) = wrSignaled) then
        begin
        WriteString(s);
        end;
      end;
  except
    on E: Exception do
      begin
      FDistributor.Log(Format('Exception raised by tcp-connection (%s): %s', [GetOrigin, E.Message]), etWarning, Null);
      end;
  end;
  FDebugTcpServer.RemoveConnection(self);
end;

procedure TDCSTcpConnectionThread.SendCommand(ACommandStr: string);
var
  ACommand: TDCSThreadCommand;
begin
  ACommand := FInOutputProcessor.TextToCommand(ACommandStr);
  if assigned(ACommand) then
    FDistributor.QueueCommand(ACommand);
end;

function TDCSTcpConnectionThread.GetListenerId: Integer;
begin
  result := FListenerId;
end;

procedure TDCSTcpConnectionThread.InitListener(AListenerId: Integer);
begin
  FListenerId := AListenerId;
end;

procedure TDCSTcpConnectionThread.SendEvent(AnEvent: TDCSEvent);
var
  s: string;
begin
  if assigned(FInOutputProcessor) then
    begin
    s := FInOutputProcessor.EventToText(AnEvent);
    FResponseQueue.PushItem(s);
    end;
end;

function TDCSTcpConnectionThread.GetOrigin: string;
var
  RemoteAddress: TSockAddr;
begin
  RemoteAddress := FData.RemoteAddress;
  result := format('%d.%d.%d.%d:%d', [RemoteAddress.sin_addr.s_bytes[1], RemoteAddress.sin_addr.s_bytes[2],RemoteAddress.sin_addr.s_bytes[3], RemoteAddress.sin_addr.s_bytes[4], RemoteAddress.sin_port])
end;

constructor TDCSTcpConnectionThread.create(ADistributor: TDCSDistributor;
  ADebugTcpServer: TDCSTcpServer; Data: TSocketStream);
begin
  FData := data;

  // Set non-blocking
  fpfcntl(FData.Handle,F_SETFL,O_NONBLOCK);

  FDistributor := ADistributor;
  FDebugTcpServer := ADebugTcpServer;
  FResponseQueue:=TThreadedQueueString.create(100, INFINITE, 100);
  FListenerId := FDistributor.AddListener(self);
  FInOutputProcessor := TDCSJSonInOutputProcessor.create(FListenerId, FDistributor);
  inherited create(false);
end;

destructor TDCSTcpConnectionThread.Destroy;
begin
  FInOutputProcessor.Free;
  FDistributor.RemoveListener(self);
  FResponseQueue.Free;
  FData.Free;
  inherited Destroy;
end;

{ TDCSTcpServer }

procedure TDCSTcpServer.FTCPConnectionAcceptError(Sender: TObject;
  ASocket: Longint; E: Exception; var ErrorAction: TAcceptErrorAction);
begin
  if E is ESocketError and (ESocketError(E).Code=seAcceptFailed) and (socketerror=53) {ECONNABORTED} then
    begin
    // The socket has stopped listening. The TCP-server is shutting down...
    ErrorAction:=aeaStop;
    end
  else
    writeln('ErrorAction a: '+e.ClassName + ' -- ',ErrorAction, '::',socketerror);
end;

procedure TDCSTcpServer.FTCPConnectionConnectQuery(Sender: TObject;
  ASocket: Longint; var Allow: Boolean);
begin
  Allow:=true;
end;

function TDCSTcpServer.CreateInetServer: TInetServer;
var
  Conn: boolean;
  InetServer: TInetServer;
  FFirstError: string;
  i: Integer;
begin
  result := nil;
  for i := 0 to FSensePorts-1 do
    begin
    conn := false;
    InetServer := TInetServer.Create(FPort+i);
    try
      InetServer.Listen;
      Conn:=true;
      Break;
    except
      on E: Exception do
        begin
        InetServer.Free;
        if (E is ESocketError) and (ESocketError(E).Code=seBindFailed) then
          begin
          // Ignore, try next port
          if FFirstError='' then
            FFirstError:=e.Message;
          end
        else
          Raise;
        end;
    end;
    end;
  if conn then
    begin
    result := InetServer;
    FPort:=result.Port;
    FDistributor.SendNotification(-1, ntListenerMessage, null, 'Listening for incoming TCP-connections on port %d', '', [FPort])
    end
  else
    begin
    FPort:=-1;
    FDistributor.SendNotification(-1, ntConnectionProblem, null, 'Failed to start listening for incoming TCP-connections: %s', '', [FFirstError])
    end;
end;

procedure TDCSTcpServer.FTCPConnectionConnect(Sender: TObject; Data: TSocketStream);
var
  AConnectionThread: TDCSTcpConnectionThread;
begin
  // Without this, on Linux at least, Data.Write can generate a SIGPIPE exception
  // when the other side disconnects unexpectedly. We do not want this and use the
  // return value to detect the lost connection.
  Data.WriteFlags := $4000; // MSG_NOSIGNAL: do not generate SIGPIPE on connection end
  AConnectionThread:=TDCSTcpConnectionThread.create(FDistributor, Self, data);
  AConnectionThread.FreeOnTerminate:=true;
  FConnectionList.Add(AConnectionThread);
end;

procedure TDCSTcpServer.Execute;
var
  AConnection: TInetServer;
begin
  try
    FTCPConnection := CreateInetServer;
    RTLeventSetEvent(FInitializationFinished);
    if assigned(FTCPConnection) then
      begin
      try
        FTCPConnection.OnConnect:=@FTCPConnectionConnect;
        FTCPConnection.OnConnectQuery:=@FTCPConnectionConnectQuery;
        FTCPConnection.OnAcceptError:=@FTCPConnectionAcceptError;
        FTCPConnection.StartAccepting;
      finally
        AConnection:=FTCPConnection;
        FTCPConnection := nil;
        AConnection.Free;
      end;
      end
  Except
    on E: Exception do
      begin
      WriteLn('Exception: '+e.Message);
      end;
  end;
end;

procedure TDCSTcpServer.WaitForInitialization(out Port: integer);
begin
  RTLeventWaitFor(FInitializationFinished);
  Port := FPort;
end;

procedure TDCSTcpServer.StopListening;
begin
  Terminate;
  if assigned(FTCPConnection) then
    FTCPConnection.StopAccepting(true);
end;

constructor TDCSTcpServer.create(ADistributor: TDCSDistributor; APort, ASensePorts: integer);
begin
  FPort:=APort;
  if ASensePorts<1 then
    ASensePorts:=1;
  FSensePorts:=ASensePorts;
  FDistributor:=ADistributor;
  FConnectionList:=TConnectionList.Create(false);
  FInitializationFinished:=RTLEventCreate;
  inherited Create(false);
end;

procedure TDCSTcpServer.RemoveConnection(ADebugTcpConnectionThread: TDCSTcpConnectionThread);
begin
  FConnectionList.Remove(ADebugTcpConnectionThread);
end;

destructor TDCSTcpServer.Destroy;
var
  i: integer;
begin
  RTLeventdestroy(FInitializationFinished);
  for i := 0 to FConnectionList.Count-1 do
    FConnectionList[i].Terminate;
  for i := 0 to FConnectionList.Count-1 do
    FConnectionList[i].WaitFor;
  if FConnectionList.Count<>0 then
    raise exception.create('Not all connections are cleared.');
  FConnectionList.Free;
  inherited Destroy;
end;

end.

