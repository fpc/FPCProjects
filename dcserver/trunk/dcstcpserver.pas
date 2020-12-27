unit DCSTCPServer;

{ Distributed Command Server

  This unit contains a simple telnet-like TCP-server which can be used to send
  thread-commands to a DCServer-distributor and to monitor all events.

  Copyright (C) 2016 Joost van der Sluis joost@cnoc.nl

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  ssockets,
  {$IFDEF UNIX}
  BaseUnix,
  pthreads,
  {$ENDIF UNIX}
  {$IFDEF Linux}
  syscall,
  {$ENDIF}
  {$IFDEF Windows}
  WinSock2,
  {$ENDIF}
  math,
  sockets,
  syncobjs,
  cnocQueue,
  fpjson,
  fgl,
  dcsInOutputProcessor,
  dcsHandler;

type

  { TDCSTcpServer }

  TDCSTcpConnectionThread = class;
  TThreadedQueueString = specialize TcnocThreadedQueue<string>;
  TConnectionList = specialize TFPGObjectList<TDCSTcpConnectionThread>;

  TDCSTcpServer = class(TThread)
  private
    FPort: integer;
    FSensePorts: integer;
    FTCPConnection: TInetServer;
    FConnectionList: TConnectionList;
    FInitializationFinished: PRTLEvent;
    function CreateInetServer: TInetServer;
    procedure FTCPConnectionConnect(Sender: TObject; Data: TSocketStream);
    procedure FTCPConnectionAcceptError(Sender: TObject; ASocket: Longint; E: Exception; var ErrorAction: TAcceptErrorAction);
    procedure FTCPConnectionConnectQuery(Sender: TObject; ASocket: Longint; var Allow: Boolean);
  protected
    FDistributor: TDCSDistributor;
    procedure Execute; override;
    function CreateTCPConnectionThread(Data: TSocketStream): TDCSTcpConnectionThread; virtual;
  public
    procedure WaitForInitialization(out Port: integer);
    procedure Stop;
    constructor create(ADistributor: TDCSDistributor; APort, ASensePorts: integer);
    procedure RemoveConnection(ADebugTcpConnectionThread: TDCSTcpConnectionThread);
    destructor Destroy; override;
  end;

  { TDCSTcpConnectionThread }

  TDCSTcpConnectionThread = class(tthread, IDCSListener)
  private
    FInOutputProcessor: TDCSCustomInOutputProcessor;
    procedure SendThreadProc();
  protected
    FData: TSocketStream;
    FDistributor: TDCSDistributor;
    FResponseQueue: TThreadedQueueString;
    FDebugTcpServer: TDCSTcpServer;
    FListenerId: integer;
    FInitialBuffer: string;
    FSendThread: TThread;
  protected
    function SendData(AString: string): Boolean; virtual;
    procedure Execute; override;
    procedure SendCommand(ACommandStr: string);
    function GetListenerId: Integer;
    procedure InitListener(AListenerId: Integer);
    function CreateInOutputProcessor: TDCSCustomInOutputProcessor; virtual;
    function StartSendThread(): TThread; virtual;
  public
    procedure SendEvent(AnEvent: TDCSEvent); virtual;
    function GetOrigin: string;
    constructor create(ADistributor: TDCSDistributor; ADebugTcpServer: TDCSTcpServer; Data: TSocketStream; InitialBuffer: string);
    destructor Destroy; override;
    procedure Stop;
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

{$IFDEF Unix}
procedure HandleSignal(signal: longint; info: psiginfo; context: psigcontext); cdecl;
begin
  // Do nothing
end;
{$ENDIF}

procedure TDCSTcpConnectionThread.SendThreadProc();
var
  Res: Boolean;
  AStr: string;
begin
  Res := True;
  while Res and (FResponseQueue.PopItem(AStr) = wrSignaled) do
    begin
    Res := SendData(AStr + #10);
    end;
end;

{$IFDEF Unix}
{$IFDEF Linux}
Function fpPSelect(N:cint;readfds,writefds,exceptfds:pfdSet;TimeOut:PTimeSpec; sigmask: BaseUnix.PSigSet):cint;
type
  TTwoSyscallParams = record
    Para6: TSysParam;
    Para7: TSysParam;
  end;
var
  Param6and7: TTwoSyscallParams;
begin
  Param6and7.Para6 := TSysParam(sigmask);
  {$ifdef CPUMIPS}
  Param6and7.Para7 := 16;
  {$else not CPUMIPS}
  Param6and7.Para7 := 8;
  {$endif not CPUMIPS}
  Result:=do_syscall(syscall_nr_pselect6,n,
                       tsysparam(readfds),tsysparam(writefds),
                       tsysparam(exceptfds),tsysparam(TimeOut),TSysParam(@Param6and7));
end;
{$ELSE}
Function fpPSelect(N:cint;readfds,writefds,exceptfds:pfdSet;TimeOut:PTimeSpec; sigmask: BaseUnix.PSigSet):cint;
var
  OldSigMask: BaseUnix.PSigSet;
begin
  Assert(TimeOut=nil, 'Timeout not supported by this implementation of fpPSelect');
  // This is not completely safe.
  FpSigProcMask(SIG_SETMASK, sigmask, @OldSigMask);
  i := fpSelect(N, readfds, writefds, exceptfds, nil);
  FpSigProcMask(SIG_SETMASK, @OldSigMask, Nil);
end;
{$ENDIF}
{$ENDIF UNIX}

procedure TDCSTcpConnectionThread.Execute;

const
  InputBufferSize = 1024;
var
  s: string;
  i: integer;
  InputBuffer: array[0..InputBufferSize-1] of char;
  InputStr: string;
{$IFDEF Unix}
  NewMaskSigterm: BaseUnix.TSigSet;
  OldMaskSigterm: BaseUnix.TSigSet;
  SocketFds: TFDSet;
  NewSignalHandler,
  OldSignalHandler: SigActionRec;
{$ENDIF}
begin
  InputStr := FInitialBuffer;
  try
    FResponseQueue.PushItem('Welcome to FPDebug-server.' + LineEnding);
    if not Terminated then
      FResponseQueue.PushItem('Your connection-idenfifier is ' + IntToStr(FListenerId) + '.' + LineEnding);
    if not Terminated then
      FResponseQueue.PushItem('Send "help<enter>" for more information.' + LineEnding);

    {$IFDEF Unix}
    // This blocking thead is terminated by sending a SIGTERM signal, which will
    // break the blocking fpSelect call.

    // First we have to make sure that there is no handler active that reacts
    // on the SIGTERM and does something that we do not want. (Like quitting the
    // application) We do so by setting our own handler.
    NewSignalHandler := Default(SigActionRec);
    OldSignalHandler := Default(SigActionRec);
    NewSignalHandler.sa_handler := @HandleSignal;
    FPSigaction(SIGTERM, @NewSignalHandler, @OldSignalHandler);

    // Then we have to block SIGTERM signals by default. We only want to receive
    // the SIGTERM during the blocking fpPSelect call. Or else the signal may be
    // reveived outside the blocking fpSelect and enter fpSelect thereafter waiting
    // indefinitely
    FpsigEmptySet(NewMaskSigterm);
    FpSigAddSet(NewMaskSigterm, SIGTERM);
    FpSigProcMask(SIG_BLOCK, @NewMaskSigterm, @OldMaskSigterm);
    {$ENDIF Unix}
    while not terminated do
      begin
      {$IFDEF Unix}
      // Use fpSelect before the read, so that the read-operation will not block.
      // fpSelect has as advantage that it can be stopped in a controlled way
      // by sending it a signal. So it is possible to terminate the thread.
      fpFD_ZERO(SocketFds);
      fpFD_SET(FData.Handle, SocketFds);
      FpsigEmptySet(OldMaskSigterm);
      i := fpPSelect(FData.Handle+1, @SocketFds, nil, nil, nil, @OldMaskSigterm);

      if i < 0 then
        begin
        if GetLastOSError=ESysEINTR then
          begin
          // We received an interrupt. Skip the read to check if the thread is
          // requested to terminate.
          Continue;
          end
        else
          begin
          FDistributor.Log(Format('Error during select. Error: %d', [GetLastOSError]), etWarning, Null);
          Terminate;
          Continue;
          end;
        end;
      {$ENDIF Unix}
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
          try
            SendCommand(S);
          except
            on E: Exception do
              begin
              FDistributor.SendNotification(FListenerId, ntInvalidCommand, Null, 'Exception raised while processing command: %s', s, [E.Message]);
              end;
          end;
          i := pos(#10, InputStr);
          end;
        end
      else if i < 0 then
        begin
        FDistributor.Log(Format('Error during read. Socket-error: %d', [FData.LastError]), etWarning, Null);
        Terminate;
        end
      else if i = 0 then
        begin
        // Zero-count -> Connection closed
        Terminate;
        end;

      end;
  except
    on E: Exception do
      begin
      FDistributor.Log(Format('Exception raised by tcp-connection (%s): %s', [GetOrigin, E.Message]), etWarning, Null, FListenerId);
      end;
  end;
  FDebugTcpServer.RemoveConnection(self);
  FResponseQueue.DoShutDown;
  FSendThread.WaitFor;
end;

procedure TDCSTcpConnectionThread.SendCommand(ACommandStr: string);
var
  ACommand: TDCSThreadCommand;
begin
  try
    ACommand := FInOutputProcessor.TextToCommand(ACommandStr);
  except
    FDistributor.SendNotification(FListenerId, ntInvalidCommand, null, 'Could not extract command from incoming data: [%s]', '', [ACommandStr]);
    ACommand := nil;
  end;
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
    UniqueString(s);
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

constructor TDCSTcpConnectionThread.create(ADistributor: TDCSDistributor; ADebugTcpServer: TDCSTcpServer; Data: TSocketStream; InitialBuffer: string);
begin
  FData := data;
  FInitialBuffer := InitialBuffer;

  FDistributor := ADistributor;
  FDebugTcpServer := ADebugTcpServer;
  FResponseQueue:=TThreadedQueueString.create(100);
  FListenerId := FDistributor.AddListener(self);
  FInOutputProcessor := CreateInOutputProcessor;

  FSendThread := StartSendThread;
  inherited create(false);
end;

destructor TDCSTcpConnectionThread.Destroy;
begin
  FDistributor.RemoveListener(self);
  FInOutputProcessor.Free;
  FResponseQueue.Free;
  FData.Free;
  inherited Destroy;
end;

function TDCSTcpConnectionThread.CreateInOutputProcessor: TDCSCustomInOutputProcessor;
var
  InOutputProcessorClass: TDCSInOutputProcessorClass;
begin
  InOutputProcessorClass :=  TDCSInOutputProcessorFactory.GetInOutputProcessorByName('json');
  if not assigned(InOutputProcessorClass) then
    InOutputProcessorClass := TDCSJSonInOutputProcessor;
  Result := InOutputProcessorClass.create(FListenerId, FDistributor);
end;

function TDCSTcpConnectionThread.StartSendThread(): TThread;
begin
  Result := TThread.ExecuteInThread(@SendThreadProc);
end;

function TDCSTcpConnectionThread.SendData(AString: string): Boolean;
var
  i: Integer;
  Written: Integer;
  Total: Integer;
begin
  Result := True;
  Total := length(AString);
  Written := 0;
  repeat
    i := FData.Write(AString[Written+1], min(Total-Written, 65536));

    if i < 0 then
      begin
      // JvdS, 20200526: Actually I am not sure that WSAECONNRESET is the correct
      // error-number to check for on Windows. And I have no means to test it.
      if FData.LastError={$IFDEF Windows}WSAECONNRESET{$ELSE}ESysEPIPE{$ENDIF} then
        begin
        // Lost connection
        end
      else
        FDistributor.SendNotification(FListenerId, ntConnectionProblem, null, 'Error during write. Socket-error: %d', '', [FData.LastError]);
      Result := False;
      end
    else
      begin
      Inc(Written, i);
      end;
  until not Result or (Written >= Total);
end;

procedure TDCSTcpConnectionThread.Stop;
begin
  Terminate;
  {$IFDEF Unix}
  pthread_kill(self.Handle, SIGTERM);
  {$ELSE}
  FileClose(FData.Handle);
  {$ENDIF}
end;

{ TDCSTcpServer }

procedure TDCSTcpServer.FTCPConnectionAcceptError(Sender: TObject;
  ASocket: Longint; E: Exception; var ErrorAction: TAcceptErrorAction);
const
  StopListeningErrCode = {$IFDEF Linux}22{EINVAL}{$ELSE}53{ECONNABORTED}{$ENDIF};
begin
  if (E is ESocketError) and (ESocketError(E).Code=seAcceptFailed) and (socketerror=StopListeningErrCode) then
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
  FFirstError := '';
  for i := 0 to FSensePorts-1 do
    begin
    conn := false;
    InetServer := TInetServer.Create(FPort+i);
    try
      InetServer.ReuseAddress := True;
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
  {$IFDEF Unix}
  // Without this, on Linux at least, Data.Write can generate a SIGPIPE exception
  // when the other side disconnects unexpectedly. We do not want this and use the
  // return value to detect the lost connection.
  Data.WriteFlags := $4000; // MSG_NOSIGNAL: do not generate SIGPIPE on connection end
  {$ENDIF Unix}
  AConnectionThread:=CreateTCPConnectionThread(Data);
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

procedure TDCSTcpServer.Stop;
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
    FConnectionList[i].Stop;
  for i := 0 to FConnectionList.Count-1 do
    FConnectionList[i].WaitFor;
  if FConnectionList.Count<>0 then
    raise exception.create('Not all connections are cleared.');
  FConnectionList.Free;
  inherited Destroy;
end;

function TDCSTcpServer.CreateTCPConnectionThread(Data: TSocketStream): TDCSTcpConnectionThread;
begin
  Result := TDCSTcpConnectionThread.create(FDistributor, Self, data, '');
end;

end.

