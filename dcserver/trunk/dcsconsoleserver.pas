unit dcsConsoleServer;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes,
  SysUtils,
  DCSHandler,
  DCSInOutputProcessor,
  cnocQueue,
  syncobjs,
  pipes;

type

  TDCSEventQueue = specialize TcnocThreadedQueue<string>;

  { TDCSConsoleServer }

  TDCSConsoleServer = class(TThread, IDCSListener)
  private
    procedure SendThreadProc();
  protected
    FListenerId: integer;
    FInOutputProcessor: TDCSCustomInOutputProcessor;
    FDistributor: TDCSDistributor;
    FEventStrQueue: TDCSEventQueue;
    FInitialBuffer: string;
    FSendThread: TThread;
    procedure Execute; override;
    procedure InitListener(AListenerId: Integer);
    function GetListenerId: Integer;
    function CreateInOutputProcessor: TDCSCustomInOutputProcessor; virtual;
    function StartSendThread(): TThread; virtual;
  public
    constructor create(ADistributor: TDCSDistributor; InitialBuffer: string);
    function GetOrigin: string;
    procedure SendEvent(AnEvent: TDCSEvent);
    destructor Destroy; override;
    procedure StopListening;
  end;

implementation

{ TDCSConsoleServer }

procedure TDCSConsoleServer.Execute;
var
  InputStream: TInputPipeStream;
  EventStr: string;
  CommandStr: string;
  ACommand: TDCSThreadCommand;
  b: char;
  i: LongInt;
begin
  CommandStr := '';
  InputStream:=TInputPipeStream.Create(StdInputHandle);
  try
    while not terminated do
      begin
      i := InputStream.Read(b,sizeof(b));
      if i > 0 then
        begin
        if b <> #10 then
          CommandStr:=CommandStr+b
        else
          begin
          ACommand := FInOutputProcessor.TextToCommand(CommandStr);
          if assigned(ACommand) then
            FDistributor.QueueCommand(ACommand);
          CommandStr:='';
          end;
        end
      else if i < 0 then
        begin
        FDistributor.Log(Format('Error during read. Socket-error: %d', [GetLastOSError]), etWarning, Null);
        Terminate;
        end
      else // i=0
        begin
        // Zero-count -> Connection closed
        Terminate;
        end;
      end;
  finally
    FDistributor.RemoveListener(self);
    InputStream.Free;
  end;
end;

constructor TDCSConsoleServer.create(ADistributor: TDCSDistributor; InitialBuffer: string);
begin
  FInitialBuffer := InitialBuffer;
  FDistributor:=ADistributor;
  FInOutputProcessor := CreateInOutputProcessor;
  FEventStrQueue:=TDCSEventQueue.Create(100);
  inherited Create(false);
  // We add ourself to the listener here already, so that all messages that are
  // send from the moment of creation are shown on the console. (Like exceptions
  // while creating a controller.)
  FDistributor.AddListener(self);
  FDistributor.SetEventsForListener(self, reAll);
  FDistributor.SetLogEventsForListener(self, reAll, [etError, etWarning, etInfo, etCustom]);
  FDistributor.SetNotificationEventsForListener(self, reAll, [ntConnectionProblem, ntExecutedCommand, ntFailedCommand, ntReceivedCommand, ntInvalidCommand, ntLostConnection, ntNewConnection, ntListenerMessage]);

  FSendThread := StartSendThread;
end;

function TDCSConsoleServer.GetOrigin: string;
begin
  result := 'console';
end;

procedure TDCSConsoleServer.SendEvent(AnEvent: TDCSEvent);
begin
  if assigned(FInOutputProcessor) then
    FEventStrQueue.PushItem(FInOutputProcessor.EventToText(AnEvent));
end;

procedure TDCSConsoleServer.InitListener(AListenerId: Integer);
begin
  FListenerId := FListenerId;
end;

function TDCSConsoleServer.GetListenerId: Integer;
begin
  Result := FListenerId;
end;

destructor TDCSConsoleServer.Destroy;
begin
  FEventStrQueue.DoShutDown;
  FSendThread.WaitFor;
  FInOutputProcessor.Free;
  FEventStrQueue.Free;

  inherited Destroy;
end;

function TDCSConsoleServer.CreateInOutputProcessor: TDCSCustomInOutputProcessor;
begin
  Result := TDCSJSonInOutputProcessor.create(FListenerId, FDistributor);
end;

function TDCSConsoleServer.StartSendThread(): TThread;
begin
  Result := TThread.ExecuteInThread(@SendThreadProc);
end;

procedure TDCSConsoleServer.SendThreadProc();
var
  AStr: string;
begin
  while (FEventStrQueue.PopItem(AStr) = wrSignaled) do
    begin
    WriteLn(AStr);
    end;
end;

procedure TDCSConsoleServer.StopListening;
begin
  Terminate;
  FileClose(StdInputHandle);
end;

end.

