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
    FListenerId: integer;
    FInOutputProcessor: TDCSCustomInOutputProcessor;
    FDistributor: TDCSDistributor;
    FEventStrQueue: TDCSEventQueue;
  protected
    procedure Execute; override;
    procedure InitListener(AListenerId: Integer);
    function GetListenerId: Integer;
  public
    constructor create(ADistributor: TDCSDistributor);
    function GetOrigin: string;
    procedure SendEvent(AnEvent: TDCSEvent);
    destructor Destroy; override;
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
begin
  CommandStr := '';
  InputStream:=TInputPipeStream.Create(StdInputHandle);
  try
    while not terminated do
      begin
      if FEventStrQueue.PopItem(EventStr) = wrSignaled then
        begin
        writeln(EventStr);
        end;
      while InputStream.NumBytesAvailable>0 do
        begin
        InputStream.Read(b,sizeof(b));
        if b <> #10 then
          CommandStr:=CommandStr+b
        else
          begin
          ACommand := FInOutputProcessor.TextToCommand(CommandStr);
          if assigned(ACommand) then
            FDistributor.QueueCommand(ACommand);
          CommandStr:='';
          end;
        end;
      end;
  finally
    FDistributor.RemoveListener(self);
    InputStream.Free;
  end;
  // Make sure that all incoming messages are shown on the console:
  while FEventStrQueue.PopItem(EventStr) = wrSignaled do
    writeln(EventStr);
end;

constructor TDCSConsoleServer.create(ADistributor: TDCSDistributor);
begin
  FDistributor:=ADistributor;
  FInOutputProcessor := TDCSJSonInOutputProcessor.create(FListenerId, FDistributor);
  FEventStrQueue:=TDCSEventQueue.Create(100, INFINITE, 100);
  inherited Create(false);
  // We add ourself to the listener here already, so that all messages that are
  // send from the moment of creation are shown on the console. (Like exceptions
  // while creating a controller.)
  FDistributor.AddListener(self);
  FDistributor.SetEventsForListener(self, reAll);
  FDistributor.SetLogEventsForListener(self, reAll, [etError, etWarning, etInfo, etCustom]);
  FDistributor.SetNotificationEventsForListener(self, reAll, [ntConnectionProblem, ntExecutedCommand, ntFailedCommand, ntReceivedCommand, ntInvalidCommand, ntLostConnection, ntNewConnection, ntListenerMessage]);
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
  FInOutputProcessor.Free;

  FEventStrQueue.Free;
  inherited Destroy;
end;

end.

