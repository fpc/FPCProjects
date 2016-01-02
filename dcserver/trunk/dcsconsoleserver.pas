unit dcsConsoleServer;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes,
  SysUtils,
  DCSHandler,
  DCSInOutputProcessor,
  lazCollections,
  syncobjs,
  pipes;

type

  TDCSEventQueue = specialize TLazThreadedQueue<string>;

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
  FDistributor.AddListener(self);
  FInOutputProcessor := TDCSJSonInOutputProcessor.create(FListenerId, FDistributor);
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
    FInOutputProcessor.Free;
    InputStream.Free;
  end;
end;

constructor TDCSConsoleServer.create(ADistributor: TDCSDistributor);
begin
  FEventStrQueue:=TDCSEventQueue.Create(100, INFINITE, 100);
  FDistributor:=ADistributor;
  inherited Create(false);
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

  FEventStrQueue.Free;
  inherited Destroy;
end;

end.

