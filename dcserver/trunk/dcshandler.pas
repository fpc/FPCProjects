unit dcsHandler;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes,
  SysUtils,
  syncobjs,
  variants,
  lazCollections;

type
  // The debug-thread sends three different kind of messages to it's listeners
  TDCSEventType = (
    dcsetEvent,             // Messages that are send by the debugger. (debuggee has been started, pauzed, stopped, etc.)
    dcsetLog,               // Log-messages send by the debugger. (Fpdebug also uses log-messages to inform users of some
                         // events (the dllInfo-log messages)
    dcsetNotification       // Messages from the debug-thread itself. Including new or lost connections and commands that
                         // are queued or executed.
  );

  // The different kinds of etNotifications
  TDCSNotificationType = (
    ntNewConnection,
    ntLostConnection,
    ntInvalidCommand,
    ntConnectionProblem,
    ntListenerMessage,
    ntReceivedCommand,
    ntExecutedCommand,
    ntFailedCommand
  );

  TDCSDistributor = class;
  TDCSThreadCommand = class;

  { TDCSEvent }

  TDCSEvent = Class
  private
    FRefCount: integer;
    FUID: variant;
  protected
    function GetEventType: TDCSEventType; virtual; abstract;
    function GetIsThreadSafe: Boolean; virtual;
    function Clone: TDCSEvent; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function AddRef: Integer;
    function Release: Integer;
    property EventType: TDCSEventType read GetEventType;
    property UID: variant read FUID write FUID;
  end;
  TDCSEventClass = class of TDCSEvent;

  { TDCSLogEvent }

  TDCSLogEvent = Class(TDCSEvent)
  private
    FLogLevel: TEventType;
    FMessage: shortstring;
  protected
    function GetEventType: TDCSEventType; override;
  public
    property LogLevel: TEventType read FLogLevel write FLogLevel;
    property Message: shortstring read FMessage write FMessage;
  end;

  { TDCSNotificationEvent }

  TDCSNotificationEvent = Class(TDCSEvent)
  private
    FCommand: shortstring;
    FLisId: Integer;
    FMessage: shortstring;
    FNotificationType: TDCSNotificationType;
  protected
    function GetEventType: TDCSEventType; override;
    function Clone: TDCSEvent; override;
  public
    property LisId: Integer read FLisId write FLisId;
    property Command: shortstring read FCommand write FCommand;
    property Message: shortstring read FMessage write FMessage;
    property NotificationType: TDCSNotificationType read FNotificationType write FNotificationType;
  end;
  TDCSNotificationEventClass = class of TDCSNotificationEvent;


  { IDCSListener }

  // Each listener should implement this interface.
  IDCSListener = interface ['{2230763A-672E-4EC1-941D-6B8814D789C8}']
     function GetListenerId: Integer;
     procedure InitListener(AListenerId: Integer);
     // This procedure is called by the debugthread when there is a message for the listener.
     // Not that this procedure will be called from within the debug-thread, and should not take too much
     // resources, or ot will slow down the debugging.
     procedure SendEvent(AnEvent: TDCSEvent);
     // Gives more information about the origin of the listener.
     function GetOrigin: string;
     property ListenerId: Integer read GetListenerId;
  end;

  { TDCSCustomController }

  TDCSCustomController = class
  protected
    FDistributor: TDCSDistributor;
  public
    constructor Create(ADistributor: TDCSDistributor); virtual;
    procedure Init; virtual;
    function AcceptCommand(ACommand: TDCSThreadCommand): Boolean; virtual;
  end;
  TDCSCustomControllerClass = class of TDCSCustomController;

  { TDCSThreadCommand }

  TDCSThreadCommand = class
  protected
    FDistributor: TDCSDistributor;
    FSendByLisId: integer;
    FUID: variant;
    function GetNotificationCommandEventClass: TDCSNotificationEventClass; virtual;
    function CreateReceivedCommandEvent: TDCSNotificationEvent; virtual;
    function CreateExecutedCommandEvent(Success: Boolean; ReturnMessage: string): TDCSNotificationEvent; virtual;
  public
    constructor Create(ASendByLisId: integer; AnUID: variant; ADistributor: TDCSDistributor); virtual;
    //procedure InitiateComand(var AnEvent: TFpDebugEvent); virtual;
    procedure Execute(AController: TDCSCustomController); virtual;
    function DoExecute(AController: TDCSCustomController; out ReturnMessage: string): Boolean; virtual;
    // This method is called before the command is queued for execution in the controller's debug loop. This
    // can happen in any thread. If DoQueueCommand is true, the result is ignored or else a success-event is
    // send if the result is true, a failure if the result is false.
    procedure PreExecute(AController: TDCSCustomController; out DoQueueCommand: boolean); virtual;
    // The name that is used to identify the command
    class function TextName: string; virtual; abstract;
    // The identifier of the Listener that has send this command
    property SendByLisId: integer read FSendByLisId;
    property UID: variant read FUID;
  end;
  TDCSThreadCommandClass = class of TDCSThreadCommand;
  TDCSThreadCommandQueue = specialize TLazThreadedQueue<TDCSThreadCommand>;

  { TDCSCustomHandlerThread }

  TDCSCustomHandlerThread = class(TThread)
  private
    FDistributor: TDCSDistributor;
    FController: TDCSCustomController;
    FCommandQueue: TDCSThreadCommandQueue;
  protected
    function CreateController: TDCSCustomController; virtual; abstract;
    procedure Execute; override;
    procedure QueueCommand(ACommand: TDCSThreadCommand);
    function AcceptCommand(ACommand: TDCSThreadCommand): Boolean;
    property Controller: TDCSCustomController read FController;
    property Distributor: TDCSDistributor read FDistributor;
  public
    constructor Create(ADistributor: TDCSDistributor); virtual;
    destructor Destroy; override;
  end;

  { TDCSHandlerThread }

  TDCSHandlerThread = class(TDCSCustomHandlerThread)
  private
    FControllerClass: TDCSCustomControllerClass;
  protected
    function CreateController: TDCSCustomController; override;
  public
    constructor Create(ADistributor: TDCSDistributor; AControllerClass: TDCSCustomControllerClass);
  end;

  TDCSController = class(TDCSCustomController)

  end;

  { TDCSDistributor }

  TDCSDistributor = class
  private
    FListenerList: TThreadList;
    FHandlerThreadList: TThreadList;
    class var GIdentifierCount: integer;
  protected
  public
    constructor Create();
    destructor Destroy(); override;
    procedure Log(const AString: string; const ALogLevel: TEventType; AnUID: variant);
    procedure AddHandlerThread(AHandlerThread: TDCSCustomHandlerThread);

    procedure SendEvent(AnEvent: TDCSEvent);

    // Procedures to send notifications and log-messages to the listeners
    procedure SendNotification(ALisId: integer; ANotificationType: TDCSNotificationType; AnUID: variant; AMessage, ACommand: string);
    procedure SendNotification(ALisId: integer; ANotificationType: TDCSNotificationType; AnUID: variant; AMessage, ACommand: string; Arg: array of const); overload;

    procedure QueueCommand(ACommand: TDCSThreadCommand);

    // Methods to add and remove listeners
    function AddListener(AListener: IDCSListener): integer;
    procedure RemoveListener(AListener: IDCSListener);
  end;

const
  DCSEventTypeNames: array[TDCSEventType] of string = (
    'Event',
    'Log',
    'Notification');

  DCSLogLevelNames: array[TEventType] of string = (
    'Custom',
    'Info',
    'Warning',
    'Error',
    'Debug'
    );

  DCSNotificationTypeNames: array[TDCSNotificationType] of string = (
    'NewConnection',
    'LostConnection',
    'InvalidCommand',
    'ConnectionProblem',
    'ListenerMessage',
    'ReceivedCommand',
    'ExecutedCommand',
    'FailedCommand'
  );

implementation

{ TDCSCustomController }

constructor TDCSCustomController.Create(ADistributor: TDCSDistributor);
begin
  FDistributor := ADistributor;
end;

procedure TDCSCustomController.Init;
begin
  //
end;

function TDCSCustomController.AcceptCommand(ACommand: TDCSThreadCommand): Boolean;
begin
  Result := True;
end;

{ TDCSEvent }

function TDCSEvent.GetIsThreadSafe: Boolean;
begin
  Result := True;
end;

function TDCSEvent.Clone: TDCSEvent;
begin
  Result := TDCSEventClass(ClassType).Create;
  Result.UID := UID;
end;

constructor TDCSEvent.Create;
begin
  FRefCount := 1;
end;

destructor TDCSEvent.Destroy;
begin
  Assert(FRefCount=0);
  inherited Destroy;
end;

function TDCSEvent.AddRef: Integer;
begin
  Result := InterLockedIncrement(FRefCount);
end;

function TDCSEvent.Release: Integer;
begin
  Result := InterLockedDecrement(FRefCount);
  if Result = 0 then
    begin
    Free;
    end;
end;

{ TDCSNotificationEvent }

function TDCSNotificationEvent.GetEventType: TDCSEventType;
begin
  result := dcsetNotification;
end;

function TDCSNotificationEvent.Clone: TDCSEvent;
var
  Cl: TDCSNotificationEvent;
begin
  Result := inherited Clone;
  Cl := TDCSNotificationEvent(Result);
  Cl.LisId := LisId;
  Cl.Command := Command;
  Cl.Message := Message;
  Cl.NotificationType := NotificationType;
end;

{ TDCSLogEvent }

function TDCSLogEvent.GetEventType: TDCSEventType;
begin
  result := dcsetLog;
end;

{ TDCSThreadCommand }

function TDCSThreadCommand.GetNotificationCommandEventClass: TDCSNotificationEventClass;
begin
  Result := TDCSNotificationEvent;
end;

function TDCSThreadCommand.CreateReceivedCommandEvent: TDCSNotificationEvent;
begin
  Result := GetNotificationCommandEventClass.Create;
  Result.NotificationType:=ntReceivedCommand;
  Result.LisId:=SendByLisId;
  Result.UID := FUID;
  Result.Message:=Format('Received %s-command.',[TextName]);
end;

function TDCSThreadCommand.CreateExecutedCommandEvent(Success: Boolean; ReturnMessage: string): TDCSNotificationEvent;
begin
  Result := GetNotificationCommandEventClass.Create;
  Result.UID := FUID;
  if Success then
    begin
    Result.NotificationType:=ntExecutedCommand;
    Result.Message:=Format('%s-command executed succesfully.',[TextName])
    end
  else
    begin
    Result.NotificationType:=ntFailedCommand;
    Result.Message:=Format('%s-command failed.',[TextName])
    end;
  Result.LisId:=SendByLisId;
  if ReturnMessage<>'' then
    Result.Message:=ReturnMessage;
end;

constructor TDCSThreadCommand.Create(ASendByLisId: integer; AnUID: variant; ADistributor: TDCSDistributor);
begin
  FUID := AnUID;
  FDistributor := ADistributor;
  FSendByLisId := ASendByLisId;
end;

procedure TDCSThreadCommand.Execute(AController: TDCSCustomController);
var
  Event: TDCSNotificationEvent;
  Success: Boolean;
  ReturnMessage: String;
begin
  Event := CreateReceivedCommandEvent;
  try
    FDistributor.SendEvent(Event);
  finally
    Event.Release;
  end;

  Success := DoExecute(AController, ReturnMessage);

  Event := CreateExecutedCommandEvent(Success, ReturnMessage);
  try
    FDistributor.SendEvent(Event);
  finally
    Event.Release;
  end;
end;

function TDCSThreadCommand.DoExecute(AController: TDCSCustomController; out ReturnMessage: string): Boolean;
begin
  Result := True;
  ReturnMessage := '';
end;

procedure TDCSThreadCommand.PreExecute(AController: TDCSCustomController; out DoQueueCommand: boolean);
begin
  DoQueueCommand := True;
end;

{ TDCSDistributor }

procedure TDCSDistributor.SendEvent(AnEvent: TDCSEvent);
var
  i: integer;
  AList: TList;
begin
  AList:=FListenerList.LockList;
  try
    for i := 0 to AList.Count-1 do
      begin
      IDCSListener(AList[i]).SendEvent(AnEvent);
      end;
  finally
    FListenerList.UnlockList;
  end;
end;

constructor TDCSDistributor.Create;
begin
  FHandlerThreadList := TThreadList.Create;
  FListenerList:=TThreadList.Create;
end;

destructor TDCSDistributor.Destroy;
var
  i: integer;
  AList: TList;
begin
  AList:=FHandlerThreadList.LockList;
  try
    for i := 0 to AList.Count-1 do
      begin
      TDCSCustomHandlerThread(AList[i]).Terminate;
      end;
  finally
    FHandlerThreadList.UnlockList;
  end;
  sleep(10);
  AList:=FHandlerThreadList.LockList;
  try
    for i := 0 to AList.Count-1 do
      begin
      TDCSCustomHandlerThread(AList[i]).WaitFor;
      TDCSCustomHandlerThread(AList[i]).Free;
      end;
  finally
    FHandlerThreadList.UnlockList;
  end;
  FListenerList.Free;
  FHandlerThreadList.Free;
  inherited Destroy;
end;

procedure TDCSDistributor.Log(const AString: string; const ALogLevel: TEventType; AnUID: variant);
var
  AnEvent: TDCSLogEvent;
begin
  AnEvent := TDCSLogEvent.Create;
  try
    AnEvent.LogLevel := ALogLevel;
    AnEvent.Message :=  AString;
    AnEvent.UID := AnUID;
    SendEvent(AnEvent);
  finally
    AnEvent.Release;
  end;
end;

procedure TDCSDistributor.AddHandlerThread(AHandlerThread: TDCSCustomHandlerThread);
begin
  FHandlerThreadList.Add(AHandlerThread);
end;

procedure TDCSDistributor.SendNotification(ALisId: integer;
  ANotificationType: TDCSNotificationType; AnUID: variant; AMessage, ACommand: string);
var
  AnEvent: TDCSNotificationEvent;
begin
  AnEvent := TDCSNotificationEvent.Create;
  try
    AnEvent.Message := AMessage;
    AnEvent.LisId := ALisId;
    AnEvent.Command := ACommand;
    AnEvent.UID := AnUID;
    SendEvent(AnEvent);
  finally
    AnEvent.Release;
  end;
end;

procedure TDCSDistributor.SendNotification(ALisId: integer;
  ANotificationType: TDCSNotificationType; AnUID: variant; AMessage, ACommand: string;
  Arg: array of const);
begin
  SendNotification(ALisId, ANotificationType, AnUID, Format(AMessage, Arg), ACommand);
end;

procedure TDCSDistributor.QueueCommand(ACommand: TDCSThreadCommand);
var
  i: integer;
  AList: TList;
  Handler: TDCSCustomHandlerThread;
begin
  AList:=FHandlerThreadList.LockList;
  try
    for i := 0 to AList.Count-1 do
      begin
      Handler := TDCSCustomHandlerThread(AList[i]);
      if Handler.AcceptCommand(ACommand) then
        begin
        Handler.QueueCommand(ACommand);
        Exit;
        end;
      end;
    SendNotification(ACommand.SendByLisId, ntFailedCommand, ACommand.UID, 'No handler available to accept the %s-command.', ACommand.TextName, [ACommand.TextName]);
  finally
    FHandlerThreadList.UnlockList;
  end;
end;

function TDCSDistributor.AddListener(AListener: IDCSListener): integer;
begin
  inc(GIdentifierCount);
  result := GIdentifierCount;
  FListenerList.Add(AListener);
  AListener.InitListener(Result);
  SendNotification(result, ntNewConnection, null, 'New listener: %s', '',[AListener.GetOrigin]);
end;

procedure TDCSDistributor.RemoveListener(AListener: IDCSListener);
begin
  SendNotification(AListener.ListenerId, ntNewConnection, null, 'Remove listener: %s', '',[AListener.GetOrigin]);
  FListenerList.Remove(AListener);
end;

{ TDCSHandlerThread }

function TDCSHandlerThread.CreateController: TDCSCustomController;
begin
  Result := FControllerClass.Create(FDistributor);
end;

constructor TDCSHandlerThread.Create(ADistributor: TDCSDistributor;
  AControllerClass: TDCSCustomControllerClass);
begin
  inherited Create(ADistributor);
  if Assigned(AControllerClass) then
    FControllerClass := AControllerClass
  else
    FControllerClass := TDCSController;
end;

{ TDCSHandlerThread }

procedure TDCSCustomHandlerThread.Execute;
var
  ACommand: TDCSThreadCommand;
begin
  try
    FController := CreateController;
    try
      FController.init;
      repeat
        try
          if FCommandQueue.PopItem(ACommand)<>wrSignaled then
            ACommand:=nil;

          if assigned(ACommand) then
            begin
            try
              ACommand.Execute(FController);
            except
              on E: Exception do
                begin
                FDistributor.Log('Exception during execution of '+ACommand.TextName+' command :'+e.Message, etError, ACommand.UID);
                FDistributor.SendNotification(ACommand.SendByLisId, ntFailedCommand, null, 'Exception during execution of %s-command.', ACommand.TextName, [ACommand.TextName]);
                end;
            end;
            ACommand.Free;
            end;
        except
          on E: Exception do
            FDistributor.Log('Exception in handler-thread: '+e.Message, etError, Null); // just continue
        end;
      until Terminated;
    finally
      FController.Free;
    end;
  except
    on E: Exception do
      FDistributor.Log('Exception in handler-thread, thread terminated: '+e.Message, etError, Null);
  end;
end;

procedure TDCSCustomHandlerThread.QueueCommand(ACommand: TDCSThreadCommand);
var
  DoQueueCommand: boolean;
begin
  try
    ACommand.PreExecute(FController, DoQueueCommand);
  except
    on E: Exception do
      begin
      FDistributor.Log('Exception during pre-execution of '+ACommand.TextName+' command :'+e.Message, etError, ACommand.UID);
      FDistributor.SendNotification(ACommand.SendByLisId, ntFailedCommand, ACommand.UID, 'Exception during pre-execution of %s-command.', ACommand.TextName, [ACommand.TextName]);
      DoQueueCommand:=false;
      end;
  end;
  if DoQueueCommand then
    FCommandQueue.PushItem(ACommand)
  else
    ACommand.Free;
end;

function TDCSCustomHandlerThread.AcceptCommand(ACommand: TDCSThreadCommand): Boolean;
begin
  Result := FController.AcceptCommand(ACommand);
end;

constructor TDCSCustomHandlerThread.Create(ADistributor: TDCSDistributor);
begin
  inherited create(false);
  FCommandQueue := TDCSThreadCommandQueue.create(100, INFINITE, 100);
  FDistributor := ADistributor;
end;

destructor TDCSCustomHandlerThread.Destroy;
begin
  FCommandQueue.Free;
  inherited Destroy;
end;

end.

