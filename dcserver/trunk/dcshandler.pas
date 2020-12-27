unit dcsHandler;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes,
  SysUtils,
  syncobjs,
  variants,
  fgl,
  cnocQueue;

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
  TDCSNotificationTypes = set of TDCSNotificationType;

  TDCSReceiveEvents = (
    reAll,
    reOwnOnly,
    reOwnAndGeneral,
    reNone
  );

  TDCSDistributor = class;
  TDCSThreadCommand = class;

  { TDCSEvent }

  TDCSEvent = Class
  private
    FRefCount: integer;
    FUID: variant;
    FLisId: Integer;
  protected
    function GetEventType: TDCSEventType; virtual; abstract;
    function GetIsThreadSafe: Boolean; virtual;
    function Clone: TDCSEvent; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function AddRef: Integer;
    property LisId: Integer read FLisId write FLisId;
    function Release: Integer;
    property EventType: TDCSEventType read GetEventType;
    property UID: variant read FUID write FUID;
  end;
  TDCSEventClass = class of TDCSEvent;

  { TDCSLogEvent }

  TDCSLogEvent = Class(TDCSEvent)
  private
    FLogLevel: TEventType;
    FMessage: string;
  protected
    function GetEventType: TDCSEventType; override;
    function GetIsThreadSafe: Boolean; override;
  public
    property LogLevel: TEventType read FLogLevel write FLogLevel;
    property Message: string read FMessage write FMessage;
  end;

  { TDCSNotificationEvent }

  TDCSNotificationEvent = Class(TDCSEvent)
  private
    FCommand: shortstring;
    FMessage: shortstring;
    FNotificationType: TDCSNotificationType;
  protected
    function GetEventType: TDCSEventType; override;
    function Clone: TDCSEvent; override;
  public
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
    procedure OnIdle; virtual;
    function AcceptCommand(ACommand: TDCSThreadCommand): Boolean; virtual;
  end;
  TDCSCustomControllerClass = class of TDCSCustomController;

  { TDCSThreadCommand }

  TDCSThreadCommand = class
  protected
    FDistributor: TDCSDistributor;
    FSendByLisId: integer;
    FUID: variant;
    FCallOnIdle: Boolean;
    function GetNotificationCommandEventClass: TDCSNotificationEventClass; virtual;
    function CreateReceivedCommandEvent: TDCSNotificationEvent; virtual;
    function CreateExecutedCommandEvent(Success: Boolean; ReturnMessage: string; NotificationClass: TDCSNotificationEventClass): TDCSNotificationEvent; virtual;
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
    class function TextName: string; virtual;
    // The identifier of the Listener that has send this command
    property SendByLisId: integer read FSendByLisId;
    property UID: variant read FUID;
    // Some implementations perform better when OnIdle is called after each
    // command, others perform better when OnIdle is only called when there
    // are no commands received for a while.
    // When CallOnIdle is set, OnIdle will be called after the command is
    // freed.
    property CallOnIdle: Boolean read FCallOnIdle;
  end;
  TDCSThreadCommandClass = class of TDCSThreadCommand;
  TDCSThreadCommandQueue = specialize TcnocThreadedQueue<TDCSThreadCommand>;

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
    // IdleTime is the timeout for waiting for new commands. When there are no
    // commands it is the interval at which OnIdle is being called.
    constructor Create(ADistributor: TDCSDistributor; IdleTime: Cardinal = 100); virtual;
    destructor Destroy; override;
  end;

  { TDCSHandlerThread }

  TDCSHandlerThread = class(TDCSCustomHandlerThread)
  private
    FControllerClass: TDCSCustomControllerClass;
  protected
    function CreateController: TDCSCustomController; override;
  public
    constructor Create(ADistributor: TDCSDistributor; AControllerClass: TDCSCustomControllerClass; IdleTime: Cardinal = 100);
  end;

  TDCSController = class(TDCSCustomController)

  end;

  { TDCSDistributor }

  TDCSDistributor = class
  private
    FListenerList: TThreadList;
    FHandlerThreadList: TThreadList;
    FTimedCommandListThread: TThread;
    class var GIdentifierCount: integer;
  protected
    procedure SetEventsForListener(Alistener: IDCSListener; AnEventType: TDCSEventType; AReceiveEvents: TDCSReceiveEvents; AReceiveLogLevels: TEventTypes; AReceiveNotificationTypes: TDCSNotificationTypes);
  public
    constructor Create();
    destructor Destroy(); override;
    procedure Log(const AString: string; const ALogLevel: TEventType; AnUID: variant; ALisId: Integer = -1);
    procedure AddHandlerThread(AHandlerThread: TDCSCustomHandlerThread);
    procedure RemoveHandlerThread(AHandlerThread: TDCSCustomHandlerThread);

    procedure SendEvent(AnEvent: TDCSEvent);

    // Procedures to send notifications and log-messages to the listeners
    procedure SendNotification(ALisId: integer; ANotificationType: TDCSNotificationType; AnUID: variant; AMessage, ACommand: string);
    procedure SendNotification(ALisId: integer; ANotificationType: TDCSNotificationType; AnUID: variant; AMessage, ACommand: string; Arg: array of const); overload;

    procedure QueueCommand(ACommand: TDCSThreadCommand);
    procedure TimeQueueCommand(ACommand: TDCSThreadCommand; Time: TDateTime);

    // Methods to add and remove listeners
    function AddListener(AListener: IDCSListener): integer;
    procedure RemoveListener(AListener: IDCSListener);
    // Methods to set Listener-options
    procedure SetEventsForListener(Alistener: IDCSListener; AReceiveEvents: TDCSReceiveEvents);
    procedure SetLogEventsForListener(Alistener: IDCSListener; AReceiveEvents: TDCSReceiveEvents; AReceiveLogLevels: TEventTypes);
    procedure SetNotificationEventsForListener(Alistener: IDCSListener; AReceiveEvents: TDCSReceiveEvents; AReceiveNotificationTypes: TDCSNotificationTypes);
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

  cAllNotificationTypes: TDCSNotificationTypes = [ntNewConnection, ntLostConnection, ntInvalidCommand, ntConnectionProblem, ntListenerMessage, ntReceivedCommand, ntExecutedCommand, ntFailedCommand];

implementation

uses
  math,
  dateutils;

type

  { TTimedCommand }

  TTimedCommand = class
  private
    FCommand: TDCSThreadCommand;
    FTriggerTime: TDateTime;
  public
    property TriggerTime: TDateTime read FTriggerTime write FTriggerTime;
    property Command: TDCSThreadCommand read FCommand write FCommand;
  end;

  TFPTimedCommandList = specialize TFPGObjectList<TTimedCommand>;

  { TTimedCommandListThread }

  TTimedCommandListThread = class(TThread)
  private
    FDistributor: TDCSDistributor;
    FCommandList: TFPTimedCommandList;
    FInterruptEvent: PRTLEvent;
    FMonitor: TcnocMonitor;
  public
    constructor Create(ADistributor: TDCSDistributor);
    destructor Destroy; override;
    procedure Execute; override;
    procedure AddCommand(ACommand: TDCSThreadCommand; Time: TDateTime);
    procedure ForceTerminate;
  end;

  TDCSListenerSubscription = record
    ReceiveEvents: TDCSReceiveEvents;
    EventTypes: TEventTypes;
    NotificationTypes: TDCSNotificationTypes;
  end;
  TDCSListenerSubscriptionArray = array[TDCSEventType] of TDCSListenerSubscription;

  { TDCSListenerContainer }

  TDCSListenerContainer = class
  private
    FListener: IDCSListener;
    FTDCSListenerSubscriptionArray: TDCSListenerSubscriptionArray;
  public
    constructor Create(AListener: IDCSListener);
    property SubscriptionArray: TDCSListenerSubscriptionArray read FTDCSListenerSubscriptionArray write FTDCSListenerSubscriptionArray;
    property Listener: IDCSListener read FListener write FListener;
  end;

constructor TDCSListenerContainer.Create(AListener: IDCSListener);
begin
  inherited Create();
  FListener := AListener;
  FTDCSListenerSubscriptionArray[dcsetEvent].ReceiveEvents := reOwnAndGeneral;
  FTDCSListenerSubscriptionArray[dcsetLog].ReceiveEvents := reOwnAndGeneral;
  FTDCSListenerSubscriptionArray[dcsetLog].EventTypes := [etWarning, etError];
  FTDCSListenerSubscriptionArray[dcsetNotification].NotificationTypes := [
    ntNewConnection,ntLostConnection,ntInvalidCommand,ntConnectionProblem,
    ntListenerMessage,ntReceivedCommand,ntExecutedCommand,ntFailedCommand];
  FTDCSListenerSubscriptionArray[dcsetNotification].ReceiveEvents := reOwnAndGeneral;
end;

{ TQueueTimeThread }

constructor TTimedCommandListThread.Create(ADistributor: TDCSDistributor);
begin
  FDistributor := ADistributor;
  FInterruptEvent := RTLEventCreate;
  FMonitor := TcnocMonitor.create;
  FCommandList := TFPTimedCommandList.Create(True);
  inherited Create(False);
end;

destructor TTimedCommandListThread.Destroy;
begin
  RTLeventdestroy(FInterruptEvent);
  FMonitor.Free;
  inherited Destroy;
end;

procedure TTimedCommandListThread.Execute;
var
  TimeToNextCheck: QWord;
  CurrentTime: TDateTime;
  FirstCommand: TTimedCommand;
begin
  while not Terminated do
    begin
    FMonitor.Enter;
    try
      if FCommandList.Count > 0 then
        begin
        CurrentTime := Now;
        FirstCommand := FCommandList[0];
        if FirstCommand.TriggerTime<CurrentTime then
          begin
          FDistributor.QueueCommand(FirstCommand.Command);
          FCommandList.Delete(0);
          TimeToNextCheck := 0;
          end
        else
          begin
          TimeToNextCheck := MilliSecondsBetween(FirstCommand.TriggerTime, CurrentTime);
          end;
        end
      else
        TimeToNextCheck := 500;
    finally
      FMonitor.Leave;
    end;

    if TimeToNextCheck > 0 then
      begin
      RTLeventWaitFor(FInterruptEvent, TimeToNextCheck);
      RTLeventResetEvent(FInterruptEvent);
      end;
    end;
end;

procedure TTimedCommandListThread.AddCommand(ACommand: TDCSThreadCommand; Time: TDateTime);
var
  TimedCommand: TTimedCommand;
  IntervalLow,
  IntervalHigh,
  IntervalMid: integer;
  Span: integer;
  Found: Boolean;
  Res: TValueRelationship;
begin
  FMonitor.Enter;
  try
    TimedCommand := TTimedCommand.Create;
    TimedCommand.TriggerTime := Time;
    TimedCommand.Command := ACommand;

    IntervalLow := 0;
    IntervalHigh := FCommandList.Count;
    Span := IntervalHigh-IntervalLow;

    if Span=0 then
      begin
      FCommandList.Add(TimedCommand);
      found := True;
      end
    else
      Found := false;

    while not Found do
      begin
      IntervalMid := IntervalLow + ((IntervalHigh-IntervalLow) div 2);

      Res := CompareDateTime(FCommandList[IntervalMid].TriggerTime,Time);

      if Res=EqualsValue then
        begin
        FCommandList.Insert(IntervalMid, TimedCommand);
        Found := True;
        end
      else if Res=GreaterThanValue then
        begin
        IntervalHigh := IntervalMid;
        end
      else
        begin
        IntervalLow := IntervalMid;
        end;

      if Span=1 then
        begin
        if Res = LessThanValue then
          begin
          FCommandList.Insert(IntervalHigh, TimedCommand);
          if IntervalHigh=0 then
            RTLeventSetEvent(FInterruptEvent);
          end
        else
          begin
          FCommandList.Insert(IntervalLow, TimedCommand);
          if IntervalLow=0 then
            RTLeventSetEvent(FInterruptEvent);
          end;
        Found:= True;
        end;

      Span := IntervalHigh-IntervalLow;
      end;
  finally
    FMonitor.Leave;
  end;
end;

procedure TTimedCommandListThread.ForceTerminate;
begin
  Terminate;
  RTLeventSetEvent(FInterruptEvent);
end;

{ TDCSCustomController }

constructor TDCSCustomController.Create(ADistributor: TDCSDistributor);
begin
  FDistributor := ADistributor;
end;

procedure TDCSCustomController.Init;
begin
  //
end;

procedure TDCSCustomController.OnIdle;
begin
  // Do nothing
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
  Result.LisId := LisId;
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

function TDCSLogEvent.GetIsThreadSafe: Boolean;
begin
  Result := False;
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

function TDCSThreadCommand.CreateExecutedCommandEvent(Success: Boolean; ReturnMessage: string; NotificationClass: TDCSNotificationEventClass): TDCSNotificationEvent;
begin
  Result := NotificationClass.Create;
  Result.UID := FUID;
  Result.Command := TextName;
  if Success then
    begin
    Result.NotificationType:=ntExecutedCommand;
    Result.Message:=Format('%s-command executed succesfully.',[Result.Command])
    end
  else
    begin
    Result.NotificationType:=ntFailedCommand;
    Result.Message:=Format('%s-command failed.',[Result.Command])
    end;
  Result.LisId:=SendByLisId;
  if ReturnMessage<>'' then
    Result.Message:=ReturnMessage;
end;

constructor TDCSThreadCommand.Create(ASendByLisId: integer; AnUID: variant; ADistributor: TDCSDistributor);
begin
  FUID := AnUID;
  if not Assigned(ADistributor) then
    raise Exception.Create('Distributor is nil. A TDCSThreadCommand can not without a Distributor.');
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

  Event := CreateExecutedCommandEvent(Success, ReturnMessage, GetNotificationCommandEventClass);
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

class function TDCSThreadCommand.TextName: string;
begin
  Result := ClassName;
end;

{ TDCSDistributor }

procedure TDCSDistributor.SendEvent(AnEvent: TDCSEvent);
var
  i: integer;
  AList: TList;
  ListenerContainer: TDCSListenerContainer;
  ASendEvent: Boolean;
begin
  AList:=FListenerList.LockList;
  try
    for i := 0 to AList.Count-1 do
      begin
      ListenerContainer := TDCSListenerContainer(AList[i]);
      ASendEvent := False;
      case ListenerContainer.SubscriptionArray[AnEvent.EventType].ReceiveEvents of
        reAll:
          ASendEvent := True;
        reOwnAndGeneral:
          begin
          if (AnEvent.LisId=-1) or (AnEvent.LisId=ListenerContainer.Listener.ListenerId) then
            ASendEvent := True;
          end;
        reOwnOnly:
          begin
          if AnEvent.LisId=ListenerContainer.Listener.ListenerId then
            ASendEvent := True;
          end;
      end;

      if ASendEvent then
        begin
        if (AnEvent is TDCSNotificationEvent) then
          begin
          if not (TDCSNotificationEvent(AnEvent).NotificationType in ListenerContainer.FTDCSListenerSubscriptionArray[dcsetNotification].NotificationTypes) then
            ASendEvent := False;
          end
        else if (AnEvent is TDCSLogEvent) then
          begin
          if not (TDCSLogEvent(AnEvent).LogLevel in ListenerContainer.FTDCSListenerSubscriptionArray[dcsetLog].EventTypes) then
            ASendEvent := False;
          end;

        if ASendEvent then
          ListenerContainer.Listener.SendEvent(AnEvent);
        end;
      end;
  finally
    FListenerList.UnlockList;
  end;
end;

procedure TDCSDistributor.SetEventsForListener(Alistener: IDCSListener; AnEventType: TDCSEventType;
  AReceiveEvents: TDCSReceiveEvents; AReceiveLogLevels: TEventTypes; AReceiveNotificationTypes: TDCSNotificationTypes);
var
  List: TList;
  ListenerContainer: TDCSListenerContainer;
  i: Integer;
begin
  List:=FListenerList.LockList;
  try
    for i := 0 to List.Count-1 do
      begin
      ListenerContainer := TDCSListenerContainer(List[i]);
      if ListenerContainer.Listener=Alistener then
        begin
        ListenerContainer.FTDCSListenerSubscriptionArray[AnEventType].ReceiveEvents := AReceiveEvents;
        ListenerContainer.FTDCSListenerSubscriptionArray[AnEventType].EventTypes := AReceiveLogLevels;
        ListenerContainer.FTDCSListenerSubscriptionArray[AnEventType].NotificationTypes := AReceiveNotificationTypes;
        break;
        end;
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
  if assigned(FTimedCommandListThread) then
    begin
    TTimedCommandListThread(FTimedCommandListThread).ForceTerminate;
    FTimedCommandListThread.WaitFor;
    FTimedCommandListThread.Free;
    end;

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

procedure TDCSDistributor.Log(const AString: string; const ALogLevel: TEventType; AnUID: variant;
  ALisId: Integer);
var
  AnEvent: TDCSLogEvent;
begin
  AnEvent := TDCSLogEvent.Create;
  try
    AnEvent.LogLevel := ALogLevel;
    AnEvent.Message :=  AString;
    AnEvent.UID := AnUID;
    AnEvent.LisId := ALisId;
    try
      SendEvent(AnEvent);
    Except
      // Exceptions while logging should not lead to any problems
      // for the calling code. So the exceptions are eaten...
      // Where is no decent log-mechanism, so fallback to writeln
      on E: Exception do
        writeln(Format('Exception while sending a log-message from [%d]: [%s]', [ALisId, AString]))
    end;
  finally
    AnEvent.Release;
  end;
end;

procedure TDCSDistributor.AddHandlerThread(AHandlerThread: TDCSCustomHandlerThread);
begin
  FHandlerThreadList.Add(AHandlerThread);
end;

procedure TDCSDistributor.RemoveHandlerThread(AHandlerThread: TDCSCustomHandlerThread);
begin
  FHandlerThreadList.Remove(AHandlerThread);
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
    AnEvent.NotificationType := ANotificationType;
    try
      SendEvent(AnEvent);
    Except
      // Exceptions while sending notifications should not lead to any problems
      // for the calling code. So the exceptions are eaten...
      // Where is no decent log-mechanism, so fallback to writeln
      on E: Exception do
        writeln(Format('Exception while sending a notification from command [%s]:[%d]: [%s]', [ACommand, ALisId, AMessage]))
    end;
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

procedure TDCSDistributor.TimeQueueCommand(ACommand: TDCSThreadCommand; Time: TDateTime);
begin
  if not assigned(FTimedCommandListThread) then
    begin
    FTimedCommandListThread := TTimedCommandListThread.Create(Self);
    end;
  TTimedCommandListThread(FTimedCommandListThread).AddCommand(ACommand, Time);
end;

function TDCSDistributor.AddListener(AListener: IDCSListener): integer;
begin
  inc(GIdentifierCount);
  result := GIdentifierCount;
  FListenerList.Add(TDCSListenerContainer.Create(AListener));
  AListener.InitListener(Result);
  SendNotification(result, ntNewConnection, null, 'New listener: %s', '',[AListener.GetOrigin]);
end;

procedure TDCSDistributor.RemoveListener(AListener: IDCSListener);
var
  List: TList;
  I: Integer;
begin
  SendNotification(AListener.ListenerId, ntLostConnection, null, 'Remove listener: %s', '',[AListener.GetOrigin]);
  List := FListenerList.LockList;
  try
    for I := 0 to List.Count -1 do
      if TDCSListenerContainer(List.Items[I]).Listener=AListener then
        begin
        TDCSListenerContainer(List.Items[I]).Free;
        List.Delete(I);
        Break;
        end;
  finally
    FListenerList.UnlockList;
  end;
end;

procedure TDCSDistributor.SetEventsForListener(Alistener: IDCSListener;
  AReceiveEvents: TDCSReceiveEvents);
begin
  SetEventsForListener(Alistener, dcsetEvent, AReceiveEvents, [], []);
end;

procedure TDCSDistributor.SetLogEventsForListener(Alistener: IDCSListener;
  AReceiveEvents: TDCSReceiveEvents; AReceiveLogLevels: TEventTypes);
begin
  SetEventsForListener(Alistener, dcsetLog, AReceiveEvents, AReceiveLogLevels, []);
end;

procedure TDCSDistributor.SetNotificationEventsForListener(Alistener: IDCSListener;
  AReceiveEvents: TDCSReceiveEvents; AReceiveNotificationTypes: TDCSNotificationTypes);
begin
  SetEventsForListener(Alistener, dcsetNotification, AReceiveEvents, [], AReceiveNotificationTypes);
end;

{ TDCSHandlerThread }

function TDCSHandlerThread.CreateController: TDCSCustomController;
begin
  Result := FControllerClass.Create(FDistributor);
end;

constructor TDCSHandlerThread.Create(ADistributor: TDCSDistributor;
  AControllerClass: TDCSCustomControllerClass;
  IdleTime: Cardinal = 100);
begin
  inherited Create(ADistributor, IdleTime);
  if Assigned(AControllerClass) then
    FControllerClass := AControllerClass
  else
    FControllerClass := TDCSController;
end;

{ TDCSHandlerThread }

procedure TDCSCustomHandlerThread.Execute;
var
  ACommand: TDCSThreadCommand;
  CallOnIdle: Boolean;
begin
  try
    if assigned(FDistributor) then
      FDistributor.AddHandlerThread(Self);
  except
    on E: Exception do
      begin
      FDistributor.Log('Exception while adding handler-thread, thread terminated: '+e.Message, etError, Null);
      Exit;
      end;
  end;

  try
    FController := CreateController;
    try
      FController.init;
      repeat
        try
          if FCommandQueue.PopItem(ACommand)<>wrSignaled then
            ACommand:=nil;

          CallOnIdle := True;
          if assigned(ACommand) then
            begin
            try
              ACommand.Execute(FController);
              CallOnIdle := ACommand.CallOnIdle;
            except
              on E: Exception do
                begin
                FDistributor.Log('Exception during execution of '+ACommand.TextName+' command: '+e.Message, etError, ACommand.UID, ACommand.SendByLisId);
                FDistributor.SendNotification(ACommand.SendByLisId, ntFailedCommand, ACommand.UID, 'Exception during execution of %s-command.', ACommand.TextName, [ACommand.TextName]);
                end;
            end;
            ACommand.Free;
            end;
          if CallOnIdle then
            begin
            try
              FController.OnIdle;
            except
              on E: Exception do
                FDistributor.Log('Exception in OnIdle: '+e.Message, etError, Null); // just continue
            end;
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
  FDistributor.RemoveHandlerThread(Self);
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

constructor TDCSCustomHandlerThread.Create(ADistributor: TDCSDistributor; IdleTime: Cardinal = 100);
begin
  inherited create(false);
  FCommandQueue := TDCSThreadCommandQueue.create(100, INFINITE, IdleTime);
  FDistributor := ADistributor;
end;

destructor TDCSCustomHandlerThread.Destroy;
begin
  FCommandQueue.Free;
  inherited Destroy;
end;

end.

