unit DBConnector;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  sqldb,
  variants,
  IBConnection,
  lazCollections,
  dcsHandler,
  dcsThreadCommandFactory,
  RepoTestCommand,
  pkgglobals;

type

  { TDBController }

  TDBController = class(TDCSCustomController, IDCSListener)
  private
    FSQLConnection: TIBConnection;
    FSQLTransaction: TSQLTransaction;
    FListenerId: Integer;
    FLogLineList: TThreadList;
    procedure ProcessQueuedLogItems;
  protected
    procedure OnIdle; override;
    // IDCSListener
    procedure InitListener(AListenerId: Integer);
    function GetListenerId: Integer;
  public
    constructor Create(ADistributor: TDCSDistributor); override;
    function AcceptCommand(ACommand: TDCSThreadCommand): Boolean; override;
    procedure InitDb(DBName, DBUser,DBPassword: string);
    function CreateQuery: TSQLQuery;
    destructor Destroy; override;

    // IDCSListener
    procedure SendEvent(AnEvent: TDCSEvent);
    function GetOrigin: string;
  end;

  TDBCommand = class(TDCSThreadCommand)
  end;

  { TDBGetUniqueIdCommand }

  TDBGetUniqueIdCommand = class(TDBCommand)
  public
    function DoExecute(AController: TDCSCustomController; out ReturnMessage: string): Boolean; override;
    class function TextName: string; override;
  end;

  { TDBStoreCommmandNotification }

  TDBStoreCommmandNotification = class(TDBCommand)
  private
    FLogLineList: TLogLineList;
    FNotificationType: TDCSNotificationType;
    FUniqueId: integer;
  public
    constructor Create(ASendByLisId: integer; AnUID: variant; ADistributor: TDCSDistributor); override;
    function DoExecute(AController: TDCSCustomController; out ReturnMessage: string): Boolean; override;
    class function TextName: string; override;
    property NotificationType: TDCSNotificationType read FNotificationType write FNotificationType;
    property UniqueId: integer read FUniqueId write FUniqueId;
    property LogLineList: TLogLineList read FLogLineList;
  end;

  { TDBStoreCommmandNotificationEvent }

  TDBStoreCommmandNotificationEvent = class(TCustomTestCommandNotificationEvent)
  private
    FState: string;
    FTestUID: String;
  published
    property TestUID: string read FTestUID write FTestUID;
    property State: string read FState write FState;
  end;

  { TDBQueryTestResultCommand }

  TDBQueryTestResultCommand = class(TDBCommand)
  private
    FUniqueId: integer;
    FState: String;
    FReceivedTime, FFinishedTime: TDateTime;
    FTestUID: string;
    FLogLineList: TLogLineList;
  protected
    function GetNotificationCommandEventClass: TDCSNotificationEventClass; override;
    function CreateExecutedCommandEvent(Success: Boolean; ReturnMessage: string; NotificationClass: TDCSNotificationEventClass): TDCSNotificationEvent; override;
    function StringToLogLevel(AStr: string): TLogLevel;
  public
    function DoExecute(AController: TDCSCustomController; out ReturnMessage: string): Boolean; override;
    class function TextName: string; override;
    destructor Destroy; override;
  published
    property UniqueId: integer read FUniqueId write FUniqueId;
  end;

implementation

uses
  RepoController;

{ TDBQueryTestResultCommand }

function TDBQueryTestResultCommand.GetNotificationCommandEventClass: TDCSNotificationEventClass;
begin
  Result := TDBStoreCommmandNotificationEvent;
end;

function TDBQueryTestResultCommand.CreateExecutedCommandEvent(Success: Boolean;
  ReturnMessage: string; NotificationClass: TDCSNotificationEventClass): TDCSNotificationEvent;
var
  Event: TDBStoreCommmandNotificationEvent;
begin
  Result := inherited CreateExecutedCommandEvent(Success, ReturnMessage, NotificationClass);
  Event := TDBStoreCommmandNotificationEvent(Result);
  Event.LogLineList.Clone(FLogLineList);
  Event.UniqueId := FUniqueId;
  Event.TestUID := FTestUID;
  case FState of
    'R' : Event.State := 'Pending';
    'C' : Event.State := 'Complete';
    'F' : Event.State := 'Failed';
  else
    Event.State := 'Unknown';
  end;
end;

function TDBQueryTestResultCommand.StringToLogLevel(AStr: string): TLogLevel;
var
  L: TLogLevel;
begin
  for L := low(TLogLevel) to high(TLogLevel) do
    begin
    if AStr=SLogLevel[L] then
      begin
      Result := L;
      Exit;
      end;
    end;
  raise exception.CreateFmt('Invalid loglevel %s',[AStr]);
end;

function TDBQueryTestResultCommand.DoExecute(AController: TDCSCustomController; out ReturnMessage: string): Boolean;
var
  Query: TSQLQuery;
  LogLine: TLogLine;
begin
  Query := TDBController(AController).CreateQuery;
  try
    Query.SQL.Text := 'select uid, state, receivedtime, finishedtime from command where commandid=:commandid;';
    Query.ParamByName('commandid').AsInteger := UniqueId;
    Query.Open;
    Result := not Query.EOF;
    if Result then
      begin
      FState := Query.FieldByName('state').AsString;
      FTestUID := Query.FieldByName('uid').AsString;
      FReceivedTime := Query.FieldByName('receivedtime').AsDateTime;
      FFinishedTime := Query.FieldByName('finishedtime').AsDateTime;
      Query.Close;
      Query.SQL.Text := 'select * from commandlogline where commandid=:commandid';
      Query.ParamByName('commandid').AsInteger := UniqueId;
      Query.Open;
      FLogLineList := TLogLineList.Create;
      while not Query.EOF do
        begin
        LogLine := TLogLine.Create;
        LogLine.Msg := Query.FieldByName('message').AsString;
        LogLine.LogLevel := StringToLogLevel(Query.FieldByName('loglevel').AsString);
        FLogLineList.Add(LogLine);
        Query.Next;
        end;
      end;
  finally
    Query.Free;
  end;
end;

class function TDBQueryTestResultCommand.TextName: string;
begin
  result := 'querytestresult';
end;

destructor TDBQueryTestResultCommand.Destroy;
begin
  FLogLineList.Free;
  inherited Destroy;
end;

{ TDBStoreCommmandNotification }

constructor TDBStoreCommmandNotification.Create(ASendByLisId: integer; AnUID: variant; ADistributor: TDCSDistributor);
begin
  inherited Create(ASendByLisId, AnUID, ADistributor);
  FLogLineList := TLogLineList.Create(True);
end;

function TDBStoreCommmandNotification.DoExecute(AController: TDCSCustomController; out ReturnMessage: string): Boolean;
var
  Query: TSQLQuery;
  I: Integer;
begin
  Query := TDBController(AController).CreateQuery;
  try
    case NotificationType of
      ntReceivedCommand:
        begin
        Query.SQL.Text := 'insert into command(commandid, uid, state, receivedtime) values(:commandid, :uid, :state, :now)';
        Query.ParamByName('uid').AsString := VarToStr(UID);
        Query.ParamByName('state').AsString := 'R';
        end;
      ntFailedCommand, ntExecutedCommand:
        begin
        Query.SQL.Text := 'update command set state=:state, finishedtime=:now where (commandid=:commandid)';
        if NotificationType=ntFailedCommand then
          Query.ParamByName('state').AsString := 'F'
        else
          Query.ParamByName('state').AsString := 'C';
        end;
    end;
    Query.ParamByName('now').AsDateTime := now;
    Query.ParamByName('commandid').AsInteger := UniqueId;
    Query.ExecSQL;

    Query.SQL.Text := 'insert into commandlogline(commandid,loglevel,message) values (:commandid,:loglevel,:message)';
    Query.ParamByName('commandid').AsInteger := UniqueId;
    for I := 0 to LogLineList.Count -1 do
      begin
      Query.ParamByName('message').AsString := TLogLine(LogLineList.Items[i]).Msg;
      Query.ParamByName('loglevel').AsString := SLogLevel[TLogLine(LogLineList.Items[i]).LogLevel];
      Query.ExecSQL;
      end;
    TDBController(AController).FSQLTransaction.CommitRetaining;
  finally
    Query.Free;
  end;
  Result := True;
end;

class function TDBStoreCommmandNotification.TextName: string;
begin
  Result := 'StoreCommmandNotification';
end;

{ TDBController }
procedure TDBController.OnIdle;
begin
  inherited OnIdle;
  ProcessQueuedLogItems;
end;

procedure TDBController.ProcessQueuedLogItems;
var
  LockedList: TList;
  TempList: TList;
  LogEvent: TRepoLogEvent;
  NotificationEvent: TTestCommandNotificationEvent;
  Query: TSQLQuery;
  CommandQuery: TSQLQuery;
  I: Integer;
begin
  TempList := TList.Create;
  try
    LockedList := FLogLineList.LockList;
    try
      for i := 0 to LockedList.Count -1 do
        begin
        TempList.Assign(LockedList, laCopy);
        end;
      LockedList.Clear;
    finally
      FLogLineList.UnlockList;
    end;

    Query := CreateQuery;
    try
      Query.SQL.Text := 'insert into commandlogline(commandid,loglevel,message) values (:commandid,:loglevel,:message)';
      for I := 0 to TempList.Count-1 do
        begin
        if TRepoLogEvent(TempList.Items[i]).EventType = TDCSEventType.dcsetLog then
          begin
          LogEvent := TRepoLogEvent(TempList.Items[i]);
          if LogEvent.UniqueId > 0 then
            begin
            Query.ParamByName('commandid').AsInteger := LogEvent.UniqueId;
            Query.ParamByName('message').AsString := LogEvent.Message;
            Query.ParamByName('loglevel').AsString := SLogLevel[LogEvent.Level];
            Query.ExecSQL;
            end;
          end
        else
          begin
          NotificationEvent := TTestCommandNotificationEvent(TempList.Items[i]);

          CommandQuery := CreateQuery;
          try
            case NotificationEvent.NotificationType of
              ntReceivedCommand:
                begin
                CommandQuery.SQL.Text := 'insert into command(commandid, uid, state, receivedtime) values(:commandid, :uid, :state, :now)';
                CommandQuery.ParamByName('uid').AsString := VarToStr(NotificationEvent.UID);
                CommandQuery.ParamByName('state').AsString := 'R';
                end;
              ntFailedCommand, ntExecutedCommand:
                begin
                CommandQuery.SQL.Text := 'update command set state=:state, finishedtime=:now where (commandid=:commandid)';
                if NotificationEvent.NotificationType=ntFailedCommand then
                  CommandQuery.ParamByName('state').AsString := 'F'
                else
                  CommandQuery.ParamByName('state').AsString := 'C';
                end;
            end;
            CommandQuery.ParamByName('now').AsDateTime := now;
            CommandQuery.ParamByName('commandid').AsInteger := NotificationEvent.UniqueId;
            CommandQuery.ExecSQL;
          finally
            CommandQuery.Free;
          end;
          end;
        TDCSEvent(TempList.Items[i]).Release;
        end;
      FSQLTransaction.CommitRetaining;
    finally
      Query.Free;
    end;
  finally
    TempList.Free;
  end;
end;

procedure TDBController.InitListener(AListenerId: Integer);
begin
  FListenerId := AListenerId;
end;

function TDBController.GetListenerId: Integer;
begin
  Result := FListenerId;
end;

constructor TDBController.Create(ADistributor: TDCSDistributor);
begin
  inherited Create(ADistributor);
  ADistributor.AddListener(Self);
  ADistributor.SetLogEventsForListener(Self, reAll, [etCustom,etInfo,etWarning,etError,etDebug]);
  ADistributor.SetNotificationEventsForListener(Self, reAll, [ntReceivedCommand, ntExecutedCommand, ntFailedCommand]);
  FLogLineList := TThreadList.Create;
  InitDb('localhost:fppkg','sysdba','masterkey');
end;

function TDBController.AcceptCommand(ACommand: TDCSThreadCommand): Boolean;
begin
  Result := ACommand is TDBCommand;
end;

procedure TDBController.InitDb(DBName, DBUser, DBPassword: string);
begin
  FSQLConnection := TIBConnection.Create(nil);
  FSQLTransaction := TSQLTransaction.Create(nil);
  FSQLTransaction.DataBase := FSQLConnection;

  FSQLConnection.DatabaseName := DBName;
  FSQLConnection.UserName := DBUser;
  FSQLConnection.Password := DBPassword;
  FSQLConnection.Open;
end;

function TDBController.CreateQuery: TSQLQuery;
begin
  Result := TSQLQuery.Create(nil);
  Result.DataBase := FSQLConnection;
end;

destructor TDBController.Destroy;
begin
  FDistributor.RemoveListener(Self);
  // Process all events which are still in the queue.
  ProcessQueuedLogItems;
  FLogLineList.Free;
  FSQLTransaction.Free;
  FSQLConnection.Free;
  inherited Destroy;
end;

procedure TDBController.SendEvent(AnEvent: TDCSEvent);
begin
  if (AnEvent is TRepoLogEvent) or (AnEvent is TTestCommandNotificationEvent) then
    begin
    AnEvent.AddRef;
    FLogLineList.Add(AnEvent);
    end;
end;

function TDBController.GetOrigin: string;
begin
  Result := 'DBConnector';
end;

{ TDBGetUniqueIdCommand }

function TDBGetUniqueIdCommand.DoExecute(AController: TDCSCustomController; out
  ReturnMessage: string): Boolean;
var
  Query: TSQLQuery;
begin
  Query := TDBController(AController).CreateQuery;
  try
    Query.SQL.Text := 'select gen_id(gen_command_id,1) from rdb$database;';
    Query.Open;
    ReturnMessage := Query.Fields[0].AsString;
  finally
    Query.Free;
  end;
  Result := True;
end;

class function TDBGetUniqueIdCommand.TextName: string;
begin
  Result := 'uniqueid';
end;

initialization
  TDCSThreadCommandFactory.RegisterCommandClass(TDBQueryTestResultCommand);
end.

