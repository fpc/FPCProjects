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
  RepoTestCommand;

type

  { TDbConnectorHandlerThread }

  TDbConnectorHandlerThread = class(TDCSHandlerThread, IDCSListener)
  private
    FDBName: string;
    FDBUser: string;
    FDBPassword: string;
    FListenerId: Integer;
  protected
    function CreateController: TDCSCustomController; override;
    // IDCSListener
    procedure InitListener(AListenerId: Integer);
    function GetListenerId: Integer;
  public
    constructor create(ADistributor: TDCSDistributor; DBName, DBUser,DBPassword: string);
    destructor Destroy; override;

    // IDCSListener
    procedure SendEvent(AnEvent: TDCSEvent);
    function GetOrigin: string;
  end;

  { TDBController }

  TDBController = class(TDCSCustomController)
  private
    FSQLConnection: TIBConnection;
    FSQLTransaction: TSQLTransaction;
  public
    function AcceptCommand(ACommand: TDCSThreadCommand): Boolean; override;
    procedure InitDb(DBName, DBUser,DBPassword: string);
    function CreateQuery: TSQLQuery;
    destructor Destroy; override;
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

implementation

uses
  RepoController;

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
  FSQLTransaction.Free;
  FSQLConnection.Free;
  inherited Destroy;
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

function TDbConnectorHandlerThread.CreateController: TDCSCustomController;
begin
  Result := inherited CreateController;
  TDBController(Result).InitDB(FDBName, FDBUser, FDBPassword);
end;

{ TDbConnectorHandlerThread }

constructor TDbConnectorHandlerThread.create(ADistributor: TDCSDistributor; DBName, DBUser, DBPassword: string);
begin
  inherited Create(ADistributor, TDBController);
  Distributor.AddListener(self);

  FDBName := DBName;
  FDBPassword :=  DBPassword;
  FDBUser := DBUser;
end;

destructor TDbConnectorHandlerThread.Destroy;
begin
  Distributor.RemoveListener(self);
  inherited Destroy;
end;

procedure TDbConnectorHandlerThread.SendEvent(AnEvent: TDCSEvent);
var
  Command: TDBStoreCommmandNotification;
  Event: TTestCommandNotificationEvent;
begin
  if AnEvent is TTestCommandNotificationEvent then
    begin
    Event := TTestCommandNotificationEvent(AnEvent);
    Command := TDBStoreCommmandNotification.Create(FListenerId, null, Distributor);
    Command.UniqueId := Event.UniqueId;
    Command.NotificationType := Event.NotificationType;
    Command.LogLineList.Clone(Event.LogLineList);
    Distributor.QueueCommand(Command);
    end;
end;

function TDbConnectorHandlerThread.GetOrigin: string;
begin
  result := 'DBConnector';
end;

procedure TDbConnectorHandlerThread.InitListener(AListenerId: Integer);
begin
  FListenerId := AListenerId;
end;

function TDbConnectorHandlerThread.GetListenerId: Integer;
begin
  Result := FListenerId;
end;

end.

