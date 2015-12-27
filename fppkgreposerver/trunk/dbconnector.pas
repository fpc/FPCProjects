unit DBConnector;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  syncobjs,
  sqldb,
  variants,
  IBConnection,
  lazCollections,
  dcsHandler,
  RepoTestCommand;

type

  TThreadedQueueEvent = specialize TLazThreadedQueue<TDCSEvent>;

  { TDbConnector }

  TDbConnector = class(TThread, IDCSListener)
  private
    FDBName: string;
    FDBUser: string;
    FDBPassword: string;
    FLisId: Integer;
    FResponseQueue: TThreadedQueueEvent;
    FDistributor: TDCSDistributor;
    FSQLConnection: TIBConnection;
    FSQLTransaction: TSQLTransaction;
  protected
    procedure Execute; override;
  public
    constructor create(ADistributor: TDCSDistributor; DBName, DBUser,DBPassword: string);
    destructor Destroy; override;

    // IDCSListener
    procedure SendEvent(AnEvent: TDCSEvent);
    function GetOrigin: string;
  end;

implementation


{ TDbConnector }

procedure TDbConnector.Execute;
var
  Event: TDCSEvent;
  Query: TSQLQuery;
begin
  Query := TSQLQuery.Create(nil);
  Query.DataBase := FSQLConnection;


  while not Terminated do
    begin
    if (FResponseQueue.PopItem(Event) = wrSignaled) then
      begin
      if Event is TDCSNotificationEvent then
        begin
        case TDCSNotificationEvent(Event).NotificationType of
          ntExecutedCommand, ntFailedCommand:
            begin
            Query.SQL.Text := 'update command set state = :state where uid=:uid';
            Query.ParamByName('uid').AsString := VarToStr(Event.UID);
            if TDCSNotificationEvent(Event).NotificationType = ntExecutedCommand then
              Query.ParamByName('state').AsString := 'C'
            else
              Query.ParamByName('state').AsString := 'F';
            Query.ParamByName('finishedtime').AsDateTime := Now;
            Query.ExecSQL;
            FSQLTransaction.CommitRetaining;

            if Event is TTestCommandNotificationEvent then
            begin
            end;

            end;
          ntReceivedCommand :
            begin
            Query.SQL.Text := 'insert into command(uid, state, receivedtime) values(:uid, :state, :receivedtime)';
            Query.ParamByName('uid').AsString := VarToStr(Event.UID);
            Query.ParamByName('state').AsString := 'R';
            Query.ParamByName('receivedtime').AsDateTime := Now;
            Query.ExecSQL;
            FSQLTransaction.CommitRetaining;
            end;
        end;
        end;
      end;
    end;
end;

constructor TDbConnector.create(ADistributor: TDCSDistributor; DBName, DBUser, DBPassword: string);
begin
  FDistributor := ADistributor;
  FDistributor.AddListener(Self);

  FSQLConnection := TIBConnection.Create(nil);
  FSQLTransaction := TSQLTransaction.Create(nil);
  FSQLTransaction.DataBase := FSQLConnection;

  FSQLConnection.DatabaseName := DBName;
  FSQLConnection.UserName := DBUser;
  FSQLConnection.Password := DBPassword;
  FSQLConnection.Open;

  FResponseQueue:=TThreadedQueueEvent.create(100, INFINITE, 100);
  FLisId := FDistributor.AddListener(self);

  inherited Create(false);
end;

destructor TDbConnector.Destroy;
begin
  FDistributor.RemoveListener(self);
  FResponseQueue.Free;

  FSQLTransaction.Free;
  FSQLConnection.Free;

  inherited Destroy;
end;

procedure TDbConnector.SendEvent(AnEvent: TDCSEvent);
begin
  //AnEvent.clone;
  if not VarIsNull(AnEvent.UID) then
    FResponseQueue.PushItem(AnEvent);
end;

function TDbConnector.GetOrigin: string;
begin
  result := 'DBConnector';
end;

end.

