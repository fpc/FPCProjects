unit RepoTestCommand;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$M+}

interface

uses
  Classes,
  SysUtils,
  contnrs,
  dcsHandler,
  fpjson,
  CustApp,
  pkgrepos,
  fprepos,
  pkgglobals,
  pkghandler,
  pkgfpmake,
  pkgcommands,
  RepoController,
  dcsThreadCommandFactory;

type

  { TLogLine }

  TLogLine = class
  private
    FLogLevel: TLogLevel;
    FMsg: string;
  published
    property Msg: string read FMsg write FMsg;
    property LogLevel: TLogLevel read FLogLevel write FLogLevel;
  end;

  { TTestCommandNotificationEvent }

  TTestCommandNotificationEvent = class(TDCSNotificationEvent)
  private
    FLogLineList: TObjectList;
    FUniqueId: Integer;
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    property LogLineList: TObjectList read FLogLineList;
    property UniqueId: Integer read FUniqueId write FUniqueId;
  end;

  { TRepoTestCommand }

  TRepoTestCommand = class(TRepoCommand)
  private
    FPackageName: string;
    FLogLineList: TObjectList;
    FUniqueId: Integer;
  protected
    function GetNotificationCommandEventClass: TDCSNotificationEventClass; override;
    function CreateExecutedCommandEvent(Success: Boolean; ReturnMessage: string): TDCSNotificationEvent; override;
    function CreateReceivedCommandEvent: TDCSNotificationEvent; override;
  public
    constructor Create(ASendByLisId: integer; AnUID: variant; ADistributor: TDCSDistributor); override;
    destructor Destroy; override;
    class function TextName: string; override;
    function DoExecuteRepoCommand(AController: TDCSCustomController; out ReturnMessage: string): Boolean; override;
    procedure AddToTestLog(ALevel: TLogLevel; AMsg: String); override;
    property UniqueId: Integer read FUniqueId;
  published
    property PackageName: string read FPackageName write FPackageName;
  end;

  { TRepoQuitCommand }

  TRepoQuitCommand = class(TDCSThreadCommand)
  public
    class function TextName: string; override;
    procedure PreExecute(AController: TDCSCustomController; out DoQueueCommand: boolean); override;
  end;

implementation

uses
  DBConnector;

{ TTestCommandNotificationEvent }

constructor TTestCommandNotificationEvent.Create;
begin
  inherited Create;
  FLogLineList := TObjectList.Create(True);
end;

destructor TTestCommandNotificationEvent.Destroy;
begin
  FLogLineList.Free;
  inherited Destroy;
end;

{ TRepoQuitCommand }

class function TRepoQuitCommand.TextName: string;
begin
  Result := 'quit'
end;

procedure TRepoQuitCommand.PreExecute(AController: TDCSCustomController; out DoQueueCommand: boolean);
var
  Event: TDCSEvent;
begin
  Event := CreateReceivedCommandEvent;
  try
    FDistributor.SendEvent(Event);
  finally
    Event.Release;
  end;
  DoQueueCommand := False;
  CustomApplication.Terminate;
  Event := CreateExecutedCommandEvent(True, '');
  try
    FDistributor.SendEvent(Event);
  finally
    Event.Release;
  end;
end;

{ TRepoTestCommand }

function TRepoTestCommand.GetNotificationCommandEventClass: TDCSNotificationEventClass;
begin
  Result := TTestCommandNotificationEvent;
end;

function TRepoTestCommand.CreateExecutedCommandEvent(Success: Boolean; ReturnMessage: string): TDCSNotificationEvent;
var
  Event: TTestCommandNotificationEvent;
begin
  Result := inherited CreateExecutedCommandEvent(Success, ReturnMessage);
  Event := TTestCommandNotificationEvent(Result);
  Event.LogLineList.Assign(FLogLineList);
  Event.UniqueId := UniqueId;
  FLogLineList.OwnsObjects := False;
  FLogLineList.Clear;
end;

function TRepoTestCommand.CreateReceivedCommandEvent: TDCSNotificationEvent;
begin
  Result := inherited CreateReceivedCommandEvent;
  TTestCommandNotificationEvent(Result).UniqueId := UniqueId;
end;

constructor TRepoTestCommand.Create(ASendByLisId: integer; AnUID: variant; ADistributor: TDCSDistributor);
var
  CommandExecutioner: TCommandExecutioner;
  Command: TDBGetUniqueIdCommand;
  Event: TDCSNotificationEvent;
begin
  inherited Create(ASendByLisId, AnUID, ADistributor);
  FLogLineList := TObjectList.Create;

  FUniqueId := -1;
  CommandExecutioner := TCommandExecutioner.Create(FDistributor);
  try
    Command := TDBGetUniqueIdCommand.Create(CommandExecutioner.LisId, null, FDistributor);
    Event := CommandExecutioner.ExecuteCommand(Command, 5000);
    if Assigned(Event) then
      begin
      FUniqueId := StrToIntDef(Event.Message, -1);
      Event.Release;
      end;
  finally
    CommandExecutioner.Free;
  end;
end;

destructor TRepoTestCommand.Destroy;
begin
  FLogLineList.Free;
  inherited Destroy;
end;

class function TRepoTestCommand.TextName: string;
begin
  Result := 'test';
end;

function TRepoTestCommand.DoExecuteRepoCommand(AController: TDCSCustomController; out ReturnMessage: string): Boolean;
var
  Package: TFPPackage;
  Message: String;
begin
  Message := '';
  Result := False;
  Package := AvailableRepository.FindPackage(PackageName);
  if Assigned(Package) then
  begin
    try
      TRepoController(AController).Init;
      pkghandler.ExecuteAction(Package.Name, 'install');
      Result := true;
    except
      on E: Exception do
        begin
        ReturnMessage := Format('Failed to install package %s: %s', [PackageName, E.Message]);
        FDistributor.Log(Message, etWarning, UID);
        end;
    end;
  end
  else
  begin
    FDistributor.Log('Package "'+PackageName+'" not found.', etWarning, UID);
    Message := Format('Test failed. Package "%s" not found.',[PackageName]);
  end;
end;

procedure TRepoTestCommand.AddToTestLog(ALevel: TLogLevel; AMsg: String);
var
  LogLine: TLogLine;
begin
  LogLine := TLogLine.Create;
  LogLine.LogLevel := ALevel;
  LogLine.Msg := AMsg;
  FLogLineList.Add(LogLine);
end;

initialization
  TDCSThreadCommandFactory.RegisterCommandClass(TRepoTestCommand);
  TDCSThreadCommandFactory.RegisterCommandClass(TRepoQuitCommand);
end.
