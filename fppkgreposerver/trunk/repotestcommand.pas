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
  FileUtil,
  dcsThreadCommandFactory;

type

  { TLogLine }

  TLogLine = class
  private
    FLogLevel: TLogLevel;
    FMsg: string;
  public
    function Clone: TLogLine;
  published
    property Msg: string read FMsg write FMsg;
    property LogLevel: TLogLevel read FLogLevel write FLogLevel;
  end;

  // TFPGObjectList are not supported by the TJSONStreamer
  // TCustLogLineList = specialize TFPGObjectList<TLogLine>;
  TCustLogLineList = TObjectList;

  { TLogLineList }

  TLogLineList = class(TCustLogLineList)
  public
    procedure Clone(AFrom: TLogLineList);
  end;

  { TTestCommandNotificationEvent }

  TCustomTestCommandNotificationEvent = class(TDCSNotificationEvent)
  private
    FLogLineList: TLogLineList;
    FUniqueId: Integer;
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    property LogLineList: TLogLineList read FLogLineList;
    property UniqueId: Integer read FUniqueId write FUniqueId;
  end;

  TTestCommandNotificationEvent = class(TCustomTestCommandNotificationEvent);
  TrepoPackageSource = (rpsPublished, rpsUrl, rpsAdded);

  { TRepoTestCommand }

  TRepoTestCommand = class(TRepoCommand)
  private
    FPackageName: string;
    FPackageSource: TrepoPackageSource;
    FPackageURL: string;
    FUniqueId: Integer;
  protected
    function GetNotificationCommandEventClass: TDCSNotificationEventClass; override;
    function CreateExecutedCommandEvent(Success: Boolean; ReturnMessage: string; NotificationClass: TDCSNotificationEventClass): TDCSNotificationEvent; override;
    function CreateReceivedCommandEvent: TDCSNotificationEvent; override;
  public
    constructor Create(ASendByLisId: integer; AnUID: variant; ADistributor: TDCSDistributor); override;
    class function TextName: string; override;
    function DoExecuteRepoCommand(AController: TDCSCustomController; out ReturnMessage: string): Boolean; override;
    procedure AddToTestLog(ALevel: TLogLevel; AMsg: String); override;
    property UniqueId: Integer read FUniqueId;
  published
    property FpcVersionName;
    property TestEnvironmentName;
    property PackageName: string read FPackageName write FPackageName;
    property PackageURL: string read FPackageURL write FPackageURL;
    property PackageSource: TrepoPackageSource read FPackageSource write FPackageSource;
  end;

  { TRepoQuitCommand }

  TRepoQuitCommand = class(TDCSThreadCommand)
  public
    class function TextName: string; override;
    procedure PreExecute(AController: TDCSCustomController; out DoQueueCommand: boolean); override;
  end;

const
  SrepoPackageSource: array[TrepoPackageSource] of string = (
    'Published',
    'Url',
    'Added'
  );

implementation

uses
  DBConnector,
  RepoSvnCommands;

{ TLogLine }

function TLogLine.Clone: TLogLine;
begin
  Result := TLogLine.Create;
  Result.Msg := Msg;
  Result.LogLevel := LogLevel;
end;

{ TLogLineList }

procedure TLogLineList.Clone(AFrom: TLogLineList);
var
  I: Integer;
begin
  Clear;
  for I := 0 to AFrom.Count -1 do
    begin
    Add(TLogLine(AFrom.Items[I]).Clone);
    end;
end;

{ TTestCommandNotificationEvent }

constructor TCustomTestCommandNotificationEvent.Create;
begin
  inherited Create;
  FLogLineList := TLogLineList.Create(True);
end;

destructor TCustomTestCommandNotificationEvent.Destroy;
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
  Event := CreateExecutedCommandEvent(True, '', GetNotificationCommandEventClass);
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

function TRepoTestCommand.CreateExecutedCommandEvent(Success: Boolean; ReturnMessage: string; NotificationClass: TDCSNotificationEventClass): TDCSNotificationEvent;
var
  Event: TTestCommandNotificationEvent;
begin
  Result := inherited CreateExecutedCommandEvent(Success, ReturnMessage, NotificationClass);
  Event := TTestCommandNotificationEvent(Result);
  Event.UniqueId := UniqueId;
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

  FUniqueId := -1;
  CommandExecutioner := TCommandExecutioner.Create(FDistributor);
  try
    Command := TDBGetUniqueIdCommand.Create(CommandExecutioner.ListenerId, null, FDistributor);
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

class function TRepoTestCommand.TextName: string;
begin
  Result := 'test';
end;

function TRepoTestCommand.DoExecuteRepoCommand(AController: TDCSCustomController; out ReturnMessage: string): Boolean;
var
  Package: TFPPackage;
  FPCVersion: TrepoFPCVersion;
  TestEnv: TrepoTestEnvironment;
  SVNRepository: TSVNRepository;
  FTempDir, s, e, StoredPath: string;

  function InstallPackage: Boolean;
  begin
    Result := False;
    try
      pkghandler.ExecuteAction(Package.Name, 'install');
      pkghandler.ExecuteAction(Package.Name, 'fpmakeuninstall');
      Result := true;
    except
      on E: Exception do
        begin
        ReturnMessage := Format('Failed to install package %s: %s', [PackageName, E.Message]);
        FDistributor.Log(ReturnMessage, etWarning, UID, SendByLisId);
        end;
    end;
  end;

begin
  Result := False;
  Package := nil;

  if not GetTestEnvironment(AController, FPCVersion, TestEnv, ReturnMessage) then
    begin
    Exit;
    end;

  if not TRepoController(AController).LoadRepository(TestEnv.LocalDir+'etc/fppkg.cfg') then
    begin
    ReturnMessage := 'Failed to load repository';
    Exit;
    end;

  if (PackageURL<>'') then
  begin
    if (PackageName<>'') then
    begin
      AddToTestLog(llWarning, 'Packagename and PackageURL can not be used combined.');
      ReturnMessage := 'Test failed. Invalid parameters (Packagename and Packageurl).';
      Exit;
    end;

    Package := AvailableRepository.AddPackage(URLPackageName);
    Package.DownloadURL := PackageURL;
  end
  else
  begin
    if PackageSource=rpsAdded then
    begin
      SVNRepository := TSVNRepository.Create(FDistributor, AController as TRepoController, FUID);
      try
        if not SVNRepository.DoesPackageExist(PackageName,'') then
        begin
          ReturnMessage := Format('Package ''%s'' not found, it has not been added to the repository.', [PackageName]);
          AddToTestLog(llWarning, ReturnMessage);
          Exit;
        end;

        FTempDir := IncludeTrailingPathDelimiter(GetTempFilename(FTempDir, 'fppkg_'));
        CreateDir(FTempDir);
        try
          if SVNRepository.RunSvn(['co',TRepoController(AController).PackagesSvnUrl+PackageName+'/branche',FTempDir+'checkout'],s,e)=0 then
          begin
            Package := AvailableRepository.AddPackage(CurrentDirPackageName);
            StoredPath := GetCurrentDir;
            chdir(FTempDir+'checkout');
            try
              Result := InstallPackage;
            finally
              chdir(StoredPath);
            end;
          end
          else
            ReturnMessage := 'Failed to checkout package: '+e;
        finally
          DeleteDirectory(FTempDir, false);
        end;
      finally
        SVNRepository.Free;
      end;
    end
    else
      Package := AvailableRepository.FindPackage(PackageName);
  end;
  if Assigned(Package) then
  begin
    Result := InstallPackage;
  end
  else
  begin
    AddToTestLog(llWarning, 'Package "'+PackageName+'" not found.');
    ReturnMessage := Format('Test failed. Package "%s" not found.',[PackageName]);
  end;
end;

procedure TRepoTestCommand.AddToTestLog(ALevel: TLogLevel; AMsg: String);
var
  AnEvent: TRepoLogEvent;
  LogLine: TLogLine;
begin
  AnEvent := TRepoLogEvent.Create;
  try
    AnEvent.LogLevel := etCustom;
    AnEvent.Message := AMsg;
    AnEvent.UID := UID;
    AnEvent.LisId := SendByLisId;
    AnEvent.UniqueId := UniqueId;
    AnEvent.Level := ALevel;
    FDistributor.SendEvent(AnEvent);
  finally
    AnEvent.Release;
  end;
end;

initialization
  TDCSThreadCommandFactory.RegisterCommandClass(TRepoTestCommand);
  TDCSThreadCommandFactory.RegisterCommandClass(TRepoQuitCommand);
end.
