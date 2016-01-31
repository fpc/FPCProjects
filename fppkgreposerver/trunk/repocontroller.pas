unit RepoController;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  IniFiles,
  dcsHandler,
  pkgglobals,
  pkgoptions,
  pkghandler,
  pkgrepos;

type

  { TCommandExecutioner }

  TCommandExecutioner = class(TObject, IDCSListener)
  private
    FDistributor: TDCSDistributor;
    FListenerId: Integer;
    FRTLEvent: PRTLEvent;
    FEvent: TDCSNotificationEvent;
  protected
    procedure SendEvent(AnEvent: TDCSEvent);
    function GetOrigin: string;
    function GetListenerId: Integer;
    procedure InitListener(AListenerId: Integer);
  public
    constructor Create(ADistributor: TDCSDistributor);
    destructor Destroy; override;
    function ExecuteCommand(ACommand: TDCSThreadCommand; ATimeout: LongInt): TDCSNotificationEvent;
    property ListenerId: Integer read FListenerId;
  end;

  { TRepoController }

  TRepoController = class(TDCSCustomController)
  private
    FPublishedRepoDir: string;
    FRepoDir: String;
    FStartCompiler: String;
    FTestCompiler: String;
    FTargetString: String;
    FCompilerVersion: String;
    procedure LoadIniFile;
  public
    constructor Create(ADistributor: TDCSDistributor); override;
    function AcceptCommand(ACommand: TDCSThreadCommand): Boolean; override;
    procedure Init; override;
    procedure LoadRepository;
    property RepoDir: string read FRepoDir;
    property StartCompiler: string read FStartCompiler;
    property TargetString: string read FTargetString;
    property CompilerVersion: string read FCompilerVersion;
    property TestCompiler: string read FTestCompiler;
    property PublishedRepoDir: string read FPublishedRepoDir;
  end;

  { TRepoCommand }

  TRepoCommand = class(TDCSThreadCommand)
  protected
    function RemoveTree(APath: String): Boolean;
  public
    procedure AddToTestLog(ALevel: TLogLevel; AMsg: String); virtual;
    function DoExecuteRepoCommand(AController: TDCSCustomController; out ReturnMessage: string): Boolean; virtual;
    function DoExecute(AController: TDCSCustomController; out ReturnMessage: string): Boolean; override;
  end;

const
  SLogLevel: array[TLogLevel] of string = (
    'Error',
    'Warning',
    'Info',
    'Commands',
    'Debug',
    'Progres'
  );

implementation

uses
  RepoTestCommand;

{ TRepoCommand }

var
  GCommand: TRepoCommand;

procedure LogCmd(Level: TLogLevel; const Msg: String);
begin
  if not Assigned(GCommand) then
    Exit;
  GCommand.AddToTestLog(Level, Msg);
end;

{ TCommandExecutioner }

procedure TCommandExecutioner.SendEvent(AnEvent: TDCSEvent);
var
  Event: TDCSNotificationEvent;
begin
  if AnEvent is TDCSNotificationEvent then
    begin
    Event := TDCSNotificationEvent(AnEvent);
    if (Event.LisId=FListenerId) and (Event.NotificationType in [ntInvalidCommand, ntFailedCommand, ntExecutedCommand]) then
      begin
      FEvent := Event;
      FEvent.AddRef;
      RTLeventSetEvent(FRTLEvent);
      end;
    end;
end;

function TCommandExecutioner.GetOrigin: string;
begin
  Result := 'TCommandExecutioner';
end;

function TCommandExecutioner.GetListenerId: Integer;
begin
  result := FListenerId;
end;

procedure TCommandExecutioner.InitListener(AListenerId: Integer);
begin
  FListenerId := AListenerId;
end;

constructor TCommandExecutioner.Create(ADistributor: TDCSDistributor);
begin
  FDistributor := ADistributor;
  FDistributor.AddListener(Self);
  FRTLEvent := RTLEventCreate;
end;

destructor TCommandExecutioner.Destroy;
begin
  FDistributor.RemoveListener(Self);
  RTLeventdestroy(FRTLEvent);
  inherited Destroy;
end;

function TCommandExecutioner.ExecuteCommand(ACommand: TDCSThreadCommand; ATimeout: LongInt): TDCSNotificationEvent;
begin
  FEvent := Nil;
  RTLeventResetEvent(FRTLEvent);
  FDistributor.QueueCommand(ACommand);
  RTLeventWaitFor(FRTLEvent, ATimeout);
  Result := FEvent;
end;

function TRepoCommand.RemoveTree(APath: String): Boolean;
var
{$ifdef MSWINDOWS}
  SHFileOpStruct: TSHFileOpStruct;
  DirBuf: array[0..MAX_PATH+1] of TCHAR;
{$else MSWINDOWS}
  searchRec: TSearchRec;
  SearchResult: longint;
  s: string;
{$endif MSWINDOWS}

begin
  result := true;
{$ifdef MSWINDOWS}
  try
    FillChar(SHFileOpStruct, Sizeof(SHFileOpStruct), 0);
    FillChar(DirBuf, Sizeof(DirBuf), 0);
    StrPCopy(DirBuf, APath);
    with SHFileOpStruct do
    begin
      pFrom := @DirBuf;
      wFunc := FO_DELETE;
      fFlags := FOF_NOCONFIRMATION or FOF_SILENT;
    end;
    Result := SHFileOperation(SHFileOpStruct) = 0;
  except
    Result := False;
  end;
{$else MSWINDOWS}
  SearchResult := FindFirst(IncludeTrailingPathDelimiter(APath)+AllFilesMask, faAnyFile+faSymLink, searchRec);
  try
    while SearchResult=0 do
      begin
        if (searchRec.Name<>'.') and (searchRec.Name<>'..') then
           begin
             s := IncludeTrailingPathDelimiter(APath)+searchRec.Name;
             if (searchRec.Attr and faDirectory)=faDirectory then
               begin
                 if not RemoveTree(s) then
                   result := false;
               end
             else if not DeleteFile(s) then
               result := False;
           end;
        SearchResult := FindNext(searchRec);
      end;
  finally
    FindClose(searchRec);
  end;

  // There were reports of RemoveDir failing due to locking-problems. To solve
  // these the RemoveDir is tried three times, with a delay of 5 seconds. See
  // bug 21868
  result := RemoveDir(APath);
{$endif WINDOWS}
end;

procedure TRepoCommand.AddToTestLog(ALevel: TLogLevel; AMsg: String);
begin
  FDistributor.Log(AMsg, etInfo, FUID);
end;

function TRepoCommand.DoExecuteRepoCommand(AController: TDCSCustomController; out ReturnMessage: string): Boolean;
begin
  Result := True;
  ReturnMessage := '';
end;

function TRepoCommand.DoExecute(AController: TDCSCustomController; out ReturnMessage: string): Boolean;
begin
  GCommand := Self;
  LogHandler := @LogCmd;
  try
    Result := DoExecuteRepoCommand(AController, ReturnMessage);
  finally
    LogHandler := nil;
    GCommand := nil;
  end;
end;

{ TRepoController }

procedure TRepoController.LoadIniFile;
var
  IniFile: TIniFile;
  CfgFile: String;
begin
  CfgFile:=ChangeFileExt(ParamStr(0), '.ini');
  IniFile := TIniFile.Create(CfgFile);
  try
    FRepoDir := IncludeTrailingPathDelimiter(ExpandFileName(IniFile.ReadString('Settings','repodir','repotest')));
    FStartCompiler := ExpandFileName(IniFile.ReadString('Settings','startcompiler','ppc386'+ExeExt));
    FTestCompiler := IniFile.ReadString('Settings','testcompiler','ppc386'+ExeExt);
    FTargetString := IniFile.ReadString('Settings','targetstring','i386-win32');
    FCompilerVersion := IniFile.ReadString('Settings','compilerversion','3.0.0');
    FPublishedRepoDir := IncludeTrailingPathDelimiter(ExpandFileName(IniFile.ReadString('Settings','publishedrepodir','repo')));
  finally
    IniFile.Free;
  end;
end;

constructor TRepoController.Create(ADistributor: TDCSDistributor);
begin
  inherited Create(ADistributor);
end;

function TRepoController.AcceptCommand(ACommand: TDCSThreadCommand): Boolean;
begin
  Result := (ACommand is TRepoCommand) or (ACommand is TRepoQuitCommand);
end;

procedure TRepoController.Init;
begin
  LoadIniFile;

  LogLevels:=DefaultLogLevels;
  LoadRepository;
end;

procedure TRepoController.LoadRepository;
var
  F: string;
begin
  F := FRepoDir+'etc/fppkg.cfg';
  if FileExists(F) then
    begin
    pkgoptions.LoadGlobalDefaults(F);
    LoadCompilerDefaults;

    FPMakeCompilerOptions.CheckCompilerValues;
    CompilerOptions.CheckCompilerValues;

    LoadLocalAvailableRepository;
    FindInstalledPackages(CompilerOptions);
    CheckFPMakeDependencies;

    LoadLocalAvailableMirrors;

    ClearExecutedAction;
    end;
end;

end.

