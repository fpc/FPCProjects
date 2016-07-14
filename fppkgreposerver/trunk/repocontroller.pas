unit RepoController;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  IniFiles,
  fgl,
  fpmkunit,
  dcsHandler,
  pkgglobals,
  pkgoptions,
  pkghandler,
  pkgrepos,
  DOM,
  XMLRead;

type

  { TRepoLogEvent }

  TRepoLogEvent = class( TDCSLogEvent )
  private
    FLevel: TLogLevel;
    FUniqueId: Integer;
  public
    property UniqueId: Integer read FUniqueId write FUniqueId;
    property Level: TLogLevel read FLevel write FLevel;
  end;

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

  { TrepoTestEnvironment }

  TrepoTestEnvironment = class
  private
    FCPUTarget: TCpu;
    FFppkgCfgTemplate: string;
    FLocalDir: string;
    FName: string;
    FOSTarget: TOS;
    FStartcompiler: string;
    FSVNUrl: string;
    FTestCompiler: string;
    FUninstallPackagesDuringInitialize: Boolean;
  published
    property LocalDir: string read FLocalDir write FLocalDir;
    property Startcompiler: string read FStartcompiler write FStartcompiler;
    property SVNUrl: string read FSVNUrl write FSVNUrl;
    property Name: string read FName write FName;
    property CPUTarget: TCpu read FCPUTarget write FCPUTarget;
    property OSTarget: TOS read FOSTarget write FOSTarget;
    property TestCompiler: string read FTestCompiler write FTestCompiler;
    property FppkgCfgTemplate: string read FFppkgCfgTemplate write FFppkgCfgTemplate;
    property UninstallPackagesDuringInitialize: Boolean read FUninstallPackagesDuringInitialize write FUninstallPackagesDuringInitialize;
  end;

  TrepoTestEnvironmentList = specialize TFPGObjectList<TrepoTestEnvironment>;

  { TRepoRepository }

  TRepoRepository = class
  private
    FName: string;
    FPublishDir: string;
    FSVNUrl: string;
    FTestEnvironmentName: string;
  published
    property Name: string read FName write FName;
    property SVNUrl: string read FSVNUrl write FSVNUrl;
    property PublishDir: string read FPublishDir write FPublishDir;
    property TestEnvironmentName: string read FTestEnvironmentName write FTestEnvironmentName;
  end;

  TrepoRepositoryList = specialize TFPGObjectList<TRepoRepository>;

  { TrepoFPCVersion }

  TrepoFPCVersion = class
  private
    FName: string;
    FRepositoryList: TrepoRepositoryList;
    FVersion: string;
    FTestEnvironmentList: TrepoTestEnvironmentList;
  public
    constructor Create;
    destructor Destroy; override;
    function GetTestEnvironment(AName: string): TrepoTestEnvironment;
    function GetRepository(AName: string): TRepoRepository;
  published
    property Name: string read FName write FName;
    property Version: string read FVersion write FVersion;
    property TestEnvironmentList: TrepoTestEnvironmentList read FTestEnvironmentList;
    property RepositoryList: TrepoRepositoryList read FRepositoryList write FRepositoryList;
  end;

  TrepoFPCVersionList = specialize TFPGObjectList<TrepoFPCVersion>;

  { TRepoController }

  TRepoController = class(TDCSCustomController)
  private
    FFPCVersionList: TrepoFPCVersionList;
    FPackagesSvnUrl: string;
    procedure LoadXmlConfigurationFile;
  public
    constructor Create(ADistributor: TDCSDistributor); override;
    destructor Destroy; override;
    function AcceptCommand(ACommand: TDCSThreadCommand): Boolean; override;

    // If no version-name is given, the default version is returned
    function GetFPCVersion(AFpcVersionName: string): TrepoFPCVersion;

    procedure Init; override;
    function LoadRepository(fppkgconfigname: string): Boolean;
    property PackagesSvnUrl: string read FPackagesSvnUrl;

    property FPCVersionList: TrepoFPCVersionList read FFPCVersionList;
  end;

  { TRepoCommand }

  TRepoCommand = class(TDCSThreadCommand)
  private
    FFpcVersionName: string;
    FRepositoryName: string;
    FTestEnvironmentName: string;
  protected
    function RemoveTree(APath: String): Boolean;
    function GetVersion(AController: TDCSCustomController; out FPCVersion: TrepoFPCVersion; out ReturnMessage: String): Boolean;
    function GetTestEnvironment(AController: TDCSCustomController; out FPCVersion: TrepoFPCVersion; out TestEnv: TrepoTestEnvironment; out ReturnMessage: String): Boolean;
    function GetRepository(AController: TDCSCustomController; out FPCVersion: TrepoFPCVersion; out Repository: TRepoRepository; out ReturnMessage: String): Boolean;

    property FpcVersionName: string read FFpcVersionName write FFpcVersionName;
    property TestEnvironmentName: string read FTestEnvironmentName write FTestEnvironmentName;
    property RepositoryName: string read FRepositoryName write FRepositoryName;
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
  RepoTestCommand,variants;

{ TRepoCommand }

var
  GCommand: TRepoCommand;

procedure LogCmd(Level: TLogLevel; const Msg: String);
begin
  if not Assigned(GCommand) then
    Exit;
  GCommand.AddToTestLog(Level, Msg);
end;

{ TrepoFPCVersion }

constructor TrepoFPCVersion.Create;
begin
  FTestEnvironmentList := TrepoTestEnvironmentList.Create;
  FRepositoryList := TrepoRepositoryList.Create;
end;

destructor TrepoFPCVersion.Destroy;
begin
  FTestEnvironmentList.Free;
  FRepositoryList.Free;
  inherited Destroy;
end;

function TrepoFPCVersion.GetTestEnvironment(AName: string): TrepoTestEnvironment;
var
  I: Integer;
  TestEnv: TrepoTestEnvironment;
begin
  Result := nil;
  for I := 0 to TestEnvironmentList.Count -1 do
    begin
    TestEnv := TestEnvironmentList.Items[I];
    if (TestEnv.Name = AName) or (AName='') then
      begin
      Result := TestEnv;
      Break;
      end;
    end;
end;

function TrepoFPCVersion.GetRepository(AName: string): TRepoRepository;
var
  I: Integer;
  Repository: TRepoRepository;
begin
  Result := nil;
  for I := 0 to RepositoryList.Count -1 do
    begin
    Repository := RepositoryList.Items[I];
    if (Repository.Name = AName) or (AName='') then
      begin
      Result := Repository;
      Break;
      end;
    end;
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

function TRepoCommand.GetVersion(AController: TDCSCustomController; out
  FPCVersion: TrepoFPCVersion; out ReturnMessage: String): Boolean;
begin
  FPCVersion := TRepoController(AController).GetFPCVersion(FpcVersionName);
  if not assigned(FPCVersion) then
    begin
    Result := False;
    ReturnMessage := Format('FPC-Version ''%s'' is not configured', [FpcVersionName]);
    end
  else
    Result := True;
end;

function TRepoCommand.GetTestEnvironment(AController: TDCSCustomController; out
  FPCVersion: TrepoFPCVersion; out TestEnv: TrepoTestEnvironment; out ReturnMessage: String): Boolean;
begin
  Result := GetVersion(AController, FPCVersion, ReturnMessage);
  if Result then
    begin
    TestEnv := FPCVersion.GetTestEnvironment(TestEnvironmentName);
    if not assigned(TestEnv) then
      begin
      ReturnMessage := Format('The test-environment ''%s'' is not configured', [TestEnvironmentName]);
      Result := False;
      Exit;
      end;
    end
  else
    TestEnv := nil;
end;

function TRepoCommand.GetRepository(AController: TDCSCustomController; out
  FPCVersion: TrepoFPCVersion; out Repository: TRepoRepository; out ReturnMessage: String): Boolean;
begin
  Result := GetVersion(AController, FPCVersion, ReturnMessage);
  if Result then
    begin
    Repository := FPCVersion.GetRepository(RepositoryName);
    if not assigned(Repository) then
      begin
      ReturnMessage := Format('The repository ''%s'' is not configured', [RepositoryName]);
      Result := False;
      Exit;
      end;
    end
  else
    Repository := nil;
end;

procedure TRepoCommand.AddToTestLog(ALevel: TLogLevel; AMsg: String);
var
  AnEvent: TRepoLogEvent;
begin
  AnEvent := TRepoLogEvent.Create;
  try
    AnEvent.LogLevel := etCustom;
    AnEvent.Level := ALevel;
    AnEvent.Message := AMsg;
    AnEvent.UID := UID;
    AnEvent.LisId := SendByLisId;
    FDistributor.SendEvent(AnEvent);
  finally
    AnEvent.Release;
  end;
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

procedure TRepoController.LoadXmlConfigurationFile;

  function GetNodeValue(AParentNode: TDOMNode; NodeName: string): string;
  var
    Node: TDOMNode;
  begin
    Node := AParentNode.FindNode(NodeName);
    if Assigned(Node) then
      Result := Node.TextContent
    else
      Result := '';
  end;

var
  CfgFile: String;
  XMLDocument: TXMLDocument;
  Node: TDOMNode;
  VersionElement, TestEnvElement, RepositoryElement: TDOMElement;
  FPCVersion: TrepoFPCVersion;
  VersionList, TestEnvList, RepositoryList: TDOMNodeList;
  i, j: Integer;
  TestEnv: TrepoTestEnvironment;
  Repository: TRepoRepository;
begin
  FFPCVersionList.Clear;
  CfgFile:=ChangeFileExt(ParamStr(0), '.xml');
  ReadXMLFile(XMLDocument, CfgFile);
  try
    Node := XMLDocument.FindNode('settings');
    if Assigned(Node) then
      begin
      FPackagesSvnUrl := GetNodeValue(Node, 'packagessvnurl');
      if FPackagesSvnUrl = '' then
        raise Exception.CreateFmt('The configuration file (%s) does not contain a package-svn url', [CfgFile]);
      if FPackagesSvnUrl[length(FPackagesSvnUrl)]<>'/' then
        FPackagesSvnUrl := FPackagesSvnUrl + '/';
      VersionList := (Node as TDOMElement).GetElementsByTagName('version');
      for i := 0 to VersionList.Count-1 do
        begin
        VersionElement := VersionList.Item[i] as TDOMElement;
        FPCVersion := TrepoFPCVersion.Create;
        FFPCVersionList.Add(FPCVersion);
        FPCVersion.Name := GetNodeValue(VersionElement,'name');
        FPCVersion.Version := GetNodeValue(VersionElement,'number');
        TestEnvList := VersionElement.GetElementsByTagName('testenvironment');
        for j := 0 to TestEnvList.Count-1 do
          begin
          TestEnvElement := TestEnvList.Item[j] as TDOMElement;
          TestEnv := TrepoTestEnvironment.Create;
          FPCVersion.TestEnvironmentList.Add(TestEnv);
          TestEnv.Name := GetNodeValue(TestEnvElement,'name');
          TestEnv.LocalDir := GetNodeValue(TestEnvElement,'localdir');
          TestEnv.LocalDir := ExpandFileName(TestEnv.LocalDir);
          if TestEnv.LocalDir <> '' then
            TestEnv.LocalDir := IncludeTrailingPathDelimiter(TestEnv.LocalDir);
          TestEnv.Startcompiler := GetNodeValue(TestEnvElement,'startcompiler');
          TestEnv.SVNUrl := GetNodeValue(TestEnvElement,'svnurl');
          TestEnv.CPUTarget := StringToCPU(GetNodeValue(TestEnvElement,'cpu-target'));
          TestEnv.OSTarget := StringToOS(GetNodeValue(TestEnvElement,'os-target'));
          TestEnv.TestCompiler := GetNodeValue(TestEnvElement,'testcompiler');
          TestEnv.FppkgCfgTemplate := GetNodeValue(TestEnvElement,'fppkgcfgtemplate');
          TestEnv.UninstallPackagesDuringInitialize := StrToBoolDef(GetNodeValue(TestEnvElement,'uninstallpackagesduringinitialize'), False);
          end;
        RepositoryList := VersionElement.GetElementsByTagName('repository');
        for j := 0 to RepositoryList.Count-1 do
          begin
          RepositoryElement := RepositoryList.Item[j] as TDOMElement;
          Repository := TRepoRepository.Create;
          FPCVersion.RepositoryList.Add(Repository);
          Repository.Name := GetNodeValue(RepositoryElement,'name');
          Repository.PublishDir := GetNodeValue(RepositoryElement,'publishdir');
          Repository.PublishDir := ExpandFileName(Repository.PublishDir);
          if Repository.PublishDir <> '' then
            Repository.PublishDir := IncludeTrailingPathDelimiter(Repository.PublishDir);
          Repository.TestEnvironmentName := GetNodeValue(RepositoryElement,'testenvironmentname');
          Repository.SVNUrl := GetNodeValue(RepositoryElement,'svnurl');
          end;
        end;
      end;
  finally
    XMLDocument.Free;
  end;
end;

constructor TRepoController.Create(ADistributor: TDCSDistributor);
begin
  inherited Create(ADistributor);
  FFPCVersionList := TrepoFPCVersionList.Create;
end;

destructor TRepoController.Destroy;
begin
  FFPCVersionList.Free;
  inherited Destroy;
end;

function TRepoController.AcceptCommand(ACommand: TDCSThreadCommand): Boolean;
begin
  Result := (ACommand is TRepoCommand) or (ACommand is TRepoQuitCommand);
end;

function TRepoController.GetFPCVersion(AFpcVersionName: string): TrepoFPCVersion;
var
  I: Integer;
  FPCVersion: TrepoFPCVersion;
begin
  Result := nil;
  for I := 0 to FFPCVersionList.Count -1 do
    begin
    FPCVersion := FPCVersionList.Items[I];
    if (FPCVersion.Name = AFpcVersionName) or (AFpcVersionName='') then
      begin
      Result := FPCVersion;
      Break;
      end;
    end;
end;

procedure TRepoController.Init;
begin
  LoadXmlConfigurationFile;

  LogLevels:=DefaultLogLevels;
end;

function TRepoController.LoadRepository(fppkgconfigname: string): Boolean;
begin
  if FileExists(fppkgconfigname) then
    begin
    pkgrepos.ClearRemoteRepository;
    pkgoptions.ClearCompilerDefaults;
    pkgoptions.LoadGlobalDefaults(fppkgconfigname);
    LoadCompilerDefaults;

    FPMakeCompilerOptions.CheckCompilerValues;
    CompilerOptions.CheckCompilerValues;

    LoadLocalAvailableRepository;
    FindInstalledPackages(CompilerOptions);
    CheckFPMakeDependencies;

    LoadLocalAvailableMirrors;

    ClearExecutedAction;
    Result := True;
    end
  else
    Result := False;
end;

end.

