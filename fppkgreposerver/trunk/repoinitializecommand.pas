unit repoinitializecommand;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  process,
  dcsHandler,
  fpjson,
  pkgrepos,
  pkgglobals,
  pkgoptions,
  fpmkunit,
  pkghandler,
  pkgfpmake,
  pkgcommands,
  RepoController,
  dcsThreadCommandFactory;

type

  { TRepoInitializeCommand }

  TRepoInitializeCommand = class(TRepoCommand)
  private
    function ExecuteProcess(ACmd: string; AParamList: array of const): boolean;
    procedure MaybeCreateLocalDirs;
  public
    class function TextName: string; override;
    function DoExecuteRepoCommand(AController: TDCSCustomController; out ReturnMessage: string): Boolean; override;
  published
    property FpcVersionName;
    property TestEnvironmentName;
  end;

  { TRepoUpdateCommand }

  TRepoUpdateCommand = class(TRepoCommand)
  public
    class function TextName: string; override;
    function DoExecuteRepoCommand(AController: TDCSCustomController; out ReturnMessage: string): Boolean; override;
  published
    property FpcVersionName;
    property TestEnvironmentName;
  end;

implementation

{ TRepoUpdateCommand }

class function TRepoUpdateCommand.TextName: string;
begin
  result := 'update';
end;

function TRepoUpdateCommand.DoExecuteRepoCommand(AController: TDCSCustomController; out ReturnMessage: string): Boolean;
var
  Message: String;
  FPCVersion: TrepoFPCVersion;
  TestEnv: TrepoTestEnvironment;

begin
  Result := False;

  if not GetTestEnvironment(AController, FPCVersion, TestEnv, ReturnMessage) then
    begin
    Exit;
    end;

  if not TRepoController(AController).LoadRepository(TestEnv.LocalDir+'etc/fppkg.cfg') then
    begin
    ReturnMessage := 'Failed to load repository';
    Exit;
    end;

  try
    pkghandler.ExecuteAction('', 'update');
    ClearExecutedAction;
    Result := true;
  except
    on E: Exception do
      begin
      ReturnMessage := Format('Failed to update: %s', [E.Message]);
      FDistributor.Log(Message, etWarning, UID);
      end;
  end;
end;

{ TRepoInitializeCommand }

function TRepoInitializeCommand.ExecuteProcess(ACmd: string; AParamList: array of const): boolean;
var
  P: TProcess;
  i: Integer;
  LogCmdLine: string;
  s: string;
begin
  result := False;
  P := TProcess.Create(nil);
  try
    LogCmdLine:=ACmd;
    P.Executable:=ACmd;
    for i := 0 to high(AParamList) do
      begin
      if AParamList[i].VType=vtAnsiString then
        begin
        s := ansistring(AParamList[i].VAnsiString);
        P.Parameters.Add(s);
        LogCmdLine := LogCmdLine + ' ' + s;
        end
      else
        raise exception.CreateFmt('parameter type %d not supported',[AParamList[i].VType]);
      end;
    P.Options:=[poWaitOnExit];

    FDistributor.Log('Executing command: '+LogCmdLine, etInfo, UID);

    P.Execute;
    result := P.ExitStatus=0;
  finally
    P.Free;
  end;
end;

class function TRepoInitializeCommand.TextName: string;
begin
  result := 'initialize';
end;

procedure TRepoInitializeCommand.MaybeCreateLocalDirs;
begin
  ForceDirectories(GlobalOptions.BuildDir);
  ForceDirectories(GlobalOptions.ArchivesDir);
  ForceDirectories(GlobalOptions.CompilerConfigDir);
end;

function TRepoInitializeCommand.DoExecuteRepoCommand(AController: TDCSCustomController; out ReturnMessage: string): Boolean;
var
  FpcmkcfgBin: string;
  FpcPath: string;
  FpccfgName: string;
  FpcBin: string;
  sr: TSearchRec;
  UnitDir: string;
  TargetString: string;
  FPCVersion: TrepoFPCVersion;
  TestEnv: TrepoTestEnvironment;
begin
  Result := False;

  if not GetTestEnvironment(AController, FPCVersion, TestEnv, ReturnMessage) then
    begin
    Exit;
    end;

  if not DirectoryExists(TestEnv.LocalDir) then
    begin
    FDistributor.Log(Format('Localdir (%s) does not exist. Create a new test-environment.', [TestEnv.LocalDir]), etInfo, UID);
    if not CreateDir(TestEnv.LocalDir) then
      begin
      ReturnMessage := Format('Failed to create localdir ().',[TestEnv.LocalDir]);
      Exit;
      end;
    SetCurrentDir(TestEnv.LocalDir);
    if not ExecuteProcess('svn'+pkgglobals.ExeExt,['checkout',TestEnv.SVNUrl,'fpcsrc']) then
      begin
      ReturnMessage := Format('Failed to checkout fpc (svn: %s, dir: %s)',[TestEnv.SVNUrl, TestEnv.LocalDir+'fpcsrc']);
      Exit;
      end;
    end;

  TargetString := MakeTargetString(TestEnv.CPUTarget,TestEnv.OSTarget);

  if not DirectoryExistsLog(TestEnv.LocalDir+'fpcsrc') then
    begin
    ReturnMessage := 'Not a valid repository-test directory: '+TestEnv.LocalDir;
    end
  else
    begin
    SetCurrentDir(TestEnv.LocalDir+'fpcsrc');
    if not ExecuteProcess('svn'+pkgglobals.ExeExt,['update']) then
      begin
      ReturnMessage := 'Faied to run svn update';
      Exit;
      end;
    if not ExecuteProcess('make'+pkgglobals.ExeExt, ['clean', 'all', 'PP='+TestEnv.Startcompiler, 'FPMAKEOPT=-T 4']) then
      begin
      ReturnMessage := 'Failed to compile fpc';
      Exit;
      end;
    RemoveTree(TestEnv.LocalDir+'fpc');
    RemoveTree(TestEnv.LocalDir+'fppkg');
    if not ExecuteProcess('make'+pkgglobals.ExeExt, ['install', 'PREFIX="'+TestEnv.LocalDir+'fpc"']) then
      begin
      ReturnMessage := 'Failed to install fpc';
      Exit;
      end;

    FpcmkcfgBin:=TestEnv.LocalDir+'fpc'+DirectorySeparator+'bin'+{$ifdef windows}DirectorySeparator+FTargetString+{$endif}DirectorySeparator+'fpcmkcfg'+pkgglobals.ExeExt;
    FpcPath:=TestEnv.LocalDir+'fpc';
    FpccfgName:=TestEnv.LocalDir+'fpc'+DirectorySeparator+'bin'+DirectorySeparator+MakeTargetString(TestEnv.CPUTarget, TestEnv.OSTarget)+DirectorySeparator+'fpc.cfg';
    FpcBin:=TestEnv.LocalDir+'fpc'+DirectorySeparator+{$ifdef unix}'lib'+DirectorySeparator+'fpc'+DirectorySeparator+FPCVersion.Version+DirectorySeparator+{$else}'bin'+DirectorySeparator+FTargetString+{$endif}DirectorySeparator+TestEnv.TestCompiler+pkgglobals.ExeExt;
    UnitDir:=TestEnv.LocalDir+'fpc'+DirectorySeparator+{$ifdef unix}'lib'+DirectorySeparator+'fpc'+DirectorySeparator+FPCVersion.Version+DirectorySeparator+{$endif}'units'+DirectorySeparator+TargetString+DirectorySeparator;

    if not ExecuteProcess(FpcmkcfgBin, ['-p', '-d "basepath='+FpcPath+'"', '-d "basepath='+FpcPath+'"', '-o', FpccfgName]) then
      raise exception.create('Failed to create fpc.cfg');
    if TestEnv.FppkgCfgTemplate <> '' then
      begin
      if not ExecuteProcess(FpcmkcfgBin, ['-p', '-t', TestEnv.FppkgCfgTemplate, '-d', 'LocalRepository='+TestEnv.LocalDir+'fppkg'+DirectorySeparator, '-o',TestEnv.LocalDir+'etc'+DirectorySeparator+'fppkg.cfg']) then
        raise exception.create('Failed to create fppkg.cfg');
      end
    else
      begin
      if not ExecuteProcess(FpcmkcfgBin, ['-p', '-3', '-d', 'LocalRepository='+TestEnv.LocalDir+'fppkg'+DirectorySeparator, '-o',TestEnv.LocalDir+'etc'+DirectorySeparator+'fppkg.cfg']) then
        raise exception.create('Failed to create fppkg.cfg');
      end;
    if not ExecuteProcess(FpcmkcfgBin, ['-p', '-4', '-d', 'GlobalPrefix='+FpcPath, '-d', 'FpcBin='+FpcBin, '-o', TestEnv.LocalDir+'fppkg'+DirectorySeparator+'config'+DirectorySeparator+'default']) then
      raise exception.create('Failed to create default');

    MaybeCreateLocalDirs;

    if TestEnv.UninstallPackagesDuringInitialize then
      begin
      RemoveTree(TestEnv.LocalDir+'fpc'+{$ifdef unix}DirectorySeparator+'lib'+DirectorySeparator+'fpc'+DirectorySeparator+FPCVersion.Version+{$endif}DirectorySeparator+'fpmkinst');
      if FindFirst(UnitDir+AllFiles, faDirectory, sr) = 0 then
        begin
        repeat
        if (sr.Name <> 'rtl') and (sr.Name <> '.') and (sr.Name <> '..') then
          begin
          RemoveTree(UnitDir+sr.Name);
          end;
        until FindNext(sr)<>0;
        end;
      end;

    Result := true;
    end;
end;

initialization
  TDCSThreadCommandFactory.RegisterCommandClass(TRepoInitializeCommand);
  TDCSThreadCommandFactory.RegisterCommandClass(TRepoUpdateCommand);
end.

