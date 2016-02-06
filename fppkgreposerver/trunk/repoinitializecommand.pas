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
  end;

  { TRepoUpdateCommand }

  TRepoUpdateCommand = class(TRepoCommand)
  public
    class function TextName: string; override;
    function DoExecuteRepoCommand(AController: TDCSCustomController; out ReturnMessage: string): Boolean; override;
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
begin
  Result := False;

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
  RepoDir: string;
  StartCompiler: string;
  TargetString: string;
  CompilerVersion: string;
  TestCompiler: string;
begin
  Result := False;
  RepoDir := TRepoController(AController).RepoDir;
  StartCompiler := TRepoController(AController).StartCompiler;
  TargetString := TRepoController(AController).TargetString;
  CompilerVersion := TRepoController(AController).CompilerVersion;
  TestCompiler := TRepoController(AController).TestCompiler;

  if not DirectoryExistsLog(RepoDir+'fpcsrc') then
    begin
    FDistributor.Log('Not a valid repository-test directory: '+RepoDir, etWarning, UID);
    end
  else
    begin
    SetCurrentDir(RepoDir+'fpcsrc');
    if not ExecuteProcess('svn'+ExeExt,['update']) then
      raise exception.create('Failed to run svn update');
    if not ExecuteProcess('make'+ExeExt, ['clean', 'all', 'PP='+StartCompiler, 'FPMAKEOPT=-T 4']) then
      raise exception.create('Failed to compile fpc');
    RemoveTree(RepoDir+'fpc');
    RemoveTree(RepoDir+'fppkg');
    if not ExecuteProcess('make'+ExeExt, ['install', 'PREFIX="'+RepoDir+'fpc"']) then
      raise exception.create('Failed to install fpc');

    FpcmkcfgBin:=RepoDir+'fpc'+DirectorySeparator+'bin'+{$ifdef windows}DirectorySeparator+FTargetString+{$endif}DirectorySeparator+'fpcmkcfg'+ExeExt;
    FpcPath:=RepoDir+'fpc';
    FpccfgName:=RepoDir+'fpc'+DirectorySeparator+'bin'+DirectorySeparator+TargetString+DirectorySeparator+'fpc.cfg';
    FpcBin:=RepoDir+'fpc'+DirectorySeparator+{$ifdef unix}'lib'+DirectorySeparator+'fpc'+DirectorySeparator+CompilerVersion+DirectorySeparator+{$else}'bin'+DirectorySeparator+FTargetString+{$endif}DirectorySeparator+TestCompiler+ExeExt;
    UnitDir:=RepoDir+'fpc'+DirectorySeparator+{$ifdef unix}'lib'+DirectorySeparator+'fpc'+DirectorySeparator+CompilerVersion+DirectorySeparator+{$endif}'units'+DirectorySeparator+TargetString+DirectorySeparator;

    if not ExecuteProcess(FpcmkcfgBin, ['-p', '-d "basepath='+FpcPath+'"', '-d "basepath='+FpcPath+'"', '-o', FpccfgName]) then
      raise exception.create('Failed to create fpc.cfg');
    if not ExecuteProcess(FpcmkcfgBin, ['-p', '-3', '-d', 'LocalRepository='+RepoDir+'fppkg'+DirectorySeparator, '-o',RepoDir+'etc'+DirectorySeparator+'fppkg.cfg']) then
      raise exception.create('Failed to create fppkg.cfg');
    if not ExecuteProcess(FpcmkcfgBin, ['-p', '-4', '-d', 'GlobalPrefix='+FpcPath, '-d', 'FpcBin='+FpcBin, '-o', RepoDir+'fppkg'+DirectorySeparator+'config'+DirectorySeparator+'default']) then
      raise exception.create('Failed to create default');

    MaybeCreateLocalDirs;

    if TRepoController(AController).UninstallPackagesDuringInitialize then
      begin
      RemoveTree(RepoDir+'fpc'+{$ifdef unix}DirectorySeparator+'lib'+DirectorySeparator+'fpc'+DirectorySeparator+CompilerVersion+{$endif}DirectorySeparator+'fpmkinst');
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

    TRepoController(AController).LoadRepository;

    Result := true;
    end;
end;

initialization
  TDCSThreadCommandFactory.RegisterCommandClass(TRepoInitializeCommand);
  TDCSThreadCommandFactory.RegisterCommandClass(TRepoUpdateCommand);
end.

