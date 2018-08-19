unit baBuildFPCEnvironment;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  BaseUnix,
  dcsHandler,
  dcsThreadCommandFactory,
  dcsGlobalSettings,
  fprDeleteTree,
  fprCopyTree,
  baCommand;

type

  { TbaBuildFPCEnvironment }

  TbaBuildFPCEnvironment = class(TbaCustomCommand)
  private
    function GetAdditionalPackages: TStringArray;
  public
    class function TextName: string; override;
    function DoExecute(AController: TDCSCustomController; out ReturnMessage: string): Boolean; override;
  published
    property OSTarget;
    property CPUTarget;
    property FPCVersion;
  end;

implementation

{ TbaBuildFPCEnvironment }

class function TbaBuildFPCEnvironment.TextName: string;
begin
  Result := 'buildfpcenvironment';
end;

function TbaBuildFPCEnvironment.DoExecute(AController: TDCSCustomController; out ReturnMessage: string): Boolean;

  function RunSaveTestCommandIndir(const Curdir: string; const Exename: string;
    const Commands: array of string; TaskDescription: string; ExpectedExitStatus: Integer = 0; const TimeoutMS: Int64 = -1): Boolean;
  var
    CommandOutput: string;
    CommandLine: string;
  begin
    case TryRunTestCommandIndir(Curdir, Exename, Commands, TaskDescription, CommandOutput, CommandLine, ExpectedExitStatus, TimeoutMS) of
      barcrFailed: raise Exception.Create('Could not build FPC Environment.' +sLineBreak+ 'Current directory: ' +Curdir+ sLineBreak + 'command line: ' + CommandLine);
      barcrTimeout: Result := False;
      barcrWrongExitcode: raise Exception.Create('Failed to build FPC Environment.' +sLineBreak+ 'Current directory: ' +Curdir+ sLineBreak + 'command line: ' + CommandLine + sLineBreak + ' Output: ' + sLineBreak + CommandOutput);
      barcrSucces: Result := True;
    else
      raise Exception.Create('Invalid TestCommand result');
    end;
  end;

var
  LocalBasePath: string;
  MakeParams: array of string;
  FPCSourcePath, StartCompiler, PristineEnvironmentPath, BuildPath: string;
  CompilerVersion: string;
  CompilerBinary: string;
  Template: string;
  AddPackages: TStringArray;
  i, j: Integer;
  RetrySucceeded: Boolean;
begin
  FPCSourcePath := GetFPCSourcePath;

  StartCompiler := GetStartCompiler;

  PristineEnvironmentPath := GetPristineEnvironmentPath;

  BuildPath := GetBuildPath;
  FDistributor.Log('Clear pristine path', etInfo, Null, FSendByLisId);
  if DirectoryExists(PristineEnvironmentPath) and not DeleteTree(PristineEnvironmentPath, False) then
    raise Exception.CreateFmt('Failed to remove pristine-environment-path ''%s''', [PristineEnvironmentPath]);

  if not ForceDirectories(PristineEnvironmentPath) then
    raise Exception.CreateFmt('Failed to create pristine-environment-path ''%s''', [PristineEnvironmentPath]);

  FDistributor.Log('Start building FPC', etInfo, Null, FSendByLisId);

  SetLength(MakeParams, 2);
  MakeParams[0] := 'clean';
  MakeParams[1] := 'all';
  if StartCompiler<>'' then
    begin
    SetLength(MakeParams, length(MakeParams)+1);
    MakeParams[High(MakeParams)] := 'PP='+StartCompiler;
    end;

  RunTestCommandIndir(FPCSourcePath, 'make', MakeParams, 'compile FPC');

  FDistributor.Log('Start installing FPC', etInfo, Null, FSendByLisId);
  MakeParams[0] := 'install';
  MakeParams[1] := 'PREFIX='+PristineEnvironmentPath;
  RunTestCommandIndir(FPCSourcePath, 'make', MakeParams, 'install FPC');

  LocalBasePath :=  IncludeTrailingPathDelimiter(ConcatPaths([BuildPath, 'user','lib','fpc']));

  // How does this work on Windows?
  CompilerBinary := ExtractFileName(Trim(RunTestCommandIndir(PristineEnvironmentPath, PristineEnvironmentPath+'bin'+PathDelim+'fpc', ['-PB'], 'get the compiler executable-name')));
  CompilerVersion := Trim(RunTestCommandIndir(PristineEnvironmentPath, ConcatPaths([FPCSourcePath, 'compiler', CompilerBinary]), ['-iV'], 'get compiler-version'));
  fpSymlink(pchar(ConcatPaths(['..', 'lib', 'fpc', CompilerVersion, ExtractFileName(CompilerBinary)])), pchar(PristineEnvironmentPath+'bin'+PathDelim+ExtractFileName(CompilerBinary)));

  //ForceDirectories(LocalBasePath+CompilerVersion);

  FDistributor.Log('Create configuration files', etInfo, Null, FSendByLisId);

  SetLength(MakeParams, 8);
  MakeParams[0] := '-o';
  MakeParams[1] := PristineEnvironmentPath+PathDelim+'fpc.cfg';
  MakeParams[2] := '-d';
  MakeParams[3] := 'basepath='+ConcatPaths([BuildPath, 'lib','fpc','$fpcversion']);
  MakeParams[4] := '-d';
  MakeParams[5] := 'sharepath='+ConcatPaths([BuildPath, 'share','fpc','$fpcversion']);
  MakeParams[6] := '-d';
  MakeParams[7] := 'localbasepath='+LocalBasePath+'$fpcversion';
  Template := TDCSGlobalSettings.GetInstance.GetSettingAsStringByKey(GetFPCEnvironmentKey, 'FpcCfgTemplate');
  if Template<>'' then
    begin
    SetLength(MakeParams, 10);
    MakeParams[8] := '-t';
    MakeParams[9] := Template;
    end;
  RunTestCommandIndir(PristineEnvironmentPath, ConcatPaths([PristineEnvironmentPath,'bin', 'fpcmkcfg']), MakeParams, 'create fpc.cfg');

  SetLength(MakeParams, 12);
  MakeParams[1] := ConcatPaths([PristineEnvironmentPath, 'etc', 'fppkg.cfg']);
  MakeParams[3] := 'GlobalPath='+ConcatPaths([BuildPath, 'lib', 'fpc']);
  MakeParams[5] := 'GlobalPrefix='+BuildPath;
  MakeParams[8] := '-p';
  MakeParams[9] := '-d';
  MakeParams[10] := 'LocalRepository='+ConcatPaths([BuildPath, 'user'])+PathDelim;
  Template := TDCSGlobalSettings.GetInstance.GetSettingAsStringByKey(GetFPCEnvironmentKey, 'FppkgCfgTemplate');
  if Template<>'' then
    begin
    SetLength(MakeParams, 13);
    MakeParams[11] := '-t';
    MakeParams[12] := Template;
    end
  else
    begin
    MakeParams[11] := '-3';
    end;
  RunTestCommandIndir(PristineEnvironmentPath, ConcatPaths([PristineEnvironmentPath,'bin', 'fpcmkcfg']), MakeParams, 'create fppkg.cfg');

  SetLength(MakeParams, 14);
  MakeParams[1] := ConcatPaths([PristineEnvironmentPath, 'user', 'config', 'default']);
  MakeParams[3] := 'GlobalPath='+ConcatPaths([BuildPath, 'lib','fpc']);
  MakeParams[5] := 'fpcbin='+ConcatPaths([BuildPath, 'bin','fpc']);
  MakeParams[8] := '-p';
  MakeParams[9] := '-d';
  MakeParams[10] := 'LocalRepository='+ConcatPaths([BuildPath, 'user'])+PathDelim;
  // The GlobalPrefix is used by fpc 3.0.4, obsolete since then
  MakeParams[11] := '-d';
  MakeParams[12] := 'GlobalPrefix='+BuildPath;
  Template := TDCSGlobalSettings.GetInstance.GetSettingAsStringByKey(GetFPCEnvironmentKey, 'FppkgDefaultTemplate');
  if Template<>'' then
    begin
    SetLength(MakeParams, 15);
    MakeParams[13] := '-t';
    MakeParams[14] := Template;
    end
  else
    begin
    MakeParams[13] := '-4';
    end;
  RunTestCommandIndir(PristineEnvironmentPath, ConcatPaths([PristineEnvironmentPath,'bin', 'fpcmkcfg']), MakeParams, 'create default fppkg compiler file');

  ForceDirectories(ConcatPaths([PristineEnvironmentPath, 'user','config','conf.d']));

  AddPackages := GetAdditionalPackages;
  if Length(AddPackages) > 0 then
    begin
    PrepareBuildEnvironment(BuildPath, '', '');
    for i := 0 to High(AddPackages) do
      begin
      FDistributor.Log(Format('Install additional package [%s]',[AddPackages[i]]), etInfo, Null, FSendByLisId);
      RetrySucceeded := True;
      // FPPKG version 3.0.4 can get stuck in an infinitive loop. So kill fppkg after 60 seconds
      // A work-around is to run 'fppkg fixbroken' when fppkg failes until it succeeds
      if not RunSaveTestCommandIndir(BuildPath, GetFppkgExecutable, ['-C', ConcatPaths([BuildPath, 'etc', 'fppkg.cfg']), 'install', AddPackages[i]], 'install additional package', 0, 60000) then
        begin
        RetrySucceeded := False;
        for j := 0 to 5 do
          if RunSaveTestCommandIndir(BuildPath, GetFppkgExecutable, ['-C', ConcatPaths([BuildPath, 'etc', 'fppkg.cfg']), 'fixbroken'], 'Fix broken dependencies after installation of additional package', 0, 60000) then
            begin
            RetrySucceeded := True;
            Break;
            end;
        if not RetrySucceeded then
          raise Exception.Create('Failed to install additional package, even after 6 retries of running ''fppkg fixbroken''.');
        end;
      end;
    CopyTree(ConcatPaths([BuildPath, 'user', 'lib']), ConcatPaths([PristineEnvironmentPath, 'user', 'lib']));
    CopyTree(ConcatPaths([BuildPath, 'user', 'build']), ConcatPaths([PristineEnvironmentPath, 'user', 'lib']));
    end;

  ReturnMessage := 'Re-created pristine FPC-installation';
  Result := True;
end;

function TbaBuildFPCEnvironment.GetAdditionalPackages: TStringArray;
var
  GlobalSettings: TDCSGlobalSettings;
begin
  GlobalSettings := TDCSGlobalSettings.GetInstance;
  Result := GlobalSettings.GetSettingAsStringByKey(GetFPCEnvironmentKey, 'AdditionalPackages').Split([',',';']);
end;

initialization
  TDCSThreadCommandFactory.RegisterCommandClass(TbaBuildFPCEnvironment);
end.

