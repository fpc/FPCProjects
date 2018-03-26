unit baBuildFPCEnvironment;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  BaseUnix,
  dcsHandler,
  dcsThreadCommandFactory,
  fprDeleteTree,
  baCommand;

type

  { TbaBuildFPCEnvironment }

  TbaBuildFPCEnvironment = class(TbaCustomCommand)
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
var
  LocalBasePath: string;
  MakeParams: array of string;
  FPCSourcePath, StartCompiler, PristineEnvironmentPath, BuildPath: string;
  CompilerVersion: string;
  CompilerBinary: string;
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
  RunTestCommandIndir(PristineEnvironmentPath, ConcatPaths([PristineEnvironmentPath,'bin', 'fpcmkcfg']), MakeParams, 'create fpc.cfg');

  SetLength(MakeParams, 12);
  MakeParams[1] := ConcatPaths([PristineEnvironmentPath, 'etc', 'fppkg.cfg']);
  MakeParams[8] := '-3';
  MakeParams[9] := '-p';
  MakeParams[3] := 'GlobalPath='+ConcatPaths([BuildPath, 'lib', 'fpc']);
  MakeParams[5] := 'GlobalPrefix='+BuildPath;
  MakeParams[10] := '-d';
  MakeParams[11] := 'LocalRepository='+ConcatPaths([BuildPath, 'user'])+PathDelim;
  RunTestCommandIndir(PristineEnvironmentPath, ConcatPaths([PristineEnvironmentPath,'bin', 'fpcmkcfg']), MakeParams, 'create fppkg.cfg');


  SetLength(MakeParams, 12);
  MakeParams[1] := ConcatPaths([PristineEnvironmentPath, 'user', 'config', 'default']);
  MakeParams[8] := '-4';
  MakeParams[9] := '-p';
  MakeParams[3] := 'GlobalPath='+ConcatPaths([BuildPath, 'lib','fpc']);
  MakeParams[5] := 'fpcbin='+ConcatPaths([BuildPath, 'bin','fpc']);
  MakeParams[10] := '-d';
  MakeParams[11] := 'LocalRepository='+ConcatPaths([BuildPath, 'user'])+PathDelim;
  RunTestCommandIndir(PristineEnvironmentPath, ConcatPaths([PristineEnvironmentPath,'bin', 'fpcmkcfg']), MakeParams, 'create default fppkg compiler file');

  ForceDirectories(ConcatPaths([PristineEnvironmentPath, 'user','config','conf.d']));

  ReturnMessage := 'Re-created pristine FPC-installation';
  Result := True;
end;

initialization
  TDCSThreadCommandFactory.RegisterCommandClass(TbaBuildFPCEnvironment);
end.

