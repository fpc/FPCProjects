unit baCommand;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  process,
  dcsHandler,
  dcsGlobalSettings;

type

  { TbaCustomCommand }

  TbaCustomCommand = class(TDCSThreadCommand)
  private
    FCPUTarget: string;
    FFPCVersion: string;
    FOSTarget: string;
    function GetPathSetting(ASettingKey: string): string;
  protected
    function GetFPCSourcePath: string;
    function GetStartCompiler: string;
    function GetPristineEnvironmentPath: string;
    function GetFppkgExecutable: string;
    function GetBuildPath: string;
    function GetFPCEnvironmentKey: string;
    function RunTestCommandIndir(const Curdir: string; const Exename: string;
      const Commands: array of string; TaskDescription: string; ExpectedExitStatus: Integer = 0): string;

    property OSTarget: string read FOSTarget write FOSTarget;
    property CPUTarget: string read FCPUTarget write FCPUTarget;
    property FPCVersion: string read FFPCVersion write FFPCVersion;
  end;


implementation

{ TbaCustomCommand }

function TbaCustomCommand.RunTestCommandIndir(const Curdir:string; const Exename:string; const Commands:array of string; TaskDescription: string; ExpectedExitStatus: Integer = 0):string;
var
  CommandOutput: string;
  i: integer;
  CommandLine: string;
  ExitStatus: Integer;
begin
  if RunCommandInDir(Curdir, Exename, Commands, CommandOutput, ExitStatus, [poStderrToOutPut]) <> 0 then
    raise Exception.CreateFmt('Failed to run ''%s''', [exename]);
  if ExitStatus<>ExpectedExitStatus then
    begin
    for i := 0 to length(Commands) -1 do
      begin
      CommandLine := CommandLine + ' ' + Commands[i];
      end;
    raise Exception.CreateFmt('Failed to %s.' +sLineBreak+ 'Current directory: ' +Curdir+ sLineBreak + 'command line: ' + Exename + CommandLine + sLineBreak + ' Output: ' + sLineBreak + CommandOutput, [TaskDescription]);
    end;
  result := CommandOutput;
end;


function TbaCustomCommand.GetPathSetting(ASettingKey: string): string;
var
  GlobalSettings: TDCSGlobalSettings;
begin
  GlobalSettings := TDCSGlobalSettings.GetInstance;
  Result :=  GlobalSettings.GetSettingAsStringByKey(GetFPCEnvironmentKey, ASettingKey);
  if Result='' then
    raise Exception.Create(ASettingKey + ' not provided')
  else
    Result := ExpandFileName(Result);
end;

function TbaCustomCommand.GetFPCSourcePath: string;
begin
  Result := GetPathSetting('FPCSourcePath');
end;

function TbaCustomCommand.GetStartCompiler: string;
var
  GlobalSettings: TDCSGlobalSettings;
begin
  GlobalSettings := TDCSGlobalSettings.GetInstance;
  Result := GlobalSettings.GetSettingAsStringByKey(GetFPCEnvironmentKey, 'StartCompiler');
end;

function TbaCustomCommand.GetPristineEnvironmentPath: string;
begin
  Result := GetPathSetting('PristineEnvironmentPath');
end;

function TbaCustomCommand.GetFppkgExecutable: string;
begin
  Result := ConcatPaths([GetBuildPath, 'bin', 'fppkg']);
end;

function TbaCustomCommand.GetBuildPath: string;
begin
  Result := GetPathSetting('BuildPath');
end;

function TbaCustomCommand.GetFPCEnvironmentKey: string;
begin
  if (FOSTarget='') or (FCPUTarget='') or (FPCVersion='') then
    raise Exception.Create('OSTarget, CPUTarget and FPCVersion are mandatory parameters');

  Result := 'TestEnv-' + FCPUTarget + '-' + FOSTarget + '-' + FPCVersion;
end;

end.

