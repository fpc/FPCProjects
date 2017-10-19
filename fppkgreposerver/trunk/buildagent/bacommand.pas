unit baCommand;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  process,
  dcsHandler,
  DCSHTTPRestServer,
  HTTPDefs,
  cnocOpenIDConnect,
  dcsGlobalSettings;

type

  { TbaCustomCommand }

  TbaCustomCommand = class(TDCSThreadCommand, IDCSHTTPCommand)
  private
    FCPUTarget: string;
    FFPCVersion: string;
    FOSTarget: string;
    FAuthorizationToken: string;
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
  public
    procedure PreExecute(AController: TDCSCustomController; out DoQueueCommand: boolean); override;
    procedure FillCommandBasedOnRequest(ARequest: TRequest); virtual;
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
  CommandLine := Exename;
  for i := 0 to length(Commands) -1 do
    CommandLine := CommandLine + ' ' + Commands[i];
  FDistributor.Log(Format('Run external command.' +sLineBreak+ 'Directory: "%s".' +sLineBreak+ 'Commandstring: "%s".', [Curdir, CommandLine]), etDebug, Null, FSendByLisId);
  if RunCommandInDir(Curdir, Exename, Commands, CommandOutput, ExitStatus, [poStderrToOutPut]) <> 0 then
    raise Exception.CreateFmt('Failed to run ''%s''', [exename]);
  if ExitStatus<>ExpectedExitStatus then
    begin
    raise Exception.CreateFmt('Failed to %s.' +sLineBreak+ 'Current directory: ' +Curdir+ sLineBreak + 'command line: ' + CommandLine + sLineBreak + ' Output: ' + sLineBreak + CommandOutput, [TaskDescription]);
    end;
  result := CommandOutput;
  FDistributor.Log(Format('External command output.' +sLineBreak+ '%s', [Result]), etDebug, Null, FSendByLisId);
end;

procedure TbaCustomCommand.PreExecute(AController: TDCSCustomController; out DoQueueCommand: boolean);
var
  s: string;
  OIDC: TcnocOpenIDConnect;
begin
  inherited PreExecute(AController, DoQueueCommand);
  if DoQueueCommand then
    begin
    OIDC := TcnocOpenIDConnect.Create;
    try
      OIDC.OpenIDProvider := TDCSGlobalSettings.GetInstance.GetSettingAsString('OpenIDProviderURL');

      if copy(FAuthorizationToken, 1, 7) = 'Bearer ' then
        begin
        s := copy(FAuthorizationToken, 8, length(FAuthorizationToken)-7);
        DoQueueCommand := OIDC.VerifyJWT(s);
        if not DoQueueCommand then
          FDistributor.SendNotification(SendByLisId, ntFailedCommand, UID, 'Access denied: '+OIDC.GetLatestError, TextName, [TextName]);
        end
      else
        begin
        DoQueueCommand := False;
        FDistributor.SendNotification(SendByLisId, ntFailedCommand, UID, 'Access denied.', TextName, [TextName]);
        end;
    finally
      OIDC.Free;
    end;
    end;
end;

procedure TbaCustomCommand.FillCommandBasedOnRequest(ARequest: TRequest);
begin
  FAuthorizationToken := ARequest.Authorization;
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

