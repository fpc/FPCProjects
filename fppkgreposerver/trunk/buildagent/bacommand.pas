unit baCommand;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  process,
  FileUtil,
  dcsHandler,
  DCSHTTPRestServer,
  HTTPDefs,
  DateUtils,
  Math,
  cnocOpenIDConnect,
  dcsGlobalSettings;

type
  TbaRunCommandResult = (barcrSucces, barcrFailed, barcrTimeout, barcrWrongExitcode);

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
    function GetAbsoluteFilenamesBug: boolean;
    function TryRunTestCommandIndir(const Curdir: string; const Exename: string;
      const Commands: array of string; TaskDescription: string;
      out CommandOutput, CommandLine: string; ExpectedExitStatus: Integer; const TimeoutMS: Int64): TbaRunCommandResult;
    function RunTestCommandIndir(const Curdir: string; const Exename: string;
      const Commands: array of string; TaskDescription: string; ExpectedExitStatus: Integer = 0; const TimeoutMS: Int64 = -1): string;

    procedure PrepareBuildEnvironment(BuildPath: string; TempArchiveFileName, ArchiveName: string);

    property OSTarget: string read FOSTarget write FOSTarget;
    property CPUTarget: string read FCPUTarget write FCPUTarget;
    property FPCVersion: string read FFPCVersion write FFPCVersion;
  public
    procedure PreExecute(AController: TDCSCustomController; out DoQueueCommand: boolean); override;
    procedure FillCommandBasedOnRequest(ARequest: TRequest); virtual;
  end;

implementation

uses
  baBuildFPCEnvironment;

// Copy of the procedure with the same name in the process unit, adapted
// to be able to handle a timeout value.
Const
  READ_BYTES = 65536; // not too small to avoid fragmentation when reading large files.
function internalRuncommand(p:TProcess;out outputstring:string;
                            out stderrstring:string; out exitstatus:integer; const TimeoutMS: Int64):integer;
var
    numbytes,bytesread,available : integer;
    outputlength, stderrlength : integer;
    stderrnumbytes,stderrbytesread : integer;
    starttime, currenttime: TDateTime;
    msecsleft: Int64;
begin
  if TimeoutMS > -1 then
    starttime := Now;
  result:=-1;
  try
    try
    p.Options := p.Options + [poUsePipes];
    bytesread:=0;
    outputlength:=0;
    stderrbytesread:=0;
    stderrlength:=0;
    p.Execute;
    while p.Running do
      begin
        if TimeoutMS > -1 then
          begin
            currenttime := Now;
            msecsleft := TimeoutMS - MilliSecondsBetween(currenttime, starttime);
            if msecsleft < 1 then
              begin
                p.Terminate(999);
                Result := -2;
                Break;
              end;
          end;

        // Only call ReadFromStream if Data from corresponding stream
        // is already available, otherwise, on  linux, the read call
        // is blocking, and thus it is not possible to be sure to handle
        // big data amounts bboth on output and stderr pipes. PM.
        available:=P.Output.NumBytesAvailable;
        if  available > 0 then
          begin
            if (BytesRead + available > outputlength) then
              begin
                outputlength:=BytesRead + READ_BYTES;
                Setlength(outputstring,outputlength);
              end;
            NumBytes := p.Output.Read(outputstring[1+bytesread], available);
            if NumBytes > 0 then
              Inc(BytesRead, NumBytes);
          end
        // The check for assigned(P.stderr) is mainly here so that
        // if we use poStderrToOutput in p.Options, we do not access invalid memory.
        else if assigned(P.stderr) and (P.StdErr.NumBytesAvailable > 0) then
          begin
            available:=P.StdErr.NumBytesAvailable;
            if (StderrBytesRead + available > stderrlength) then
              begin
                stderrlength:=StderrBytesRead + READ_BYTES;
                Setlength(stderrstring,stderrlength);
              end;
            StderrNumBytes := p.StdErr.Read(stderrstring[1+StderrBytesRead], available);
            if StderrNumBytes > 0 then
              Inc(StderrBytesRead, StderrNumBytes);
          end
        else
          Sleep(Min(msecsleft, 100));
      end;
        exitstatus:=p.ExitCode;

    // Get left output after end of execution
    available:=P.Output.NumBytesAvailable;
    while available > 0 do
      begin
        if (BytesRead + available > outputlength) then
          begin
            outputlength:=BytesRead + READ_BYTES;
            Setlength(outputstring,outputlength);
          end;
        NumBytes := p.Output.Read(outputstring[1+bytesread], available);
        if NumBytes > 0 then
          Inc(BytesRead, NumBytes);
        available:=P.Output.NumBytesAvailable;
      end;
    setlength(outputstring,BytesRead);
    while assigned(P.stderr) and (P.Stderr.NumBytesAvailable > 0) do
      begin
        available:=P.Stderr.NumBytesAvailable;
        if (StderrBytesRead + available > stderrlength) then
          begin
            stderrlength:=StderrBytesRead + READ_BYTES;
            Setlength(stderrstring,stderrlength);
          end;
        StderrNumBytes := p.StdErr.Read(stderrstring[1+StderrBytesRead], available);
        if StderrNumBytes > 0 then
          Inc(StderrBytesRead, StderrNumBytes);
      end;
    setlength(stderrstring,StderrBytesRead);
    exitstatus:=p.ExitCode;
    if Result <> -2 then // It's not a timeout
      result:=0; // we came to here, document that.
    except
      on e : Exception do
         begin
           result:=1;
           setlength(outputstring,BytesRead);
         end;
     end;
  finally
    p.free;
    end;
end;


// Copy of the procedure with the same name in the process unit
Const
  ForbiddenOptions = [poRunSuspended,poWaitOnExit];
function RunCommandIndir(const curdir:string;const exename:string;const commands:array of string;out outputstring:string;out exitstatus:integer; Options : TProcessOptions = []; const TimeoutMS: integer = -1):integer;
Var
    p : TProcess;
    i : integer;
    ErrorString : String;
begin
  p:=TProcess.create(nil);
  if Options<>[] then
    P.Options:=Options - ForbiddenOptions;
  p.Executable:=exename;
  if curdir<>'' then
    p.CurrentDirectory:=curdir;
  if high(commands)>=0 then
   for i:=low(commands) to high(commands) do
     p.Parameters.add(commands[i]);
  result:=internalruncommand(p,outputstring,errorstring,exitstatus, TimeoutMS);
end;

{ TbaCustomCommand }

function TbaCustomCommand.TryRunTestCommandIndir(const Curdir: string; const Exename: string;
  const Commands: array of string; TaskDescription: string; out CommandOutput, CommandLine: string;
  ExpectedExitStatus: Integer; const TimeoutMS: Int64): TbaRunCommandResult;
var
  i: integer;
  res: integer;
  ExitStatus: Integer;
begin
  Result := barcrSucces;
  CommandLine := Exename;
  for i := 0 to length(Commands) -1 do
    CommandLine := CommandLine + ' ' + Commands[i];
  FDistributor.Log(Format('Run external command.' +sLineBreak+ 'Directory: "%s".' +sLineBreak+ 'Commandstring: "%s".', [Curdir, CommandLine]), etDebug, Null, FSendByLisId);
  res := RunCommandInDir(Curdir, Exename, Commands, CommandOutput, ExitStatus, [poStderrToOutPut], TimeoutMS);

  if res = -2 then
    begin
    FDistributor.Log(Format('External command timed out. Output: ' +sLineBreak+ '%s', [CommandOutput]), etDebug, Null, FSendByLisId);
    Result := barcrTimeout;
    end
  else if res <> 0 then
    Result := barcrFailed
  else if ExitStatus<>ExpectedExitStatus then
    begin
    Result := barcrWrongExitcode;
    FDistributor.Log(Format('External command returned unexpected exit-code ['+IntToStr(ExitStatus)+']. Output: ' +sLineBreak+ '%s', [CommandOutput]), etDebug, Null, FSendByLisId);
    end;
  FDistributor.Log(Format('External command output.' +sLineBreak+ '%s', [CommandOutput]), etDebug, Null, FSendByLisId);
end;

function TbaCustomCommand.RunTestCommandIndir(const Curdir:string; const Exename:string; const Commands:array of string; TaskDescription: string; ExpectedExitStatus: Integer = 0; const TimeoutMS: Int64 = -1):string;
var
  CommandOutput: string;
  CommandLine: string;
begin
  case TryRunTestCommandIndir(Curdir, Exename, Commands, TaskDescription, CommandOutput, CommandLine, ExpectedExitStatus, TimeoutMS) of
    barcrWrongExitcode: raise Exception.CreateFmt('Failed to %s.' +sLineBreak+ 'Current directory: ' +Curdir+ sLineBreak + 'command line: ' + CommandLine + sLineBreak + ' Output: ' + sLineBreak + CommandOutput, [TaskDescription]);
    barcrFailed: raise Exception.CreateFmt('Could not %s.' +sLineBreak+ 'Current directory: ' +Curdir+ sLineBreak + 'command line: ' + CommandLine, [TaskDescription]);
    barcrTimeout: raise Exception.CreateFmt('Timeout on %s.' +sLineBreak+ 'Current directory: ' +Curdir+ sLineBreak + 'command line: ' + CommandLine + sLineBreak + ' Output: ' + sLineBreak + CommandOutput, [TaskDescription]);
  end;
  result := CommandOutput;
end;

procedure TbaCustomCommand.PrepareBuildEnvironment(BuildPath: string; TempArchiveFileName, ArchiveName: string);
var
  PristineEnvironmentPath: String;
begin
  PristineEnvironmentPath := GetPristineEnvironmentPath;
  if not DirectoryExists(PristineEnvironmentPath) then
    raise Exception.CreateFmt('No pristine FPC-installation available (%s). Please run  %s command first.', [PristineEnvironmentPath, TbaBuildFPCEnvironment.TextName]);

  FDistributor.Log(Format('Create build-environment at (%s), based on pristine FPC-installation at (%s)', [BuildPath, PristineEnvironmentPath]), etInfo, Null, FSendByLisId);

  ForceDirectories(BuildPath);
  RunTestCommandIndir(PristineEnvironmentPath, 'rsync', ['-rtvul', '--delete', PristineEnvironmentPath, BuildPath], 'sync FPC-installation');

  if (TempArchiveFileName<>'') and not CopyFile(TempArchiveFileName, ArchiveName) then
    raise Exception.CreateFmt('Failed to copy archive (%s) to build-environment (%s).', [TempArchiveFileName, ArchiveName]);
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
        FDistributor.SendNotification(SendByLisId, ntFailedCommand, UID, 'Access denied. Missing authorization token.', TextName, [TextName]);
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

function TbaCustomCommand.GetAbsoluteFilenamesBug: boolean;
var
  GlobalSettings: TDCSGlobalSettings;
begin
  GlobalSettings := TDCSGlobalSettings.GetInstance;
  Result := GlobalSettings.GetSettingAsBooleanByKey(GetFPCEnvironmentKey, 'AbsoluteFilenamesBug');
end;

end.

