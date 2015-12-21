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

  { TRepoController }

  TRepoController = class(TDCSCustomController)
  private
    FRepoDir: String;
    FStartCompiler: String;
    FTestCompiler: String;
    FTargetString: String;
    FCompilerVersion: String;
    procedure LoadIniFile;
  public
    constructor Create(ADistributor: TDCSDistributor); override;
    procedure Init; override;
    property RepoDir: string read FRepoDir;
    property StartCompiler: string read FStartCompiler;
    property TargetString: string read FTargetString;
    property CompilerVersion: string read FCompilerVersion;
    property TestCompiler: string read FTestCompiler;
  end;

  { TRepoCommand }

  TRepoCommand = class(TDCSThreadCommand)
  public
    procedure AddToTestLog(ALevel: TLogLevel; AMsg: String); virtual;
    function DoExecuteRepoCommand(AController: TDCSCustomController; out ReturnMessage: string): Boolean; virtual;
    function DoExecute(AController: TDCSCustomController; out ReturnMessage: string): Boolean; override;
  end;


implementation

{ TRepoCommand }

var
  GCommand: TRepoCommand;

const
  SLogLevel: array[TLogLevel] of string = (
    'Error',
    'Warning',
    'Info',
    'Commands',
    'llDebug',
    'llProgres'
  );

procedure LogCmd(Level: TLogLevel; const Msg: String);
begin
  if not Assigned(GCommand) then
    Exit;
  GCommand.AddToTestLog(Level, Msg);
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
  finally
    IniFile.Free;
  end;
end;

constructor TRepoController.Create(ADistributor: TDCSDistributor);
begin
  inherited Create(ADistributor);
end;

procedure TRepoController.Init;
begin
  LoadIniFile;

  LogLevels:=DefaultLogLevels;
  pkgoptions.LoadGlobalDefaults(FRepoDir+'etc/fppkg.cfg');
  LoadCompilerDefaults;

  FPMakeCompilerOptions.CheckCompilerValues;
  CompilerOptions.CheckCompilerValues;

  LoadLocalAvailableRepository;
  FindInstalledPackages(CompilerOptions);
  CheckFPMakeDependencies;

  LoadLocalAvailableMirrors;

  ClearExecutedAction;
end;

end.

