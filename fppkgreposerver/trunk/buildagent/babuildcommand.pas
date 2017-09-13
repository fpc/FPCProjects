unit baBuildCommand;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  HTTPDefs,
  FileUtil,
  dcsHandler,
  dcsThreadCommandFactory,
  DCSHTTPRestServer,
  baCommand;

type

  { TbaBuildCommand }

  TbaBuildCommand = class(TbaCustomCommand)
  protected
    FTempArchiveFileName: string;
  public
    class function TextName: string; override;
    destructor Destroy; override;
    function DoExecute(AController: TDCSCustomController; out ReturnMessage: string): Boolean; override;
    procedure FillCommandBasedOnRequest(ARequest: TRequest); override;
  published
    property OSTarget;
    property CPUTarget;
    property FPCVersion;
  end;


implementation

uses
  baBuildFPCEnvironment;

{ TbaBuildCommand }

class function TbaBuildCommand.TextName: string;
begin
  Result := 'build';
end;

destructor TbaBuildCommand.Destroy;
begin
  DeleteFile(FTempArchiveFileName);
  inherited Destroy;
end;

function TbaBuildCommand.DoExecute(AController: TDCSCustomController; out ReturnMessage: string): Boolean;
var
  PristineEnvironmentPath, BuildPath: String;
  ArchiveName: string;
begin
  PristineEnvironmentPath := GetPristineEnvironmentPath;
  if not DirectoryExists(PristineEnvironmentPath) then
    raise Exception.CreateFmt('No pristine FPC-installation available (%s). Please run  %s command first.', [PristineEnvironmentPath, TbaBuildFPCEnvironment.TextName]);

  BuildPath := GetBuildPath;

  FDistributor.Log(Format('Create build-environment at (%s), based on pristine FPC-installation at (%s)', [BuildPath, PristineEnvironmentPath]), etInfo, Null, FSendByLisId);

  ForceDirectories(BuildPath);
  RunTestCommandIndir(PristineEnvironmentPath, 'rsync', ['-rtvu', '--delete', PristineEnvironmentPath, BuildPath], 'sync FPC-installation');
  ArchiveName := ConcatPaths([BuildPath, ChangeFileExt(ExtractFileName(FTempArchiveFileName), '.zip')]);

  if not CopyFile(FTempArchiveFileName, ArchiveName) then
    raise Exception.CreateFmt('Failed to copy archive (%s) to build-environment (%s).', [FTempArchiveFileName, ArchiveName]);

  RunTestCommandIndir(BuildPath, GetFppkgExecutable, ['-C', ConcatPaths([BuildPath, 'etc', 'fppkg.cfg']), 'build', ArchiveName], 'build package');

  ReturnMessage := '';
  Result := true;
end;

procedure TbaBuildCommand.FillCommandBasedOnRequest(ARequest: TRequest);
var
  fs: TFileStream;
begin
  Inherited;
  FTempArchiveFileName := GetTempFileName;
  fs := TFileStream.Create(FTempArchiveFileName, fmCreate);
  try
    fs.WriteBuffer(ARequest.Content[1], length(ARequest.Content));
  finally
    fs.Free;
  end;
end;

initialization
  TDCSThreadCommandFactory.RegisterCommandClass(TbaBuildCommand);
end.

