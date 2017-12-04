unit baBuildCommand;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  zipper,
  HTTPDefs,
  FileUtil,
  fpjson,
  DOM,
  XMLRead,
  dcsHandler,
  dcsThreadCommandFactory,
  dcsInOutputProcessor,
  DCSHTTPRestServer,
  fprPackageUtils,
  baCommand;

type

  { TbaCustomBuildCommand }

  TbaCustomBuildCommand = class(TbaCustomCommand)
  protected
    FTempArchiveFileName: string;
    procedure PrepareBuildEnvironment(BuildPath: string; ArchiveName: string);
  public
    procedure FillCommandBasedOnRequest(ARequest: TRequest); override;
    destructor Destroy; override;
  published
    property OSTarget;
    property CPUTarget;
    property FPCVersion;
  end;

  { TbaBuildCommand }

  TbaBuildCommand = class(TbaCustomBuildCommand)
  public
    function DoExecute(AController: TDCSCustomController; out ReturnMessage: string): Boolean; override;
    class function TextName: string; override;
  end;

  { TDCSManifestNotificationEvent }

  TDCSManifestNotificationEvent = class(TDCSNotificationEvent, IDCSJSonResultEvent)
  private
    FManifest: TJSONArray;
  public
    procedure AfterEventToJSon(Self: TDCSJSonInOutputProcessor; AnEvent: TDCSEvent; JSonEvent: TJSONObject);
    destructor Destroy; override;
  published
    property Manifest: TJSONArray read FManifest write FManifest;
  end;


  { TbaCustomManifestCommand }

  TbaCustomManifestCommand = class(TbaCustomBuildCommand)
  protected
    FZipOutputPath: string;
  public
    function DoExecute(AController: TDCSCustomController; out ReturnMessage: string): Boolean; override;
  end;

  { TbaManifestCommand }

  TbaManifestCommand = class(TbaCustomManifestCommand)
  protected
    FManifestJSON: TJSONArray;
    function GetNotificationCommandEventClass: TDCSNotificationEventClass; override;
    function CreateExecutedCommandEvent(Success: Boolean; ReturnMessage: string; NotificationClass: TDCSNotificationEventClass): TDCSNotificationEvent; override;
  public
    destructor Destroy; override;
    function DoExecute(AController: TDCSCustomController; out ReturnMessage: string): Boolean; override;
    class function TextName: string; override;
  end;

  { TbaUploadCommand }

  TbaUploadCommand = class(TbaCustomManifestCommand)
  public
    function DoExecute(AController: TDCSCustomController; out ReturnMessage: string): Boolean; override;
    class function TextName: string; override;
  end;

implementation

uses
  baBuildFPCEnvironment;

{ TbaCustomManifestCommand }

function TbaCustomManifestCommand.DoExecute(AController: TDCSCustomController; out ReturnMessage: string): Boolean;
var
  BuildPath: String;
  ArchiveName: RawByteString;
  UnZipper: TUnZipper;
  ManifestXML: TXMLDocument;
begin
  Result := False;
  BuildPath := GetBuildPath;
  ArchiveName := ConcatPaths([BuildPath, ChangeFileExt(ExtractFileName(FTempArchiveFileName), '.zip')]);
  FZipOutputPath := ConcatPaths([BuildPath, 'tmppackage']);

  PrepareBuildEnvironment(BuildPath, ArchiveName);

  try
    UnZipper := TUnZipper.Create;
    try
      UnZipper.FileName := ArchiveName;
      UnZipper.OutputPath := FZipOutputPath;
      UnZipper.UnZipAllFiles();
    finally
      UnZipper.Free;
    end;

  except
    on E: Exception do
    begin
    ReturnMessage := 'Failed to extract zip-file. ' + E.Message;
    Exit;
    end;
  end;

  RunTestCommandIndir(FZipOutputPath, GetFppkgExecutable, ['-C', ConcatPaths([BuildPath, 'etc', 'fppkg.cfg']), 'archive'], 'create source-archive for package');

  ReturnMessage := '';
  Result := true;
end;

{ TbaUploadCommand }

function TbaUploadCommand.DoExecute(AController: TDCSCustomController; out ReturnMessage: string): Boolean;
var
  ManifestXML: TXMLDocument;
begin
  Result := inherited DoExecute(AController, ReturnMessage);
  if Result then
    begin
    ReadXMLFile(ManifestXML, ConcatPaths([FZipOutputPath, 'manifest.xml']));
    try

    finally
      ManifestXML.Free;
    end;
    end;

  ReturnMessage := '';
  Result := true;
end;

class function TbaUploadCommand.TextName: string;
begin
  Result := 'upload';
end;

{ TDCSManifestNotificationEvent }

procedure TDCSManifestNotificationEvent.AfterEventToJSon(Self: TDCSJSonInOutputProcessor; AnEvent: TDCSEvent; JSonEvent: TJSONObject);
begin
  if Assigned(Manifest) then
    JSonEvent.Add('manifest', Manifest.Clone);
end;

destructor TDCSManifestNotificationEvent.Destroy;
begin
  FManifest.Free;
  inherited Destroy;
end;

{ TbaManifestCommand }

function TbaManifestCommand.GetNotificationCommandEventClass: TDCSNotificationEventClass;
begin
  Result := TDCSManifestNotificationEvent;
end;

function TbaManifestCommand.CreateExecutedCommandEvent(Success: Boolean; ReturnMessage: string;
  NotificationClass: TDCSNotificationEventClass): TDCSNotificationEvent;
begin
  Result := inherited CreateExecutedCommandEvent(Success, ReturnMessage, NotificationClass);
  (result as TDCSManifestNotificationEvent).Manifest := FManifestJSON;
  FManifestJSON := nil;
end;

destructor TbaManifestCommand.Destroy;
begin
  if assigned(FManifestJSON) then
    FManifestJSON.Free;
  inherited Destroy;
end;

function TbaManifestCommand.DoExecute(AController: TDCSCustomController; out ReturnMessage: string): Boolean;
var
  ManifestXML: TXMLDocument;
begin
  Result := inherited DoExecute(AController, ReturnMessage);
  if Result then
    begin
    ReadXMLFile(ManifestXML, ConcatPaths([FZipOutputPath, 'manifest.xml']));
    try
      FManifestJSON := ManifestToJSON(ManifestXML);
    finally
      ManifestXML.Free;
    end;
    end;

  ReturnMessage := '';
  Result := true;
end;

class function TbaManifestCommand.TextName: string;
begin
  Result := 'manifest';
end;

{ TbaBuildCommand }

function TbaBuildCommand.DoExecute(AController: TDCSCustomController; out ReturnMessage: string): Boolean;
var
  BuildPath: String;
  ArchiveName: RawByteString;
begin
  BuildPath := GetBuildPath;
  ArchiveName := ConcatPaths([BuildPath, ChangeFileExt(ExtractFileName(FTempArchiveFileName), '.zip')]);

  PrepareBuildEnvironment(BuildPath, ArchiveName);

  RunTestCommandIndir(BuildPath, GetFppkgExecutable, ['-C', ConcatPaths([BuildPath, 'etc', 'fppkg.cfg']), 'build', ArchiveName], 'build package');

  ReturnMessage := '';
  Result := true;
end;

class function TbaBuildCommand.TextName: string;
begin
  Result := 'build';
end;

{ TbaCustomBuildCommand }

destructor TbaCustomBuildCommand.Destroy;
begin
  DeleteFile(FTempArchiveFileName);
  inherited Destroy;
end;

procedure TbaCustomBuildCommand.PrepareBuildEnvironment(BuildPath: string; ArchiveName: string);
var
  PristineEnvironmentPath: String;
begin
  PristineEnvironmentPath := GetPristineEnvironmentPath;
  if not DirectoryExists(PristineEnvironmentPath) then
    raise Exception.CreateFmt('No pristine FPC-installation available (%s). Please run  %s command first.', [PristineEnvironmentPath, TbaBuildFPCEnvironment.TextName]);

  FDistributor.Log(Format('Create build-environment at (%s), based on pristine FPC-installation at (%s)', [BuildPath, PristineEnvironmentPath]), etInfo, Null, FSendByLisId);

  ForceDirectories(BuildPath);
  RunTestCommandIndir(PristineEnvironmentPath, 'rsync', ['-rtvul', '--delete', PristineEnvironmentPath, BuildPath], 'sync FPC-installation');

  if not CopyFile(FTempArchiveFileName, ArchiveName) then
    raise Exception.CreateFmt('Failed to copy archive (%s) to build-environment (%s).', [FTempArchiveFileName, ArchiveName]);
end;

procedure TbaCustomBuildCommand.FillCommandBasedOnRequest(ARequest: TRequest);
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
  TDCSThreadCommandFactory.RegisterCommandClass(TbaManifestCommand);
  TDCSThreadCommandFactory.RegisterCommandClass(TbaUploadCommand);
end.

