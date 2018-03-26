unit baBuildCommand;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  zipper,
  HTTPDefs,
  URIParser,
  FileUtil,
  fpjson,
  DOM,
  XMLRead,
  dcsHandler,
  dcsThreadCommandFactory,
  dcsInOutputProcessor,
  DCSHTTPRestServer,
  dcsGlobalSettings,
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


  { TDCSSourceArchiveNotificationEvent }

  TDCSSourceArchiveNotificationEvent = class(TDCSNotificationEvent, IDCSJSonResultEvent)
  private
    FManifest: string;
    FSourceArchive: string;
  public
    procedure AfterEventToJSon(Self: TDCSJSonInOutputProcessor; AnEvent: TDCSEvent; JSonEvent: TJSONObject);
  published
    property SourceArchive: string read FSourceArchive write FSourceArchive;
    property Manifest: string read FManifest write FManifest;
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

  { TbaCreateSourceArchiveCommand }

  TbaCreateSourceArchiveCommand = class(TbaCustomManifestCommand)
  private
    FSourceFilename: string;
    FManifestXML: string;
  protected
    function GetNotificationCommandEventClass: TDCSNotificationEventClass; override;
    function CreateExecutedCommandEvent(Success: Boolean; ReturnMessage: string; NotificationClass: TDCSNotificationEventClass): TDCSNotificationEvent; override;
  public
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

{ TDCSSourceArchiveNotificationEvent }

procedure TDCSSourceArchiveNotificationEvent.AfterEventToJSon(Self: TDCSJSonInOutputProcessor;
  AnEvent: TDCSEvent; JSonEvent: TJSONObject);
begin
  JSonEvent.Add('sourcearchive', FSourceArchive);
  JSonEvent.Add('manifest', FManifest);
end;

{ TbaCreateSourceArchiveCommand }

function TbaCreateSourceArchiveCommand.GetNotificationCommandEventClass: TDCSNotificationEventClass;
begin
  Result := TDCSSourceArchiveNotificationEvent;
end;

function TbaCreateSourceArchiveCommand.CreateExecutedCommandEvent(Success: Boolean;
  ReturnMessage: string; NotificationClass: TDCSNotificationEventClass): TDCSNotificationEvent;
var
  BuildFilesURL: String;
begin
  Result := inherited CreateExecutedCommandEvent(Success, ReturnMessage, NotificationClass);
  if Success then
    begin
    BuildFilesURL := TDCSGlobalSettings.GetInstance.GetSettingAsString('BuildFilesURL');
    (Result as TDCSSourceArchiveNotificationEvent).SourceArchive := ConcatPaths([BuildFilesURL, FSourceFilename]);
    (Result as TDCSSourceArchiveNotificationEvent).Manifest := FManifestXML;
    end;
end;

function TbaCreateSourceArchiveCommand.DoExecute(AController: TDCSCustomController; out ReturnMessage: string): Boolean;
var
  FN: string;
  ManifestXML: TXMLDocument;
  ArchiveName: string;
  Packages, Package, Filename: TDOMNode;
  BuildFilesLocation: string;
  i: Integer;
  SourceFilePath: RawByteString;
  Stream: TStringStream;
begin
  Result := inherited DoExecute(AController, ReturnMessage);
  if Result then
    begin
    Result := False;
    FN := ConcatPaths([FZipOutputPath, 'manifest.xml']);

    Stream := TStringStream.Create();
    try
      Stream.LoadFromFile(FN);
      FManifestXML := Stream.DataString;
      ReadXMLFile(ManifestXML, Stream, FilenameToURI(FN));
    finally
      Stream.Free;
    end;

    try
      Packages := ManifestXML.FindNode('packages');
      if not Assigned(Packages) then
        begin
        ReturnMessage := 'Invalid manifest file, no packages element.';
        Exit;
        end;
      if Packages.ChildNodes.Count = 0 then
        begin
        ReturnMessage := 'Invalid manifest file, packages element has no childs.';
        Exit;
        end;
      Package := Packages.ChildNodes[0];
      Filename := Package.FindNode('filename');
      if not Assigned(Package) then
        begin
        ReturnMessage := 'Invalid manifest file, no filename element.';
        Exit;
        end;
      ArchiveName := ConcatPaths([FZipOutputPath, Filename.TextContent]);

      if not FileExists(ArchiveName) then
        begin
        ReturnMessage := Format('Archive ''%s'' does not exist.', [Filename.TextContent]);
        Exit;
        end;

      BuildFilesLocation := TDCSGlobalSettings.GetInstance.GetSettingAsString('BuildFilesLocation');
      if BuildFilesLocation <> '' then
        begin
        repeat
        FSourceFilename := '';
        for i := 0 to 10 do
          begin
          FSourceFilename := FSourceFilename + chr(ord('a') + Random(26));
          end;
        SourceFilePath := ConcatPaths([BuildFilesLocation, FSourceFilename]);
        until not DirectoryExists(SourceFilePath);

        ForceDirectories(SourceFilePath);

        FSourceFilename := ConcatPaths([FSourceFilename, Filename.TextContent]);

        if not CopyFile(ArchiveName, ConcatPaths([BuildFilesLocation, FSourceFilename])) then
          begin
          ReturnMessage := 'Failed to copy archive to the build-files-location';
          Exit;
          end;
        end;
      Result := True;
    finally
      ManifestXML.Free;
    end;
    end;
end;

class function TbaCreateSourceArchiveCommand.TextName: string;
begin
  Result := 'createsourcearchive';
end;

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
  TDCSThreadCommandFactory.RegisterCommandClass(TbaCreateSourceArchiveCommand);
end.

