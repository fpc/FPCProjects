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
    FManifestXML: string;
    FSourceArchive: string;
  public
    procedure AfterEventToJSon(Self: TDCSJSonInOutputProcessor; AnEvent: TDCSEvent; JSonEvent: TJSONObject);
  published
    property SourceArchive: string read FSourceArchive write FSourceArchive;
    property ManifestXML: string read FManifestXML write FManifestXML;
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
  JSonEvent.Add('manifestxml', FManifestXML);
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
    (Result as TDCSSourceArchiveNotificationEvent).ManifestXML := FManifestXML;
    end;
end;

function TbaCreateSourceArchiveCommand.DoExecute(AController: TDCSCustomController; out ReturnMessage: string): Boolean;
var
  FN: string;
  ManifestXML: TXMLDocument;
  ArchiveName: string;
  Packages, Package, Filename, Version: TDOMNode;
  BuildFilesLocation: string;
  i: Integer;
  SourceFilePath: RawByteString;
  Stream: TStringStream;

  function GetXMLVersionString(): string;
  var
    Node: TDOMNode;
  begin
    Result := '';
    Node := Version.Attributes.getNamedItem('major');
    if Assigned(Node) then
      Result := Node.NodeValue;
    Node := Version.Attributes.getNamedItem('minor');
    if Assigned(Node) then
      Result := Result + '.' + Node.NodeValue;
    Node := Version.Attributes.getNamedItem('micro');
    if Assigned(Node) then
      Result := Result + '.' + Node.NodeValue;
    Node := Version.Attributes.getNamedItem('build');
    if Assigned(Node) then
      Result := Result + '-' + Node.NodeValue;
  end;

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
        // FPC 304 has the annoying bug that the filename does not match the
        // source-archive name. Try to cunstruct the name ourselves, and try
        // that one.
        Version := Package.FindNode('version');
        if not Assigned(Version) then
          begin
          ReturnMessage := 'Invalid manifest file, no version element.';
          Exit;
          end;
        ArchiveName := ConcatPaths([FZipOutputPath, Package.Attributes.GetNamedItem('name').NodeValue+'-'+GetXMLVersionString+'.source.zip']);
        if not FileExists(ArchiveName) then
          begin
          ReturnMessage := Format('Archive ''%s'' does not exist.', [ArchiveName]);
          Exit;
          end;
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

        if not ForceDirectories(SourceFilePath) then
          begin
          ReturnMessage := 'Failed to create the directory for the source-files: [' + SourceFilePath + ']';
          Exit;
          end;

        FSourceFilename := ConcatPaths([FSourceFilename, Filename.TextContent]);

        if not CopyFile(ArchiveName, ConcatPaths([BuildFilesLocation, FSourceFilename])) then
          begin
          ReturnMessage := 'Failed to copy archive to the build-files-location';
          Exit;
          end;
        end
        else
          FDistributor.SendNotification(SendByLisId, ntListenerMessage, UID, 'Archive created, but the BuildFiles:Location setting is not provided. The archive will not be available for download.', TextName);
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
  BuildPath, s: String;
  ArchiveName: RawByteString;
  UnZipper: TUnZipper;
  ManifestXML: TXMLDocument;
begin
  Result := False;
  BuildPath := GetBuildPath;
  ArchiveName := ConcatPaths([BuildPath, ChangeFileExt(ExtractFileName(FTempArchiveFileName), '.zip')]);
  FZipOutputPath := ConcatPaths([BuildPath, 'tmppackage']);

  PrepareBuildEnvironment(BuildPath, FTempArchiveFileName, ArchiveName);

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

  if GetAbsoluteFilenamesBug then
    begin
    // Fppkg in fpc 3.0.4 has a bug which includes absolute filenames in source-
    // archives. These can not be used. Call fpmake directly instead.
    RunTestCommandIndir(FZipOutputPath, ConcatPaths([FZipOutputPath, 'fpmake']), ['--compiler='+ConcatPaths([BuildPath, 'bin', 'fpc']), 'archive'], 'call fpmake directly to create source-archive for package');
    end;

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

  PrepareBuildEnvironment(BuildPath, FTempArchiveFileName, ArchiveName);

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

