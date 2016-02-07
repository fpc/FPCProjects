unit RepoSvnCommands;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  RepoController,
  pkgDownload,
  dcsHandler,
  LazFileUtils,
  dcsThreadCommandFactory,
  zipper,
  fprepos,
  pkgrepos,
  pkgcommands,
  fpxmlrep,
  pkghandler,
  pkgglobals,
  process,
  XMLRead,
  DOM,
  FileUtil;


type

  { TSVNRepository }

  TSVNRepository = class
  private
    FDistributor: TDCSDistributor;
    FController: TRepoController;
    FUID: variant;
  public
    constructor Create(ADistributor: TDCSDistributor; AController: TRepoController; AnUID: Variant); virtual;

    function RunSvn(Params: array of string; Output,ErrOutput: TStream): Integer;
    function RunSvn(Params: array of string; out Output, ErrOutput: string): Integer;
    function RunSvn(Params: array of string; out Output: string): Integer;
    function RunSvnXML(Params: array of string; out ErrOutput: string): TXMLDocument;
    function DoesSVNPathExist(SvnUrl: string): Boolean;
    function DoesPackageExist(PackageName: string; FPCVersion, Version: string): Boolean;
    function GetPackageFromManifestFile(AFileName: string): TFPPackage;

    property Distributor: TDCSDistributor read FDistributor;
    property UID: variant read FUID;
  end;

  { TRepoCustomSvnCommand }

  TRepoCustomSvnCommand = class(TRepoCommand)
  private
    FCommitMessage: string;
    FRepositoryFPCVersion: string;
  public
    constructor Create(ASendByLisId: integer; AnUID: variant; ADistributor: TDCSDistributor); override;
  published
    property RepositoryFPCVersion: string read FRepositoryFPCVersion write FRepositoryFPCVersion;
    property CommitMessage: string read FCommitMessage write FCommitMessage;
  end;

  { TRepoCustomPackageSvnCommand }

  TRepoCustomPackageSvnCommand = class(TRepoCustomSvnCommand)
  private
    FPackageURL: string;
  protected
    FPackageZipFile: string;
    FTempDir: string;
    function DownloadPackage: Boolean;
    function UnzipPackage: Boolean;
  public
  published
    property PackageURL: string read FPackageURL write FPackageURL;
  end;


  { TRepoSvnAddPackageCommand }

  TRepoSvnAddPackageCommand = class(TRepoCustomPackageSvnCommand)
  protected
    FSVNRepository: TSVNRepository;
  public
    class function TextName: string; override;
    function DoExecuteRepoCommand(AController: TDCSCustomController; out ReturnMessage: string): Boolean; override;
  end;

  { TRepoSvnUpdatePackageCommand }

  TRepoSvnUpdatePackageCommand = class(TRepoCustomPackageSvnCommand)
  private
    procedure UpdatePackageFileSearcherFileFound(FileIterator: TFileIterator);
    procedure UpdatePackageFileSearcherDirectoryFound(FileIterator: TFileIterator);
  protected
    FSVNRepository: TSVNRepository;
    procedure UpdatePackageFiles;
    function HasUpdates: Boolean;
  public
    class function TextName: string; override;
    function DoExecuteRepoCommand(AController: TDCSCustomController; out ReturnMessage: string): Boolean; override;
  end;

  { TRepoTagPackageSvnCommand }

  TRepoTagPackageSvnCommand = class(TRepoCustomSvnCommand)
  private
    FPackageName: string;
  protected
    FTempDir: string;
    FSVNRepository: TSVNRepository;
  public
    class function TextName: string; override;
    function DoExecuteRepoCommand(AController: TDCSCustomController; out ReturnMessage: string): Boolean; override;
  published
    property PackageName: string read FPackageName write FPackageName;
  end;

  { TRepoReleasePackageSvnCommand }

  TRepoReleasePackageSvnCommand = class(TRepoCustomSvnCommand)
  private
    FPackageName: string;
    FRepository: string;
    FVersion: string;
  protected
    FTempDir: string;
    FSVNRepository: TSVNRepository;
    function AddManifestToRepository(AController: TRepoController; PackageName, ManifestFileName: String
      ): Boolean;
  public
    constructor Create(ASendByLisId: integer; AnUID: variant; ADistributor: TDCSDistributor); override;
    class function TextName: string; override;
    function DoExecuteRepoCommand(AController: TDCSCustomController; out ReturnMessage: string): Boolean; override;
  published
    property PackageName: string read FPackageName write FPackageName;
    property Version: string read FVersion write FVersion;
    property Repository: string read FRepository write FRepository;
  end;

  { TPublishRepository }

  TPublishRepository = class(TRepoCommand)
  private
    FRepository: string;
    FRepositoryFPCVersion: string;
  public
    constructor Create(ASendByLisId: integer; AnUID: variant; ADistributor: TDCSDistributor); override;
    class function TextName: string; override;
    function DoExecuteRepoCommand(AController: TDCSCustomController; out ReturnMessage: string): Boolean; override;
  published
    property Repository: string read FRepository write FRepository;
    property RepositoryFPCVersion: string read FRepositoryFPCVersion write FRepositoryFPCVersion;
  end;


implementation

{ TPublishRepository }

constructor TPublishRepository.Create(ASendByLisId: integer; AnUID: variant; ADistributor: TDCSDistributor);
begin
  inherited Create(ASendByLisId, AnUID, ADistributor);
  Repository := 'testing';
  RepositoryFPCVersion := 'trunk';
end;

class function TPublishRepository.TextName: string;
begin
  result := 'publishrepository';
end;

function TPublishRepository.DoExecuteRepoCommand(AController: TDCSCustomController; out ReturnMessage: string): Boolean;
var
  RepoDir: string;
  TmpRepoDir: string;
  OldRepoDir: string;
  s, e: String;
  SVNRepository: TSVNRepository;
  RepoVersion: string;
begin
  RepoDir := TRepoController(AController).PublishedRepoDir;
  RepoVersion := TRepoController(AController).SvnBranchToFPCVersion(FRepositoryFPCVersion);

  TmpRepoDir := RepoDir+Repository+PathDelim+RepoVersion+'_tmp';
  if DirectoryExists(TmpRepoDir) then
    RemoveTree(TmpRepoDir);
  OldRepoDir := RepoDir+Repository+PathDelim+RepoVersion+'_old';
  if DirectoryExists(OldRepoDir) then
    RemoveTree(OldRepoDir);

  ForceDirectory(RepoDir+Repository);

  SVNRepository := TSVNRepository.Create(FDistributor, AController as TRepoController, FUID);
  try
    result := SVNRepository.RunSvn(['export',TRepoController(AController).SvnUrl+'repositories/'+FRepositoryFPCVersion+'/'+FRepository,TmpRepoDir],s,e)=0;
    if not Result then
      ReturnMessage := e
    else
      begin
      if DirectoryExists(RepoDir+Repository+PathDelim+RepoVersion) then
        RenameFile(RepoDir+Repository+PathDelim+RepoVersion, OldRepoDir);
      Result := RenameFile(TmpRepoDir, RepoDir+Repository+PathDelim+RepoVersion);
      RemoveTree(OldRepoDir);
      end;
  finally
    SVNRepository.Free;
  end;
end;

{ TRepoReleasePackageSvnCommand }

function TRepoReleasePackageSvnCommand.AddManifestToRepository(AController: TRepoController; PackageName, ManifestFileName: String): Boolean;
var
  Rep: TFPRepository;
  AddRepository: TFPRepository;
  RepositoryHandler: TFPXMLRepositoryHandler;
  UpdatePackagesXml: Boolean;
  s,e: string;
  i: Integer;
begin
  Rep := TFPRepository.Create(nil);
  try
    RepositoryHandler := TFPXMLRepositoryHandler.Create;
    try
      UpdatePackagesXml :=  FSVNRepository.DoesSVNPathExist(AController.SvnUrl+'repositories/'+FRepositoryFPCVersion+'/'+FRepository+'/packages.xml');
      if UpdatePackagesXml then
        begin
        if FSVNRepository.RunSvn(['up', FTempDir+'repo_checkout/packages.xml'],s,e) <> 0 then
          raise Exception.CreateFmt('Failed to checkout packages.xml. Msg: %s',[e]);
        RepositoryHandler.LoadFromXml(Rep, FTempDir+'repo_checkout/packages.xml');
        end;

      AddRepository := TFPRepository.Create(nil);
      try
        RepositoryHandler.LoadFromXml(AddRepository.PackageCollection, ManifestFileName);
        for i := AddRepository.PackageCollection.Count -1 downto 0 do
          begin
          AddRepository.Packages[i].Collection := Rep.PackageCollection;
          end;
      finally
        AddRepository.Free;
      end;
      RepositoryHandler.SaveToXml(Rep, FTempDir+'repo_checkout/packages.xml');

      if not UpdatePackagesXml then
        begin
        if FSVNRepository.RunSvn(['add',FTempDir+'repo_checkout/packages.xml'],s,e)<>0 then
          raise Exception.CreateFmt('Failed to add packages.xml to repository. Msg: %s',[e]);
        end;
    finally
      RepositoryHandler.Free;
    end;
  finally
    Rep.Free;
  end;
  Result := True;
end;

constructor TRepoReleasePackageSvnCommand.Create(ASendByLisId: integer; AnUID: variant; ADistributor: TDCSDistributor);
begin
  inherited Create(ASendByLisId, AnUID, ADistributor);
  FRepository := 'testing';
end;

class function TRepoReleasePackageSvnCommand.TextName: string;
begin
  Result := 'releasepackage'
end;

function TRepoReleasePackageSvnCommand.DoExecuteRepoCommand(AController: TDCSCustomController; out ReturnMessage: string): Boolean;
var
  s, e: string;
  StoredPath: string;
  ZipFile: string;
  ZipPath: string;
  Package: TFPPackage;
begin
  Result := False;
  if PackageName='' then
    begin
    ReturnMessage := 'Missing packagename parameter';
    Exit;
    end;
  if Version='' then
    begin
    ReturnMessage := 'Missing version parameter';
    Exit;
    end;

  TRepoController(AController).Init;

  FSVNRepository:= TSVNRepository.Create(FDistributor, AController as TRepoController, FUID);
  try
    if not FSVNRepository.DoesPackageExist(PackageName,FRepositoryFPCVersion,Version) then
      begin
      ReturnMessage := Format('Package "%s" version "%s" does not exist.',[PackageName, Version]);
      Exit;
      end;

    ZipFile := PackageName + '-' + Version + '.source.zip';

    if FSVNRepository.DoesSVNPathExist(TRepoController(AController).SvnUrl+'repositories/'+FRepositoryFPCVersion+'/'+Repository+'/'+ZipFile) then
      begin
      ReturnMessage := Format('Package "%s" version "%s" is already released.',[PackageName, Version]);
      Exit;
      end;

    FTempDir := IncludeTrailingPathDelimiter(GetTempFilename(FTempDir, 'fppkg_'));
    CreateDir(FTempDir);
    try
      if FSVNRepository.RunSvn(['co',TRepoController(AController).SvnUrl+'packages/'+PackageName+'/'+FRepositoryFPCVersion+'/tags/'+Version,FTempDir+'checkout'],s,e)=0 then
      begin
        Package := InstalledRepository.FindPackage(PackageName);
        if not assigned(Package) then
          begin
          FDistributor.Log('Package fpmkunit will be installed', etDebug, FUID);
          pkghandler.ExecuteAction('fpmkunit', 'install');
          end;
        StoredPath := GetCurrentDir;
        try
          chdir(FTempDir+'checkout');
          AvailableRepository.AddPackage(CurrentDirPackageName);
          pkghandler.ExecuteAction(CurrentDirPackageName , 'archive');
          ZipPath := FTempDir+'checkout'+PathDelim+ZipFile;
        finally
          chdir(StoredPath);
        end;
    FDistributor.Log('Tadaaa!!!!Package fpmkunit will be installed '+ZipPath, etDebug, FUID);
        if not FileExists(ZipPath) then
          begin
          if FileExists(FTempDir+'checkout'+PathDelim+PackageName + '-' + Version + '.zip') then
            begin
            FDistributor.Log('Zip-file does not have the .source-suffix. It will be added.', etInfo, FUID);
            CopyFile(FTempDir+'checkout'+PathDelim+PackageName + '-' + Version + '.zip', ZipPath);
            end;
          end;
        if FileExists(ZipPath) then
          begin
          FDistributor.Log('Toevoegen maar!Tadaaa!!!!Package fpmkunit will be installed '+ZipPath, etDebug, FUID);

          if FSVNRepository.RunSvn(['co','--depth=empty',TRepoController(AController).SvnUrl+'repositories/'+FRepositoryFPCVersion+'/'+Repository,FTempDir+'repo_checkout'],s,e)=0 then
            begin

            if CopyFile(ZipPath, FTempDir+PathDelim+'repo_checkout'+PathDelim+ZipFile) then
              begin
              if FSVNRepository.RunSvn(['add',FTempDir+PathDelim+'repo_checkout'+PathDelim+ZipFile],s,e)=0 then
                begin
                if CommitMessage='' then
                  CommitMessage := 'Release of package '+PackageName+ ' version ' +Version;
                Result := AddManifestToRepository(TRepoController(AController), PackageName, FTempDir+'checkout'+PathDelim+'manifest.xml') and
                  (FSVNRepository.RunSvn(['commit',FTempDir+PathDelim+'repo_checkout','-m '+QuotedStr(CommitMessage)],s,e)=0);
                end;
              end;
            end
          else
            FDistributor.Log('Failed to checkout repository. Msg: '+e, etError, FUID);
          end
        else
          begin
          FDistributor.Log('Kan tnoch niet?!?!?Zip-file does not have the .source-suffix. It will be added.', etInfo, FUID);

          ReturnMessage := Format('''fpmake archive'' did not create a source-zipfile. (%s)', [ZipPath]);

          end;
        FDistributor.Log('Vreemddd....Tadaaa!!!!Package fpmkunit will be installed '+ZipPath, etDebug, FUID);
      end;

    finally
      DeleteDirectory(FTempDir, false);
    end;
  finally
    FSVNRepository.Free;
  end;
end;

{ TRepoCustomSvnCommand }

constructor TRepoCustomSvnCommand.Create(ASendByLisId: integer; AnUID: variant; ADistributor: TDCSDistributor);
begin
  inherited Create(ASendByLisId, AnUID, ADistributor);
  FRepositoryFPCVersion := 'trunk';
end;

{ TRepoTagPackageSvnCommand }

class function TRepoTagPackageSvnCommand.TextName: string;
begin
  Result := 'tagpackage';
end;

function TRepoTagPackageSvnCommand.DoExecuteRepoCommand(AController: TDCSCustomController; out
  ReturnMessage: string): Boolean;
var
  Package: TFPPackage;
  VersionStr, s, e: string;
begin
  Result := False;
  if PackageName='' then
    begin
    ReturnMessage := 'Missing packagename parameter';
    Exit;
    end;
  FSVNRepository:= TSVNRepository.Create(FDistributor, AController as TRepoController, FUID);
  try
    if not FSVNRepository.DoesPackageExist(PackageName,'','') then
      begin
      ReturnMessage := Format('Package "%s" does not exist.',[PackageName]);
      Exit;
      end;

    FTempDir := IncludeTrailingPathDelimiter(GetTempFilename(FTempDir, 'fppkg_'));
    CreateDir(FTempDir);
    try
      if FSVNRepository.RunSvn(['co',TRepoController(AController).SvnUrl+'packages/'+PackageName+'/'+FRepositoryFPCVersion+'/branche',FTempDir+'checkout'],s,e)=0 then
      begin
        Package := FSVNRepository.GetPackageFromManifestFile(FTempDir+'checkout'+PathDelim+PathDelim+'manifest.xml');
        try
          VersionStr := Package.Version.AsString;
        finally
          Package.Free;
        end;
        if FSVNRepository.DoesPackageExist(PackageName,FRepositoryFPCVersion, VersionStr) then
        begin
          ReturnMessage := Format('Package "%s" already has a tag for version %s',[PackageName,VersionStr]);
          Exit;
        end;
        if FSVNRepository.RunSvn(['copy',TRepoController(AController).SvnUrl+'packages/'+PackageName+'/'+FRepositoryFPCVersion+'/branche',TRepoController(AController).SvnUrl+'packages/'+PackageName+'/'+FRepositoryFPCVersion+'/tags/'+VersionStr+'/','-m ''Tagged version '+VersionStr+''''],s,e)=0 then
          begin
          Result := True;
          end
        else
          begin
          ReturnMessage := 'Failed to tag package. SVN error: '+e;
          end;
      end;
    finally
      DeleteDirectory(FTempDir, false);
    end;
    Result := True;
  finally
    FSVNRepository.Free;
  end;
end;

{ TRepoSvnUpdatePackageCommand }

procedure TRepoSvnUpdatePackageCommand.UpdatePackageFiles;
var
  FileSearcher: TFileSearcher;
begin
  FileSearcher := TFileSearcher.Create;
  try
    FileSearcher.FollowSymLink := false;
    FileSearcher.OnFileFound := @UpdatePackageFileSearcherFileFound;
    FileSearcher.OnDirectoryFound := @UpdatePackageFileSearcherDirectoryFound;
    FileSearcher.Search(FTempDir+'src');
  finally
    FileSearcher.Free;
  end;
end;

function TRepoSvnUpdatePackageCommand.HasUpdates: Boolean;
var
  XMLDoc: TXMLDocument;
  StatusNode: TDOMNode;
  TargetNode: TDOMNode;
  EntryNode: TDOMNode;
  e: string;
begin
  Result := False;
  XMLDoc := FSVNRepository.RunSvnXML(['status','--xml',FTempDir+'checkout'], e);
  if Assigned(XMLDoc) then
    begin
    try
      StatusNode := XMLDoc.FindNode('status');
      if Assigned(StatusNode) then
        begin
        TargetNode := StatusNode.FindNode('target');
        if Assigned(TargetNode) then
          begin
          EntryNode:=TargetNode.FindNode('entry');
          Result := Assigned(EntryNode);
          end;
        end;
    finally
      XMLDoc.Free;
    end;
    end;
end;

procedure TRepoSvnUpdatePackageCommand.UpdatePackageFileSearcherDirectoryFound(FileIterator: TFileIterator);
var
  NewLoc: string;
  NewFile: Boolean;
  s,e: string;
begin
  NewLoc:=StringReplace(FileIterator.FileName, FTempDir+'src', FTempDir+'checkout', []);
  NewFile:=not DirectoryExists(NewLoc);
  if NewFile then
    begin
    CreateDir(NewLoc);
    if FSVNRepository.RunSvn(['add','--depth=empty',NewLoc], s, e) <> 0 then
      raise Exception.CreateFmt('Failed to svn-add directory %s. Msg: %s',[NewLoc, e]);
    end;
end;

procedure TRepoSvnUpdatePackageCommand.UpdatePackageFileSearcherFileFound(FileIterator: TFileIterator);
var
  NewLoc: string;
  NewFile: Boolean;
  s,e: string;
begin
  NewLoc:=StringReplace(FileIterator.FileName, FTempDir+'src', FTempDir+'checkout', []);
  NewFile:=not FileExists(NewLoc);
  CopyFile(FileIterator.FileName, NewLoc, [cffCreateDestDirectory,cffOverwriteFile,cffPreserveTime],true);
  if NewFile then
    begin
    if FSVNRepository.RunSvn(['add',NewLoc], s, e) <> 0 then
      raise Exception.CreateFmt('Failed to svn-add file %s. Msg: %s',[NewLoc, e]);
    end;
end;

class function TRepoSvnUpdatePackageCommand.TextName: string;
begin
  result := 'updatepackage';
end;

function TRepoSvnUpdatePackageCommand.DoExecuteRepoCommand(AController: TDCSCustomController; out ReturnMessage: string): Boolean;
var
  Package: TFPPackage;
  s: string;
begin
  Result := False;
  if FCommitMessage='' then
    begin
    ReturnMessage := 'Missing commitmessage parameter';
    Exit;
    end;
  try
    FSVNRepository := TSVNRepository.Create(FDistributor, AController as TRepoController, UID);
    try
      FTempDir := IncludeTrailingPathDelimiter(GetTempFilename(FTempDir, 'fppkg_'));
      CreateDir(FTempDir);
      try
        if DownloadPackage and UnzipPackage then
          begin
          Package := FSVNRepository.GetPackageFromManifestFile(FTempDir+'src'+PathDelim+'manifest.xml');
          try
            if not FSVNRepository.DoesPackageExist(Package.Name, '', '') then
              begin
              ReturnMessage := Format('Package "%s" does not exist.',[Package.Name]);
              Exit;
              end;
            if FSVNRepository.RunSvn(['co',TRepoController(AController).SvnUrl+'packages/'+Package.Name+'/'+FRepositoryFPCVersion+'/branche',FTempDir+'checkout'],s)=0 then
              begin
              UpdatePackageFiles;
              if HasUpdates then
                begin
                if (FSVNRepository.RunSvn(['commit',FTempDir+'checkout','-m '+QuotedStr(CommitMessage)+''],s)=0) then
                  Result := True;
                end
              else
                begin
                ReturnMessage := 'No changes';
                end;
              end;
          finally
            Package.Free;
          end;
          end;
      finally
        DeleteDirectory(FTempDir, false);
      end;
    finally
      FSVNRepository.Free;
    end;
  except
    on E: Exception do
      ReturnMessage := E.Message;
  end
end;

{ TRepoCustomPackageSvnCommand }

function TRepoCustomPackageSvnCommand.DownloadPackage: Boolean;
begin
  Result := False;
  FPackageZipFile := FTempDir + 'package.zip';
  try
    FDistributor.Log(Format('Downloading package from "%s"',[PackageURL]), etInfo, FUID);
    DownloadFile(PackageURL, FPackageZipFile);
    Result := True;
  except
    on E: Exception do
      FDistributor.Log(Format('Failed to dowload package from "%s". Message: %s',[PackageURL, FPackageZipFile, E.Message]), etWarning, UID);
  end;
end;

function TRepoCustomPackageSvnCommand.UnzipPackage: Boolean;
begin
  Result := False;
  try
    FDistributor.Log('Unzipping package in "%s"', etInfo, FUID);
    With TUnZipper.Create do
      try
        OutputPath:=FTempDir+'src';
        UnZipAllFiles(FPackageZipFile);
        Result := true;
      Finally
        Free;
      end;
  except
    on E: Exception do
      FDistributor.Log(Format('Failed to unzip package. Message: %s',[E.Message]), etWarning, UID);
  end;
end;

{ TSVNRepository }

constructor TSVNRepository.Create(ADistributor: TDCSDistributor; AController: TRepoController;
  AnUID: Variant);
begin
  FController := AController;
  FDistributor := ADistributor;
  FUID := AnUID;
end;

function TSVNRepository.RunSvn(Params: array of string; Output, ErrOutput: TStream): Integer;
const
  BUF_SIZE = 2048;
var
  SVNExe: string;
  SVNProcess: TProcess;
  i: Integer;
  BytesReadO: longint;
  BytesReadE: longint;
  Buffer: array[1..BUF_SIZE] of byte;
begin
  Result := -1;
  SVNExe := ExeSearch('svn',GetEnvironmentVariable('PATH'));
  if not FileExists(SVNExe) then
    raise exception.create('Subversion client not found.');

  SVNProcess := TProcess.Create(nil);
  try
    SVNProcess.Executable := SVNExe;
    for i := 0 to high(Params) do
      SVNProcess.Parameters.Add(Params[i]);
    SVNProcess.Options := [poUsePipes];

    SVNProcess.Execute;

    repeat
    BytesReadO := SVNProcess.Output.Read(Buffer, BUF_SIZE);
    Output.Write(Buffer, BytesReadO);

    BytesReadE := SVNProcess.Stderr.Read(Buffer, BUF_SIZE);
    ErrOutput.Write(Buffer, BytesReadE);
    until (BytesReadO = 0) and (BytesReadE = 0) and (not SVNProcess.Running);  // Stop if no more data is available
    i := SVNProcess.ExitStatus;
    Result :=  SVNProcess.ExitCode;//SVNProcess.ExitStatus;
  finally
    SVNProcess.Free;
  end;
end;

function TSVNRepository.RunSvn(Params: array of string; out Output, ErrOutput: string): Integer;
var
  Stream, ErrStream: TStringStream;
begin
  Stream := TStringStream.Create('');
  ErrStream := TStringStream.Create('');
  try
    Result := RunSvn(Params, Stream, ErrStream);
    Output := Stream.DataString;
    ErrOutput := ErrStream.DataString;
  finally
    ErrStream.Free;
    Stream.Free;
  end;
end;

function TSVNRepository.RunSvn(Params: array of string; out Output: string): Integer;
var
  e: string;
begin
  Result := RunSvn(Params, Output, e);
  if e <> '' then
    FDistributor.Log('SVN Problem: '+e, etWarning, UID);
end;

function TSVNRepository.RunSvnXML(Params: array of string; out ErrOutput: string): TXMLDocument;
var
  Output: TStream;
  ErrOutputStream: TStringStream;
begin
  Result := nil;

  Output := TMemoryStream.Create;
  ErrOutputStream := TStringStream.Create;
  try
    if RunSvn(Params, Output, ErrOutputStream) = 0 then
      begin
      Output.Seek(0, soFromBeginning);
      ReadXMLFile(Result, Output);
      ErrOutput:='';
      end
    else
      ErrOutput := ErrOutputStream.DataString;
  finally
    ErrOutputStream.Free;
    Output.Free;
  end;
end;

function TSVNRepository.DoesSVNPathExist(SvnUrl: string): Boolean;
var
  s,e: string;
  i: integer;
begin
  i := RunSvn(['--xml','ls',SvnUrl],s,e);
  if i <> 0 then
    begin
    if (pos('W160013',e)>0) then
      Result := False
    else if e <> '' then
      raise Exception.Create('Subversion problem: '+e)
    else
      raise Exception.Create('Problem running subversion.');
    end
  else
    Result := True;
end;

function TSVNRepository.DoesPackageExist(PackageName: string; FPCVersion, Version: string): Boolean;
var
  url: string;
begin
  if Version='' then
    url := FController.SvnUrl + 'packages/'+PackageName
  else
    url := FController.SvnUrl + 'packages/'+PackageName+'/'+FPCVersion+'/tags/'+Version;
  Result := DoesSVNPathExist(url);
end;

function TSVNRepository.GetPackageFromManifestFile(AFileName: string): TFPPackage;
begin
  Result := LoadManifestFromFile(AFileName);
end;

{ TRepoSvnAddPackageCommand }

class function TRepoSvnAddPackageCommand.TextName: string;
begin
  result := 'addpackage';
end;

function TRepoSvnAddPackageCommand.DoExecuteRepoCommand(AController: TDCSCustomController; out ReturnMessage: string): Boolean;
var
  Package: TFPPackage;
  PackageDir: string;
  s: string;
begin
  Result := False;
  try
    FSVNRepository := TSVNRepository.Create(FDistributor,AController as TRepoController, UID);
    try
      FTempDir := IncludeTrailingPathDelimiter(GetTempFilename(FTempDir, 'fppkg_'));
      CreateDir(FTempDir);
      try
        if DownloadPackage and UnzipPackage then
          begin
          Package := FSVNRepository.GetPackageFromManifestFile(FTempDir+'src'+PathDelim+'manifest.xml');
          try
            if FSVNRepository.DoesPackageExist(Package.Name, '', '') then
              begin
              ReturnMessage := Format('Package "%s" does already exist.',[Package.Name]);
              Exit;
              end;
            if FSVNRepository.RunSvn(['co','--depth=empty',TRepoController(AController).SvnUrl+'packages',FTempDir+'checkout'],s)=0 then
              begin
              PackageDir := FTempDir+'checkout'+PathDelim+Package.Name+PathDelim+RepositoryFPCVersion+PathDelim;
              ForceDirectories(PackageDir+'branche');
              ForceDirectories(PackageDir+'tags');
              CopyDirTree(FTempDir+'src',PackageDir+'branche');
              if CommitMessage='' then
                CommitMessage := 'Import of package '+Package.Name+ ' version ' +Package.Version.AsString;
              if (FSVNRepository.RunSvn(['add',FTempDir+'checkout'+PathDelim+Package.Name],s)=0) and
                 (FSVNRepository.RunSvn(['commit',FTempDir+'checkout','-m '+QuotedStr(CommitMessage)+''],s)=0) then
                Result := True;
              end;
          finally
            Package.Free;
          end;
          end;
      finally
        DeleteDirectory(FTempDir, false);
      end;
    finally
      FSVNRepository.Free;
    end;
  except
    on E: Exception do
      ReturnMessage := E.Message;
  end
end;

initialization
  TDCSThreadCommandFactory.RegisterCommandClass(TRepoSvnAddPackageCommand);
  TDCSThreadCommandFactory.RegisterCommandClass(TRepoSvnUpdatePackageCommand);
  TDCSThreadCommandFactory.RegisterCommandClass(TRepoTagPackageSvnCommand);
  TDCSThreadCommandFactory.RegisterCommandClass(TRepoReleasePackageSvnCommand);
  TDCSThreadCommandFactory.RegisterCommandClass(TPublishRepository);
end.

