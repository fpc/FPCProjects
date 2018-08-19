unit prPackageWebModule;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  Classes,
  process,
  httpdefs,
  FileUtil,
  odataservice,
  DOM,
  XMLRead,
  dcsGlobalSettings,
  fpHTTP,
  fphttpclient,
  fpjson,
  fpmkunit,
  fprepos,
  pkgrepos,
  zipper,
  jsonparser,
  fprPackageUtils,
  fprBuildAgentResponse,
  fprFPCVersion,
  fphttpserver,
  fprWebModule;

type

  { TprPackageWM }

  TprPackageWM = class(TfprWebModule)
    Procedure DataModuleRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse; Var Handled: Boolean);
  private
    function GetFPCVersionCollection: TfprFPCVersionCollection;
    procedure SetGitUserAndEmail(ARepoPath: string);
    function GetPackageRepoPath(APackageName: string): string;
    procedure AddGITRepositoryForNewPackage(APackageName: string);
    function CheckUploadedSourceArchive(APackageName: string; AFPCVersion: TfprFPCVersion; AFile: TUploadedFile; out ErrorStr: string): Boolean;
    function AddSourcesToGITRepository(APackageName: string; AFPCVersion: TfprFPCVersion; AFile: TUploadedFile): string;
    procedure RunGit(const curdir:string; const desc: string; const commands:array of string;out outputstring:string);
    function CheckIfGitBranchExists(const curdir, BranchName: string): Boolean;
    function CheckIfGitHasUnstagedChanges(const curdir: string): Boolean;
    function TagPackage(PackageName, TagMessage: string; FPCVersion: TfprFPCVersion; GITTag: string = ''): string;
    procedure DownloadTAGSource(AResponse: TResponse; APackageName, ATag: string);
    procedure CloneGITPackage(APackageName, TmpPath: string);
    function GetVersionFromPathInfo(APathInfo: string): TfprFPCVersion;
  public
    constructor Create(AOwner: TComponent); override;

  end;

var
  prPackageWM: TprPackageWM;

implementation

{$R *.lfm}

{ TprPackageWM }

Procedure TprPackageWM.DataModuleRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse; Var Handled: Boolean);
var
  PackageName: string;
  PackageURL: string;
  PackageJSON: TJSONData;
  PackageObject: TJSONObject;
  Command: string;
  RevHash: string;
  PackageTag, TagMessage: string;
  FPCVersion: TfprFPCVersion;
  s: string;
  IsNew: Boolean;
begin
  Handled := True;
  PackageName := ARequest.GetNextPathInfo;

  PackageURL := TDCSGlobalSettings.GetInstance.GetSettingAsString('packagemanagerurl') + '/package/'+PackageName;

  try
    PackageJSON := ObtainJSONRestRequest(PackageURL, True);
  Except
    on E: Exception do
      begin
      if E is EHTTPClient then
        begin
        // Probably.... 404
        AResponse.Code := 404;
        AResponse.CodeText := GetStatusCode(AResponse.Code);
        Exit;
        end
      else
        Raise;
      end;
  end;
  if PackageJSON.JSONType = jtObject then
    begin
    PackageObject := PackageJSON as TJSONObject;
    Command := ARequest.GetNextPathInfo;
    if Command = 'tagpackage' then
      begin
      if (PackageObject.Get('ownerid', '') <> FSubjectId) and (GetUserRole<>'admin') then
        raise Exception.Create('You have not enough rights to add a tag for this package');

      if (PackageObject.Get('packagestate', '') <> 'approved') and (PackageObject.Get('packagestate', '') <> 'published') then
        raise Exception.Create('To be able to add tags package has to be approved or published.');

      TagMessage := ARequest.QueryFields.Values['message'];
      if TagMessage = '' then
        raise Exception.Create('Missing message');

      FPCVersion := GetVersionFromPathInfo(ARequest.GetNextPathInfo);

      PackageTag := TagPackage(PackageName, TagMessage, FPCVersion);

      AResponse.Content := '{"tag": "'+PackageTag+'"}';
      AResponse.Code := 200;
      end
    else if ARequest.Method='POST' then
      begin
      if (PackageObject.Get('ownerid', '') <> FSubjectId) and (GetUserRole<>'admin') then
        raise Exception.Create('You have not enough rights to upload sources for this package');

      if ARequest.Files.Count <> 1 then
        raise Exception.Create('Missing package in request');

      s := Command;
      FPCVersion := GetVersionFromPathInfo(Command);

      if not CheckUploadedSourceArchive(PackageName, FPCVersion, ARequest.Files.First, s) then
        begin
        raise Exception.Create('Validity check on source failed: ' + s);
        end;

      IsNew := PackageObject.Get('packagestate', '') = 'new';
      if IsNew then
        AddGITRepositoryForNewPackage(PackageName);

      RevHash := AddSourcesToGITRepository(PackageName, FPCVersion, ARequest.Files.First);

      if IsNew then
        TagPackage(PackageName, 'Initial version', FPCVersion, '0.0.0.0');

      AResponse.Content := '{"sourcehash": "'+RevHash+'"}';
      AResponse.Code := 200;
      end
    else if Command <> '' then
      begin
      DownloadTAGSource(AResponse, PackageName, Command);
      AResponse.Code := 200;
      end
    else
      begin
      AResponse.Code := 405;
      end;
    AResponse.CodeText := GetStatusCode(AResponse.Code);
    end
  else
    raise Exception.Create('Invalid response from PackageManager.');
end;

function TprPackageWM.GetFPCVersionCollection: TfprFPCVersionCollection;
begin
  Result := TfprFPCVersionCollection.Instance;
  if Result.Count = 0 then
    begin
    if not JSONObjectRestRequest(IncludeHTTPPathDelimiter(TDCSGlobalSettings.GetInstance.GetSettingAsString('packagemanagerurl'))+'fpcversion', True, Result) then
      raise Exception.Create('Failed to get FPC version list');
    end;
end;

procedure TprPackageWM.SetGitUserAndEmail(ARepoPath: string);
var
  CmdRes: string;
begin
  RunGit(ARepoPath, 'set user-email', ['config', '--global', 'user.email', QuotedStr(TDCSGlobalSettings.GetInstance.GetSettingAsString('GITEmail'))], CmdRes );
  RunGit(ARepoPath, 'set user-name', ['config', '--global', 'user.name', QuotedStr(TDCSGlobalSettings.GetInstance.GetSettingAsString('GITUserName'))], CmdRes );
end;

function TprPackageWM.GetPackageRepoPath(APackageName: string): string;
var
  GITRepositoriesPath: String;
begin
  GITRepositoriesPath := TDCSGlobalSettings.GetInstance.GetSettingAsString('GITRepositoriesPath');
  Result := ConcatPaths([GITRepositoriesPath, ChangeFileExt(APackageName, '.git')]);
end;

procedure TprPackageWM.AddGITRepositoryForNewPackage(APackageName: string);
var
  GITRepositoriesPath: string;
  PackageRepoPath: string;
  TmpPath: string;
  CmdRes: string;
  CmdExit: Integer;
begin
  GITRepositoriesPath := TDCSGlobalSettings.GetInstance.GetSettingAsString('GITRepositoriesPath');
  PackageRepoPath := GetPackageRepoPath(APackageName);
  if DirectoryExists(PackageRepoPath) then
    raise Exception.CreateFmt('A repository for package %s already exists, while the package is new.', [APackageName]);

  TmpPath := GetTempFileName(GITRepositoriesPath, '__newpackage_'+APackageName+'_');
  if ForceDirectories(TmpPath) then
    begin
    try
      if RunCommandInDir(TmpPath, 'git', ['init', '--bare'], CmdRes, CmdExit) <> 0 then
        Raise Exception.Create('Failed to execute GIT to create GIT repository');
      if CmdExit <> 0 then
        Raise Exception.Create('Failed to create GIT repository');

      RenameFile(TmpPath, PackageRepoPath);

      SetGitUserAndEmail(PackageRepoPath);
    finally
      DeleteDirectory(TmpPath, False);
    end;
    end
  else
    raise Exception.Create('Failed to create temporary path');
end;

function TprPackageWM.CheckUploadedSourceArchive(APackageName: string; AFPCVersion: TfprFPCVersion;
  AFile: TUploadedFile; out ErrorStr: string): Boolean;
var
  BuildAgentResponseList: TfprBuildAgentResponseList;
  BuildAgentResponse: TfprBuildAgentResponse;
  s: TJSONUnicodeStringType;
  URL: string;
begin
  Result := True;
  try
    URL := RetrieveBuildAgentURL(AFPCVersion.Name);
    AFile.Stream.Seek(0, soFromBeginning);
    URL := URL + 'manifest?cputarget=x86_64&ostarget=linux&fpcversion='+AFPCVersion.Name+'&chunked=false';
    BuildAgentResponseList := TfprBuildAgentResponseList.Create;
    try
      if not JSONObjectRestRequest(URL, True, BuildAgentResponseList, 'POST', AFile.Stream) then
        begin
        Result := False;
        ErrorStr := 'Call to buildagent failed.';
        Exit;
        end;
      BuildAgentResponse := BuildAgentResponseList.Items[BuildAgentResponseList.Count -1];

      if (BuildAgentResponse.AType <> 'Done') or not Assigned(BuildAgentResponse.Manifest) then
        begin
        Result := False;
        ErrorStr := 'Manifest-creation failed. (' + BuildAgentResponse.Message + ')';
        Exit;
        end;
      s := ((BuildAgentResponse.Manifest as TJSONArray).Items[0] as TJSONObject).Get('name', '');
      if not SameText(APackageName, s) then
        begin
        ErrorStr := Format('Package-name of source archive (%s) does not match package-name (%s).', [s, APackageName]);
        Result := False;
        end;
    finally
      BuildAgentResponseList.Free;
    end;

  except
    on E: Exception do
      begin
      raise EHTTPClient.CreateHelp('Problem while checking the validity of the package: ' + E.Message, 500);
      end;
  end;
end;

function TprPackageWM.AddSourcesToGITRepository(APackageName: string; AFPCVersion: TfprFPCVersion; AFile: TUploadedFile): string;
var
  TmpPath: string;
  FS: TFileStream;
  ArchiveName: string;
  ClonePath: string;
  CmdRes: string;
  UnZipper: TUnZipper;
  BranchIsNew: Boolean;
begin
  BranchIsNew := False;
  TmpPath := GetTempFileName;
  ForceDirectories(TmpPath);
  try
    CloneGITPackage(APackageName, TmpPath);
    ClonePath := ConcatPaths([TmpPath, 'pkgclone']);

    if not CheckIfGitBranchExists(ClonePath, AFPCVersion.GetBranchName) then
      begin
      // Create branch for specific version
      RunGit(ClonePath, 'create branch ' + AFPCVersion.GetBranchName, ['checkout', '-b' + AFPCVersion.GetBranchName], CmdRes);
      BranchIsNew := True;
      end
    else
      begin
      RunGit(ClonePath, 'switch to branch ' + AFPCVersion.Name, ['checkout', AFPCVersion.GetBranchName], CmdRes);
      end;

    ArchiveName := ConcatPaths([TmpPath, 'package.zip']);
    AFile.Stream.Seek(0, soFromBeginning);
    FS := TFileStream.Create(ArchiveName, fmCreate);
    try
      FS.CopyFrom(AFile.Stream, AFile.Size);
    finally
      FS.Free;
    end;

    UnZipper := TUnZipper.Create;
    try
      UnZipper.FileName := ArchiveName;
      UnZipper.OutputPath := ClonePath;
      UnZipper.UnZipAllFiles();
    finally
      UnZipper.Free;
    end;

    if CheckIfGitHasUnstagedChanges(ClonePath) then
      begin
      RunGit(ClonePath, 'stage all files', ['add', '--all'], CmdRes);

      RunGit(ClonePath, 'commit changes', ['commit', '-m ''' + 'Commit new source-zip' + ''''], CmdRes);

      if BranchIsNew then
        RunGit(ClonePath, 'push branch ' + AFPCVersion.GetBranchName, ['push', 'origin', AFPCVersion.GetBranchName], CmdRes)
      else
        RunGit(ClonePath, 'push changes', ['push'], Result);
      end
    else
      begin
      if BranchIsNew then
        begin
        RunGit(ClonePath, 'push branch ' + AFPCVersion.GetBranchName, ['push', 'origin', AFPCVersion.GetBranchName], CmdRes)
        end
      else
        raise Exception.Create('This archive does not contain any changes compared to the version within this repository');
      end;

    RunGit(ClonePath, 'get revision', ['rev-parse', 'HEAD'], Result);
    Result := Trim(Result);
  finally
    DeleteDirectory(TmpPath, False);
  end;
end;

procedure TprPackageWM.RunGit(const curdir:string; const desc: string; const commands:array of string;out outputstring:string);
var
  ExitStatus, i: Integer;
  CommandLine: String;
begin
  if RunCommandInDir(curdir, 'git', commands, outputstring, ExitStatus, [poStderrToOutPut]) <> 0 then
    begin
    CommandLine := 'git';
    for i := 0 to length(Commands) -1 do
      CommandLine := CommandLine + ' ' + Commands[i];
    raise Exception.CreateFmt('Failed to execute GIT to %s.' +sLineBreak+ 'Current directory: ' +Curdir+ sLineBreak + 'command line: ' + CommandLine, [desc]);
    end;
  if ExitStatus <> 0 then
    Raise Exception.Create('Failed to ' + desc + '. ' + outputstring);
end;

function TprPackageWM.CheckIfGitBranchExists(const curdir, BranchName: string): Boolean;
var
  ExitStatus: Integer;
  outputstring: string;
begin
  // This line is to check for local branches. We need to search for remote branches
  // here.
  // if RunCommandInDir(curdir, 'git', ['show-ref', '--verify', '--quiet', 'refs/heads/' + BranchName], outputstring, ExitStatus, [poStderrToOutPut]) <> 0 then
  if RunCommandInDir(curdir, 'git', ['ls-remote', '--heads', '--quiet', '--exit-code','origin', BranchName], outputstring, ExitStatus, [poStderrToOutPut]) <> 0 then
    Raise Exception.Create('Failed to execute GIT to check if branch ' + BranchName + ' exists');
  Result := ExitStatus = 0;
end;

function TprPackageWM.CheckIfGitHasUnstagedChanges(const curdir: string): Boolean;
var
  ExitStatus: Integer;
  outputstring: string;
begin
  if RunCommandInDir(curdir, 'git', ['update-index', '--refresh'], outputstring, ExitStatus, [poStderrToOutPut]) <> 0 then
    Raise Exception.Create('Failed to run GIT update-index');
  if RunCommandInDir(curdir, 'git', ['diff-files', '--quiet'], outputstring, ExitStatus, []) <> 0 then
    Raise Exception.Create('Failed to execute GIT to check for unstaged changes');
  Result := ExitStatus <> 0;
  if not Result then
    begin
    RunGit(curdir, 'check for untracked files', ['ls-files', '--exclude-standard', '--others'], outputstring);
    Result := outputstring <> '';
    end;
end;

function TprPackageWM.TagPackage(PackageName, TagMessage: string; FPCVersion: TfprFPCVersion;
  GITTag: string): string;
var
  TmpPath: string;
  ClonePath: string;
  CmdRes, NotifyPackageManagerTagAddedURL: string;
  VersionJSON: TJSONObject;
  VersionNumberJSON: TJSONObject;
  TagAdded: string;
  ResponseJSON: TJSONData;
  StrStream: TStringStream;
  JSO: TJSONObject;
  Package: TFPPackage;
begin
  TmpPath := GetTempFileName;
  ClonePath := ConcatPaths([TmpPath, 'pkgclone']);
  ForceDirectories(TmpPath);
  try
    CloneGITPackage(PackageName, TmpPath);
    RunGit(ClonePath, 'switch to branch ' + FPCVersion.Name, ['checkout', FPCVersion.GetBranchName], CmdRes);

    Package := LoadManifestFromFile(ConcatPaths([ClonePath, 'manifest.xml']));
    try
      VersionJSON := TJSONObject.Create();
      try
        if GITTag = '' then
          GITTag := 'FPC' + FPCVersion.Name + '_v' + Package.Version.AsString;

        VersionJSON.Add('tag', GITTag);
        Result := GITTag;

        VersionJSON.Add('filename', Package.FileName);
        VersionJSON.Add('author', Package.Author);
        VersionJSON.Add('license', Package.License);
        VersionJSON.Add('homepageurl', Package.HomepageURL);
        VersionJSON.Add('email', Package.Email);
        VersionJSON.Add('description', Package.Description);
        VersionJSON.Add('fpcversion', FPCVersion.Name);

        VersionNumberJSON := TJSONObject.Create;
        try
          VersionNumberJSON.Add('major', Package.Version.Major);
          VersionNumberJSON.Add('minor', Package.Version.Minor);
          VersionNumberJSON.Add('micro', Package.Version.Micro);
          VersionNumberJSON.Add('build', Package.Version.Build);
          VersionJSON.Add('version', VersionNumberJSON);
          VersionNumberJSON := nil;
        finally
          VersionNumberJSON.Free;
        end;

        TagAdded := VersionJSON.AsJSON;
      finally
        VersionJSON.Free;
      end;
    finally
      Package.Free;
    end;

    RunGit(ClonePath, 'tag new version', ['tag', '-a', GITTag, '-m', '''' +TagMessage + ''''], CmdRes);
    NotifyPackageManagerTagAddedURL := TDCSGlobalSettings.GetInstance.GetSettingAsString('packagemanagerurl') + '/package/'+PackageName+'/version';

    StrStream := TStringStream.Create(TagAdded);
    try
      ResponseJSON := ObtainJSONRestRequest(NotifyPackageManagerTagAddedURL, True, 'POST', StrStream);
      try
        if ResponseJSON.JSONType <> jtObject then
          raise Exception.Create('Invalid response from package-manager while adding tag.');
        JSO := TJSONObject(ResponseJSON).Get('error', TJSONObject(nil));
        if Assigned(JSO) then
          raise Exception.Create('Failed to add tag to package-manager. ' + JSO.Get('msg', 'Unknown failure.'));

      finally
        ResponseJSON.Free;
      end;
    finally
      StrStream.Free;
    end;

    RunGit(ClonePath, 'push tag', ['push', 'origin', GITTag], CmdRes);

  finally
    DeleteDirectory(TmpPath, False);
  end;
end;

procedure TprPackageWM.DownloadTAGSource(AResponse: TResponse; APackageName, ATag: string);
var
  TmpPath: string;
  FS: TFileStream;
  MS: TMemoryStream;
  ArchiveName: string;
  ClonePath: string;
  CmdRes: string;
begin
  TmpPath := GetTempFileName;
  ForceDirectories(TmpPath);
  try
    CloneGITPackage(APackageName, TmpPath);

    ClonePath := ConcatPaths([TmpPath, 'pkgclone']);
    ArchiveName := ConcatPaths([TmpPath, 'package.zip']);
    RunGit(ClonePath, 'archive git repository', ['archive', '--output', ArchiveName, '--format', 'zip', ATag], CmdRes );

    FS := TFileStream.Create(ArchiveName, fmOpenRead);
    try
      MS := TMemoryStream.Create;
      try
        MS.CopyFrom(FS, FS.Size);

        AResponse.ContentStream := MS;
        AResponse.FreeContentStream := True;
        MS := nil;
      finally
        MS.Free;
      end;
    finally
      FS.Free;
    end;
    AResponse.Code := 200;
    AResponse.ContentType := 'application/zip';
  finally
    DeleteDirectory(TmpPath, False);
  end;
end;

procedure TprPackageWM.CloneGITPackage(APackageName, TmpPath: string);
var
  RepoPath, CmdRes: String;
begin
  RepoPath := GetPackageRepoPath(APackageName);

  if not FileExists(RepoPath) then
    raise Exception.CreateFmt('Repository for package %s does not exist.', [APackageName]);

  RunGit(TmpPath, 'clone git repository', ['clone', RepoPath, 'pkgclone'], CmdRes );
  SetGitUserAndEmail(RepoPath);
end;

function TprPackageWM.GetVersionFromPathInfo(APathInfo: string): TfprFPCVersion;
begin
  if APathInfo='' then
    Result := GetFPCVersionCollection.DefaultVersion
  else
    begin
    Result := GetFPCVersionCollection.FindVersion(APathInfo);
    if not Assigned(Result) then
      raise Exception.CreateFmt('Unknown FPC-Version [%s]', [APathInfo]);
    end;
end;

constructor TprPackageWM.Create(AOwner: TComponent);
var
  GlobalSettings: TDCSGlobalSettings;
begin
  inherited Create(AOwner);

  GlobalSettings := TDCSGlobalSettings.GetInstance;
  if GlobalSettings.GetSettingAsString('AllowCorsOrigin') <> '' then
    AddCorsOrigin(GlobalSettings.GetSettingAsString('AllowCorsOrigin'), 'POST, GET', '', True);
end;

initialization
  RegisterHTTPModule('package', TprPackageWM);
end.

