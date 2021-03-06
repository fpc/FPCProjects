unit prPackageWebModule;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  Classes,
  process,
  httpdefs,
  FileUtil,
  DateUtils,
  odataservice,
  dcsGlobalSettings,
  csModel,
  csJSONRttiStreamHelper,
  fpHTTP,
  fphttpclient,
  httproute,
  fpjson,
  fpmkunit,
  fprepos,
  pkgrepos,
  zipper,
  jsonparser,
  TLevelUnit,
  cnocStackMessageTypes,
  cnocStackHandlerThread,
  cnocStackJSONHandlerThread,
  fprLog,
  fprSerializer,
  fprErrorHandling,
  fprBuildAgentResponse,
  fprAuthenticationHandler,
  fprInterfaceClasses,
  fprFPCVersion,
  fphttpserver,
  fprWebHandler,
  fprWebModule;

type

  { TprPackageWM }

  TprPackageWM = class(TfprWebHandler, IRouteInterface, IcnocStackHandler, IcnocStackJSONHandleMessage)
  protected
    FPackageObject: TfprPackage;

    function DoHandleRawRequest(ARequest: TRequest; AResponse: TResponse): Boolean; override;

    function DoHandleRequest(ARequest : TRequest; JSONContent: TJSONData): TJSONData; override;
    procedure HandleJSONMessage(const IncomingMessage: PcnocStackMessage; const JSONData: TJSONObject; var Handled: Boolean);

    function HandleRepositoryRequest(
      Method,
      PackageName,
      Command,
      FPCVersionStr,
      TagMessage,
      RevHash,
      AccessToken: string;
      Files: TUploadedFiles): TJSONData;
  private
    function GetFPCVersionCollection(AccessToken: string): TfprFPCVersionCollection;
    procedure SetGitUserAndEmail(ARepoPath: string);
    function GetPackageRepoPath(APackageName: string): string;
    function GetAndCheckPackageRepoPath(APackageName: string): string;
    procedure AddGITRepositoryForNewPackage(APackageName: string);
    function CheckUploadedSourceArchive(APackage: TfprPackage; AFPCVersion: TfprFPCVersion; AFile: TUploadedFile; AccessToken: string; out ErrorStr: string): Boolean;
    function AddSourcesToGITRepository(APackageName: string; AFPCVersion: TfprFPCVersion; AFile: TUploadedFile): string;
    procedure RunGit(const curdir:string; const desc: string; const commands:array of string;out outputstring:string);
    function CheckIfGitBranchExists(const curdir, BranchName: string): Boolean;
    function CheckIfGitHasUnstagedChanges(const curdir: string): Boolean;
    function TagPackage(PackageName, TagMessage: string; FPCVersion: TfprFPCVersion; GITTag, AccessToken: string; Hash: string = ''): string;
    procedure DownloadTAGSource(AResponse: TResponse; APackageName, ATag: string);
    procedure CloneGITPackage(APackageName, TmpPath: string);
    function ObtainGITLogForPackage(APackageName, Branch: string): TfprPackageRepoLogCollection;
    function GetVersionFromPathInfo(APathInfo, AccessToken: string): TfprFPCVersion;
    function GetAccessToken(var ARequest: TRequest): String;
  public
    constructor Create(); override;

  end;

implementation

{ TprPackageWM }

function TprPackageWM.HandleRepositoryRequest(
  Method,
  PackageName,
  Command,
  FPCVersionStr,
  TagMessage,
  RevHash,
  AccessToken: string;
  Files: TUploadedFiles): TJSONData;
var
  PackageTag, ErrString: string;
  FPCVersion: TfprFPCVersion;
  IsNew: Boolean;
  RepoLogCollection: TfprPackageRepoLogCollection;
begin
  if assigned(FPackageObject) then
    begin
    try
      if Command = 'tagpackage' then
        begin
        if (FPackageObject.OwnerId <> TfprAuthenticationHandler.GetInstance.GetSubject(AccessToken)) and (TfprAuthenticationHandler.GetInstance.GetUserRole(AccessToken)<>'admin') then
          raise Exception.Create('You have not enough rights to add a tag for this package');

        if (FPackageObject.PackageState in [prspsRevoked]) then
          raise Exception.Create('To be able to add tags package has to be approved or published.');

        if TagMessage = '' then
          raise Exception.Create('Missing message');

        FPCVersion := GetVersionFromPathInfo(FPCVersionStr, AccessToken);

        PackageTag := TagPackage(PackageName, TagMessage, FPCVersion, '', AccessToken, RevHash);

        Result := TJSONObject.Create(['tag', PackageTag]);
        end
      else if (Command='list') and (Method='GET') then
        begin
        FPCVersion := GetVersionFromPathInfo(FPCVersionStr, AccessToken);
        RepoLogCollection := ObtainGITLogForPackage(PackageName, FPCVersion.GetBranchName);
        try
          Result := TJSONRttiStreamHelper.ObjectToJSON(RepoLogCollection, nil, [tcsdfCollectionAsList]);
        finally
          RepoLogCollection.Free;
        end;
        end
      else
        raise EJsonWebException.CreateHelp('Not clear what to do', 500);
    finally
      FPackageObject.Free;
    end;
    end
  else
    raise EJsonWebException.CreateHelp('Invalid response from PackageManager.', 500);
end;

function TprPackageWM.GetFPCVersionCollection(AccessToken: string): TfprFPCVersionCollection;
begin
  Result := TfprFPCVersionCollection.Instance;
  if Result.Count = 0 then
    begin
    if not JSONObjectRestRequest(IncludeHTTPPathDelimiter(TDCSGlobalSettings.GetInstance.GetSettingAsString('packagemanagerurl'))+'fpcversion', AccessToken, Result) then
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

  TmpPath := GetTempFileName(GITRepositoriesPath, '__newpackage_'+APackageName+'_');
  if ForceDirectories(TmpPath) then
    begin
    try
      TfprLog.Log(Format('Call git to initialize the new git repository for the [%s] package in [%s].', [APackageName, TmpPath]));
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
    begin
    TfprLog.Log(Format('Failed to create temporary path [%s].', [TmpPath]), nil, ERROR);
    raise Exception.Create('Failed to create temporary path');
    end;
end;

function TprPackageWM.CheckUploadedSourceArchive(APackage: TfprPackage; AFPCVersion: TfprFPCVersion; AFile: TUploadedFile; AccessToken: string; out ErrorStr: string): Boolean;
var
  BuildAgentResponseList: TfprBuildAgentResponseList;
  BuildAgentResponse: TfprBuildAgentResponse;
  PackageManifestJSONObject: TJSONObject;
  VersionJSONObject: TJSONObject;
  Version: TFPVersion;
  URL: string;
  s: TJSONUnicodeStringType;
  i: Integer;
begin
  Result := True;
  try
    URL := RetrieveBuildAgentURL(AFPCVersion.Name, AccessToken);
    AFile.Stream.Seek(0, soFromBeginning);
    URL := URL + 'manifest?cputarget=x86_64&ostarget=linux&fpcversion='+AFPCVersion.Name+'&chunked=false';
    BuildAgentResponseList := TfprBuildAgentResponseList.Create;
    try
      if not JSONObjectRestRequest(URL, AccessToken, BuildAgentResponseList, 'POST', AFile.Stream) then
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
      PackageManifestJSONObject := ((BuildAgentResponse.Manifest as TJSONArray).Items[0] as TJSONObject);
      s := PackageManifestJSONObject.Get('name', '');
      if not SameText(APackage.Name, s) then
        begin
        ErrorStr := Format('Package-name of source archive (%s) does not match package-name (%s).', [s, APackage.Name]);
        Result := False;
        end;

      // Check version of package, it may not be lower then existing versions for
      // the same fpc-version.
      VersionJSONObject := PackageManifestJSONObject.Get('version', TJSONObject(nil));
      Version := TFPVersion.Create;
      try
        TfprSerializerSingleton.Instance.JSONToObject(VersionJSONObject, Version);
        for i := 0 to APackage.PackageVersionList.Count -1 do
          begin
          if (APackage.PackageVersionList.Items[i].FPCVersion=AFPCVersion.Name) and
            (APackage.PackageVersionList.Items[i].Version.CompareVersion(Version) >= 0) then
            begin
            ErrorStr := Format('The version [%s] of package [%s] in the source archive is lower then the current release [%s].', [Version.AsString, APackage.Name, APackage.PackageVersionList.Items[i].Version.AsString]);
            Result := False;
            end;
          end;
      finally
        Version.Free;
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
  Package: TFPPackage;
  CommitMessage: string;
  PackageVersionString: string;
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

    Package := LoadManifestFromFile(ConcatPaths([ClonePath, 'manifest.xml']));
    try
      PackageVersionString := Package.Version.AsString;
      CommitMessage := 'Version ' + PackageVersionString;
    finally
      Package.Free;
    end;

    if CheckIfGitHasUnstagedChanges(ClonePath) then
      begin
      RunGit(ClonePath, 'stage all files', ['add', '--all'], CmdRes);

      RunGit(ClonePath, 'commit changes', ['commit', '-m' + CommitMessage], CmdRes);

      if BranchIsNew then
        RunGit(ClonePath, 'push branch ' + AFPCVersion.GetBranchName, ['push', '--set-upstream', 'origin', AFPCVersion.GetBranchName], CmdRes)
      else
        RunGit(ClonePath, 'push changes', ['push'], Result);
      end
    else
      begin
      if BranchIsNew then
        begin
        RunGit(ClonePath, 'push branch ' + AFPCVersion.GetBranchName, ['push', '--set-upstream', 'origin', AFPCVersion.GetBranchName], CmdRes)
        end
      else
        raise Exception.Create('This archive does not contain any changes compared to the version within this repository');
      end;

    RunGit(ClonePath, 'fetch notes', ['fetch','origin','refs/notes/*:refs/notes/*'], CmdRes);
    RunGit(ClonePath, 'Add the version as a note to the commit', ['notes','add','-m'+PackageVersionString], CmdRes);
    RunGit(ClonePath, 'push notes', ['push', 'origin', 'refs/notes/*'], CmdRes);

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
  TfprLog.Log(Format('Call git in [%s], description: [%s].', [curdir, desc]));
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
  GITTag, AccessToken: string; Hash: string = ''): string;
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

    if Hash <> '' then
      begin
      RunGit(ClonePath, 'check if branch ' + FPCVersion.GetBranchName + ' + contains commit ' + Hash, ['branch', '-r', '--contains', Hash], CmdRes);
      if Trim(CmdRes) <> 'origin/' + FPCVersion.GetBranchName then
        raise EHTTPServer.CreateFmtHelp('The given commit does not match with the FPC-version [%s] ([%s] branch)', [FPCVersion.Name, FPCVersion.GetBranchName], 400);

      RunGit(ClonePath, 'switch to branch ' + FPCVersion.Name, ['checkout', Hash], CmdRes);
      end
    else
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
      ResponseJSON := TfprWebModule.ObtainJSONRestRequest(NotifyPackageManagerTagAddedURL, AccessToken, 'POST', StrStream);
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
    RunGit(ClonePath, 'download package-archive from git', ['archive', '--output', ArchiveName, '--format', 'zip', ATag], CmdRes );

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
  RepoPath := GetAndCheckPackageRepoPath(APackageName);

  RunGit(TmpPath, 'clone git repository', ['clone', ExpandFileName(RepoPath), 'pkgclone'], CmdRes );
  SetGitUserAndEmail(RepoPath);
end;

function TprPackageWM.GetVersionFromPathInfo(APathInfo, AccessToken: string): TfprFPCVersion;
begin
  if APathInfo='' then
    Result := GetFPCVersionCollection(AccessToken).DefaultVersion
  else
    begin
    Result := GetFPCVersionCollection(AccessToken).FindVersion(APathInfo);
    if not Assigned(Result) then
      raise Exception.CreateFmt('Unknown FPC-Version [%s]', [APathInfo]);
    end;
end;

constructor TprPackageWM.Create();
var
  GlobalSettings: TDCSGlobalSettings;
begin
  inherited Create();

  GlobalSettings := TDCSGlobalSettings.GetInstance;
  if GlobalSettings.GetSettingAsString('AllowCorsOrigin') <> '' then
    AddCorsOrigin(GlobalSettings.GetSettingAsString('AllowCorsOrigin'), 'POST, GET', '', True);
end;

function TprPackageWM.ObtainGITLogForPackage(APackageName, Branch: string): TfprPackageRepoLogCollection;
var
  Collection: TfprPackageRepoLogCollection;
  RepoPath, CmdRes: String;
  LogLines, LogItems, Tags: TStringArray;
  i, j: Integer;
  RepoLogItem: TfprPackageRepoLog;
  TagsString: String;
begin
  Collection := TfprPackageRepoLogCollection.Create();
  try
    RepoPath := GetAndCheckPackageRepoPath(APackageName);
    // Check of fpc-version branch exists at all:
    try
      RunGit(RepoPath, 'check if package ' + APackageName + ' is available for branch ' + Branch, ['show-ref', '--verify', '--quiet', 'refs/heads/' + Branch], CmdRes);
    except
      Result := Collection;
      Collection := nil;
      Exit;
    end;

    RunGit(RepoPath, 'retrieve log for package '+APackageName, ['log', '--pretty=format:%h^%aI^%s^%D^%N^^', Branch], CmdRes);

    LogLines := CmdRes.Split('^^'+LineEnding);
    for i := 0 to Length(LogLines) -1 do
      begin
      LogItems := Trim(LogLines[i]).Split('^');
      RepoLogItem := Collection.Add;
      RepoLogItem.Hash := LogItems[0];
      RepoLogItem.AuthorDate := ScanDateTime('yyyy-mm-dd''T''hh:nn:ss', LogItems[1]);
      RepoLogItem.Description := LogItems[2];
      // LogItems[3] looks like: "tag: initial, FPCtrunk"
      TagsString := '';
      Tags := LogItems[3].Split(',');
      for j := 0 to High(Tags) do
        begin
        if Copy(Trim(Tags[j]), 1, 4) = 'tag:' then
          TagsString := TagsString + ', ' + Copy(Trim(Tags[j]),6);
        end;
      TagsString := Copy(TagsString, 3);
      RepoLogItem.Tags := TagsString;

      RepoLogItem.Description := LogItems[2];
      RepoLogItem.Version := Trim(LogItems[4]);
      end;
    Result := Collection;
    Collection:=nil;
  finally
    Collection.Free;
  end;
end;

function TprPackageWM.GetAndCheckPackageRepoPath(APackageName: string): string;
begin
  Result := GetPackageRepoPath(APackageName);

  if not DirectoryExists(Result) then
    begin
    TfprLog.Log(Format('Can not find the repository of package [%s] at location [%s]', [APackageName, Result]));
    raise Exception.CreateFmt('Repository for package %s does not exist.', [APackageName]);
    end;
end;

function TprPackageWM.DoHandleRequest(ARequest: TRequest; JSONContent: TJSONData): TJSONData;
var
  AccessToken: String;
begin
  AccessToken := GetAccessToken(ARequest);

  Result := HandleRepositoryRequest(
    ARequest.Method,
    ARequest.RouteParams['packagename'],
    ARequest.RouteParams['command'],
    ARequest.RouteParams['fpcversion'],
    ARequest.QueryFields.Values['message'],
    ARequest.QueryFields.Values['hash'],
    AccessToken,
    ARequest.Files);
end;

procedure TprPackageWM.HandleJSONMessage(const IncomingMessage: PcnocStackMessage; const JSONData: TJSONObject; var Handled: Boolean);
var
  AccessToken: String;
  PackageName, PackageRepoPath: String;
begin
  AccessToken := IncomingMessage^.GetExtAccessKey(0);
  Handled := False;

  if JSONData.get('method', '')='DELETE' then
    begin
    PackageName := JSONData.Get('package','');
    if (PackageName='') then
      raise EJsonWebException.CreateHelp('No package provided', 400);

    if (TfprAuthenticationHandler.GetInstance.GetUserRole(AccessToken)<>'admin') then
      raise EJsonWebException.CreateFmtHelp('You have not enough rights to delete the [%s] package', [PackageName], 403);

    PackageRepoPath := GetPackageRepoPath(PackageName);
    if not DirectoryExists(PackageRepoPath) then
      raise EJsonWebException.CreateFmtHelp('Can not delete package [%s] because it is not found.', [PackageName], 403);

    RenameFile(PackageRepoPath, ChangeFileExt(PackageRepoPath, '')+'_'+FormatDateTime('yyyymmddhhnn', Now)+'.bck');

    Handled := True;
    Exit;
    end;
end;

function TprPackageWM.DoHandleRawRequest(ARequest: TRequest; AResponse: TResponse): Boolean;
var
  AccessToken,
  PackageName,
  PackageURL: String;

var
  ErrString, RevHash: string;
  FPCVersion: TfprFPCVersion;
  IsNew: Boolean;
  Command: string;
begin
  Result := False;
  AccessToken := GetAccessToken(ARequest);

  PackageName := ARequest.RouteParams['packagename'];
  PackageURL := TDCSGlobalSettings.GetInstance.GetSettingAsString('packagemanagerurl') + '/package/'+PackageName;
  Command := ARequest.RouteParams['command'];
  try
    // To my regret FPackageObject is used in DoHandleRawRequest and in
    // DoHandleRequest. That's why the code to free it is so damn complex.
    FPackageObject := TfprWebModule.specialize ObtainCollectionItemRestRequest<TfprPackage>(PackageURL, AccessToken);
  Except
    on E: Exception do
      begin
      if E is EHTTPClient then
        begin
        // Probably.... 404
        AResponse.Code := 404;
        AResponse.CodeText := GetStatusCode(AResponse.Code);
        Result := True;
        Exit;
        end
      else
        Raise;
      end;
  end;

  if assigned(FPackageObject) then
    begin
    try
      if ARequest.Method = 'POST' then
        begin
        if (FPackageObject.OwnerId <> TfprAuthenticationHandler.GetInstance.GetSubject(AccessToken)) and (TfprAuthenticationHandler.GetInstance.GetUserRole(AccessToken)<>'admin') then
          raise Exception.Create('You have not enough rights to upload sources for this package');

        if ARequest.Files.Count <> 1 then
          raise Exception.Create('Missing package in request');

        FPCVersion := GetVersionFromPathInfo(Command, AccessToken);

        if not CheckUploadedSourceArchive(FPackageObject, FPCVersion, ARequest.Files.First, AccessToken, ErrString) then
          begin
          raise EJsonWebException.CreateHelp('Validity check on source failed: ' + ErrString, 400);
          end;

        IsNew := not DirectoryExists(GetPackageRepoPath(PackageName));

        if IsNew and (FPackageObject.PackageState <> prspsInitial) then
          raise Exception.CreateFmt('There is no repository for package %s, while the package is not new anymore.', [PackageName]);

        if IsNew then
          begin
          TfprLog.Log(Format('Package [%s] is new. Create a repository for it.', [PackageName]));
          AddGITRepositoryForNewPackage(PackageName);
          end;

        RevHash := AddSourcesToGITRepository(PackageName, FPCVersion, ARequest.Files.First);

        AResponse.Content := '{"sourcehash": "'+RevHash+'"}';
        AResponse.Code := 200;
        AResponse.CodeText := GetStatusCode(AResponse.Code);
        Result := True;
        end
      else if (Command <> '') and (Command <> 'list') and (Command <> 'tagpackage') then
        begin
        DownloadTAGSource(AResponse, FPackageObject.Name, Command);
        AResponse.Code := 200;
        AResponse.CodeText := GetStatusCode(AResponse.Code);
        Result := True;
        end
    finally
      if Result then
        FPackageObject.Free;
    end;
    end
  else
    raise EJsonWebException.CreateHelp('Invalid response from PackageManager.', 500);
end;

function TprPackageWM.GetAccessToken(var ARequest: TRequest): String;
var
  AccessToken: String;
  AuthorizationToken: String;
begin
  AuthorizationToken := ARequest.Authorization;
  if copy(AuthorizationToken, 1, 7) = 'Bearer ' then
    AccessToken := copy(AuthorizationToken, 8, length(AuthorizationToken) - 7)
  else
    AccessToken := '';
  Result := AccessToken;
end;

end.

