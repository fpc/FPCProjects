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
  zipper,
  jsonparser,
  fprPackageUtils,
  fphttpserver,
  fprWebModule;

type

  { TprPackageWM }

  TprPackageWM = class(TfprWebModule)
    Procedure DataModuleRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse; Var Handled: Boolean);
  private
    function GetPackageRepoPath(APackageName: string): string;
    procedure AddGITRepositoryForNewPackage(APackageName: string);
    function AddSourcesToGITRepository(APackageName: string; AFile: TUploadedFile): string;
    procedure RunGit(const curdir:string; const desc: string; const commands:array of string;out outputstring:string);
    procedure TagPackage(PackageName, GITTag, TagMessage: string);

  public

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
  RevHash: string;
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
        Raise E;
      end;
  end;
  if PackageJSON.JSONType = jtObject then
    begin
    // Todo: Admin?

    PackageObject := PackageJSON as TJSONObject;
    if PackageObject.Get('ownerid', '') <> FSubjectId then
      raise Exception.Create('You have not enough rights to upload sources for this package');

    if PackageObject.Get('packagestate', '') = 'new' then
      AddGITRepositoryForNewPackage(PackageName);

    if ARequest.Files.Count <> 1 then
      raise Exception.Create('Missing package in request');

    RevHash := AddSourcesToGITRepository(PackageName, ARequest.Files.First);

    if PackageObject.Get('packagestate', '') = 'new' then
      TagPackage(PackageName, '0.0.0.0', 'Initial version');

    AResponse.Content := '{sourcehash: '''+RevHash+'''}';
    AResponse.Code := 200;
    AResponse.CodeText := GetStatusCode(AResponse.Code);
    end
  else
    raise Exception.Create('Invalid response from PackageManager.');
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
    finally
      DeleteDirectory(TmpPath, False);
    end;
    end
  else
    raise Exception.Create('Failed to create temporary path');
end;

function TprPackageWM.AddSourcesToGITRepository(APackageName: string; AFile: TUploadedFile): string;
var
  TmpPath: string;
  FS: TFileStream;
  ArchiveName: string;
  ClonePath: string;
  CmdRes: string;
  UnZipper: TUnZipper;
begin
  TmpPath := GetTempFileName;
  ForceDirectories(TmpPath);
  try
    RunGit(TmpPath, 'clone git repository', ['clone', GetPackageRepoPath(APackageName), 'pkgclone'], CmdRes );

    ClonePath := ConcatPaths([TmpPath, 'pkgclone']);
    ArchiveName := ConcatPaths([TmpPath, 'package.zip']);
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

    RunGit(ClonePath, 'stage all files', ['add', '--all'], CmdRes);

    RunGit(ClonePath, 'commit changes', ['commit', '-m ''' + 'Commit new source-zip' + ''''], CmdRes);

    RunGit(ClonePath, 'push changes', ['push'], Result);

    RunGit(ClonePath, 'get revision', ['rev-parse', 'HEAD'], Result);
    Result := Trim(Result);
  finally
    DeleteDirectory(TmpPath, False);
  end;
end;

procedure TprPackageWM.RunGit(const curdir:string; const desc: string; const commands:array of string;out outputstring:string);
var
  ExitStatus: Integer;
begin
  if RunCommandInDir(curdir, 'git', commands, outputstring, ExitStatus) <> 0 then
    Raise Exception.Create('Failed to execute GIT to ' + desc);
  if ExitStatus <> 0 then
    Raise Exception.Create('Failed to ' + desc + '. ' + outputstring);
end;

procedure TprPackageWM.TagPackage(PackageName, GITTag, TagMessage: string);
var
  TmpPath: string;
  ClonePath: string;
  CmdRes, NotifyPackageManagerTagAddedURL: string;
  ManifestXML: TXMLDocument;
  ManifestJSON: TJSONObject;
  VersionJSON: TJSONObject;
  TagAdded: string;
  ResponseJSON: TJSONData;
  StrStream: TStringStream;
  JSO: TJSONObject;
begin
  TmpPath := GetTempFileName;
  ClonePath := ConcatPaths([TmpPath, 'pkgclone']);
  ForceDirectories(TmpPath);
  try
    RunGit(TmpPath, 'clone git repository', ['clone', GetPackageRepoPath(PackageName), 'pkgclone'], CmdRes );
    RunGit(ClonePath, 'tag new version', ['tag', '-a', '''' + GITTag + '''', '-m', '''' +TagMessage + ''''], CmdRes);

    ReadXMLFile(ManifestXML, ConcatPaths([ClonePath, 'manifest.xml']));
    try
      ManifestJSON := ManifestToJSON(ManifestXML).Items[0] as TJSONObject;
      try
        VersionJSON := TJSONObject.Create();
        try
          VersionJSON.Add('tag', GITTag);

          VersionJSON.Add('filename', ManifestJSON.Get('filename', ''));
          VersionJSON.Add('author', ManifestJSON.Get('author', ''));
          VersionJSON.Add('license', ManifestJSON.Get('license', ''));
          VersionJSON.Add('homepageurl', ManifestJSON.Get('homepageurl', ''));
          VersionJSON.Add('email', ManifestJSON.Get('email', ''));
          VersionJSON.Add('description', ManifestJSON.Get('description', ''));

          TagAdded := VersionJSON.AsJSON;
        finally
          VersionJSON.Free;
        end;
      finally
        ManifestJSON.Free;
      end;
    finally
      ManifestXML.Free;
    end;

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

    RunGit(ClonePath, 'push tag', ['push'], CmdRes);

  finally
    DeleteDirectory(TmpPath, False);
  end;
end;

initialization
  RegisterHTTPModule('package', TprPackageWM);
end.

