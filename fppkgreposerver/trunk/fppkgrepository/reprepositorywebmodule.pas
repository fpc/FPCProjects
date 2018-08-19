unit repRepositoryWebmodule;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  Classes,
  httpdefs,
  XMLRead,
  XMLWrite,
  fpHTTP,
  DOM,
  LazFileUtils,
  LazUtils,
  fpjson,
  fphttpclient,
  fphttpserver,
  dcsGlobalSettings,
  fprWebModule,
  fprFPCVersion,
  fprBuildAgentResponse,
  fprDeleteTree,
  fprCopyTree,
  fprErrorHandling,
  fprGCollection,
  repPackage;

type

  { TrepRepositoryWM }

  TrepRepositoryWM = class(TfprWebModule)
    procedure DataModuleCreate(Sender: TObject);
    Procedure DataModuleRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse; Var Handled: Boolean);
  private
    function GetFPCVersionCollection: TfprFPCVersionCollection;
    function RebuildRepository(AFPCVersion: TrepFPCVersion; ARepository: TrepRepository): string;
  protected
    procedure RebuildPackage(APackage: TrepPackage; AFPCVersion: string; AManifest: TXMLDocument; APackagesNode: TDOMElement;
      ARepositoryURL, ABuildAgentURL, ARepoTempPath: string);
  end;

var
  repRepositoryWM: TrepRepositoryWM;

implementation

{$R *.lfm}

{ TrepRepositoryWM }

procedure TrepRepositoryWM.DataModuleCreate(Sender: TObject);
var
  GlobalSettings: TDCSGlobalSettings;
begin
  GlobalSettings := TDCSGlobalSettings.GetInstance;
  if GlobalSettings.GetSettingAsString('AllowCorsOrigin') <> '' then
    AddCorsOrigin(GlobalSettings.GetSettingAsString('AllowCorsOrigin'), 'POST, GET, PUT', '', True);
end;

Procedure TrepRepositoryWM.DataModuleRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse; Var Handled: Boolean);
var
  FPCVersionStr, RepName: String;
  FPCVersion: TrepFPCVersion;
  Repository: TrepRepository;
begin
  if not Handled then
    begin
    FPCVersionStr := ARequest.GetNextPathInfo;
    if FPCVersionStr = '' then
      begin
      AResponse.Content := ObjectToJSONContentString( TrepFPCVersionList.Instance );
      AResponse.Code := 200;
      AResponse.CodeText := GetStatusCode(AResponse.Code);;
      end
    else
      begin
      FPCVersion := TrepFPCVersionList.Instance.FindFPCVersion(FPCVersionStr);
      if not Assigned(FPCVersion) then
        raise EHTTPClient.CreateFmt('FPC-Version %s is not available', [FPCVersionStr]);

      RepName := ARequest.GetNextPathInfo;
      if RepName = '' then
        AResponse.Content := ObjectToJSONContentString( FPCVersion.RepositoryList )
      else
        begin
        Repository := FPCVersion.RepositoryList.FindRepository(RepName);
        if not Assigned(Repository) then
          raise EHTTPClient.CreateFmtHelp('Repository %s does not exist', [RepName] , 404);

        if ARequest.GetNextPathInfo = '' then
          AResponse.Content := ObjectToJSONContentString( Repository.PackageList )
        else
          AResponse.Content := RebuildRepository(FPCVersion, Repository);

        end;
      end;
    AResponse.Code := 200;
    AResponse.CodeText := GetStatusCode(AResponse.Code);;
    end;
  Handled := True;
end;

function TrepRepositoryWM.GetFPCVersionCollection: TfprFPCVersionCollection;
begin
  Result := TfprFPCVersionCollection.Instance;
  if Result.Count = 0 then
    begin
    if not JSONObjectRestRequest(IncludeHTTPPathDelimiter(TDCSGlobalSettings.GetInstance.GetSettingAsString('packagemanagerurl'))+'fpcversion', True, Result) then
      raise Exception.Create('Failed to get FPC version list');
    end;
end;

function TrepRepositoryWM.RebuildRepository(AFPCVersion: TrepFPCVersion; ARepository: TrepRepository): string;
var
  i: Integer;
  RepositoryURL: string;
  Package: TrepPackage;
  RepoTempPath: string;
  BuildAgentURL: string;
  ManifestStream: TStringStream;
  Manifest: TXMLDocument;
  Mirrors: TXMLDocument;
  MirrorNode: TDOMElement;
  MirrorsNode: TDOMElement;
  MirrorChildNode: TDOMElement;
  PackagesNode: TDOMElement;
  RepositoryNode: TDOMElement;
  RepoPath: string;
  MasterRepository: TrepRepository;
  FPCVersion: TfprFPCVersion;
begin
  Result := '';
  if Arepository.Path='' then
    raise EHTTPClient.CreateFmtHelp('Path of repository %s is not defined.', [ARepository.Name], 500);

  RepositoryURL := TDCSGlobalSettings.GetInstance.GetSettingAsString('repositoryurl');
  BuildAgentURL := RetrieveBuildAgentURL(AFPCVersion.FPCVersion);

  Manifest := TXMLDocument.Create;
  try
    RepositoryNode := Manifest.CreateElement('repository');
    Manifest.AppendChild(RepositoryNode);
    PackagesNode := Manifest.CreateElement('packages');
    RepositoryNode.AppendChild(PackagesNode);

    RepoTempPath := GetTempFileName(GetTempDir(False), 'repodir');
    ForceDirectories(RepoTempPath);
    try
      if ARepository.MasterRepositoryName<>'' then
        begin
        MasterRepository := AFPCVersion.RepositoryList.FindRepository(ARepository.MasterRepositoryName);
        if not Assigned(MasterRepository) then
          raise EHTTPClient.CreateFmtHelp('Master repository %s does not exist', [ARepository.MasterRepositoryName] , 400);

        for i := 0 to MasterRepository.PackageList.Count -1 do
          begin
          Package := MasterRepository.PackageList.Items[i];
          if not Assigned(ARepository.PackageList.FindPackage(Package.Name)) then
            RebuildPackage(Package, AFPCVersion.FPCVersion, Manifest, PackagesNode, RepositoryURL, BuildAgentURL, RepoTempPath);
          end;
        end;

      for i := 0 to ARepository.PackageList.Count -1 do
        begin
        Package := ARepository.PackageList.Items[i];

        RebuildPackage(Package, AFPCVersion.FPCVersion, Manifest, PackagesNode, RepositoryURL, BuildAgentURL, RepoTempPath);
        end;

      ManifestStream := TStringStream.Create(nil);
      try
        WriteXML(Manifest, ManifestStream);
        ManifestStream.SaveToFile(ConcatPaths([RepoTempPath, 'packages.xml']));
        Result := ManifestStream.DataString;
      finally
        ManifestStream.Free;
      end;

      FPCVersion := GetFPCVersionCollection.FindVersion(ARepository.FPCVersion);
      if not Assigned(FPCVersion) then
        raise Exception.CreateFmt('FPC-version [%s] of repository [%s] is not known by the packagemanager', [ARepository.FPCVersion, ARepository.Name]);
      RepoPath := ConcatPaths([ARepository.Path, FPCVersion.URLPrefix]);
      DeleteTree(RepoPath + '_old', False);
      RenameFile(RepoPath, RepoPath + '_old');

      ForceDirectories(ExtractFileDir(RepoPath));

      Mirrors := TXMLDocument.Create;
      try
        MirrorsNode := Mirrors.CreateElement('mirrors');
        Mirrors.AppendChild(MirrorsNode);

        MirrorNode := Mirrors.CreateElement('mirror');
        MirrorsNode.AppendChild(MirrorNode);

        MirrorChildNode := Mirrors.CreateElement('url');
        MirrorChildNode.TextContent := ARepository.BaseURL;
        MirrorNode.AppendChild(MirrorChildNode);

        MirrorChildNode := Mirrors.CreateElement('contact');
        MirrorChildNode.TextContent := ARepository.Contact;
        MirrorNode.AppendChild(MirrorChildNode);

        MirrorChildNode := Mirrors.CreateElement('weight');
        MirrorChildNode.TextContent := '100';
        MirrorNode.AppendChild(MirrorChildNode);

        ManifestStream := TStringStream.Create(nil);
        try
          WriteXML(Mirrors, ManifestStream);
          ManifestStream.SaveToFile(ConcatPaths([ARepository.Path, 'mirrors.xml']));
        finally
          ManifestStream.Free;
        end;

      finally
        Mirrors.Free;
      end;

      if not CopyTree(IncludeTrailingPathDelimiter(RepoTempPath), ExcludeTrailingPathDelimiter(RepoPath)) then
        Raise Exception.CreateFmt('Failed to copy directory ''%s'' to ''%s''. Errorcode: %d.', [RepoTempPath, RepoPath, IOResult]);
    finally
      DeleteTree(RepoTempPath, False);
    end;
  finally
    Manifest.Free;
  end;
end;

procedure TrepRepositoryWM.RebuildPackage(APackage: TrepPackage; AFPCVersion: string;
  AManifest: TXMLDocument; APackagesNode: TDOMElement; ARepositoryURL, ABuildAgentURL,
  ARepoTempPath: string);
var
  HTTPClient: TFPHTTPClient;
  BytesStream: TBytesStream;
  Url, Resp: String;
  BuildResponseList: TfprBuildAgentResponseList;
  BuildResponse: TfprBuildAgentResponse;
  FileStream: TFileStream;
  ManifestStream: TStringStream;
  PackageManifest: TXMLDocument;
  PackagePackagesNode, PackageNode: TDOMNode;
begin
  try
    HTTPClient := TFPHTTPClient.Create(nil);
    try
      HTTPClient.RequestHeaders.Values['authorization'] := 'Bearer ' + FAccessToken;
      BytesStream := TBytesStream.Create;
      try
        try
          Url := IncludeHTTPPathDelimiter(ARepositoryURL)+'package/'+APackage.Name+'/'+APackage.Tag;
          HTTPClient.Get(Url, BytesStream);
        except
          on E: Exception do
            Raise EHTTPClient.CreateFmtHelp('Exception during downloading ''%s'': %s (%s)', [Url, E.Message, E.ClassName], 500);
        end;
        BytesStream.Seek(0, soFromBeginning);
        HTTPClient.RequestBody := BytesStream;
        try
          Url := IncludeHTTPPathDelimiter(ABuildAgentURL)+'createsourcearchive?cputarget=x86_64&ostarget=linux&fpcversion=' + AFPCVersion + '&chunked=false';
          Resp := HTTPClient.Get(Url);
        except
          on E: Exception do
            Raise EHTTPClient.CreateFmtHelp('Exception during request to create source-archive ''%s'': %s (%s)', [Url, E.Message, E.ClassName], 500);
        end;
      finally
        BytesStream.Free;
      end;
    finally
      HTTPClient.Free;
    end;

    BuildResponseList := TfprBuildAgentResponseList.Create();
    try
      JSONContentStringToObject(Resp, BuildResponseList);
      if (BuildResponseList.Count > 0) then
        begin
        BuildResponse := BuildResponseList.Items[BuildResponseList.Count -1];
        if BuildResponse.AType <> 'Done' then
          raise EJsonWebException.CreateFmtHelp('Failed to create source-archive for package %s (%s): %s', [APackage.Name, APackage.Tag, BuildResponse.Message], 500);

        if BuildResponse.SourceArchive = '' then
          raise EJsonWebException.CreateFmtHelp('Failed to create source-archive: %s', [BuildResponse.Message], 500);

        HTTPClient := TFPHTTPClient.Create(nil);
        try
          HTTPClient.RequestHeaders.Values['authorization'] := 'Bearer ' + FAccessToken;
          FileStream := TFileStream.Create(ConcatPaths([ARepoTempPath, ExtractFileName(BuildResponse.SourceArchive)]), fmCreate);
          try
            HTTPClient.Get(BuildResponse.SourceArchive, FileStream);
          finally
            FileStream.Free;
          end;
        finally
          HTTPClient.Free;
        end;

        ManifestStream := TStringStream.Create(BuildResponse.ManifestXML);
        try
          ReadXMLFile(PackageManifest, ManifestStream);
          try
            PackagePackagesNode := PackageManifest.FindNode('packages');
            if not Assigned(PackagePackagesNode) then
              raise EJsonWebException.CreateHelp('Manifest does not contain packages.', 500);

            if PackagePackagesNode.ChildNodes.Count =0 then
              raise EJsonWebException.CreateHelp('Manifest does not contain any package.', 500);

            PackageNode := PackagePackagesNode.ChildNodes[0];
            PackageNode := PackageNode.CloneNode(True, AManifest);
            APackagesNode.AppendChild(PackageNode);
          finally
            PackageManifest.Free;
          end;
        finally
          ManifestStream.Free;
        end;
        end
      else
        raise EJsonWebException.CreateFmtHelp('Failed to create source-archive for package [%s]', [APackage.Name],500);
    finally
      BuildResponseList.Free;
    end;
  except
    on E: Exception do
      Raise Exception.CreateFmt('Exception during handling package ''%s'' (%s): %s (%s)', [APackage.Name, APackage.Tag, E.Message, E.ClassName]);
  end;
end;

initialization
  RegisterHTTPModule('repository', TrepRepositoryWM);
end.

