unit repRepositoryWebmodule;

{$mode objfpc}{$H+}
{$interfaces corba}

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
  httproute,
  fphttpclient,
  fphttpserver,
  dcsGlobalSettings,
  fprWebModule,
  fprFPCVersion,
  fprLog,
  fprBuildAgentResponse,
  fprDeleteTree,
  fprCopyTree,
  fprErrorHandling,
  fprGCollection,
  cnocStackJSONHandlerThread,
  repPackage,
  cnocStackHandlerThread,
  fprWebHandler,
  cnocStackMessageTypes;

type

  { TrepRepositoryHander }

  TrepRepositoryHander = class(TfprWebHandler, IRouteInterface, IcnocStackHandler, IcnocStackJSONRespondToMessage)
  private
    function GetFPCVersionCollection(const AccessToken: string): TfprFPCVersionCollection;
    function RebuildRepository(AFPCVersion: TrepFPCVersion; ARepository: TrepRepository; AccessToken: string): TJSONData;
  protected
    procedure RebuildPackage(APackage: TrepPackage; AFPCVersion: string; AManifest: TXMLDocument; APackagesNode: TDOMElement;
      ARepositoryURL, ABuildAgentURL, ARepoTempPath: string; AccessToken: string);
    function DoHandleRequest(ARequest: TRequest; JSONContent: TJSONData): TJSONData; override;
    function HandleRepositoryRequest(const FPCVersionStr, RepName, ExtraParam, AccessToken: string): TJSONData;
    procedure DoRespondToJSONMessage(const IncomingMessage: PcnocStackMessage; const JSONData: TJSONObject; out AResponse: TJSONData; var Handled: Boolean); override;
  public
    constructor Create; override;
  end;

var
  GStackClient: TcnocStackHandlerThread;

implementation

{ TrepRepositoryHander }

procedure TrepRepositoryHander.DoRespondToJSONMessage(const IncomingMessage: PcnocStackMessage;
  const JSONData: TJSONObject; out AResponse: TJSONData; var Handled: Boolean);
begin
  AResponse := HandleRepositoryRequest(JSONData.Get('fpcversion',''), JSONData.Get('repository',''), JSONData.get('package', ''), IncomingMessage^.GetExtAccessKey(0));
end;

constructor TrepRepositoryHander.Create;
var
  GlobalSettings: TDCSGlobalSettings;
begin
  inherited Create;
  GlobalSettings := TDCSGlobalSettings.GetInstance;
  if GlobalSettings.GetSettingAsString('AllowCorsOrigin') <> '' then
    AddCorsOrigin(GlobalSettings.GetSettingAsString('AllowCorsOrigin'), 'POST, GET, PUT', '', True);
end;

function TrepRepositoryHander.HandleRepositoryRequest(const FPCVersionStr, RepName, ExtraParam, AccessToken: string): TJSONData;
var
  FPCVersion: TrepFPCVersion;
  Repository: TrepRepository;
begin
  if FPCVersionStr = '' then
    begin
    Result := ObjectToJSON(TrepFPCVersionList.Instance);
    end
  else
    begin
    FPCVersion := TrepFPCVersionList.Instance.FindFPCVersion(FPCVersionStr);
    if not Assigned(FPCVersion) then
      raise EHTTPClient.CreateFmt('FPC-Version %s is not available', [FPCVersionStr]);

    if RepName = '' then
      Result := ObjectToJSON(FPCVersion.RepositoryList)
    else
      begin
      Repository := FPCVersion.RepositoryList.FindRepository(RepName);
      if not Assigned(Repository) then
        raise EHTTPClient.CreateFmtHelp('Repository %s does not exist', [RepName] , 404);

      if ExtraParam = '' then
        Result := ObjectToJSON(Repository.PackageList)
      else
        Result := RebuildRepository(FPCVersion, Repository, AccessToken);

      end;
    end;
end;

function TrepRepositoryHander.DoHandleRequest(ARequest: TRequest; JSONContent: TJSONData): TJSONData;
var
  AuthorizationToken, AccessToken: String;
begin
  AuthorizationToken := ARequest.Authorization;
  if copy(AuthorizationToken, 1, 7) = 'Bearer ' then
    AccessToken := copy(AuthorizationToken, 8, length(AuthorizationToken) - 7)
  else
    AccessToken := '';

  Result := HandleRepositoryRequest(ARequest.RouteParams['fpcversion'], ARequest.RouteParams['repository'], ARequest.RouteParams['extra'], AccessToken);
end;

function TrepRepositoryHander.GetFPCVersionCollection(const AccessToken: string): TfprFPCVersionCollection;
begin
  Result := TfprFPCVersionCollection.Instance;
  if not JSONObjectRestRequest(IncludeHTTPPathDelimiter(TDCSGlobalSettings.GetInstance.GetSettingAsString('packagemanagerurl'))+'fpcversion', AccessToken, Result) then
    raise Exception.Create('Failed to get FPC version list');
end;

function TrepRepositoryHander.RebuildRepository(AFPCVersion: TrepFPCVersion; ARepository: TrepRepository; AccessToken: string): TJSONData;
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
  VersionCollection: TfprFPCVersionCollection;
begin
  Result := nil;
  TfprLog.Log(Format('Rebuild repository [%s] for fpc-version [%s]', [ARepository.Name, AFPCVersion.FPCVersion]));
  if Arepository.Path='' then
    raise EHTTPClient.CreateFmtHelp('Path of repository %s is not defined.', [ARepository.Name], 500);

  RepositoryURL := TDCSGlobalSettings.GetInstance.GetSettingAsString('repositoryurl');
  if RepositoryURL='' then
    raise EHTTPClient.CreateHelp('Repositoryurl has not been configured.', 500);

  BuildAgentURL := RetrieveBuildAgentURL(AFPCVersion.FPCVersion, AccessToken);

  TfprLog.Log(Format('Use RepositoryURL [%s] and BuildAgentURL [%s]', [RepositoryURL, BuildAgentURL]));

  Manifest := TXMLDocument.Create;
  try
    RepositoryNode := Manifest.CreateElement('repository');
    Manifest.AppendChild(RepositoryNode);
    PackagesNode := Manifest.CreateElement('packages');
    RepositoryNode.AppendChild(PackagesNode);

    RepoTempPath := GetTempFileName(GetTempDir(False), 'repodir');
    TfprLog.Log(Format('Use RepoTempPath [%s]', [RepoTempPath]));
    if not ForceDirectories(RepoTempPath) then
      begin
      raise EHTTPClient.CreateHelp('Failed to create temporary directory to build repository.', 500);
      end;

    try
      if ARepository.MasterRepositoryName<>'' then
        begin
        TfprLog.Log(Format('Add packages from the master-repository [%s]', [ARepository.MasterRepositoryName]));
        MasterRepository := AFPCVersion.RepositoryList.FindRepository(ARepository.MasterRepositoryName);
        if not Assigned(MasterRepository) then
          raise EHTTPClient.CreateFmtHelp('Master repository %s does not exist', [ARepository.MasterRepositoryName] , 400);

        for i := 0 to MasterRepository.PackageList.Count -1 do
          begin
          Package := MasterRepository.PackageList.Items[i];
          if not Assigned(ARepository.PackageList.FindPackage(Package.Name)) then
            RebuildPackage(Package, AFPCVersion.FPCVersion, Manifest, PackagesNode, RepositoryURL, BuildAgentURL, RepoTempPath, AccessToken)
          end;
        end;

      TfprLog.Log('Add packages from the repository');
      for i := 0 to ARepository.PackageList.Count -1 do
        begin
        Package := ARepository.PackageList.Items[i];

        RebuildPackage(Package, AFPCVersion.FPCVersion, Manifest, PackagesNode, RepositoryURL, BuildAgentURL, RepoTempPath, AccessToken);
        end;

      TfprLog.Log('Create the manifest (packages.xml)');
      ManifestStream := TStringStream.Create(nil);
      try
        WriteXML(Manifest, ManifestStream);
        ManifestStream.SaveToFile(ConcatPaths([RepoTempPath, 'packages.xml']));
        Result := TJSONString.Create(ManifestStream.DataString);
      finally
        ManifestStream.Free;
      end;

      TfprLog.Log('Ensure all directies exist');
      VersionCollection := GetFPCVersionCollection(AccessToken);
      FPCVersion := VersionCollection.FindVersion(ARepository.FPCVersion);
      if not Assigned(FPCVersion) then
        raise Exception.CreateFmt('FPC-version [%s] of repository [%s] is not known by the packagemanager', [ARepository.FPCVersion, ARepository.Name]);
      RepoPath := ConcatPaths([ARepository.Path, FPCVersion.URLPrefix]);
      DeleteTree(RepoPath + '_old', False);
      RenameFile(RepoPath, RepoPath + '_old');

      ForceDirectories(ExtractFileDir(RepoPath));

      TfprLog.Log('Create the mirrors file');
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

      TfprLog.Log('Copy temporary files to final destination');
      if not CopyTree(IncludeTrailingPathDelimiter(RepoTempPath), ExcludeTrailingPathDelimiter(RepoPath)) then
        Raise Exception.CreateFmt('Failed to copy directory ''%s'' to ''%s''. Errorcode: %d.', [RepoTempPath, RepoPath, IOResult]);
    finally
      TfprLog.Log('Remove temporary files');
      DeleteTree(RepoTempPath, False);
    end;
  finally
    Manifest.Free;
  end;
end;

procedure TrepRepositoryHander.RebuildPackage(APackage: TrepPackage; AFPCVersion: string;
  AManifest: TXMLDocument; APackagesNode: TDOMElement; ARepositoryURL, ABuildAgentURL,
  ARepoTempPath: string; AccessToken: string);
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
  TfprLog.Log(Format('Rebuild package [%s]', [APackage.Name]));
  try
    HTTPClient := TFPHTTPClient.Create(nil);
    try
      HTTPClient.RequestHeaders.Values['authorization'] := 'Bearer ' + AccessToken;
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
          HTTPClient.RequestHeaders.Values['authorization'] := 'Bearer ' + AccessToken;
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

end.

