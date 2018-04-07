unit repRepositoryWebmodule;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  Classes,
  httpdefs,
  BaseUnix,
  XMLRead,
  XMLWrite,
  fpHTTP,
  DOM,
  fpWeb,
  LazFileUtils,
  LazUtils,
  LazUtilities,
  fpjson,
  fphttpclient,
  fphttpserver,
  dcsGlobalSettings,
  fprWebModule,
  fprBuildAgentResponse,
  fprDeleteTree,
  fprCopyTree,
  fprErrorHandling,
  repPackage;

type

  { TrepRepositoryWM }

  TrepRepositoryWM = class(TfprWebModule)
    procedure DataModuleCreate(Sender: TObject);
    Procedure DataModuleRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse; Var Handled: Boolean);
  private
    function RebuildRepository(ARepository: TrepRepository): string;
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
          AResponse.Content := RebuildRepository(Repository);

        end;
      end;
    AResponse.Code := 200;
    AResponse.CodeText := GetStatusCode(AResponse.Code);;
    end;
  Handled := True;
end;

function TrepRepositoryWM.RebuildRepository(ARepository: TrepRepository): string;
var
  i: Integer;
  RepositoryURL: string;
  Package: TrepPackage;
  RepoTempPath: string;
  HTTPClient: TFPHTTPClient;
  BytesStream: TBytesStream;
  Resp, BuildAgentURL: string;
  BuildResponseList: TfprBuildAgentResponseList;
  BuildResponse: TfprBuildAgentResponse;
  FileStream: TFileStream;
  ManifestStream: TStringStream;
  Manifest: TXMLDocument;
  Mirrors: TXMLDocument;
  MirrorNode: TDOMElement;
  MirrorsNode: TDOMElement;
  MirrorChildNode: TDOMElement;
  PackageManifest: TXMLDocument;
  PackagePackagesNode, PackageNode: TDOMNode;
  PackagesNode: TDOMElement;
  RepositoryNode: TDOMElement;
  RepoPath: string;
begin
  Result := '';
  if Arepository.Path='' then
    raise EHTTPClient.CreateFmtHelp('Path of repository %s is not defined.', [ARepository.Name], 500);

  RepositoryURL := TDCSGlobalSettings.GetInstance.GetSettingAsString('repositoryurl');
  BuildAgentURL := TDCSGlobalSettings.GetInstance.GetSettingAsString('BuildAgentURL');

  Manifest := TXMLDocument.Create;
  try
    RepositoryNode := Manifest.CreateElement('repository');
    Manifest.AppendChild(RepositoryNode);
    PackagesNode := Manifest.CreateElement('packages');
    RepositoryNode.AppendChild(PackagesNode);

    RepoTempPath := GetTempFileName(GetTempDir(False), 'repodir');
    ForceDirectories(RepoTempPath);
    try
      for i := 0 to ARepository.PackageList.Count -1 do
        begin
        Package := ARepository.PackageList.Items[i];

        try
          HTTPClient := TFPHTTPClient.Create(nil);
          try
            HTTPClient.RequestHeaders.Values['authorization'] := 'Bearer ' + FAccessToken;
            BytesStream := TBytesStream.Create;
            try
              HTTPClient.Get(IncludeHTTPPathDelimiter(RepositoryURL)+'package/'+Package.Name+'/'+Package.Tag, BytesStream);
              BytesStream.Seek(0, soFromBeginning);
              HTTPClient.RequestBody := BytesStream;
              Resp := HTTPClient.Get(IncludeHTTPPathDelimiter(BuildAgentURL)+'createsourcearchive?cputarget=x86_64&ostarget=linux&fpcversion=3.1.1&chunked=false');
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
              //if BuildResponse.AType <> 'Done' then
              //  raise EJsonWebException.CreateFmtHelp('Failed to create source-archive for package %s (%s): %s', [Package.Name, Package.Tag, BuildResponse.Message], 500);

              if BuildResponse.SourceArchive = '' then
                raise EJsonWebException.CreateFmtHelp('Failed to create source-archive: %s', [BuildResponse.Message], 500);


              HTTPClient := TFPHTTPClient.Create(nil);
              try
                HTTPClient.RequestHeaders.Values['authorization'] := 'Bearer ' + FAccessToken;
                FileStream := TFileStream.Create(ConcatPaths([RepoTempPath, ExtractFileName(BuildResponse.SourceArchive)]), fmCreate);
                try
                  HTTPClient.Get(BuildResponse.SourceArchive, FileStream);
                finally
                  FileStream.Free;
                end;
              finally
                HTTPClient.Free;
              end;

              ManifestStream := TStringStream.Create(BuildResponse.Manifest);
              try
                ReadXMLFile(PackageManifest, ManifestStream);
                try
                  PackagePackagesNode := PackageManifest.FindNode('packages');
                  if not Assigned(PackagePackagesNode) then
                    raise EJsonWebException.CreateHelp('Manifest does not contain packages.', 500);

                  if PackagePackagesNode.ChildNodes.Count =0 then
                    raise EJsonWebException.CreateHelp('Manifest does not contain any package.', 500);

                  PackageNode := PackagePackagesNode.ChildNodes[0];
                  PackageNode := PackageNode.CloneNode(True, Manifest);
                  PackagesNode.AppendChild(PackageNode);
                finally
                  PackageManifest.Free;
                end;
              finally
                ManifestStream.Free;
              end;
              end
            else
              raise EJsonWebException.CreateHelp('Failed to create source-archive', 500);
          except
            on E: Exception do
              Raise Exception.CreateFmt('Exception during handling package ''%s'' (%s): %s (%s)', [Package.Name, Package.Tag, E.Message, E.ClassName]);
          end;
        finally
          BuildResponseList.Free;
        end;
        end;

      ManifestStream := TStringStream.Create(nil);
      try
        WriteXML(Manifest, ManifestStream);
        ManifestStream.SaveToFile(ConcatPaths([RepoTempPath, 'packages.xml']));
        Result := ManifestStream.DataString;
      finally
        ManifestStream.Free;
      end;

      RepoPath := ConcatPaths([ARepository.Path, ARepository.FPCVersion]);
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

initialization
  RegisterHTTPModule('repository', TrepRepositoryWM);
end.

