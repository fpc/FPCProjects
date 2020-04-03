program fppkgbatchprocessor;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  SysUtils,
  CustApp,
  Zipper,
  FileUtil,
  pkgrepos, fprepos,
  opensslsockets,
  fpjson,
  jsonparser,
  HTTPDefs,
  fphttpclient;

type
  TProcessingOptions = (poBuild, poUpload, poTag, poPublish);
  TProcessingOptionsSet = set of TProcessingOptions;

  { TFppkgRepoBatchProcessing }

  TFppkgRepoBatchProcessing = class(TCustomApplication)
  protected
    procedure DoRun; override;
    function BuildPackage(APackageSource: string): Boolean;
    function UploadPackage(APackageName, APackageSource: string; out Hash: string): Boolean;
    function TagPackage(APackageName, AHash: string; out ATag: string): Boolean;
    function PublishPackage(APackageName, ATag: string): Boolean;
    function DeletePackagefile(APackageFilename: string): Boolean;
    function ExtractMetaData(APackageSource: string; out PackageName: string): Boolean;
    procedure BatchProcessDirectory(ADir: string; AProcessingOptions: TProcessingOptionsSet);

    function GetBuildagentOption: string;
    function GetJWKOption: string;
    function GetCPUOption: string;
    function GetOSOption: string;
    function GetFPCVersionOption: string;
    function GetRepoOption: string;
    function GetRepositoryOption: string;
    function GetFppkgRepositoryOption: string;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TFppkgRepoBatchProcessing }

procedure TFppkgRepoBatchProcessing.DoRun;
var
  ErrorMsg: String;
  Opts, NonOpts: TStrings;

begin
  Opts := TStringList.Create;
  NonOpts := TStringList.Create;
  try
    // quick check parameters
    ErrorMsg := CheckOptions('hb:j:o:c:f:r:F:R:d', ['help','buildagent:', 'jwk:', 'os:', 'cpu:', 'fpc:', 'repository:', 'fppkgrepository:', 'repo:', 'delete'], Opts, NonOpts);
    if ErrorMsg <> '' then begin
      WriteLn(ErrorMsg);
      Terminate;
      Exit;
    end;

    // parse parameters
    if HasOption('h', 'help') then begin
      WriteHelp;
      Terminate;
      Exit;
    end;

    if NonOpts.Count <> 1 then
    begin
      WriteLn('Invalid options: Command is missing.');
      Terminate;
      Exit;
    end;

    if NonOpts[0] = 'build' then
      BatchProcessDirectory(GetCurrentDir, [poBuild])
    else if NonOpts[0] = 'update' then
      BatchProcessDirectory(GetCurrentDir, [poBuild, poUpload, poTag])
    else if NonOpts[0] = 'publish' then
      BatchProcessDirectory(GetCurrentDir, [poBuild, poUpload, poTag, poPublish])
    else
    begin
      WriteLn('Unkown command: ' + NonOpts[0]);
      Terminate;
      Exit;
    end;

  finally
    NonOpts.Free;
    Opts.Free;
  end;
  // stop program loop
  Terminate;
end;

constructor TFppkgRepoBatchProcessing.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException := True;
end;

destructor TFppkgRepoBatchProcessing.Destroy;
begin
  inherited Destroy;
end;

procedure TFppkgRepoBatchProcessing.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' [OPTION]... COMMAND');
  writeln('');
  writeln('Options:');
  writeln('  -b URL, --buildagent=URL');
  writeln('      URL of the buildagent');
  writeln('  -j JWK, --jwk=JWK');
  writeln('      Login-token (jwk)');
  writeln('  -F URL, --fppkgrepository=URL');
  writeln('      URL of the fppkg-repository');
  writeln('');
  writeln('Commands:');
  writeln('  build');
  writeln('  update');
end;

procedure TFppkgRepoBatchProcessing.BatchProcessDirectory(ADir: string; AProcessingOptions: TProcessingOptionsSet);
var
  SR: TSearchRec;
  Continue: Boolean;
  PackageFilename: string;
  PackageName: string;
  Hash, VersionTag: string;
begin
  if FindFirst(ConcatPaths([ADir, '*.zip']), faAnyFile - faDirectory,SR) = 0 then
  begin
    repeat
      Continue := True;
      Hash := '';
      writeln('Processing ' + SR.Name);
      PackageFilename := ConcatPaths([ADir,SR.Name]);
      if poBuild in AProcessingOptions then
        Continue := BuildPackage(PackageFilename);

      if Continue and (poUpload in AProcessingOptions) then
        begin
        Continue := ExtractMetaData(PackageFilename, PackageName);
        if continue then
          Continue := UploadPackage(PackageName, PackageFilename, Hash);
        end;

      if Continue and (poTag in AProcessingOptions) then
        begin
        if PackageName='' then
          Continue := ExtractMetaData(PackageFilename, PackageName);

        if Continue then
          Continue := TagPackage(PackageName, Hash, VersionTag);
        end;

      if Continue and (poPublish in AProcessingOptions) then
        begin
        Continue := PublishPackage(PackageName, VersionTag);
        end;

      if Continue and HasOption('d', 'delete') then
        begin
        Continue := DeletePackagefile(PackageFilename);
        end;
    until FindNext(SR) <> 0;
    FindClose(SR);
  end
  else
  begin
    writeln('Failed to search for archives');
  end;
end;

function TFppkgRepoBatchProcessing.BuildPackage(APackageSource: string): Boolean;
var
  URL, s: String;
  Client: TFPHTTPClient;
  Resp: TStringStream;
  ResultArr: array of string;
  JSONRes: TJSONObject;
  i: Integer;
  FS: TFileStream;
begin
  Result := False;
  URL := IncludeHTTPPathDelimiter(GetBuildagentOption) + Format('build?cputarget=%s&ostarget=%s&fpcversion=%s', [GetCPUOption, GetOSOption, GetFPCVersionOption]);

  Client := TFPHTTPClient.Create(nil);
  try
    Client.AddHeader('authorization', 'Bearer ' + GetJWKOption);
    Resp := TStringStream.Create;
    try
      FS := TFileStream.Create(APackageSource, fmOpenRead);
      try
        Client.RequestBody := FS;
        Client.Post(URL, Resp);
      finally
        FS.Free;
      end;
      ResultArr := Resp.DataString.Split(#10);
      s := Resp.DataString;

      for i := 0 to High(ResultArr) do
      begin
        s := ResultArr[i];
        JSONRes := GetJSON(ResultArr[i]) as TJSONObject;
        try
          if Assigned(JSONRes) then
            begin
            if ((JSONRes.Get('type', '') = 'Error') or (JSONRes.Get('type', '') = 'Failed')) then
              begin
              WriteLn('  Build failed: ' + JSONRes.Get('message'));
              Break;
              end
            else if ((JSONRes.Get('type', '') = 'Done')) then
              begin
              WriteLn('  Build succeeded');
              Result := True;
              end;
            end;
        finally
          JSONRes.Free;
        end;
      end;
    finally
      Resp.Free;
    end;
  finally
    Client.Free;
  end;
end;

function TFppkgRepoBatchProcessing.GetBuildagentOption: string;
begin
  Result := GetOptionValue('b', 'buildagent');
  if Result='' then
    begin
    writeln('Error: Please provide a buildagent.');
    Terminate;
    Halt;
    end;
end;

function TFppkgRepoBatchProcessing.GetJWKOption: string;
begin
  Result := GetOptionValue('j', 'jwk');
  if Result='' then
    begin
    writeln('Error: Please provide a JWK.');
    Terminate;
    Halt;
    end;
end;

function TFppkgRepoBatchProcessing.GetCPUOption: string;
begin
  Result := GetOptionValue('c', 'cpu');
  if Result='' then
    begin
    writeln('Error: Please provide a CPU.');
    Terminate;
    Halt;
    end;
end;

function TFppkgRepoBatchProcessing.GetFPCVersionOption: string;
begin
  Result := GetOptionValue('f', 'fpc');
  if Result='' then
    begin
    writeln('Error: Please provide a FPC-version.');
    Terminate;
    Halt;
    end;
end;

function TFppkgRepoBatchProcessing.GetOSOption: string;
begin
  Result := GetOptionValue('o', 'os');
  if Result='' then
    begin
    writeln('Error: Please provide a OS.');
    Terminate;
    Halt;
    end;
end;

function TFppkgRepoBatchProcessing.UploadPackage(APackageName, APackageSource: string; out Hash: string): Boolean;
var
  URL: String;
  Client: TFPHTTPClient;
  Resp: TStringStream;
  JSONRes: TJSONObject;
  StatusCode: Integer;
begin
  Result := False;
  URL := IncludeHTTPPathDelimiter(GetRepositoryOption) + Format('package/%s/%s', [APackageName, GetFPCVersionOption]);

  Client := TFPHTTPClient.Create(nil);
  try
    Client.AddHeader('authorization', 'Bearer ' + GetJWKOption);
    Resp := TStringStream.Create;
    try
      Client.FileFormPost(URL, nil, '', APackageSource, Resp);
      StatusCode := Client.ResponseStatusCode;

      if StatusCode<>200 then
        begin
        WriteLn('  Upload failed ('+IntToStr(StatusCode)+'): ' + Resp.DataString);
        end
      else
        begin
        JSONRes := GetJSON(Resp.DataString) as TJSONObject;
        Hash := JSONRes.Get('sourcehash', '');
        WriteLn('  Upload succeeded: ' + Hash);
        Result := True;
        end;
    finally
      Resp.Free;
    end;
  finally
    Client.Free;
  end;
end;

function TFppkgRepoBatchProcessing.GetRepositoryOption: string;
begin
  Result := GetOptionValue('r', 'repository');
  if Result='' then
    begin
    writeln('Error: Please provide the URL of the repository-service.');
    Terminate;
    Halt;
    end;
end;

function TFppkgRepoBatchProcessing.ExtractMetaData(APackageSource: string; out PackageName: string): Boolean;
var
  BuildPath: string;
  UnZipper: TUnZipper;
  Package: TFPPackage;
begin
  Result := False;
  BuildPath := GetTempFileName();
  ForceDirectories(BuildPath);
  try
    UnZipper := TUnZipper.Create;
    try
      UnZipper.FileName := APackageSource;
      UnZipper.OutputPath := BuildPath;
      UnZipper.Examine;
      UnZipper.UnZipAllFiles;
    finally
      UnZipper.Free;
    end;

    Package := LoadManifestFromFile(ConcatPaths([BuildPath, 'manifest.xml']));
    try
      PackageName := Package.Name;
    finally
      Package.Free;
    end;
    Result := True;
  finally
    DeleteDirectory(BuildPath, False);
  end;
end;

function TFppkgRepoBatchProcessing.TagPackage(APackageName, AHash: string; out ATag: string): Boolean;
var
  URL, s: String;
  Client: TFPHTTPClient;
  Resp: TStringStream;
  JSONRes: TJSONObject;
  StatusCode: Integer;
begin
  Result := False;
  URL := IncludeHTTPPathDelimiter(GetRepositoryOption) + Format('package/%s/tagpackage/%s?message=%s&hash=%s', [APackageName, GetFPCVersionOption, EncodeURLElement('Version updated'), AHash]);

  Client := TFPHTTPClient.Create(nil);
  try
    Client.AddHeader('authorization', 'Bearer ' + GetJWKOption);
    Resp := TStringStream.Create;
    try
      Client.HTTPMethod('GET', URL, Resp, []);
      StatusCode := Client.ResponseStatusCode;
      if StatusCode<>200 then
        begin
        WriteLn('  Tagging failed ('+IntToStr(StatusCode)+'): ' + Resp.DataString);
        end
      else
        begin
        JSONRes := GetJSON(Resp.DataString) as TJSONObject;
        try
          ATag := JSONRes.Get('tag', '');
          WriteLn('  Tagging succeeded: ' + JSONRes.Get('tag', ''));
        finally
          JSONRes.Free;
        end;
        Result := True;
        end;
    finally
      Resp.Free;
    end;
  finally
    Client.Free;
  end;
end;

function TFppkgRepoBatchProcessing.PublishPackage(APackageName, ATag: string): Boolean;
var
  URL: String;
  Client: TFPHTTPClient;
  Resp, SS: TStringStream;
  StatusCode: Integer;
begin
  Result := False;
  URL := IncludeHTTPPathDelimiter(GetFppkgRepositoryOption) + Format('package/%s/%s', [GetFPCVersionOption, GetRepoOption]);

  Client := TFPHTTPClient.Create(nil);
  try
    Client.AddHeader('authorization', 'Bearer ' + GetJWKOption);
    Resp := TStringStream.Create;
    try
      SS := TStringStream.Create(Format('{"name":"%s","tag":"%s"}', [APackageName, ATag]));
      try
        Client.RequestBody := SS;
        Client.HTTPMethod('POST', URL, Resp, []);
      finally
        SS.Free;
      end;
      StatusCode := Client.ResponseStatusCode;
      if StatusCode<>200 then
        begin
        WriteLn('  Publishing failed ('+IntToStr(StatusCode)+'): ' + Resp.DataString);
        end
      else
        begin
        WriteLn('  Publishing succeeded');
        Result := True;
        end;
    finally
      Resp.Free;
    end;
  finally
    Client.Free;
  end;
end;

function TFppkgRepoBatchProcessing.GetFppkgRepositoryOption: string;
begin
  Result := GetOptionValue('F', 'fppkgrepository');
  if Result='' then
    begin
    writeln('Error: Please provide the URL of the fppkgrepository-service.');
    Terminate;
    Halt;
    end;
end;

function TFppkgRepoBatchProcessing.GetRepoOption: string;
begin
  Result := GetOptionValue('R', 'repo');
  if Result='' then
    begin
    writeln('Error: Please provide a repository (ie: prod or testing)');
    Terminate;
    Halt;
    end;
end;

function TFppkgRepoBatchProcessing.DeletePackagefile(APackageFilename: string): Boolean;
begin
  Result := DeleteFile(APackageFilename);
  if not Result then
    WriteLn('  Failed to remove archive')
  else
    WriteLn('  Archive has been removed');
end;

var
  Application: TFppkgRepoBatchProcessing;
begin
  Application := TFppkgRepoBatchProcessing.Create(nil);
  Application.Title := 'Fppkg batch processing';
  Application.Run;
  Application.Free;
end.

