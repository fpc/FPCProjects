unit bmBuildTaskWebmodule;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  Classes,
  httpdefs,
  fpHTTP,
  fphttpclient,
  fphttpserver,
  fpWeb,
  jsonparser,
  fpjson,
  fpjsonrtti,
  dcsGlobalSettings,
  fprWebModule,
  fprBuildTask,
  fprBuildAgentResponse,
  bmBuildAgentWebmodule;

type

  { TbmBuildTaskWM }

  TbmBuildTaskWM = class(TfprWebModule)
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    Procedure DataModuleRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse; Var Handled: Boolean);
  private
    FBuildTaskList: TfprBuildTaskList;
    Procedure HandleNewBuildRequest(ARequest: TRequest; AResponse: TResponse);
    function RequestBuild(BuildAgent: TbmBuildAgent; SourceZIP: TBytes; PackageName, AccessToken: string): TfprSubBuildTask;
    function RequestSourceBuild(BuildAgent: TbmBuildAgent; PackageName, AccessToken, TAGName,
      FPCVersion: string): TfprSubBuildTask;
    function GetSourceZip(TaskResponse: string): TBytes;
  public
    procedure SetSubBuildTask(UniqueString, AccessToken: string; Failed: Boolean; Log: string);

  end;

var
  bmBuildTaskWM: TbmBuildTaskWM;

implementation

type

  { TbmBuildTaskThread }

  TbmBuildTaskThread = class(TThread)
  private
    FBuildURL: string;
    FAccessToken: string;
    FWebModule: TbmBuildTaskWM;
    FFailed: Boolean;
    FUniqueString: string;
    FRequestContent: TBytes;
    FLog: string;
    Procedure DoAfterReadBuildAgentResponse(Sender: TObject; AObject: TObject; JSON: TJSONObject);
    procedure JSONStringToObject(AString: string; AnObject: TObject);
    procedure DoDoneThread;
  public
    constructor Create(ABuildURL, AnAccessToken, AnUniqueString: string; ARequestContent: TBytes; AWebModule: TbmBuildTaskWM);
    procedure Execute; override;
  end;

{$R *.lfm}

{ TbmBuildTaskThread }

Procedure TbmBuildTaskThread.DoAfterReadBuildAgentResponse(Sender: TObject; AObject: TObject;
  JSON: TJSONObject);
begin
  if AObject is TfprBuildAgentResponse then
    begin
    TfprBuildAgentResponse(AObject).AType := JSON.Get('type', '');
    end;
end;

procedure TbmBuildTaskThread.JSONStringToObject(AString: string; AnObject: TObject);
var
  DeStreamer: TJSONDeStreamer;

begin
  DeStreamer := TJSONDeStreamer.Create(nil);
  try
    DeStreamer.AfterReadObject := @DoAfterReadBuildAgentResponse;
    DeStreamer.JSONToObject(AString, AnObject);
  finally
    DeStreamer.Free;
  end;
end;

procedure TbmBuildTaskThread.DoDoneThread;
begin
  FWebModule.SetSubBuildTask(FUniqueString, FAccessToken, FFailed, FLog);
  Free;
end;

constructor TbmBuildTaskThread.Create(ABuildURL, AnAccessToken, AnUniqueString: string;
  ARequestContent: TBytes; AWebModule: TbmBuildTaskWM);
begin
  FBuildURL := ABuildURL;
  FAccessToken := AnAccessToken;
  FWebModule := AWebModule;
  FFailed := True;
  FUniqueString := AnUniqueString;
  FRequestContent := Copy(ARequestContent);
  Inherited Create(False);
end;

procedure TbmBuildTaskThread.Execute;
var
  HTTPClient: TFPHTTPClient;
  MemStream: TMemoryStream;
  ContentStream: TBytesStream;
  BuildAgentResponseList: TfprBuildAgentResponseList;
  s: string;
  i: Integer;
begin
  try
    try
      HTTPClient := TFPHTTPClient.Create(nil);
      try
        if FAccessToken<>'' then
          HTTPClient.RequestHeaders.Values['authorization'] := 'Bearer ' + FAccessToken;

        MemStream := TMemoryStream.Create;
        try
          ContentStream := TBytesStream.Create(FRequestContent);
          try
            if Length(FRequestContent) > 0 then
              HTTPClient.RequestBody := ContentStream;
            HTTPClient.HTTPMethod('GET', FBuildURL, MemStream, [200]);
          finally
            ContentStream.Free;
          end;
          MemStream.Seek(0, soBeginning);

          SetLength(s, MemStream.Size);
          MemStream.Read(s[1], MemStream.Size);
          MemStream.Seek(0, soBeginning);

          FLog := s;

          BuildAgentResponseList := TfprBuildAgentResponseList.Create;
          try
            JSONStringToObject(s, BuildAgentResponseList);
            for i := 0 to BuildAgentResponseList.Count -1 do
              begin
              if sametext(BuildAgentResponseList.items[i].AType, 'Done') then
                FFailed := False;
              end;
          finally
            BuildAgentResponseList.Free;
          end;
        finally
          MemStream.Free;
        end;
      finally
        HTTPClient.Free;
      end;
    except
      on E: Exception do
        begin
        FLog := 'Exception during buildtask (' +FBuildURL + '): ' + E.Message + ' (' + E.ClassName + ')';
        end;
    end;
  Finally
    Queue(@DoDoneThread);
  end;
end;



{ TbmBuildTaskWM }

procedure TbmBuildTaskWM.DataModuleCreate(Sender: TObject);
var
  GlobalSettings: TDCSGlobalSettings;
begin
  FBuildTaskList := TfprBuildTaskList.Create(True);

  GlobalSettings := TDCSGlobalSettings.GetInstance;
  if GlobalSettings.GetSettingAsString('AllowCorsOrigin') <> '' then
    AddCorsOrigin(GlobalSettings.GetSettingAsString('AllowCorsOrigin'), 'POST, GET, PUT', '', True);
end;

procedure TbmBuildTaskWM.DataModuleDestroy(Sender: TObject);
begin
  FBuildTaskList.Free;
end;

Procedure TbmBuildTaskWM.DataModuleRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse; Var Handled: Boolean);
var
  UniqueString: string;
  BuildTask: TfprCustomBuildTask;
begin
  if ARequest.Method = 'POST' then
    HandleNewBuildRequest(ARequest, AResponse)
  else
    begin
    UniqueString := ARequest.GetNextPathInfo;
    if UniqueString <> '' then
      begin
      BuildTask := FBuildTaskList.FindByUniqueString(UniqueString);
      if Assigned(BuildTask) then
        begin
        AResponse.Content := ObjectToJSONContentString(BuildTask);
        AResponse.Code := 200;
        end
      else
        begin
        AResponse.Code := 404;
        AResponse.CodeText := GetStatusCode(AResponse.Code);
        end;
      end
    else
      begin
      AResponse.Content := ObjectToJSONContentString(FBuildTaskList);
      AResponse.Code := 200;
      end;
    end;
  Handled := True;
end;

Procedure TbmBuildTaskWM.HandleNewBuildRequest(ARequest: TRequest; AResponse: TResponse);
var
  BuildTask: TfprBuildTask;
  PackageJSON: TJSONObject;
  PackageURL: String;
  BuildAgentList: TbmBuildAgentList;
  PackageState: TJSONUnicodeStringType;

begin
  BuildTask := TfprBuildTask.Create;
  try
    JSONContentStringToObject(ARequest.Content, BuildTask);
    if BuildTask.PackageName='' then
      raise Exception.Create('Missing Packagename to build');

    if BuildTask.Tag='' then
      raise Exception.Create('Missing tag to build');

    PackageURL := TDCSGlobalSettings.GetInstance.GetSettingAsString('packagemanagerurl');
    if PackageURL = '' then
      Raise Exception.Create('No PackageManaterURL configured');
    PackageURL := PackageURL + '/package/'+BuildTask.PackageName;

    try
      PackageJSON := ObtainJSONRestRequest(PackageURL, True) as TJSONObject;
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
    try
      PackageState := PackageJSON.Get('packagestate', '');
      if (PackageState<>'published') and (PackageState<>'approved') then
        raise Exception.CreateFmt('Package %s can not be build because it''s state is %s', [BuildTask.PackageName, PackageState]);

      BuildAgentList := TbmBuildAgentList.Instance;

      if BuildTask.FPCVersion <> '' then
        BuildAgentList := BuildAgentList.ObtainBuildAgentListForFPCVersion(BuildTask.FPCVersion);

      if BuildAgentList.Count = 0 then
        raise Exception.Create('There are no buildagents registered, unable to build package.');

      BuildTask.SubTasks.Add(RequestSourceBuild(BuildAgentList.Items[0], BuildTask.PackageName, FAccessToken, BuildTask.Tag, BuildTask.FPCVersion));

      BuildTask.State := btsBuilding;

      AResponse.Content := ObjectToJSONContentString(BuildTask);

      FBuildTaskList.Add(BuildTask);
      BuildTask := nil;
    finally
      PackageJSON.Free;
    end;
  finally
    BuildTask.Free;
  end;
end;

function TbmBuildTaskWM.RequestBuild(BuildAgent: TbmBuildAgent; SourceZIP: TBytes; PackageName, AccessToken: string): TfprSubBuildTask;
var
  URL: string;
begin
  URL := ConcatPaths([BuildAgent.URL, 'build?cputarget=x86_64&ostarget=linux&fpcversion='+BuildAgent.FPCVersion+'&chunked=false']);
  Result := TfprSubBuildTask.Create();
  try
    Result.BuildAgent := BuildAgent.Name;
    Result.State := btsBuilding; // incorrect, should be waiting
    Result.URL := URL;
    TbmBuildTaskThread.Create(URL, AccessToken, Result.UniqueString, SourceZIP, Self);
  except
    result.Free;
    raise;
  end;
end;

function TbmBuildTaskWM.RequestSourceBuild(BuildAgent: TbmBuildAgent; PackageName, AccessToken, TAGName, FPCVersion: string): TfprSubBuildTask;
var
  URL: string;
  SourceZIP: TBytes;
  HTTPClient: TFPHTTPClient;
  BytesStream: TBytesStream;
begin
  URL := TDCSGlobalSettings.GetInstance.GetSettingAsString('repositoryurl');
  URL := URL + '/package/'+PackageName+'/'+TAGName;

  HTTPClient := TFPHTTPClient.Create(nil);
  try
    HTTPClient.RequestHeaders.Values['authorization'] := 'Bearer ' + AccessToken;
    BytesStream := TBytesStream.Create;
    try
      HTTPClient.Get(URL, BytesStream);
      SourceZIP := BytesStream.Bytes;
      SetLength(SourceZIP, BytesStream.Size);
    finally
      BytesStream.Free;
    end;
  finally
    HTTPClient.Free;
  end;

  URL := BuildAgent.URL + 'createsourcearchive?cputarget=x86_64&ostarget=linux&fpcversion='+BuildAgent.FPCVersion+'&chunked=false';
  Result := TfprSubBuildTask.Create();
  try
    Result.BuildAgent := BuildAgent.Name;
    Result.State := btsBuilding; // incorrect, should be waiting
    Result.SourceBuild := True;
    Result.FPCVersion := FPCVersion;
    Result.URL := URL;
    TbmBuildTaskThread.Create(URL, AccessToken, Result.UniqueString, SourceZIP, Self);
  except
    result.Free;
    raise;
  end;
end;

function TbmBuildTaskWM.GetSourceZip(TaskResponse: string): TBytes;
var
  BuildResponseList: TfprBuildAgentResponseList;
  SourceURL: string;
  BytesStream: TBytesStream;

begin
  BuildResponseList := TfprBuildAgentResponseList.Create;
  try
    JSONContentStringToObject(TaskResponse, BuildResponseList);
    if BuildResponseList.Count = 0 then
      begin
      raise Exception.Create('Invalid source-build-task response: ' + TaskResponse);
      end;
    SourceURL := BuildResponseList.Items[BuildResponseList.Count -1].SourceArchive;

    if SourceURL='' then
      raise Exception.Create('Missing SourceURL');
  finally
    BuildResponseList.Free;
  end;


  BytesStream := TBytesStream.Create([]);
  try
    try
      TFPHTTPClient.SimpleGet(SourceURL, BytesStream);
    except
      on E: Exception do
        raise Exception.CreateFmt('Exception during retrieval of source-zip (%s): %s (%s)', [SourceURL, E.Message, E.ClassName]);
    end;
    Result := BytesStream.Bytes;
    SetLength(Result, BytesStream.Size);
  finally
    BytesStream.Free;
  end;
end;

procedure TbmBuildTaskWM.SetSubBuildTask(UniqueString, AccessToken: string; Failed: Boolean;
  Log: string);
var
  i,j, k: Integer;
  SubTask: TfprSubBuildTask;
  TaskFinished: Boolean;
  TaskFailed, HasAppropiateBuildAgent: Boolean;
  BuildAgentList: TbmBuildAgentList;
  SourceZip: TBytes;
begin
  for i := 0 to FBuildTaskList.Count -1 do
    begin
    try
      if FBuildTaskList[i].State in [btsFailed, btsSuccess] then
        Continue;

      TaskFinished := True;
      TaskFailed := False;
      for j := 0 to FBuildTaskList[i].SubTasks.Count -1 do
        begin
        SubTask := FBuildTaskList[i].SubTasks[j];
        if FBuildTaskList[i].SubTasks[j].UniqueString = UniqueString then
          begin
          if Failed then
            SubTask.State := btsFailed
          else
            begin
            SubTask.State := btsSuccess;
            if SubTask.SourceBuild then
              begin
              BuildAgentList := TbmBuildAgentList.Instance.ObtainBuildAgentListForFPCVersion(SubTask.FPCVersion);
              try
                HasAppropiateBuildAgent := False;
                SourceZip := GetSourceZip(Log);
                for k := 0 to BuildAgentList.Count -1 do
                  begin
                  FBuildTaskList[i].SubTasks.Add(RequestBuild(BuildAgentList.Items[k], SourceZip, FBuildTaskList[i].PackageName, AccessToken));
                  HasAppropiateBuildAgent := True;
                  end;
                if HasAppropiateBuildAgent then
                  TaskFinished := False
                else
                  TaskFailed := True;
              finally
                BuildAgentList.Free;;
              end;
              end;
            end;
          SubTask.Log := Log;
          end;
        if SubTask.State in [btsBuilding, btsWaiting] then
          TaskFinished := False;
        if SubTask.State = btsFailed then
          TaskFailed := True;
        end;
      if TaskFinished then
        begin
        if TaskFailed then
          FBuildTaskList[i].State := btsFailed
        else
          FBuildTaskList[i].State := btsSuccess;
        end;
    except
      on E: Exception do
        begin
        FBuildTaskList[i].State := btsFailed;
        FBuildTaskList[i].ErrorMessage := 'Exception during setup of subtasks: ' + E.Message + ' (' + E.ClassName + ')';
        end;
    end;
    end;
end;

initialization
  RegisterHTTPModule('buildtask', TbmBuildTaskWM);
end.

