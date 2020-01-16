unit fprStackClient;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpjson,
  dcsGlobalSettings,
  csJSONRttiStreamHelper,
  csModel,
  cnocStackBinaryClient,
  cnocStackJSONHandlerThread,
  cnocStackErrorCodes,
  fprGCollection;

type

  { TfprStackClient }

  TfprStackClient = class
  private
    FHandler: TcnocStackJSONHandlerThread;
    FClient: TcnocStackBinaryClient;

    function GetHandler: TcnocStackJSONHandlerThread;
    function GetClient: TcnocStackBinaryClient;
    function GetSerializer: TJSONRttiStreamClass;
  public
    destructor Destroy; override;
    procedure InitHandler(SubscribeToStacks: TStringArray);

    generic function Call<T: TObject>(const StackName, Message: string; AccessToken: string = ''): T;
    function Call(const StackName, Message: string; AccessToken: string = ''): TJSONData;

    property Handler: TcnocStackJSONHandlerThread read GetHandler;
    property Client: TcnocStackBinaryClient read GetClient;
  end;

  TfprStackClientSingleton = specialize TcnocSingleton<TfprStackClient>;

implementation

uses
  fprSerializer;

{ TfprStackClient }

function TfprStackClient.GetClient: TcnocStackBinaryClient;
begin
  if not Assigned(FClient) then
    FClient := TcnocStackBinaryClient.Create(TDCSGlobalSettings.GetInstance.GetSettingAsString('StackHost'), StrToInt(TDCSGlobalSettings.GetInstance.GetSettingAsString('StackPort')));
  Result := FClient;
  if not Result.IsConnected then
    try
      Result.Connect();
    except
      // GetClient should not fail when the connection failed.
      // So do nothing...
    end;
end;

function TfprStackClient.GetHandler: TcnocStackJSONHandlerThread;
begin
  if not Assigned(FHandler) then
    raise Exception.Create('Stack client handler not initialized');
  Result := FHandler;
end;

procedure TfprStackClient.InitHandler(SubscribeToStacks: TStringArray);
begin
  if Assigned(FHandler) then
    raise Exception.Create('Stack client handler already initialized');
  FHandler := TcnocStackJSONHandlerThread.Create(TDCSGlobalSettings.GetInstance.GetSettingAsString('StackHost'), StrToInt(TDCSGlobalSettings.GetInstance.GetSettingAsString('StackPort')), SubscribeToStacks);
end;

destructor TfprStackClient.Destroy;
begin
  FClient.Free;
  FHandler.Terminate;
  inherited Destroy;
end;

function TfprStackClient.Call(const StackName, Message: string; AccessToken: string = ''): TJSONData;
var
  ResponseData: string;
  Response: TcnocStackErrorCodes;
begin
  Response := Client.SendMessage(StackName, Message, ResponseData, [AccessToken]);
  if Response = ecNone then
    begin
    Result := GetJSON(ResponseData);
    end
  else
    Result := nil;
end;

generic function TfprStackClient.Call<T>(const StackName, Message: string; AccessToken: string = ''): T;
var
  JSData: TJSONObject;
begin
  Result := nil;
  JSData := Call(StackName, Message, AccessToken) as TJSONObject;
  try
    if Assigned(JSData) then
      begin
      Result := T.ClassType.Create() as T;
      GetSerializer.JSONToObject(JSData, Result);
      end;
  finally
    JSData.Free;
  end;
end;

function TfprStackClient.GetSerializer: TJSONRttiStreamClass;
begin
  Result := TfprSerializerSingleton.Instance;
end;

end.

