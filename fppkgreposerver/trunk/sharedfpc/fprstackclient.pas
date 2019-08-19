unit fprStackClient;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  dcsGlobalSettings,
  cnocStackBinaryClient,
  cnocStackJSONHandlerThread,
  fprGCollection;

type

  { TfprStackClient }

  TfprStackClient = class
  private
    FHandler: TcnocStackJSONHandlerThread;
    FClient: TcnocStackBinaryClient;

    function GetHandler: TcnocStackJSONHandlerThread;
    function GetClient: TcnocStackBinaryClient;
  public
    destructor Destroy; override;
    procedure InitHandler(SubscribeToStacks: TStringArray);
    property Handler: TcnocStackJSONHandlerThread read GetHandler;
    property Client: TcnocStackBinaryClient read GetClient;
  end;

  TfprStackClientSingleton = specialize TcnocSingleton<TfprStackClient>;

implementation

{ TfprStackClient }

function TfprStackClient.GetClient: TcnocStackBinaryClient;
begin
  if not Assigned(FClient) then
    FClient := TcnocStackBinaryClient.Create(TDCSGlobalSettings.GetInstance.GetSettingAsString('StackHost'), StrToInt(TDCSGlobalSettings.GetInstance.GetSettingAsString('StackPort')));
  Result := FClient;
  if not Result.IsConnected then
    Result.Connect();
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

end.

