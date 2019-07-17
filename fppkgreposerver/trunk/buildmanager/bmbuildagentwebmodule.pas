unit bmBuildAgentWebmodule;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  Classes,
  fgl,
  resolve,
  sockets,
  httpdefs,
  dcsGlobalSettings,
  fpHTTP,
  fphttpserver,
  fprWebModule;

type

  { TbmBuildAgent }

  TbmBuildAgent = class(TObject)
  private
    FName: string;
    FURL: string;
    FFPCVersion: string;
    procedure SetURL(AValue: string);
  published
    property Name: string read FName write FName;
    property URL: string read FURL write SetURL;
    property FPCVersion: string read FFPCVersion write FFPCVersion;
  end;

  TbmCustomBuildAgentList = specialize TFPGObjectList<TbmBuildAgent>;

  { TbmBuildAgentList }

  TbmBuildAgentList = class(TbmCustomBuildAgentList)
  private
    class var FListInstance: TbmBuildAgentList;
    class destructor Destroy;
  public
    class function Instance: TbmBuildAgentList;
    function FindBuildAgentByName(AName: string): TbmBuildAgent;
    function ObtainBuildAgentListForFPCVersion(FPCVersion: string): TbmBuildAgentList;
  end;

  { TbmBuildWM }

  TbmBuildWM = class(TfprWebModule)
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    Procedure listRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse;
      Var Handled: Boolean);
    Procedure RegisteragentRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse; Var Handled: Boolean);
    Procedure UnregisterRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse;
      Var Handled: Boolean);
  private
    FHostMappingIPAdresses: TStringList;
  protected
    function RequireAuthentication(ARequest: TRequest): Boolean; override;
    function TranslateBuildagentListToJSONString(MapHosts: boolean; BuildagentList: TbmBuildAgentList): string;
  public

  end;

var
  bmBuildWM: TbmBuildWM;

implementation

uses
  fprErrorHandling;

{$R *.lfm}

{ TbmBuildAgent }

procedure TbmBuildAgent.SetURL(AValue: string);
begin
  if FURL = AValue then Exit;
  FURL := IncludeHTTPPathDelimiter(AValue);
end;

{ TbmBuildAgentList }

class destructor TbmBuildAgentList.Destroy;
begin
  FListInstance.Free;
end;

class function TbmBuildAgentList.Instance: TbmBuildAgentList;
begin
  if not Assigned(FListInstance) then
    FListInstance := TbmBuildAgentList.Create(True);
  Result := FListInstance;
end;

function TbmBuildAgentList.FindBuildAgentByName(AName: string): TbmBuildAgent;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count -1 do
    begin
    if Items[i].Name = AName then
      begin
      Result := Items[i];
      Break;
      end;
    end;
end;

function TbmBuildAgentList.ObtainBuildAgentListForFPCVersion(FPCVersion: string): TbmBuildAgentList;
var
  AList: TbmBuildAgentList;
  i: Integer;
begin
  AList := TbmBuildAgentList.Create(False);
  try
    for i := 0 to Count -1 do
      if (FPCVersion='') or (Items[i].FPCVersion = FPCVersion) then
        AList.Add(Items[i]);
    Result := AList;
    AList := nil;
  finally
    AList.Free;
  end;
end;

{ TbmBuildWM }

procedure TbmBuildWM.DataModuleCreate(Sender: TObject);
var
  GlobalSettings: TDCSGlobalSettings;
  s: ansistring;
  HostResolver: THostResolver;
  i: Integer;
begin
  GlobalSettings := TDCSGlobalSettings.GetInstance;
  if GlobalSettings.GetSettingAsString('AllowCorsOrigin') <> '' then
    AddCorsOrigin(GlobalSettings.GetSettingAsString('AllowCorsOrigin'), 'POST, GET, PUT', '', True);

  FHostMappingIPAdresses := TStringList.Create;

  s := GlobalSettings.GetSettingAsString('HostNameToEnableMapping');
  if s <> '' then
    begin
    HostResolver := THostResolver.Create(nil);
    try
      if not HostResolver.NameLookup(s) then
        raise Exception.CreateFmt('Could not find hostname %s', [s]);
      for i := 0 to HostResolver.AddressCount -1 do
        begin
        s := HostAddrToStr(HostResolver.Addresses[i]);
        FHostMappingIPAdresses.Add(HostAddrToStr(HostResolver.Addresses[i]));
        end;
    finally
      HostResolver.Free;
    end;
    end;
end;

procedure TbmBuildWM.DataModuleDestroy(Sender: TObject);
begin
  FHostMappingIPAdresses.Free;
end;

Procedure TbmBuildWM.listRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse;
  Var Handled: Boolean);
var
  FPCVersion, ExtHost: string;
  BuildAgentList: TbmBuildAgentList;
  i: Integer;
  MustMap: Boolean;
begin
  ExtHost := ARequest.RemoteAddr;
  MustMap := FHostMappingIPAdresses.IndexOf(ExtHost) > -1;

  FPCVersion := ARequest.GetNextPathInfo;
  if FPCVersion<>'' then
    begin
    BuildAgentList := TbmBuildAgentList.Create(False);
    try
      for i := 0 to TbmBuildAgentList.Instance.Count -1 do
        begin
        if TbmBuildAgentList.Instance.Items[i].FPCVersion = FPCVersion then
          BuildAgentList.Add(TbmBuildAgentList.Instance.Items[i]);
        end;
      AResponse.Content := TranslateBuildagentListToJSONString(MustMap, BuildAgentList);
    finally
      BuildAgentList.Free;
    end;
    end
  else
    begin
    AResponse.Content := TranslateBuildagentListToJSONString(MustMap, TbmBuildAgentList.Instance);
    end;

  AResponse.Code := 200;
  AResponse.CodeText := GetStatusCode(AResponse.Code);
  Handled := True;
end;

Procedure TbmBuildWM.RegisteragentRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse; Var Handled: Boolean);
var
  OldAgent: TbmBuildAgent;
  NewAgent: TbmBuildAgent;
begin
  NewAgent := TbmBuildAgent.Create;
  try
    JSONContentStringToObject(ARequest.Content, NewAgent);

    if Length(NewAgent.Name) < 4 then
      begin
      raise Exception.CreateFmt('Invalid agent name ''%s''', [NewAgent.Name]);
      end;

    if NewAgent.FPCVersion = '' then
      begin
      raise Exception.Create('Missing FPC-Version for agent');
      end;

    AResponse.Content := ObjectToJSONContentString(NewAgent);
    AResponse.Code := 200;
    AResponse.CodeText := GetStatusCode(AResponse.Code);

    OldAgent := TbmBuildAgentList.Instance.FindBuildAgentByName(NewAgent.Name);
    if Assigned(OldAgent) then
      begin
      OldAgent.URL := NewAgent.URL;
      OldAgent.FFPCVersion := NewAgent.FPCVersion;
      end
    else
      begin
      TbmBuildAgentList.Instance.Add(NewAgent);
      NewAgent := nil;
      end;
  finally
    NewAgent.Free;
  end;
  Handled := True
end;

Procedure TbmBuildWM.UnregisterRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse;Var Handled: Boolean);
var
  Agent: TbmBuildAgent;
  BuildAgentName: string;
begin
  if ARequest.Method<>'DELETE' then
    raise EJsonWebException.CreateHelp('Only DELETE requests are allowed to unregister clients.', 405);

  BuildAgentName := ARequest.GetNextPathInfo;
  if BuildAgentName='' then
    raise EJsonWebException.CreateHelp('No buildagent name provided', 400);
  Agent := TbmBuildAgentList.Instance.FindBuildAgentByName(BuildAgentName);
  if not Assigned(Agent) then
    raise EJsonWebException.CreateFmtHelp('Buildagent [%s] is not registered.', [BuildAgentName], 404);

  AResponse.Content := ObjectToJSONContentString(Agent);

  TbmBuildAgentList.Instance.Remove(Agent);
  AResponse.Code := 200;
  AResponse.CodeText := GetStatusCode(AResponse.Code);

  Handled := True
end;

function TbmBuildWM.RequireAuthentication(ARequest: TRequest): Boolean;
begin
  Result := False;
end;

function TbmBuildWM.TranslateBuildagentListToJSONString(MapHosts: boolean; BuildagentList: TbmBuildAgentList): string;
var
  ListClone: TbmBuildAgentList;
  Original, Clone: TbmBuildAgent;
  i: Integer;
  GlobalSettings: TDCSGlobalSettings;
  TranslatedHost: string;
begin
  GlobalSettings := TDCSGlobalSettings.GetInstance;
  if MapHosts then
    begin
    ListClone := TbmBuildAgentList.Create(True);
    try
      for i := 0 to BuildagentList.Count -1 do
        begin
        Original := BuildagentList.Items[i];
        Clone := TbmBuildAgent.Create;
        try
          Clone.FPCVersion := Original.FPCVersion;
          Clone.Name := Original.Name;
          TranslatedHost := GlobalSettings.GetDictionarySettingAsString('HostMap', Original.URL);
          if TranslatedHost <> '' then
            Clone.URL := TranslatedHost
          else
            Clone.URL := Original.URL;
          ListClone.Add(Clone);
          Clone := nil;
        finally
          Clone.Free;
        end;
        end;
      Result := ObjectToJSONContentString(ListClone);
    finally
      ListClone.Free;
    end;
    end
  else
    Result := ObjectToJSONContentString(BuildagentList);
end;

initialization
  RegisterHTTPModule('agent', TbmBuildWM, False);
end.

