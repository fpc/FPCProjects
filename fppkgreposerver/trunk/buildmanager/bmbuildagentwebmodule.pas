unit bmBuildAgentWebmodule;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  Classes,
  fgl,
  httpdefs,
  fpHTTP,
  fphttpserver,
  fprWebModule;

type

  { TbmBuildAgent }

  TbmBuildAgent = class(TObject)

  private
    FName: string;
    FURL: string;
  published
    property Name: string read FName write FName;
    property URL: string read FURL write FURL;
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
  protected
    function RequireAuthentication(ARequest: TRequest): Boolean; override;
  public

  end;

var
  bmBuildWM: TbmBuildWM;

implementation

uses
  fprErrorHandling;

{$R *.lfm}

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

{ TbmBuildWM }

procedure TbmBuildWM.DataModuleCreate(Sender: TObject);
begin
end;

procedure TbmBuildWM.DataModuleDestroy(Sender: TObject);
begin
end;

Procedure TbmBuildWM.listRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse;
  Var Handled: Boolean);
begin
  AResponse.Content := ObjectToJSONContentString(TbmBuildAgentList.Instance);
  AResponse.Code := 200;
  AResponse.CodeText := GetStatusCode(AResponse.Code);
  Handled := True;
end;

Procedure TbmBuildWM.RegisteragentRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse; Var Handled: Boolean);
var
  NewAgent: TbmBuildAgent;
begin
  NewAgent := TbmBuildAgent.Create;
  try
    JSONContentStringToObject(ARequest.Content, NewAgent);

    if Assigned(TbmBuildAgentList.Instance.FindBuildAgentByName(NewAgent.Name)) then
      begin
      raise Exception.CreateFmt('Agent with name ''%s'' already registered.', [NewAgent.Name]);
      end;

    if Length(NewAgent.Name) < 4 then
      begin
      raise Exception.CreateFmt('Invalid agent name ''%s''', [NewAgent.Name]);
      end;

    AResponse.Content := ObjectToJSONContentString(NewAgent);
    AResponse.Code := 200;
    AResponse.CodeText := GetStatusCode(AResponse.Code);

    TbmBuildAgentList.Instance.Add(NewAgent);
    NewAgent := nil;
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

initialization
  RegisterHTTPModule('agent', TbmBuildWM, False);
end.

