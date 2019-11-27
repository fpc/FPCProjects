unit fprCrudWebHandler;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  Classes,
  httpdefs,
  fpHTTP,
  fpjson,
  httproute,
  dcsGlobalSettings,
  cnocStackJSONHandlerThread,
  cnocStackHandlerThread,
  cnocStackMessageTypes,
  fprErrorHandling,
  fprGCollection,
  fprAuthenticationHandler,
  fprModel,
  fprJSONFileStreaming,
  fprWebHandler,
  fprStackClient;

type

  TfprCrudRequestMethod = (catrmGet, catrmUpdate, catrmAdd, catrmDelete);

  { TfprCrudWebHandler }

  generic TfprCrudWebHandler<T: TfprCrudCollectionItem> = class(TfprWebHandler, IRouteInterface, IcnocStackHandler, IcnocStackJSONRespondToMessage)
  private
    type TfprCrudCollection = specialize TcnocGCrudCollection<T>;
    type TfprCrudCollectionSingleton = specialize TcnocSingleton<TfprCrudCollection>;
  private
    FSaveCollectionFilename: string;
    FStackName: string;
    FUrlPath: string;
    FCollection: TfprCrudCollection;
  protected
    procedure SaveCollection(Collection: TCollection); virtual;
    procedure CheckValid(Item: TfprCrudCollectionItem); virtual;
  protected
    function DoHandleRequest(ARequest: TRequest; JSONContent: TJSONData): TJSONData; override;
    procedure DoRespondToJSONMessage(const IncomingMessage: PcnocStackMessage; const JSONData: TJSONObject; out AResponse: TJSONData; var Handled: Boolean); override;

    function HandleCrudRequest(Method: TfprCrudRequestMethod; Id: string; JSONData: TJSONData; AccessToken: string): TJSONData; virtual;
    function HandleCrudGetRequest(Id: string; AccessToken: string): TJSONData; virtual;
    function HandleCrudDeleteRequest(Id: string; AccessToken: string): TJSONData; virtual;
    function HandleCrudAddRequest(JSONData: TJSONData; AccessToken: string): TJSONData; virtual;
    function HandleCrudUpdateRequest(Id: string; JSONData: TJSONData; AccessToken: string): TJSONData; virtual;
  public
    property SaveCollectionFilename: string read FSaveCollectionFilename write FSaveCollectionFilename;
    property StackName: string read FStackName write FStackname;
    property UrlPath: string read FUrlPath write FUrlPath;
    property Collection: TfprCrudCollection read FCollection write FCollection;

  end;

implementation

{ TfprCrudWebHandler }

function TfprCrudWebHandler.DoHandleRequest(ARequest: TRequest; JSONContent: TJSONData): TJSONData;
var
  AuthorizationToken, AccessToken: String;
begin
  Result := nil;
  AuthorizationToken := ARequest.Authorization;
  if copy(AuthorizationToken, 1, 7) = 'Bearer ' then
    AccessToken := copy(AuthorizationToken, 8, length(AuthorizationToken) - 7)
  else
    AccessToken := '';

  if ARequest.Method = 'GET' then
    Result := HandleCrudRequest(catrmGet, ARequest.RouteParams[FUrlPath], JSONContent, AccessToken)
  else if ARequest.Method = 'POST' then
    Result := HandleCrudRequest(catrmAdd, ARequest.RouteParams[FUrlPath], JSONContent, AccessToken)
  else if ARequest.Method = 'DELETE' then
    Result := HandleCrudRequest(catrmDelete, ARequest.RouteParams[FUrlPath], JSONContent, AccessToken)
  else if ARequest.Method = 'PUT' then
    Result := HandleCrudRequest(catrmUpdate, ARequest.RouteParams[FUrlPath], JSONContent, AccessToken);
end;

procedure TfprCrudWebHandler.DoRespondToJSONMessage(const IncomingMessage: PcnocStackMessage; const JSONData: TJSONObject; out AResponse: TJSONData; var Handled: Boolean);
var
  Action: TJSONUnicodeStringType;
  ReqMethod: TfprCrudRequestMethod;
  AccessToken: String;
begin
  Action := JSONData.Get('action', 'get');
  if (Action = 'get') then
    ReqMethod := catrmGet
  else if Action = 'delete' then
    ReqMethod := catrmDelete
  else if Action = 'add' then
    ReqMethod := catrmAdd
  else if Action = 'update' then
    ReqMethod := catrmUpdate
  else
    raise EJsonWebException.CreateFmtHelp('Unsupported action [%s].', [Action], 400);

  if IncomingMessage^.Header.ExtAccessKeyCount>0 then
    AccessToken := IncomingMessage^.GetExtAccessKey(0);

  AResponse := HandleCrudRequest(ReqMethod, JSONData.Get('key', ''), JSONData.Get('data', TJSONObject(nil)) as TJSONObject, AccessToken);
end;

function TfprCrudWebHandler.HandleCrudRequest(Method: TfprCrudRequestMethod;
  Id: string; JSONData: TJSONData; AccessToken: string): TJSONData;
var
  Msg: string;
begin
  Result := nil;

  if AccessToken<>'' then
    begin
    if not TfprAuthenticationHandler.GetInstance.VerifyAccessToken(AccessToken, Msg) then
      raise EJsonWebException.CreateFmtHelp('Authentication failed: %s', [Msg], 403);
    end;

  case Method of
    catrmGet:
      begin
      if Assigned(JSONData) and not JSONData.IsNull then
        raise EJsonWebException.CreateHelp('Did not expect any object data.', 400);
      Result := HandleCrudGetRequest(Id, AccessToken);
      end;
    catrmAdd:
      begin
      if Id<>'' then
        raise EJsonWebException.CreateFmtHelp('Received an unexpected Id [%s].', [Id], 400);
      Result := HandleCrudAddRequest(JSONData, AccessToken);
      end;
    catrmDelete:
      begin
      if Assigned(JSONData) and not JSONData.IsNull then
        raise EJsonWebException.CreateHelp('Did not expect any object data.', 400);
      Result := HandleCrudDeleteRequest(Id, AccessToken);
      end;
    catrmUpdate:
      begin
      Result := HandleCrudUpdateRequest(Id, JSONData, AccessToken);
      end;
  end;
end;

function TfprCrudWebHandler.HandleCrudGetRequest(Id: string; AccessToken: string): TJSONData;
begin
  Result := ObjectToJSON(FCollection);
end;

function TfprCrudWebHandler.HandleCrudAddRequest(JSONData: TJSONData; AccessToken: string): TJSONData;
var
  Item: T;
begin
  if not Assigned(JSONData) or JSONData.IsNull then
    raise EJsonWebException.CreateHelp('Missing object data.', 400);

  If TfprAuthenticationHandler.GetInstance.GetUserRole(AccessToken) <> 'admin' then
    raise EJsonWebException.CreateHelp('Not enough rights to modify package categories.', 403);

  Item := T.Create(nil);

  JSONContentToObject(JSONData as TJSONObject, Item);

  CheckValid(Item);

  if Assigned(Collection.FindObjectByName(Item.Name)) then
    raise EJsonWebException.CreateFmtHelp('Object with name [%s] already exists.', [Item.Name], 422);
  if Assigned(Collection.FindObjectById(Item.Id)) then
    raise EJsonWebException.CreateFmtHelp('Object with id [%d] already exists.', [Item.Id], 422);
  Item.Collection := Collection;

  SaveCollection(Collection);

  Result := ObjectToJSON(Item);
end;

function TfprCrudWebHandler.HandleCrudDeleteRequest(Id: string; AccessToken: string): TJSONData;
var
  Item: T;
begin
  If TfprAuthenticationHandler.GetInstance.GetUserRole(AccessToken) <> 'admin' then
    raise EJsonWebException.CreateHelp('Not enough rights to delete package categories.', 403);

  if Id='' then
    begin
    FCollection.Clear;
    SaveCollection(FCollection);
    Result := ObjectToJSON(FCollection);
    end
  else
    begin
    Item := FCollection.FindObjectById(StrToIntDef(Id, -1));
    if not Assigned(Item) then
      raise EJsonWebException.CreateFmtHelp('Object [%s] not found', [Id], 404);
    result := ObjectToJSON(Item);
    Item.Free;
    SaveCollection(FCollection);
    end;
end;

procedure TfprCrudWebHandler.SaveCollection(Collection: TCollection);
begin
  if FSaveCollectionFilename <> '' then
    SaveCollectionToJSONFile(Collection, FSaveCollectionFilename);
  TfprStackClientSingleton.Instance.Client.SendMessage(smtToStack, FStackName + 'Monitor', [], 0, '{"name":"signal"}');
end;

function TfprCrudWebHandler.HandleCrudUpdateRequest(Id: string; JSONData: TJSONData; AccessToken: string): TJSONData;
var
  Item: T;
  TempItem: T;
  JSData: TJSONObject;
  i: Integer;
begin
  if not Assigned(JSONData) or JSONData.IsNull then
    raise EJsonWebException.CreateHelp('Missing object data.', 400);

  if JSONData.JSONType <> jtObject then
    raise EJsonWebException.CreateHelp('Invalid object data', 400);

  if Id='' then
    raise EJsonWebException.CreateHelp('Missing id', 400);

  If TfprAuthenticationHandler.GetInstance.GetUserRole(AccessToken) <> 'admin' then
    raise EJsonWebException.CreateHelp('Not enough rights to modify package categories.', 403);

  Item := Collection.FindObjectById(StrToIntDef(Id, -1));
  if not Assigned(Item) then
    raise EJsonWebException.CreateFmtHelp('Object with id [%d] does not exist', [Id], 404);

  // Create a clone of the existing package, and apply the changes to this clone
  // to do the validity-checks.
  JSData := ObjectToJSON(Item) as TJSONObject;
  try
    TempItem := T.Create(nil);
    try
      JSONContentToObject(JSData, TempItem);
      JSONContentToObject(TJSONObject(JSONData), TempItem);
      CheckValid(TempItem);

      if TempItem.Id <> StrToIntDef(Id, -1) then
        raise EJsonWebException.CreateFmtHelp('Id [%d] of object does not match with [%s].', [TempItem.Id, Id], 400);

      for i := 0 to Collection.Count -1 do
        begin
        if (Collection.Items[i].Id <> TempItem.Id) and
           SameText(Collection.Items[i].Name, TempItem.Name) then
          raise EJsonWebException.CreateFmtHelp('Another object does already exist with name [%s].', [TempItem.Name], 422);
        end;
    finally
      TempItem.Free;
    end;
  finally
    JSData.Free;
  end;

  JSONContentToObject(TJSONObject(JSONData), Item);
  SaveCollection(Collection);

  Result := ObjectToJSON(Item);
end;

procedure TfprCrudWebHandler.CheckValid(Item: TfprCrudCollectionItem);
begin
  if Item.Id = 0 then
    raise EJsonWebException.CreateHelp('Object must have an id.', 422);

  if not Item.CheckValidity then
    raise EJsonWebException.CreateHelp('Object does not validate', 422);
end;

end.
