unit catCategoryWebHandler;

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
  fprAuthenticationHandler,
  fprModel,
  fprJSONFileStreaming,
  fprWebHandler,
  fprStackClient;

type

  TcatCategoryRequestMethod = (catrmGet, catrmUpdate, catrmAdd, catrmDelete);

  { TcatCategoryWebHandler }

  TcatCategoryWebHandler = class(TfprWebHandler, IRouteInterface, IcnocStackHandler, IcnocStackJSONRespondToMessage)
  protected
    procedure SaveCollection(Collection: TCollection);
    procedure CheckValid(Category: TfprPackageCategory);
  protected
    function DoHandleRequest(ARequest: TRequest; JSONContent: TJSONData): TJSONData; override;
    procedure DoRespondToJSONMessage(const IncomingMessage: PcnocStackMessage; const JSONData: TJSONObject; out AResponse: TJSONData; var Handled: Boolean); override;

    function HandleCategoryRequest(Method: TcatCategoryRequestMethod; CategoryId: string; Category: TJSONData; AccessToken: string): TJSONData; virtual;
    function HandleCategoryGetRequest(CategoryId: string; AccessToken: string): TJSONData; virtual;
    function HandleCategoryDeleteRequest(CategoryId: string; AccessToken: string): TJSONData; virtual;
    function HandleCategoryAddRequest(CategoryJSON: TJSONData; AccessToken: string): TJSONData; virtual;
    function HandleCategoryUpdateRequest(CategoryId: string; CategoryJSON: TJSONData; AccessToken: string): TJSONData; virtual;
  public

  end;

implementation

{ TcatCategoryWebHandler }

function TcatCategoryWebHandler.DoHandleRequest(ARequest: TRequest; JSONContent: TJSONData): TJSONData;
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
    Result := HandleCategoryRequest(catrmGet, ARequest.RouteParams['category'], JSONContent, AccessToken)
  else if ARequest.Method = 'POST' then
    Result := HandleCategoryRequest(catrmAdd, ARequest.RouteParams['category'], JSONContent, AccessToken)
  else if ARequest.Method = 'DELETE' then
    Result := HandleCategoryRequest(catrmDelete, ARequest.RouteParams['category'], JSONContent, AccessToken)
  else if ARequest.Method = 'PUT' then
    Result := HandleCategoryRequest(catrmUpdate, ARequest.RouteParams['category'], JSONContent, AccessToken);
end;

procedure TcatCategoryWebHandler.DoRespondToJSONMessage(const IncomingMessage: PcnocStackMessage; const JSONData: TJSONObject; out AResponse: TJSONData; var Handled: Boolean);
var
  Action: TJSONUnicodeStringType;
  ReqMethod: TcatCategoryRequestMethod;
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

  AResponse := HandleCategoryRequest(ReqMethod, JSONData.Get('key', ''), JSONData.Get('data', TJSONObject(nil)) as TJSONObject, AccessToken);
end;

function TcatCategoryWebHandler.HandleCategoryRequest(Method: TcatCategoryRequestMethod;
  CategoryId: string; Category: TJSONData; AccessToken: string): TJSONData;
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
      if Assigned(Category) and not Category.IsNull then
        raise EJsonWebException.CreateHelp('Did not expect any category data.', 400);
      Result := HandleCategoryGetRequest(CategoryId, AccessToken);
      end;
    catrmAdd:
      begin
      if CategoryId<>'' then
        raise EJsonWebException.CreateFmtHelp('Received an unexpected CategoryId [%s].', [CategoryId], 400);
      Result := HandleCategoryAddRequest(Category, AccessToken);
      end;
    catrmDelete:
      begin
      if Assigned(Category) and not Category.IsNull then
        raise EJsonWebException.CreateHelp('Did not expect any category data.', 400);
      Result := HandleCategoryDeleteRequest(CategoryId, AccessToken);
      end;
    catrmUpdate:
      begin
      Result := HandleCategoryUpdateRequest(CategoryId, Category, AccessToken);
      end;
  end;
end;

function TcatCategoryWebHandler.HandleCategoryGetRequest(CategoryId: string; AccessToken: string): TJSONData;
begin
  Result := ObjectToJSON(TfprPackageCategoryCollectionSingleton.Instance);
end;

function TcatCategoryWebHandler.HandleCategoryAddRequest(CategoryJSON: TJSONData; AccessToken: string): TJSONData;
var
  Category: TfprPackageCategory;
  Collection: TfprPackageCategoryCollection;
begin
  if not Assigned(CategoryJSON) or CategoryJSON.IsNull then
    raise EJsonWebException.CreateHelp('Missing category data.', 400);

  If TfprAuthenticationHandler.GetInstance.GetUserRole(AccessToken) <> 'admin' then
    raise EJsonWebException.CreateHelp('Not enough rights to modify package categories.', 403);

  Category := TfprPackageCategory.Create(nil);

  JSONContentToObject(CategoryJSON as TJSONObject, Category);

  CheckValid(Category);

  Collection := TfprPackageCategoryCollectionSingleton.Instance;
  if Assigned(Collection.FindObjectByName(Category.Name)) then
    raise EJsonWebException.CreateFmtHelp('Package category with name [%s] already exists.', [Category.Name], 422);
  if Assigned(Collection.FindObjectById(Category.CategoryId)) then
    raise EJsonWebException.CreateFmtHelp('Package category with id [%d] already exists.', [Category.CategoryId], 422);
  Category.Collection := Collection;

  SaveCollection(Collection);

  Result := ObjectToJSON(Category);
end;

function TcatCategoryWebHandler.HandleCategoryDeleteRequest(CategoryId: string; AccessToken: string): TJSONData;
var
  CategoryList: TfprPackageCategoryCollection;
  Category: TfprPackageCategory;
begin
  If TfprAuthenticationHandler.GetInstance.GetUserRole(AccessToken) <> 'admin' then
    raise EJsonWebException.CreateHelp('Not enough rights to delete package categories.', 403);

  CategoryList := TfprPackageCategoryCollectionSingleton.Instance;
  if CategoryId='' then
    begin
    CategoryList.Clear;
    SaveCollection(CategoryList);
    Result := ObjectToJSON(CategoryList);
    end
  else
    begin
    Category := CategoryList.FindObjectById(StrToIntDef(CategoryId, -1));
    if not Assigned(Category) then
      raise EJsonWebException.CreateFmtHelp('Category [%s] not found', [CategoryId], 404);
    result := ObjectToJSON(Category);
    Category.Free;
    SaveCollection(CategoryList);
    end;
end;

procedure TcatCategoryWebHandler.SaveCollection(Collection: TCollection);
var
  Filename: String;
begin
  Filename := TDCSGlobalSettings.GetInstance.GetSettingAsString('CategoryListFile');
  if Filename <> '' then
    SaveCollectionToJSONFile(Collection, Filename);
  TfprStackClientSingleton.Instance.Client.SendMessage(smtToStack, 'CategoryMonitor', [], 0, '{"name":"signal"}');
end;

function TcatCategoryWebHandler.HandleCategoryUpdateRequest(CategoryId: string; CategoryJSON: TJSONData; AccessToken: string): TJSONData;
var
  Category: TfprPackageCategory;
  Collection: TfprPackageCategoryCollection;
  TempCategory: TfprPackageCategory;
  JSData: TJSONObject;
  i: Integer;
begin
  if not Assigned(CategoryJSON) or CategoryJSON.IsNull then
    raise EJsonWebException.CreateHelp('Missing category data.', 400);

  if CategoryJSON.JSONType <> jtObject then
    raise EJsonWebException.CreateHelp('Invalid category data', 400);

  if CategoryId='' then
    raise EJsonWebException.CreateHelp('Missing categoryid', 400);

  If TfprAuthenticationHandler.GetInstance.GetUserRole(AccessToken) <> 'admin' then
    raise EJsonWebException.CreateHelp('Not enough rights to modify package categories.', 403);

  Collection := TfprPackageCategoryCollectionSingleton.Instance;
  Category := Collection.FindObjectById(StrToIntDef(CategoryId, -1));
  if not Assigned(Category) then
    raise EJsonWebException.CreateFmtHelp('Category with id [%d] does not exist', [CategoryId], 404);

  // Create a clone of the existing package, and apply the changes to this clone
  // to do the validity-checks.
  JSData := ObjectToJSON(Category) as TJSONObject;
  try
    TempCategory := TfprPackageCategory.Create(nil);
    try
      JSONContentToObject(JSData, TempCategory);
      JSONContentToObject(TJSONObject(CategoryJSON), TempCategory);
      CheckValid(TempCategory);

      if TempCategory.CategoryId <> StrToIntDef(CategoryId, -1) then
        raise EJsonWebException.CreateFmtHelp('Categoryid [%d] of object does not match with [%s].', [TempCategory.CategoryId, CategoryId], 400);

      for i := 0 to Collection.Count -1 do
        begin
        if (Collection.Items[i].CategoryId <> TempCategory.CategoryId) and
           SameText(Collection.Items[i].Name, TempCategory.Name) then
          raise EJsonWebException.CreateFmtHelp('Another category does already exist with name [%s].', [TempCategory.Name], 422);
        end;
    finally
      TempCategory.Free;
    end;
  finally
    JSData.Free;
  end;

  JSONContentToObject(TJSONObject(CategoryJSON), Category);
  SaveCollection(Collection);

  Result := ObjectToJSON(Category);
end;

procedure TcatCategoryWebHandler.CheckValid(Category: TfprPackageCategory);
begin
  if Category.CategoryId = 0 then
    raise EJsonWebException.CreateHelp('Package category must have a categoryid.', 422);

  if Category.Name = '' then
    raise EJsonWebException.CreateFmtHelp('Package category [%d] must have a name.', [Category.CategoryId], 422);

  if (length(Category.Name) < 3) or (length(Category.Name) > 15) then
    raise EJsonWebException.CreateFmtHelp('The package category name [%s] must have between 3 and 15 characters.', [Category.Name], 422);
end;

end.

