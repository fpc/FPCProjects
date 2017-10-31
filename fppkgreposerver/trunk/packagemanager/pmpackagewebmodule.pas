unit pmPackageWebModule;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  Classes,
  httpdefs,
  fpjson,
  fpHTTP,
  fpWeb,
  fphttpserver,
  cnocOpenIDConnect,
  cnocOIDCIDToken,
  dcsGlobalSettings,
  pmErrorHandling,
  pmPackage,
  pmPackageJSonStreaming;

type

  { TpmPackageWM }

  TpmPackageWM = class(TFPWebModule)
    Procedure DataModuleBeforeRequest(Sender: TObject; ARequest: TRequest);
    Procedure DataModuleRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse; Var Handled: Boolean);
  private
    FPackageStreamer: TpmPackageJSonStreaming;
    Procedure HandlePostRequest(ARequest: TRequest; AResponse: TResponse; Var Handled: Boolean; SubjectId: string);
    Procedure HandleGetRequest(ARequest: TRequest; AResponse: TResponse; Var Handled: Boolean);

  public
    constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;

  end;

var
  pmPackageWM: TpmPackageWM;

implementation

{$R *.lfm}

{ TpmPackageWM }

Procedure TpmPackageWM.DataModuleBeforeRequest(Sender: TObject; ARequest: TRequest);
begin
  //
end;

Procedure TpmPackageWM.DataModuleRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse; Var Handled: Boolean);
var
  OIDC: TcnocOpenIDConnect;
  AuthorizationToken, s: string;
  AllowRequest: Boolean;
  JWT: TcnocOIDCIDTokenJWT;
  SubjectId: string;
begin
  OIDC := TcnocOpenIDConnect.Create;
  try
    OIDC.OpenIDProvider := TDCSGlobalSettings.GetInstance.GetSettingAsString('OpenIDProviderURL');

    AuthorizationToken := ARequest.Authorization;

    if copy(AuthorizationToken, 1, 7) = 'Bearer ' then
      begin
      s := copy(AuthorizationToken, 8, length(AuthorizationToken)-7);
      AllowRequest := OIDC.VerifyJWT(s);
      if not AllowRequest then
        s := OIDC.GetLatestError;

      JWT := TcnocOIDCIDTokenJWT.Create;
      try
        JWT.AsEncodedString := s;
        SubjectId := JWT.Claims.sub;
      finally
        JWT.Free;
      end;

      end
    else
      begin
      AllowRequest := False;
      s := 'No authorization information';
      end;
  finally
    OIDC.Free;
  end;

  if not AllowRequest then
    begin
    AResponse.Code := 401;
    AResponse.CodeText := GetStatusCode(AResponse.Code);
    AResponse.Content := Format('Authentication failed (%s).', [s]);
    Handled := True;
    Exit;
    end;

  if ARequest.Method = 'POST' then
    HandlePostRequest(ARequest, AResponse, Handled, SubjectId)
  else if ARequest.Method = 'GET' then
    HandleGetRequest(ARequest, AResponse, Handled)
end;

Procedure TpmPackageWM.HandlePostRequest(ARequest: TRequest; AResponse: TResponse; Var Handled: Boolean; SubjectId: string);
var
  Package: TpmPackage;
  ErrStr: string;
begin
  Package := TpmPackage.Create;
  try
    FPackageStreamer.JSonToPackage(ARequest.Content, Package);
    Package.OwnerId := SubjectId;
    Package.PackageState := pmpsInitial;

    if not Package.Validate(ErrStr) then
      Raise EJsonWebException.Create(ErrStr);

    if Assigned(TpmPackageList.Instance.FindPackageByName(Package.Name)) then
      Raise EJsonWebException.CreateFmt('Package with the name %s does already exist', [Package.Name]);

    AResponse.Content := FPackageStreamer.PackageToJSon(Package);
    AResponse.Code := 200;
    AResponse.CodeText := GetStatusCode(AResponse.Code);

    TpmPackageList.Instance.Add(Package);
    Handled := True;
    Package := Nil;
  finally
    Package.Free;
  end;
end;

Procedure TpmPackageWM.HandleGetRequest(ARequest: TRequest; AResponse: TResponse; Var Handled: Boolean);
begin
  AResponse.Content := FPackageStreamer.PackageListToJSon(TpmPackageList.Instance);
  AResponse.Code := 200;
  AResponse.CodeText := GetStatusCode(AResponse.Code);
  Handled := True;
end;

constructor TpmPackageWM.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPackageStreamer := TpmPackageJSonStreaming.Create;
end;

Destructor TpmPackageWM.Destroy;
begin
  FPackageStreamer.Free;
  inherited Destroy;
end;

initialization
  RegisterHTTPModule('package', TpmPackageWM);
end.

