unit idIdentityWebmodule;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  Classes,
  httpdefs,
  fpHTTP,
  fpjson,
  URIParser,
  fprWebModule,
  fpWeb,
  dcsGlobalSettings,
  cnocOpenIDConnectProvider;

type

  { TidAuthenicator }

  TidAuthenicator = class(IcnocOpenIDConnectLoginCheck, IcnocOpenIDConnectUsedEndpointJSON)
    procedure IsValidLogin(const Provider: TcnocOpenIDConnectProvider; const AUserName, APassword: string; const ARequest: TRequest; var Handled: Boolean; var Subject: string; out FailureMessage: string);
    function ObtainUserInfoEndpoint(const Provider: TcnocOpenIDConnectProvider; const Subject: string): TJSONObject;
  end;

  { TidIdentityWM }

  TidIdentityWM = class(TfprWebModule)
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    Procedure DataModuleRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse; Var Handled: Boolean);
  private
    FProvider: TcnocOpenIDConnectProvider;
    FAuthenticator: TidAuthenicator;
  protected
    function RequireAuthentication(ARequest: TRequest): Boolean; override;
  end;

var
  FidIdentityWM: TidIdentityWM;

implementation

{$R *.lfm}

{ TidAuthenicator }

procedure TidAuthenicator.IsValidLogin(const Provider: TcnocOpenIDConnectProvider; const AUserName,
  APassword: string; const ARequest: TRequest; var Handled: Boolean; var Subject: string; out
  FailureMessage: string);
begin
  FailureMessage := '';
  if Handled then
    Exit;
  Handled := True;

  if AUserName='' then
    FailureMessage := 'Please enter a username'
  else if APassword='' then
    FailureMessage := 'Please enter a password'
  else if (ARequest.ContentFields.Values['Environment']='Mantis') and (AUserName='joost') and (APassword='jachtluipaard') then
    Subject := '2'
  else
    FailureMessage := 'Invalid username or password';
end;

function TidAuthenicator.ObtainUserInfoEndpoint(const Provider: TcnocOpenIDConnectProvider;
  const Subject: string): TJSONObject;
var
  UserJSON: TJSONObject;
begin
  UserJSON := TJSONObject.Create;
  try
    UserJSON.Add('sub', Subject);
    UserJSON.Add('name', 'Joost van der Sluis');
    UserJSON.Add('role', 'admin');

    Result := UserJSON;
    UserJSON := Nil;
  finally
    UserJSON.Free;
  end;
end;

{ TidIdentityWM }

procedure TidIdentityWM.DataModuleCreate(Sender: TObject);
var
  GlobalSettings: TDCSGlobalSettings;
  URI: TURI;
  s: string;
  arr: TStringArray;
  i: Integer;
begin
  GlobalSettings := TDCSGlobalSettings.GetInstance;

  s := GlobalSettings.GetSettingAsString('BaseURL');
  URI := ParseURI(s);
  FProvider := TcnocOpenIDConnectProvider.Create(s);
  FProvider.GenerateNewKey(2048, 65537);
  FProvider.Protocol := URI.Protocol;

  FProvider.LoginTemplate.FileName := 'logintemplate.html';
  FProvider.LogoutTemplate.FileName := 'logouttemplate.html';

  FAuthenticator := TidAuthenicator.Create;
  FProvider.AddContentProvider(FAuthenticator);

  s := GlobalSettings.GetSettingAsString('AllowCorsOrigin');
  if s <> '' then
    begin
    arr := s.Split(';');
    for    i := 0 to length(arr) -1 do
      AddCorsOrigin(arr[i], 'POST, GET, PUT', '', True);
    end;
end;

procedure TidIdentityWM.DataModuleDestroy(Sender: TObject);
begin
  FProvider.Free;
  FAuthenticator.Free;
end;

Procedure TidIdentityWM.DataModuleRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse; Var Handled: Boolean);
begin
  ARequest.ReturnedPathInfo := '';
  FProvider.HandleRequest(Sender, ARequest, AResponse, Handled);
end;

function TidIdentityWM.RequireAuthentication(ARequest: TRequest): Boolean;
begin
  Result := False;
end;

initialization
  RegisterHTTPModule('identityserver', TidIdentityWM);
end.

