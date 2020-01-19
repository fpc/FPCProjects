unit idIdentityWebmodule;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  Classes,
  StrUtils,
  httpdefs,
  fphttpserver,
  fpHTTP,
  fphttpclient,
  fpjson,
  TLevelUnit,
  URIParser,
  fprWebModule,
  fprLog,
  dcsGlobalSettings,
  cnocOpenIDConnectProvider;

type

  { TidAuthenicator }

  TidAuthenicator = class(IcnocOpenIDConnectLoginCheck, IcnocOpenIDConnectUsedEndpointJSON)
  private
    procedure IsValidMantisLogin(const Provider: TcnocOpenIDConnectProvider; const AUserName, APassword: string; var Subject: string; out FailureMessage: string);
  public
    procedure IsValidLogin(const Provider: TcnocOpenIDConnectProvider; const AUserName, APassword: string; const ARequest: TRequest; var Handled: Boolean; var Subject: string; out FailureMessage: string);
    function ObtainUserInfoEndpoint(const Provider: TcnocOpenIDConnectProvider; const Subject: string): TJSONObject;
  end;

  { TidIdentityWM }

  TidIdentityWM = class(TfprWebModule)
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    Procedure DataModuleRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse; Var Handled: Boolean);
    procedure Default404Request(Sender: TObject; ARequest: TRequest; AResponse: TResponse; var Handled: Boolean);
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
  else if (ARequest.ContentFields.Values['Environment']='Mantis') then
    IsValidMantisLogin(Provider, AUserName, APassword, Subject, FailureMessage)
  else
    FailureMessage := 'Invalid username or password';

  if Subject<>'' then
    TfprLog.Log(Format('User with subject [%s] logged in.', [Subject]), nil, TLevelUnit.INFO);
end;

function TidAuthenicator.ObtainUserInfoEndpoint(const Provider: TcnocOpenIDConnectProvider;
  const Subject: string): TJSONObject;
var
  AdminSubjects: array of string;
  UserJSON: TJSONObject;
  GlobalSettings: TDCSGlobalSettings;
  s: String;
begin
  GlobalSettings := TDCSGlobalSettings.GetInstance;
  s := GlobalSettings.GetSettingAsString('AdminSubjects');
  AdminSubjects := s.Split([',',';']);

  UserJSON := TJSONObject.Create;
  try
    UserJSON.Add('sub', Subject);
    UserJSON.Add('name', RightStr(Subject, 3));
    if Subject in AdminSubjects then
      UserJSON.Add('role', 'admin');

    Result := UserJSON;
    UserJSON := Nil;
  finally
    UserJSON.Free;
  end;
end;

procedure TidAuthenicator.IsValidMantisLogin(const Provider: TcnocOpenIDConnectProvider; const AUserName, APassword: string; var Subject: string; out FailureMessage: string);
var
  HTTPClient: TFPHTTPClient;
  RequestData, Location: string;
  Resp: RawByteString;
begin
  HTTPClient := TFPHTTPClient.Create(nil);
  try
    RequestData := Format('return=my_view_page.php&username=%s&password=%s&secure_session=on', [AUserName, APassword]);
    Resp := HTTPClient.FormPost('https://bugs.freepascal.org/login.php', RequestData);
    if (Resp='') and (HTTPClient.ResponseStatusCode = 302) then
      begin
      Location := Trim(HTTPClient.ResponseHeaders.Values['Location']);
      if StartsText('https://bugs.freepascal.org/login_cookie_test.php', Location) then
        begin
        Subject := 'M_'+AUserName;
        end
      else if StartsText('https://bugs.freepascal.org/login_page.php', Location) then
        begin
        if pos('error=1', Location) > 0 then
          FailureMessage := 'Login failed, invalid username or password'
        else
          begin
          FailureMessage := 'Login faied, unexpected failure code';
          TfprLog.Log(Format('Received unexpected error from Mantis. Redirect-url: [%s]', [Location]), nil, TLevelUnit.WARN);
          end;
        end
      else
        begin
        FailureMessage := 'Login failed, unexpected failure';
        TfprLog.Log(Format('Received unexpected redirect-url from Mantis: [%s]', [Location]), nil, TLevelUnit.WARN);
        end;
      end
    else
      begin
      FailureMessage := 'Login failed. Received an unknown response from Mantis';
      TfprLog.Log(Format('Received unexpected response from Mantis. Response: [%s], StatusCode: [%d]', [Resp, HTTPClient.ResponseStatusCode]), nil, TLevelUnit.WARN);
      end;
  finally
    HTTPClient.Free;
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

procedure TidIdentityWM.Default404Request(Sender: TObject; ARequest: TRequest; AResponse: TResponse; var Handled: Boolean);
begin
  AResponse.Code := 404;
  AResponse.CodeText := GetStatusCode(AResponse.Code);
  Handled := True;
end;

initialization
  RegisterHTTPModule('identityserver', TidIdentityWM);
end.

