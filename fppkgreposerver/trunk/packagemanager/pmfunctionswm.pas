unit pmFunctionsWM;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  Classes,
  httpdefs,
  fpHTTP,
  fphttpserver,
  fprWebModule,
  fprInterfaceClasses,
  fprAuthenticationHandler,
  pmPackage;

type

  { TpmFunctionsWM }

  TpmFunctionsWM = class(TfprWebModule)
    Procedure ClearRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse; Var Handled: Boolean);
  private

  public

  end;

implementation

{$R *.lfm}

{ TpmFunctionsWM }

Procedure TpmFunctionsWM.ClearRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse; Var Handled: Boolean);
begin
  if TfprAuthenticationHandler.GetInstance.GetUserRole(FAccessToken) <> 'admin' then
    raise EHTTP.CreateHelp('You need admin rights to be able to clear the packages-data', 403);

  TfprPackageCollection.Instance.Clear;
  AResponse.Content := '{Message: ''Cleared all packages''}';
  AResponse.Code := 200;
  AResponse.CodeText := GetStatusCode(AResponse.Code);
  Handled := True;
end;

initialization
  RegisterHTTPModule('functions', TpmFunctionsWM);
end.

