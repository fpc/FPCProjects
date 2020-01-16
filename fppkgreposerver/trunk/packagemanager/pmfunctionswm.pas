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
  TfprPackageCollection.Instance.Clear;
  AResponse.Content := '{Message: ''Cleared all packages''}';
  AResponse.Code := 200;
  AResponse.CodeText := GetStatusCode(AResponse.Code);
  Handled := True;
end;

initialization
  RegisterHTTPModule('functions', TpmFunctionsWM);
end.

