unit prFunctionsWebModule;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  Classes,
  httpdefs,
  fpHTTP,
  fphttpserver,
  FileUtil,
  dcsGlobalSettings,
  fprWebModule;

type

  { TprFunctionsWM }

  TprFunctionsWM = class(TfprWebModule)
    Procedure ClearRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse; Var Handled: Boolean);
  private

  public

  end;

implementation

{$R *.lfm}

{ TprFunctionsWM }

Procedure TprFunctionsWM.ClearRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse; Var Handled: Boolean);
var
  GITRepositoriesPath: String;
begin
  GITRepositoriesPath := TDCSGlobalSettings.GetInstance.GetSettingAsString('GITRepositoriesPath');
  if DeleteDirectory(GITRepositoriesPath, True) then
    begin
    AResponse.Content := '{Message: ''Cleared all package-repositories''}';
    AResponse.Code := 200;
    AResponse.CodeText := GetStatusCode(AResponse.Code);
    Handled := True;
    end
  else
    begin
    Exception.Create('Failed to clear package-repositories');
    end;
end;

initialization
  RegisterHTTPModule('functions', TprFunctionsWM);
end.

