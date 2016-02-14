program fppkgfront;

{$mode objfpc}{$H+}

uses
  cthreads, sysutils, custapp, custcgi, fpCGI, wmfppkg,iniwebsession, authmantis, eventlog, cfgpkgfront;

Type

  { TMyCGIRequest }

  TMyCGIRequest = Class(TCGIRequest)
  Protected
    Function RequestUploadDir: String; override;
  end;

{ TMyCGIRequest }

Function TMyCGIRequest.RequestUploadDir: String;
begin
  Result:=TSettings.Global.UploadDir;
end;

Procedure DoConfig;

Var
  FN : String;

begin
  FN:=GetAppConfigFile(True,False);
  Application.EventLogFilter:=[etCustom,etInfo,etWarning,etError,etDebug];
  Application.EventLog.Identification:='fppkgfront';
  Application.EventLog.Paused:=False;
  Application.Log(etInfo,'Config file = '+FN);
  TSettings.GLobal.LoadFromFile(FN);
  MantisAuthentication.ConfigMantis(FN);
  CGIRequestClass:=TMyCGIRequest;
end;

begin
  DoConfig;
  Application.AllowDefaultModule:=True;
  Application.Initialize;
  Application.Run;
end.

