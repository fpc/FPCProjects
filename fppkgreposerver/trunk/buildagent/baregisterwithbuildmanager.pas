unit baRegisterWithBuildManager;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DateUtils,
  fphttpclient,
  dcsGlobalSettings,
  dcsHandler;

type

  { TbaRegisterWithBuildManagerCommand }

  TbaRegisterWithBuildManagerCommand = class(TDCSThreadCommand)
  public
    class function TextName: string; override;
    function DoExecute(AController: TDCSCustomController; out ReturnMessage: string): Boolean; override;
  end;


implementation

{ TbaRegisterWithBuildManagerCommand }

class function TbaRegisterWithBuildManagerCommand.TextName: string;
begin
  Result := 'RegisterWithBuildManager';
end;

function TbaRegisterWithBuildManagerCommand.DoExecute(AController: TDCSCustomController; out ReturnMessage: string): Boolean;
var
  BuildManagerURL: string;
  AgentName: string;
  AgentURL: string;
  FPCVersion: string;
  HttpClient: TFPHTTPClient;
  SectionList: TStringList;
  i: Integer;
  SS: TStringStream;
  RegistrationSucceeded: Boolean;
begin
  Result := False;
  RegistrationSucceeded := False;
  BuildManagerURL := TDCSGlobalSettings.GetInstance.GetSettingAsString('buildmanagerurl');
  AgentName := TDCSGlobalSettings.GetInstance.GetSettingAsString('AgentName');
  AgentURL := TDCSGlobalSettings.GetInstance.GetSettingAsString('AgentURL');
  // ToDo: Filter list of FPC-versions from configured environments, and use
  // them to register
  FPCVersion := TDCSGlobalSettings.GetInstance.GetSettingAsString('AgentFPCVersion');

  if BuildManagerURL = '' then
    ReturnMessage := 'No buildmanager configured'
  else if AgentName = '' then
    ReturnMessage := 'No agentname configured'
  else if AgentURL = '' then
    ReturnMessage := 'No agent-url configured'
  else if FPCVersion = '' then
    ReturnMessage := 'No FPC-version configured'
  else
    begin
    SectionList := TStringList.Create;
    try
      try
        TDCSGlobalSettings.GetInstance.FillSectionList(SectionList);
        for i := 0 to SectionList.Count -1 do
          begin
          if SameText(copy(SectionList[i], 1, 7), 'TestEnv') then
            begin
            HttpClient := TFPHTTPClient.Create(nil);
            try
              SS := TStringStream.Create(Format('{"name":"%s","url":"%s","fpcversion":"%s"}', [AgentName+'-'+copy(SectionList[i],9), AgentURL, FPCVersion]));
              try
                HttpClient.RequestBody := SS;
                ReturnMessage := HttpClient.Get(BuildManagerURL+'/agent/register');
                RegistrationSucceeded := True;
              finally
                SS.Free;
              end;
            finally
              HttpClient.Free;
            end;
            end;
          end;
      finally
        SectionList.Free;
      end;
    finally
      if RegistrationSucceeded then
        FDistributor.TimeQueueCommand(TbaRegisterWithBuildManagerCommand.Create(-1, null, FDistributor), IncSecond(Now, 300))
      else
        FDistributor.TimeQueueCommand(TbaRegisterWithBuildManagerCommand.Create(-1, null, FDistributor), IncSecond(Now, 10));
    end;
    ReturnMessage := 'Registered with BuildManager';
    Result := True;
    end
end;

end.

