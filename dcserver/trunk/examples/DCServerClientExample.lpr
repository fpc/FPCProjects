program DCServerClientExample;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  SysUtils,
  CustApp,
  DCSClientThread,
  dcsHandler,
  fpjson,
  jsonparser;

type

  { TDCServerClientExample }

  TDCServerClientExample = class(TCustomApplication)
  protected
    FClientThread: TDCSClientThread;
    FMessage: string;
    FCommandReceived: Boolean;
    FDontWaitForCommandCompletion: Boolean;
    procedure DoRun; override;
    procedure HandleLostConnection(ErrMessage: string);
    procedure HandleReceiveData(Data: TJSONData);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TDCServerClientExample }

procedure TDCServerClientExample.DoRun;
var
  ErrorMsg: string;
  Server: string;
  Port: Integer;
  Command: string;
  JSONData: TJSONData;
begin
  // quick check parameters
  ErrorMsg := CheckOptions('hs:p:c:d', ['help','server:','port:','command:','dontwait']);
  if ErrorMsg <> '' then
    begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
    end;

  // parse parameters
  if HasOption('h', 'help') then
    begin
    WriteHelp;
    Terminate;
    Exit;
    end;

  if HasOption('s','server') then
    Server := GetOptionValue('s','server')
  else
    Server := 'localhost';

  if HasOption('p','port') then
    Port := StrToInt(GetOptionValue('p','port'))
  else
    Port := 9250;

  if HasOption('c','command') then
    Command := GetOptionValue('c','command')
  else
    begin
    WriteHelp;
    Terminate;
    Exit;
    end;

  if HasOption('d','dontwait') then
    FDontWaitForCommandCompletion := True;

  JSONData := GetJSON(Command);
  if Assigned(JSONData) then
    begin
    FClientThread := TDCSClientThread.Create(Server, Port, @HandleReceiveData, @HandleLostConnection);
    try
      FClientThread.SendData(JSONData);
      while not terminated do
        CheckSynchronize(100);
    finally
      FClientThread.Terminate;
      FClientThread.WaitFor;
      FClientThread.Free;
    end;
    end
  else
    begin
      FMessage := 'Invalid command: '+Command;
    end;

  if FMessage<>'' then
    writeln(FMessage);

  // stop program loop
  Terminate;
end;

procedure TDCServerClientExample.HandleLostConnection(ErrMessage: string);
begin
  if (ErrMessage<>'') then
    begin
    FMessage := ErrMessage;
    end;
  Terminate;
end;

procedure TDCServerClientExample.HandleReceiveData(Data: TJSONData);
var
  JSObj: TJSONObject;
  NotificationType: string;
begin
  if Data.JSONType=jtObject then
    begin
    JSObj := Data as TJSONObject;
    if (JSObj.Get('type','')='Notification') and (JSObj.Get('lisId',-1)=FClientThread.ConnectionIdentifier) then
      begin
      NotificationType := JSObj.Get('notificationType','');
      if not FCommandReceived then
        begin
        if NotificationType='InvalidCommand' then
          begin
          FMessage := 'Invalid command: '+Data.AsJSON;
          FClientThread.Terminate;
          end
        else if NotificationType='ReceivedCommand' then
          begin
          FCommandReceived := True;
          if FDontWaitForCommandCompletion then
            begin
            FMessage := 'Command queued: '+Data.AsJSON;
            FClientThread.Terminate;
            end;
          end;
        end
      else if (NotificationType='ExecutedCommand') then
        begin
        FMessage := 'Command executed: '+Data.AsJSON;
        FClientThread.Terminate;
        end
      else if (NotificationType='FailedCommand') then
        begin
        FMessage := 'Command failed: '+Data.AsJSON;
        FClientThread.Terminate;
        end;
      end;
    end;
end;

constructor TDCServerClientExample.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException := True;
end;

destructor TDCServerClientExample.Destroy;
begin
  inherited Destroy;
end;

procedure TDCServerClientExample.WriteHelp;
begin
  writeln('Usage: ', ExeName, ' -h -s <server> -p <port> -c <commandstring> -d');
  writeln('Sends the json-command to the server and waits for the result.');
  writeln('The command-string is obligatory. ');
end;

var
  Application: TDCServerClientExample;
begin
  Application := TDCServerClientExample.Create(nil);
  Application.Title := 'DCServer client example';
  Application.Run;
  Application.Free;
end.

