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
  fpjson,
  jsonparser;

type

  { TDCServerClientExample }

  TDCServerClientExample = class(TCustomApplication)
  protected
    FClientThread: TDCSClientThread;
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
  s:string;
begin
  // quick check parameters
  ErrorMsg := CheckOptions('hs:p:c:', ['help','server:','port:','command:']);
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

  FClientThread := TDCSClientThread.Create(Server, Port, @HandleReceiveData, @HandleLostConnection);
  try
    JSONData := GetJSON(Command);
    s := JSONData.AsJSON;
    FClientThread.SendData(JSONData);
    while not terminated do
      CheckSynchronize(100);
  finally
    FClientThread.Free;
  end;


  // stop program loop
  Terminate;
end;

procedure TDCServerClientExample.HandleLostConnection(ErrMessage: string);
begin
  if (ErrMessage<>'') then
    begin
    WriteLn(ErrMessage);
    Terminate;
    end;
end;

procedure TDCServerClientExample.HandleReceiveData(Data: TJSONData);
begin
  writeln('Data: '+data.AsJSON);
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
  writeln('Usage: ', ExeName, ' -h -s <server> -p <port> -c <commandstring>');
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

