program lSMTPClient;

{$mode objfpc}{$H+}

uses
  SysUtils, Crt, lNet, lSMTP;
  
type

  { TDoer }

  { TLSMTPClientTest }

  TLSMTPClientTest = class
   private
    FSMTP: TLSMTPClient; // this is THE smtp connection
    FQuit: Boolean;  // helper for main loop
    function GetAnswer(const s: string): string;
    procedure PrintUsage(const Msg: string);
   {  these events are used to see what happens on the SMTP connection. They are used via "CallAction".
      OnReceive will get fired whenever new data is received from the SMTP server.
      OnConnect will get fired when connecting to server ended with success.
      OnDisconnect will get fired when the other side closed connection gracefully.
      OnError will get called when any kind of net error occurs on the connection.
   }
    procedure OnReceive(aSocket: TLSocket);
    procedure OnConnect(aSocket: TLSocket);
    procedure OnDisconnect(aSocket: TLSocket);
    procedure OnError(const msg: string; aSocket: TLSocket);
   public
    constructor Create;
    destructor Destroy; override;
    procedure Run; // main loop 
  end;

{ TLSMTPClientTest }

function TLSMTPClientTest.GetAnswer(const s: string): string;
var
  c: Char;
begin
  Result := '';
  Write(s, ': ');
  while True do begin
    FSMTP.CallAction;
    if KeyPressed then begin
      c := ReadKey;
      case c of
        #13, #27 : begin
                     Writeln;
                     Exit;
                   end;
        #8       : if Length(Result) > 0 then begin
                     SetLength(Result, Length(Result)-1);
                     GotoXY(WhereX-1, WhereY);
                     Write(' ');
                     GotoXY(WhereX-1, WhereY);
                   end;
        else begin
          Result := Result + c;
          Write(c);
        end;
      end;
    end;
  end;
end;

procedure TLSMTPClientTest.PrintUsage(const Msg: string);
begin
  Writeln('Usage: ', ExtractFileName(ParamStr(0)), ' ', Msg);
end;

procedure TLSMTPClientTest.OnReceive(aSocket: TLSocket);
var
  s: string;
begin
  if FSMTP.GetMessage(s) > 0 then // if we actually received something from SMTP server, write it for the user
    Write(s);
end;

procedure TLSMTPClientTest.OnConnect(aSocket: TLSocket);
begin
  Writeln('Connected'); // inform user of successful connect
end;

procedure TLSMTPClientTest.OnDisconnect(aSocket: TLSocket);
begin
  Writeln('Lost connection'); // inform user about lost connection
  FQuit := True; // since SMTP shouldn't do this unless we issued a QUIT, consider it to be end of session and quit program
end;

procedure TLSMTPClientTest.OnError(const msg: string; aSocket: TLSocket);
begin
  Writeln(msg); // inform about error
end;

constructor TLSMTPClientTest.Create;
begin
  FQuit := False;
  FSMTP := TLSMTPClient.Create(nil);
  FSMTP.Timeout := 100; // responsive enough, but won't hog CPU
  FSMTP.OnReceive := @OnReceive; // assign all events
  FSMTP.OnConnect := @OnConnect;
  FSMTP.OnDisconnect := @OnDisconnect;
  FSMTP.OnError := @OnError;
end;

destructor TLSMTPClientTest.Destroy;
begin
  FSMTP.Free;
  inherited Destroy;
end;

procedure TLSMTPClientTest.Run;
const
  MAX_RECIPIENTS = 10;
var
  Addr, Subject, Sender, Recipients, Message: string;
  Port: Word = 25;
begin
  if ParamCount > 0 then begin
    Addr := ParamStr(1); // get address and port from commandline args
    if ParamCount > 1 then
      Port := Word(StrToInt(ParamStr(2)));

    Write('Connecting to ', Addr, '... '); 
    if FSMTP.Connect(Addr, Port) then repeat  // try to connect 
      FSMTP.CallAction;  // if inital connect went ok, wait for "acknowlidgment" or otherwise
      if KeyPressed then
        if ReadKey = #27 then
          FQuit := True;  // if user doesn't wish to wait, quit
      Sleep(1);
    until FQuit or FSMTP.Connected; // if user quit, or we connected, then continue

    if not FQuit then begin // if we connected send HELO
      FSMTP.Helo;
      Writeln('Press escape to quit or any other key to compose an email');
    end;

    while not FQuit do begin // if we connected, do main loop
      SetLength(Recipients, MAX_RECIPIENTS);
      FSMTP.CallAction; // main event mechanism, make sure to call periodicly and ASAP, or specify high timeout
      if KeyPressed then
        if ReadKey = #27 then begin // if user wants to quit
          if FSMTP.Connected then // and we're connected, then do a graceful QUIT, waiting for server to disconnect
            FSMTP.Quit
          else
            FQuit := True; // otherwise just quit from this end
        end else begin // otherwise, user wants to compose email
          Sender := GetAnswer('From'); // get info about email from console input
          Recipients := GetAnswer('Recipients');
          Subject := GetAnswer('Subject');
          Message := GetAnswer('Data');

          FSMTP.SendMail(Sender, Recipients, Subject, Message); // send the mail given user data
        end;
      Sleep(1);
    end;
  end else PrintUsage('<SMTP server hostname/IP> [port]');
end;

var
  SMTP: TLSMTPClientTest;
begin
  SMTP := TLSMTPClientTest.Create;
  SMTP.Run;
  SMTP.Free;
end.
