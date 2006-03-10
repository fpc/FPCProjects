program lSMTPClient;

{$mode objfpc}{$H+}

uses
  SysUtils, Crt, lNet, lSMTP;
  
type

  { TDoer }

  TDoer = class
   public
    procedure OnReceive(Sender: TLSMTPClient);
    procedure OnConnect(Sender: TLSMTPClient);
    procedure OnDisconnect(Sender: TLSMTPClient);
    procedure OnError(const msg: string; Sender: TLSocket);
  end;
  
var
  SMTP: TLSMTPClient;
  Quit: Boolean;

{ TDoer }

procedure TDoer.OnReceive(Sender: TLSMTPClient);
var
  s: string;
begin
  Sender.GetMessage(s);
  Write(s);
end;

procedure TDoer.OnConnect(Sender: TLSMTPClient);
begin
  Writeln('Connected');
end;

procedure TDoer.OnDisconnect(Sender: TLSMTPClient);
begin
  Writeln('Lost connection');
end;

procedure TDoer.OnError(const msg: string; Sender: TLSocket);
begin
  Writeln(msg);
end;

procedure PrintUsage(const Msg: string);
begin
  Writeln('Usage: ', ExtractFileName(ParamStr(0)), ' ', Msg);
end;

function GetAnswer(const s: string): string;
var
  c: Char;
begin
  Result:='';
  Write(s, ': ');
  while True do begin
    SMTP.CallAction;
    if KeyPressed then begin
      c:=ReadKey;
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
          Result:=Result + c;
          Write(c);
        end;
      end;
    end;
    Sleep(1);
  end;
end;

procedure Main(const Host: string; const Port: Word = 25);
const
  MAX_RECIPIENTS = 10;
var
  Doer: TDoer;
  Subject, Sender, Recipients, Message: string;
begin
  Quit:=False;
  Doer:=TDoer.Create;
  SMTP:=TLSMTPClient.Create(nil);
  SMTP.OnReceive:=@Doer.OnReceive;
  SMTP.OnConnect:=@Doer.OnConnect;
  SMTP.OnDisconnect:=@Doer.OnDisconnect;
  SMTP.OnError:=@Doer.OnError;
  
  if SMTP.Connect(Host, Port) then repeat
    SMTP.CallAction;
    if KeyPressed then
      Quit:=True;
    Sleep(1);
  until Quit or SMTP.Connected;
  
  if not Quit then
    SMTP.Helo;
    
  if not Quit then
    Writeln('Press escape to quit or any other key to compose an email');
  
  while not Quit do begin
    SetLength(Recipients, MAX_RECIPIENTS);
    SMTP.CallAction;
    if KeyPressed then
      if ReadKey = #27 then
        Quit:=True
      else begin
        Sender:=GetAnswer('From');
        Recipients:=GetAnswer('Recipients');
        Subject:=GetAnswer('Subject');
        Message:=GetAnswer('Data');
        
        SMTP.SendMail(Sender, Recipients, Subject, Message);
      end;
    Sleep(1);
  end;
  
  SMTP.Free;
  Doer.Free;
end;

begin
  if ParamCount > 0 then begin
    if ParamCount > 1 then
      Main(ParamStr(1), StrToInt(ParamStr(2)))
    else
      Main(ParamStr(1));
  end else PrintUsage('<SMTP server hostname/IP> [port]');
end.
