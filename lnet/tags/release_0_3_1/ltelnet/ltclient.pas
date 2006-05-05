program ltclient;

{$mode objfpc}{$H+}

uses
  SysUtils, Crt, lTelnet, lNet;
  
{ This is a rather simple Telnet client,
  it accepts IP/url and port as arguments
  See file ltelnet.pas if you want to know
  how it works. }
  
type

  { TDoer }

  TDoer = class
   public
    procedure OnError(const msg: string; aSocket: TLSocket);
  end;

var
  Con: TLTelnetClient;
  Doer: TDoer;

procedure QuitProgram;
begin
  Con.Free;
  Doer.Free;
  Halt;
end;

{ TDoer }

procedure TDoer.OnError(const msg: string; aSocket: TLSocket);
begin
  Writeln(msg);
  QuitProgram;
end;
  
procedure Main;
var
  Quit: Boolean;
  s: string;
  c: Char;
  l: Longint; // length of line currently written
  AD: string;
  PORT: Word;
begin
  if ParamCount > 1 then
    try
      AD:=Paramstr(1);
      PORT:=Word(StrToInt(Paramstr(2)));
    except
      Writeln('Invalid command line parameters');
      Halt;
    end else begin
      Writeln('Usage: ', ExtractFileName(ParamStr(0)), ' IP PORT');
      Halt;
    end;
  Con:=TLTelnetClient.Create(nil);
  Doer:=TDoer.Create;
  Con.OnError:=@Doer.OnError;
  Quit:=False;
  l:=0;
  if Con.Connect(AD, PORT) then begin
    Writeln('Connecting... press any key to cancel');
    repeat
      Con.CallAction;
      Sleep(1);
      if KeyPressed then
        Halt;
    until Con.Connected;
    while not Quit do begin
      if KeyPressed then begin
        c:=ReadKey;
        case c of
          #27: Quit:=True;
           #8: if l > 0 then
                 begin
                   GotoXY(WhereX-1, WhereY);
                   Write(' ');
                   Dec(l);
                   Con.SendMessage(c);
                 end;
        else begin
               Inc(l);
               if c = #13 then begin
                 Writeln;
               l:=0;
               end;
               Con.SendMessage(c);
             end;
        end;
      end;
      if Con.GetMessage(s) > 0 then
        Write(s);
      Con.CallAction;
      Delay(1);
    end;
  end;
  if Con.GetMessage(s) > 0 then
    Write(s);
  QuitProgram;
end;

begin
  Main;
end.

