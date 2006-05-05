program ludp;

{$mode objfpc}{$H+}

uses
  SysUtils, lNet, Crt;
  
type
  TLEvents = class
   public
    procedure OnEr(const msg: string; aSocket: TLSocket);
    procedure OnRe(aSocket: TLSocket);
  end;
     
var
  Con: TLUDP;
  s: string;
  Server: Boolean = False;
  Quit: boolean;
  Event: TLEvents;

procedure TLEvents.OnEr(const msg: string; aSocket: TLSocket);
begin
  Writeln(msg);
  quit:=true;
end;

procedure TLEvents.OnRe(aSocket: TLSocket);
var
  s: string;
begin
  if aSocket.GetMessage(s) > 0 then begin
    Writeln(s);
    Writeln('Host at: ', aSocket.PeerAddress);
    if Server then
      Con.SendMessage('Welcome');
  end;
end;

var
  n: Boolean;
  Port: Word;
  Address: string;
begin
  if ParamCount > 0 then begin

    n:=False;
    s:='';
    Quit:=false;

    if ParamCount < 1 then begin
      Writeln('Usage: ludp <port> [address]');
      Halt;
    end;
    
    try
      Port:=Word(StrToInt(ParamStr(1)));
    except
      on e: Exception do begin
        Writeln(e.message);
        Halt;
      end;
    end;

    if ParamCount > 1 then
      Address:=ParamStr(2)
    else
      Address:=LADDR_BR;

    Event:=TLEvents.Create;
    Con:=TLUDP.Create(nil);
    Con.OnError:=@event.OnEr;
    Con.OnDisconnect:=@event.OnRe;
    Con.OnReceive:=@event.OnRe;

    Writeln('Server?');
    Readln(s);
    if s='y' then begin
      n:=Con.Listen(port);
      Server:=True;
    end else n:=Con.Connect(Address, port);

    if n then repeat
      Con.CallAction;
      if KeyPressed then
        if ReadKey<>#27 then
          Con.SendMessage('Hello')
        else
          Quit:=true;
      Delay(20);
    until Quit;

    Writeln;
    Con.free;
    Event.free;
  end else Writeln('Usage: ', ParamStr(0), ' <port> [address]');
end.

