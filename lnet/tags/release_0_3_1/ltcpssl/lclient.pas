program lclient;

{$mode objfpc}{$H+}

uses
  Classes, Crt, SysUtils, lNet, lSSL;
  
type

  { TLEvents }

  TLEvents = class
   public
    procedure DsProc(aSocket: TLSocket);
    procedure ReProc(aSocket: TLSocket);
    procedure ErProc(const msg: string; aSocket: TLSocket);
  end;
     

var
  con: TLTcp; quit: boolean;
  port: word;
  s, addr: string;
  c: char;
  event: TLEvents;

procedure TLEvents.DsProc(aSocket: TLSocket);
begin
  Writeln('Lost connection');
end;

procedure TLEvents.ReProc(aSocket: TLSocket);
var
  s: string;
begin
  aSocket.GetMessage(s);
  Writeln(s);
end;

procedure TLEvents.ErProc(const msg: string; aSocket: TLSocket);
begin
  writeln('ERROR: ', msg);
  quit:=true;
end;

procedure mainloop;

  procedure Reconnect;
  begin
    writeln('Reconnecting');
    con.Disconnect;
    if not con.Connect(addr, port) then
      begin
        quit:=true;
        writeln('Failure');
      end;
  end;

begin
  s:='';
  event:=TLEvents.Create;
  quit:=false;
  con:=TLTcp.Create(nil);
  con.OnReceive:=@event.ReProc;
  con.OnDisconnect:=@event.DsProc;
  con.OnError:=@event.ErProc;
  write('Address: ');
  readln(addr);
  write('Port: ');
  readln(port);
  if con.connect(addr, port) then begin
    Writeln('Connecting... press any key to cancel');
    repeat
      Con.CallAction;
      Sleep(1);
      if KeyPressed then
        Halt;
    until Con.Connected;
    Writeln('Connected');
    repeat
      if keypressed then
        begin
          c:=readkey;
          case c of
            'r': Reconnect;
            #8:  begin
                   if Length(s) > 1 then
                     delete(s, length(s)-1, 1)
                   else
                     s:='';
                   gotoxy(WhereX-1, WhereY);
                   write(' ');
                   gotoxy(WhereX-1, WhereY);
                 end;
            #10, #13: begin
                   con.SendMessage(s);
                   s:='';
                   Writeln;
                end;
            #27: quit:=true;
          else
            begin
              s:=s+c;
              write(c);
            end;
          end;
        end;
      con.callaction;
      delay(1);
    until quit;
  end;
  con.free;
  event.free;
end;

begin
 mainloop;
end.
