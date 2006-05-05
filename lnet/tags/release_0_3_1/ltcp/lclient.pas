program lclient;

{$mode objfpc}{$H+}

uses
  Classes, Crt, SysUtils, lnet;
  
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
  s: string;
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

procedure MainLoop(const Address: string; const Port: Word);

  procedure Reconnect;
  begin
    writeln('Reconnecting');
    con.Disconnect;
    if not con.Connect(Address, Port) then
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
  if Con.Connect(Address, Port) then begin
    Write('Connecting... ');
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

var
  p: Word;
begin
  if ParamCount > 1 then begin
    try
      p:=Word(StrToInt(ParamStr(2)));
    except
      on e: Exception do begin
        Writeln(e.message);
        Halt;
      end;
    end;
    MainLoop(ParamStr(1), p);
  end else Writeln('Usage: ', ParamStr(0), ' <address> <port>');
end.
