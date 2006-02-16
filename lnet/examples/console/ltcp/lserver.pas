program lserver;

{$mode objfpc}{$H+}

uses
  Classes, Crt, SysUtils, lNet;
  
type

{ TLEvents }

  TLEvents = class
   public
    procedure er(const msg: string; aSocket: TLSocket);
    procedure ac(aSocket: TLSocket);
    procedure re(aSocket: TLSocket);
    procedure ds(aSocket: TLSocket);
  end;
     
var quit: boolean;
    con: TLTcp;

procedure TLEvents.er(const msg: string; aSocket: TLSocket);
begin
  writeln('ERROR: ', msg);
end;

procedure TLEvents.re(aSocket: TLSocket);
var
  s: string;
begin
  if aSocket.GetMessage(s) > 0 then begin
    writeln('Got: "', s, '" with length: ', Length(s));
    if Assigned(Con.Iterator) then repeat
      Con.CallAction;
      if Con.SendMessage(s, Con.Iterator) < Length(s) then
        writeln('Unsuccessful send');
    until not Con.IterNext;
  end;
end;

procedure TLEvents.ds(aSocket: TLSocket);
begin
  Writeln('Lost connection');
end;

procedure TLEvents.ac(aSocket: TLSocket);
begin
  writeln('Connection accepted from ', aSocket.PeerAddress);
end;

procedure bigloop(const port: Word);
var
  event: TLEvents;
begin
  quit:=false;
  event:=TLEvents.Create;
  con:=TLTcp.Create(nil);
  con.OnError:=@event.er;
  con.OnAccept:=@event.ac;
  con.OnReceive:=@event.re;
  con.OnDisconnect:=@event.ds;
  if con.Listen(port) then begin
    Writeln('Server running!');
    Writeln('Press ''escape'' to quit, ''r'' to restart');
    repeat
      con.callaction;
      delay(1);
      if keypressed then
        case readkey of
         #27: quit:=true;
         'r': begin
                writeln('Restarting...');
                con.Disconnect;
                con.Listen(port);
                quit:=false;
              end;
        end;
    until quit;
  end;
  con.free;
  event.free;
end;

var
  p: Word;
begin
//  if ParamCount > 0 then begin
    try
//      p:=Word(StrToInt(ParamStr(1)));
      p:=4665;
    except
      on e: Exception do begin
        Writeln(e.message);
        Halt;
      end;
    end;
    bigloop(p);
//  end else Writeln('Usage: ', ParamStr(0), ' <port>');
end.

