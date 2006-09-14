program getter;

{$mode objfpc}{$H+}

uses
  SysUtils, lNet;
  
type

  { TGetter }

  TGetter = class
   protected
    FTCP: TLTcp;
    FSent: Boolean;
    procedure OnEr(const msg: string; aSocket: TLSocket);
    procedure OnCo(aSocket: TLSocket);
    procedure OnDs(aSocket: TLSocket);
    procedure OnRe(aSocket: TLSocket);
   public
    constructor Create;
    destructor Destroy; override;
    procedure Run;
  end;

{ TGetter }

procedure TGetter.OnEr(const msg: string; aSocket: TLSocket);
begin
  Writeln(msg);
end;

procedure TGetter.OnCo(aSocket: TLSocket);
begin
  // nothing
end;

procedure TGetter.OnDs(aSocket: TLSocket);
begin
  Writeln('DISCONNECTED');
  FTCP.Connect('localhost', 3880);
end;

procedure TGetter.OnRe(aSocket: TLSocket);
var
  s: string;
begin
  if aSocket.GetMessage(s) > 0 then
    Write(s)
  else
    Writeln('GETMESSAGE RETURNED 0');
  FSent:=False;
end;

constructor TGetter.Create;
begin
  FSent:=False;
  FTCP:=TLTcp.Create(nil);
  FTCP.OnError:=@OnEr;
  FTCP.OnConnect:=@OnCo;
  FTCP.OnDisconnect:=@OnDs;
  FTCP.OnReceive:=@OnRe;
end;

destructor TGetter.Destroy;
begin
  FTCP.Free;
end;

procedure TGetter.Run;
const
  GET_STR = 'GET /info.php HTTP/1.1'#13#10'Host: ps1:3880'#13#10#13#10;
begin
  FTCP.Connect('localhost', 3880);
  while true do begin
    FTCP.CallAction;
    Sleep(1);
    if FTCP.Connected and not FSent then begin
      if FTCP.SendMessage(GET_STR) = Length(GET_STR) then
        Writeln('SENT MESSAGE');
      FSent:=True;
    end;
  end;
end;

var
  Get: TGetter;
begin
  Get:=TGetter.Create;
  Get.Run;
  Get.Free;
end.

