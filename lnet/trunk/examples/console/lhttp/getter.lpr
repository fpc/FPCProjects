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
  GET_STR = 'GET /hello.php HTTP/1.1'#13#10'Host: ps1:3880'#13#10#13#10;
  GET_BAD = 'GET info.php';
  SendString: string = GET_STR;
var
  I, index: integer;
begin
  FTCP.Connect('90.0.0.4', 3880);
  index := 0;
  while true do begin
    FTCP.CallAction;
    Sleep(1);
    if FTCP.Connected and not FSent then begin
      for I := 0 to 1 do
      begin
        index += FTCP.Send(SendString[index+1], Length(SendString)-index);
        if index = Length(SendString) then
        begin
          Writeln('SENT MESSAGE');
          index := 0;
        end else
          break;
      end;
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

