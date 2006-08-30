program fpfcgi;

{$mode objfpc}{$H+}

uses
  SysUtils, Sockets, BaseUnix;
  
procedure Bail(const Msg: string);
begin
  Writeln(Msg, ': ', fpgeterrno);
  Halt(-1);
end;

procedure Spawn(const AppName: string; const aPort: Word = 6000);
var
  TheSocket: Integer;
  Addr: TInetSockAddr;
  i: Integer = 1;
  PID: TPid;
  
  procedure HandleChild;
  begin
  end;
  
  procedure HandleParent;
  begin
  end;
  
begin
  CloseSocket(StdInputHandle);

  Addr.sin_family:=AF_INET;
  Addr.sin_addr.s_addr:=htonl(INADDR_ANY);
  Addr.sin_port:=htons(aPort);

  TheSocket:=fpSocket(AF_INET, SOCK_STREAM, 0);
  if TheSocket <> 0 then
    Bail('Socket not 0');

  if SetSocketOptions(TheSocket, SOL_SOCKET, SO_REUSEADDR, i, SizeOf(i)) < 0 then
    Bail('SetSocketOptions error');
    
  if fpBind(TheSocket, @Addr, SizeOf(Addr)) < 0 then
    Bail('Bind error');
    
  if fpListen(TheSocket, 1024) < 0 then
    Bail('Listen error');
    
  PID:=fpFork;
  
  if PID = 0 then
    HandleChild
  else
    HandleParent;
end;

begin
  if ParamCount > 0 then begin
    if ParamCount > 1 then
      Spawn(ParamStr(1), StrToInt(ParamStr(2)))
    else
      Spawn(ParamStr(1));
  end else
    Writeln('Usage: ', ParamStr(0), ' <appname> [port]');
end.

