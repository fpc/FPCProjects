program fpfcgi;

{$mode objfpc}{$H+}

uses
  SysUtils, Sockets, BaseUnix;
  
procedure Bail(const Msg: string);
begin
  Writeln(Msg, ': ', fpgeterrno);
  Halt(-1);
end;

procedure Spawn(const AppName, Enviro: string; const aPort: Word = 6000);
var
  TheSocket: Integer;
  Addr: TInetSockAddr;
  i: Integer = 1;
  PID: TPid;
  
  procedure HandleChild;
  var
    pEnv: ppChar;
  begin
    if TheSocket <> 0 then begin
      CloseSocket(0);
      fpdup2(TheSocket, 0);
      CloseSocket(TheSocket);
    end;
    if Length(Enviro) > 0 then begin
      GetMem(pEnv, SizeOf(PChar) * 2);
      pEnv[0]:=pChar(Enviro);
      pEnv[1]:=nil;
    end else
      pEnv:=nil;
    FpExecve(ParamStr(1), nil, pEnv);
    Halt(fpgeterrno);
  end;
  
  procedure HandleParent;
  var
    Status: Integer;
  begin
    Sleep(100);
    case FpWaitpid(PID, Status, WNOHANG) of
      0: Writeln('SUCCESS');
     -1: Bail('WaitPID error');
    else if wifexited(Status) then
           Writeln('Child exited with: ', wexitStatus(status))
         else if wifsignaled(Status) then
           Writeln('Child signalled: ', wtermsig(Status))
         else
           Writeln('Child died somehow: ', Status);
    end;
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
  else if PID < 0 then
    Bail('Fork error')
  else
    HandleParent;
end;

begin
  if ParamCount > 1 then
    Spawn(ParamStr(1), ParamStr(2))
  else
    Writeln('Usage: ', ParamStr(0), ' <appname> <environmental>');
end.

