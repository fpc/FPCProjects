program fpFCGI;

{$mode objfpc}{$H+}

uses
  SysUtils, Process, lNet, lCommon, Sockets;

type

  { TLBlockingSocket }

  TLBlockingSocket = class(TLSocket)
   protected
    procedure SetNonBlock; override;
  end;
  
{ TLBlockingSocket }

procedure TLBlockingSocket.SetNonBlock;
begin
  // do nothing
end;

var
  TheSocket: TLBlockingSocket;
  p: TProcess;
  aPort: Word;
begin
  if ParamCount > 1 then begin
    aPort:=StrToInt(ParamStr(2));
    p:=TProcess.Create(nil);
    TheSocket:=TLBlockingSocket.Create;
    TheSocket.Protocol:=LPROTO_TCP;
    TheSocket.SocketType:=SOCK_STREAM;

    if CloseSocket(StdInputHandle) <> 0 then
      Halt(LSocketError);

    if not TheSocket.Listen(aPort) then
      Halt(LSocketError);

    p.CommandLine:=ParamStr(1);
    if ParamCount > 2 then
      p.Environment.Add(ParamStr(3));
    p.Execute;

    TheSocket.Free;
    p.Free;
  end else
    Writeln('Usage: ', ExtractFileName(ParamStr(0)), ' <appname> <port> [environment]');
end.

