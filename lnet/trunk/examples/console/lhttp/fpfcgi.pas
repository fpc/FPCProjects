program fpFCGI;

{$mode objfpc}{$H+}

uses
  SysUtils, Process, lNet, lEvents, lCommon, Sockets;

type

  { TLBlockingSocket }

  TLBlockingSocket = class(TLSocket)
   protected
    procedure SetNonBlock; override;
  end;
  
  { TSpawner }

  TSpawner = class
   protected
    procedure OnError(aHandle: TLHandle; const msg: string);
   public
    procedure Spawn;
  end;

{ TLBlockingSocket }

procedure TLBlockingSocket.SetNonBlock;
begin
  // do nothing
end;

{ TSpawner }

procedure TSpawner.OnError(aHandle: TLHandle; const msg: string);
begin
  Writeln(msg);
  Halt(1);
end;

procedure TSpawner.Spawn;
var
  TheSocket: TLBlockingSocket;
  p: TProcess;
  aPort: Word;
  i: Integer;
begin
  if ParamCount > 1 then begin
    aPort:=StrToInt(ParamStr(2));
    p:=TProcess.Create(nil);
    TheSocket:=TLBlockingSocket.Create;
    TheSocket.OnError:=@OnError;
    TheSocket.Protocol:=LPROTO_TCP;
    TheSocket.SocketType:=SOCK_STREAM;

    for i:=3 to 10000 do
      CloseSocket(i);
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
end;

var
  Spawner: TSpawner;
begin
  Spawner:=TSpawner.Create;
  Spawner.Spawn;
  Spawner.Free;
end.

