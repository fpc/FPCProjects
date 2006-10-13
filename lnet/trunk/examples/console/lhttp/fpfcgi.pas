program fpFCGI;

{$mode objfpc}{$H+}

uses
  {$ifdef WINDOWS}
  WinSock2,
  {$endif}
  SysUtils, Process, lNet, lEvents, lCommon, Sockets;

type

  { TLBlockingSocket }

  TLBlockingSocket = class(TLSocket)
   protected
    procedure SetOptions; override;
  end;
  
  { TSpawner }

  TSpawner = class
   protected
    procedure OnError(aHandle: TLHandle; const msg: string);
   public
    procedure Spawn;
  end;

{ TLBlockingSocket }

procedure TLBlockingSocket.SetOptions;
var
  Arg: Integer = 1;
begin
  if SetSocketOptions(FHandle, SOL_SOCKET, SO_REUSEADDR, Arg, Sizeof(Arg)) = SOCKET_ERROR then
    Bail('SetSockOpt error', LSocketError);
  // don't call inherited, remain blocking
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
  Enviro, s: string;
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
    if ParamCount > 2 then begin
      Enviro:=ParamStr(3);
      repeat
        i:=Pos(':', Enviro);
        if i > 0 then begin
          s:=Copy(Enviro, 1, i - 1);
          Delete(Enviro, 1, i);
        end else
          s:=Enviro;
        p.Environment.Add(s);
      until i = 0;
    end;
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

