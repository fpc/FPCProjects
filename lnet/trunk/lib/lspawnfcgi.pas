unit lSpawnFCGI;

{$mode objfpc}{$H+}

interface

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
    function Spawn(App, Enviro: string; const aPort: Word): Integer;
  end;
  
implementation

{$ifdef WINDOWS}
  {$i lspawnfcgiwin.inc}
{$else}
  {$i lspawnfcgiunix.inc}
{$endif}

{ TLBlockingSocket }

procedure TLBlockingSocket.SetOptions;
var
  Arg: Integer = 1;
begin
  FBlocking:=True; // let's block
  if SetSocketOptions(FHandle, SOL_SOCKET, SO_REUSEADDR, Arg, Sizeof(Arg)) = SOCKET_ERROR then
    Bail('SetSockOpt error', LSocketError);
  inherited SetOptions;
end;

end.

