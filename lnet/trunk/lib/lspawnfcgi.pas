unit lSpawnFCGI;

{$mode objfpc}{$H+}

interface

uses
  lNet, Sockets, lCommon;

type

  { TLBlockingSocket }

  TLBlockingSocket = class(TLSocket)
   protected
    procedure SetOptions; override;
  end;

 function SpawnFCGIProcess(App, Enviro: string; const aPort: Word): Integer;

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
    Bail('SetSockOpt error', LSocketError); // let's reuse address so we can fail gracefuly
  inherited SetOptions;
end;

end.

