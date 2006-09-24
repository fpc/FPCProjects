{ Web serving daemon

  Copyright (C) 2006 Micha Nelissen

  This library is Free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is diStributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; withOut even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a Copy of the GNU Library General Public License
  along with This library; if not, Write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
  
  This license has been modified. See file LICENSE.ADDON for more information.
  Should you find these sources without a LICENSE File, please contact
  me at ales@chello.sk
}

program fphttpd;

{$mode objfpc}{$h+}

uses
  Classes, lNet,
{$ifndef MSWINDOWS}
  BaseUnix, Errors,
{$endif}
  lWebserver, lHTTPSettings;

var
  Server: TLWebServer;
  Quit: Boolean = False;
  
procedure HandleTermSignal(sig: longint); cdecl;
begin
  Quit := true;
end;
  
procedure MainLoop;
var
  lRuns: dword;
begin
  Writeln('Succesfully started server');
  lRuns := $FFFFFFFF;
  while (lRuns > 0) and (Server.Connected) and (not Quit) do
  begin
    Server.CallAction;
    if Assigned(Server.PHPCGIHandler.Pool.Timer) then
    begin
      Server.Eventer.Timeout := 2000;
      Server.PHPCGIHandler.Pool.Timer.CallAction;
    end;
    dec(lruns);
    if (lruns and $FFF) = 0 then
      writeln(lruns);
  end;
end;

function SetServer: Boolean;
begin
  Result:=False;
  Server := TLWebServer.Create(nil);
  Server.TimeOut := 300000;
  if not Server.Listen(GetPort) then
    Writeln('Error starting server.')
  else
    Result:=True;
end;

procedure HandleSignals;
begin
  {$ifndef MSWINDOWS}
  FpSignal(SIGTERM, @HandleTermSignal);
  FpSignal(SIGINT, @HandleTermSignal);
  FpSignal(SIGHUP, signalhandler(SIG_IGN));
  {$else}
  {$endif}
end;

function Daemonize: Boolean;
  {$ifndef MSWINDOWS}
var
  PID: TPid;
begin
  PID:=fpFork;
  if PID = 0 then begin
    Result:=SetServer;
    if Result then
      EnableWriteln:=False;
  end else if PID < 0 then
    Writeln('Error on fork: ', StrError(fpGetErrno));
  {$else}
begin
  Result:=False;
  {$endif}
end;

procedure Run(const BG: Boolean);
begin
  if BG then begin
    if Daemonize then
      MainLoop
    else
      Writeln('Unable to start daemon/service, please try with -c');
  end else if SetServer then
    MainLoop;
  Server.Free;
end;

begin
  HandleSignals;
  if (LowerCase(ParamStr(1)) = '-h')
  or (LowerCase(ParamStr(1)) = '--help') then begin
    Writeln('Usage: ', ParamStr(0), ' [-c]');
    Writeln('       -c -- starts server in console (not as daemon/service)');
  end else
    Run(LowerCase(ParamStr(1)) <> '-c');
end.
