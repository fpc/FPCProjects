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
{$ifdef UNIX}
  BaseUnix,
{$endif}
  lWebserver, lHTTPSettings;

var
  Server: TLWebServer;
  
procedure MainLoop;
var
  lRuns: dword;
begin
  Writeln('Succesfully started server');
  lRuns := $FFFFFFFF;
  while (lRuns > 0) and (Server.Connected) do
  begin
    Server.CallAction;
    dec(lruns);
    if (lruns and $FFF) = 0 then
      writeln(lruns);
  end;
end;

procedure Run(const DoFork: Boolean);
begin
  Server := TLWebServer.Create(nil);
  Server.TimeOut := 300000;
  if not Server.Listen(GetPort) then
  begin
    Writeln('Error starting server.');
  end else begin
    if DoFork then begin
      if fpfork = 0 then
        MainLoop
      else
        fpExit(0);
    end else
      MainLoop;
  end;
  Server.Free;
end;

begin
  if (LowerCase(ParamStr(1)) = '-h')
  or (LowerCase(ParamStr(1)) = '--help') then begin
    Writeln('Usage: ', ParamStr(0), ' [-f]');
    Writeln('       -f -- starts server without forking');
  end else
    Run(LowerCase(ParamStr(1)) <> '-f');
end.
