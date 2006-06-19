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
  classes, lnet, 
{$ifdef UNIX}
  baseunix, 
{$endif}
  lwebserver;

var
  Server: TLWebServer;
  
procedure MainLoop;
var
  lRuns: dword;
begin
  lRuns := $FFFFFFFF;
  while (lRuns > 0) and (Server.Connected) do
  begin
    Server.CallAction;
    dec(lruns);
    if (lruns and $FFF) = 0 then
      writeln(lruns);
  end;
end;

begin
  Server := TLWebServer.Create(nil);
  Server.TimeOut := 300000;
  if not Server.Listen(3880) then
  begin
    writeln('Error starting server.');
  end else begin
{
    writeln('Succesfully started server');
    if fpfork = 0 then
    begin
}
      Server.Eventer.Timeout := 30000;
      MainLoop;
{
    end else
      fpexit(0);
}
  end;
  Server.Free;
end.
