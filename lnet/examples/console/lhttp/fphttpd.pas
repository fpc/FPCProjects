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

program fpchttpd;

{$mode objfpc}{$h+}

uses
  classes, lnet, baseunix, lwebserver;

var
  Server: TLWebServer;
  
procedure MainLoop;
begin
  while Server.Connected do
    Server.CallAction;
end;

begin
  Server := TLWebServer.Create(nil);
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
