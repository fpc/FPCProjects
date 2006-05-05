program fpchttpd;

{$mode objfpc}{$h+}

uses
  classes, lnet, httpserver, crt;

var
  Server: THTTPServer;
  
procedure MainLoop;
begin
  while Server.Connected do begin
    Server.CallAction;
    if KeyPressed then Exit;
  end;
end;

begin
  Server := THTTPServer.Create(nil);
  if not Server.Listen(3880) then
  begin
    writeln('Error starting server.');
  end else begin
{
    writeln('Succesfully started server');
    if fpfork = 0 then
    begin
}
      MainLoop;
{
    end else
      fpexit(0);
}
  end;
  Server.Free;
end.
