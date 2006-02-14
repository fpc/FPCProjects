{$ifdef mswindows}
  TomWinsock,
{$else}
  BaseUnix, NetDB,
{$endif}
  SysUtils, Sockets;

