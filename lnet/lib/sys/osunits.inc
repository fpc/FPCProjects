{$ifdef mswindows}
  Winsock2,
{$else}
  BaseUnix, NetDB,
{$endif}
  SysUtils, Sockets;

