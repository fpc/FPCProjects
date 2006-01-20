{%MainUnit lnet.pas}
{$IFDEF WIN32}

uses
  Windows;

function StrError(const Ernum: Longint; const UseUTF8: Boolean = False): string;
var
  Tmp, TmpW: string;
begin
  Result:='[' + IntToStr(Ernum) + '] ';
  if USEUtf8 then begin
    SetLength(TmpW, 256);
    SetLength(TmpW, FormatMessageW(FORMAT_MESSAGE_FROM_SYSTEM or
                                   FORMAT_MESSAGE_IGNORE_INSERTS or
                                   FORMAT_MESSAGE_ARGUMENT_ARRAY,
                                   nil, Ernum, 0, @TmpW[1], 256, nil));
    Tmp:=UTF8Encode(TmpW);
  end else begin
    SetLength(Tmp, 256);
    SetLength(Tmp, FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM or
                                 FORMAT_MESSAGE_IGNORE_INSERTS or
                                 FORMAT_MESSAGE_ARGUMENT_ARRAY,
                                 nil, Ernum, 0, @Tmp[1], 256, nil));
  end;
  if Length(Tmp) > 2 then
    Delete(Tmp, Length(Tmp)-1, 2);
  Result:=Tmp;
end;

function CleanError(const Ernum: Longint): Byte;
begin
  Result:=Byte(Ernum - 10000);
end;

function fpSelect(const nfds: Integer; const readfds, writefds, exceptfds: PFDSet;
                  const timeout: PTimeVal): Longint; inline;
begin
  Result:=Select(nfds, readfds, writefds, exceptfds, timeout);
end;

function fpFD_ISSET(const Socket: Longint; var FDSet: TFDSet): Integer; inline;
begin
  Result:=0;
  if FD_ISSET(Socket, FDSet) then
    Result:=1;
end;

procedure fpFD_SET(const Socket: Longint; var FDSet: TFDSet); inline;
begin
  FD_SET(Socket, FDSet);
end;

procedure fpFD_ZERO(var FDSet: TFDSet); inline;
begin
  FD_ZERO(FDSet);
end;

function GetHostName(const Address: string): string;
var
  HE: PHostEnt;
  Addr: DWord;
begin
  Result:='';
  HE:=nil;
  Addr:=inet_addr(PChar(Address));
  HE:=gethostbyaddr(@Addr, SizeOf(Addr), AF_INET);
  if Assigned(HE) then
    Result:=HE^.h_name;
end;

function GetHostIP(const Name: string): string;
var
  HE: PHostEnt;
  P: PDWord;
begin
  Result:='';
  HE:=nil;
  HE:=gethostbyname(PChar(Name));
  if Assigned(HE) then begin
    P:=Pointer(HE^.h_addr_list[0]);
    Result:=HostAddrToStr(P^);
  end;
end;

function SocketError: Longint;
begin
  Result:=WSAGetLastError;
end;

{$ELSE}

uses
  Errors;

function StrError(const Ernum: Longint; const UseUTF8: Boolean = False): string;
begin
  Result:='[' + IntToStr(Ernum) + '] ' + Errors.StrError(Ernum);
end;

function CleanError(const Ernum: Longint): Longint; inline;
begin
  Result:=Byte(Ernum);
end;

function GetHostName(const Address: string): string;
var
  HE: THostEntry;
begin
  Result:='';
  {$ifdef darwin}
  if GetHostbyAddr(in_addr(StrToNetAddr(Address)), HE) then
  {$else}
  if GetHostbyAddr(in_addr(StrToHostAddr(Address)), HE) then
  {$endif}
    Result:=HE.Name
  {$ifdef darwin}
  else if ResolveHostbyAddr(in_addr(StrToNetAddr(Address)), HE) then
  {$else}
  else if ResolveHostbyAddr(in_addr(StrToHostAddr(Address)), HE) then
  {$endif}
    Result:=HE.Name;
end;

function GetHostIP(const Name: string): string;
var
  HE: THostEntry;
begin
  Result:='';
  if GetHostByName(Name, HE) then
  {$ifdef darwin}
    Result:=HostAddrToStr(Cardinal(HE.Addr)) // for localhost
  {$else}
    Result:=NetAddrToStr(Cardinal(HE.Addr)) // for localhost
  {$endif}
  else if ResolveHostByName(Name, HE) then
    Result:=HostAddrToStr(Cardinal(HE.Addr));
end;

function SocketError: Longint;
begin
  Result:=Sockets.SocketError;
end;

{$ENDIF}

function NetAddrToStr (Entry : Cardinal) : string;

type THostAddr = array[1..4] of Byte;
Var Dummy : string[4];
    I : LongInt;

begin
  NetAddrToStr:='';
  For I:=4 Downto 1 do
   begin
   Dummy:='';
   Str(THostAddr(Entry)[I],Dummy);
   NetAddrToStr:=NetAddrToStr+Dummy;
   If I>1 Then NetAddrToStr:=NetAddrToStr+'.';
   end;
end;

function StrToNetAddr(IP : string) : Cardinal;
type THostAddr = array[1..4] of Byte;
Var Dummy : string[4];
   I : LongInt;
   J : Integer;
   Temp : THostAddr;
begin
 Result:=0;
 For I:=1 to 4 do
  begin
  If I<4 Then
    begin
    J:=Pos('.',IP);
    If J=0 then exit;
    Dummy:=Copy(IP,1,J-1);
    Delete (IP,1,J);
    end
  else
    Dummy:=IP;
  Val (Dummy, Temp[I], J);
  If J<>0 then Exit;
  end;
 Result:=Cardinal(Temp);
end;

function HostAddrToStr (Entry : Cardinal) : string;
type THostAddr = array[1..4] of Byte;
var Dummy : string[4];
    I : LongInt;
begin
  HostAddrToStr:='';
  For I:=1 to 4 do
   begin
   Dummy:='';
   Str(THostAddr(Entry)[I],Dummy);
   HostAddrToStr:=HostAddrToStr+Dummy;
   If I < 4 Then HostAddrToStr:=HostAddrToStr+'.';
   end;
end;

function StrToHostAddr(IP: string): Cardinal;
type THostAddr = array[1..4] of Byte;
var Dummy : string[4];
   I : LongInt;
   J : Integer;
   Temp : THostAddr;
begin
 Result:=0;
 For I:=4 downto 1 do
  begin
  If I > 1 Then
    begin
    J:=Pos('.',IP);
    If J=0 then exit;
    Dummy:=Copy(IP,1,J-1);
    Delete (IP,1,J);
    end
  else
    Dummy:=IP;
  Val (Dummy, Temp[I], J);
  If J <> 0 then Exit;
  end;
 Result:=Cardinal(Temp);
end;
