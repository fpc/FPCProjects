{ lNet v2.0.6

  CopyRight (C) 2004 Ales Katona

  This library is Free software; you can rediStribute it and/or modify it
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
  
  This license has been modified. See File LICENSE for more inFormation.
  Should you find these sources withOut a LICENSE File, please contact
  me at ales@chello.sk
}

unit lNet;

{$mode objfpc}{$H+}
//{$define nonblocking}

interface

uses
  {$ifdef unix} BaseUnix, NetDB, {$else} TomWinsock, {$endif}
  SysUtils, Contnrs, Sockets, Classes;

const
  { Protocol specifiers }
  LTCP = 0;
  LUDP = 1;
  { Default Values }
  DefaultMaxSockets =    64;
  DefaultBufferSize = 65535;
  DefaultMaxMsgs    =  1024;
  { Address constants }
  LADDR_ANY = '0.0.0.0';
  LADDR_BR  = '255.255.255.255';
  LADDR_LO  = '127.0.0.1';
  {$IFDEF WIN32}
  SOL_SOCKET = $ffff;
  LMSG = 0;
  {$ELSE}
  LMSG = MSG_NOSIGNAL;
  {$ENDIF}

type
  TLConnection = class;

  { CallBack Event procedure }
  TLObjProc = procedure(const msg: string; const snum: LongInt) of object;

  { Base socket class, Holds Address and socket info, perForms basic
    socket operations, each has it's own Data buffer for Recieves }
  TLSocket = class
   protected
    FParent: TLConnection;
    FBuffer: TStringList;
    FAddr: TInetSockAddr;
    FCliAddr: TInetSockAddr;
    FConnected: Boolean;
    FBufferSize: Longint;
    FMaxmsgs: Longint;
    FAddrlen: Longint;
    FSock: Longint;
    FFlag: Longint;
    FSnum: LongInt;
    FSockPort: Word;
    function SetupSocket(const APort: Word; const Address: string): Boolean;
    procedure SetNonBlock;
    procedure SetBufferSize(const Size: LongInt);
    procedure Bail(const msg: string; const ernum: LongInt);
    procedure LogError(const msg: string; const ernum: LongInt);
   public
    constructor Create(const protocol: Byte; Owner: TLConnection; const num: LongInt = 0);
    constructor Create(const protocol: Byte; const num: LongInt = 0);
    destructor Destroy; override;
    function SetServer(const APort: Word): Boolean;
    function Accept(const SerSock: LongInt): Boolean;
    function Connect(const APort: Word; const Address: string): Boolean;
    function Send(const msg: string): Boolean;
    function Recieve: Boolean;
    function GetHostAddress: string;
    function GetMessage(out msg: string): Boolean;
    procedure Disconnect;
    property BufferSize: LongInt read FBufferSize write SetBufferSize;
    property Port: Word read FSockPort;
    property MaxMsgs: LongInt read FMaxMsgs write FMaxMsgs;
    property Connected: Boolean read FConnected;
    property SNum: LongInt read FSnum;
  end;

  {$i lsocketlisth.inc}

  { Common ancestor for TLTcp and TLUdp classes. Holds Event properties
    and common variables }
  TLConnection = class
   protected
    FSerSock: TLSocket;
    FBufferSize: LongInt;
    FTimeVal: TTimeVal;
    FMaxMsgs: LongInt;
    FOnRecieve,
    FOnError,
    FOnAccept,
    FOnDisconnect: TLObjProc;
    procedure SetBufferSize(const Size: LongInt);
    procedure SetMaxMsgs(const Size: LongInt);
   public
    constructor Create;
    destructor Destroy; override;
    function Connect(const APort: Word; const Address: string): Boolean; virtual; abstract;
    function Accept(const APort: Word): Boolean; virtual; abstract;
    function SendMessage(const msg: string): Boolean; virtual; abstract;
    procedure Disconnect(const s: LongInt = -1); virtual; abstract;
    procedure CallAction; virtual; abstract;
    property OnAccept: TLObjProc read FOnAccept write FOnAccept;
    property OnError: TLObjProc read FOnError write FOnError;
    property OnRecieve: TLObjProc read FOnRecieve write FOnRecieve;
    property OnDisconnect: TLObjProc read FOnDisconnect write FOnDisconnect;
    property BufferSize: LongInt read FBufferSize write SetBufferSize;
    property MaxMsgs: LongInt read FMaxMsgs write SetMaxMsgs;
  end;
  
  { UDP Client/Server class. Provided to enable usage of UDP sockets }
  TLUdp = class(TLConnection)
   protected
    function GetMessageCount: Cardinal;
    procedure Bail(const msg: string);
   public
    constructor Create;
    destructor Destroy; override;
    function Connect(const APort: Word; const Address: string = LADDR_BR): Boolean; override;
    function Accept(const APort: Word): Boolean; override;
    function GetMessage(out s: string): Boolean;
    function GetHostAddress: string;
    function SendMessage(const msg: string): Boolean; override;
    function SendMessage(const msg: string; const Address: string): Boolean;
    procedure CallAction; override;
    procedure Disconnect(const snum: LongInt = -1); override;
    property MessageCount: Cardinal read GetMessageCount;
  end;
  
  { TCP Client/Server class. Provided to enable usage of TCP sockets }
  TLTcp = class(TLConnection)
   protected
    FSmax: Cardinal;
    FSocks: TLSocketList;
    FAccepting: Boolean;
    FMaxSockets: Longint;
    FReadFDSet: TFDSet;
    function GetCount: Integer;
    function GetItem(const i: Longint): TLSocket;
    function CallRecieve(const snum: LongInt = -1): Boolean;
    procedure CallAccept;
    procedure Bail(const msg: string; const socknum: LongInt);
   public
    constructor Create;
    destructor Destroy; override;
    function Connect(const APort: Word; const Address: string): Boolean; override;
    function Accept(const APort: Word): Boolean; override;
    function GetMessage(out s: string; snum: LongInt = 0): Boolean;
    function SendMessage(const msg: string): Boolean; override;
    function SendMessage(const msg: string; const snum: LongInt): Boolean;
    function GetMessageCount(snum: LongInt): Cardinal;
    function GetPosition(const snum: LongInt): LongInt;
    function GetSnum(const Pos: LongInt): LongInt;
    function GetHostAddress(const snum: LongInt): string;
    procedure CallAction; override;
    procedure Disconnect(const s: LongInt = -1); override;
    property Socks[index: Longint]: TLSocket read GetItem; default;
    property Count: LongInt read GetCount;
    property Accepting: Boolean read FAccepting write FAccepting;
    property MaxSockets: Longint read FMaxSockets write FMaxSockets;
  end;
  
  {$i tlconnectionlisth.inc}
  
  { Common functions}
  procedure CallConnections;

  { Base functions }
  function StrToHostAddr(IP: string): Cardinal;
  function HostAddrToStr(Entry: Cardinal): string;
  function StrToNetAddr(IP: string): Cardinal;
  function NetAddrToStr(Entry: Cardinal): string;
  function DataTostring(const Data; const Size: Cardinal): string;
  procedure StringToData(const Str: string; out Data; const Size: Cardinal = 0);
  {$IFDEF WIN32}
  function fpSelect(const nfds: Integer; const readfds, writefds, exceptfds: PFDSet;
                    const timeout: PTimeVal): Longint; inline;
  function fpFD_ISSET(const Socket: Longint; var FDSet: TFDSet): Integer; inline;
  procedure fpFD_SET(const Socket: Longint; var FDSet: TFDSet); inline;
  procedure fpFD_ZERO(var FDSet: TFDSet); inline;
  {$ENDIF}
  { DNS }
  function GetHostName(const Address: string): string;
  function GetHostIP(const Name: string): string;
  
implementation

var
  Connections: TLConnectionList;

{$i lfunc.inc}  // the borrowed functions from inet.pp
{$i lsocketlist.inc}
{$i tlconnectionlist.inc}

function SocketError: Longint;
begin
  {$ifdef win32}
  Result:=WSAGetLastError;
  {$else}
  Result:=Sockets.SocketError;
  {$endif}
end;

procedure CallConnections;
var
  i: Longint;
begin
  if Connections.Count > 0 then
    for i:=0 to Connections.Count-1 do
      Connections[i].CallAction;
end;

//*******************************TLConnection*********************************

constructor TLConnection.Create;
begin
  Connections.Add(Self);
end;

destructor TLConnection.Destroy;
begin
  Connections.Remove(Self);
end;

procedure TLConnection.SetBufferSize(const Size: LongInt);
begin
  if  (Size > 4)
  and (Size <= DefaultBufferSize) then begin
    FBufferSize:=Size;
    FSerSock.BufferSize:=Size;
  end;
end;

procedure TLConnection.SetMaxMsgs(const Size: LongInt);
begin
  if Size > 4 then
    FMaxMsgs:=Size;
end;

///*******************************TLUdp*********************************

constructor TLUdp.Create;
begin
  inherited Create;
  FTimeVal.tv_usec:=0;
  FTimeVal.tv_sec:=0;
  FBufferSize:=DefaultBufferSize;
  FMaxMsgs:=DefaultMaxMsgs;
  FOnRecieve:=nil; FOnAccept:=nil; FOnError:=nil;
  FSerSock:=TLSocket.Create(LUDP, Self, 1);
end;

destructor TLUdp.Destroy;
begin
  Disconnect;
  FSerSock.Free;
  inherited Destroy;
end;

procedure TLUdp.Disconnect(const snum: LongInt = -1);
begin
  FSersock.Disconnect;
//   if Assigned(FOnDisconnect) then FOnDisconnect('Disconnecting', -1);
end;

function TLUdp.Accept(const APort: Word): Boolean;
begin
  if FSerSock.Connected then
    Disconnect;
  Result:=false;
  Result:=FSerSock.SetServer(APort);
  FSersock.FCliAddr:=FSerSock.FAddr;
  FSersock.FCliAddr.Addr:=StrToNetAddr(LADDR_BR);
  FSersock.FConnected:=true;
end;

function TLUdp.Connect(const APort: Word; const Address: string): Boolean;
begin
  Result:=False;
  if  FSerSock.Connected then Disconnect;
  Result:=FSerSock.SetupSocket(APort, LADDR_ANY);
  FSerSock.FCliAddr:=FSerSock.FAddr;
  FSerSock.FCliAddr.Addr:=StrToNetAddr(Address);
  {$ifdef nonblocking}
  if Result then FSerSock.SetNonBlock;
  {$endif}
  FSersock.FConnected:=true;
end;

procedure TLUdp.Bail(const msg: string);
begin
  Disconnect;
  if Assigned(FOnError) then
    FOnError(msg, 0);
end;

function TLUdp.GetHostAddress: string;
begin
  Result:='';
  Result:=FSerSock.GetHostAddress;
end;

procedure TLUdp.CallAction;
var
  s: string;
  FDSet: TFDSet;
  n: Longint;
begin
  if FSerSock.Connected then
    begin
      fpFD_ZERO(FDSet);
      fpFD_SET(FSerSock.FSock, FDSet);
      n:=fpSelect(FSerSock.FSock+1, @FDSet, nil, nil, @FTimeVal);
      if n < 0 then
        Bail('Error on select');
      if n > 0 then
        if FSerSock.Recieve and GetMessage(s) then
          if Assigned(FOnRecieve) then
            FOnRecieve(s, 0);
    end;
end;

function TLUdp.GetMessageCount: Cardinal;
begin
  Result:=0;
  Result:=FSerSock.FBuffer.Count;
end;

function TLUdp.GetMessage(out s: string): Boolean;
begin
  Result:=FSerSock.GetMessage(s);
end;

function TLUdp.SendMessage(const msg: string): Boolean;
begin
  Result:=False;
  if (Length(msg) > 0) and (Length(msg) < FBufferSize) then begin
    Result:=True;
    Result:=FSerSock.Send(msg);
  end else Bail('Message too long');
end;

function TLUdp.SendMessage(const msg: string; const Address: string): Boolean;
begin
  if (Length(msg) > 0) and (Length(msg) < FBufferSize) then
    begin
      Result:=True;
      FSersock.FCliAddr.Addr:=StrToNetAddr(Address);
      Result:=FSerSock.Send(msg);
    end else Bail('Message too long');
end;

//******************************TLTCP**********************************

constructor TLTcp.Create;
begin
  inherited Create;
  FTimeVal.tv_usec:=10;
  FTimeVal.tv_sec:=0;
  FBufferSize:=DefaultBufferSize;
  FMaxMsgs:=DefaultMaxMsgs;
  maxsockets:=defaultMaxSockets;
  FSmax:=1;
  FSocks:=TLSocketList.Create;
  FOnRecieve:=nil; FOnAccept:=nil; FOnError:=nil;
  FAccepting:=false;
  FSerSock:=TLSocket.Create(LTCP, Self, -1);
end;

destructor TLTcp.Destroy;
begin
  Disconnect;
  FSocks.Free;
  FSerSock.Free;
  inherited Destroy;
end;

procedure TLTcp.Disconnect(const s: LongInt = -1);
var i, z: LongInt;
begin
  z:=-1;
  if (FSocks.Count > 0) and (s >= 0) then
    for i:=FSocks.Count-1 Downto 0 do
       if FSocks[i].FSnum = s then z:=i;
  if z < 0 then
    begin
      FAccepting:=False;
      FSocks.Clear;
      FSerSock.Disconnect;
    end else
    if z < FSocks.Count then FSocks.Delete(z);
end;

procedure TLTcp.Bail(const msg: string; const socknum: LongInt);
begin
  if socknum >= 0 then
    Disconnect(socknum) else Disconnect;
  if Assigned(FOnError) then
    FOnError(msg, socknum);
end;

function TLTcp.GetHostAddress(const snum: LongInt): string;
var i: LongInt;
begin
  Result:='';
  if FSocks.Count > 0 then
    for i:=FSocks.Count-1 Downto 0 do
      if FSocks[i].FSnum = snum then
        Result:=FSocks[i].GetHostAddress;
end;

function TLTcp.Accept(const APort: Word): Boolean;
begin
  if not FAccepting then
    begin
      Result:=false;
      if FSocks.Count > 0 then Disconnect;
      if FSerSock.SetServer(APort) then
        begin
          FSerSock.FConnected:=true;
          FAccepting:=true;
          Result:=true;
        end else bail('Error on Accept', FSerSock.FSnum);
    end else bail('Error, alReady Accepting', FSerSock.FSnum);
end;

function TLTcp.Connect(const APort: Word; const Address: string): Boolean;
begin
  Result:=False;
  if FSocks.Count > 0 then Disconnect;
  FSocks.Add(TLSocket.Create(LTCP, Self, 0));
  with FSocks.Last do
    begin
      SetupSocket(APort, Address);
      if fpConnect(FSock, @FAddr, FAddrlen) = 0 then
        begin
          FConnected:=true;
          {$ifdef nonblocking}
           Setnonblock;
          {$endif}
          if Assigned(FOnAccept) then
             FOnAccept('Connected to remote Host', 1);
          Result:=Connected;
        end else Bail('Error on Connect', FSocks.Last.FSnum);
    end;
end;

procedure TLTcp.CallAction;

  function CallSelect: Boolean;
  var
    i: Longint;
    max: Longint;
  begin
    result:=False;
    max:=0;
    fpFD_ZERO(FReadFDSet);
    if FAccepting and FSerSock.Connected then
      begin
        max:=FSerSock.FSock;
        fpFD_SET(FSerSock.FSock, FReadFDSet);
      end;
    if FSocks.Count > 0 then
      for i:=FSocks.Count-1 downto 0 do
        if FSocks[i].Connected then
          begin
            fpFD_SET(FSocks[i].FSock, FReadFDSet);
            if FSocks[i].FSock > max then
              max:=FSocks[i].FSock;
          end else FSocks.Delete(i);
    if max > 0 then
      i:=fpSelect(max+1, @FReadFDSet, nil, nil, @FTimeVal)
        else i:=0;
    if i < 0 then
      Bail('Error on select ', SocketError);
    if i = 0 then result:=False;
    if i > 0 then result:=True;
  end;
  
begin
  if (FSerSock.Connected or (FSocks.Count > 0)) and CallSelect then
    begin
      CallRecieve;
      if FAccepting then CallAccept;
    end;
end;

function TLTcp.CallRecieve(const snum: LongInt = -1): Boolean;

  procedure ControlSock(const num: LongInt);
  var s: string;
  begin
    if  (num >= 0) and (num < FSocks.Count)
    and (fpFD_ISSET(FSocks[num].FSock, FReadFDSet) <> 0) then
      with FSocks[num] do
        if Recieve then begin
          while FSocks[num].GetMessage(s) do
            if Assigned(FOnRecieve) then FOnRecieve(s, FSocks[num].FSnum);
          Result:=true;
        end else FSocks.Delete(num);
  end;
  
var
  i: LongInt;
begin
  Result:=False;
  if snum < 0 then
    begin
      if FSocks.Count > 0 then
        for i:=FSocks.Count-1 downto 0 do
          ControlSock(i);
    end else if snum < FSocks.Count then
      ControlSock(snum);
end;

procedure TLTcp.CallAccept;

begin
  if  (fpFD_ISSET(FSerSock.FSock, FReadFDSet) <> 0)
  and ((FSocks.Count) < MaxSockets) then begin
    FSocks.Add(TLSocket.Create(LTCP, Self, FSmax));
    with FSocks.Last as TLSocket do
      if Accept(FSerSock.FSock) then begin
        Inc(FSmax);
        BufferSize:=Self.BufferSize;
        if Assigned(FOnAccept) then
          FOnAccept('Connection Accepted from '+GetHostAddress, FSmax-1);
      end else FSocks.Delete(FSocks.Count-1);
  end;
end;

function TLTcp.GetPosition(const snum: LongInt): LongInt;
var i: LongInt;
begin
  Result:=-1;
  if FSocks.Count > 0 then
    for i:=0 to FSocks.Count-1 do
      if snum = FSocks[i].FSnum then Result:=i;
end;

function TLTcp.GetSnum(const Pos: LongInt): LongInt;
begin
  Result:=-1;
  if (FSocks.Count > 0) and (Pos < FSocks.Count) then
    if Pos >= 0 then Result:=FSocks[Pos].FSnum;
end;

function TLTcp.GetItem(const i: longint): TLSocket;
begin
  Result:=FSocks[i];
end;

function TLTcp.GetCount: LongInt;
begin
  Result:=FSocks.Count;
end;

function TLTcp.GetMessageCount(snum: LongInt): Cardinal;
begin
  Result:=0;
  if (snum >= 0) and (Snum < FSocks.Count) then
    Result:=FSocks[Snum].FBuffer.Count;
end;

function TLTcp.GetMessage(out s: string; snum: LongInt = 0): Boolean;
begin
  Result:=false;
  if Count > 0 then
    if (snum >= 0) and (snum < Count) then
      result:=FSocks[snum].GetMessage(s);
end;

function TLTcp.SendMessage(const msg: string): Boolean;
var
  i: LongInt;
  j: Longint;
begin
  Result:=True;
  if (Length(msg) > 0) and (FSocks.Count > 0) then
    for i:=FSocks.Count-1 downto 0 do begin
      if Length(msg) > FBufferSize then begin
        for j:=0 to (Length(msg)-1) div FBufferSize do
          if not FSocks[i].Send(Copy(msg, j * FBufferSize + 1, FBufferSize)) then
            Result:=False;
      end else Result:=FSocks[i].Send(msg);
    end;
end;

function TLTcp.SendMessage(const msg: string; const snum: LongInt = -1): Boolean;
var
  i, j: LongInt;
begin
  if (Length(msg) > 0) and (FSocks.Count > 0) then
    if Snum >= 0 then
      for i:=FSocks.Count-1 Downto 0 do
        if FSocks[i].FSnum = snum then begin
          Result:=True;
          if Length(msg) > FBufferSize then begin
            for j:=0 to (Length(msg)-1) div FBufferSize do
              if not FSocks[i].Send(Copy(msg, j * FBufferSize + 1, FBufferSize)) then
                Result:=False;
          end else Result:=FSocks[i].Send(msg);
          Break;
        end;
end;

//********************************TLSocket*************************************

constructor TLSocket.Create(const protocol: Byte; Owner: TLConnection; const num: LongInt = 0);
begin
  FConnected:=False;
  FBuffer:=TStringList.Create;
  FMaxMsgs:=DefaultMaxMsgs;
  FAddrlen:=Sizeof(FAddr);
  FParent:=Owner;
  if Assigned(FParent) then
    begin
      FBufferSize:=FParent.FBufferSize;
      FMaxMsgs:=FParent.FMaxMsgs;
    end else
    begin
      FBufferSize:=DefaultBufferSize;
      FMaxMsgs:=DefaultMaxMsgs;
    end;
  FSnum:=num;
  case protocol of
    LTCP: FFlag:=SOCK_STREAM;
    LUDP: FFlag:=SOCK_DGRAM;
  end;
end;

constructor TLSocket.Create(const protocol: Byte; const num: LongInt = 0);
begin
  FConnected:=false;
  FBuffer:=TstringList.Create;
  FMaxMsgs:=DefaultMaxMsgs;
  FbufferSize:=DefaultBufferSize;
  FSnum:=num;
  case protocol of
    LTCP: FFlag:=SOCK_STREAM;
    LUDP: FFlag:=SOCK_DGRAM;
  end;
end;

destructor TLSocket.Destroy;
begin
  Disconnect;
  FBuffer.Free;
end;

procedure TLSocket.Disconnect;
begin
  if Connected then begin
    ShutDown(FSock, 2);// <> 0 then logerror('ShutDown error', socketerror);
    if Closesocket(FSock) <> 0 then logerror('Closesocket error', socketerror);
    FConnected:=false;
  end;
end;

procedure TLSocket.SetBufferSize(const Size: LongInt);
begin
  if  (Size > 4)
  and (Size <= DefaultBufferSize) then FbufferSize:=Size;
end;

procedure TLSocket.LogError(const msg: string; const ernum: LongInt);
begin
  if Assigned(FParent) then
    if (ernum > 0) and Assigned(FParent.FOnError) then
      FParent.FOnError(msg+' '+IntToStr(ernum), FSnum)
        else if Assigned(FParent.FOnDisconnect) then
          FParent.FOnDisconnect(msg, FSnum);
end;

procedure TLSocket.Bail(const msg: string; const ernum: LongInt);
begin
  Disconnect;
  LogError(msg, ernum);
end;

function TLSocket.GetHostAddress: string;
begin
  Result:='';
  if FFlag = SOCK_STREAM then
    Result:=HostAddrtoStr(FAddr.Addr)
      else Result:=HostAddrtoStr(FCliAddr.Addr);
end;

function TLSocket.GetMessage(out msg: string): Boolean;
begin
  msg:='';
  result:=false;
  if FBuffer.Count > 0 then begin
    while FBuffer.Count > 0 do begin
      msg:=msg + FBuffer[0];
      FBuffer.Delete(0);
    end;
    FBuffer.Clear;
    result:=True;
  end;
end;

function TLSocket.SetupSocket(const APort: Word; const Address: string): Boolean;
var
  Done: Boolean;
begin
  Result:=false;
  if not Connected then
    begin
      Done:=true;
      FSock:=fpsocket(AF_INET, FFlag, 0);
      FSockPort:=APort;
      if FSock < 0 then bail('Socket error', SocketError);
      SetSocketOptions(FSock, SOL_SOCKET, SO_REUSEADDR, 'TRUE', Length('TRUE'));
      if FFlag = SOCK_DGRAM then
        SetSocketOptions(FSock, SOL_SOCKET, SO_BROADCAST, 'TRUE', Length('TRUE'));
      FAddr.family:=AF_INET;
      FAddr.Port:=htons(APort);
      FAddr.Addr:=StrToNetAddr(Address);
      if (Address <> LADDR_ANY) and (FAddr.Addr = 0) then
        FAddr.Addr:=StrToNetAddr(GetHostIP(Address));
      Result:=Done;
    end;
end;

function TLSocket.SetServer(const APort: Word): Boolean;
begin
  if not Connected then
    begin
      Result:=false;
      SetUpSocket(APort, LADDR_ANY);
      if fpBind(FSock, @FAddr, FAddrLen) < 0 then
        Bail('Error on bind', SocketError) else Result:=true;
      if (FFlag = SOCK_STREAM) and Result then
        if fpListen(FSock, 5) < 0 then
          Bail('Error on Listen', SocketError) else Result:=true;
      {$ifdef nonblocking}
       if Result then SetNonBlock;
      {$endif}
    end;
end;

function TLSocket.Accept(const sersock: LongInt): Boolean;
begin
  Result:=false;
  if not Connected then
    begin
      FSock:=fpAccept(sersock, @FAddr, @FAddrlen);
      if FSock > -1 then
        begin
         {$ifdef nonblocking}
          SetNonBlock;
         {$endif}
          Result:=true;
          FConnected:=true;
        end else Result:=false;
    end;
end;

function TLSocket.Connect(const APort: Word; const Address: string): Boolean;
begin
  Result:=False;
  if FConnected then Disconnect;
  if SetupSocket(APort, Address) then
    if fpConnect(FSock, @FAddr, FAddrlen) = 0 then
      begin
        FConnected:=true;
        {$ifdef nonblocking}
         Setnonblock;
        {$endif}
        Result:=FConnected;
      end else bail('Error on Connect', SocketError);
end;

function TLSocket.Send(const msg: string): Boolean;
var n: LongInt;
    Temp: string;
   {$ifdef win32}
    S: string;
   {$endif}
begin
  Result:=false;
  if Length(msg) > FBufferSize then
    Bail('Send error: Message bigger than buffersize', -1);
  if Connected then begin
    Writeln('INNER SEND: ', msg);
    Temp:=msg;
    if FFlag = SOCK_STREAM then
    {$ifdef win32}
      n:=tomwinsock.send(FSock, Temp[1], Length(Temp), LMSG)
    else begin
        s:=Temp;
        n:=tomwinsock.sendto(FSock, s[1], Length(Temp), LMSG, TSockAddrIn(FCliAddr), FAddrlen);
    end; // win32 sendto is totaly shitty with sockets, I have to use winsock
    {$else}
      n:=sockets.send(FSock, Temp[1], Length(Temp), LMSG)
    else
      n:=sockets.sendto(FSock, Temp[1], Length(Temp), LMSG, FCliAddr, FAddrlen);
    {$endif}
    Writeln('INNER RESULT: ', n);
    if n < 0 then Bail('Send error', socketerror);
    if n > 0 then Result:=true;
  end;
end;

function TLSocket.Recieve: Boolean;
var
  n: LongInt;
  s: string;
begin
  if Connected then
    begin
      SetLength(s, FbufferSize);
      if FFlag = SOCK_STREAM then
        {$ifdef win32}
        n:=tomwinsock.Recv(FSock, s[1], FbufferSize, LMSG)
          else
            n:=tomwinsock.Recvfrom(FSock, s[1], FBufferSize, LMSG, TSockAddrIn(FCliAddr), FAddrlen);
        {$else}
        n:=sockets.Recvfrom(FSock, s[1], FBufferSize, LMSG, FCliAddr, FAddrlen)
          else
            n:=sockets.Recvfrom(FSock, s[1], FBufferSize, LMSG, FCliAddr, FAddrlen);
        {$endif}
      if n = 0 then Bail('Lost Connection', -1);
      if n < 0 then Bail('Recieve Error', SocketError);
      if Connected then begin
        if (n <= FBufferSize) then begin
        
          SetLength(s, n);
          if FBuffer.Count < FMaxMsgs then
            FBuffer.Add(s)
          else
            Bail('Buffer full', -1);
            
        end else Bail('Buffer overrun!', -1);
      end;
    end;
  Result:=Connected;
end;

procedure TLSocket.SetNonBlock;
var opt: LongInt;
begin
  {$ifdef win32}
   opt:=1;
   if ioctlsocket(FSock, DWORD(FIONBIO), opt) < 0 then
     bail('Error on SetFL', wsaGetLasterror);
  {$else}
   opt:=fpfcntl(FSock, F_GETFL);
   if opt<0 then bail('ERROR on GetFD', socketerror);
   if fpfcntl(FSock, F_SETFL, opt or O_NONBLOCK) < 0 then
     bail('Error on SetFL', socketerror);
  {$endif}
end;

initialization
// {$ifdef unix} fpsignal(sigpipe, signalhandler(sig_ign)); {$endif}
  Connections:=TLConnectionList.Create(False);

finalization
  Connections.Free;

end.

