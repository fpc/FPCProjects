{ lNet v2.2.0

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
  
  This license has been modified. See File LICENSE.ADDON for more inFormation.
  Should you find these sources without a LICENSE File, please contact
  me at ales@chello.sk
}

{ TODO LIST

  1. Optimize "max" in CallSelect in TLTcp

}

unit lNet;

{$mode objfpc}{$H+}
//{$define nonblocking}

interface

uses
  {$ifdef unix} BaseUnix, NetDB, {$else} TomWinsock, {$endif}
  SysUtils, Contnrs, Sockets, Classes;

const
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
  { Base socket class, Holds Address and socket info, perForms basic
    socket operations, each has it's own Data buffer for Receives }
  TLBaseSocket = class
   protected
    FBuffer: TStringList;
    FAddr: TInetSockAddr;
    FCliAddr: TInetSockAddr;
    FConnected: Boolean;
    FBufferSize: Longint;
    FMaxmsgs: Longint;
    FAddrlen: Longint;
    FSock: Longint;
    FFlag: Longint;
    FProt: Longint;
    FSockPort: Word;
   protected
    function SetupSocket(const APort: Word; const Address: string): Boolean; virtual;
    function GetMessageCount: Integer;
    function GetHostAddress: string;
    procedure SetNonBlock(const Value: Boolean);
    procedure SetBufferSize(const Size: LongInt);
    procedure Bail(const msg: string; const ernum: LongInt);
    procedure LogError(const msg: string; const ernum: LongInt); virtual;
   public
    constructor Create(const stype, protocol: Byte); virtual;
    destructor Destroy; override;
    function SetServer(const APort: Word): Boolean;
    function Accept(const SerSock: LongInt): Boolean; virtual;
    function Connect(const APort: Word; const Address: string): Boolean; virtual;
    function Send(const msg: string): Boolean;
    function Receive: Boolean;
    function GetMessage(out msg: string): Boolean;
    procedure Disconnect; virtual;
   public
    property BufferSize: LongInt read FBufferSize write SetBufferSize;
    property Port: Word read FSockPort;
    property MaxMsgs: LongInt read FMaxMsgs write FMaxMsgs;
    property Connected: Boolean read FConnected;
    property MessageCount: Integer read GetMessageCount;
    property HostAddress: string read GetHostAddress;
  end;

  TLConnection = class;

  { this is the socket used by TLConnection }
  TLSocket = class(TLBaseSocket)
   protected
    FParent: TLConnection;
    FPrev: TLSocket;
    FNext: TLSocket;
    FSnum: LongInt;
    procedure LogError(const msg: string; const ernum: LongInt); override;
   public
    constructor Create(const sType, Protocol: Byte; Owner: TLConnection;
                       const aSocketNumber: Integer); virtual; overload;
    property SNum: LongInt read FSnum;
    property Prev: TLSocket read FPrev write FPrev;
    property Next: TLSocket read FNext write FNext;
  end;
  
  TLSocketClass = class of TLSocket;

  { CallBack Event procedure }
  TLObjProc = procedure(const msg: string; aSocket: TLSocket) of object;

  { Common ancestor for TLTcp and TLUdp classes. Holds Event properties
    and common variables }
  TLConnection = class
   protected
    FBlocking: Boolean;
    FSerSock: TLSocket;
    FBufferSize: LongInt;
    FTimeVal: TTimeVal;
    FMaxMsgs: LongInt;
    FOnReceive: TLObjProc;
    FOnError: TLObjProc;
    FOnAccept: TLObjProc;
    FOnDisconnect: TLObjProc;
    FForceUTF8: Boolean;
    FSocketType: TLSocketClass;
    FID: Integer; // internal number for server
    function GetItem(const i: Longint): TLSocket;
    function GetBlockTime: DWord;
    procedure SetBlockTime(const Value: DWord);
    procedure SetBufferSize(const Size: LongInt);
    procedure SetMaxMsgs(const Size: LongInt);
   public
    constructor Create(SocketClass: TLSocketClass = nil);
    destructor Destroy; override;
    function Connect(const APort: Word; const Address: string): Boolean; virtual; abstract;
    function Listen(const APort: Word): Boolean; virtual; abstract;
    function SendMessage(const msg: string; aSocket: TLSocket = nil): Boolean; virtual; abstract;
    function Disconnect(aSocket: TLSocket = nil): Boolean; virtual; abstract;
    function ControlSock(aSocket: TLSocket; const Check: Boolean = True): Boolean; virtual; abstract;
    procedure CallAction; virtual; abstract;
   public
    property OnAccept: TLObjProc read FOnAccept write FOnAccept;
    property OnError: TLObjProc read FOnError write FOnError;
    property OnReceive: TLObjProc read FOnReceive write FOnReceive;
    property OnDisconnect: TLObjProc read FOnDisconnect write FOnDisconnect;
    property BufferSize: LongInt read FBufferSize write SetBufferSize;
    property MaxMsgs: LongInt read FMaxMsgs write SetMaxMsgs;
    property BlockTime: DWord read GetBlockTime write SetBlockTime;
    property ForceUTF8: Boolean read FForceUTF8 write FForceUTF8;
    property Socks[index: Longint]: TLSocket read GetItem; default;
  end;
  
  { UDP Client/Server class. Provided to enable usage of UDP sockets }
  TLUdp = class(TLConnection)
   protected
    function GetMessageCount: Cardinal;
    procedure Bail(const msg: string);
   public
    constructor Create(SocketClass: TLSocketClass = nil);
    destructor Destroy; override;
    function Connect(const APort: Word; const Address: string = LADDR_BR): Boolean; override;
    function Listen(const APort: Word): Boolean; override;
    function GetMessage(out s: string): Boolean;
    function GetHostAddress: string;
    function SendMessage(const msg: string; aSocket: TLSocket = nil): Boolean; override;
    function SendMessage(const msg: string; const Address: string): Boolean;
    function Disconnect(aSocket: TLSocket = nil): Boolean; override;
    function ControlSock(aSocket: TLSocket; const Check: Boolean = True): Boolean; override;
    procedure CallAction; override;
    property MessageCount: Cardinal read GetMessageCount;
  end;
  
  { TCP Client/Server class. Provided to enable usage of TCP sockets }
  TLTcp = class(TLConnection)
   protected
    FLast: TLSocket;
    FCount: Integer;
    FSMax: Integer;
    FAccepting: Boolean;
    FMaxSockets: Integer;
    FReadFDSet: TFDSet;
    procedure SetMaxSockets(const Value: Integer);
    procedure Bail(const msg: string; aSocket: TLSocket);
   public
    constructor Create(SocketClass: TLSocketClass = nil);
    destructor Destroy; override;
    function Connect(const APort: Word; const Address: string): Boolean; override;
    function Listen(const APort: Word): Boolean; override;
    function SendMessage(const msg: string; aSocket: TLSocket = nil): Boolean; override;
    function Disconnect(aSocket: TLSocket = nil): Boolean; override;
    function CallReceive(aSocket: TLSocket = nil): Boolean;
    function ControlSock(aSocket: TLSocket; const Check: Boolean = True): Boolean; override;
    procedure CallAccept(const Check: Boolean = True);
    procedure CallAction; override;
   public
    property Count: Integer read FCount;
    property Accepting: Boolean read FAccepting write FAccepting;
    property MaxSockets: Integer read FMaxSockets write SetMaxSockets;
  end;
  
  {$i tlconnectionlisth.inc}
  
  { Common functions}
  procedure CallConnections;

  { Base functions }
  function StrToHostAddr(IP: string): Cardinal;
  function HostAddrToStr(Entry: Cardinal): string;
  function StrToNetAddr(IP: string): Cardinal;
  function NetAddrToStr(Entry: Cardinal): string;
  function DataToString(const Data; const Size: Cardinal): string;
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

uses
{$ifndef win32}
  Errors;
  
  function StrError(const Ernum: Longint; const UseUTF8: Boolean = False): string;
  begin
    Result:='[' + IntToStr(Ernum) + '] ' + Errors.StrError(Ernum);
  end;
  
{$else}
  Windows;

  function StrError(const Ernum: Longint; const UseUTF8: Boolean = False): string;
  var
    Tmp, TmpW: string;
    i: Integer;
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
      SetLength(Tmp, FormatMessageA(FORMAT_MESSAGE_FROM_SYSTEM or
                                    FORMAT_MESSAGE_IGNORE_INSERTS or
                                    FORMAT_MESSAGE_ARGUMENT_ARRAY,
                                    nil, Ernum, 0, @Tmp[1], 256, nil));
    end;
    if Length(Tmp) > 2 then
      Result:=Result + Copy(Tmp, 1, Length(Tmp)-2);
  end;
{$endif}

var
  Connections: TLConnectionList;

{$i lfunc.inc}  // the borrowed functions from inet.pp
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

constructor TLConnection.Create(SocketClass: TLSocketClass = nil);
begin
  Connections.Add(Self);
  if Assigned(SocketClass) then
    FSocketType:=SocketClass
  else
    FSocketType:=TLSocket;
  FBlocking:=False;
  FTimeVal.tv_sec:=0;
  FTimeVal.tv_usec:=0;
  FForceUTF8:=False;
end;

destructor TLConnection.Destroy;
begin
  Connections.Remove(Self);
end;

function TLConnection.GetItem(const i: longint): TLSocket;
var
  Tmp: TLSocket;
begin
  Result:=nil;
  Tmp:=FSerSock;
  while Assigned(Tmp.Next) and (Tmp.SNum <> i) do
    Tmp:=Tmp.Next;
  if Tmp.SNum = i then
    Result:=Tmp;
end;

function TLConnection.GetBlockTime: DWord;
begin
  Result:=(FTimeVal.tv_sec * 1000) + (FTimeVal.tv_usec div 1000);
end;

procedure TLConnection.SetBlockTime(const Value: DWord);
begin
  FTimeVal.tv_sec:=Value div 1000;
  FTimeVal.tv_usec:=(Value mod 1000) * 1000;
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

constructor TLUdp.Create(SocketClass: TLSocketClass = nil);
begin
  inherited Create(SocketClass);
  FTimeVal.tv_usec:=0;
  FTimeVal.tv_sec:=0;
  FBufferSize:=DefaultBufferSize;
  FMaxMsgs:=DefaultMaxMsgs;
  FOnReceive:=nil; FOnAccept:=nil; FOnError:=nil;
  FSerSock:=FSocketType.Create(SOCK_DGRAM, 17, Self, 1);
end;

destructor TLUdp.Destroy;
begin
  Disconnect;
  FSerSock.Free;
  inherited Destroy;
end;

function TLUdp.Disconnect(aSocket: TLSocket = nil): Boolean;
begin
  Result:=True;
  FSersock.Disconnect;
end;

function TLUdp.Listen(const APort: Word): Boolean;
begin
  Result:=False;
  if FSerSock.Connected then
    Disconnect;
  if FSerSock.SetServer(APort) then begin
    FSersock.FCliAddr:=FSerSock.FAddr;
    FSersock.FCliAddr.Addr:=StrToNetAddr(LADDR_BR);
    FSersock.FConnected:=True;
  end;
  Result:=FSerSock.Connected;
  {$ifdef nonblocking}
  if Result then FSerSock.SetNonBlock;
  {$endif}
end;

function TLUdp.Connect(const APort: Word; const Address: string): Boolean;
begin
  Result:=False;
  if FSerSock.Connected then Disconnect;
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
    FOnError(msg, FSerSock);
end;

function TLUdp.GetHostAddress: string;
begin
  Result:='';
  Result:=FSerSock.GetHostAddress;
end;

function TLUdp.ControlSock(aSocket: TLSocket; const Check: Boolean = True): Boolean;
var
  s: string;
  FDSet: TFDSet;
  n: Longint;
  TempVal: TTimeVal;

  procedure CheckSock;
  begin
    if FSerSock.Receive and GetMessage(s) then begin
      if Assigned(FOnReceive) then
        FOnReceive(s, FSerSock);
      Result:=True;
    end;
  end;

begin
  Result:=False;
  TempVal:=FTimeVal;
  if Check then begin
    fpFD_ZERO(FDSet);
    fpFD_SET(FSerSock.FSock, FDSet);
    n:=fpSelect(FSerSock.FSock+1, @FDSet, nil, nil, @TempVal);
    if n < 0 then
      Bail('Error on select');
    if n > 0 then
      CheckSock;
  end else
    CheckSock;
end;

procedure TLUdp.CallAction;
begin
  if FSerSock.Connected then
    ControlSock(FSerSock);
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

function TLUdp.SendMessage(const msg: string; aSocket: TLSocket = nil): Boolean;
begin
  Result:=False;
  if (Length(msg) > 0) and (Length(msg) < FBufferSize) then
    Result:=FSerSock.Send(msg)
  else
    Bail('Message too long');
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

constructor TLTcp.Create(SocketClass: TLSocketClass = nil);
begin
  inherited Create(SocketClass);
  FCount:=0;
  FSmax:=1;
  FBufferSize:=DefaultBufferSize;
  FMaxMsgs:=DefaultMaxMsgs;
  FMaxSockets:=DefaultMaxSockets;
  FOnReceive:=nil; FOnAccept:=nil; FOnError:=nil;
  FAccepting:=false;
  FSerSock:=FSocketType.Create(SOCK_STREAM, 6, Self, 0);
  FLast:=FSerSock;
end;

destructor TLTcp.Destroy;
begin
  Disconnect;
  FSerSock.Free;
  inherited Destroy;
end;

function TLTcp.Disconnect(aSocket: TLSocket = nil): Boolean;
var
  Tmp, Tmp2: TLSocket;
begin
  Result:=False;
  if not Assigned(aSocket) then begin
    FAccepting:=False;
    Tmp:=FSerSock;
    Tmp2:=FSerSock.Next;
    while Assigned(Tmp2) do begin
      Tmp:=Tmp2;
      Tmp2:=Tmp.Next;
      Tmp.Free;
    end;
    FSerSock.Next:=nil;
    FSerSock.Disconnect;
    FLast:=FSerSock;
    Result:=True;
    FCount:=0;
  end else begin
    if aSocket <> FSerSock then begin
      if aSocket = FLast then
        FLast:=aSocket.Prev;
      aSocket.Prev.Next:=aSocket.Next;
      if Assigned(aSocket.Next) then
        aSocket.Next.Prev:=aSocket.Prev;
      aSocket.Free;
      Dec(FCount);
    end else FSerSock.Disconnect;
    Result:=True;
  end;
end;

procedure TLTcp.Bail(const msg: string; aSocket: TLSocket);
begin
  if Assigned(aSocket) then
    Disconnect(aSocket) else Disconnect;
  if Assigned(FOnError) then
    FOnError(msg, aSocket);
end;

function TLTcp.ControlSock(aSocket: TLSocket; const Check: Boolean = True): Boolean;
var
  s: string;
begin
  Result:=False;
  if (not Check) or (fpFD_ISSET(aSocket.FSock, FReadFDSet) <> 0) then
    with aSocket do
      if Receive then begin
        while aSocket.GetMessage(s) do
          if Assigned(FOnReceive) then FOnReceive(s, aSocket);
        Result:=True;
      end else Self.Disconnect(aSocket);
end;

function TLTcp.Listen(const APort: Word): Boolean;
begin
  Result:=false;
  if not FAccepting then begin
    if Assigned(FSerSock.Next) then Disconnect;
    if FSerSock.SetServer(APort) then begin
      FSerSock.FConnected:=true;
      FAccepting:=true;
      Result:=true;
    end;
  end else Bail('Error, already listening', nil);
end;

function TLTcp.Connect(const APort: Word; const Address: string): Boolean;
begin
  Result:=False;
  if Assigned(FSerSock.Next) then Disconnect;
  FSersock.Next:=FSocketType.Create(SOCK_STREAM, 6, Self, 1);
  FLast:=FSerSock.Next;
  FLast.Prev:=FSerSock;
  Result:=FLast.Connect(aPort, Address);
  if Result then
    Inc(FCount)
  else begin
    FLast.Free;
    FLast:=FSerSock;
    FSerSock.Next:=nil;
  end;
end;

procedure TLTcp.CallAction;

  function CallSelect: Boolean;
  var
    i: Longint;
    max: Longint;
    TempVal: TTimeVal;
    Tmp: TLSocket;
  begin
    result:=False;
    TempVal:=FTimeVal;
    max:=0;
    fpFD_ZERO(FReadFDSet);
    if FAccepting and FSerSock.Connected then begin
      max:=FSerSock.FSock;
      fpFD_SET(FSerSock.FSock, FReadFDSet);
    end;
    if Assigned(FSerSock.Next) then begin
      Tmp:=FSerSock.Next;
      repeat
        if Tmp.Connected then begin
          fpFD_SET(Tmp.FSock, FReadFDSet);
          if Tmp.FSock > max then
            max:=Tmp.FSock;
          Tmp:=Tmp.Next;
        end else begin
          Tmp:=Tmp.Next;
          Disconnect(Tmp);
        end;
      until not Assigned(Tmp);
    end;
    if max > 0 then
      i:=fpSelect(max+1, @FReadFDSet, nil, nil, @TempVal)
    else
      i:=0;
    if i < 0 then
      Bail('Error on select', nil);
    if i = 0 then result:=False;
    if i > 0 then result:=True;
  end;
  
begin
  if (FSerSock.Connected or Assigned(FSerSock.Next)) and CallSelect then begin
    CallReceive;
    if FAccepting then CallAccept;
  end;
end;

function TLTcp.CallReceive(aSocket: TLSocket = nil): Boolean;
var
  Tmp: TLSocket;
begin
  Result:=False;
  if not Assigned(aSocket) then begin
    if Assigned(FSerSock.Next) then begin
      Tmp:=FSerSock.Next;
      repeat
        Result:=ControlSock(Tmp);
        Tmp:=Tmp.Next;
      until not Assigned(Tmp);
    end;
  end else
    Result:=ControlSock(aSocket);
end;

procedure TLTcp.SetMaxSockets(const Value: Integer);
begin
  if Value > 0 then
    FMaxSockets:=Value;
end;

procedure TLTcp.CallAccept(const Check: Boolean = True);
begin
  if ((not Check) or (fpFD_ISSET(FSerSock.FSock, FReadFDSet) <> 0))
  and (Count < MaxSockets) then begin
    FLast.Next:=FSocketType.Create(SOCK_STREAM, 6, Self, FSmax);
    if FLast.Next.Accept(FSerSock.FSock) then begin
      FLast.Next.Prev:=FLast;
      FLast:=FLast.Next;
      Inc(FCount);
      Inc(FSmax);
      if Assigned(FOnAccept) then
        FOnAccept('Connection Accepted from ' + FLast.GetHostAddress, FLast);
    end else begin
      FLast.Next.Free;
      FLast.Next:=nil;
    end;
  end;
end;

function TLTcp.SendMessage(const msg: string; aSocket: TLSocket = nil): Boolean;

  function SendParts(Dest: TLSocket): Boolean;
  var
    j: LongInt;
  begin
    Result:=True;
    if Length(msg) <= FBufferSize then
      Result:=Dest.Send(msg)
    else
      for j:=0 to (Length(msg)-1) div FBufferSize do
        if not Dest.Send(Copy(msg, j * FBufferSize + 1, FBufferSize)) then
          Result:=False;
  end;
  
var
  Tmp: TLSocket;
begin
  Result:=False;
  if (Length(msg) > 0) and (Count > 0) then
    if Assigned(aSocket) then
      Result:=SendParts(aSocket)
    else begin
      Result:=True;
      Tmp:=FSerSock.Next;
      while Assigned(Tmp) do begin
        if not SendParts(Tmp) then
          Result:=False;
        Tmp:=Tmp.Next;
      end;
    end;
end;

//********************************TLBaseSocket*************************************

constructor TLBaseSocket.Create(const stype, protocol: Byte);
begin
  FConnected:=false;
  FBuffer:=TstringList.Create;
  FMaxMsgs:=DefaultMaxMsgs;
  FAddrlen:=Sizeof(FAddr);
  FBufferSize:=DefaultBufferSize;
  FFlag:=stype;
  FProt:=protocol;
end;

destructor TLBaseSocket.Destroy;
begin
  Disconnect;
  FBuffer.Free;
end;

procedure TLBaseSocket.Disconnect;
begin
  if Connected then begin
    if FFlag = SOCK_STREAM then
      if ShutDown(FSock, 2) <> 0 then
        LogError('Shutdown error', SocketError);
    if Closesocket(FSock) <> 0 then
      LogError('Closesocket error', SocketError);
    FConnected:=false;
  end;
end;

procedure TLBaseSocket.SetBufferSize(const Size: LongInt);
begin
  if  (Size > 4)
  and (Size <= DefaultBufferSize) then FbufferSize:=Size;
end;

procedure TLBaseSocket.LogError(const msg: string; const ernum: LongInt);
begin
end;

procedure TLBaseSocket.Bail(const msg: string; const ernum: LongInt);
begin
  LogError(msg, ernum);
  Disconnect;
end;

function TLBaseSocket.GetHostAddress: string;
begin
  Result:='';
  if FFlag = SOCK_STREAM then
    Result:=HostAddrtoStr(FAddr.Addr)
      else Result:=HostAddrtoStr(FCliAddr.Addr);
end;

function TLBaseSocket.GetMessage(out msg: string): Boolean;
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

function TLBaseSocket.SetupSocket(const APort: Word; const Address: string): Boolean;
var
  Done: Boolean;
begin
  Result:=false;
  if not Connected then begin
    Done:=true;
    FSock:=fpsocket(AF_INET, FFlag, FProt);
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

function TLBaseSocket.GetMessageCount: Integer;
begin
  Result:=FBuffer.Count;
end;

function TLBaseSocket.SetServer(const APort: Word): Boolean;
begin
  if not Connected then begin
    Result:=false;
    SetupSocket(APort, LADDR_ANY);
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

function TLBaseSocket.Accept(const sersock: LongInt): Boolean;
begin
  Result:=false;
  if not Connected then begin
    FSock:=fpAccept(sersock, @FAddr, @FAddrlen);
    if FSock > -1 then begin
     {$ifdef nonblocking}
      SetNonBlock;
     {$endif}
      Result:=true;
      FConnected:=true;
    end;
  end;
end;

function TLBaseSocket.Connect(const APort: Word; const Address: string): Boolean;
begin
  Result:=False;
  if FConnected then Disconnect;
  if SetupSocket(APort, Address) then
    if fpConnect(FSock, @FAddr, FAddrlen) = 0 then begin
      FConnected:=true;
      {$ifdef nonblocking}
       Setnonblock;
      {$endif}
      Result:=FConnected;
    end else bail('Error on Connect', SocketError);
end;

function TLBaseSocket.Send(const msg: string): Boolean;
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
    if n < 0 then Bail('Send error', socketerror);
    if n > 0 then Result:=true;
  end;
end;

function TLBaseSocket.Receive: Boolean;
var
  n: LongInt;
  s: string;
begin
  if Connected then begin
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
    if n < 0 then Bail('Receive Error', SocketError);
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

procedure TLBaseSocket.SetNonBlock(const Value: Boolean);
var opt: LongInt;
begin
  {$ifdef win32}
   if Value then
     opt:=1
   else
     opt:=0;
   if ioctlsocket(FSock, Integer(FIONBIO), opt) < 0 then
     bail('Error on SetFL', wsaGetLasterror);
  {$else}
   opt:=fpfcntl(FSock, F_GETFL);
   if opt < 0 then bail('ERROR on GetFD', socketerror);

   if Value then begin
     if fpfcntl(FSock, F_SETFL, opt or O_NONBLOCK) < 0 then
       bail('Error on SetFL', socketerror);
   end else
     if fpfcntl(FSock, F_SETFL, opt and (not O_NONBLOCK)) < 0 then
       bail('Error on SetFL', socketerror);
  {$endif}
end;

//********************************TLSocket***********************************

constructor TLSocket.Create(const sType, Protocol: Byte; Owner: TLConnection;
                            const aSocketNumber: Integer);
begin
  inherited Create(stype, protocol);
  FParent:=Owner;
  if Assigned(FParent) then begin
    FBufferSize:=FParent.FBufferSize;
    FMaxMsgs:=FParent.FMaxMsgs;
  end;
  FSnum:=aSocketNumber;
  FPrev:=nil;
  FNext:=nil;
end;

procedure TLSocket.LogError(const msg: string; const ernum: Integer);
begin
  if Assigned(FParent) then
    if (ernum > 0) and Assigned(FParent.FOnError) then
      FParent.FOnError(msg + ': ' + StrError(ernum, FParent.ForceUTF8), Self)
    else if Assigned(FParent.FOnDisconnect) then
      FParent.FOnDisconnect(msg, Self);
end;

initialization
  Connections:=TLConnectionList.Create(False);

finalization
  Connections.Free;

end.

