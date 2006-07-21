{ lNet v2.4.1

  CopyRight (C) 2004-2006 Ales Katona

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

unit lNet;

{$mode objfpc}{$H+}
{$interfaces corba}

interface

uses
  Classes, lEvents,
  {$i sys/osunits.inc}

const
  { Address constants }
  LADDR_ANY = '0.0.0.0';
  LADDR_BR  = '255.255.255.255';
  LADDR_LO  = '127.0.0.1';
  { ICMP }
  LICMP_ECHOREPLY     = 0;
  LICMP_UNREACH       = 3;
  LICMP_ECHO          = 8;
  LICMP_TIME_EXCEEDED = 11;
  { Protocols }
  LPROTO_IP     =     0;
  LPROTO_ICMP   =     1;
  LPROTO_IGMP   =     2;
  LPROTO_TCP    =     6;
  LPROTO_UDP    =    17;
  LPROTO_IPV6   =    41;
  LPROTO_ICMPV6 =    58;
  LPROTO_RAW    =   255;
  LPROTO_MAX    =   256;

type
  PLIPHeader = ^TLIPHeader;
  TLIPHeader = record
      VerLen      : Byte;
      TOS         : Byte;
      TotalLen    : Word;
      Identifer   : Word;
      FragOffsets : Word;
      TTL         : Byte;
      Protocol    : Byte;
      CheckSum    : Word;
      SourceIp    : DWord;
      DestIp      : DWord;
      Options     : DWord;
  end;  // TLIPHeader


  TLSocket = class;
  
  { CallBack Event procedure for errors }
  TLErrorProc = procedure(const msg: string; aSocket: TLSocket) of object;

  { CallBack Event procedure for others }
  TLProc = procedure(aSocket: TLSocket) of object;

  { Base socket class, Holds Address and socket info, perForms basic
    socket operations, uses select always to figure out if it can work (slow) }

  { TLSocket }

  TLSocket = class(TLHandle)
   protected
    FAddress: TInetSockAddr;
    FPeerAddress: TInetSockAddr;
    FConnected: Boolean;
    FConnecting: Boolean;
    FSocketClass: Integer;
    FProtocol: Integer;
    FNextSock: TLSocket;
    FPrevSock: TLSocket;
    FIgnoreShutdown: Boolean;
    FCanSend: Boolean;
    FCanReceive: Boolean;
    FServerSocket: Boolean;
    FOnFree: TLProc;
   protected
    function DoSend(const TheData; const TheSize: Integer): Integer;
    function SetupSocket(const APort: Word; const Address: string): Boolean; virtual;
    function GetLocalPort: Word;
    function GetPeerPort: Word;
    function GetPeerAddress: string;
    function GetLocalAddress: string;
    function CanSend: Boolean; virtual;
    function CanReceive: Boolean; virtual;
    procedure SetOptions; virtual;
    procedure SetNonBlock; virtual;
    procedure Bail(const msg: string; const ernum: Integer);
    procedure LogError(const msg: string; const ernum: Integer); virtual;
   public
    constructor Create; override;
    destructor Destroy; override;
    function Listen(const APort: Word; const AIntf: string = LADDR_ANY): Boolean;
    function Accept(const SerSock: Integer): Boolean;
    function Connect(const Address: string; const APort: Word): Boolean;
    function Send(const aData; const aSize: Integer): Integer; virtual;
    function SendMessage(const msg: string): Integer;
    function Get(var aData; const aSize: Integer): Integer; virtual;
    function GetMessage(out msg: string): Integer;
    procedure Disconnect; virtual;
   public
    property Connected: Boolean read FConnected;
    property Connecting: Boolean read FConnecting;
    property Protocol: Integer read FProtocol write FProtocol;
    property SocketType: Integer read FSocketClass write FSocketClass;
    property PeerAddress: string read GetPeerAddress;
    property PeerPort: Word read GetPeerPort;
    property LocalAddress: string read GetLocalAddress;
    property LocalPort: Word read GetLocalPort;
    property NextSock: TLSocket read FNextSock write FNextSock;
    property PrevSock: TLSocket read FPrevSock write FPrevSock;
  end;
  TLSocketClass = class of TLSocket;

  { this is the socket used by TLConnection }
  
  TLActionEnum = (acConnect, acAccept, acSend, acReceive, acError);

  { Base interface common to ALL connections }
  
  ILBase = interface
    function Get(var aData; const aSize: Integer; aSocket: TLSocket = nil): Integer;
    function GetMessage(out msg: string; aSocket: TLSocket = nil): Integer;
    function Send(const aData; const aSize: Integer; aSocket: TLSocket = nil): Integer;
    function SendMessage(const msg: string; aSocket: TLSocket = nil): Integer;
    procedure Disconnect;
    procedure CallAction;
    property SocketClass: TLSocketClass;
  end;
  
  { Interface for all servers }
  
  ILServer = interface(ILBase)
    function Listen(const APort: Word; const AIntf: string = LADDR_ANY): Boolean;
  end;

  { Interface for all clients }
  
  ILClient = interface(ILBase)
    function Connect(const Address: string; const APort: Word): Boolean;
  end;
  
  { TLConnection
    Common ancestor for TLBaseTcp and TLUdp classes. Holds Event properties
    and common variables. }

  TLConnection = class(TComponent, ILServer, ILClient)
   protected
    FTimeVal: TTimeVal;
    FOnReceive: TLProc;
    FOnAccept: TLProc;
    FOnConnect: TLProc;
    FOnDisconnect: TLProc;
    FOnCanSend: TLProc;
    FOnError: TLErrorProc;
    FSocketClass: TLSocketClass;
    FRootSock: TLSocket;
    FIterator: TLSocket;
    FID: Integer; // internal number for server
    FEventer: TLEventer;
    FEventerClass: TLEventerClass;
    FTimeout: DWord;
    function InitSocket(aSocket: TLSocket): TLSocket; virtual;
    function GetConnected: Boolean; virtual; abstract;
    function GetCount: Integer; virtual;
    function GetItem(const i: Integer): TLSocket;
    function GetTimeout: DWord;
    procedure ConnectAction(aSocket: TLHandle); virtual;
    procedure AcceptAction(aSocket: TLHandle); virtual;
    procedure ReceiveAction(aSocket: TLHandle); virtual;
    procedure SendAction(aSocket: TLHandle); virtual;
    procedure ErrorAction(aSocket: TLHandle; const msg: string); virtual;
    procedure ConnectEvent(aSocket: TLHandle); virtual;
    procedure DisconnectEvent(aSocket: TLHandle); virtual;
    procedure AcceptEvent(aSocket: TLHandle); virtual;
    procedure ReceiveEvent(aSocket: TLHandle); virtual;
    procedure CanSendEvent(aSocket: TLHandle); virtual;
    procedure ErrorEvent(const msg: string; aSocket: TLHandle); virtual;
    procedure SetTimeout(const AValue: DWord);
    procedure SetEventer(Value: TLEventer);
    procedure EventerError(const msg: string; Sender: TLEventer);
    procedure RegisterWithEventer; virtual;
    procedure FreeSocks; virtual;
   public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    function Connect(const Address: string; const APort: Word): Boolean; virtual; abstract;
    function Listen(const APort: Word; const AIntf: string = LADDR_ANY): Boolean; virtual; abstract;
    function Get(var aData; const aSize: Integer; aSocket: TLSocket = nil): Integer; virtual; abstract;
    function GetMessage(out msg: string; aSocket: TLSocket = nil): Integer; virtual; abstract;
    function Send(const aData; const aSize: Integer; aSocket: TLSocket = nil): Integer; virtual; abstract;
    function SendMessage(const msg: string; aSocket: TLSocket = nil): Integer; virtual; abstract;
    function IterNext: Boolean; virtual; abstract;
    procedure Disconnect; virtual; abstract;
    procedure IterReset; virtual; abstract;
    procedure CallAction; virtual; abstract;
   public
    property OnError: TLErrorProc read FOnError write FOnError;
    property OnReceive: TLProc read FOnReceive write FOnReceive;
    property OnDisconnect: TLProc read FOnDisconnect write FOnDisconnect;
    property OnCanSend: TLProc read FOnCanSend write FOnCanSend;
    property Socks[index: Integer]: TLSocket read GetItem; default;
    property Count: Integer read GetCount;
    property Connected: Boolean read GetConnected;
    property Iterator: TLSocket read FIterator;
    property Timeout: DWord read GetTimeout write SetTimeout;
    property SocketClass: TLSocketClass read FSocketClass write FSocketClass;
    property Eventer: TLEventer read FEventer write SetEventer;
    property EventerClass: TLEventerClass read FEventerClass write FEventerClass;
  end;
  
  { UDP Client/Server class. Provided to enable usage of UDP sockets }

  { TLUdp }

  TLUdp = class(TLConnection)
   protected
    function InitSocket(aSocket: TLSocket): TLSocket; override;
    function GetConnected: Boolean; override;
    procedure ReceiveAction(aSocket: TLHandle); override;
    procedure SendAction(aSocket: TLHandle); override;
    procedure ErrorAction(aSocket: TLHandle; const msg: string); override;
    procedure Bail(const msg: string);
    procedure SetAddress(const Address: string);
   public
    constructor Create(aOwner: TComponent); override;
    function Connect(const Address: string; const APort: Word): Boolean; override;
    function Listen(const APort: Word; const AIntf: string = LADDR_ANY): Boolean; override;
    function Get(var aData; const aSize: Integer; aSocket: TLSocket = nil): Integer; override;
    function GetMessage(out msg: string; aSocket: TLSocket = nil): Integer; override;
    function SendMessage(const msg: string; aSocket: TLSocket = nil): Integer; override;
    function SendMessage(const msg: string; const Address: string): Integer; overload;
    function Send(const aData; const aSize: Integer; aSocket: TLSocket = nil): Integer; override;
    function Send(const aData; const aSize: Integer; const Address: string): Integer; overload;
    function IterNext: Boolean; override;
    procedure Disconnect; override;
    procedure IterReset; override;
    procedure CallAction; override;
  end;
  
  { TCP Client/Server class. Provided to enable usage of TCP sockets }

  { TLTcp }

  TLTcp = class(TLConnection)
   protected
    FCount: Integer;
    function InitSocket(aSocket: TLSocket): TLSocket; override;
    function GetConnected: Boolean; override;
    procedure ConnectAction(aSocket: TLHandle); override;
    procedure AcceptAction(aSocket: TLHandle); override;
    procedure ReceiveAction(aSocket: TLHandle); override;
    procedure SendAction(aSocket: TLHandle); override;
    procedure ErrorAction(aSocket: TLHandle; const msg: string); override;
    procedure Bail(const msg: string; aSocket: TLSocket);
    procedure SocketDisconnect(aSocket: TLSocket);
   public
    constructor Create(aOwner: TComponent); override;
    function Connect(const Address: string; const APort: Word): Boolean; override;
    function Listen(const APort: Word; const AIntf: string = LADDR_ANY): Boolean; override;
    function Get(var aData; const aSize: Integer; aSocket: TLSocket = nil): Integer; override;
    function GetMessage(out msg: string; aSocket: TLSocket = nil): Integer; override;
    function Send(const aData; const aSize: Integer; aSocket: TLSocket = nil): Integer; override;
    function SendMessage(const msg: string; aSocket: TLSocket = nil): Integer; override;
    function IterNext: Boolean; override;
    procedure CallAction; override;
    procedure IterReset; override;
    procedure Disconnect; override;
    property OnAccept: TLProc read FOnAccept write FOnAccept;
    property OnConnect: TLProc read FOnConnect write FOnConnect;
  end;
  
implementation

uses
  lCommon;
  
//********************************TLSocket*************************************

constructor TLSocket.Create;
begin
  inherited Create;
  FServerSocket:=False;
  FPrevSock:=nil;
  FNextSock:=nil;
  FCanSend:=True;
  FCanReceive:=False;
  FConnected:=False;
  FConnecting:=False;
  FIgnoreShutdown:=False;
  FSocketClass:=SOCK_STREAM;
  FProtocol:=LPROTO_TCP;
end;

destructor TLSocket.Destroy;
begin
  if Assigned(FOnFree) then
    FOnFree(Self);
  Disconnect;
  inherited Destroy;
end;

procedure TLSocket.Disconnect;
begin
  FDispose:=True;
  FCanSend:=True;
  FCanReceive:=True;
  FIgnoreWrite:=True;
  if Connected or FConnecting then begin
    FConnected:=false;
    FConnecting:=False;
    if (FSocketClass = SOCK_STREAM) and (not FIgnoreShutdown) and FConnected then
      if ShutDown(FHandle, 2) <> 0 then
        LogError('Shutdown error', LSocketError);
    if CloseSocket(FHandle) <> 0 then
      LogError('Closesocket error', LSocketError);
  end;
end;

procedure TLSocket.LogError(const msg: string; const ernum: Integer);
begin
  if Assigned(FOnError) then
    if ernum > 0 then
      FOnError(Self, msg + ': ' + LStrError(ernum))
    else
      FOnError(Self, msg);
end;

procedure TLSocket.Bail(const msg: string; const ernum: Integer);
begin
  Disconnect;
  LogError(msg, ernum);
end;

function TLSocket.GetPeerAddress: string;
begin
  Result:='';
  if FSocketClass = SOCK_STREAM then
    Result:=HostAddrtoStr(FAddress.Addr)
  else
    Result:=HostAddrtoStr(FPeerAddress.Addr);
end;

function TLSocket.GetLocalAddress: string;
var
  a: TSockAddr;
  l: Integer;
begin
  l:=SizeOf(a);
  GetSocketName(FHandle, a, l);
  Result:=HostAddrToStr(LongWord(a.sin_addr));
end;

function TLSocket.CanSend: Boolean;
begin
  Result:=FCanSend and FConnected;
end;

function TLSocket.CanReceive: Boolean;
begin
  Result:=FCanReceive and FConnected;
end;

procedure TLSocket.SetOptions;
begin
  SetNonBlock;
end;

procedure TLSocket.SetNonBlock;
{$ifdef MSWINDOWS}
var
  opt: DWord;
begin
   opt:=1;
   if ioctlsocket(FHandle, FIONBIO, opt) = SOCKET_ERROR then
     bail('Error on SetFD', wsaGetLasterror);
{$else}
var
  opt: cInt;
begin
   opt:=fpfcntl(FHandle, F_GETFL);
   if opt = SOCKET_ERROR then begin
     bail('ERROR on GetFD', LSocketError);
     Exit;
   end;

   if fpfcntl(FHandle, F_SETFL, opt or O_NONBLOCK) = SOCKET_ERROR then
     bail('Error on SetFL', LSocketError);
{$endif}
end;

function TLSocket.GetMessage(out msg: string): Integer;
begin
  Result:=0;
  SetLength(msg, BUFFER_SIZE);
  SetLength(msg, Get(PChar(msg)^, Length(msg)));
  Result:=Length(msg);
end;

function TLSocket.Get(var aData; const aSize: Integer): Integer;
var
  AddressLength: Integer = SizeOf(FAddress);
begin
  Result:=0;
  if CanReceive then begin
    if FSocketClass = SOCK_STREAM then
      Result:=sockets.Recv(FHandle, aData, aSize, LMSG)
    else
      Result:=sockets.Recvfrom(FHandle, aData, aSize, LMSG, FPeerAddress, AddressLength);
    if Result = 0 then
      Disconnect;
    if Result = SOCKET_ERROR then begin
      if LSocketError = BLOCK_ERROR then begin
        FCanReceive := False;
        FIgnoreRead := False;
      end else Bail('Receive Error', LSocketError);
      Result:=0;
    end;
  end;
end;

function TLSocket.DoSend(const TheData; const TheSize: Integer): Integer;
begin
  if FSocketClass = SOCK_STREAM then
    Result:=sockets.send(FHandle, TheData, TheSize, LMSG)
  else
    Result:=sockets.sendto(FHandle, TheData, TheSize, LMSG, FPeerAddress, SizeOf(FPeerAddress));
end;

function TLSocket.SetupSocket(const APort: Word; const Address: string): Boolean;
var
  Done: Boolean;
  Arg: Integer;
begin
  Result:=false;
  if not Connected then begin
    Done:=true;
    FHandle:=fpSocket(AF_INET, FSocketClass, FProtocol);
    if FHandle = INVALID_SOCKET then
      Bail('Socket error', LSocketError);
    SetOptions;
    if FSocketClass = SOCK_DGRAM then begin
      Arg:=1;
      if SetSocketOptions(FHandle, SOL_SOCKET, SO_BROADCAST, Arg, Sizeof(Arg)) = SOCKET_ERROR then
        Bail('SetSockOpt error', LSocketError);
    end;
    FAddress.family:=AF_INET;
    FAddress.Port:=htons(APort);
    FAddress.Addr:=StrToNetAddr(Address);
    if (Address <> LADDR_ANY) and (FAddress.Addr = 0) then
      FAddress.Addr:=StrToNetAddr(GetHostIP(Address));
    FPeerAddress:=FAddress;
    FPeerAddress.addr:=StrToNetAddr(LADDR_BR);
    Result:=Done;
  end;
end;

function TLSocket.GetLocalPort: Word;
begin
  Result:=FAddress.sin_port;
end;

function TLSocket.GetPeerPort: Word;
begin
  Result:=FPeerAddress.sin_port;
end;

function TLSocket.Listen(const APort: Word; const AIntf: string = LADDR_ANY): Boolean;
begin
  if not Connected then begin
    Result:=false;
    SetupSocket(APort, AIntf);
    if fpBind(FHandle, @FAddress, SizeOf(FAddress)) = INVALID_SOCKET then
      Bail('Error on bind', LSocketError)
    else
      Result:=true;
    if (FSocketClass = SOCK_STREAM) and Result then
      if fpListen(FHandle, 5) = INVALID_SOCKET then
        Bail('Error on Listen', LSocketError) else Result:=true;
  end;
end;

function TLSocket.Accept(const sersock: Integer): Boolean;
var
  AddressLength: Integer = SizeOf(FAddress);
begin
  Result:=false;
  if not Connected then begin
    FHandle:=fpAccept(sersock, @FAddress, @AddressLength);
    if FHandle <> INVALID_SOCKET then begin
      SetOptions;
      Result:=true;
      FConnected:=true;
    end else Bail('Error on accept', LSocketError);
  end;
end;

function TLSocket.Connect(const Address: string; const aPort: Word): Boolean;
begin
  Result:=False;
  if Connected or FConnecting then Disconnect;
  if SetupSocket(APort, Address) then begin
    fpConnect(FHandle, @FAddress, SizeOf(FAddress));
    FConnecting:=True;
    Result:=FConnecting;
  end;
end;

function TLSocket.SendMessage(const msg: string): Integer;
begin
  Result:=Send(PChar(msg)^, Length(msg));
end;

function TLSocket.Send(const aData; const aSize: Integer): Integer;
begin
  Result:=0;
  if not FServerSocket then begin
    if aSize <= 0 then
      Bail('Send error: wrong size (Size <= 0)', -1);

    if CanSend then begin
      Result:=DoSend(aData, aSize);
      if Result = SOCKET_ERROR then begin
        if LSocketError = BLOCK_ERROR then begin
          FCanSend:=False;
          FIgnoreWrite:=False;
          Result:=0;
        end else
          Bail('Send error', LSocketError);
        Result:=0;
      end;
    end;
 end;
end;

//*******************************TLConnection*********************************

constructor TLConnection.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FTimeout:=0;
  FSocketClass:=TLSocket;
  FOnReceive:=nil;
  FOnError:=nil;
  FOnDisconnect:=nil;
  FOnCanSend:=nil;
  FOnConnect:=nil;
  FOnAccept:=nil;
  FTimeVal.tv_sec:=0;
  FTimeVal.tv_usec:=0;
  FIterator:=nil;
  FEventer:=nil;
  FEventerClass:=BestEventerClass;
end;

destructor TLConnection.Destroy;
begin
  FreeSocks;
  if Assigned(FEventer) then
    FEventer.DeleteRef;
  inherited Destroy;
end;

function TLConnection.InitSocket(aSocket: TLSocket): TLSocket;
begin
  aSocket.OnRead:=@ReceiveAction;
  aSocket.OnWrite:=@SendAction;
  aSocket.OnError:=@ErrorAction;
  Result:=aSocket;
end;

function TLConnection.GetCount: Integer;
begin
  Result:=1;
end;

function TLConnection.GetItem(const i: Integer): TLSocket;
var
  Tmp: TLSocket;
  Jumps: Integer;
begin
  Result:=nil;
  Tmp:=FRootSock;
  Jumps:=0;
  while Assigned(Tmp.NextSock) and (Jumps < i) do begin
    Tmp:=Tmp.NextSock;
    Inc(Jumps);
  end;
  if Jumps = i then
    Result:=Tmp;
end;

function TLConnection.GetTimeout: DWord;
begin
  if Assigned(FEventer) then
    Result:=FEventer.Timeout
  else
    Result:=FTimeout;
end;

procedure TLConnection.ConnectAction(aSocket: TLHandle);
begin
end;

procedure TLConnection.AcceptAction(aSocket: TLHandle);
begin
end;

procedure TLConnection.ReceiveAction(aSocket: TLHandle);
begin
end;

procedure TLConnection.SendAction(aSocket: TLHandle);
begin
end;

procedure TLConnection.ErrorAction(aSocket: TLHandle; const msg: string);
begin
end;

procedure TLConnection.ConnectEvent(aSocket: TLHandle);
begin
  if Assigned(FOnConnect) then
    FOnConnect(TLSocket(aSocket));
end;

procedure TLConnection.DisconnectEvent(aSocket: TLHandle);
begin
  if Assigned(FOnDisconnect) then
    FOnDisconnect(TLSocket(aSocket));
end;

procedure TLConnection.AcceptEvent(aSocket: TLHandle);
begin
  if Assigned(FOnAccept) then
    FOnAccept(TLSocket(aSocket));
end;

procedure TLConnection.ReceiveEvent(aSocket: TLHandle);
begin
  if Assigned(FOnReceive) then
    FOnReceive(TLSocket(aSocket));
end;

procedure TLConnection.CanSendEvent(aSocket: TLHandle);
begin
  if Assigned(FOnCanSend) then
    FOnCanSend(TLSocket(aSocket));
end;

procedure TLConnection.ErrorEvent(const msg: string; aSocket: TLHandle);
begin
  if Assigned(FOnError) then
    FOnError(msg, TLSocket(aSocket));
end;

procedure TLConnection.SetTimeout(const AValue: DWord);
begin
  if Assigned(FEventer) then
    FEventer.Timeout:=aValue;
  FTimeout:=aValue;
end;

procedure TLConnection.SetEventer(Value: TLEventer);
begin
  if Assigned(FEventer) then
    FEventer.DeleteRef;
  FEventer:=Value;
  FEventer.AddRef;
end;

procedure TLConnection.EventerError(const msg: string; Sender: TLEventer);
begin
  ErrorEvent(msg, nil);
end;

procedure TLConnection.RegisterWithEventer;
begin
  if not Assigned(FEventer) then begin
    FEventer:=FEventerClass.Create;
    FEventer.OnError:=@EventerError;
  end;
  if Assigned(FRootSock) then
    FEventer.AddHandle(FRootSock);
  if (FEventer.Timeout = 0) and (FTimeout > 0) then
    FEventer.Timeout:=FTimeout
  else
    FTimeout:=FEventer.Timeout;
end;

procedure TLConnection.FreeSocks;
var
  Tmp, Tmp2: TLSocket;
begin
  Tmp:=FRootSock;
  while Assigned(Tmp) do begin
    Tmp2:=Tmp;
    Tmp:=Tmp.NextSock;
    Tmp2.Free;
  end;
end;

//*******************************TLUdp*********************************

constructor TLUdp.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FTimeVal.tv_usec:=0;
  FTimeVal.tv_sec:=0;
end;

procedure TLUdp.Disconnect;
begin
  if Assigned(FRootSock) then begin
    FRootSock.Disconnect;
    FRootSock.Dispose:=False;
  end;
end;

function TLUdp.Connect(const Address: string; const APort: Word): Boolean;
var
  IP: string;
begin
  Result:=False;
  FRootSock:=InitSocket(FSocketClass.Create);
  FIterator:=FRootSock;
  if FRootSock.Connected then Disconnect;
  IP:=GetHostIP(Address);
  if Length(IP) = 0 then
    IP:=Address;
  Result:=FRootSock.SetupSocket(APort, LADDR_ANY);
  FRootSock.FPeerAddress:=FRootSock.FAddress;
  FRootSock.FPeerAddress.Addr:=StrToNetAddr(IP);
  FRootSock.FConnected:=true;
  if Result then
    RegisterWithEventer;
end;

function TLUdp.Listen(const APort: Word; const AIntf: string = LADDR_ANY): Boolean;
begin
  Result:=False;
  FRootSock:=InitSocket(FSocketClass.Create);
  FIterator:=FRootSock;
  if FRootSock.Connected then
    Disconnect;
  if FRootSock.Listen(APort, AIntf) then begin
    FRootSock.FPeerAddress:=FRootSock.FAddress;
    FRootSock.FPeerAddress.Addr:=StrToNetAddr(LADDR_BR);
    FRootSock.FConnected:=True;
    RegisterWithEventer;
  end;
  Result:=FRootSock.Connected;
end;

procedure TLUdp.Bail(const msg: string);
begin
  Disconnect;
  ErrorEvent(msg, FRootSock);
end;

procedure TLUdp.SetAddress(const Address: string);
var
  n: Integer;
  s: string;
  p: Word;
begin
  n:=Pos(':', Address);
  if n > 0 then begin
    s:=Copy(Address, 1, n-1);
    p:=StrToInt(Copy(Address, n+1, Length(Address)));
    FRootSock.FPeerAddress.Addr:=StrToNetAddr(s);
    FRootSock.FPeerAddress.port:=p;
  end else
    FRootSock.FPeerAddress.Addr:=StrToNetAddr(Address);
end;

function TLUdp.InitSocket(aSocket: TLSocket): TLSocket;
begin
  Result:=FRootSock;
  if not Assigned(FRootSock) then begin
    Result:=inherited InitSocket(aSocket);
    aSocket.SocketType:=SOCK_DGRAM;
    aSocket.Protocol:=LPROTO_UDP;
  end;
end;

procedure TLUdp.ReceiveAction(aSocket: TLHandle);
begin
  with TLSocket(aSocket) do begin
    FCanReceive:=True;
    ReceiveEvent(aSocket);
  end;
end;

procedure TLUdp.SendAction(aSocket: TLHandle);
begin
  with TLSocket(aSocket) do begin
    FCanSend:=True;
    FIgnoreWrite:=True;
    CanSendEvent(aSocket);
  end;
end;

procedure TLUdp.ErrorAction(aSocket: TLHandle; const msg: string);
begin
  Bail(msg);
end;

function TLUdp.IterNext: Boolean;
begin
  Result:=False;
end;

procedure TLUdp.IterReset;
begin
end;

procedure TLUdp.CallAction;
begin
  if Assigned(FEventer) then
    FEventer.CallAction;
end;

function TLUdp.GetConnected: Boolean;
begin
  Result:=False;
  if Assigned(FRootSock) then
  Result:=FRootSock.Connected;
end;

function TLUdp.Get(var aData; const aSize: Integer; aSocket: TLSocket): Integer;
begin
  Result:=0;
  if Assigned(FRootSock) then
    Result:=FRootSock.Get(aData, aSize);
end;

function TLUdp.GetMessage(out msg: string; aSocket: TLSocket): Integer;
begin
  Result:=0;
  if Assigned(FRootSock) then
    Result:=FRootSock.GetMessage(msg);
end;

function TLUdp.SendMessage(const msg: string; aSocket: TLSocket = nil): Integer;
begin
  Result:=0;
  if Assigned(FRootSock) then
    Result:=FRootSock.SendMessage(msg)
end;

function TLUdp.SendMessage(const msg: string; const Address: string): Integer;
begin
  Result:=0;
  if Assigned(FRootSock) then begin
    SetAddress(Address);
    Result:=FRootSock.SendMessage(msg)
  end;
end;

function TLUdp.Send(const aData; const aSize: Integer; aSocket: TLSocket): Integer;
begin
  Result:=0;
  if Assigned(FRootSock) then
    Result:=FRootSock.Send(aData, aSize)
end;

function TLUdp.Send(const aData; const aSize: Integer; const Address: string
  ): Integer;
begin
  Result:=0;
  if Assigned(FRootSock) then begin
    SetAddress(Address);
    Result:=FRootSock.Send(aData, aSize);
  end;
end;

//******************************TLTcp**********************************

constructor TLTcp.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FIterator:=nil;
  FCount:=0;
  FRootSock:=nil;
end;

function TLTcp.Connect(const Address: string; const APort: Word): Boolean;
begin
  Result:=False;
  if Assigned(FRootSock) then
    Disconnect;
  FRootSock:=InitSocket(FSocketClass.Create);
  Result:=FRootSock.Connect(Address, aPort);
  if Result then begin
    Inc(FCount);
    FIterator:=FRootSock;
    RegisterWithEventer;
  end else begin
    FreeAndNil(FRootSock);
    FIterator:=nil;
  end;
end;

function TLTcp.Listen(const APort: Word; const AIntf: string = LADDR_ANY): Boolean;
begin
  Result:=false;
  if (aPort > 0) and not Assigned(FRootSock) then begin
    FRootSock:=InitSocket(FSocketClass.Create);
    FRootSock.FIgnoreShutdown:=True;
    if FRootSock.Listen(APort, AIntf) then begin
      FRootSock.FConnected:=True;
      FRootSock.FServerSocket:=True;
      RegisterWithEventer;
      Result:=true;
    end;
  end else Bail('Error, already listening', nil);
end;

procedure TLTcp.Bail(const msg: string; aSocket: TLSocket);
begin
  ErrorEvent(msg, aSocket);
  if Assigned(aSocket) then
    aSocket.Disconnect
  else
    Disconnect;
end;

procedure TLTcp.SocketDisconnect(aSocket: TLSocket);
begin
  if aSocket = FIterator then begin
    if Assigned(FIterator.NextSock) then
      FIterator:=FIterator.NextSock
    else if Assigned(FIterator.PrevSock) then
      FIterator:=FIterator.PrevSock
    else FIterator:=nil; // NOT iterreset, not reorganized yet
    if Assigned(FIterator) and FIterator.FServerSocket then
      FIterator:=nil;
  end;

  if aSocket = FRootSock then
    FRootSock:=aSocket.NextSock;
  if Assigned(aSocket.PrevSock) then
    aSocket.PrevSock.NextSock:=aSocket.NextSock;
  if Assigned(aSocket.NextSock) then
    aSocket.NextSock.PrevSock:=aSocket.PrevSock;
  Dec(FCount);
end;

function TLTcp.InitSocket(aSocket: TLSocket): TLSocket;
begin
  Result:=inherited InitSocket(aSocket);
  aSocket.SocketType:=SOCK_STREAM;
  aSocket.Protocol:=LPROTO_TCP;
  aSocket.FOnFree:=@SocketDisconnect;
end;

function TLTcp.IterNext: Boolean;
begin
  Result:=False;
  if Assigned(FIterator.NextSock) then begin
    FIterator:=FIterator.NextSock;
    Result:=True;
  end else IterReset;
end;

procedure TLTcp.IterReset;
begin
  if Assigned(FRootSock) and FRootSock.FServerSocket then
    FIterator:=FRootSock.NextSock
  else
    FIterator:=FRootSock;
end;

procedure TLTcp.Disconnect;
begin
  FreeSocks;
  FRootSock:=nil;
  FCount:=0;
  FIterator:=nil;
end;

procedure TLTcp.CallAction;
begin
  if Assigned(FEventer) then
    FEventer.CallAction;
end;

procedure TLTcp.ConnectAction(aSocket: TLHandle);
var
  {$ifdef MSWINDOWS}
  a: TSockAddrin;
  {$else}
  a: TInetSockAddr;
  {$endif}
  l: Integer;
begin
  with TLSocket(aSocket) do begin
    l:=SizeOf(a);
    {$ifndef mswindows}
    if fpgetpeername(FHandle, @a, @l) <> 0 then
    {$else}
    if winsock2.getpeername(FHandle, a, l) <> 0 then
    {$endif}
      Self.Bail('Error on connect: connection refused', TLSocket(aSocket))
    else begin
      FConnected:=True;
      FConnecting:=False;
      ConnectEvent(aSocket);
    end;
  end;
end;

procedure TLTcp.AcceptAction(aSocket: TLHandle);
var
  Tmp: TLSocket;
begin
  Tmp:=InitSocket(FSocketClass.Create);
  if Tmp.Accept(FRootSock.FHandle) then begin
    if Assigned(FRootSock.FNextSock) then begin
      Tmp.FNextSock:=FRootSock.FNextSock;
      FRootSock.FNextSock.FPrevSock:=Tmp;
    end;
    FRootSock.FNextSock:=Tmp;
    Tmp.FPrevSock:=FRootSock;
    if not Assigned(FIterator) then
      FIterator:=Tmp;
    Inc(FCount);
    FEventer.AddHandle(Tmp);
    AcceptEvent(Tmp);
  end else Tmp.Free;
end;

procedure TLTcp.ReceiveAction(aSocket: TLHandle);
begin
  if (TLSocket(aSocket) = FRootSock) and TLSocket(aSocket).FServerSocket then
    AcceptAction(aSocket)
  else with TLSocket(aSocket) do begin
    if Connected then begin
      FCanReceive:=True;
      ReceiveEvent(aSocket);
      if not Connected then begin
        DisconnectEvent(aSocket);
        aSocket.Free;
      end;
    end;
  end;
end;

procedure TLTcp.SendAction(aSocket: TLHandle);
begin
  with TLSocket(aSocket) do begin
    if Connecting then
      ConnectAction(aSocket)
    else begin
      FCanSend:=True;
      FIgnoreWrite:=True;
      CanSendEvent(aSocket);
    end;
  end;
end;

procedure TLTcp.ErrorAction(aSocket: TLHandle; const msg: string);
begin
  with TLSocket(aSocket) do begin
    if Connecting then
      Self.Bail('Error on connect: connection refused' , TLSocket(aSocket))
    else
      Self.Bail(msg, TLSocket(aSocket));
  end;
end;

function TLTcp.GetConnected: Boolean;
var
  Tmp: TLSocket;
begin
  Result:=False;
  Tmp:=FRootSock;
  while Assigned(Tmp) do begin
    if Tmp.Connected then begin
      Result:=True;
      Exit;
    end else Tmp:=Tmp.NextSock;
  end;
end;

function TLTcp.Get(var aData; const aSize: Integer; aSocket: TLSocket): Integer;
begin
  Result:=0;
  if not Assigned(aSocket) then
    aSocket:=FIterator;
  if Assigned(aSocket) then
    Result:=aSocket.Get(aData, aSize);
end;

function TLTcp.GetMessage(out msg: string; aSocket: TLSocket): Integer;
begin
  Result:=0;
  if not Assigned(aSocket) then
    aSocket:=FIterator;
  if Assigned(aSocket) then
    Result:=aSocket.GetMessage(msg);
end;

function TLTcp.Send(const aData; const aSize: Integer; aSocket: TLSocket): Integer;
begin
  Result:=0;
  if not Assigned(aSocket) then
    aSocket:=FIterator;
  if Assigned(aSocket) and (aSize > 0) then
    Result:=aSocket.Send(aData, aSize);
end;

function TLTcp.SendMessage(const msg: string; aSocket: TLSocket): Integer;
begin
  Result:=Send(PChar(msg)^, Length(msg), aSocket);
end;


end.

