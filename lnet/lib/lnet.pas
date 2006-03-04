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
{$ifdef mswindows}
  Winsock2,
{$else}
  BaseUnix, NetDB,
{$endif}
  SysUtils, Sockets;

//  {$i osunits.inc}

const
  { Address constants }
  LADDR_ANY = '0.0.0.0';
  LADDR_BR  = '255.255.255.255';
  LADDR_LO  = '127.0.0.1';
  { Protocols }
  PROTO_TCP =  6;
  PROTO_UDP = 17;

type
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
   protected
    function DoSend(const TheData; const TheSize: Integer): Integer;
    function SetupSocket(const APort: Word; const Address: string): Boolean; virtual;
    function GetPort: Word;
    function GetPeerAddress: string;
    function GetLocalAddress: string;
    function CanSend: Boolean; virtual;
    function CanReceive: Boolean; virtual;
    procedure SetNonBlock; virtual;
    procedure Bail(const msg: string; const ernum: Integer);
    procedure LogError(const msg: string; const ernum: Integer); virtual;
   public
    constructor Create; override;
    destructor Destroy; override;
    function Listen(const APort: Word): Boolean;
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
    property LocalAddress: string read GetLocalAddress;
    property Port: Word read GetPort;
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
    function Listen(const APort: Word): Boolean;
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
    FOnError: TLErrorProc;
    FOnDisconnect: TLProc;
    FOnCanSend: TLProc;
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
    procedure ConnectAction(aSocket: TLHandle); virtual;
    procedure AcceptAction(aSocket: TLHandle); virtual;
    procedure ReceiveAction(aSocket: TLHandle); virtual;
    procedure SendAction(aSocket: TLHandle); virtual;
    procedure ErrorAction(aSocket: TLHandle); virtual;
    procedure SetEventer(Value: TLEventer);
    procedure CanSend(aSocket: TLSocket); virtual;
    procedure EventError(const msg: string; Sender: TLEventer);
    procedure RegisterWithEventer; virtual;
    procedure FreeSocks; virtual;
   public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    function Connect(const Address: string; const APort: Word): Boolean; virtual; abstract;
    function Listen(const APort: Word): Boolean; virtual; abstract;
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
    property Timeout: DWord read FTimeout write FTimeout;
    property SocketClass: TLSocketClass read FSocketClass write FSocketClass;
    property Eventer: TLEventer read FEventer write SetEventer;
    property EventerClass: TLEventerClass read FEventerClass write FEventerClass;
  end;
  
  { UDP Client/Server class. Provided to enable usage of UDP sockets }

  TLUdp = class(TLConnection)
   protected
    function InitSocket(aSocket: TLSocket): TLSocket; override;
    function GetConnected: Boolean; override;
    procedure ReceiveAction(aSocket: TLHandle); override;
    procedure SendAction(aSocket: TLHandle); override;
    procedure ErrorAction(aSocket: TLHandle); override;
    procedure Bail(const msg: string);
   public
    constructor Create(aOwner: TComponent); override;
    function Connect(const Address: string; const APort: Word): Boolean; override;
    function Listen(const APort: Word): Boolean; override;
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

  TLTcp = class(TLConnection)
   protected
    FCount: Integer;
    FOnAccept: TLProc;
    FOnConnect: TLProc;
    function InitSocket(aSocket: TLSocket): TLSocket; override;
    function GetConnected: Boolean; override;
    procedure ConnectAction(aSocket: TLHandle); override;
    procedure AcceptAction(aSocket: TLHandle); override;
    procedure ReceiveAction(aSocket: TLHandle); override;
    procedure SendAction(aSocket: TLHandle); override;
    procedure ErrorAction(aSocket: TLHandle); override;
    procedure Bail(const msg: string; aSocket: TLSocket);
   public
    constructor Create(aOwner: TComponent); override;
    function Connect(const Address: string; const APort: Word): Boolean; override;
    function Listen(const aPort: Word): Boolean; override;
    function Get(var aData; const aSize: Integer; aSocket: TLSocket = nil): Integer; override;
    function GetMessage(out msg: string; aSocket: TLSocket = nil): Integer; override;
    function Send(const aData; const aSize: Integer; aSocket: TLSocket = nil): Integer; override;
    function SendMessage(const msg: string; aSocket: TLSocket = nil): Integer; override;
    function IterNext: Boolean; override;
    procedure CallAction; override;
    procedure IterReset; override;
    procedure Disconnect; override;
    procedure DisconnectSocket(var aSocket: TLSocket);
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
  FProtocol:=PROTO_TCP;
end;

destructor TLSocket.Destroy;
begin
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
    if Closesocket(FHandle) <> 0 then
      LogError('Closesocket error', LSocketError);
  end;
end;

procedure TLSocket.LogError(const msg: string; const ernum: Integer);
begin
  if Assigned(FOnError) then
    FOnError(Self);
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
    {$ifdef MSWINDOWS}
      Result:=winsock2.Recv(FHandle, aData, aSize, LMSG)
    else
      Result:=winsock2.Recvfrom(FHandle, aData, aSize, LMSG, TSockAddrIn(FPeerAddress), AddressLength);
    {$else}
      Result:=sockets.Recv(FHandle, aData, aSize, LMSG)
    else
      Result:=sockets.Recvfrom(FHandle, aData, aSize, LMSG, FPeerAddress, AddressLength);
    {$endif}
    if Result = 0 then
      Disconnect;
    if Result = INVALID_SOCKET then
      if LSocketError = BLOCK_ERROR then begin
        FCanReceive := False;
        FIgnoreRead := False;
      end else Bail('Receive Error', LSocketError);
  end;
  if not Connected then
    Result:=0; // if it failed subsequently
end;

function TLSocket.DoSend(const TheData; const TheSize: Integer): Integer;
begin
  if FSocketClass = SOCK_STREAM then
  {$ifdef MSWINDOWS}
    Result:=winsock2.send(FHandle, TheData, TheSize, LMSG)
  else
    Result:=winsock2.sendto(FHandle, TheData, TheSize, LMSG, TSockAddrIn(FPeerAddress), SizeOf(FPeerAddress));
  {$else}
    Result:=sockets.send(FHandle, TheData, TheSize, LMSG)
  else
    Result:=sockets.sendto(FHandle, TheData, TheSize, LMSG, FPeerAddress, SizeOf(FPeerAddress));
  {$endif}
end;

function TLSocket.SetupSocket(const APort: Word; const Address: string): Boolean;
var
  Done: Boolean;
begin
  Result:=false;
  if not Connected then begin
    Done:=true;
    FHandle:=fpsocket(AF_INET, FSocketClass, FProtocol);
    if FHandle = INVALID_SOCKET then
      Bail('Socket error', LSocketError);
    SetNonBlock;
    if FSocketClass = SOCK_DGRAM then
      SetSocketOptions(FHandle, SOL_SOCKET, SO_BROADCAST, True, SizeOf(True));
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

function TLSocket.GetPort: Word;
begin
  Result:=FAddress.sin_port;
end;

function TLSocket.Listen(const APort: Word): Boolean;
begin
  if not Connected then begin
    Result:=false;
    SetupSocket(APort, LADDR_ANY);
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
      SetNonBlock;
      Result:=true;
      FConnected:=true;
    end;
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
      if Result <= 0 then begin
        if LSocketError = BLOCK_ERROR then begin
          FCanSend:=False;
          FIgnoreWrite:=False;
          Result:=0;
        end else
          Bail('Send error', LSocketError);
      end else FIgnoreWrite:=False;
    end;
 end;
end;

procedure TLSocket.SetNonBlock;
var
  opt: DWord;
begin
  {$ifdef MSWINDOWS}
   opt:=1;
   if ioctlsocket(FHandle, FIONBIO, opt) = INVALID_SOCKET then
     bail('Error on SetFD', wsaGetLasterror);
  {$else}
   opt:=fpfcntl(FHandle, F_GETFL);
   if opt = INVALID_SOCKET then begin
     bail('ERROR on GetFD', LSocketError);
     Exit;
   end;

   if fpfcntl(FHandle, F_SETFL, opt or O_NONBLOCK) = INVALID_SOCKET then
     bail('Error on SetFL', LSocketError);
  {$endif}
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

procedure TLConnection.ErrorAction(aSocket: TLHandle);
begin
end;

procedure TLConnection.SetEventer(Value: TLEventer);
begin
  if Assigned(FEventer) then
    FEventer.DeleteRef;
  FEventer:=Value;
  FEventer.AddRef;
end;

procedure TLConnection.CanSend(aSocket: TLSocket);
begin
  if Assigned(FOnCanSend) then
    FOnCanSend(aSocket);
end;

procedure TLConnection.EventError(const msg: string; Sender: TLEventer);
begin
  if Assigned(FOnError) then
    FOnError(msg, nil);
end;

procedure TLConnection.RegisterWithEventer;
begin
  if not Assigned(FEventer) then begin
    FEventer:=FEventerClass.Create;
    FEventer.OnError:=@EventError;
  end;
  if Assigned(FRootSock) then
    FEventer.AddHandle(FRootSock);
  FEventer.Timeout:=FTimeout;
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
  if Assigned(FRootSock) then
    FRootSock.Disconnect;
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

function TLUdp.Listen(const APort: Word): Boolean;
begin
  Result:=False;
  FRootSock:=InitSocket(FSocketClass.Create);
  FIterator:=FRootSock;
  if FRootSock.Connected then
    Disconnect;
  if FRootSock.Listen(APort) then begin
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
  if Assigned(FOnError) then
    FOnError(msg, FRootSock);
end;

function TLUdp.InitSocket(aSocket: TLSocket): TLSocket;
begin
  Result:=nil;
  if not Assigned(FRootSock) then begin
    Result:=inherited InitSocket(aSocket);
    aSocket.SocketType:=SOCK_DGRAM;
    aSocket.Protocol:=PROTO_UDP;
  end;
end;

procedure TLUdp.ReceiveAction(aSocket: TLHandle);
begin
  with TLSocket(aSocket) do begin
    FCanReceive:=True;
    if Assigned(FOnReceive) then
      FOnReceive(FRootSock);
  end;
end;

procedure TLUdp.SendAction(aSocket: TLHandle);
begin
  with TLSocket(aSocket) do begin
    FCanSend:=True;
    FIgnoreWrite:=True;
    if Assigned(FOnCanSend) then
      FOnCanSend(FRootSock);
  end;
end;

procedure TLUdp.ErrorAction(aSocket: TLHandle);
begin
  Bail('Error' + LStrError(LSocketError));
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
    FRootSock.FPeerAddress.Addr:=StrToNetAddr(Address);
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
    FRootSock.FPeerAddress.Addr:=StrToNetAddr(Address);
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
  FOnAccept:=nil;
  FOnConnect:=nil;
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

function TLTcp.Listen(const aPort: Word): Boolean;
begin
  Result:=false;
  if (aPort > 0) and not Assigned(FRootSock) then begin
    FRootSock:=InitSocket(FSocketClass.Create);
    FRootSock.FIgnoreShutdown:=True;
    if FRootSock.Listen(APort) then begin
      FRootSock.FConnected:=True;
      FRootSock.FServerSocket:=True;
      FIterator:=FRootSock;
      RegisterWithEventer;
      Result:=true;
    end;
  end else Bail('Error, already listening', nil);
end;

procedure TLTcp.Bail(const msg: string; aSocket: TLSocket);
begin
  if Assigned(FOnError) then
    FOnError(msg, aSocket);
  if Assigned(aSocket) then
    DisconnectSocket(aSocket)
  else
    Disconnect;
end;

function TLTcp.InitSocket(aSocket: TLSocket): TLSocket;
begin
  Result:=inherited InitSocket(aSocket);
  aSocket.SocketType:=SOCK_STREAM;
  aSocket.Protocol:=PROTO_TCP;
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
var
  aSocket, Tmp: TLSocket;
begin
  aSocket:=FRootSock;
  while Assigned(aSocket) do begin
    Tmp:=aSocket;
    DisconnectSocket(aSocket);
    aSocket:=aSocket.NextSock;
    Tmp.Free;
  end;

  FRootSock:=nil;
  FCount:=0;
  FIterator:=nil;
end;

procedure TLTcp.DisconnectSocket(var aSocket: TLSocket);
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
  aSocket.Disconnect;
  Dec(FCount);
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
      if Assigned(FOnConnect) then
        FOnConnect(TLSocket(aSocket));
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
    if Assigned(FOnAccept) then
      FOnAccept(Tmp);
  end else Tmp.Free;
end;

procedure TLTcp.ReceiveAction(aSocket: TLHandle);
begin
  if (TLSocket(aSocket) = FRootSock) and TLSocket(aSocket).FServerSocket then
    AcceptAction(aSocket)
  else with TLSocket(aSocket) do begin
    if Connected then begin
      FCanReceive:=True;
      if Assigned(FOnReceive) then
        FOnReceive(TLSocket(aSocket));
      if not Connected then begin
        if Assigned(FOnDisconnect) then
          FOnDisconnect(TLSocket(aSocket));
        DisconnectSocket(TLSocket(aSocket));
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
      if Assigned(FOnCanSend) then
        FOnCanSend(TLSocket(aSocket));
    end;
  end;
end;

procedure TLTcp.ErrorAction(aSocket: TLHandle);
begin
  with TLSocket(aSocket) do begin
    if Connecting then
      Self.Bail('Error on connect: connection refused' , TLSocket(aSocket))
    else
      Self.Bail('Error' + LStrError(LSocketError), TLSocket(aSocket));
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

