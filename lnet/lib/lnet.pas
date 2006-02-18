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

{ TODO LIST

  1. Optimize "max" in CallSelect in TLBaseTcp

}

unit lNet;

{$mode objfpc}{$H+}
{$interfaces corba}

interface

uses
  Classes, lEvents,
{$ifdef mswindows}
  TomWinsock,
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
  { Base socket class, Holds Address and socket info, perForms basic
    socket operations, uses select always to figure out if it can work (slow) }

  { TLBaseSocket }

  TLBaseSocket = class(TLHandle)
   protected
    FAddress: TInetSockAddr;
    FPeerAddress: TInetSockAddr;
    FConnected: Boolean;
    FConnecting: Boolean;
    FSocketType: Integer;
    FProtocol: Integer;
    FIgnoreShutdown: Boolean;
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
    procedure HandleReceiveError(var aError: Integer); virtual;
    function Get(var aData; const aSize: Integer): Integer; virtual;
    function GetMessage(out msg: string): Integer;
    procedure Disconnect; virtual;
   public
    property Connected: Boolean read FConnected;
    property Connecting: Boolean read FConnecting;
    property Protocol: Integer read FProtocol write FProtocol;
    property SocketType: Integer read FSocketType write FSocketType;
    property PeerAddress: string read GetPeerAddress;
    property LocalAddress: string read GetLocalAddress;
    property Handle: Integer read FHandle;
    property Port: Word read GetPort;
  end;

  TLConnection = class;

  { this is the socket used by TLConnection }
  
  TLActionEnum = (acConnect, acAccept, acSend, acReceive, acError);

  { TLSocket }

  TLSocket = class(TLBaseSocket)
   protected
    FParent: TLConnection;
    FCanSend: Boolean;
    FCanReceive: Boolean;
    FNextSock: TLSocket;
    FPrevSock: TLSocket;
    FServerSocket: Boolean;
    function CanSend: Boolean; override;
    function CanReceive: Boolean; override;
    procedure SetParent(Value: TLConnection);
    procedure LogError(const msg: string; const ernum: Integer); override;
    procedure Disconnect; override;
   public
    constructor Create; override;
    procedure HandleReceiveError(var aError: Integer); override;
    function Send(const aData; const aSize: Integer): Integer; override;
    property NextSock: TLSocket read FNextSock write FNextSock;
    property PrevSock: TLSocket read FPrevSock write FPrevSock;
    property Parent: TLConnection read FParent write SetParent;
  end;
  
  TLSocketClass = class of TLSocket;

  { CallBack Event procedure for errors }
  TLErrorProc = procedure(const msg: string; aSocket: TLSocket) of object;

  { CallBack Event procedure for others }
  TLProc = procedure(aSocket: TLSocket) of object;
  
  { Base interface common to ALL connections }
  
  ILBase = interface
    function Get(var aData; const aSize: Integer; aSocket: TLSocket = nil): Integer;
    function GetMessage(out msg: string; aSocket: TLSocket = nil): Integer;
    function Send(const aData; const aSize: Integer; aSocket: TLSocket = nil): Integer;
    function SendMessage(const msg: string; aSocket: TLSocket = nil): Integer;
    procedure Disconnect;
    procedure CallAction;
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
    FSocketType: TLSocketClass;
    FRootSock: TLSocket;
    FIterator: TLSocket;
    FID: Integer; // internal number for server
    FEventer: TLEventer;
    FEventerClass: TLEventerClass;
    function GetConnected: Boolean; virtual; abstract;
    function GetCount: Integer; virtual;
    function GetItem(const i: Integer): TLSocket;
    function GetBlockTime: DWord;
    procedure ConnectAction(aSocket: TLHandle); virtual;
    procedure AcceptAction(aSocket: TLHandle); virtual;
    procedure ReceiveAction(aSocket: TLHandle); virtual;
    procedure SendAction(aSocket: TLHandle); virtual;
    procedure ErrorAction(aSocket: TLHandle); virtual;
    procedure SetBlockTime(const Value: DWord);
    procedure SetEventer(Value: TLEventer);
    procedure CanSend(aSocket: TLSocket); virtual;
    procedure EventError(const msg: string; Sender: TLEventer);
    procedure RegisterWithEventer; virtual;
    procedure FreeSocks; virtual;
   public
    constructor Create(aOwner: TComponent); override;
    constructor Create(aOwner: TComponent; SocketClass: TLSocketClass);
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
    property BlockTime: DWord read GetBlockTime write SetBlockTime;
    property Socks[index: Integer]: TLSocket read GetItem; default;
    property Count: Integer read GetCount;
    property Connected: Boolean read GetConnected;
    property Iterator: TLSocket read FIterator;
    property Eventer: TLEventer read FEventer write SetEventer;
    property EventerClass: TLEventerClass read FEventerClass write FEventerClass;
  end;
  
  { UDP Client/Server class. Provided to enable usage of UDP sockets }

  { TLBaseUdp }

  TLUdp = class(TLConnection)
   protected
    function GetConnected: Boolean; override;
    procedure ReceiveAction(aSocket: TLHandle); override;
    procedure SendAction(aSocket: TLHandle); override;
    procedure ErrorAction(aSocket: TLHandle); override;
    procedure Bail(const msg: string);
    procedure SetCallbacks(aSocket: TLSocket);
   public
    constructor Create(aOwner: TComponent); override;
    constructor Create(aOwner: TComponent; SocketClass: TLSocketClass);
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

  { TLBaseTcp }

  TLTcp = class(TLConnection)
   protected
    FCount: Integer;
    FOnAccept: TLProc;
    FOnConnect: TLProc;
    function GetConnected: Boolean; override;
    procedure ConnectAction(aSocket: TLHandle); override;
    procedure AcceptAction(aSocket: TLHandle); override;
    procedure ReceiveAction(aSocket: TLHandle); override;
    procedure SendAction(aSocket: TLHandle); override;
    procedure ErrorAction(aSocket: TLHandle); override;
    procedure Bail(const msg: string; aSocket: TLSocket);
   public
    constructor Create(aOwner: TComponent); override;
    constructor Create(aOwner: TComponent; SocketClass: TLSocketClass);
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
  
//********************************TLBaseSocket*************************************

constructor TLBaseSocket.Create;
begin
  inherited Create;
  FConnected:=False;
  FConnecting:=False;
  FIgnoreShutdown:=False;
  FSocketType:=SOCK_STREAM;
  FProtocol:=IPPROTO_TCP;
end;

destructor TLBaseSocket.Destroy;
begin
  Disconnect;
  inherited Destroy;
end;

procedure TLBaseSocket.Disconnect;
begin
  if Connected or FConnecting then begin
    if (FSocketType = SOCK_STREAM) and (not FIgnoreShutdown) and FConnected then
      if ShutDown(FHandle, 2) <> 0 then
        LogError('Shutdown error', LSocketError);
    if Closesocket(FHandle) <> 0 then
      LogError('Closesocket error', LSocketError);
    FConnected:=false;
    FConnecting:=False;
  end;
  FDispose:=True;
end;

procedure TLBaseSocket.LogError(const msg: string; const ernum: Integer);
begin
end;

procedure TLBaseSocket.Bail(const msg: string; const ernum: Integer);
begin
  Disconnect;
  LogError(msg, ernum);
end;

function TLBaseSocket.GetPeerAddress: string;
begin
  Result:='';
  if FSocketType = SOCK_STREAM then
    Result:=HostAddrtoStr(FAddress.Addr)
  else
    Result:=HostAddrtoStr(FPeerAddress.Addr);
end;

function TLBaseSocket.GetLocalAddress: string;
var
  a: TSockAddr;
  l: Integer;
begin
  l:=SizeOf(a);
  GetSocketName(FHandle, a, l);
  Result:=HostAddrToStr(LongWord(a.sin_addr));
end;

function TLBaseSocket.CanSend: Boolean;
var
  FDSet: TFDSet;
begin
  Result:=False;
  fpFD_ZERO(FDSet);
  fpFD_SET(FHandle, FDSet);
  if fpSelect(FHandle+1, nil, @FDSet, nil, nil) < 0 then
    Bail('Error on select', LSocketError)
  else begin
    if fpFD_ISSET(FHandle, FDSet) <> 0 then
      Result:=True and FConnected;
  end;
end;

function TLBaseSocket.CanReceive: Boolean;
var
  FDSet: TFDSet;
begin
  Result:=False;
  fpFD_ZERO(FDSet);
  fpFD_SET(FHandle, FDSet);
  if fpSelect(FHandle+1, @FDSet, nil, nil, nil) < 0 then
    Bail('Error on select', LSocketError)
  else begin
    if fpFD_ISSET(FHandle, FDSet) <> 0 then
      Result:=True and FConnected;
  end;
end;

function TLBaseSocket.GetMessage(out msg: string): Integer;
begin
  Result:=0;
  SetLength(msg, BUFFER_SIZE);
  SetLength(msg, Get(PChar(msg)^, Length(msg)));
  Result:=Length(msg);
end;

procedure TLBaseSocket.HandleReceiveError(var aError: Integer);
begin
  Bail('Receive Error', LSocketError);
end;

function TLBaseSocket.Get(var aData; const aSize: Integer): Integer;
var
  AddressLength: Integer = SizeOf(FAddress);
begin
  Result:=0;
  if CanReceive then begin
    if FSocketType = SOCK_STREAM then
    {$ifdef MSWINDOWS}
      Result:=tomwinsock.Recv(FHandle, aData, aSize, LMSG)
    else
      Result:=tomwinsock.Recvfrom(FHandle, aData, aSize, LMSG, TSockAddrIn(FPeerAddress), AddressLength);
    {$else}
      Result:=sockets.Recv(FHandle, aData, aSize, LMSG)
    else
      Result:=sockets.Recvfrom(FHandle, aData, aSize, LMSG, FPeerAddress, AddressLength);
    {$endif}
    if Result = 0 then Bail('Lost Connection', -1);
    if Result < 0 then
      HandleReceiveError(Result);
  end;
  if not Connected then
    Result:=0; // if it failed subsequently
end;

function TLBaseSocket.DoSend(const TheData; const TheSize: Integer): Integer;
begin
  if FSocketType = SOCK_STREAM then
  {$ifdef MSWINDOWS}
    Result:=tomwinsock.send(FHandle, TheData, TheSize, LMSG)
  else
    Result:=tomwinsock.sendto(FHandle, TheData, TheSize, LMSG, TSockAddrIn(FPeerAddress), SizeOf(FPeerAddress));
  {$else}
    Result:=sockets.send(FHandle, TheData, TheSize, LMSG)
  else
    Result:=sockets.sendto(FHandle, TheData, TheSize, LMSG, FPeerAddress, SizeOf(FPeerAddress));
  {$endif}
end;

function TLBaseSocket.SetupSocket(const APort: Word; const Address: string): Boolean;
var
  Done: Boolean;
begin
  Result:=false;
  if not Connected then begin
    Done:=true;
    FHandle:=fpsocket(AF_INET, FSocketType, FProtocol);
    if FHandle < 0 then bail('Socket error', LSocketError);
    SetNonBlock;
    if FSocketType = SOCK_DGRAM then
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

function TLBaseSocket.GetPort: Word;
begin
  Result:=FAddress.sin_port;
end;

function TLBaseSocket.Listen(const APort: Word): Boolean;
begin
  if not Connected then begin
    Result:=false;
    SetupSocket(APort, LADDR_ANY);
    if fpBind(FHandle, @FAddress, SizeOf(FAddress)) < 0 then
      Bail('Error on bind', LSocketError) else Result:=true;
    if (FSocketType = SOCK_STREAM) and Result then
      if fpListen(FHandle, 5) < 0 then
        Bail('Error on Listen', LSocketError) else Result:=true;
  end;
end;

function TLBaseSocket.Accept(const sersock: Integer): Boolean;
var
  AddressLength: Integer = SizeOf(FAddress);
begin
  Result:=false;
  if not Connected then begin
    FHandle:=fpAccept(sersock, @FAddress, @AddressLength);
    if FHandle > -1 then begin
      SetNonBlock;
      Result:=true;
      FConnected:=true;
    end;
  end;
end;

function TLBaseSocket.Connect(const Address: string; const aPort: Word): Boolean;
begin
  Result:=False;
  if Connected or FConnecting then Disconnect;
  if SetupSocket(APort, Address) then begin
    fpConnect(FHandle, @FAddress, SizeOf(FAddress));
    FConnecting:=True;
    Result:=FConnecting;
  end;
end;

function TLBaseSocket.SendMessage(const msg: string): Integer;
begin
  Result:=Send(PChar(msg)^, Length(msg));
end;

function TLBaseSocket.Send(const aData; const aSize: Integer): Integer;
begin
  Result:=0;
  if aSize <= 0 then
    Bail('Send error: wrong size (Size <= 0)', -1);

  if CanSend then begin
    Result:=DoSend(aData, aSize);
    if Result <= 0 then
      Bail('Send error', LSocketError);
  end;
end;

procedure TLBaseSocket.SetNonBlock;
var
  opt: Integer;
begin
  {$ifdef MSWINDOWS}
   opt:=1;
   if ioctlsocket(FHandle, FIONBIO, opt) < 0 then
     bail('Error on SetFD', wsaGetLasterror);
  {$else}
   opt:=fpfcntl(FHandle, F_GETFL);
   if opt < 0 then begin
     bail('ERROR on GetFD', LSocketError);
     Exit;
   end;

   if fpfcntl(FHandle, F_SETFL, opt or O_NONBLOCK) < 0 then
     bail('Error on SetFL', LSocketError);
  {$endif}
end;

//********************************TLSocket***********************************

constructor TLSocket.Create;
begin
  inherited Create;
  FServerSocket:=False;
  FParent:=nil;
  FPrevSock:=nil;
  FNextSock:=nil;
  FCanSend:=True;
  FCanReceive:=False;
end;

procedure TLSocket.HandleReceiveError(var aError: Integer);
begin
  if LSocketError = BLOCK_ERROR then
  begin
    FCanReceive := False;
    FIgnoreRead := False;
    aError := 0;
  end
    else
      Bail('Receive Error', LSocketError);
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

function TLSocket.CanSend: Boolean;
begin
  Result:=FCanSend and FConnected;
end;

function TLSocket.CanReceive: Boolean;
begin
  Result:=FCanReceive and FConnected;
end;

procedure TLSocket.SetParent(Value: TLConnection);
begin
  FParent:=Value;
  FOnRead:=@Value.ReceiveAction;
  FOnWrite:=@Value.SendAction;
  FOnError:=@Value.ErrorAction;
end;

procedure TLSocket.LogError(const msg: string; const ernum: Integer);
begin
  if Assigned(FParent) then
    if (ernum > 0) and Assigned(FParent.FOnError) then
      FParent.FOnError(msg + ': ' + LStrError(ernum), Self)
end;

procedure TLSocket.Disconnect;
begin
  inherited Disconnect;
  FCanSend:=True;
  FCanReceive:=True;
  FIgnoreWrite:=True;
end;

//*******************************TLConnection*********************************

constructor TLConnection.Create(aOwner: TComponent);
begin
  Create(aOwner, nil);
end;

constructor TLConnection.Create(aOwner: TComponent; SocketClass: TLSocketClass);
begin
  inherited Create(aOwner);
  if Assigned(SocketClass) then
    FSocketType:=SocketClass
  else
    FSocketType:=TLSocket;
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

function TLConnection.GetBlockTime: DWord;
begin
  Result:=(FTimeVal.tv_sec * 1000) + (FTimeVal.tv_usec div 1000);
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

procedure TLConnection.SetBlockTime(const Value: DWord);
begin
  FTimeVal.tv_sec:=Value div 1000;
  FTimeVal.tv_usec:=(Value mod 1000) * 1000;
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
  Create(aOwner, nil)
end;

constructor TLUdp.Create(aOwner: TComponent; SocketClass: TLSocketClass);
begin
  inherited Create(aOwner, SocketClass);
  FTimeVal.tv_usec:=0;
  FTimeVal.tv_sec:=0;
  FRootSock:=FSocketType.Create;
  FRootSock.SocketType:=SOCK_DGRAM;
  FRootSock.Protocol:=PROTO_UDP;
  FRootSock.Parent:=Self;
  FIterator:=FRootSock;
  SetCallbacks(FRootSock);
end;

procedure TLUdp.Disconnect;
begin
  FRootSock.Disconnect;
end;

function TLUdp.Listen(const APort: Word): Boolean;
begin
  Result:=False;
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

function TLUdp.Connect(const Address: string; const APort: Word): Boolean;
var
  IP: string;
begin
  Result:=False;
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

procedure TLUdp.Bail(const msg: string);
begin
  Disconnect;
  if Assigned(FOnError) then
    FOnError(msg, FRootSock);
end;

procedure TLUdp.SetCallbacks(aSocket: TLSocket);
begin
  aSocket.OnRead:=@ReceiveAction;
  aSocket.OnWrite:=@SendAction;
  aSocket.OnError:=@ErrorAction;
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
  Result:=FRootSock.Connected;
end;

function TLUdp.Get(var aData; const aSize: Integer; aSocket: TLSocket): Integer;
begin
  Result:=FRootSock.Get(aData, aSize);
end;

function TLUdp.GetMessage(out msg: string; aSocket: TLSocket): Integer;
begin
  Result:=FRootSock.GetMessage(msg);
end;

function TLUdp.SendMessage(const msg: string; aSocket: TLSocket = nil): Integer;
begin
  Result:=FRootSock.SendMessage(msg)
end;

function TLUdp.SendMessage(const msg: string; const Address: string): Integer;
begin
  FRootSock.FPeerAddress.Addr:=StrToNetAddr(Address);
  Result:=FRootSock.SendMessage(msg)
end;

function TLUdp.Send(const aData; const aSize: Integer; aSocket: TLSocket): Integer;
begin
  Result:=FRootSock.Send(aData, aSize)
end;

function TLUdp.Send(const aData; const aSize: Integer; const Address: string
  ): Integer;
begin
  FRootSock.FPeerAddress.Addr:=StrToNetAddr(Address);
  Result:=FRootSock.Send(aData, aSize);
end;

//******************************TLTcp**********************************

constructor TLTcp.Create(aOwner: TComponent);
begin
  Create(aOwner, nil)
end;

constructor TLTcp.Create(aOwner: TComponent; SocketClass: TLSocketClass);
begin
  inherited Create(aOwner, SocketClass);
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
  FRootSock:=FSocketType.Create;
  FRootSock.Parent:=Self;
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
    FRootSock:=FSocketType.Create;
    FRootSock.Parent:=Self;
    FRootSock.FIgnoreShutdown:=True;
    if FRootSock.Listen(APort) then begin
      FRootSock.FConnected:=True;
      FRootSock.FServerSocket:=True;
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
    if TomWinSock.getpeername(FHandle, a, l) <> 0 then
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
  Tmp:=FSocketType.Create;
  Tmp.Parent:=Self;
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

