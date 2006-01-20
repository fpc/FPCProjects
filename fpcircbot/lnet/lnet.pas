{ lNet v2.3.0

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

  1. Optimize "max" in CallSelect in TLTcp

}

unit lNet;

{$mode objfpc}{$H+}
{$interfaces corba}
{$define nonblocking}

interface

uses
  {$ifdef unix} BaseUnix, NetDB, {$else} TomWinsock, {$endif}
  SysUtils, Contnrs, Sockets, Classes;

const
  { Default Values }
  BUFFER_SIZE = 65536;
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
  PROTO_TCP =  6;
  PROTO_UDP = 17;

type
  { Base socket class, Holds Address and socket info, perForms basic
    socket operations, each has it's own Data buffer for Receives }

  { TLBaseSocket }

  TLBaseSocket = class
   protected
    FAddress: TInetSockAddr;
    FFdSet: TFDSet;
    FPeerAddress: TInetSockAddr;
    FConnected: Boolean;
    FConnecting: Boolean;
    FHandle: Integer;
    FFlag: Integer;
    FProtocol: Integer;
    FIgnoreShutdown: Boolean;
   protected
    function SetupSocket(const APort: Word; const Address: string): Boolean; virtual;
    function GetPort: Word;
    function GetPeerAddress: string;
    function GetLocalAddress: string;
    function CanSend: Boolean; virtual;
    procedure SetNonBlock; virtual;
    procedure Bail(const msg: string; const ernum: Integer);
    procedure LogError(const msg: string; const ernum: Integer); virtual;
   public
    constructor Create(const stype, protocol: Byte); virtual;
    destructor Destroy; override;
    function Listen(const APort: Word): Boolean;
    function Accept(const SerSock: Integer): Boolean;
    function Connect(const Address: string; const APort: Word): Boolean;
    function Send(const aData; const aSize: Integer): Integer;
    function SendMessage(const msg: string): Integer;
    function Get(var aData; const aSize: Integer): Integer;
    function GetMessage(out msg: string): Integer;
    procedure Disconnect; virtual;
   public
    property Connected: Boolean read FConnected;
    property Connecting: Boolean read FConnecting;
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
    FPrev: TLSocket;
    FNext: TLSocket;
    FSnum: Integer;
    FCanSend: Boolean;     // this is for automagical sending too
    function CanSend: Boolean; override;
    procedure LogError(const msg: string; const ernum: Integer); override;
    procedure ParentCall(const anAction: TLActionEnum);
    procedure Disconnect; override;
   public
    constructor Create(const sType, Protocol: Byte; Owner: TLConnection;
                       const aSocketNumber: Integer); virtual; overload;
    property Prev: TLSocket read FPrev write FPrev;
    property Next: TLSocket read FNext write FNext;
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
  
  { Interface for all clients }

  { TLConnection
    Common ancestor for TLTcp and TLUdp classes. Holds Event properties
    and common variables. }

  TLConnection = class(TComponent, ILServer, ILClient)
   protected
    FSerSock: TLSocket;
    FTimeVal: TTimeVal;
    FOnReceive: TLProc;
    FOnError: TLErrorProc;
    FOnDisconnect: TLProc;
    FOnCanSend: TLProc;
    FSocketType: TLSocketClass;
    FIterator: TLSocket;
    FID: Integer; // internal number for server
    function GetConnected: Boolean; virtual; abstract;
    function GetCount: Integer; virtual;
    function GetItem(const i: Integer): TLSocket;
    function GetBlockTime: DWord;
    procedure ControlAction(var aSocket: TLSocket; const anAction: TLActionEnum); virtual; abstract;
    procedure SetBlockTime(const Value: DWord);
    procedure CanSend(aSocket: TLSocket); virtual;
   public
    constructor Create(aOwner: TComponent); override;
    constructor Create(aOwner: TComponent; SocketClass: TLSocketClass);
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
  end;
  
  { UDP Client/Server class. Provided to enable usage of UDP sockets }

  { TLUdp }

  TLUdp = class(TLConnection)
   protected
    function GetConnected: Boolean; override;
    procedure ControlAction(var aSocket: TLSocket; const anAction: TLActionEnum); override;
    procedure ControlSock(var aSocket: TLSocket);
    procedure Bail(const msg: string);
   public
    constructor Create(aOwner: TComponent); override;
    constructor Create(aOwner: TComponent; SocketClass: TLSocketClass);
    destructor Destroy; override;
    function Connect(const Address: string; const APort: Word): Boolean; override;
    function Listen(const APort: Word): Boolean; override;
    function Get(var aData; const aSize: Integer; aSocket: TLSocket = nil): Integer; override;
    function GetMessage(out s: string; aSocket: TLSocket = nil): Integer; override;
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
    FLast: TLSocket;
    FCount: Integer;
    FReadFDSet: TFDSet;
    FWriteFDSet: TFDSet;
    FErrorFDSet: TFDSet;
    FOnAccept: TLProc;
    FOnConnect: TLProc;
    function GetConnected: Boolean; override;
    procedure ControlAction(var aSocket: TLSocket; const anAction: TLActionEnum); override;
    procedure ControlSock(var aSocket: TLSocket);
    procedure Bail(const msg: string; aSocket: TLSocket);
   public
    constructor Create(aOwner: TComponent); override;
    constructor Create(aOwner: TComponent; SocketClass: TLSocketClass);
    destructor Destroy; override;
    function Connect(const Address: string; const APort: Word): Boolean; override;
    function Listen(const aPort: Word): Boolean; override;
    function Get(var aData; const aSize: Integer; aSocket: TLSocket = nil): Integer; override;
    function GetMessage(out s: string; aSocket: TLSocket = nil): Integer; override;
    function Send(const aData; const aSize: Integer; aSocket: TLSocket = nil): Integer; override;
    function SendMessage(const msg: string; aSocket: TLSocket = nil): Integer; override;
    function IterNext: Boolean; override;
    procedure Disconnect; override;
    procedure DisconnectSocket(var aSocket: TLSocket);
    procedure IterReset; override;
    procedure CallAction; override;
    property OnAccept: TLProc read FOnAccept write FOnAccept;
    property OnConnect: TLProc read FOnConnect write FOnConnect;
  end;
  
  { Base functions }
  function StrToHostAddr(IP: string): Cardinal;
  function HostAddrToStr(Entry: Cardinal): string;
  function StrToNetAddr(IP: string): Cardinal;
  function NetAddrToStr(Entry: Cardinal): string;
  {$IFDEF WIN32}
  function fpSelect(const nfds: Integer; const readfds, writefds, exceptfds: PFDSet;
                    const timeout: PTimeVal): Integer; inline;
  function fpFD_ISSET(const Socket: Integer; var FDSet: TFDSet): Integer; inline;
  procedure fpFD_SET(const Socket: Integer; var FDSet: TFDSet); inline;
  procedure fpFD_ZERO(var FDSet: TFDSet); inline;
  {$ENDIF}
  { DNS }
  function GetHostName(const Address: string): string;
  function GetHostIP(const Name: string): string;

var
  ForceUTF8: Boolean;
  
implementation

{$i lfunc.inc}  // the borrowed functions from inet.pp

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
  Tmp:=FSerSock;
  Jumps:=0;
  while Assigned(Tmp.Next) and (Jumps < i) do begin
    Tmp:=Tmp.Next;
    Inc(Jumps);
  end;
  if Jumps = i then
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

procedure TLConnection.CanSend(aSocket: TLSocket);
begin
  if Assigned(FOnCanSend) then
    FOnCanSend(aSocket);
end;

///*******************************TLUdp*********************************

constructor TLUdp.Create(aOwner: TComponent);
begin
  Create(aOwner, nil)
end;

constructor TLUdp.Create(aOwner: TComponent; SocketClass: TLSocketClass);
begin
  inherited Create(aOwner, SocketClass);
  FTimeVal.tv_usec:=0;
  FTimeVal.tv_sec:=0;
  FSerSock:=FSocketType.Create(SOCK_DGRAM, PROTO_UDP, Self, 1);
  FIterator:=FSerSock;
end;

destructor TLUdp.Destroy;
begin
  Disconnect;
  FSerSock.Free;
  inherited Destroy;
end;

procedure TLUdp.Disconnect;
begin
  FSersock.Disconnect;
end;

function TLUdp.Listen(const APort: Word): Boolean;
begin
  Result:=False;
  if FSerSock.Connected then
    Disconnect;
  if FSerSock.Listen(APort) then begin
    FSersock.FPeerAddress:=FSerSock.FAddress;
    FSersock.FPeerAddress.Addr:=StrToNetAddr(LADDR_BR);
    FSersock.FConnected:=True;
  end;
  Result:=FSerSock.Connected;
end;

function TLUdp.Connect(const Address: string; const APort: Word): Boolean;
var
  IP: string;
begin
  Result:=False;
  if FSerSock.Connected then Disconnect;
  IP:=GetHostIP(Address);
  if Length(IP) = 0 then
    IP:=Address;
  Result:=FSerSock.SetupSocket(APort, LADDR_ANY);
  FSerSock.FPeerAddress:=FSerSock.FAddress;
  FSerSock.FPeerAddress.Addr:=StrToNetAddr(IP);
  FSersock.FConnected:=true;
end;

procedure TLUdp.Bail(const msg: string);
begin
  Disconnect;
  if Assigned(FOnError) then
    FOnError(msg, FSerSock);
end;

procedure TLUdp.ControlSock(var aSocket: TLSocket);
var
  WSet, RSet, ESet: TFDSet;
  n: Integer;
  TempVal: TTimeVal;
begin
  TempVal:=FTimeVal;
  fpFD_ZERO(RSet);
  fpFD_ZERO(ESet);
  fpFD_ZERO(WSet);
  fpFD_SET(FSerSock.FHandle, RSet);
  fpFD_SET(FSerSock.FHandle, ESet);
  if not FSerSock.FCanSend then
    fpFD_SET(FSerSock.FHandle, WSet);
  n:=fpSelect(FSerSock.FHandle+1, @RSet, @WSet, @ESet, @TempVal);
  if n < 0 then
    Bail('Error on select: ' + StrError(SocketError));
  if n > 0 then begin
    if  (fpFD_ISSET(FSerSock.FHandle, WSet) <> 0)
    and (fpFD_ISSET(FSerSock.FHandle, ESet) = 0) then
      ControlAction(FSerSock, acSend);

    if  FSerSock.Connected
    and (fpFD_ISSET(FSerSock.FHandle, RSet) <> 0)
    and (fpFD_ISSET(FSerSock.FHandle, ESet) = 0) then
      ControlAction(FSerSock, acReceive);
      
    if FSerSock.Connected
    and (fpFD_ISSET(FSerSock.FHandle, ESet) <> 0) then
      ControlAction(FSerSock, acError);
  end;
end;

procedure TLUdp.ControlAction(var aSocket: TLSocket; const anAction: TLActionEnum);
begin
  case anAction of
    acSend    : if Assigned(FOnCanSend) then
                  FOnCanSend(FSerSock);
    
    acReceive : if Assigned(FOnReceive) then
                  FOnReceive(FSerSock);
                  
    acError   : Bail('Error' + StrError(SocketError));
  end;
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
  if FSerSock.Connected then
    ControlSock(FSerSock);
end;

function TLUdp.GetConnected: Boolean;
begin
  Result:=FSerSock.Connected;
end;

function TLUdp.Get(var aData; const aSize: Integer; aSocket: TLSocket): Integer;
begin
  Result:=FSerSock.Get(aData, aSize);
end;

function TLUdp.GetMessage(out s: string; aSocket: TLSocket): Integer;
begin
  Result:=FSerSock.GetMessage(s);
end;

function TLUdp.SendMessage(const msg: string; aSocket: TLSocket = nil): Integer;
begin
  Result:=FSerSock.SendMessage(msg)
end;

function TLUdp.SendMessage(const msg: string; const Address: string): Integer;
begin
  FSersock.FPeerAddress.Addr:=StrToNetAddr(Address);
  Result:=FSerSock.SendMessage(msg)
end;

function TLUdp.Send(const aData; const aSize: Integer; aSocket: TLSocket): Integer;
begin
  Result:=FSerSock.Send(aData, aSize)
end;

function TLUdp.Send(const aData; const aSize: Integer; const Address: string
  ): Integer;
begin
  FSersock.FPeerAddress.Addr:=StrToNetAddr(Address);
  Result:=FSerSock.Send(aData, aSize);
end;

//******************************TLTCP**********************************

constructor TLTcp.Create(aOwner: TComponent);
begin
  Create(aOwner, nil)
end;

constructor TLTcp.Create(aOwner: TComponent; SocketClass: TLSocketClass);
begin
  inherited Create(aOwner, SocketClass);
  FIterator:=nil;
  FCount:=0;
  FSerSock:=FSocketType.Create(SOCK_STREAM, PROTO_TCP, Self, 0);
  FSerSock.FIgnoreShutdown:=True;
  FLast:=FSerSock;
  FOnAccept:=nil;
  FOnConnect:=nil;
end;

destructor TLTcp.Destroy;
begin
  Disconnect;
  FSerSock.Free;
  inherited Destroy;
end;

procedure TLTcp.Disconnect;
var
  Tmp, aSocket: TLSocket;
begin
  aSocket:=FSerSock.Next;
  while Assigned(aSocket) do begin
    Tmp:=aSocket;
    aSocket:=aSocket.Next;
    Tmp.Free;
  end;

  FSerSock.Next:=nil;
  FSerSock.Disconnect;
  FLast:=FSerSock;
  FCount:=0;
  FIterator:=nil;
end;

procedure TLTcp.DisconnectSocket(var aSocket: TLSocket);
begin
  if aSocket = FIterator then begin
    if Assigned(FIterator.Next) then
      FIterator:=FIterator.Next
    else if Assigned(FIterator.Prev) and (FIterator.Prev <> FSerSock) then
      FIterator:=FIterator.Prev
    else IterReset;
  end;

  if aSocket <> FSerSock then begin
    if aSocket = FLast then
      FLast:=aSocket.Prev;
    aSocket.Prev.Next:=aSocket.Next;
    if Assigned(aSocket.Next) then
      aSocket.Next.Prev:=aSocket.Prev;
    aSocket.Free;
    Dec(FCount);
  end else FSerSock.Disconnect;
  aSocket:=nil;
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
  if Assigned(FIterator.Next) then begin
    FIterator:=FIterator.Next;
    Result:=True;
  end else IterReset;
end;

procedure TLTcp.IterReset;
begin
  FIterator:=FSerSock.Next;
end;

function TLTcp.Listen(const aPort: Word): Boolean;
begin
  Result:=false;
  if (aPort > 0) and not FSerSock.Connected then begin
    if Assigned(FSerSock.Next) then Disconnect;
    if FSerSock.Listen(APort) then begin
      FSerSock.FConnected:=true;
      Result:=true;
    end;
  end else Bail('Error, already listening', nil);
end;

function TLTcp.Connect(const Address: string; const APort: Word): Boolean;
begin
  Result:=False;
  if Assigned(FSerSock.Next)
  or FSerSock.Connected then
    Disconnect;
  FSersock.Next:=FSocketType.Create(SOCK_STREAM, PROTO_TCP, Self, 1);
  FLast:=FSerSock.Next;
  FLast.Prev:=FSerSock;
  Result:=FLast.Connect(Address, aPort);
  if Result then begin
    Inc(FCount);
    FIterator:=FLast;
  end else begin
    FLast.Free;
    FLast:=FSerSock;
    FSerSock.Next:=nil;
  end;
end;

procedure TLTcp.CallAction;

  function CallSelect: Boolean;
  var
    i: Integer;
    max: Integer;
    TempVal: TTimeVal;
    Tmp: TLSocket;
  begin
    result:=False;
    TempVal:=FTimeVal;
    max:=0;
    fpFD_ZERO(FReadFDSet);
    fpFD_ZERO(FWriteFDSet);
    fpFD_ZERO(FErrorFDSet);
    
    if FSerSock.Connected then begin
      max:=FSerSock.FHandle;
      fpFD_SET(FSerSock.FHandle, FReadFDSet);
      fpFD_SET(FSerSock.FHandle, FErrorFDSet);
    end;
    
    if Assigned(FSerSock.Next) then begin
      Tmp:=FSerSock.Next;
      repeat
        if Tmp.Connected or Tmp.Connecting then begin
          fpFD_SET(Tmp.FHandle, FReadFDSet);
          fpFD_SET(Tmp.FHandle, FErrorFDSet);
          if not Tmp.FCanSend then
            fpFD_SET(Tmp.FHandle, FWriteFDSet);
          if Tmp.FHandle > max then
            max:=Tmp.FHandle;
          Tmp:=Tmp.Next;
        end else begin
          Tmp:=Tmp.Next;
          DisconnectSocket(Tmp);
        end;
      until not Assigned(Tmp);
    end;
    
    if max > 0 then
      i:=fpSelect(max+1, @FReadFDSet, @FWriteFDSet, @FErrorFDSet, @TempVal)
    else
      i:=0;

    if i < 0 then
      Bail('Error on select: ' + StrError(SocketError), nil);

    if i = 0 then result:=False;
    if i > 0 then result:=True;
  end;
  
var
  Tmp, Tmp2: TLSocket;
begin
  if (FSerSock.Connected or Assigned(FSerSock.Next)) and CallSelect then begin
    Tmp:=FSerSock;
    repeat
      Tmp2:=Tmp.Next;
      ControlSock(Tmp);
      Tmp:=Tmp2;
    until not Assigned(Tmp);
  end;
end;

procedure TLTcp.ControlAction(var aSocket: TLSocket; const anAction: TLActionEnum);
var
  {$ifdef win32}
  a: TSockAddrin;
  {$else}
  a: TInetSockAddr;
  {$endif}
  l: Integer;
begin
  case anAction of
    acSend    : begin
                  aSocket.FCanSend:=True;
                  if Assigned(FOnCanSend) then
                    FOnCanSend(aSocket);
                end;
                
    acReceive : if aSocket.Connected then begin
                  if Assigned(FOnReceive) then
                    FOnReceive(aSocket);
                  if not aSocket.Connected then begin
                    if Assigned(FOnDisconnect) then
                      FOnDisconnect(aSocket);
                    DisconnectSocket(aSocket);
                  end;
                end;

    acConnect : begin
                  l:=SizeOf(a);
                  {$ifndef mswindows}
                  if fpgetpeername(aSocket.Handle, @a, @l) <> 0 then
                  {$else}
                  if TomWinSock.getpeername(aSocket.Handle, a, l) <> 0 then
                  {$endif}
                    Bail('Error on connect: connection refused', aSocket)
                  else begin
                    aSocket.FConnected:=True;
                    aSocket.FConnecting:=False;
                    if Assigned(FOnConnect) then
                      FOnConnect(aSocket);
                  end;
                end;

    acAccept  : begin
                  FLast.Next:=FSocketType.Create(SOCK_STREAM, PROTO_TCP, Self, 1);
                  if FLast.Next.Accept(FSerSock.FHandle) then begin
                    FLast.Next.Prev:=FLast;
                    FLast:=FLast.Next;
                    if not Assigned(FIterator) then
                      FIterator:=FLast;
                    Inc(FCount);
                    if Assigned(FOnAccept) then
                      FOnAccept(FLast);
                  end else begin
                    FLast.Next.Free;
                    FLast.Next:=nil;
                  end;
                end;
                
    acError   : if aSocket.Connecting then
                  Bail('Error on connect: connection refused' , aSocket)
                else
                  Bail('Error: ' + StrError(SocketError), aSocket);
  end;
end;

procedure TLTcp.ControlSock(var aSocket: TLSocket);
begin
  if fpFD_ISSET(aSocket.FHandle, FErrorFDSet) <> 0 then
    ControlAction(aSocket, acError);

  if  Assigned(aSocket) and (aSocket = FSerSock)
  and (fpFD_ISSET(aSocket.FHandle, FReadFDSet) <> 0) then
    ControlAction(aSocket, acAccept);

  if Assigned(aSocket) and (aSocket <> FSerSock) then begin
    if Assigned(aSocket) and aSocket.Connected then begin
      if  ((fpFD_ISSET(aSocket.FHandle, FWriteFDSet) <> 0)
      and  (fpFD_ISSET(aSocket.FHandle, FErrorFDSet) = 0)) then
        ControlAction(aSocket, acSend);

      if  (Assigned(aSocket) and (fpFD_ISSET(aSocket.FHandle, FReadFDSet) <> 0)
      and ( fpFD_ISSET(aSocket.FHandle, FErrorFDSet) = 0)) then
        ControlAction(aSocket, acReceive);
    end;

    if Assigned(aSocket) and aSocket.Connecting then
      if fpFD_ISSET(aSocket.FHandle, FWriteFDSet) <> 0 then
        ControlAction(aSocket, acConnect);
  end;
end;

function TLTcp.GetConnected: Boolean;
var
  Tmp: TLSocket;
begin
  Result:=False;
  Tmp:=FSerSock.FNext;
  if FSerSock.Connected then
    Result:=True
  else while Assigned(Tmp) do begin
    if Tmp.Connected then begin
      Result:=True;
      Exit;
    end else Tmp:=Tmp.Next;
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

function TLTcp.GetMessage(out s: string; aSocket: TLSocket): Integer;
begin
  Result:=0;
  if not Assigned(aSocket) then
    aSocket:=FIterator;
  if Assigned(aSocket) then
    Result:=aSocket.GetMessage(s);
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

//********************************TLBaseSocket*************************************

constructor TLBaseSocket.Create(const stype, protocol: Byte);
begin
  FConnected:=False;
  FConnecting:=False;
  FIgnoreShutdown:=False;
  FFlag:=stype;
  FProtocol:=protocol;
end;

destructor TLBaseSocket.Destroy;
begin
  Disconnect;
end;

procedure TLBaseSocket.Disconnect;
begin
  if Connected or FConnecting then begin
    if (FFlag = SOCK_STREAM) and (not FIgnoreShutdown) and FConnected then
      if ShutDown(FHandle, 2) <> 0 then
        LogError('Shutdown error', SocketError);
    if Closesocket(FHandle) <> 0 then
      LogError('Closesocket error', SocketError);
    FConnected:=false;
  end;
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
  if FFlag = SOCK_STREAM then
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
begin
  Result:=False;
  fpFD_ZERO(FFDSet);
  fpFD_SET(FHandle, FFDSet);
  if fpSelect(FHandle+1, nil, @FFDSet, nil, nil) < 0 then
    Bail('Error on select', SocketError)
  else begin
    if fpFD_ISSET(FHandle, FFDSet) <> 0 then
      Result:=True;
  end;
end;

function TLBaseSocket.GetMessage(out msg: string): Integer;
begin
  Result:=0;
  SetLength(msg, BUFFER_SIZE);
  SetLength(msg, Get(PChar(msg)^, Length(msg)));
  Result:=Length(msg);
end;

function TLBaseSocket.Get(var aData; const aSize: Integer): Integer;
var
  AddressLength: Integer = SizeOf(FAddress);
begin
  Result:=0;
  if Connected then begin
    if FFlag = SOCK_STREAM then
    {$ifdef win32}
      Result:=tomwinsock.Recv(FHandle, aData, aSize, LMSG)
    else
      Result:=tomwinsock.Recvfrom(FHandle, aData, aSize, LMSG, TSockAddrIn(FPeerAddress), AddressLength);
    {$else}
      Result:=sockets.Recv(FHandle, aData, aSize, LMSG)
    else
      Result:=sockets.Recvfrom(FHandle, aData, aSize, LMSG, FPeerAddress, AddressLength);
    {$endif}
    if Result = 0 then Bail('Lost Connection', -1);
    if Result < 0 then Bail('Receive Error', SocketError);
  end;
  if not Connected then
    Result:=0; // if it failed subsequently
end;

function TLBaseSocket.SetupSocket(const APort: Word; const Address: string): Boolean;
var
  Done: Boolean;
begin
  Result:=false;
  if not Connected then begin
    Done:=true;
    FHandle:=fpsocket(AF_INET, FFlag, FProtocol);
    if FHandle < 0 then bail('Socket error', SocketError);
    SetNonBlock;
    if FFlag = SOCK_DGRAM then
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
      Bail('Error on bind', SocketError) else Result:=true;
    if (FFlag = SOCK_STREAM) and Result then
      if fpListen(FHandle, 5) < 0 then
        Bail('Error on Listen', SocketError) else Result:=true;
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

  function DoSend(const TheData; const TheSize: Integer): Integer;
  begin
    if FFlag = SOCK_STREAM then
    {$ifdef win32}
      Result:=tomwinsock.send(FHandle, TheData, TheSize, LMSG)
    else
      Result:=tomwinsock.sendto(FHandle, TheData, TheSize, LMSG, TSockAddrIn(FPeerAddress), SizeOf(FPeerAddress));
    {$else}
      Result:=sockets.send(FHandle, TheData, TheSize, LMSG)
    else
      Result:=sockets.sendto(FHandle, TheData, TheSize, LMSG, FPeerAddress, SizeOf(FPeerAddress));
    {$endif}
  end;
  
begin
  Result:=0;
  if aSize > BUFFER_SIZE then
    Bail('Send error: Message bigger than buffersize', -1);
  if aSize <= 0 then
    Bail('Send error: wrong size (Size <= 0)', -1);

  if Connected and CanSend then begin
    Result:=DoSend(aData, aSize);
    if Result <= 0 then
      Bail('Send error', socketerror);
  end;
end;

procedure TLBaseSocket.SetNonBlock;
var
  opt: Integer;
begin
  {$ifdef win32}
   opt:=1;
   if ioctlsocket(FHandle, FIONBIO, opt) < 0 then
     bail('Error on SetFD', wsaGetLasterror);
  {$else}
   opt:=fpfcntl(FHandle, F_GETFL);
   if opt < 0 then begin
     bail('ERROR on GetFD', socketerror);
     Exit;
   end;

   if fpfcntl(FHandle, F_SETFL, opt or O_NONBLOCK) < 0 then
     bail('Error on SetFL', socketerror);
  {$endif}
end;

//********************************TLSocket***********************************

constructor TLSocket.Create(const sType, Protocol: Byte; Owner: TLConnection;
                            const aSocketNumber: Integer);
begin
  inherited Create(stype, protocol);
  FParent:=Owner;
  FSnum:=aSocketNumber;
  FPrev:=nil;
  FNext:=nil;
  FCanSend:=False;
end;

function TLSocket.CanSend: Boolean;
begin
  Result:=FCanSend or FConnected;
  FCanSend:=False;
end;

procedure TLSocket.LogError(const msg: string; const ernum: Integer);
begin
  if Assigned(FParent) then
    if (ernum > 0) and Assigned(FParent.FOnError) then
      FParent.FOnError(msg + ': ' + StrError(ernum, ForceUTF8), Self)
end;

procedure TLSocket.ParentCall(const anAction: TLActionEnum);
var
  TempSelf: TLSocket;
begin
  TempSelf:=Self; // let's not get suicidal, self:=nil after self.free is crash :)
  FParent.ControlAction(TempSelf, anAction);
end;

procedure TLSocket.Disconnect;
begin
  inherited Disconnect;
  FCanSend:=False;
end;

end.

