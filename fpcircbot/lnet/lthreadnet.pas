{ lNet v2.1.2

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

unit lThreadNet;

{$mode objfpc}{$H+}

interface

uses
  Classes, lNet;
  
const
  ERROR_NOTASSIGNED  = 'Network module not assigned';
  ERROR_NOTSUSPENDED = 'Thread already in use';
  ERROR_NOTACTIVE    = 'Thread not active';
  
type
  TLSocket = lNet.TLSocket;

  TLObjProcPure = procedure of object;

  TLConnectionThread = class(TThread)
   protected
    FActive: Boolean;
    FInitialized: Boolean;
    FNet: TLConnection;
    FOnError: TLObjProcPure;
    FOnReceive: TLObjProcPure;
    FOnDisconnect: TLObjProcPure;
    FOnAccept: TLObjProcPure;
    FSleeping: Boolean;
    FLastMessage: string;
    FLastSocket: TLSocket;
    FLastError: string;
    function GetItem(const i: Longint): TLSocket;
    function GetBufferSize: Longint;
    function GetMaxMsgs: Longint;
    function GetBlockTime: DWord;
    function GetForceUTF8: Boolean;
    procedure SetBufferSize(const Value: Longint);
    procedure SetMaxMsgs(const Value: Longint);
    procedure SetBlockTime(const Value: DWord);
    procedure SetForceUTF8(const Value: Boolean);
    procedure StartThread;
    procedure WaitForSleep;
   protected
    procedure OnEr(const msg: string; aSocket: TLSocket);
    procedure OnRe(const msg: string; aSocket: TLSocket);
    procedure OnDs(const msg: string; aSocket: TLSocket);
    procedure OnAc(const msg: string; aSocket: TLSocket);
   public
    constructor Create(Interval: Integer);
    destructor Destroy; override;
    function Connect(const APort: Word; const Address: string): Boolean;
    function Listen(const APort: Word): Boolean;
    function SendMessage(const msg: string; aSocket: TLSocket = nil): Boolean;
    function Disconnect(aSocket: TLSocket = nil): Boolean;
    procedure Execute; override;
   public
    property OnAccept: TLObjProcPure read FOnAccept write FOnAccept;
    property OnError: TLObjProcPure read FOnError write FOnError;
    property OnReceive: TLObjProcPure read FOnReceive write FOnReceive;
    property OnDisconnect: TLObjProcPure read FOnDisconnect write FOnDisconnect;
    property BufferSize: LongInt read GetBufferSize write SetBufferSize;
    property MaxMsgs: LongInt read GetMaxMsgs write SetMaxMsgs;
    property BlockTime: DWord read GetBlockTime write SetBlockTime;
    property LastMessage: string read FLastMessage;
    property LastSocket: TLSocket read FLastSocket;
    property LastError: string read FLastError;
    property Active: Boolean read FActive;
    property ForceUTF8: Boolean read GetForceUTF8 write SetForceUTF8;
    property Socks[index: Integer]: TLSocket read GetItem; default;
  end;
  
  TLTCPThread = class(TLConnectionThread)
   protected
    function GetSocketCount: Integer;
    function GetMaxSockets: Integer;
    procedure SetMaxSockets(const Value: Integer);
   public
    constructor Create(Interval: Integer);
    property MaxSockets: Integer read GetMaxSockets write SetMaxSockets;
    property SocketCount: Integer read GetSocketCount;
  end;
  
  TLUDPThread = class(TLConnectionThread)
   public
    constructor Create(Interval: Integer);
    function SendMessage(const msg, Address: string): Boolean;
    function GetPeerAddress: string;
  end;
  
implementation

uses SysUtils;

constructor TLConnectionThread.Create(Interval: Integer);
begin
  FActive:=False;
  FInitialized:=False;
  FSleeping:=False;
  FOnAccept:=nil;
  FOnError:=nil;
  FOnDisconnect:=nil;
  FOnReceive:=nil;
  FLastMessage:='';
  FLastError:='';
  FLastSocket:=nil;
  FNet:=nil;
end;

destructor TLConnectionThread.Destroy;
const
  TIMEOUT = 50; // ms
begin
  if FActive then
    Disconnect;
    
  if FInitialized then begin
    Terminate;
    Sleep(TIMEOUT); // hackycracky
    WaitFor;
    inherited Destroy;
  end;
  
  if Assigned(FNet) then
    FNet.Free;
end;

function TLConnectionThread.GetItem(const i: Longint): TLSocket;
begin
  Result:=FNet[i];
end;

function TLConnectionThread.GetBufferSize: Longint;
begin
  Result:=-1;
  if Assigned(FNet) then
    Result:=FNet.BufferSize;
end;

function TLConnectionThread.GetMaxMsgs: Longint;
begin
  Result:=-1;
  if Assigned(FNet) then
    Result:=FNet.MaxMsgs;
end;

function TLConnectionThread.GetBlockTime: DWord;
begin
  Result:=0;
  if Assigned(FNet) then
    Result:=FNet.BlockTime;
end;

function TLConnectionThread.GetForceUTF8: Boolean;
begin
  Result:=False;
  if Assigned(FNet) then
    Result:=FNet.ForceUTF8;
end;

procedure TLConnectionThread.SetBufferSize(const Value: Longint);
begin
  if FSleeping and Assigned(FNet) then
    FNet.BufferSize:=Value;
end;

procedure TLConnectionThread.SetMaxMsgs(const Value: Longint);
begin
  if FSleeping and Assigned(FNet)then
    FNet.MaxMsgs:=Value;
end;

procedure TLConnectionThread.SetBlockTime(const Value: DWord);
begin
  if FSleeping and Assigned(FNet)then
    FNet.BlockTime:=Value;
end;

procedure TLConnectionThread.SetForceUTF8(const Value: Boolean);
begin
  if FSleeping and Assigned(FNet) then
    FNet.ForceUTF8:=Value;
end;

procedure TLConnectionThread.StartThread;
begin
  inherited Create(True);
  FInitialized:=True;
end;

procedure TLConnectionThread.WaitForSleep;
const
  MAXTIME  = 1000;
  INTERVAL = 20;
var
  ElapsedTime: DWord;
begin
  FActive:=False;
  ElapsedTime:=0;
  while not FSleeping and (ElapsedTime < MAXTIME) do begin
    Sleep(INTERVAL);
    Inc(ElapsedTime, INTERVAL);
  end;
end;

// begin callbacks

procedure TLConnectionThread.OnEr(const msg: string; aSocket: TLSocket);
begin
  FLastMessage:=msg;
  FLastSocket:=aSocket;
  if Assigned(FOnError) then
    Synchronize(FOnError);
end;

procedure TLConnectionThread.OnRe(const msg: string; aSocket: TLSocket);
begin
  FLastMessage:=msg;
  FLastSocket:=aSocket;
  if Assigned(FOnReceive) then
    Synchronize(FOnReceive);
end;

procedure TLConnectionThread.OnDs(const msg: string; aSocket: TLSocket);
begin
  FLastMessage:=msg;
  FLastSocket:=aSocket;
  if Assigned(FOnDisconnect) then
    Synchronize(FOnDisconnect);
end;

procedure TLConnectionThread.OnAc(const msg: string; aSocket: TLSocket);
begin
  FLastMessage:=msg;
  FLastSocket:=aSocket;
  if Assigned(FOnAccept) then
    Synchronize(FOnAccept);
end;

// end callbacks

function TLConnectionThread.Connect(const APort: Word; const Address: string): Boolean;
begin
  Result:=False;
  if FActive then
    Disconnect;

  if not FInitialized then
    StartThread;
    
  if Assigned(FNet) then
    Result:=FNet.Connect(APort, Address)
  else FLastError:=ERROR_NOTASSIGNED;
  
  if Result then begin
    FSleeping:=False;
    FActive:=True;
    if Suspended then
      Resume;
  end;
end;

function TLConnectionThread.Listen(const APort: Word): Boolean;
begin
  Result:=False;
  if FActive then
    Disconnect;

  if not FInitialized then
    StartThread;
    
  if Assigned(FNet) then
    Result:=FNet.Listen(APort)
  else FLastError:=ERROR_NOTASSIGNED;

  if Result then begin
    FSleeping:=False;
    FActive:=True;
    if Suspended then
      Resume;
  end;
end;

function TLConnectionThread.SendMessage(const msg: string;
                                        aSocket: TLSocket = nil): Boolean;
begin
  Result:=False;
  if Assigned(FNet) then
    Result:=FNet.SendMessage(msg, aSocket)
  else FLastError:=ERROR_NOTASSIGNED;
end;

function TLConnectionThread.Disconnect(aSocket: TLSocket = nil): Boolean;
begin
  Result:=False;
  if FActive then begin
    WaitForSleep;
    if Assigned(FNet) then
      Result:=FNet.Disconnect(aSocket)
    else FLastError:=ERROR_NOTASSIGNED;
  end;
end;

procedure TLConnectionThread.Execute;
begin
  while not Terminated do begin
    if FActive then
      FNet.CallAction
    else begin
      Sleep(0); // enter alternatives for other OS
      FSleeping:=True;
    end;
  end;
end;

//*****************************TLTCPThread**********************************

constructor TLTCPThread.Create(Interval: Integer);
begin
  inherited Create(Interval); // the interval should get done in there.. noone's perfect
  if Interval < 1 then Interval:=1;
  FNet:=TLTcp.Create;
  FNet.BlockTime:=Interval;
  FNet.OnAccept:=@OnAc;
  FNet.OnDisconnect:=@OnDs;
  FNet.OnError:=@OnEr;
  FNet.OnReceive:=@OnRe;
end;

function TLTCPThread.GetSocketCount: Integer;
begin
  Result:=TLTCP(FNet).Count;
end;

function TLTCPThread.GetMaxSockets: Integer;
begin
  Result:=TLTCP(FNet).MaxSockets;
end;

procedure TLTCPThread.SetMaxSockets(const Value: Integer);
begin
  if FSleeping and Assigned(FNet) then
    TLTCP(FNet).MaxSockets:=Value;
end;

//*****************************TLUDPThread**********************************

constructor TLUDPThread.Create(Interval: Integer);
begin
  inherited Create(Interval); // the interval should get done in there.. noone's perfect
  if Interval < 1 then Interval:=1;
  FNet:=TLUdp.Create;
  FNet.BlockTime:=Interval;
  FNet.OnAccept:=@OnAc;
  FNet.OnDisconnect:=@OnDs;
  FNet.OnError:=@OnEr;
  FNet.OnReceive:=@OnRe;
end;

function TLUDPThread.SendMessage(const msg, Address: string): Boolean;
begin
  Result:=False;
  if Assigned(FNet) then
    Result:=TLUdp(FNet).SendMessage(msg, Address)
  else FLastError:=ERROR_NOTASSIGNED;
end;

function TLUDPThread.GetPeerAddress: string;
begin
  Result:='';
  if Assigned(FNet) then
    Result:=TLUdp(FNet).GetHostAddress
  else FLastError:=ERROR_NOTASSIGNED;
end;

end.

