{ lTelnet CopyRight (C) 2004-2006 Ales Katona

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

unit lTelnet;

{$mode objfpc}{$H+}
//{$define debug}

interface

uses
  Classes, lNet, lControlStack;
  
const
  // Telnet printer signals
  TS_NUL         = #0;
  TS_ECHO        = #1;
  TS_SGA         = #3; // Surpass go-ahead
  TS_BEL         = #7;
  TS_BS          = #8;
  TS_HT          = #9;
  TS_LF          = #10;
  TS_VT          = #11;
  TS_FF          = #12;
  TS_CR          = #13;
  // Telnet control signals
  TS_NAWS        = #31;
  TS_DATA_MARK   = #128;
  TS_BREAK       = #129;
  TS_HYI         = #133; // Hide Your Input
  // Data types codes
  TS_STDTELNET   = #160;
  TS_TRANSPARENT = #161;
  TS_EBCDIC      = #162;
  // Control bytes
  TS_SE          = #240;
  TS_NOP         = #241;
  TS_GA          = #249; // go ahead currently ignored(full duplex)
  TS_SB          = #250;
  TS_WILL        = #251;
  TS_WONT        = #252;
  TS_DO          = #253;
  TS_DONT        = #254;
  // Mother of all power
  TS_IAC         = #255;
  
type
  TLTelnetControlChars = set of Char;
  TLHowEnum = (TE_WILL = 251, TE_WONT, TE_DO, TE_DONW);
  
  { TLTelnet }

  TLTelnet = class(TComponent, ILBase)
   protected
    FStack: TLControlStack;
    FConnection: TLTcp;
    FPossible: TLTelnetControlChars;
    FActive: TLTelnetControlChars;
    FOutput: TMemoryStream;
    FOperation: Char;
    FCommandCharIndex: Byte;
    FOnReceive: TLProc;
    FOnConnect: TLProc;
    FOnDisconnect: TLProc;
    FOnError: TLErrorProc;
    FCommandArgs: string[3];
    FOrders: TLTelnetControlChars;
    FConnected: Boolean;
    function Question(const Command: Char; const Value: Boolean): Char;
    function GetSocketClass: TLSocketClass;
    function GetTimeout: DWord;
    procedure SetSocketClass(Value: TLSocketClass);
    procedure SetTimeout(const Value: DWord);
    procedure StackFull;
    procedure DoubleIAC(var s: string);
    procedure TelnetParse(const msg: string);
    procedure React(const Operation, Command: Char); virtual; abstract;
    procedure SendCommand(const Command: Char; const Value: Boolean); virtual; abstract;
   public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    function Get(var aData; const aSize: Integer; aSocket: TLSocket = nil): Integer; virtual; abstract;
    function GetMessage(out msg: string; aSocket: TLSocket = nil): Integer; virtual; abstract;
    function Send(const aData; const aSize: Integer; aSocket: TLSocket = nil): Integer; virtual; abstract;
    function SendMessage(const msg: string; aSocket: TLSocket = nil): Integer; virtual; abstract;
    function OptionIsSet(const Option: Char): Boolean;
    function RegisterOption(const aOption: Char; const aCommand: Boolean): Boolean;
    procedure SetOption(const Option: Char);
    procedure UnSetOption(const Option: Char);
    procedure Disconnect; virtual;
    procedure CallAction; virtual; abstract;
    property Output: TMemoryStream read FOutput;
    property Connected: Boolean read FConnected;
    property Timeout: DWord read GetTimeout write SetTimeout;
    property OnReceive: TLProc read FOnReceive write FOnReceive;
    property OnDisconnect: TLProc read FOnDisconnect write FOnDisconnect;
    property OnConnect: TLProc read FOnConnect write FOnConnect;
    property OnError: TLErrorProc read FOnError write FOnError;
    property Connection: TLTCP read FConnection;
    property SocketClass: TLSocketClass read GetSocketClass write SetSocketClass;
  end;

  { TLTelnetClient }

  { TLTelnetClient }

  TLTelnetClient = class(TLTelnet, ILClient)
   protected
    FLocalEcho: Boolean;
    procedure OnEr(const msg: string; aSocket: TLSocket);
    procedure OnDs(aSocket: TLSocket);
    procedure OnRe(aSocket: TLSocket);
    procedure OnCo(aSocket: TLSocket);
    procedure React(const Operation, Command: Char); override;
    procedure SendCommand(const Command: Char; const Value: Boolean); override;
   public
    constructor Create(aOwner: TComponent); override;
    function Connect(const anAddress: string; const aPort: Word): Boolean;
    function Get(var aData; const aSize: Integer; aSocket: TLSocket = nil): Integer; override;
    function GetMessage(out msg: string; aSocket: TLSocket = nil): Integer; override;
    function Send(const aData; const aSize: Integer; aSocket: TLSocket = nil): Integer; override;
    function SendMessage(const msg: string; aSocket: TLSocket = nil): Integer; override;
    procedure SendCommand(const aCommand: Char; const How: TLHowEnum);
    procedure CallAction; override;
    property LocalEcho: Boolean read FLocalEcho write FLocalEcho;
  end;
  
implementation

uses
  SysUtils;

var
  zz: Char;
  TNames: array[Char] of string;

//*******************************TLTelnetClient********************************

constructor TLTelnet.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FOnReceive:=nil;
  FOnDisconnect:=nil;
  FOnError:=nil;
  FOnConnect:=nil;
  FConnection:=TLTCP.Create(aOwner);
  FOutput:=TMemoryStream.Create;
  FCommandCharIndex:=0;
  FStack:=TLControlStack.Create;
  FStack.OnFull:=@StackFull;
end;

destructor TLTelnet.Destroy;
begin
  Disconnect;
  FOutput.Free;
  FConnection.Free;
  FStack.Free;
  inherited Destroy;
end;

function TLTelnet.Question(const Command: Char; const Value: Boolean): Char;
begin
  Result:=TS_NOP;
  if Value then begin
    if Command in FOrders then
      Result:=TS_DO
    else
      Result:=TS_WILL;
  end else begin
    if Command in FOrders then
      Result:=TS_DONT
    else
      Result:=TS_WONT;
  end;
end;

function TLTelnet.GetSocketClass: TLSocketClass;
begin
  Result:=FConnection.SocketClass;
end;

function TLTelnet.GetTimeout: DWord;
begin
  Result:=FConnection.Timeout;
end;

procedure TLTelnet.SetSocketClass(Value: TLSocketClass);
begin
  FConnection.SocketClass:=Value;
end;

procedure TLTelnet.SetTimeout(const Value: DWord);
begin
  FConnection.Timeout:=Value;
end;

procedure TLTelnet.StackFull;
begin
  {$ifdef debug}
  Writeln('**STACKFULL**');
  {$endif}
  if FStack[1] = TS_IAC then
    begin
      FOutput.WriteByte(Byte(FStack[1]));
      FOutput.WriteByte(Byte(FStack[2]));
    end else React(FStack[1], FStack[2]);
  FStack.Clear;
end;

procedure TLTelnet.DoubleIAC(var s: string);
var
  i: Longint;
begin
  i:=0;
  if Length(s) > 0 then
    while i < Length(s) do begin
      Inc(i);
      if s[i] = TS_IAC then begin
        Insert(TS_IAC, s, i);
        Inc(i, 2);
      end;
    end;
end;

procedure TLTelnet.TelnetParse(const msg: string);
var
  i: Longint;
begin
  for i:=1 to Length(msg) do
    if (FStack.ItemIndex > 0) or (msg[i] = TS_IAC) then begin
      if msg[i] = TS_GA then
        FStack.Clear
      else
        FStack.Push(msg[i])
    end else
      FOutput.WriteByte(Byte(msg[i]));
end;

function TLTelnet.OptionIsSet(const Option: Char): Boolean;
begin
  Result:=False;
  Result:=Option in FActive;
end;

function TLTelnet.RegisterOption(const aOption: Char;
                                     const aCommand: Boolean): Boolean;
begin
  Result:=False;
  if not (aOption in FPossible) then begin
    FPossible:=FPossible + [aOption];
    if aCommand then
      FOrders:=FOrders + [aOption];
    Result:=True;
  end;
end;

procedure TLTelnet.SetOption(const Option: Char);
begin
  if Option in FPossible then
    SendCommand(Option, True);
end;

procedure TLTelnet.UnSetOption(const Option: Char);
begin
  if Option in FPossible then
    SendCommand(Option, False);
end;

procedure TLTelnet.Disconnect;
begin
  FConnection.Disconnect;
  FConnected:=False;
end;

//****************************TLTelnetClient*****************************

constructor TLTelnetClient.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FConnection.OnError:=@OnEr;
  FConnection.OnDisconnect:=@OnDs;
  FConnection.OnReceive:=@OnRe;
  FConnection.OnConnect:=@OnCo;
  FConnected:=False;
  FPossible:=[TS_ECHO, TS_HYI, TS_SGA];
  FActive:=[];
  FOrders:=[];
end;

procedure TLTelnetClient.OnEr(const msg: string; aSocket: TLSocket);
begin
  if Assigned(FOnError) then
    FOnError(msg, aSocket)
  else
    FOutput.Write(Pointer(msg)^, Length(msg));
end;

procedure TLTelnetClient.OnDs(aSocket: TLSocket);
begin
  if Assigned(FOnDisconnect) then
    FOnDisconnect(aSocket);
end;

procedure TLTelnetClient.OnRe(aSocket: TLSocket);
var
  s: string;
begin
  if aSocket.GetMessage(s) > 0 then begin
    TelnetParse(s);
    if Assigned(FOnReceive) then
      FOnReceive(aSocket);
  end;
end;

procedure TLTelnetClient.OnCo(aSocket: TLSocket);
begin
  FConnected:=True;
  if Assigned(FOnConnect) then
    FOnConnect(aSocket);
end;

procedure TLTelnetClient.React(const Operation, Command: Char);

  procedure Accept(const Operation, Command: Char);
  begin
    FActive:=FActive + [Command];
    {$ifdef debug}
    Writeln('**SENT** ', TNames[Operation], ' ', TNames[Command]);
    {$endif}
    FConnection.SendMessage(TS_IAC + Operation + Command);
  end;
  
  procedure Refuse(const Operation, Command: Char);
  begin
    FActive:=FActive - [Command];
    {$ifdef debug}
    Writeln('**SENT** ', TNames[Operation], ' ', TNames[Command]);
    {$endif}
    FConnection.SendMessage(TS_IAC + Operation + Command);
  end;
  
begin
  {$ifdef debug}
  Writeln('**GOT** ', TNames[Operation], ' ', TNames[Command]);
  {$endif}
  case Operation of
    TS_DO   : if Command in FPossible then Accept(TS_WILL, Command)
              else Refuse(TS_WONT, Command);
              
    TS_DONT : if Command in FPossible then Refuse(TS_WONT, Command);
    
    TS_WILL : if Command in FPossible then FActive:=FActive + [Command]
              else Refuse(TS_DONT, Command);
                 
    TS_WONT : if Command in FPossible then FActive:=FActive - [Command];
  end;
end;

procedure TLTelnetClient.SendCommand(const Command: Char; const Value: Boolean);
begin
  if FConnected then begin
    {$ifdef debug}
    Writeln('**SENT** ', TNames[Question(Command, Value)], ' ', TNames[Command]);
    {$endif}
    case Question(Command, Value) of
      TS_WILL : FActive:=FActive + [Command];
    end;
    FConnection.SendMessage(TS_IAC + Question(Command, Value) + Command);
  end;
end;

function TLTelnetClient.Connect(const anAddress: string; const aPort: Word): Boolean;
begin
  Result:=FConnection.Connect(anAddress, aPort);
end;

function TLTelnetClient.Get(var aData; const aSize: Integer; aSocket: TLSocket): Integer;
begin
  Result:=FOutput.Read(aData, aSize);
  if FOutput.Position = FOutput.Size then
    FOutput.Clear;
end;

function TLTelnetClient.GetMessage(out msg: string; aSocket: TLSocket): Integer;
begin
  Result:=0;
  msg:='';
  if FOutput.Size > 0 then begin
    FOutput.Position:=0;
    SetLength(msg, FOutput.Size);
    Result:=FOutput.Read(PChar(msg)^, Length(msg));
    FOutput.Clear;
  end;
end;

function TLTelnetClient.Send(const aData; const aSize: Integer;
  aSocket: TLSocket): Integer;
var
  Tmp: string;
begin
  {$ifdef debug}
  Writeln('**SEND START** ');
  {$endif}
  Result:=0;
  if aSize > 0 then begin
    SetLength(Tmp, aSize);
    Move(aData, PChar(Tmp)^, aSize);
    DoubleIAC(Tmp);
    if LocalEcho and (not OptionIsSet(TS_ECHO)) and (not OptionIsSet(TS_HYI)) then
      FOutput.Write(PChar(Tmp)^, Length(Tmp));
    Result:=FConnection.SendMessage(Tmp);
  end;
  {$ifdef debug}
  Writeln('**SEND END** ');
  {$endif}
end;

function TLTelnetClient.SendMessage(const msg: string; aSocket: TLSocket
  ): Integer;
begin
  Result:=Send(PChar(msg)^, Length(msg));
end;

procedure TLTelnetClient.SendCommand(const aCommand: Char; const How: TLHowEnum);
begin
  {$ifdef debug}
  Writeln('**SENT** ', TNames[Char(How)], ' ', TNames[aCommand]);
  {$endif}
  FConnection.SendMessage(TS_IAC + Char(How) + aCommand);
end;

procedure TLTelnetClient.CallAction;
begin
  FConnection.CallAction;
end;

initialization
  for zz:=#0 to #255 do
    TNames[zz]:=IntToStr(Ord(zz));
  TNames[#1]:='TS_ECHO';
  TNames[#133]:='TS_HYI';
  TNames[#251]:='TS_WILL';
  TNames[#252]:='TS_WONT';
  TNames[#253]:='TS_DO';
  TNames[#254]:='TS_DONT';

end.

