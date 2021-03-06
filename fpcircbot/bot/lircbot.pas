unit lIrcBot;

{ CopyRight (C) 2004 Ales Katona

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

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Contnrs, lnet, levents;

{$i lircerrors.inc}
// Not used yet
  
type
  TLIrcRec = class
   public
    FSender: string;
    FReceiver: string;
    FArguments: string;
    FMsg: string;
    FCommand: string;
    FTime: TDateTime;
    FWasChat: Boolean;
    FIsIdentified: Boolean;
   public
    function CloneSelf: TLIrcRec;
    property Sender: string read FSender;
    property Receiver: string read FReceiver;
    property Arguments: string read FArguments;
    property Msg: string read FMsg;
    property Time: TDateTime read FTime;
    property WasChat: Boolean read FWasChat;
    property IsIdentified: Boolean read FIsIdentified;
  end;
  
  TLIrcBot = class;
  
  TLIrcCallback = procedure(Caller: TLIrcBot) of object;
  
  TLCommand = class
   protected
    FAction: TLIrcCallback;
    FCommand: string;
    FHelp: string;
   public
    constructor Create(const s: string; a: TLIrcCallback; const h: string = '');
    property Command: string read FCommand;
    property Help: string read FHelp;
  end;
  
  {$i tlcommandlisth.inc}
  {$i tstringlistlisth.inc}

  { TLIrcBot }

  TLIrcBot = class
   protected
    FCon: TLTcp;
    FNick: string;
    FLogin: string;
    FServer: string;
    FLastLine: TLIrcRec;
    FPort: Word;
    FChannels: TStringList;
    FPUsers: TStringList;
    FCommands: TLCommandList;
    FPCommands: TLCommandList;
    FRespondTo: string;
    FOnReceive: TLIrcCallback;
    FOnDisconnect: TLIrcCallback;
    FOnConnect: TLIrcCallback;
    FOnUserJoin: TLIrcCallback;
    FOnChannelJoin: TLIrcCallback;
    FOnChannelQuit: TLIrcCallback;
    FLogLine: TLIrcCallback;
    FRIP: Boolean;
    FNickOK: string; // a little hack
    FChan: string; // detto
    FNickPass: string;
    FPeople: TStringListList;
    FWords: TStringList;
   protected
    procedure DoRe(const msg: string);
    procedure DoReJoin(const msg: string);
    //******TCP callbacks********
    procedure OnEr(const msg: string; aSocket: TLSocket);
    procedure OnRe(aSocket: TLSocket);
    procedure OnReJoin(aSocket: TLSocket);
    procedure OnDs(aSocket: TLSocket);
    procedure OnCo(aSocket: TLSocket);
    //******TCP callbacks end****
    function CleanEnding(const astr: string): string;
    function SeparateByEnding(var astr: string): string;
    function GetConnected: Boolean;
    function GetChannel(const i: Longint): string;
    function GetChannelCount: Longint;
    function GetCommand(const i: Longint): TLCommand;
    function GetCommandCount: Longint;
    function GetPCommand(const i: Longint): TLCommand;
    function GetPCommandCount: Longint;
    function GetPuserCount: LongInt;
    function GetPuser(const i: Longint): string;
    function ParseLine(const aMsg: string): Boolean;
    procedure SetNick(const Value: string);
   public
    constructor Create(const Nick, Login: string);
    destructor Destroy; override;
    function Connect(const Server: string; const Port: Word): Boolean;
    function Join(const Channel: string): Boolean;
    function Part(const Channel: string): Boolean;
    function UserInChannel(const Channel, User: string): Boolean;
    function IsPuser(LL: TLIrcRec): Boolean;
    procedure Quit;
    procedure SendMessage(const Msg: string; const Receiver: string = '');
    procedure Respond(const aMsg: string);
    procedure RegisterSelf;
    procedure AddCommand(const Command: string; Action: TLIrcCallback; const Help: string = '');
    procedure AddPCommand(const Command: string; Action: TLIrcCallback; const Help: string = '');
    procedure AddPuser(const Nick: string);
    procedure RemovePuser(const Nick: string);
    procedure CallAction;
   public
    property Nick: string read FNick write SetNick;
    property Login: string read FLogin;
    property Channels[i: Longint]: string read GetChannel;
    property ChannelCount: Longint read GetChannelCount;
    property Commands[i: Longint]: TLCommand read GetCommand;
    property CommandCount: Longint read GetCommandCount;
    property PCommands[i: Longint]: TLCommand read GetPCommand;
    property PCommandCount: Longint read GetPCommandCount;
    property PUserCount: Longint read GetPuserCount;
    property PUsers[i: Longint]: string read GetPUser;
    property Connected: Boolean read GetConnected;
    property LastLine: TLIrcRec read FLastLine;
    property ReplyInPrivate: Boolean read FRIP write FRIP;
    property NickServPassword: string read FNickPass write FNickPass;
    // CALLBACKS
    property OnReceive: TLIrcCallback read FOnReceive write FOnReceive;
    property OnUserJoin: TLIrcCallback read FOnUserJoin write FOnUserJoin;
    property OnChannelJoin: TLIrcCallback read FOnChannelJoin write FOnChannelJoin;
    property OnChannelQuit: TLIrcCallback read FOnChannelQuit write FOnChannelQuit;
    property OnDisconnect: TLIrcCallback read FOnDisconnect write FOnDisconnect;
    property OnConnect: TLIrcCallback read FOnConnect write FOnConnect;
    property LogLine: TLIrcCallback read FLogLine write FLogLine;
    property Connection: TLTcp read FCon;
  end;

implementation

function TLIrcRec.CloneSelf: TLIrcRec;
begin
  Result := TLIrcRec.Create;
  Result.FSender := FSender;
  Result.FReceiver := FReceiver;
  Result.FArguments := FArguments;
  Result.FMsg := FMsg;
  Result.FCommand := FCommand;
  Result.FTime := FTime;
  Result.FWasChat := FWasChat;
  Result.FIsIdentified := FIsIdentified;
end;

constructor TLCommand.Create(const s: string; a: TLIrcCallback; const h: string = '');
begin
  FCommand:=LowerCase(s);
  FAction:=a;
  FHelp:=h;
end;

{$i tstringlistlist.inc}
{$i tlcommandlist.inc}

constructor TLIrcBot.Create(const Nick, Login: string);
begin
  FNick:=Nick;
  FNickOK := '';
  FRIP:=False;
  FLastLine:=TLIrcRec.Create;
  FLogin:=Login;
  FPusers:=TStringList.Create;
  FWords:=TStringList.Create;
  FWords.Delimiter:=' ';
  FPusers.CaseSensitive:=False;
  FPeople:=TStringListList.Create(True);
  FChannels:=TStringList.Create;
  FChannels.CaseSensitive:=False;
  FCommands:=TLCommandList.Create;
  FPCommands:=TLCommandList.Create;
  FLastLine.FSender:='';
  FLastLine.FReceiver:='';
  FLastLine.FMsg:='';
  FRespondTo:='';
  
  FOnReceive:=nil;
  FOnUserJoin:=nil;
  FOnChannelJoin:=nil;
  FOnChannelQuit:=nil;
  FOnConnect:=nil;
  FOnDisconnect:=nil;

  FPort:=0;
  FServer:='';
  FCon:=TLTcp.Create(nil);
  FCon.Timeout := 500; // low enough to be responsible, high enough to not bother CPU
  FCon.OnError:=@OnEr;
  FCon.OnDisconnect:=@OnDs;
  FCon.OnReceive:=@OnRe;
end;

destructor TLIrcBot.Destroy;
begin
  FCon.Free;
  FChannels.Free;
  FPUsers.Free;
  FCommands.Free;
  FPCommands.Free;
  FLastLine.Free;
  FPeople.Free;
  FWords.Free;
end;

//******TCP callbacks********

procedure TLIrcBot.DoRe(const msg: string);

  function ParseCommands: Integer;
  var
    i, j: Longint;
    TheWord: string;
  begin
    Result:=-1;

    FWords.Clear;
    FWords.DelimitedText:=Trim(FLastLine.Msg);
    for i:=FWords.Count-1 downto 0 do
      if FWords[i] = '' then FWords.Delete(i);

    if FCommands.Count > 0 then
      for i:=0 to FCommands.Count-1 do begin
        if FWords.Count > 0 then begin
          if FWords[0][1] = '!' then
            TheWord:=LowerCase(Copy(FWords[0], 2, Length(FWords[0])))
          else
            TheWord:=LowerCase(FWords[0]);
          if ((LowerCase(FLastLine.Receiver) = LowerCase(FNick)) or (FWords[0][1] = '!'))
          and (FCommands[i].Command = TheWord) then begin
            if Assigned(FCommands[i].FAction) then begin
              if FWords[0][1] <> '!' then
                FRespondTo:=FLastLine.Sender
              else
                FRespondTo:=FLastLine.Receiver;
              FLastLine.FArguments:='';
              if FWords.Count > 1 then
                for j:=1 to FWords.Count-1 do
                  FLastLine.FArguments:=FLastLine.FArguments + FWords[j] + ' ';
              FLastLine.FArguments:=Trim(FLastLine.FArguments);
            end;
            Result:=i;
            Exit;
          end;
        end;

        if FWords.Count > 1 then
          if  (Pos(LowerCase(Nick), LowerCase(FWords[0])) > 0)
          and (FCommands[i].Command = LowerCase(FWords[1])) then begin
            if Assigned(FCommands[i].FAction) then begin
              FLastLine.FArguments:='';
              if FWords.Count > 2 then
                for j:=2 to FWords.Count-1 do
                  FLastLine.FArguments:=FLastLine.FArguments + FWords[j] + ' ';
              FLastLine.FArguments:=Trim(FLastLine.FArguments);
              if FRIP then FRespondTo:=FLastLine.Sender
              else FRespondTo:=FLastLine.Receiver;
            end;
            Result:=i;
            Exit;
          end;
      end;
  end;

  function ParsePCommands: Integer;
  var
    i, j: Longint;
    TheWord: string;
  begin
    Result:=-1;

    FWords.Clear;
    FWords.DelimitedText:=Trim(FLastLine.Msg);
    for i:=FWords.Count-1 downto 0 do
      if FWords[i] = '' then FWords.Delete(i);
    if FPCommands.Count > 0 then
      for i:=0 to FPCommands.Count-1 do begin
        // PRIVATE
        if FWords.Count > 0 then begin
          if FWords[0][1] = '!' then
            TheWord:=LowerCase(Copy(FWords[0], 2, Length(FWords[0])))
          else
            TheWord:=LowerCase(FWords[0]);

          if ((LowerCase(FLastLine.Receiver) = LowerCase(FNick)) or (FWords[0][1] = '!'))
          and (FPCommands[i].Command = TheWord) then begin
            if Assigned(FPCommands[i].FAction)
            and IsPuser(FLastLine) then begin
              if FWords[0][1] <> '!' then
                FRespondTo:=FLastLine.Sender
              else
                FRespondTo:=FLastLine.Receiver;
              FLastLine.FArguments:='';
              if FWords.Count > 1 then
                for j:=1 to FWords.Count-1 do
                  FLastLine.FArguments:=FLastLine.FArguments + FWords[j] + ' ';
              FLastLine.FArguments:=Trim(FLastLine.FArguments);
            end else begin
              SendMessage('Sorry but you are not a power user', FLastLine.Sender);
              Exit;
            end;
            Result:=i + 1000;
            Exit;
          end;
        end;

        // CHANNEL
        if FWords.Count > 1 then
          if  (Pos(LowerCase(Nick), LowerCase(FWords[0])) > 0)
          and (LowerCase(FWords[1]) = FPCommands[i].Command) then begin
            if Assigned(FPCommands[i].FAction)
            and IsPuser(FLastLine) then begin
              FLastLine.FArguments:='';
              if FWords.Count > 2 then
                for j:=2 to FWords.Count-1 do
                  FLastLine.FArguments:=FLastLine.FArguments + FWords[j] + ' ';
              FLastLine.FArguments:=Trim(FLastLine.FArguments);
              if FRIP then FRespondTo:=FLastLine.Sender
              else FRespondTo:=FLastLine.Receiver;
            end else begin
              SendMessage('Sorry but you are not a power user', FLastLine.Receiver);
              Exit;
            end;
            Result:=i + 1000;
            Exit;
          end;
      end; // for
  end;

  function ParseForCommands: Integer;
  begin
    Result:=-1;
    if FLastLine.Sender <> FNick then
      if FCon.OnReceive = @OnRe then begin
        Result:=ParseCommands;
        if Result < 0 then
          Result:=ParsePCommands;
        if Result >= 0 then FLastLine.FWasChat:=False;
      end;
  end;

  procedure ParseForPings(const nMsg: string);
  begin
    FLastLine.FMsg:=CleanEnding(nMsg);
    if Pos('PING', FLastLine.FMsg) = 1 then
      FCon.SendMessage('PONG :' + CleanEnding(Copy(FLastLine.FMsg, 7, Length(FLastLine.FMsg))) + #13#10);
  end;

var
  s: string;
  i, x: Integer;
  nMsg: string;
  Parsed: Boolean;
  SL: TStringList;
begin
  nMsg:=Msg;
  s:=SeparateByEnding(nMsg);
  if Length(s) > 0 then
    DoRe(s);

  Parsed := ParseLine(nMsg);
  if Assigned(FOnReceive) then
    if Pos(',', FLastLine.FReceiver) > 0 then begin
      SL:=TStringList.Create;
      nMsg:=FLastLine.Receiver;
      SL.CommaText:=nMsg;
      if SL.Count > 0 then
        for i:=0 to SL.Count-1 do if Length(SL[i]) > 0 then begin
          FLastLine.FReceiver:=SL[i];
          x:=-1;
          if Parsed then x:=ParseForCommands
          else ParseForPings(nMsg);
          FOnReceive(Self);

          if x >= 0 then
            if x >= 1000 then begin
              if Assigned(FPCommands[x - 1000].FAction) then
                FPCommands[x - 1000].FAction(Self);
            end else begin
              if Assigned(FCommands[x].FAction) then
                FCommands[x].FAction(Self);
            end;
        end;
      SL.Free;
    end else begin
      x:=-1;
      if Parsed then x:=ParseForCommands
      else ParseForPings(nMsg);
      FOnReceive(Self);

      if x >= 0 then
        if x >= 1000 then begin
          if Assigned(FPCommands[x - 1000].FAction) then
            FPCommands[x - 1000].FAction(Self);
        end else begin
          if Assigned(FCommands[x].FAction) then
            FCommands[x].FAction(Self);
        end;
    end;
end;

procedure TLIrcBot.OnEr(const msg: string; aSocket: TLSocket);
begin
  Writeln(StdErr, msg);
end;

procedure TLIrcBot.OnRe(aSocket: TLSocket);
var
  s: string;
begin
  if FCon.GetMessage(s) > 0 then
    DoRe(s);
end;

procedure TLIrcBot.DoReJoin(const msg: string);
const
  RPL_NAMREPLY = '353'; // successful join reply with names
var
  n: Longint;
  s: string;
  nMsg: string;
begin
  nMsg:=msg;
  s:=SeparateByEnding(nMsg);
  if Length(s) > 0 then DoReJoin(s);
  n:=Pos(RPL_NAMREPLY, nMsg);
  if n > 0 then begin
    s:=Copy(nmsg, n, Length(nmsg));
    n:=Pos(':', s);
    s:=Copy(s, n + 1, Length(s));
    Trim(s);
    n:=Pos(' ', s);
    while n > 0 do begin
      FPeople[FChannels.IndexOf(FChan)].Add(Copy(s, 1, n-1));
      s:=Copy(s, n + 1, Length(s));
      n:=Pos(' ', s);
    end;
    if FPeople[FChannels.IndexOf(FChan)].Count > 0 then
      for n:=0 to FPeople[FChannels.IndexOf(FChan)].Count - 1 do
        Writeln(FPeople[FChannels.IndexOf(FChan)][n]);
    FCon.OnReceive:=@OnRe;
  end else if Pos('NOTICE', msg) = 0 then DoRe(nmsg);
end;

procedure TLIrcBot.OnReJoin(aSocket: TLSocket);
var
  s: string;
begin
  if FCon.GetMessage(s) > 0 then
    DoReJoin(s);
end;

procedure TLIrcBot.OnDs(aSocket: TLSocket);
begin
  if Length(FServer) > 0 then begin
    Connect(FServer, FPort);
    if Assigned(OnDisconnect) then
      OnDisconnect(Self);
  end;
end;

procedure TLIrcBot.OnCo(aSocket: TLSocket);
begin
  if Assigned(FOnConnect) then
    FOnConnect(Self);
end;

//******TCP callbacks end****

function TLIrcBot.CleanEnding(const astr: string): string;
var
  i, l: Longint;
begin
  Result:='';
  l:=Length(astr);
  if l > 0 then begin
    i:=l;
    while ((astr[i] = #13) or (astr[i] = #10)) and (i >= 1) do Dec(i);
    if i = l then
      Result:=astr
    else
      Result:=Copy(astr, 1, i);
  end;
end;

function TLIrcBot.SeparateByEnding(var astr: string): string;
var
  n: Longint;
begin
  Result:='';
  n:=Pos(#13#10, astr);
  if (n > 0) and (n + 2 < Length(astr)) then begin
    Result:=Copy(astr, n + 2, Length(astr));
    astr:=Copy(astr, 1, n + 1);
  end;
end;

function TLIrcBot.GetConnected: Boolean;
begin
  Result:=FCon.Connected
end;

function TLIrcBot.GetChannel(const i: Longint): string;
begin
  Result:='';
  if (i >= 0 ) and (i < FChannels.Count) then
    Result:=FChannels[i];
end;

function TLIrcBot.GetChannelCount: Longint;
begin
  Result:=FChannels.Count;
end;

function TLIrcBot.GetCommand(const i: Longint): TLCommand;
begin
  Result:=nil;
  if (i >= 0 ) and (i < FCommands.Count) then
    Result:=FCommands[i];
end;

function TLIrcBot.GetCommandCount: Longint;
begin
  Result:=FCommands.Count;
end;

function TLIrcBot.GetPCommand(const i: Longint): TLCommand;
begin
  Result:=nil;
  if (i >= 0 ) and (i < FPCommands.Count) then
    Result:=FPCommands[i];
end;

function TLIrcBot.GetPCommandCount: Longint;
begin
  Result:=FPCommands.Count;
end;

function TLIrcBot.GetPuserCount: LongInt;
begin
  Result := FPUsers.Count;
end;

function TLIrcBot.GetPuser(const i: Longint): string;
begin
  Result := FPUsers[i];
end;

function TLIrcBot.IsPuser(LL: TLIrcRec): Boolean;
begin
  Result := LL.IsIdentified and (FPUsers.IndexOf(LL.Sender) >= 0);
end;

function TLIrcBot.ParseLine(const aMsg: string): Boolean;
var
  x, n, m: Longint;
  
  procedure ParseCommand;
  begin
    with FLastLine do begin
      n := Pos(' ', aMsg);
      FCommand := Copy(aMsg, n+1, Length(aMsg));
      n := Pos(' ', FCommand);
      Delete(FCommand, n, Length(FCommand));
      n := Pos(FCommand, aMsg);

      if n > 0 then begin
        x := Length(FCommand);
        if n > 1 then begin // Get Sender name
          FSender:=Copy(aMsg, 2, n); // ignore the initial ":"
          m := Pos('!', FSender);
          if m > 0 then
            Delete(FSender, m, Length(FSender)); // ignore after "!"
        end; // if
        m := Pos(':', Copy(aMsg, 2, Length(aMsg)-1)) + 1;
        FReceiver := Copy(aMsg, n + x + 1, m - n - x - 2);
        FMsg := Copy(aMsg, m + 1, Length(aMsg) - m);
        FMsg := CleanEnding(FMsg); // clean #13#10

        if (Length(FMsg) > 0) and (FMsg[1] in ['+', '-']) then begin // CAPAB IDENTIFY-MSG is on, use it
          if FMsg[1] = '+' then
            FIsIdentified := True;
          Delete(FMsg, 1, 1);
        end;

        FTime := Now;
      end; // if
    end;
  end;
  
  procedure FindInChannels(const Del: Boolean = False);
  var
    n, m: Integer;
    TheMan: string;
  begin
    if FPeople.Count > 0 then
      for m:=0 to FPeople.Count-1 do begin
        n:=FPeople[m].IndexOf(FLastLine.Sender);
        if n > 0 then begin
          if Length(FLastLine.FReceiver) = 0 then
            FLastLine.FReceiver:=FChannels[m]
          else
            FLastLine.FReceiver:=FLastLine.FReceiver + ',' + FChannels[m];
          if Del then begin
            Writeln('Deleting ', FPeople[m][n], ' from ', FChannels[m]);
            TheMan:=FPeople[m][n];
            FPeople[m].Delete(n);
          end;
        end;
      end;
  end;
  
  procedure FilterSpecial(var s: string);
  begin
    if Length(s) > 0 then
      if s[1] = '@' then Delete(s, 1, 1);
  end;

var
  s: string; // helper string
begin
  Result := False;
  with FLastLine do begin
    FSender := '';
    FReceiver := '';
    FMsg := '';
    FArguments := '';
    FIsIdentified := False;
  end;

  ParseCommand;
  FilterSpecial(FLastLine.FSender);

  with FLastLine do begin
    FWasChat := False;
    if FCommand = 'PRIVMSG' then begin
      Result := True;
      FWasChat := True;
      m := Pos('ACTION', FLastLine.FMsg);
      if (m = 2) and (FLastLine.FMsg[1] = #1) then begin
        FLastLine.FMsg := Copy(FLastLine.FMsg, m + x, Length(FLastLine.FMsg));
        FLastLine.FMsg := FLastLine.FSender + ' ' + FLastLine.FMsg;
        Delete(FLastLine.FMsg, Length(FLastLine.FMsg), 1);
        FLastLine.FSender:='*';
      end; // if/and
    end;
    
    if FCommand = 'JOIN' then begin
      FLastLine.FReceiver := FLastLine.FMsg;
      FLastLine.FMsg := FLastLine.Sender + ' joins ' + FLastLine.FMsg;
      n:=FChannels.IndexOf(FLastLine.Receiver);
      if n >= 0 then with FPeople[n] do begin
        Writeln('Adding ', FLastLine.Sender, ' to ', FChannels[n]);
        Add(FLastLine.Sender);
      end;
      if FLastLine.FSender <> Nick then begin
        Result:=True;
        if Assigned(FOnUserJoin) then
          FOnUserJoin(Self);
      end;
    end; // if
    
    if FCommand = 'PART' then begin
      FLastLine.FMsg := FLastLine.Sender + ' leaves ' +
                      FLastLine.Receiver + '(' + FLastLine.FMsg + ')';
      n:=FChannels.IndexOf(FLastLine.Receiver);
      if n >= 0 then with FPeople[n] do
        if IndexOf(FlastLine.Sender) >= 0 then begin
          Writeln('Deleting ', FLastLine.Sender, ' from ', FChannels[n]);
          Delete(IndexOf(FlastLine.Sender));
        end;
      Result:=True;
    end; // if
    
    if FCommand = 'QUIT' then begin
      FLastLine.FMsg:=FLastLine.Sender + ' quits' + '(' + FLastLine.FMsg + ')';
      FindInChannels(True);
      Result:=True;
    end; // if
    
    if FCommand = 'KICK' then begin
      n:=Pos(' ', FLastLine.FReceiver);
      if n > 0 then begin
        s:=Copy(FLastLine.FReceiver, n+1, Length(FLastLine.FReceiver));
        Delete(FLastLine.FReceiver, n, Length(FLastLine.FReceiver));
      end else s:=FLastLine.FReceiver;
      n:=FChannels.IndexOf(FLastLine.FReceiver);
      if LowerCase(s) = LowerCase(Nick) then begin
        if n >= 0 then begin
          Writeln('Deleting channel ', FChannels[n], ' and all users from it');
          FChannels.Delete(n);
          FPeople.Delete(n);
          if Assigned(FOnChannelQuit) then
            FOnChannelQuit(Self);
        end;
      end else if n >= 0 then with FPeople[n] do
        if IndexOf(s) >= 0 then begin
          Delete(IndexOf(s));
          Writeln('Deleting ', s, ' from ', FChannels[n]);
        end;
      FLastLine.FMsg:=s + ' has been kicked by ' + FLastLine.Sender + ' (' + FLastLine.FMsg + ')';
      Result:=True;
    end;
    
    if FCommand = 'NICK' then begin
      FindInChannels;
      if FPeople.Count > 0 then
        for m:=0 to FPeople.Count-1 do
          if FPeople[m].Count > 0 then
            for n:=0 to FPeople[m].Count-1 do
              if FPeople[m][n] = FLastLine.Sender then FPeople[m][n]:=FLastLine.Msg;

      FLastLine.FMsg:=FLastLine.FSender + ' is now known as ' + FLastLine.FMsg;
      Result:=True;
    end;

    if FCommand = 'NOTICE' then Result:=False;
    if FCommand = 'MODE' then Result:=False;
  end;

  if not Result then with FLastLine do begin
    FSender := '';
    FReceiver := '';
    FArguments := '';
    FMsg := aMsg;
  end;
end;

procedure TLIrcBot.SetNick(const Value: string);
begin
  if Connected then begin
    // TODO: try to change nick
    // TODO: get result from server
    FNick:=Value;
  end else FNick:=Value;
end;

function TLIrcBot.Connect(const Server: string; const Port: Word): Boolean;
begin
  Result:=False;
  if FCon.Connect(Server, Port) then begin
    Result:=True;
    FPort:=Port;
    FServer:=Server;
  end;
end;

function TLIrcBot.Join(const Channel: string): Boolean;
var
  i: Longint;
  Backup: TLIrcRec;
begin
  Result:=True;
  if Connected then begin
    i:=FChannels.IndexOf(Channel);
    if i >= 0 then
      Exit;
//      FChannels.Delete(i);
    Backup:=FLastLine.CloneSelf;
    FCon.SendMessage('JOIN ' + Channel + #13#10);
    FChannels.Add(Channel);
    FPeople.Add(TStringList.Create);
    FPeople.Last.CaseSensitive:=False;
    FPeople.Last.Duplicates:=dupIgnore;
    FCon.OnReceive:=@OnReJoin;
    i:=0;
    FChan:=Channel;
    while (FCon.OnReceive = @OnReJoin) and (i < 2) do begin
      FCon.CallAction;
      Inc(i);
    end;
    FLastLine.Free;
    FLastLine:=Backup;
    if Assigned(FOnChannelJoin) then
      FOnChannelJoin(Self);
  end;
end;

function TLIrcBot.Part(const Channel: string): Boolean;
var
  n: Longint;
begin
  Result:=False;
  n:=FChannels.IndexOf(Channel);
  if n >= 0 then begin
    FCon.SendMessage('PART ' + Channel  + #13#10);
    FPeople.Delete(n);
    FChannels.Delete(n);
    Result:=True;
    if Assigned(FOnChannelQuit) then
      FOnChannelQuit(Self);
  end;
end;

function TLIrcBot.UserInChannel(const Channel, User: string): Boolean;
var
  n: Longint;
begin
  Result:=False;
  n:=FChannels.IndexOf(Channel);
  if n >= 0 then
    if FPeople[n].IndexOf(User) >= 0 then Result:=True;
end;

procedure TLIrcBot.Quit;
begin
  FCon.SendMessage('QUIT' + #13#10);
  if Assigned(FOnChannelQuit) then
    FOnChannelQuit(Self);
end;

procedure TLIrcBot.SendMessage(const Msg: string; const Receiver: string = '');
var
  i: Longint;
  Backup: TLIrcRec;
begin
  if Connected then begin
    if Length(Receiver) > 0 then begin
      Backup:=FLastLine.CloneSelf;
      FLastLine.FSender:=FNick;
      FLastLine.FReceiver:=Receiver;
      FLastLine.FMsg:=Msg;
      FCon.SendMessage('PRIVMSG ' + Receiver + ' :' + Msg + #13#10);
      if Assigned(FLogLine) then LogLine(Self);
      FLastLine.Free;
      FLastLine:=Backup;
    end else if FChannels.Count > 0 then begin
      Backup:=FLastLine.CloneSelf;
      for i:=0 to FChannels.Count-1 do begin
        FLastLine.FSender:=FNick;
        FLastLine.FReceiver:=Receiver;
        FLastLine.FMsg:=Msg;
        FCon.SendMessage('PRIVMSG ' + FChannels[i] + ' :' + Msg + #13#10);
        if Assigned(FLogLine) then LogLine(Self);
      end;
      FLastLine.Free;
      FLastLine:=Backup;
    end;
  end;
end;

procedure TLIrcBot.Respond(const aMsg: string);
begin
  SendMessage(FLastLine.Sender + ': ' + aMsg, FRespondTo);
end;

procedure TLIrcBot.RegisterSelf;
//var
//  i: Longint;
begin
  FCon.SendMessage('NICK ' + FNick + #13#10);
  FCon.SendMessage('USER ' + FLogin + ' 8 * :FPC rag-tag bot' + #13#10);
  FCon.SendMessage('NICKSERV :IDENTIFY ' + FNickPass + #13#10);
  FCon.SendMessage('CAP REQ IDENTIFY-MSG' + #13#10);

{  i:=0;
  while i < 2 do begin
    FCon.CallAction;
    Inc(i);
  end;}
end;

function SortCommands(i1, i2: Pointer): Integer;
var
  c1, c2: TLCommand;
begin
  c1:=TLCommand(i1);
  c2:=TLCommand(i2);
  if Length(c1.Command) > Length(c2.Command) then Result:=1 else
  if Length(c2.Command) > Length(c1.Command) then Result:=-1 else
  Result:=0;
end;

procedure TLIrcBot.AddCommand(const Command: string; Action: TLIrcCallback; const Help: string = '');
begin
  FCommands.Add(TLCommand.Create(Command, Action, Help));
  FCommands.Sort(@SortCommands);
end;

procedure TLIrcBot.AddPCommand(const Command: string; Action: TLIrcCallback; const Help: string = '');
begin
  FPCommands.Add(TLCommand.Create(Command, Action, Help));
  FPCommands.Sort(@SortCommands);
end;

procedure TLIrcBot.AddPuser(const Nick: string);
begin
  FPusers.Add(Nick);
end;

procedure TLIrcBot.RemovePuser(const Nick: string);
var
  n: Longint;
begin
  n:=FPusers.IndexOf(Nick);
  if n >= 0 then
    FPusers.Delete(n);
end;

procedure TLIrcBot.CallAction;
begin
  FCon.CallAction;
end;

end.

