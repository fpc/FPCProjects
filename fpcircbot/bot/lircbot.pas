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
  SysUtils, Classes, Contnrs, lnet;

{$i lircerrors.inc}
// Not used yet
  
type
  TLIrcRec = class
   public
    FSender: string;
    FReciever: string;
    FArguments: string;
    FMsg: string;
    FCommand: string;
    FTime: TDateTime;
    FWasChat: Boolean;
   public
    function CloneSelf: TLIrcRec;
    property Sender: string read FSender;
    property Reciever: string read FReciever;
    property Arguments: string read FArguments;
    property Msg: string read FMsg;
    property Time: TDateTime read FTime;
    property WasChat: Boolean read FWasChat;
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
    FOnRecieve: TLIrcCallback;
    FOnDisconnect: TLIrcCallback;
    FRIP: Boolean;
    FNickOK: string; // a little hack
    FChan: string; // detto
    FNickPass: string;
    FPeople: TStringListList;
    FWords: TStringList;
   protected
    //******TCP callbacks********
    procedure OnEr(const msg: string; const snum: Longint);
    procedure OnRe(const msg: string; const snum: Longint);
    procedure OnReTest(const msg: string; const snum: Longint); // for testing of input
    procedure OnReJoin(const msg: string; const snum: Longint);
    procedure OnDs(const msg: string; const snum: Longint);
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
    function GetPuser(const i: Longint): string;
    function GetPuserCount: Longint;
    function ParseLine(const aMsg: string): Boolean;
    procedure SetNick(const Value: string);
   public
    constructor Create(const Nick, Login: string);
    destructor Destroy; override;
    function Connect(const Port: Word; const Server: string): Boolean;
    function IsPuser(const aNick: string): Boolean;
    function Join(const Channel: string): Boolean;
    function Part(const Channel: string): Boolean;
    function UserInChannel(const Channel, User: string): Boolean;
    procedure Quit;
    procedure SendMessage(const Msg: string; const Reciever: string = '');
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
    property PUsers[i: Longint]: string read GetPuser;
    property PuserCount: Longint read GetPuserCount;
    property Connected: Boolean read GetConnected;
    property OnRecieve: TLIrcCallback read FOnRecieve write FOnRecieve;
    property OnDisconnect: TLIrcCallback read FOnDisconnect write FOnDisconnect;
    property LastLine: TLIrcRec read FLastLine;
    property ReplyInPrivate: Boolean read FRIP write FRIP;
    property NickServPassword: string read FNickPass write FNickPass;
  end;

implementation

function TLIrcRec.CloneSelf: TLIrcRec;
begin
  Result:=TLIrcRec.Create;
  Result.FSender:=FSender;
  Result.FReciever:=FReciever;
  Result.FArguments:=FArguments;
  Result.FMsg:=FMsg;
  Result.FCommand:=FCommand;
  Result.FTime:=FTime;
  Result.FWasChat:=FWasChat;
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
  FNickOK:='';
  FRIP:=False;
  FLastLine:=TLIrcRec.Create;
  FLogin:=Login;
  FPusers:=TStringList.Create;
  FWords:=TStringList.Create;
  FPusers.CaseSensitive:=False;
  FPeople:=TStringListList.Create(True);
  FChannels:=TStringList.Create;
  FChannels.CaseSensitive:=False;
  FCommands:=TLCommandList.Create;
  FPCommands:=TLCommandList.Create;
  FLastLine.FSender:='';
  FLastLine.FReciever:='';
  FLastLine.FMsg:='';
  FRespondTo:='';
  FOnRecieve:=nil;
  FPort:=0;
  FServer:='';
  FCon:=TLTcp.Create;
  FCon.OnError:=@OnEr;
  FCon.OnDisconnect:=@OnDs;
  FCon.OnRecieve:=@OnRe;
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
procedure TLIrcBot.OnEr(const msg: string; const snum: Longint);
begin
  Writeln(StdErr, msg);
end;

procedure TLIrcBot.OnRe(const msg: string; const snum: Longint);

  function ParseCommands: Boolean;
  var
    i, j: Longint;
  begin
    Result:=False;
    
    FWords.Clear;
    FWords.CommaText:=StringReplace(Trim(FLastLine.Msg), ' ', ',', [rfReplaceAll]);
    for i:=FWords.Count-1 downto 0 do
      if FWords[i] = '' then FWords.Delete(i);

    if FCommands.Count > 0 then
      for i:=0 to FCommands.Count-1 do begin
        if FWords.Count > 0 then
          if ((LowerCase(FLastLine.Reciever) = LowerCase(FNick)) or (FWords[0][1] = '!'))
          and ((FCommands[i].Command = LowerCase(FWords[0])) or (FCommands[i].Command = LowerCase(Copy(FWords[0], 2, Length(FWords[0]))))) then begin
            if Assigned(FCommands[i].FAction) then begin
              if FWords[0][1] <> '!' then
                FRespondTo:=FLastLine.Sender
              else
                FRespondTo:=FLastLine.Reciever;
              FLastLine.FArguments:='';
              if FWords.Count > 1 then
                for j:=1 to FWords.Count-1 do
                  FLastLine.FArguments:=FLastLine.FArguments + FWords[j] + ' ';
              FLastLine.FArguments:=Trim(FLastLine.FArguments);
              FCommands[i].FAction(Self);
            end;
            Result:=True;
            Exit;
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
              else FRespondTo:=FLastLine.Reciever;
              FCommands[i].FAction(Self);
            end;
            Result:=True;
            Exit;
          end;
      end;
  end;
  
  function ParsePCommands: Boolean;
  var
    i, j: Longint;
  begin
    Result:=False;

    FWords.Clear;
    FWords.CommaText:=StringReplace(Trim(FLastLine.Msg), ' ', ',', [rfReplaceAll]);
    for i:=FWords.Count-1 downto 0 do
      if FWords[i] = '' then FWords.Delete(i);
    if FPCommands.Count > 0 then
      for i:=0 to FPCommands.Count-1 do begin
        // PRIVATE
        if FWords.Count > 0 then
          if ((LowerCase(FLastLine.Reciever) = LowerCase(FNick)) or (FWords[0][1] = '!'))
          and ((FPCommands[i].Command = LowerCase(FWords[0])) or (FPCommands[i].Command = LowerCase(Copy(FWords[0], 2, Length(FWords[0]))))) then begin
            if Assigned(FPCommands[i].FAction)
            and (IsPuser(FLastLine.Sender)) then begin
              if FWords[0][1] <> '!' then
                FRespondTo:=FLastLine.Sender
              else
                FRespondTo:=FLastLine.Reciever;
              FLastLine.FArguments:='';
              if FWords.Count > 1 then
                for j:=1 to FWords.Count-1 do
                  FLastLine.FArguments:=FLastLine.FArguments + FWords[j] + ' ';
              FLastLine.FArguments:=Trim(FLastLine.FArguments);
              FPCommands[i].FAction(Self);
            end else SendMessage('Sorry but you are not a power user', FLastLine.Sender);
            Result:=True;
            Exit;
          end;
        // CHANNEL
        if FWords.Count > 1 then
          if  (Pos(LowerCase(Nick), LowerCase(FWords[0])) > 0)
          and (LowerCase(FWords[1]) = FPCommands[i].Command) then begin
            if Assigned(FPCommands[i].FAction)
            and (IsPuser(FLastLine.Sender)) then begin
              FLastLine.FArguments:='';
              if FWords.Count > 2 then
                for j:=2 to FWords.Count-1 do
                  FLastLine.FArguments:=FLastLine.FArguments + FWords[j] + ' ';
              FLastLine.FArguments:=Trim(FLastLine.FArguments);
              if FRIP then FRespondTo:=FLastLine.Sender
              else FRespondTo:=FLastLine.Reciever;
              FPCommands[i].FAction(Self);
            end else SendMessage('Sorry but you are not a power user', FLastLine.Reciever);
            Result:=True;
            Exit;
          end;
      end; // for
  end;
  
  procedure ParseForCommands;
  var
    WasCommand: Boolean;
  begin
    if FLastLine.Sender <> FNick then
      if FCon.OnRecieve = @OnRe then begin
        WasCommand:=ParseCommands;
        if not WasCommand then
          WasCommand:=ParsePCommands;
        if WasCommand then FLastLine.FWasChat:=False;
      end;
  end;
  
  procedure ParseForPings(const nMsg: string);
  begin
    FLastLine.FMsg:=CleanEnding(nMsg);
    if Pos('PING', FLastLine.FMsg) = 1 then
      FCon.SendMessage('PONG :' + Copy(FLastLine.FMsg, 7, Length(FLastLine.FMsg)));
  end;

var
  s: string;
  i: Longint;
  nMsg: string;
  Parsed: Boolean;
  SL: TStringList;
begin
  nMsg:=Msg;
  Writeln(nMsg);
  s:=SeparateByEnding(nMsg);
  if Length(s) > 0 then OnRe(s, 0);
  Parsed:=ParseLine(nMsg);

  if Assigned(FOnRecieve) then
    if Pos(',', FLastLine.FReciever) > 0 then begin
      SL:=TStringList.Create;
      nMsg:=FLastLine.Reciever;
      SL.CommaText:=nMsg;
      if SL.Count > 0 then
        for i:=0 to SL.Count-1 do if Length(SL[i]) > 0 then begin
          FLastLine.FReciever:=SL[i];
          if Parsed then ParseForCommands
          else ParseForPings(nMsg);
          FOnRecieve(Self);
        end;
      SL.Free;
    end else begin
      if Parsed then ParseForCommands
      else ParseForPings(nMsg);
      FOnRecieve(Self);
    end;
end;

procedure TLIrcBot.OnReTest(const msg: string; const snum: Longint);
begin
  if Pos(FNickOK + ' << ONLINE >>', Msg) > 0 then FNickOK:=''
  else if Pos('NOTICE', msg) = 0 then OnRe(msg, 0);
end;

procedure TLIrcBot.OnReJoin(const msg: string; const snum: Longint);
const
  RPL_NAMREPLY = '353'; // successful join reply with names
var
  n: Longint;
  s: string;
  nMsg: string;
begin
  nMsg:=Msg;
  s:=SeparateByEnding(nMsg);
  if Length(s) > 0 then OnReJoin(s, 0);
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
    FCon.OnRecieve:=@OnRe;
  end else if Pos('NOTICE', msg) = 0 then OnRe(nmsg, 0);
end;

procedure TLIrcBot.OnDs(const msg: string; const snum: Longint);
begin
  if Length(FServer) > 0 then begin
    Connect(FPort, FServer);
    if Assigned(OnDisconnect) then
      OnDisconnect(Self);
  end;
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
  Result:=False;
  if FCon.Count > 0 then Result:=FCon[0].Connected
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

function TLIrcBot.GetPuser(const i: Longint): string;
begin
  Result:='';
  if (i >= 0 ) and (i < FPUsers.Count) then
    Result:=FPUsers[i];
end;

function TLIrcBot.GetPuserCount: Longint;
begin
  Result:=FPusers.Count;
end;

function TLIrcBot.ParseLine(const aMsg: string): Boolean;
var
  x, n, m: Longint;
  
  procedure ParseCommand;
  begin
    with FLastLine do begin
      n:=Pos(' ', aMsg);
      FCommand:=Copy(aMsg, n+1, Length(aMsg));
      n:=Pos(' ', FCommand);
      Delete(FCommand, n, Length(FCommand));
      n:=Pos(FCommand, aMsg);

      if n > 0 then begin
        x:=Length(FCommand);
        if n > 1 then begin // Get Sender name
          FSender:=Copy(aMsg, 2, n); // ignore the initial ":"
          m:=Pos('!', FSender);
          if m > 0 then
            Delete(FSender, m, Length(FSender)); // ignore after "!"
        end; // if
        m:=Pos(':', Copy(aMsg, 2, Length(aMsg)-1)) + 1;
        FReciever:=Copy(aMsg, n + x + 1, m - n - x - 2);
        FMsg:=Copy(aMsg, m + 1, Length(aMsg) - m);
        FMsg:=CleanEnding(FMsg); // clean #13#10
        FTime:=Now;
      end; // if
    end;
  end;
  
  procedure FindInChannels(const Del: Boolean = False);
  var
    n, m: Longint;
  begin
    if FPeople.Count > 0 then
      for m:=0 to FPeople.Count-1 do begin
        n:=FPeople[m].IndexOf(FLastLine.Sender);
        if n > 0 then begin
          if Length(FLastLine.FReciever) = 0 then
            FLastLine.FReciever:=FChannels[m]
          else
            FLastLine.FReciever:=FLastLine.FReciever + ',' + FChannels[m];
          if Del then begin
            Writeln('Deleting ', FPeople[m][n], ' from ', FChannels[m]);
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
  Writeln('PARSING LINE: ', aMsg);
  Result:=False;
  with FLastLine do begin
    FSender:='';
    FReciever:='';
    FMsg:='';
    FArguments:='';
  end;
  ParseCommand;
  FilterSpecial(FLastLine.FSender);
  with FLastLine do begin
    FWasChat:=False;
    if FCommand = 'PRIVMSG' then begin
      Result:=True;
      FWasChat:=True;
      m:=Pos('ACTION', FLastLine.FMsg);
      if (m = 2) and (FLastLine.FMsg[1] = #1) then begin
        FLastLine.FMsg:=Copy(FLastLine.FMsg, m + x, Length(FLastLine.FMsg));
        FLastLine.FMsg:=FLastLine.FSender + ' ' + FLastLine.FMsg;
        Delete(FLastLine.FMsg, Length(FLastLine.FMsg), 1);
        FLastLine.FSender:='*';
      end; // if/and
    end;
    
    if FCommand = 'JOIN' then begin
      FLastLine.FReciever:=FLastLine.FMsg;
      FLastLine.FMsg:=FLastLine.Sender + ' joins ' + FLastLine.FMsg;
      n:=FChannels.IndexOf(FLastLine.Reciever);
      if n >= 0 then with FPeople[n] do begin
        Writeln('Adding ', FLastLine.Sender, ' to ', FChannels[n]);
        Add(FLastLine.Sender);
      end;
      if FLastLine.FSender <> Nick then
        Result:=True;
    end; // if
    
    if FCommand = 'PART' then begin
      FLastLine.FMsg:=FLastLine.Sender + ' leaves ' +
                      FLastLine.Reciever + '(' + FLastLine.FMsg + ')';
      n:=FChannels.IndexOf(FLastLine.Reciever);
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
      n:=Pos(' ', FLastLine.FReciever);
      if n > 0 then begin
        s:=Copy(FLastLine.FReciever, n+1, Length(FLastLine.FReciever));
        Delete(FLastLine.FReciever, n, Length(FLastLine.FReciever));
      end else s:=FLastLine.FReciever;
      n:=FChannels.IndexOf(FLastLine.FReciever);
      if s = Nick then begin
        if n >= 0 then begin
          Writeln('Deleting channel ', FChannels[n], ' and all users from it');
          FChannels.Delete(n);
          FPeople.Delete(n);
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
    FSender:='';
    FReciever:='';
    FArguments:='';
    FMsg:=aMsg;
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

function TLIrcBot.Connect(const Port: Word; const Server: string): Boolean;
begin
  Result:=False;
  if FCon.Connect(Port, Server) then begin
    Result:=True;
    FPort:=Port;
    FServer:=Server;
  end;
end;

function TLIrcBot.IsPuser(const aNick: string): Boolean;
var
  i: Longint;
  Backup: TLIrcRec;
begin
  Result:=False;
  if Length(aNick) > 0 then begin
    FNickOK:=aNick;
    if FPUsers.IndexOf(aNick) >= 0 then begin
      Backup:=FLastLine.CloneSelf;
      FCon.SendMessage('NICKSERV :INFO ' + aNick + #13#10);
      FCon.OnRecieve:=@OnReTest;
      for i:=0 to 1000 do begin // LOOONG wait time for nickServ
        FCon.CallAction;
        Sleep(10);
        if Length(FNickOK) = 0 then Result:=True;
        if Result then Break;
      end;
      FLastLine.Free;
      FLastLine:=Backup;
      FCon.OnRecieve:=@OnRe;
    end;
  end;
end;

function TLIrcBot.Join(const Channel: string): Boolean;
var
  i: Longint;
  Backup: TLIrcRec;
begin
  Result:=True;
  if Connected and (FChannels.IndexOf(Channel) < 0) then begin
    Backup:=FLastLine.CloneSelf;
    FCon.SendMessage('JOIN ' + Channel + #13#10);
    FChannels.Add(Channel);
    FPeople.Add(TStringList.Create);
    FPeople.Last.CaseSensitive:=False;
    FPeople.Last.Duplicates:=dupIgnore;
    FCon.OnRecieve:=@OnReJoin;
    i:=0;
    FChan:=Channel;
    while (FCon.OnRecieve = @OnReJoin) and (i < 100) do begin
      FCon.CallAction;
      Sleep(10);
      Inc(i);
    end;
    FLastLine.Free;
    FLastLine:=Backup;
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
  FCon.SendMessage('QUIT' + #13#10)
end;

procedure TLIrcBot.SendMessage(const Msg: string; const Reciever: string = '');
var
  i: Longint;
begin
  if Connected then begin
    if Length(Reciever) > 0 then begin
      FCon.SendMessage('PRIVMSG ' + Reciever + ' :' + Msg + #13#10)
    end else if FChannels.Count > 0 then
      for i:=0 to FChannels.Count-1 do
        FCon.SendMessage('PRIVMSG ' + FChannels[i] + ' :' + Msg + #13#10);
  end;
end;

procedure TLIrcBot.Respond(const aMsg: string);
begin
  SendMessage(FLastLine.Sender + ': ' + aMsg, FRespondTo);
end;

procedure TLIrcBot.RegisterSelf;
var
  i: Longint;
begin
  FCon.SendMessage('NICK ' + FNick + #13#10);
  FCon.SendMessage('USER ' + FLogin + ' 8 * :FPC rag-tag bot' + #13#10);
  FCon.SendMessage('NICKSERV :IDENTIFY ' + FNickPass + #13#10);
  i:=0;
  while i < 100 do begin
    FCon.CallAction;
    Sleep(10);
    Inc(i);
  end;
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

