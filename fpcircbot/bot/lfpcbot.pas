program lfpcbot;

{                            FPCBot

CopyRight (c) 2005 by Ales Katona and Joost van der Sluis

This program is Free software; you can rediStribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is diStributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; withOut even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a Copy of the GNU General Public License
along with This program; if not, Write to the Free Software Foundation,
 Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
}

{$mode objfpc}{$H+}

uses
  Crt, SysUtils, SqlDB, IBConnection, lNet, lIrcBot;
  
const
  BoolStr: array[Boolean] of string = ('Off', 'On');
  
  {$Warning Make sure you created hiddeninc.inc file according to INSTALL}
  {$i hiddeninc.inc} // create this file with your nickserv password there

  DefaultUsers   : array[0..1] of string = ('Almindor', 'fpk');
  DefaultChannels: array[0..1] of string = ('#fpc', '#lazarus-ide');

type
  TDoer = class
   protected
    Logging: Boolean;
    FLogQuery: TSQLQuery;
    FSeenQuery: TSQLQuery;
    FLogTransaction: TSQLTransaction;
    FLogFBConnection: TSQLConnection;
   public
    Quit: Boolean;
    TimeStarted: string;
    constructor Create;
    destructor Destroy; override;
    procedure OnHelp(Caller: TLIrcBot);
    procedure OnStatus(Caller: TLIrcBot);
    procedure OnAbout(Caller: TLIrcBot);
    procedure OnSeen(Caller: TLIrcBot);
    procedure OnLogUrl(Caller: TLIrcBot);
    procedure OnReplyPrv(Caller: TLIrcBot);
    procedure OnPart(Caller: TLIrcBot);
    procedure OnJoin(Caller: TLIrcBot);
    procedure OnQuit(Caller: TLIrcBot);
    procedure OnSayAll(Caller: TLIrcBot);
    procedure OnSayTo(Caller: TLIrcBot);
    procedure OnLog(Caller: TLIrcBot);
    procedure OnAddPuser(Caller: TLIrcBot);
    procedure OnRemovePuser(Caller: TLIrcBot);
    procedure OnListPusers(Caller: TLIrcBot);
    procedure OnRecieve(Caller: TLIrcBot);
  end;
  
constructor TDoer.Create;
begin
  Quit:=False;
  FLogFBConnection := tIBConnection.Create(nil);
  with FLogFBConnection do begin
    DatabaseName := DBPath;
    UserName := BotDBName;
    Password := BotDBPass;
  end;

  FLogTransaction := tsqltransaction.create(nil);
  FLogTransaction.database := FLogFBConnection;

  FLogQuery := tsqlquery.Create(nil);
  FLogQuery.ReadOnly:=True;
  with FLogQuery do begin
    DataBase := FLogFBConnection;
    transaction := FLogTransaction;

    sql.clear;
    sql.add('insert into tbl_LogLines(loglineid,logtime,sender,reciever,msg) values (gen_id(GEN_LOGLINEID,1),current_timestamp,:sender,:reciever,:msg)');
    prepare;
  end;
  
   FSeenQuery := tsqlquery.Create(nil);
   FSeenQuery.DataBase := FLogFBConnection;
   FSeenQuery.transaction := FLogTransaction;
   FSeenQuery.ParseSQL:=False;
end;

destructor TDoer.Destroy;
begin
  FLogFBConnection.Close;
  FLogFBConnection.Free;
  FLogTransaction.Free;
  FLogQuery.Free;
  FSeenQuery.Free;
end;

procedure TDoer.OnHelp(Caller: TLIrcBot);
var
  i: Longint;
begin
  Caller.Respond(BotName + ' ' + Version + ' help system initialized');
  if Caller.CommandCount > 0 then begin
    Caller.Respond('User commands:');
    for i:=0 to Caller.CommandCount-1 do
      Caller.Respond(Caller.Commands[i].Command + ' - ' + Caller.Commands[i].Help);
  end;
  if Caller.IsPuser(Caller.LastLine.Sender)
  and (Caller.PCommandCount > 0) then begin
      Caller.Respond('Power user commands:');
      for i:=0 to Caller.PCommandCount-1 do
        Caller.Respond(Caller.PCommands[i].Command + ' - ' + Caller.PCommands[i].Help);
  end;
end;

procedure TDoer.OnStatus(Caller: TLIrcBot);
begin
  Caller.Respond(BotName + ' ' + Version);
  Caller.Respond('Logging: ' + BoolStr[Logging]);
  Caller.Respond('Private Response: ' + BoolStr[Caller.ReplyInPrivate]);
  Caller.Respond('Online since: ' + TimeStarted);
end;

procedure TDoer.OnAbout(Caller: TLIrcBot);
begin
  Caller.Respond(BotName + ' ' + Version + ' , copyright (C) 2005 by Ales Katona and Joost van der Sluis');
  Caller.Respond('Contact: almindor@gmail.com     Web: http://members.chello.sk/ales');
  Caller.Respond('This bot was programmed in Object Pascal language using the Free Pascal Compiler');
end;

procedure TDoer.OnSeen(Caller: TLIrcBot);
begin
  with Caller.LastLine, Caller do
    if UserInChannel(Reciever, Trim(Arguments)) then
      Respond(Trim(Arguments) + ' is already in here!')
    else if Logging then with FSeenQuery do begin
      Sql.Clear;
      if (Length(Reciever) > 0) and (Reciever[1] = '#') then
        Sql.Add('select first 1 cast(logtime as varchar(25)) as logtime from tbl_loglines where (reciever=''' + Reciever + ''' and sender=''' + Trim(Arguments) + ''') order by logtime desc')
      else
        Sql.Add('select first 1 cast(logtime as varchar(25)) as logtime from tbl_loglines where sender=''' + Trim(Arguments) + ''' order by logtime desc');
      Open;
      if not Eof then
        Respond(Trim(Arguments) + ' last seen ' + Copy(fieldbyname('logtime').asstring, 1, 19))
      else
        Respond('I''ve never seen ' + Trim(Arguments));
      Close;
    end;
end;

procedure TDoer.OnLogUrl(Caller: TLIrcBot);
begin
  with Caller, Caller.LastLine do
    if Length(Reciever) > 0 then begin
      if Reciever[1] = '#' then
        Respond('see yourself chat ' + LogURL + '?' + Copy(Reciever, 2, Length(Reciever)))
      else
        Respond('see yourself chat ' + LogURL + '?' + Reciever)
    end;
end;

procedure TDoer.OnReplyPrv(Caller: TLIrcBot);
begin
  if LowerCase(Trim(Caller.LastLine.Arguments)) = 'on' then begin
    Caller.ReplyInPrivate:=True;
    Caller.Respond('As ordered');
  end else if LowerCase(Trim(Caller.LastLine.Arguments)) = 'off' then begin
    Caller.ReplyInPrivate:=False;
    Caller.Respond('As ordered');
  end else Caller.Respond('Currently: ' + BoolStr[Caller.ReplyInPrivate]);
end;

procedure TDoer.OnPart(Caller: TLIrcBot);
var
  i: Longint;
  args: string;
begin
  with Caller, Caller.LastLine do begin
    if Trim(Arguments) = '' then args:=Reciever
    else args:=Arguments;
    if not Part(args) then begin
      Respond('Unable to comply, I''m not in' + args);
      if ChannelCount > 0 then begin
        Respond('Try some of these: ');
        for i:=0 to ChannelCount - 1 do
          Respond(Channels[i]);
      end;
    end else Respond('As ordered');
  end;
end;

procedure TDoer.OnJoin(Caller: TLIrcBot);
begin
  with Caller do
    if not Join(LastLine.Arguments) then
      Respond('Unable to join, cannot join such channel')
    else
      Respond('As ordered');
end;

procedure TDoer.OnQuit(Caller: TLIrcBot);
begin
  with Caller do begin
    if LowerCase(Trim(LastLine.Arguments)) = 'confirm' then begin
      Respond('As ordered');
      Quit;
      Self.Quit:=True;
    end else Respond('I don''t want to!');
  end;
end;

procedure TDoer.OnSayAll(Caller: TLIrcBot);
begin
  with Caller, Caller.LastLine do
    SendMessage(Arguments);
end;

procedure TDoer.OnSayTo(Caller: TLIrcBot);
var
  s, m: string;
  n: Longint;
begin
  with Caller, Caller.LastLine do begin
    s:=Arguments;
    n:=Pos(' ', s);
    if n > 0 then begin
      m:=Copy(s, n+1, Length(s));
      Delete(s, n, Length(s));
      SendMessage(Copy(m, 1, Length(m)), s);
    end;
  end;
end;

procedure TDoer.OnLog(Caller: TLIrcBot);
begin
  if LowerCase(Trim(Caller.LastLine.Arguments)) = 'on' then begin
    Logging:=True;
    Caller.Respond('As ordered');
  end else if LowerCase(Trim(Caller.LastLine.Arguments)) = 'off' then begin
    Logging:=False;
    Caller.Respond('As ordered');
  end else Caller.Respond('Currently: ' + BoolStr[Logging]);
end;

procedure TDoer.OnAddPuser(Caller: TLIrcBot);
begin
  Caller.AddPuser(Trim(Caller.LastLine.Arguments));
  Caller.Respond('As ordered');
end;

procedure TDoer.OnRemovePuser(Caller: TLIrcBot);
begin
  Caller.RemovePuser(Trim(Caller.LastLine.Arguments));
  Caller.Respond('As ordered');
end;

procedure TDoer.OnListPusers(Caller: TLIrcBot);
var
  i: Longint;
begin
  if Caller.PuserCount > 0 then
    for i:=0 to Caller.PuserCount-1 do
      Caller.Respond(Caller.PUsers[i]);
end;

procedure TDoer.OnRecieve(Caller: TLIrcBot);
begin
  Writeln('---------------------BEGIN----------------------');
  with Caller.LastLine do
    Writeln('(', DateTimeToStr(Now), ')$', Sender, '$', Reciever, '$', Msg);
  Writeln('----------------------END-----------------------');
  if Logging then with Caller.Lastline do begin
    if  (Length(Reciever) > 0)
    and (Length(Sender) < 50)
    and (Length(Reciever) < 50)
    and (Length(Msg) < 4096) then begin
      FLogQuery.params.ParamByName('Sender').AsString:=Sender;
      FLogQuery.params.ParamByName('Reciever').AsString:=Reciever;
      FLogQuery.params.ParamByName('msg').AsString:=Msg;
      try
        FLogQuery.ExecSQL;
        FLogTransaction.CommitRetaining;
      except
        Writeln('Error writing to FB. Following information isn''t stored:');
        Writeln('Sender: ' + Sender);
        Writeln('Reciever: ' + Reciever);
        Writeln('MSg: ' + Msg);
        Caller.Respond('Error writing to DB!');
      end;
    end;
  end;
end;

procedure Main;
var
  Con: TLIrcBot;
  AD: string;
  i: Longint;
  PORT: Word;
  Doer: TDoer;
begin
  AD:='irc.freenode.net';
  PORT:=6667;

  Doer:=TDoer.Create;
  Doer.Logging:=True;
  Con:=TLIrcBot.Create(BotName, 'SomeLogin');
  Con.NickServPassword:=NickPass;
  for i:=Low(DefaultUsers) to High(DefaultUsers) do
    Con.AddPuser(DefaultUsers[i]);

  // Normal commands
  Con.AddCommand('help', @Doer.OnHelp, 'Makes me display this message');
  Con.AddCommand('about', @Doer.OnAbout, 'Makes me tell about my creator');
  Con.AddCommand('status', @Doer.OnStatus, 'Makes me display the current status');
  Con.AddCommand('seen', @Doer.OnSeen, 'Makes me tell you when I last saw someone');
  Con.AddCommand('logurl', @Doer.OnLogUrl, 'Makes me tell you where the log is');
  Con.AddCommand('listpusers', @Doer.OnListPUsers, 'Makes me list power users');

  // Power user commands
  Con.AddPCommand('replyprv', @Doer.OnReplyPrv, 'Makes me reply in private, param is (On/Off)');
  Con.AddPCommand('part', @Doer.OnPart, 'Makes me part the channel');
  Con.AddPCommand('join', @Doer.OnJoin, 'Makes me join a channel');
  Con.AddPCommand('quit', @Doer.OnQuit, 'Makes me quit(use "quit confirm" if you are sure)');
  Con.AddPCommand('sayall', @Doer.OnSayAll, 'Makes me say something to everyone');
  Con.AddPCOmmand('sayto', @Doer.OnSayTo, 'Makes me say something to someone/channel');
  Con.AddPCommand('log', @Doer.OnLog, 'Makes me begin/end logging. Param is (On/Off)');
  Con.AddPCommand('addpuser', @Doer.OnAddPuser, 'Makes ma add a power user');
  Con.AddPCommand('removepuser', @Doer.OnRemovePuser, 'Makes me remove a power user');
  
  Con.OnRecieve:=@Doer.OnRecieve;
  if Con.Connect(PORT, AD) then begin
    Con.RegisterSelf;
    Doer.TimeStarted:=DateTimeToStr(Now);
    for i:=Low(DefaultChannels) to High(DefaultChannels) do
      Con.Join(DefaultChannels[i]);
    while not Doer.Quit do begin
      if  KeyPressed
      and (ReadKey = #27) then Doer.Quit:=True;
      Con.CallAction;
      Delay(1);
    end;
  end;
  
  Con.Free;
  Doer.Free;
end;

begin
  Main;
end.

