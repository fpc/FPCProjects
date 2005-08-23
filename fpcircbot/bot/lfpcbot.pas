program lFPCBot;

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
  uDoer, Crt, Classes, SysUtils, lIrcBot;
  
function LoadConfig(const FileName: string): TStringList;
var
  i: Longint;
begin
  Result:=nil;
  if FileExists(FileName) then begin
    Result:=TStringList.Create;
    Result.LoadFromFile(FileName);
    if Result.Count > 0 then
      for i:=Result.Count-1 downto 0 do begin
        if Length(Result[i]) = 0 then
          Result.Delete(i)
        else if Result[i][1] = '!' then
          Result.Delete(i);
      end;
  end;
end;

procedure Main;
var
  Con: TLIrcBot;
  AD: string;
  PORT: Word;
  Doer: TDoer;
  ConfigList: TStringList;
  ChannelsUsers: TStringList;
  n, i: Longint;
begin
  ConfigList:=LoadConfig('botconfig.cfg');
  ChannelsUsers:=TStringList.Create;
  if ConfigList = nil then begin
    Writeln('Unable to work with config file');
    Halt;
  end;
  ChannelsUsers.CommaText:=ConfigList[0];
  ConfigList.Delete(0);
  AD:='irc.freenode.net';
  PORT:=6667;
  Doer:=TDoer.Create;
  Doer.Logging:=True;
  Doer.MarkovOn:=False;
  Doer.Greetings:=True;
  Doer.GreetList:=ConfigList;
  Con:=TLIrcBot.Create(BotName, 'SomeLogin');
  Con.NickServPassword:=NickPass;

  // Normal commands
  Con.AddCommand('help', @Doer.OnHelp, 'Syntax: help [command] Info: makes me display help. If [command] is an empty string I will display list of all possible commands.');
  Con.AddCommand('about', @Doer.OnAbout, 'Syntax: about Info: makes me tell about my authors and where you can get more info about me.');
  Con.AddCommand('status', @Doer.OnStatus, 'Syntax: status Info: makes me tell you the status of most of my variables.');
  Con.AddCommand('seen', @Doer.OnSeen, 'Syntax: seen <nick> Info: makes me tell you when I last saw someone in this channel. <nick> is required.');
  Con.AddCommand('define', @Doer.OnDefine, 'Syntax: define <word> <definition> Info: makes me add a definition to the database. <word> and <definition> are required');
  Con.AddCommand('whatis', @Doer.OnWhatIs, 'Syntax: whatis <word> Info: makes me tell you what something is if it''s defined in the database. <word> is required.');
  Con.AddCommand('logurl', @Doer.OnLogUrl, 'Syntax: logurl Info: makes me tell you where the log is');
  Con.AddCommand('listpusers', @Doer.OnListPUsers, 'Syntax: listpusers Info: makes me list power users');
  Con.AddCommand('markov', @Doer.OnMarkov, 'Syntax: markov [on/off] Info: if parameter is empty, I will display info about the markov generator. If parameter is on/off it will start/shutdown the markov response generator (starting and shuting down the generator works only for power users).');

  // Power user commands
  Con.AddPCommand('replyprv', @Doer.OnReplyPrv, 'Syntax: replyprv [on/off] Info: if parameter is empty, I will tell you if replyprv is on or off, otherwise it makes [not] me reply in private.');
  Con.AddPCommand('part', @Doer.OnPart, 'Syntax: part <#channel> Info: makes me part the channel. <#channel> is required.');
  Con.AddPCommand('join', @Doer.OnJoin, 'Syntax: join <#channel> Info: makes me join a channel. <#channel> is required.');
  Con.AddPCommand('quit', @Doer.OnQuit, 'Syntax: quit [confirm] Info: makes me quit (use "quit confirm" if you are sure)');
  Con.AddPCommand('sayall', @Doer.OnSayAll, 'Syntax: sayall <msg> Info: makes me say something to everyone. <msg> is required.');
  Con.AddPCOmmand('sayto', @Doer.OnSayTo, 'Syntax: sayto <recipient> <msg> Info: makes me say something to someone/channel. <recipient> and <msg> are required. <msg> can be a channel or username.');
  Con.AddPCommand('log', @Doer.OnLog, 'Syntax: log [on/off] Info: if parameter is empty, I will tell you if logging is off or on, otherwise it makes me start/stop logging.');
  Con.AddPCommand('greetings', @Doer.OnGreetings, 'Syntax: greetings [on/off] Info: if parameter is empty, I will tell you if greetings are off or on, otherwise it makes me start/stop greeting people.');
  Con.AddPCommand('addpuser', @Doer.OnAddPuser, 'Syntax: addpuser <nick> Info: makes ma add a power user. <nick> is required.');
  Con.AddPCommand('removepuser', @Doer.OnRemovePuser, 'Syntax: removepuser <nick> Info: makes me remove a power user. <nick> is required.');
  Con.AddPCommand('setmarkov', @Doer.OnSetMarkov, 'Syntax: setmarkov <deviation> <threshold> Info: makes me set the deviation and threshold of the markov generator. <deviation> and <threshold> are required. Both are ints <0..100>');
  Con.AddPCommand('cleanchans', @Doer.OnCleanChans, 'Syntax: cleanchans Info: makes me clean no longer occupied channels from the DB (so the CGI page doesn''t list them. They are still accesible via ?channelname in the URL).');
  // CALLBACKS
  Con.OnRecieve:=@Doer.OnRecieve;
  Con.OnUserJoin:=@Doer.OnUserJoin;
  Con.OnChannelJoin:=@Doer.OnChannelJoin;
  Con.OnChannelQuit:=@Doer.OnChannelQuit;

  if Con.Connect(PORT, AD) then begin
    Con.RegisterSelf;
    Doer.TimeStarted:=DateTimeToStr(Now);
    
    if ChannelsUsers.Count > 0 then begin
      n:=ChannelsUsers.IndexOf('?');
      if n > 0 then
        for i:=0 to n-1 do Con.AddPuser(ChannelsUsers[i]);
      if n < ChannelsUsers.Count-1 then
        for i:=n+1 to ChannelsUsers.Count-1 do Con.Join(ChannelsUsers[i]);
    end;
    
    while not Doer.Quit do begin
      if  KeyPressed
      and (ReadKey = #27) then Doer.Quit:=True;
      Con.CallAction;
      Delay(1);
    end;
  end else Writeln('Unable to connect to: ', AD, ' PORT: ', Port);
  
  Con.Free;
  Doer.Free;
  ChannelsUsers.Free;
  ConfigList.Free;
end;

begin
  Main;
end.

