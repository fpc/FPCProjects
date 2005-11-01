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

function GetAllUsers(Con: TLIrcBot): string;
var
  i: Integer;
begin
  Result:='';
  if Con.PuserCount > 0 then
    for i:=0 to Con.PuserCount-1 do Result:=Result + Con.PUsers[i] + ',';
end;

function GetAllChannels(Con: TLIrcBot): string;
var
  i: Integer;
begin
  Result:='';
  if Con.ChannelCount > 0 then
    for i:=0 to Con.ChannelCount-1 do Result:=Result + Con.Channels[i] + ',';
  if Length(Result) > 0 then
    Delete(Result, Length(Result), 1); // delete trailing ","
end;

procedure SetGreetings(var aList: TStringList; const s: string);
begin
  FreeAndNil(aList);
  aList:=TStringList.Create;
  if Length(s) > 6 then
    aList.CommaText:=Copy(s, 7, Length(s));
end;

function CleanDoubles(const s: string): string;
begin
  Result:=s;
  while Pos(',,', Result) > 0 do
    Result:=StringReplace(Result, ',,', ',', [rfReplaceAll]);
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
  if ConfigList = nil then begin
    Writeln('Unable to work with config file');
    Halt;
  end;
  ChannelsUsers:=TStringList.Create;
  ChannelsUsers.CommaText:=ConfigList[0];
  ConfigList.Delete(0);
  AD:='irc.freenode.net';
  PORT:=6667;
  Doer:=TDoer.Create;
  Doer.Logging:=True;
  Doer.MarkovOn:=True;
  SetGreetings(Doer.Greetings, ConfigList[0]);
  ConfigList.Delete(0);
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
  Con.AddCommand('calc', @Doer.OnCalc, 'Syntax: calc <math expression> Info: makes me calculate the result for your expression. Approved symbols are {+,-,/,*,^,%}');
  Con.AddCommand('google', @Doer.OnGoogle, 'Syntax: google <word> Info: makes me write a link to google search for you lazy asses');
  Con.AddCommand('spell', @Doer.OnSpell, 'Syntax: spell <sentence> Info: makes me tell you if your spelling is correct english and if not, suggest a better one. Encoding is UTF-8.');
//  Con.AddCommand('lspell', @Doer.OnLSpell, 'Syntax: lspell <language code> <sentence> Info: makes me tell you if your spelling is correct in given language and if not, suggest a better one. Encoding is UTF-8. Example: lspell en mother goes shopping');
  Con.AddCommand('firstword', @Doer.OnFirstWord, 'Syntax: firstword <nick> Info: makes me tell you the 1st thing someone sayed');
  Con.AddCommand('lastword', @Doer.OnLastWord, 'Syntax: lastword <nick> Info: makes me tell you the last thing someone sayed');
  Con.AddCommand('logurl', @Doer.OnLogUrl, 'Syntax: logurl Info: makes me tell you where the log is (don''t use in private');
  Con.AddCommand('listpusers', @Doer.OnListPUsers, 'Syntax: listpusers Info: makes me list power users');
  Con.AddCommand('markov', @Doer.OnMarkov, 'Syntax: markov [on/off] Info: if parameter is empty, I will display info about the markov generator. If parameter is on/off it will start/shutdown the markov response generator (starting and shuting down the generator works only for power users).');

  // Power user commands
  Con.AddPCommand('spellcount', @Doer.OnSpellCount, 'Syntax: spellcount [count] Info: makes me set the maximum number of suggested spellings for a word. If [count] is empty I will tell the current count, otherwise I will set to the given number');
  Con.AddPCommand('replyprv', @Doer.OnReplyPrv, 'Syntax: replyprv [on/off] Info: if parameter is empty, I will tell you if replyprv is on or off, otherwise it makes [not] me reply in private.');
  Con.AddPCommand('part', @Doer.OnPart, 'Syntax: part <#channel> Info: makes me part the channel. <#channel> is required.');
  Con.AddPCommand('join', @Doer.OnJoin, 'Syntax: join <#channel> Info: makes me join a channel. <#channel> is required.');
  Con.AddPCommand('quit', @Doer.OnQuit, 'Syntax: quit [confirm] Info: makes me quit (use "quit confirm" if you are sure)');
  Con.AddPCommand('sayall', @Doer.OnSayAll, 'Syntax: sayall <msg> Info: makes me say something to everyone. <msg> is required.');
  Con.AddPCOmmand('sayto', @Doer.OnSayTo, 'Syntax: sayto <recipient> <msg> Info: makes me say something to someone/channel. <recipient> and <msg> are required. <msg> can be a channel or username.');
  Con.AddPCommand('log', @Doer.OnLog, 'Syntax: log [on/off] Info: if parameter is empty, I will tell you if logging is off or on, otherwise it makes me start/stop logging.');
  Con.AddPCommand('greetings', @Doer.OnGreetings, 'Syntax: greetings [on/off/list] Info: if parameter is empty, I will tell you all channels I greet in, if it''s [list] it will list all greetings I know, otherwise it makes me start/stop greeting people in current channel (don''t use in private).');
  Con.AddPCommand('addgreeting', @Doer.OnAddGreeting, 'Syntax: addgreeting <greeting> Info: makes me add a greeting. Examples: 1. "Hello $nick. Welcome to $channel. 2. #channel1 Welcome to #channel1." Example n.1 will add a generic greeting. Example n.2 will add a greeting specific for channel #channel1');
  Con.AddPCommand('deletegreeting', @Doer.OnDeleteGreeting, 'Syntax: deletegreetings <greetnumber> Info: makes me delete a greeing from the list. Works only if the # is >=0 and < greetings count');
  Con.AddPCommand('addpuser', @Doer.OnAddPuser, 'Syntax: addpuser <nick> Info: makes ma add a power user. <nick> is required.');
  Con.AddPCommand('removepuser', @Doer.OnRemovePuser, 'Syntax: removepuser <nick> Info: makes me remove a power user. <nick> is required.');
  Con.AddPCommand('setmarkov', @Doer.OnSetMarkov, 'Syntax: setmarkov <deviation> <threshold> Info: makes me set the deviation and threshold of the markov generator. <deviation> and <threshold> are required. Both are ints <0..100>');
  Con.AddPCommand('cleanchans', @Doer.OnCleanChans, 'Syntax: cleanchans Info: makes me clean no longer occupied channels from the DB (so the CGI page doesn''t list them. They are still accesible via ?channelname in the URL).');
  // CALLBACKS
  Con.OnRecieve:=@Doer.OnRecieve;
  Con.OnDisconnect:=@Doer.OnDisconnect;
  Con.OnUserJoin:=@Doer.OnUserJoin;
  Con.OnChannelJoin:=@Doer.OnChannelJoin;
  Con.OnChannelQuit:=@Doer.OnChannelQuit;
  Con.LogLine:=@Doer.OnRecieve;

  if Con.Connect(PORT, AD) then begin
    Con.RegisterSelf;
    Doer.TimeStarted:=Now;
    
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
  
  // Save channels and power users as they are now for next use
  ConfigList.Clear;
  ConfigList.Add(GetAllUsers(Con));
  ConfigList[0]:=ConfigList[0] + '?,' + GetAllChannels(Con);
  // Save channels in which greetings are on
  ConfigList.Add('$none' + CleanDoubles(',' + Doer.Greetings.CommaText));
  // Save greetings list
  ConfigList.Add(Doer.GreetList.Text);
  ConfigList.SaveToFile('botconfig.cfg');
  
  Con.Free;
  Doer.Free;
  ChannelsUsers.Free;
  ConfigList.Free;
end;

begin
  Randomize;
  Main;
end.

