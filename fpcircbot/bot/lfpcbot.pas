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

procedure Main;
var
  Con: TLIrcBot;
  AD: string;
  i: Longint;
  PORT: Word;
  Doer: TDoer;
  ConfigList: TStringList;
begin
  ConfigList:=TStringList.Create;
  ConfigList.LoadFromFile('botconfig.cfg');
  ConfigList.CommaText:=ConfigList.Text;
  AD:='irc.freenode.net';
  PORT:=6667;
  Doer:=TDoer.Create;
  Doer.Logging:=True;
  Con:=TLIrcBot.Create(BotName, 'SomeLogin');
  Con.NickServPassword:=NickPass;
  Con.AddPuser(ConfigList[0]);

  // Normal commands
  Con.AddCommand('help', @Doer.OnHelp, 'Makes me display this message');
  Con.AddCommand('about', @Doer.OnAbout, 'Makes me tell about my creator');
  Con.AddCommand('status', @Doer.OnStatus, 'Makes me display the current status');
  Con.AddCommand('seen', @Doer.OnSeen, 'Makes me tell you when I last saw someone');
  Con.AddCommand('define', @Doer.OnDefine, 'Makes me add a definition to the database');
  Con.AddCommand('whatis', @Doer.OnWhatIs, 'Makes me tell you what something is if it''s defined in the database');
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
    Con.Join(ConfigList[1]);
    while not Doer.Quit do begin
      if  KeyPressed
      and (ReadKey = #27) then Doer.Quit:=True;
      Con.CallAction;
      Delay(1);
    end;
  end else Writeln('Unable to connect to: ', AD, ' PORT: ', Port);
  
  Con.Free;
  Doer.Free;
  ConfigList.Free;
end;

begin
  Main;
end.

