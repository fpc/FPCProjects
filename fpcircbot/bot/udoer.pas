unit uDoer;

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

{$i baseinc.inc}

//{$define noDB}

interface

uses
  {$ifndef noDB}
  DB, SqlDB, PQConnection,
  {$endif}
  Classes, SysUtils,
  lNet, lIrcBot, StringUtils, Markov, InfixMath;
const
  BoolStr: array[Boolean] of string = ('Off', 'On');
  YESSIR = 'As ordered';

  {$Warning Make sure you modified hiddeninc.inc.default file according to INSTALL}
  {$i hiddeninc.inc} // create this file with your nickserv password there

type

  { TDoer }

  TDoer = class
   protected
    {$ifndef noDB}
    FLogQuery: TSQLQuery;
    FSeenQuery: TSQLQuery;
    FDefinesQuery: TSQLQuery;
    FDefViewQuery: TSQLQuery;
    FChanQuery: TSQLQuery;
    FLogTransaction: TSQLTransaction;
    FLogConnection: TSQLConnection;
    FPasteUDP: TLUdp;
    {$endif}
    FMarkov: TMarkov;
    FLL: TLIrcRec; // for speed purposes
    FSList: TStringList;
    FGreetList: TStringList;
    FGreetings: TStringList; // for each channel
    FIrcBot: TLIrcBot;
    function TrimQuestion(const s: string): string;
    function SepString(s: string): TStringList;
    function SpellCheck(const Word, Lang: string): string;
    procedure SetGreetings(const Value: TStringList);
    procedure SetGreetList(const Value: TStringList);
    procedure CleanChannels;
    procedure AddChannels(Bot: TLIrcBot);
    {$ifndef nodb}
    procedure UDPError(const msg: string; aSocket: TLSocket);
    procedure UDPReceive(aSocket: TLSocket);
    {$endif}
   public
    Quit: Boolean;
    TimeStarted: TDateTime;
    Logging: Boolean;
    MarkovOn: Boolean;
    SpellCount: Integer;
    constructor Create(aBot: TLIrcBot);
    destructor Destroy; override;
    procedure OnHelp(Caller: TLIrcBot);
    procedure OnStatus(Caller: TLIrcBot);
    procedure OnAbout(Caller: TLIrcBot);
    procedure OnSeen(Caller: TLIrcBot);
    procedure OnDefine(Caller: TLIrcBot);
    procedure OnWhatIs(Caller: TLIrcBot);
    procedure OnCalc(Caller: TLIrcBot);
    procedure OnGoogle(Caller: TLIrcBot);
    procedure OnLogUrl(Caller: TLIrcBot);
    procedure OnPasteUrl(Caller: TLIrcBot);
    procedure OnSpell(Caller: TLIrcBot);
    procedure OnLSpell(Caller: TLIrcBot);
    procedure OnSpellCount(Caller: TLIrcBot);
    procedure OnLastWord(Caller: TLIrcBot);
    procedure OnFirstWord(Caller: TLIrcBot);
    procedure OnReplyPrv(Caller: TLIrcBot);
    procedure OnPart(Caller: TLIrcBot);
    procedure OnJoin(Caller: TLIrcBot);
    procedure OnQuit(Caller: TLIrcBot);
    procedure OnSayAll(Caller: TLIrcBot);
    procedure OnSayTo(Caller: TLIrcBot);
    procedure OnLog(Caller: TLIrcBot);
    procedure OnGreetings(Caller: TLIrcBot);
    procedure OnAddGreeting(Caller: TLIrcBot);
    procedure OnDeleteGreeting(Caller: TLIrcBot);
    procedure OnAddPuser(Caller: TLIrcBot);
    procedure OnRemovePuser(Caller: TLIrcBot);
    procedure OnListPusers(Caller: TLIrcBot);
    procedure OnMarkov(Caller: TLIrcBot);
    procedure OnSetMarkov(Caller: TLIrcBot);
    procedure OnCleanChans(Caller: TLIrcBot);
    procedure OnRecieve(Caller: TLIrcBot);
    procedure OnDisconnect(Caller: TLIrcBot);
    procedure OnUserJoin(Caller: TLIrcBot);
    procedure OnChannelJoin(Caller: TLIrcBot);
    procedure OnChannelQuit(Caller: TLIrcBot);
    procedure CallAction;
    property GreetList: TStringList read FGreetList write SetGreetList;
    property Greetings: TStringList read FGreetings write SetGreetings;
  end;

implementation

uses
  sCheck;

constructor TDoer.Create(aBot: TLIrcBot);

  procedure CreateMarkov(const Filename: string; const n, m: Byte);
  var
    f: TextFile;
  begin
    if not FileExists(Filename) then begin
      AssignFile(f, Filename);
      Rewrite(f);
    end;
    FMarkov:=TMarkov.Create(Filename, n, m);
  end;

begin
  FIrcBot:=aBot;
  Quit:=False;
  SpellCount:=3;
  Greetings:=TStringList.Create;
  Greetings.Duplicates:=dupIgnore;
  FLL:=TLIrcRec.Create;
  CreateMarkov('markovdata.txt', 15, 65);
  FSList:=TStringList.Create;
  FSList.Delimiter:=' ';
  FGreetList:=TStringList.Create;
  FGreetList.Duplicates:=dupIgnore;
  MarkovOn:=False;

  {$ifndef nodb}
  FPasteUDP:=TLUdp.Create(nil);
  FPasteUDP.Listen(PastePort, LADDR_LO);
  FPasteUDP.OnReceive:=@UDPReceive;
  FPasteUDP.OnError:=@UDPError;

  FLogConnection := TPQConnection.Create(nil);
  with FLogConnection do begin
    DatabaseName := 'fpcbot';
    HostName :='localhost';
    UserName := BotDBName;
    Password := BotDBPass;
  end;

  FLogTransaction := tsqltransaction.create(nil);
  FLogTransaction.database := FLogConnection;

  FLogQuery := tsqlquery.Create(nil);
  FLogQuery.ReadOnly:=True;
  with FLogQuery do begin
    DataBase := FLogConnection;
    transaction := FLogTransaction;

    sql.clear;
  end;

  FSeenQuery := tsqlquery.Create(nil);
  FSeenQuery.DataBase := FLogConnection;
  FSeenQuery.transaction := FLogTransaction;
  FSeenQuery.ParseSQL:=False;

  FDefinesQuery := tsqlquery.Create(nil);
  with FDefinesQuery do begin
    DataBase := FLogConnection;
    Transaction := FLogTransaction;

  end;

  FDefViewQuery := tsqlquery.Create(nil);
  FDefViewQuery.DataBase := FLogConnection;
  FDefViewQuery.transaction := FLogTransaction;
  FDefViewQuery.ParseSQL:=False;
  
  FChanQuery := tsqlquery.Create(nil);
  with FChanQuery do begin
    DataBase := FLogConnection;
    Transaction := FLogTransaction;
  end;
  {$endif}
end;

destructor TDoer.Destroy;
begin
  FMarkov.Free;
  FSList.Free;
  FLL.Free;
  FGreetList.Free;
  Greetings.Free;
  {$ifndef noDB}
  try
    FPasteUDP.Free;
    FLogQuery.Free;
    FSeenQuery.Free;
    FChanQuery.Free;
    FDefinesQuery.Free;
    FDefViewQuery.Free;
    FLogTransaction.Free;
    FLogConnection.Close;
    FLogConnection.Free;
  except on e: Exception do
    Writeln(e.message);
  end;
  {$endif}
end;

function TDoer.TrimQuestion(const s: string): string;

procedure CleanChars(const c: array of Char);
var
  n, i: Longint;
begin
  if High(c) >= 0 then
    for i:=0 to High(c) do begin
      n:=Pos(c[i], Result);
      if n > 0 then begin
        Delete(Result, n, Length(Result));
        Result:=Trim(Result);
      end;
    end;
end;

begin
  Result:=Trim(s);
  CleanChars(['?', '!', '@', '#', '$', '%', '&', '*', '/', ';', ':', '.', ',']);
end;

function TDoer.SepString(s: string): TStringList;
var
  i: Longint;
begin
  Result:=nil;
  FSList.Clear;
  if Length(s) > 0 then begin
    s:=StringReplace(s, ':', ' ', [rfReplaceAll]);
    s:=StringReplace(s, ';', ' ', [rfReplaceAll]);
    s:=StringReplace(s, ')', ' ', [rfReplaceAll]);
    s:=StringReplace(s, '(', ' ', [rfReplaceAll]);
    s:=StringReplace(s, '.', ' ', [rfReplaceAll]);
    s:=StringReplace(s, ',', ' ', [rfReplaceAll]);
    s:=StringReplace(s, '"', ' ', [rfReplaceAll]);
    s:=StringReplace(s, '-', ' ', [rfReplaceAll]);
    s:=StringReplace(s, '_', ' ', [rfReplaceAll]);
    s:=StringReplace(s, '=', ' ', [rfReplaceAll]);
    s:=StringReplace(s, '+', ' ', [rfReplaceAll]);
    s:=StringReplace(s, '!', ' ', [rfReplaceAll]);
    s:=StringReplace(s, '?', ' ', [rfReplaceAll]);
    s:=StringReplace(s, '/', ' ', [rfReplaceAll]);
    s:=StringReplace(s, '\', ' ', [rfReplaceAll]);
    s:=StringReplace(s, '>', ' ', [rfReplaceAll]);
    s:=StringReplace(s, '<', ' ', [rfReplaceAll]);
    s:=StringReplace(s, '[', ' ', [rfReplaceAll]);
    s:=StringReplace(s, ']', ' ', [rfReplaceAll]);
    s:=StringReplace(s, '{', ' ', [rfReplaceAll]);
    s:=StringReplace(s, '}', ' ', [rfReplaceAll]);

    FSList.DelimitedText:=s;
    if FSList.Count > 0 then
      for i:=FSList.Count-1 downto 0 do
        if FSList[i] = '' then
          FSList.Delete(i)
        else
          FSList[i]:=Trim(LowerCase(FSList[i]));
    if FSList.Count > 0 then
      Result:=FSList;
  end;
end;

function TDoer.SpellCheck(const Word, Lang: string): string;
var
  Suggestions: TSuggestionArray;
  Count, i: Integer;
begin
  Result := '';

  Count := sCheck.SpellCheck(Word, Lang, Suggestions);
  
  case Count of
    -1      : Result := 'Unknown language or error on spellcheck';
     0      : Result := 'Your spelling is correct';
     1..100 : begin
                Result := 'Incorrect, try: ';
                for i := Low(Suggestions) to High(Suggestions) do
                  if i < High(Suggestions) then
                    Result := Result + Suggestions[i] + ', '
                  else
                    Result := Result + Suggestions[i];
              end;
  else        Result := 'Too long suggestions list';
  end;
end;

procedure TDoer.SetGreetList(const Value: TStringList);
var
  i: Longint;
begin
  if Assigned(Value) then begin
    FGreetList.Clear;
    if Value.Count > 0 then
      for i:=0 to Value.Count-1 do
        FGreetList.Add(Value[i]);
  end;
end;

procedure TDoer.SetGreetings(const Value: TStringList);
begin
  FreeAndNil(FGreetings);
  FGreetings:=Value;
end;

procedure TDoer.OnHelp(Caller: TLIrcBot);

  function IsCommand(aCommand: string): Longint; // returns # of command or # of pcommand + 1000
  var
    i: Longint;
  begin
    Result:=-1;
    aCommand:=LowerCase(aCommand);
    with Caller do begin
      if CommandCount > 0 then
        for i:=0 to CommandCount-1 do
          if Commands[i].Command = aCommand then begin
            Result:=i;
            Exit;
          end;
          
      if PCommandCount > 0 then
        for i:=0 to PCommandCount-1 do
          if PCommands[i].Command = aCommand then begin
            Result:=i + 1000;
            Exit;
          end;
    end;
  end;
  
var
  i, n: Longint;
  s: string;
begin
  if Length(Trim(Caller.LastLine.Arguments)) = 0 then begin
    s:='';
    Caller.Respond(BotName + ' ' + Version + ' use help <topic> to get more info');
    if Caller.CommandCount > 0 then begin
      s:=s + 'User commands: ';
      for i:=0 to Caller.CommandCount-1 do
        s:=s + Caller.Commands[i].Command + ' ';
      Caller.Respond(s);
    end;
    s:='';
    if Caller.IsPuser(Caller.LastLine.Sender)
    and (Caller.PCommandCount > 0) then begin
      s:=s + 'Power user commands: ';
      for i:=0 to Caller.PCommandCount-1 do
        s:=s + Caller.PCommands[i].Command + ' ';
      Caller.Respond(s);
    end;
  end else begin
    n:=IsCommand(Caller.LastLine.Arguments);
    if n >= 0 then begin
      if n < 1000 then
        Caller.Respond(Caller.LastLine.Arguments + ': ' + Caller.Commands[n].Help)
      else if Caller.IsPuser(Caller.LastLine.Sender) then begin
        Dec(n, 1000);
        Caller.Respond(Caller.LastLine.Arguments + ': ' + Caller.PCommands[n].Help);
      end else Caller.Respond('You are not allowed to perform that command');
    end else Caller.Respond(Caller.LastLine.Arguments + ' is not a command');
  end;
end;

procedure TDoer.OnStatus(Caller: TLIrcBot);
var
  Grt: string;
begin
  Grt:='';
  if Length(Trim(Greetings.Text)) > 0 then
    Grt:='Greetings: ' + Greetings.Text;
  Caller.Respond(BotName + ' ' + Version + ', ' +
                 'Logging: ' + BoolStr[Logging] + ', ' +
                 'Private Response: ' + BoolStr[Caller.ReplyInPrivate] + ', ' +
                 'Markov generator: ' + BoolStr[MarkovOn] + ', ' +
                 'Online for: ' + CreateDiffStr(TimeStarted, Now) + ', ' + Grt);

end;

procedure TDoer.OnAbout(Caller: TLIrcBot);
begin
  Caller.Respond(BotName + ' ' + Version + ' , copyright (C) 2005 by Ales Katona (Almindor).' +
                 'Database: Joost van der Sluis (Loesje) ' +
                 'Markov generator: J. Aldo G. de Freitas Junior (Pepe_Le_Pew) and Vincent Snijders (fpcfan) ' +
                 'Contact: almindor@gmail.com SVN: > http://svn.freepascal.org/svn/fpcprojects/fpcircbot < ' +
                 'This bot was programmed in Object Pascal language using the Free Pascal Compiler.');
end;

procedure TDoer.OnSeen(Caller: TLIrcBot);
var
  Args: string;
  Seen: TDateTime;
  SeenStr: string;
begin
  {$ifndef noDB}
  Args:=SQLEscape(TrimQuestion(Caller.LastLine.Arguments));

  with Caller.LastLine, Caller do
    if UserInChannel(Reciever, Args) then
      Respond(Args + ' is already in here!')
    else if Logging then with FSeenQuery do begin
      Sql.Clear;
      if (Length(Reciever) > 0) and (Reciever[1] = '#') then
      
        Sql.Add('select logtime from tbl_loglines ' +
                'where (reciever=''' + SQLEscape(Reciever) +
                ''' and upper(sender)=''' + SQLEscape(UpperCase(Args)) +
                ''') order by logtime desc limit 1')
      else
        Sql.Add('select logtime from tbl_loglines where ' +
                'upper(sender)=''' + SQLEscape(UpperCase(Args)) +
                ''' order by logtime desc limit 1');
      Writeln;
      Writeln('SEEN: ', Sql.Text);
      Writeln;
      Open;
      if not Eof then begin
        Seen:=FSeenQuery.fieldbyname('logtime').AsDateTime;
        SeenStr:=CreateDiffStr(Seen, Now);
        if Length(SeenStr) > 0 then
          Respond(Args + ' last seen ' + SeenStr + 'ago.')
        else
          Respond(Args + ' was here less than a minute ago.');
      end else Respond('I''ve never seen ' + Args);
      Close;
    end;
  {$else}
  Caller.Respond('I have no DB compiled in, I cannot see the history');
  {$endif}
end;

procedure TDoer.OnDefine(Caller: TLIrcBot);
var
  n: Longint;
  DefWord, Args: string;
{$ifndef noDB}
  function UpdateDef: Boolean;
  begin
    Result:=False;
    with FDefViewQuery, Caller, Caller.LastLine do try
      Sql.Clear;
      Sql.Add('select description from tbl_definitions where definition=''' +
              SQLEscape(DefWord) + ''' limit 1');

      Open;
      if not Eof then Result:=True;
      Close;
    except
      Respond('DB read error');
    end;
    
    if Result then
      with Caller, FDefinesQuery do try
        FLogTransaction.EndTransaction;
        FLogTransaction.StartTransaction;
        Sql.Clear;
        Sql.Add('update tbl_definitions set description=''' + SQLEscape(Args) +
                ''' where definition=''' + SQLEscape(DefWord) + '''');
        ExecSQL;
        FLogTransaction.Commit;
        Respond(YESSIR);
      except
        Respond('DB update error');
        FlogTransaction.EndTransaction;
      end;
  end;

  procedure AddIt;
  begin
    if not UpdateDef then with FDefinesQuery do try
      FLogTransaction.EndTransaction;
      FLogTransaction.StartTransaction;
      Sql.Clear;
      Sql.Add('insert into tbl_definitions(definition,description) ' +
              'values ('''+ SQLEscape(DefWord) +''',''' + SQLEscape(Args) + ''')');
      ExecSQL;
      FLogTransaction.Commit;
      Caller.Respond(YESSIR);
    except
      Caller.Respond('DB insert error');
      FLogTransaction.EndTransaction;
    end;
   end;
   
begin
  Args:=SQLEscape(Caller.LastLine.Arguments);
  with Caller, Caller.LastLine do begin
    if Length(Args) < 256 then begin
      if Length(Args) > 0 then begin
        n:=Pos(' ', Args);
        if n > 0 then begin
          DefWord:=Args;
          Delete(DefWord, n, Length(DefWord));
          DefWord:=Trim(LowerCase(DefWord));
          Args:=Copy(Args, n + 1, Length(Args));
          AddIt;
        end else Respond('Usage: ' + Nick + ': define <what> <definition>');
      end else Respond('Usage: ' + Nick + ': define <what> <definition>');
    end else Respond('Description string too long, max size is 255 chars');
  end;
{$else}
begin
  Caller.Respond('I have no DB compiled in, I cannot add definitions');
{$endif}
end;

procedure TDoer.OnWhatIs(Caller: TLIrcBot);
var
  Args: string;
begin
{$ifndef noDB}
  with FDefViewQuery, Caller, Caller.LastLine do try
    Args:=SQLEscape(TrimQuestion(Arguments));
    Sql.Clear;
    Sql.Add('select description from tbl_definitions where definition=''' +
            SQLEscape(LowerCase(Args)) + ''' limit 1');
    
    Open;
    if not Eof then
      Respond(Args + ' ' + FieldByName('description').AsString)
    else
      Respond('I don''t have ' + Arguments + ' in my database');
    Close;
  except
    Respond('DB read error');
  end;
{$else}
  Caller.Respond('I have no DB compiled in, I cannot search definitions')
{$endif}
end;

procedure TDoer.OnCalc(Caller: TLIrcBot);
begin
  with Caller, Caller.LastLine do begin
    if Length(Arguments) > 0 then try
      Respond('Result = ' + IntToStr(ParseStringForMath(Arguments)));
    except
      Respond('Unable to calculate');
    end else Respond('Usage: calc <math expression>');
  end;
end;

procedure TDoer.OnGoogle(Caller: TLIrcBot);
var
  Args: string;
  i: Integer;
  IsIn: Boolean;
begin
  with Caller, Caller.LastLine do begin
    Writeln(Arguments);
    if Length(Arguments) > 0 then begin
      Args:=Arguments;
      i:=Length(Args);
      IsIn:=False;
      while i > 1 do begin
        if Args[i] = ' ' then begin
          if IsIn then
            Delete(Args, i, 1)
          else begin
            IsIn:=True;
            Args[i]:='+';
          end;
        end else IsIn:=False;
        Dec(i);
      end;
      Respond('http://www.google.com/search?&q=' + Args);
    end else Respond('Usage: google <searchitem>');
  end;
end;

procedure TDoer.OnLogUrl(Caller: TLIrcBot);
begin
  {$ifndef noDB}
  with Caller, Caller.LastLine do
    if Length(Reciever) > 0 then begin
      if Reciever[1] = '#' then
        Respond('see yourself chat ' + CGIURL + 'cgifpcbot?channel=' + Copy(Reciever, 2, Length(Reciever)))
      else
        Respond('see yourself chat ' + CGIURL + 'cgifpcbot?channel=' + Reciever)
    end;
  {$else}
  Caller.Respond('I have no DB compiled in, I cannot log the chat');
  {$endif}
end;

procedure TDoer.OnPasteUrl(Caller: TLIrcBot);
var
  Args: string;
begin
  {$ifndef noDB}
  with Caller, Caller.LastLine do begin
    Args:=StringReplace(Arguments, ' ', '+', [rfReplaceAll]);
    if Length(Reciever) > 0 then begin
      if Reciever[1] = '#' then
        Respond('paste your stuff here ' + CGIURL + 'cgipastebin?channel=' +
                Copy(Reciever, 2, Length(Reciever)) + '&sender=' + Sender +
                '&ancheck=on&title=' + Args)
      else
        Respond('paste your stuff here ' + CGIURL + 'cgipastebin?channel=' +
                Reciever + '&sender=' + Sender + '&ancheck=on&title=' + Args);
    end;
  end;
  {$else}
  Caller.Respond('I have no DB compiled in, I cannot paste the bin');
  {$endif}
end;

procedure TDoer.OnSpell(Caller: TLIrcBot);
var
  s: string;
begin
  if Length(Caller.LastLine.Arguments) > 0 then begin
    s := SpellCheck(Caller.LastLine.Arguments, 'en');
    
    if Length(s) > 0 then
      Caller.Respond(s);
      
  end else Caller.Respond('Syntax: spell <sentence>');
end;

procedure TDoer.OnLSpell(Caller: TLIrcBot);
var
  s: string;
begin
  with Caller, Caller.LastLine do
    if Length(Arguments) > 3 then begin
      if Arguments[3] = ' ' then begin
        s := SpellCheck(Copy(Arguments, 4, Length(Arguments)), Copy(Arguments, 1, 2));
        Respond(s);
      end else Respond('Syntax: lspell <language code> <sentence>');
    end else Respond('Syntax: lspell <language code> <sentence>');
end;

procedure TDoer.OnSpellCount(Caller: TLIrcBot);
begin
  with Caller, Caller.Lastline do begin
    if Length(Trim(Arguments)) = 0 then
      Respond('Currently ' + IntToStr(SpellCount))
    else try
      SpellCount:=StrToInt(Arguments);
      Respond(YESSIR);
    except on e: Exception do
      Respond(e.message);
    end;
  end;
end;

procedure TDoer.OnLastWord(Caller: TLIrcBot);
var
  Args: string;
begin
  {$ifndef noDB}
  with Caller, Caller.LastLine do begin
    if Length(Arguments) > 0 then with FSeenQuery do try
      Args:=Trim(UpperCase(Arguments));
      Sql.Clear;
      Sql.Add('select msg from tbl_loglines where ' +
              'upper(sender)=''' + SQLEscape(Args) + ''' order by logtime desc limit 1');

      Open;
      if not Eof then
        Respond(FieldByName('msg').AsString)
      else
        Respond('I don''t have ' + SQLEscape(Arguments) + ' in my database');
      Close;
    except on e: Exception do
      Respond(e.message);
    end else Respond('Usage: lastword <nick>');
  end;
  {$else}
  Caller.Respond('I have no DB compiled in, cannot comply');
  {$endif}
end;

procedure TDoer.OnFirstWord(Caller: TLIrcBot);
const
  WORD_COUNT = 5;
  RAR: array[0..WORD_COUNT-1] of string = ('mommy', 'daddy', 'bobo', 'gugu', 'waaaah!');
begin
  if Length(Caller.LastLine.Arguments) > 0 then
    Caller.Respond(RAR[Random(WORD_COUNT)])
  else
    Caller.Respond('Are you nuts?');
end;

procedure TDoer.OnReplyPrv(Caller: TLIrcBot);
begin
  if LowerCase(Trim(Caller.LastLine.Arguments)) = 'on' then begin
    Caller.ReplyInPrivate:=True;
    Caller.Respond(YESSIR);
  end else if LowerCase(Trim(Caller.LastLine.Arguments)) = 'off' then begin
    Caller.ReplyInPrivate:=False;
    Caller.Respond(YESSIR);
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
      Respond('Unable to comply, I''m not in ' + args);
      if ChannelCount > 0 then begin
        args:='';
        for i:=0 to ChannelCount - 1 do
          args:=args + Channels[i] + ' ';
        Respond('Try some of these: ' + args);
      end;
    end else Respond(YESSIR);
  end;
end;

procedure TDoer.OnJoin(Caller: TLIrcBot);
begin
  with Caller do
    if not Join(LastLine.Arguments) then
      Respond('Unable to join, cannot join such channel')
    else
      Respond(YESSIR);
end;

procedure TDoer.OnQuit(Caller: TLIrcBot);
begin
  with Caller do begin
    if LowerCase(Trim(LastLine.Arguments)) = 'confirm' then begin
      Respond(YESSIR);
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
    Caller.Respond(YESSIR);
  end else if LowerCase(Trim(Caller.LastLine.Arguments)) = 'off' then begin
    Logging:=False;
    Caller.Respond(YESSIR);
  end else Caller.Respond('Currently: ' + BoolStr[Logging]);
end;

procedure TDoer.OnGreetings(Caller: TLIrcBot);
var
  i: Integer;
  s: string;
begin
  if LowerCase(Trim(Caller.LastLine.Arguments)) = 'on' then begin
    Greetings.Add(Caller.LastLine.Reciever);
    Caller.Respond(YESSIR);
  end else if LowerCase(Trim(Caller.LastLine.Arguments)) = 'off' then begin
    if Greetings.IndexOf(Caller.LastLine.Reciever) >= 0 then
      Greetings.Delete(Greetings.IndexOf(Caller.LastLine.Reciever));
    Caller.Respond(YESSIR);
  end else begin
    if LowerCase(Caller.LastLine.Arguments) = 'list' then begin
      Caller.Respond('Greetings count: ' + IntToStr(FGreetList.Count));
      if FGreetList.Count > 0 then begin
        for i:=0 to FGreetList.Count-1 do
          s:=s + IntToStr(i) + '. ' + FGreetList[i] + '  ';
        Caller.Respond(s);
      end;
    end else begin
      s:='';
      if Greetings.Count > 0 then
        for i:=0 to Greetings.Count-1 do s:=s + Greetings[i] + ' ';
      s:=Trim(s);
      if Length(s) > 0 then
        Caller.Respond('Greeting in: ' + s)
      else
        Caller.Respond('I''m not greeting anyone in any channel.');
    end;
  end;
end;

procedure TDoer.OnAddGreeting(Caller: TLIrcBot);
begin
  with Caller.Lastline do
    if Length(Arguments) > 0 then begin
      FGreetList.Add(Arguments);
      Caller.Respond(YESSIR);
    end else Caller.Respond('Syntax: AddGreeting <greeting>');
end;

procedure TDoer.OnDeleteGreeting(Caller: TLIrcBot);
var
  x: Integer;
begin
  with Caller, Caller.LastLine do
    if Length(Arguments) > 0 then try
      x:=StrToInt(Arguments);
      FGreetList.Delete(x);
      Respond(YESSIR);
    except on e: Exception do
      Respond(e.message);
    end;
end;

procedure TDoer.OnAddPuser(Caller: TLIrcBot);
begin
  Caller.AddPuser(Trim(Caller.LastLine.Arguments));
  Caller.Respond(YESSIR);
end;

procedure TDoer.OnRemovePuser(Caller: TLIrcBot);
begin
  Caller.RemovePuser(Trim(Caller.LastLine.Arguments));
  Caller.Respond(YESSIR);
end;

procedure TDoer.OnListPusers(Caller: TLIrcBot);
var
  i: Longint;
  s: string;
begin
  s:='';
  if Caller.PuserCount > 0 then
    for i:=0 to Caller.PuserCount-1 do
      s:=s + Caller.PUsers[i] + ' ';
  Caller.Respond(s);
end;

procedure TDoer.OnMarkov(Caller: TLIrcBot);
begin
  if LowerCase(Trim(Caller.LastLine.Arguments)) = 'on' then begin
    if Caller.IsPuser(Caller.LastLine.Sender) then begin
      MarkovOn:=True;
      Caller.Respond(YESSIR);
    end else Caller.Respond('Only power users can change settings');
  end else if LowerCase(Trim(Caller.LastLine.Arguments)) = 'off' then begin
    if Caller.IsPuser(Caller.LastLine.Sender) then begin
      MarkovOn:=False;
      Caller.Respond(YESSIR);
    end else Caller.Respond('Only power users can change settings');
  end else Caller.Respond('Currently: ' + BoolStr[MarkovOn] +
                          ' with deviation: ' + IntToStr(FMarkov.ErrorMargin) +
                          '% and threshold: ' + IntToStr(FMarkov.Threshold) + '%' +
                          ' word count is: ' + IntToStr(FMarkov.DictionaryWords) +
                          ' markov entries: ' + IntToStr(FMarkov.EntriesMarkov));
end;

procedure TDoer.OnSetMarkov(Caller: TLIrcBot);
var
  l: TStringList;
  m, n: Byte;
begin
  if MarkovOn then with Caller, Caller.LastLine do begin
    if Length(Trim(Arguments)) > 0 then begin
      l:=SepString(Arguments);
      if Assigned(l) and (l.Count > 1) then begin
        try
          m:=Byte(StrToInt(Trim(l[0])));
          n:=Byte(StrToInt(Trim(l[1])));
          if (m >= 0) and (m <= 100) and (n >= 0) and (n <= 100) then begin
            FMarkov.ErrorMargin:=m;
            FMarkov.Threshold:=n;
            Respond(YESSIR);
          end else Respond('Argument values out of range, range is <0..100>');
        except
          on e: Exception do
            Respond('Conversion Error: ' + e.message);
        end;
      end else Respond('Syntax: SetMarkov <deviation> <threshold> where both are integer <0..100>');
    end else Respond('Syntax: SetMarkov <deviation> <threshold> where both are integer <0..100>');
  end else Caller.Respond('Markov generator is turned off, turn it on with Markov command');
end;

procedure TDoer.OnCleanChans(Caller: TLIrcBot);
begin
  CleanChannels;
  AddChannels(Caller);
  Caller.Respond(YESSIR);
end;

procedure TDoer.OnRecieve(Caller: TLIrcBot);
var
  TheLastLine: TLIrcRec;

  procedure LogMessage;
  begin
  {$ifndef noDB}
    with TheLastline do begin
      if  (Length(Reciever) > 0)
      and (Length(Sender) < 50)
      and (Length(Reciever) < 50)
      and (Length(Msg) < 4096) then begin
        FLogQuery.SQL.Clear;
        FLogQuery.SQL.Add('insert into tbl_loglines(sender,reciever,msg) ' +
                          'values(''' + SQLEscape(Sender) + ''',''' + SQLEscape(Reciever) + ''',''' + SQLEscape(msg) + ''')');
        Writeln('Log MESSAGE: ', FLogQuery.SQL.Text);
        try
          FLogTransaction.EndTransaction;
          FLogTransaction.StartTransaction;
          FLogQuery.ExecSQL;
          FLogTransaction.Commit;
        except
          on e: Exception do begin
            Writeln('-----------------ERROR----------------');
            Writeln(e.Message);
            Writeln('Error writing to FB. Following information isn''t stored:');
            Writeln('Sender: ' + Sender);
            Writeln('Reciever: ' + Reciever);
            Writeln('MSg: ' + Msg);
            Writeln('----------------[ERROR]---------------');
            FLogTransaction.EndTransaction;
//            Caller.Respond('Error writing to DB: ' + e.Message);
            Halt;
          end;
        end;
      end;
    end;
  {$endif}
  end;

begin
  with Caller.LastLine do
    Writeln('(', DateTimeToStr(Now), ')$', Sender, '$', Reciever, '$', Msg);
  if Logging then begin
    TheLastLine:=Caller.LastLine;
    LogMessage;
  end;

  // MARCOV
  if MarkovOn then
    with Caller.LastLine, Caller do begin
      TheLastLine:=FLL;
      FLL.FSender:=Caller.Nick;
      if (Sender <> Nick) and (Length(Reciever) > 0) and WasChat then
        if LowerCase(Reciever) = LowerCase(Nick) then begin
          FLL.FReciever:=Sender;
          FLL.FMsg:=FMarkov.Talk(SepString(Msg));
          SendMessage(FLL.FMsg, Sender);
        end else if Pos(LowerCase(Nick), LowerCase(Msg)) = 1 then begin
          FLL.FReciever:=Reciever;
          FLL.FMsg:=FMarkov.Talk(SepString((Copy(Msg, Length(Nick) + 1, Length(Msg)))));
          SendMessage(Sender + ': ' + FLL.Msg, Reciever);
        end else FMarkov.TalkTo(SepString(Msg));
    end;
end;

procedure TDoer.OnDisconnect(Caller: TLIrcBot);
begin
  Caller.RegisterSelf;
end;

procedure TDoer.OnUserJoin(Caller: TLIrcBot);
type
  TChoice = array of Integer;

  function FillChoice(var Choice: TChoice; const Chan: string): Boolean;
  var
    i: Integer;
  begin
    Result:=False;
    for i:=0 to FGreetList.Count-1 do
      if Pos(Chan, FGreetList[i]) = 1 then begin
        SetLength(Choice, Length(Choice) + 1);
        Choice[High(Choice)]:=i;
        Result:=True;
      end;

    if not Result then // in case the channel has no specific messages
      for i:=0 to FGreetList.Count-1 do
        if FGreetList[i][1] <> '#' then begin
          SetLength(Choice, Length(Choice) + 1);
          Choice[High(Choice)]:=i;
        end;
  end;

var
  s: string;
  Choice: TChoice;
  Specific: Boolean;
begin
  with Caller, Caller.LastLine do begin
    if Greetings.IndexOf(Reciever) >= 0 then
      if FGreetList.Count > 0 then begin
        SetLength(Choice, 0);
        Specific:=FillChoice(Choice, Reciever);
        Writeln('Choice: ', Length(Choice));
        s:=FGreetList[Choice[Random(Length(Choice))]];
        if Specific then Delete(s, 1, Length(Reciever) + 1); // delete the 1st word (channel specification + space)
        s:=StringReplace(s, '$nick', Sender, [rfReplaceAll]);
        s:=StringReplace(s, '$channel', Reciever, [rfReplaceAll]);
        if ReplyInPrivate then
          SendMessage(s, Sender)
        else
          SendMessage(s, Reciever);
      end;
  end;
end;

procedure TDoer.CleanChannels;
begin
  {$ifndef noDB}
  with FChanQuery do try
    FLogTransaction.EndTransaction;
    FLogTransaction.StartTransaction;
    SQL.Clear;
    SQL.Add('delete from tbl_channels');
    ExecSQL;
    FLogTransaction.Commit;
  except
    on e: Exception do begin
      Writeln('Error deleting channels list in DB: ', e.message);
      FLogTransaction.EndTransaction;
    end;
  end;
  {$endif}
end;

procedure TDoer.AddChannels(Bot: TLIrcBot);
var
  i: Integer;
begin
  {$ifndef noDB}
  with FChanQuery.params, FChanQuery do
    if Bot.ChannelCount > 0 then
      for i:=0 to Bot.ChannelCount-1 do try
        FLogTransaction.EndTransaction;
        FLogTransaction.StartTransaction;
        Sql.Clear;
        Sql.Add('insert into tbl_channels(channelname) ' +
                'values ('''+ SQLEscape(Bot.Channels[i]) + ''')');
        Writeln('Log CHANNEL: ', FLogQuery.SQL.Text);
        ExecSQL;
        FLogTransaction.Commit;
      except
        on e: Exception do begin
          Writeln('-----------------ERROR----------------');
          Writeln(e.message);
          Writeln('Error writing to DB. Following information isn''t stored:');
          Writeln('Channel: ' + Bot.Channels[i]);
          Writeln('Highest probability is that the DB has this channel in already');
          Writeln('----------------[ERROR]---------------');
          FLogTransaction.EndTransaction;
        end;
//        Bot.Respond('Error writing to DB!');
      end;
  {$endif}
end;

{$ifndef nodb}

procedure TDoer.UDPError(const msg: string; aSocket: TLSocket);
begin
  Writeln('UDP ERROR: ', msg);
end;

procedure TDoer.UDPReceive(aSocket: TLSocket);
var
  s: string;

  function GetChannel: string;
  var
    n: Integer;
  begin
    Result:='';
    n:=Pos('~', s);
    if n > 1 then
      Result:=Copy(s, 1, n - 1);
  end;
  
  function GetMessage: string;
  var
    n: Integer;
  begin
    Result:='';
    n:=Pos('~', s);
    if (n > 0) and (Length(s) >= n + 1) then
      Result:=Copy(s, n + 1, Length(s));
  end;

begin
  if aSocket.GetMessage(s) > 0 then
    FIrcBot.SendMessage(GetMessage, GetChannel);
end;

{$endif}

procedure TDoer.OnChannelJoin(Caller: TLIrcBot);
begin
  AddChannels(Caller);
end;

procedure TDoer.OnChannelQuit(Caller: TLIrcBot);
begin
  // no, I didn't forget to put anything here
end;

procedure TDoer.CallAction;
begin
  {$ifndef nodb}
  FPasteUDP.CallAction;
  {$endif}
end;

end.

