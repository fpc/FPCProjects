unit uDoer;

{$mode objfpc}{$H+}

{$i baseinc.inc}

interface

uses
  {$ifndef noDB}
  SqlDB, IBConnection,
  {$endif}
  Classes, SysUtils, lIrcBot, StringUtils, Markov;

const
  BoolStr: array[Boolean] of string = ('Off', 'On');

  {$Warning Make sure you modified hiddeninc.inc.default file according to INSTALL}
  {$i hiddeninc.inc} // create this file with your nickserv password there

type
  TDoer = class
   protected
    {$ifndef noDB}
    FLogQuery: TSQLQuery;
    FSeenQuery: TSQLQuery;
    FDefinesQuery: TSQLQuery;
    FDefViewQuery: TSQLQuery;
    FLogTransaction: TSQLTransaction;
    FLogFBConnection: TSQLConnection;
    {$endif}
    FMarkov: TMarkov;
    FSList: TStringList;
    function TrimQuestion(const s: string): string;
    function SepString(s: string): TStringList;
   public
    Quit: Boolean;
    TimeStarted: string;
    Logging: Boolean;
    MarkovOn: Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure OnHelp(Caller: TLIrcBot);
    procedure OnStatus(Caller: TLIrcBot);
    procedure OnAbout(Caller: TLIrcBot);
    procedure OnSeen(Caller: TLIrcBot);
    procedure OnDefine(Caller: TLIrcBot);
    procedure OnWhatIs(Caller: TLIrcBot);
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
    procedure OnMarkov(Caller: TLIrcBot);
    procedure OnSetMarkov(Caller: TLIrcBot);
    procedure OnRecieve(Caller: TLIrcBot);
    procedure OnUnknown(Caller: TLIrcBot);
  end;

implementation

constructor TDoer.Create;

  procedure CreateMarkov(const a, b: string; const n, m: Byte);
  var
    f: TextFile;
  begin
    if not FileExists(a) then begin
      AssignFile(f, a);
      Rewrite(f);
    end;
    if not FileExists(b) then begin
      AssignFile(f, b);
      Rewrite(f);
    end;
    FMarkov:=TMarkov.Create(a, b, n, m);
  end;

begin
  Quit:=False;
  CreateMarkov('words1.txt', 'markov1.txt', 15, 85);
  FSList:=TStringList.Create;
  MarkovOn:=False;
  {$ifndef nodb}
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

  FDefinesQuery := tsqlquery.Create(nil);
  with FDefinesQuery do begin
    DataBase := FLogFBConnection;
    Transaction := FLogTransaction;

{    sql.clear;
    sql.add('insert into tbl_definitions(definitionid,definition,description) values (gen_id(GEN_DEFINITIONID,1),:definition,:description)');
    prepare;}
  end;

  FDefViewQuery := tsqlquery.Create(nil);
  FDefViewQuery.DataBase := FLogFBConnection;
  FDefViewQuery.transaction := FLogTransaction;
  FDefViewQuery.ParseSQL:=False;
  {$endif}
end;

destructor TDoer.Destroy;
begin
  {$ifndef noDB}
  FLogFBConnection.Close;
  FLogFBConnection.Free;
  FLogTransaction.Free;
  FLogQuery.Free;
  FSeenQuery.Free;
  FDefinesQuery.Free;
  FDefViewQuery.Free;
  {$endif}
  FMarkov.Free;
  FSList.Free;
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

    FSList.CommaText:=StringReplace(s, ' ', ',', [rfReplaceAll]);
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
begin
  Caller.Respond(BotName + ' ' + Version);
  Caller.Respond('Logging: ' + BoolStr[Logging]);
  Caller.Respond('Private Response: ' + BoolStr[Caller.ReplyInPrivate]);
  Caller.Respond('Markov generator: ' + BoolStr[MarkovOn]);
  Caller.Respond('Online since: ' + TimeStarted);
end;

procedure TDoer.OnAbout(Caller: TLIrcBot);
begin
  Caller.Respond(BotName + ' ' + Version + ' , copyright (C) 2005 by Ales Katona and Joost van der Sluis');
  Caller.Respond('Contact: almindor@gmail.com     SVN: http://svn.freepascal.org/svn/fpcprojects/fpcircbot');
  Caller.Respond('This bot was programmed in Object Pascal language using the Free Pascal Compiler');
end;

procedure TDoer.OnSeen(Caller: TLIrcBot);

  function GetDateDiff(const ADate: string): string;
  begin
    Result:='';
  end;

var
  Args: string;
begin
  {$ifndef noDB}
  Args:=SQLEscape(TrimQuestion(Caller.LastLine.Arguments));

  with Caller.LastLine, Caller do
    if UserInChannel(Reciever, Args) then
      Respond(Args + ' is already in here!')
    else if Logging then with FSeenQuery do begin
      Sql.Clear;
      if (Length(Reciever) > 0) and (Reciever[1] = '#') then
      
        Sql.Add('select first 1 cast(logtime as varchar(25)) as ' +
                'logtime from tbl_loglines where (reciever=''' + Reciever +
                ''' and upper(sender)=''' + UpperCase(Args) + ''') order by logtime desc')
      else
        Sql.Add('select first 1 cast(logtime as varchar(25)) as ' +
                'logtime from tbl_loglines where upper(sender)=''' + UpperCase(Args) +
                ''' order by logtime desc');
                
      Open;
      if not Eof then
        Respond(Args + ' last seen ' +
                Copy(fieldbyname('logtime').asstring, 1, 19))
      else
        Respond('I''ve never seen ' + Args);
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
      Sql.Add('select first 1 description from tbl_definitions where definition=''' +
              DefWord + '''');

      Open;
      if not Eof then Result:=True;
      Close;
    except
      Respond('DB read error');
    end;
    
    if Result then
      with Caller, FDefinesQuery do try
        Sql.Clear;
        Sql.Add('update tbl_definitions set description=''' + Args +
                ' -- defined by ' + LastLine.Sender +
                ''' where definition=''' + DefWord + '''');
        ExecSQL;
        FLogTransaction.CommitRetaining;
        Respond('As ordered');
      except
        Respond('DB update error');
      end;
  end;

  procedure AddIt;
  begin
    if not UpdateDef then with FDefinesQuery do try
      Sql.Clear;
      Sql.Add('insert into tbl_definitions(definitionid,definition,description) ' +
              'values (gen_id(GEN_DEFINITIONID,1),'''+ DefWord+''',''' +
               Args + ' -- defined by ' + Caller.LastLine.Sender + ''')');
      ExecSQL;
      Caller.Respond('As ordered');
    except
      Caller.Respond('DB insert error');
    end;
   end;
   
begin
  Args:=SQLEscape(StringReplace((Caller.LastLine.Arguments), ':', '`dd', [rfReplaceAll]));
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
    Args:=StringReplace(SQLEscape(TrimQuestion(Arguments)), '`dd', ':', [rfReplaceAll]);
    Sql.Clear;
    Sql.Add('select first 1 description from tbl_definitions where definition=''' +
            LowerCase(Args) + '''');
    
    Open;
    if not Eof then
      Respond(Args + ' ' +
              StringReplace(FieldByName('description').AsString, '`dd', ':', [rfReplaceAll]))
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

procedure TDoer.OnMarkov(Caller: TLIrcBot);
begin
  if LowerCase(Trim(Caller.LastLine.Arguments)) = 'on' then begin
    if Caller.IsPuser(Caller.LastLine.Sender) then begin
      MarkovOn:=True;
      Caller.Respond('As ordered');
    end else Caller.Respond('Only power users can change settings');
  end else if LowerCase(Trim(Caller.LastLine.Arguments)) = 'off' then begin
    if Caller.IsPuser(Caller.LastLine.Sender) then begin
      MarkovOn:=False;
      Caller.Respond('As ordered');
    end else Caller.Respond('Only power users can change settings');
  end else Caller.Respond('Currently: ' + BoolStr[MarkovOn] +
                          ' with deviation: ' + IntToStr(FMarkov.ErrorMargin) +
                          '% and threshold: ' + IntToStr(FMarkov.Threshold) + '%' +
                          ' most used word is: "' + FMarkov.HighestWord + '"' +
                          ' word count is: ' + IntToStr(FMarkov.DictionaryWords) +
                          ' markov entries: ' + IntToStr(FMarkov.EntriesMarkov));
end;

procedure TDoer.OnSetMarkov(Caller: TLIrcBot);
var
  l: TStringList;
  m, n: Longint;
begin
  if MarkovOn then with Caller, Caller.LastLine do begin
    if Length(Trim(Arguments)) > 0 then begin
      l:=SepString(Arguments);
      if Assigned(l) and (l.Count > 1) then begin
        try
          m:=StrToInt(Trim(l[0]));
          n:=StrToInt(Trim(l[1]));
          if (m >= 0) and (m <= 100) and (n >= 0) and (n <= 100) then begin
            FMarkov.ErrorMargin:=m;
            FMarkov.Threshold:=n;
            Respond('As ordered');
          end else Respond('Argument values out of range, range is <0..100>');
        except
          on e: Exception do
            Respond('Conversion Error: ' + e.message);
        end;
      end else Respond('Syntax: SetMarkov <deviation> <threshold> where both are integer <0..100>');
    end else Respond('Syntax: SetMarkov <deviation> <threshold> where both are integer <0..100>');
  end else Caller.Respond('Markov generator is turned off, turn it on with Markov command');
end;

procedure TDoer.OnRecieve(Caller: TLIrcBot);
begin
  Writeln('---------------------BEGIN----------------------');
  with Caller.LastLine do begin
    Write('(', DateTimeToStr(Now), ')$', Sender, '$', Reciever, '$', Msg);
    if WasCommand then Writeln(' [COMMAND]') else Writeln;
  end;
  Writeln('----------------------END-----------------------');
  {$ifndef noDB}
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
  {$endif}
  
  // MARCOV
  if MarkovOn then
    with Caller.LastLine, Caller do if Sender <> Nick then begin
      if (Length(Reciever) > 0) and not WasCommand then
        if LowerCase(Reciever) = LowerCase(Nick) then
          SendMessage(FMarkov.Talk(SepString(Msg)), Sender)
        else if Pos(LowerCase(Nick), LowerCase(Msg)) = 1 then begin
          SendMessage(Sender + ': ' + FMarkov.Talk(SepString((Copy(Msg, Length(Nick) + 1, Length(Msg))))), Reciever)
        end
        else FMarkov.TalkTo(SepString(Msg));
    end;
end;

procedure TDoer.OnUnknown(Caller: TLIrcBot);
begin
  Caller.Respond('Command not found: ' +
                  Trim(Copy(Caller.LastLine.Msg,
                       Length(Caller.Nick) + 2,
                       Length(Caller.LastLine.Msg)))
                  );
end;

end.

