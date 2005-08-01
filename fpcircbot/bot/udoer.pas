unit udoer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lIrcBot, SqlDB, IBConnection;

const
  BoolStr: array[Boolean] of string = ('Off', 'On');

  {$Warning Make sure you modified hiddeninc.inc.default file according to INSTALL}
  {$i hiddeninc.inc} // create this file with your nickserv password there

type
  TDoer = class
   protected
    FLogQuery: TSQLQuery;
    FSeenQuery: TSQLQuery;
    FLogTransaction: TSQLTransaction;
    FLogFBConnection: TSQLConnection;
    FDefinitions: TStringList;
    FDefWords: TStringList;
    function TrimQuestion(const s: string): string;
   public
    Quit: Boolean;
    TimeStarted: string;
    Logging: Boolean;
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
    procedure OnRecieve(Caller: TLIrcBot);
  end;

implementation

constructor TDoer.Create;
begin
  FDefWords:=TStringList.Create;
  FDefWords.CaseSensitive:=False;
  FDefWords.Duplicates:=dupIgnore;
  FDefinitions:=TStringList.Create;
  FDefinitions.CaseSensitive:=False;
  FDefinitions.Duplicates:=dupIgnore;

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

{   FDefinesQuery := tsqlquery.Create(nil);
   FDefinesQuery.DataBase := FLogFBConnection;
   FDefinesQuery.transaction := FLogTransaction;
   FDefinesQuery.ParseSQL:=False;}
end;

destructor TDoer.Destroy;
begin
  FDefinitions.Free;
  FDefWords.Free;

  FLogFBConnection.Close;
  FLogFBConnection.Free;
  FLogTransaction.Free;
  FLogQuery.Free;
  FSeenQuery.Free;
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
  Caller.Respond('Contact: almindor@gmail.com     SVN: http://svn.freepascal.org/svn/fpcprojects');
  Caller.Respond('This bot was programmed in Object Pascal language using the Free Pascal Compiler');
end;

procedure TDoer.OnSeen(Caller: TLIrcBot);
var
  Args: string;
begin
  Args:=TrimQuestion(Caller.LastLine.Arguments);

  with Caller.LastLine, Caller do
    if UserInChannel(Reciever, Args) then
      Respond(Args + ' is already in here!')
    else if Logging then with FSeenQuery do begin
      Sql.Clear;
      if (Length(Reciever) > 0) and (Reciever[1] = '#') then
        Sql.Add('select first 1 cast(logtime as varchar(25)) as logtime from tbl_loglines where (reciever like ''' + Reciever + ''' and sender like ''' + Args + ''') order by logtime desc')
      else
        Sql.Add('select first 1 cast(logtime as varchar(25)) as logtime from tbl_loglines where sender like ''' + Args + ''' order by logtime desc');
      Open;
      if not Eof then
        Respond(Args + ' last seen ' + Copy(fieldbyname('logtime').asstring, 1, 19))
      else
        Respond('I''ve never seen ' + Args);
      Close;
    end;
end;

procedure TDoer.OnDefine(Caller: TLIrcBot);
var
  n, m: Longint;
  DefWord, Args: string;

procedure AddIt;
begin
  FDefWords.Add(DefWord);
  FDefinitions.Add(Copy(Args, n+1, Length(Args)) +
                   ' -- defined by ' + Caller.LastLine.Sender);
  Caller.Respond('As ordered');
end;

begin
  Args:=TrimQuestion(Caller.LastLine.Arguments);
  with Caller, Caller.LastLine do begin
    if Length(Args) > 0 then begin
      n:=Pos(' ', Args);
      if n > 0 then begin
        DefWord:=Args;
        Delete(DefWord, n, Length(DefWord));
        m:=FDefWords.IndexOf(DefWord);
        if m < 0 then AddIt
        else begin
          FDefWords.Delete(n);
          FDefinitions.Delete(n);
          AddIt;
        end;
      end else Respond('Usage: ' + Nick + ': define <what> <definition>');
    end else Respond('Usage: ' + Nick + ': define <what> <definition>');
  end;
end;

procedure TDoer.OnWhatIs(Caller: TLIrcBot);
var
  n: Longint;
  Args: string;
begin
  Args:=TrimQuestion(Caller.LastLine.Arguments);
  with Caller, Caller.LastLine do begin
    n:=FDefWords.IndexOf(Args);
    if (n >= 0) and (n < FDefinitions.Count) then
      Respond(FDefWords[n] + ' ' + FDefinitions[n])
    else
      Respond('I don''t have ' + Args + ' in my definitions table');
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


end.

