program cgiFpcBot;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, Web, SqlDB, IBConnection, stringutils;
  
procedure Main;

const
  {$Warning Don't forget to make hiddeninc.inc file!}
  {$i hiddeninc.inc}

var
  LogFBConnection : TIBConnection;
  LogTransaction  : TSQLTransaction;
  LogQuery        : TSQLQuery;
  ChanQuery       : TSQLQuery;
  Channel, Sender : string;
  Count, Msg, Date: string;
  i               : Longint;
  HTMLCode        : TStringList;
  ChanList        : TStringList;
  GetList         : TStringList;
  
procedure InitDB;
begin
  LogFBConnection := tIBConnection.Create(nil);
  with LogFBConnection do begin
    DatabaseName := DBPath;
    UserName := CgiDBName;
    Password := CgiDBPass;
  end;

  LogTransaction := tsqltransaction.create(nil);
  LogTransaction.database := LogFBConnection;

  LogQuery := tsqlquery.Create(nil);
  with LogQuery do begin
    DataBase := LogFBConnection;
    transaction := LogTransaction;
    ReadOnly:=True;
  end;

  ChanQuery := tsqlquery.Create(nil);
  with ChanQuery do begin
    DataBase := LogFBConnection;
    transaction := LogTransaction;
    ReadOnly:=True;
  end;
end;

procedure FreeDB;
begin
  LogFBConnection.Close;
  LogFBConnection.free;
  LogTransaction.free;
  LogQuery.free;
  ChanQuery.Free;
end;

procedure InitCommon;
begin
  Channel:='';
  Sender:='';
  Count:='50';
  Msg:='';
  Date:=''; // today
  HTMLCode:=TStringList.Create;
  ChanList:=TStringList.Create;
  GetList:=TStringList.Create;
  GetList.CommaText:=StringReplace(GetEnvironmentVariable('QUERY_STRING'), '&', ',', [rfReplaceAll]);
end;

procedure Init;
begin
  InitCommon;
  InitDB;
end;

procedure FreeCommon;
begin
  HTMLCode.Free;
  ChanList.Free;
  GetList.Free;
end;

procedure Free;
begin
  FreeCommon;
  FreeDB;
end;

procedure GetWebVars;
var
  Counter: Integer;
  
  function GetValue(const From: string): string;
  var
    m, n: Integer;
    s: string;
  begin
    Result:='';
    s:='';
    m:=Pos('=', From);
    if (m > 0) and (Length(From) > m) then Result:=Copy(From, m+1, Length(From));
    n:=Pos('%', Result);
  end;

  procedure TryGetWebVar(var LocalVar: string; const VarName, DefValue: string);
  begin
    if (GetList.Count > Counter) and (Length(GetValue(GetList[Counter])) > 0) then begin
      LocalVar:=GetValue(GetList[Counter]);
      if Counter = 0 then LocalVar:='#' + LocalVar;
      Web_SetVar(VarName, LocalVar);
    end else if Web_VarExists(VarName) then try
      LocalVar:=Web_GetVar(VarName);
      if Length(LocalVar) = 0 then LocalVar:=DefValue;
      if Counter = 0 then LocalVar:='#' + LocalVar;
    except
      LocalVar:=DefValue;
    end else begin
      Web_SetVar(VarName, DefValue);
      LocalVar:=DefValue;
    end;
    Inc(Counter);
  end;

var
  DefChan: string;
begin
  DefChan:='';
  if ChanList.Count > 0 then DefChan:='#' + ChanList[0];
  Counter:=0;
  
  TryGetWebVar(Channel, 'channel', DefChan);
	
	Channel := return_Channel_sanitize(Channel); //Make sure to have only allowed chars ... :)

  TryGetWebVar(Date, 'date', FormatDateTime('yyyy-mm-dd', SysUtils.Date));
	Date := return_datetimestamp_sanitize (Date); //Making sure that only allowed chars can be for the date

  TryGetWebVar(Count, 'linecount', '50');
 	Count := return_Number_sanitize (Count); //Making sure that we have only number chars ...
	
  TryGetWebVar(Msg, 'msg', '');

  TryGetWebVar(Sender, 'sender', '');
	Sender := return_Nick_sanitize (Sender); //Making sure that only RFC allowed chars exists
end;

procedure FillChannels;
var
  s: string;
begin
  with ChanQuery do try
    SQL.Clear;
    SQL.Add('select channelname from tbl_channels');
    open;
    while not eof do begin
      ChanList.Add(FieldByName('channelname').AsString);
      Next;
    end;
    close;
  except on e: Exception do
    Writeln('SQL Error: ', FilterHTML(e.message));
  end;
end;

function GetHTMLChannels: string;
var
  s: string;
  i: Integer;
begin
  Result:='';
  if ChanList.Count > 0 then
    for i:=0 to ChanList.Count-1 do begin
      s:=ChanList[i];
      if Length(s) > 0 then System.Delete(s, 1, 1); // Delete #
      Result:=Result + '<option value="' + s + '"';
      if '#' + s <> Channel then
        Result:=Result + '>'
      else
        Result:=Result + ' selected>';
      Result:=Result + '#' + s + '</option>';
    end;
end;


var
  LN, s: string;
  Started: Boolean;
begin
  Init;
  LN:='';
  s:='';

  Web_Header;
  FillChannels;
  GetWebVars;

  HTMLCode.LoadFromFile('html' + PathDelim + 'footer1.html');
  HTMLCode.Text:=StringReplace(HTMLCode.Text, '$channels', GetHTMLChannels, [rfReplaceAll]);
  Web_OutLn(HTMLCode.Text);
  if Channel = '#' then
    if ChanList.Count > 0 then Channel:=ChanList[0];
  HTMLCode.Clear;

  Web_TemplateOut('html' + PathDelim + 'footer2.html');

  writeln('<hr><table border="0">');
  writeln('<font size="10">');

  if Length(Sender) > 0 then s:=s + ' and UPPER(sender)=' + UpperCase(AnsiQuotedStr(SQLEscape(Sender), #39));
  if Length(Msg) > 0 then s:=s + ' and msg like ''%' + SQLEscape(Msg) + '%''';
  if Length(Date) > 0 then s:=s + ' and CAST (logtime as date) = ' + AnsiQuotedStr(SQLEscape (Date), #39) ;

  with LogQuery do try
    sql.clear;
    sql.add('select first ' + SQLEscape(Count) + ' sender, msg, cast(logtime as varchar(25)) ' +
            'as logtime from tbl_loglines where (reciever=' + AnsiQuotedStr(SQLEscape(Channel), #39) +
             s + ') order by loglineid desc');
    open;
    i:=2;
    Started:=True;
    while not eof do begin
      if (i mod 2) = 0 then
        HTMLCode.Add('<tr style="background-color:#FFFFFF">')
      else
        HTMLCode.Add('<tr style="background-color:#E0E0E0">');

	s:=FilterHtml(fieldbyname('sender').asstring);
      HTMLCode.Add('<td nowrap>' +
                   FilterHtml(Copy(fieldbyname('logtime').asstring, 1, 19)) +
                   '</td><td>' + s +'</td><td>' +
                   FilterHtml(fieldbyname('msg').asstring) + '</td></tr>');
      Next;
      if (LN <> s) and not Started then Inc(i);
      LN:=s;
      Started:=False;
    end;
    close;

  except on E: Exception do
    Writeln('SQL', E.Message);
  end;
    
  if HTMLCode.Count > 0 then
    for i:=HTMLCode.Count - 1 downto 0 do
      Writeln(HTMLCode[i]);

  writeln('</table></font><hr>');
  writeln('</body></html>');

  Free;
end;

begin
  Main;
end.

