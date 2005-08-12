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
  Channel, Sender : string;
  Count, Msg, Date: string;
  i               : Longint;
  HTMLCode        : TStringList;
  
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
end;

procedure FreeDB;
begin
  LogFBConnection.Close;
  LogFBConnection.free;
  LogTransaction.free;
  LogQuery.free;
end;

procedure InitCommon;
begin
  Channel:='#' + GetEnvironmentVariable('QUERY_STRING');
  Sender:='';
  Count:='50';
  Msg:='';
  Date:=''; // today
  HTMLCode:=TStringList.Create;
end;

procedure Init;
begin
  InitCommon;
  InitDB;
end;

procedure FreeCommon;
begin
  HTMLCode.Free;
end;

procedure Free;
begin
  FreeCommon;
  FreeDB;
end;

procedure GetWebVars;

  procedure TryGetWebVar(var LocalVar: string; const VarName, DefValue: string);
  begin
    if Web_VarExists(VarName) then try
      LocalVar:=Web_GetVar(VarName);
    except
      LocalVar:=DefValue;
    end else Web_SetVar(VarName, LocalVar);
  end;

begin
  if Channel = '#' then Channel:='#lentilwars';
  
  TryGetWebVar(Channel, 'channel', '#lentilwars');
	
	Channel := return_Channel_sanitize (Channel); //Make sure to have only allowed chars ... :)
	
  TryGetWebVar(Count, 'linecount', '50');
 	Count := return_Number_sanitize (Count); //Making sure that we have only number chars ...
	
  TryGetWebVar(Sender, 'sender', '');
	Sender := return_Nick_sanitize (Sender); //Making sure that only RFC allowed chars exists
	
  TryGetWebVar(Msg, 'msg', '');
	
  TryGetWebVar(Date, 'date', '');
	Date := return_datetimestamp_sanitize (Date); //Making sure that only allowed chars can be for the date
	
end;

var
  LN, s: string;
begin
  Init;
  LN:='';
  s:='';

  with LogQuery do begin
    DataBase := LogFBConnection;
    transaction := LogTransaction;
    ReadOnly:=True;

    Web_Header;
    Web_FileOut('html/header.html');
    GetWebVars;
    Web_TemplateOut('html/footer.html');

    writeln('<hr><table border="0">');
    writeln('<font size="10">');

    if Length(Sender) > 0 then s:=s + ' and sender=' + AnsiQuotedStr(SQLEscape(Sender), #39);
    if Length(Msg) > 0 then s:=s + ' and msg like ''%' + SQLEscape(Msg) + '%''';
    if Length(Date) > 0 then s:=s + ' and CAST (logtime as date) = ' + AnsiQuotedStr(SQLEscape (Date), #39) ;
    
    try
      sql.clear;
      sql.add('select first ' + SQLEscape(Count) + ' sender, msg, cast(logtime as varchar(25)) ' +
              'as logtime from tbl_loglines where (reciever=' + AnsiQuotedStr(SQLEscape(Channel), #39) +
               s + ') order by loglineid desc');
      open;
      i:=2;
      while not eof do begin
        if (i mod 2) = 0 then
          HTMLCode.Add('<tr style="background-color:#FFFFFF">')
        else
          HTMLCode.Add('<tr style="background-color:#E0E0E0">');

				s := FilterHtml(fieldbyname('sender').asstring);
        HTMLCode.Add('<td nowrap>' + FilterHtml(Copy(fieldbyname('logtime').asstring, 1, 19)) +
                     '</td><td>' + s +'</td><td>' + FilterHtml(fieldbyname('msg').asstring) + '</td></tr>');
        Next;
        if LN <> s then Inc(i);
        LN:=s;
      end;

    except on E: Exception do
      Writeln('SQL', E.Message);
    end;
    
    if HTMLCode.Count > 0 then
      for i:=HTMLCode.Count - 1 downto 0 do
        Writeln(FilterHtml(HTMLCode[i]));
        
    close;
    writeln('</table></font><hr>');
  end;
  writeln('</body></html>');
  
  Free;
end;

begin
  Main;
end.

