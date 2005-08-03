program cgiFpcBot;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, Web, SqlDB, IBConnection;
  
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
  TryGetWebVar(Count, 'linecount', '50');
  TryGetWebVar(Sender, 'sender', '');
  TryGetWebVar(Msg, 'msg', '');
  TryGetWebVar(Date, 'date', '');
end;

{< = &lt;
> = &gt;
" = &quot;
" = &#34;
' = &#39;
\x8b = &#8249;
\x9b = &#8250;
\012 = &#10;
\015 = &#13;}

function FilterHtml(const s: string): string;
begin
  Result:=s;
  if Length(Result) > 0 then begin
    Result:=StringReplace(Result, '<', '&lt;', [rfReplaceAll]);
    Result:=StringReplace(Result, '>', '&gt;', [rfReplaceAll]);
    Result:=StringReplace(Result, '"', '&quot;', [rfReplaceAll]);
    Result:=StringReplace(Result, #34, '&#34;', [rfReplaceAll]);
    Result:=StringReplace(Result, #39, '&#39;', [rfReplaceAll]);
  end;
end;

var
  s: string;
begin
  Init;
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

    if Length(Sender) > 0 then s:=s + ' and sender=''' + Sender + '''';
    if Length(Msg) > 0 then s:=s + ' and msg like ''%' + Msg + '%''';
    if Length(Date) > 0 then s:=s + ' and CAST (logtime as date) = ''' + Date + '''';
    
    try
      sql.clear;
      sql.add('select first ' + Count + ' sender, msg, cast(logtime as varchar(25)) ' +
              'as logtime from tbl_loglines where (reciever=''' + Channel +
              '''' + s + ') order by loglineid desc');
      open;
      i:=1;
      while not eof do begin
        if (i mod 2) = 0 then
          HTMLCode.Add('<tr style="background-color:#FFFFFF">')
        else
          HTMLCode.Add('<tr style="background-color:#E0E0E0">');

        HTMLCode.Add('<td nowrap>' + Copy(fieldbyname('logtime').asstring, 1, 19) +
                     '</td><td>' + fieldbyname('sender').asstring+'</td><td>' +
                     FilterHtml(fieldbyname('msg').asstring) + '</td></tr>');
        Next;
        Inc(i);
      end;

    except on E: Exception do
      Writeln('SQL', E.Message);
    end;
    
    if HTMLCode.Count > 0 then
      for i:=HTMLCode.Count - 1 downto 0 do
        Writeln(HTMLCode[i]);
        
    close;
    writeln('</table></font><hr>');
  end;
  writeln('</body></html>');
  
  Free;
end;

begin
  Main;
end.

