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
  Count           : string;
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
begin
    if (Channel = '#') and Web_VarExists('channel') then try
      Channel:=Web_GetVar('channel');
    except
      Channel:='#fpc';
    end;

    if Web_VarExists('linecount') then try
      Count:=Web_GetVar('linecount');
    except
      Count:='50';
    end;

    if Web_VarExists('sender') then try
      Sender:=Web_GetVar('sender');
    except
      Sender:='';
    end;
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

  procedure Exchange(const src, dst: string);
  var
    n: Longint;
  begin
    n:=Pos(src, Result);
    while n > 0 do begin
      Delete(Result, n, 1);
      Insert(dst, Result, n);
    end;
  end;
  
begin
  Result:=s;
  if Length(Result) > 0 then begin
    Exchange('>', '&lt');
    Exchange('>', '&gt');
    Exchange('"', '&quot');
    Exchange(#34, '&#34');
    Exchange(#39, '&#39');
  end;
end;

begin
  Init;

  with LogQuery do begin
    DataBase := LogFBConnection;
    transaction := LogTransaction;
    ReadOnly:=True;

    Web_Header;
    Web_FileOut('html/header.html');
    Web_FileOut('html/footer.html');

    GetWebVars;

    writeln('<hr><table border="0">');
    writeln('<font size="10">');

    sql.clear;
    if Length(Sender) > 0 then
      sql.add('select first ' + Count + ' sender, msg, cast(logtime as varchar(25)) as logtime from tbl_loglines where (reciever=''' + Channel + ''' and sender=''' + Sender + ''') order by loglineid desc')
    else
      sql.add('select first ' + Count + ' sender, msg, cast(logtime as varchar(25)) as logtime from tbl_loglines where reciever=''' + Channel + ''' order by loglineid desc');

    open;
    i:=1;
    while not eof do begin
      if (i mod 2) = 0 then
        HTMLCode.Add('<tr style="background-color:#FFFFFF">')
      else
        HTMLCode.Add('<tr style="background-color:#E0E0E0">');

      HTMLCode.Add('<td nowrap>' + Copy(fieldbyname('logtime').asstring, 1, 19) +
                   '</td><td>' + fieldbyname('sender').asstring+'</td><td>' +
                   fieldbyname('msg').asstring+'</td></tr>');
      Next;
      Inc(i);
    end;

    HTMLCode.Text:=FilterHtml(HTMLCode.Text);
    if HTMLCode.Count > 0 then
      for i:=HTMLCode.Count - 1 downto 0 do
        Writeln(HTMLCode[i]);
        
    close;
    writeln('</table></font><hr>');
  end;

  if i > 25 then
    Web_FileOut('html/footer.html');
  writeln('</body></html>');
  
  Free;
end;

begin
  Main;
end.

