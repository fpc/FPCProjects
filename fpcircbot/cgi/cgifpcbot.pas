program cgiFpcBot;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, Web, SqlDB, IBConnection;
  
const
  {$Warning Don't forget to make hiddeninc.inc file!}
  {$i hiddeninc.inc}

var LogFBConnection : TIBConnection;
    LogTransaction  : TSQLTransaction;
    LogQuery        : TSQLQuery;
    Channel, Sender : string;
    i, Count        : Longint;
    HTMLCode        : TStringList;

begin
  Channel:='#' + GetEnvironmentVariable('QUERY_STRING');
  Sender:='';
  Count:=50;
  LogFBConnection := tIBConnection.Create(nil);
  with LogFBConnection do begin
    DatabaseName := DBPath;
    UserName := 'sysdba';
    Password := MASTERKEY;
  end;

  LogTransaction := tsqltransaction.create(nil);
  LogTransaction.database := LogFBConnection;
  HTMLCode:=TStringList.Create;

  LogQuery := tsqlquery.Create(nil);
  with LogQuery do begin
    DataBase := LogFBConnection;
    transaction := LogTransaction;
    ReadOnly:=True;

    Web_Header;
    Web_FileOut('html/header.html');

    if (Channel = '#') and Web_VarExists('channel') then try
      Channel:=Web_GetVar('channel');
    except
      Channel:='#lentilwars';
    end;

    if Web_VarExists('linecount') then try
      Count:=StrToInt(Web_GetVar('linecount'));
    except
      Count:=50;
    end;

    if Web_VarExists('sender') then try
      Sender:=Web_GetVar('sender');
    except
      Sender:='';
    end;

    writeln('<table border="0">');
    writeln('<font size="10">');

    sql.clear;
    if Length(Sender) > 0 then
      sql.add('select first ' + IntToStr(Count) + ' sender, msg, cast(logtime as varchar(25)) as logtime from tbl_loglines where (reciever=''' + Channel + ''' and sender=''' + Sender + ''') order by loglineid desc')
    else
      sql.add('select first ' + IntToStr(Count) + ' sender, msg, cast(logtime as varchar(25)) as logtime from tbl_loglines where reciever=''' + Channel + ''' order by loglineid desc');

    open;
    while not eof do begin
      HTMLCode.Add('<tr><td>' + Copy(fieldbyname('logtime').asstring, 1, 19) +
                   '</td><td>' + fieldbyname('sender').asstring+'</td><td>' +
                   fieldbyname('msg').asstring+'</td></tr>');
      Next;
    end;
    
    if HTMLCode.Count > 0 then
      for i:=HTMLCode.Count - 1 downto 0 do
        Writeln(HTMLCode[i]);
        
    close;
    writeln('</table></font><hr>');
    HTMLCode.Free;
  end;

  Web_FileOut('html/footer.html');
  writeln('</body></html>');

  LogFBConnection.Close;
  LogFBConnection.free;
  LogTransaction.free;
  LogQuery.free;
end.

