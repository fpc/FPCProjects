program cgiFpcBot;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils,sqldb,ibconnection;

const
  MASTERKEY = 'masterkey';

var LogFBConnection : TIBConnection;
    LogTransaction  : TSQLTransaction;
    LogQuery        : TSQLQuery;
    Channel         : string;

begin
  Channel := GetEnvironmentVariable('QUERY_STRING');

  LogFBConnection := tIBConnection.Create(nil);
  with LogFBConnection do
    begin
    DatabaseName := '/opt/firebird/data/fpcbot.fdb';
    UserName := 'sysdba';
    Password := MASTERKEY;
    end;

  LogTransaction := tsqltransaction.create(nil);
  LogTransaction.database := LogFBConnection;

  LogQuery := tsqlquery.Create(nil);
  with LogQuery do
    begin
    DataBase := LogFBConnection;
    transaction := LogTransaction;

    ReadOnly:=True;

    sql.clear;
    sql.add('select sender, msg, cast(logtime as varchar(25)) as logtime from tbl_loglines where reciever=''#' + channel + ''' order by loglineid');
//    sql.add('select * from tbl_loglines where reciever=''#' + channel + ''' order by loglineid');
    writeln('Content-type: text/HTML');
    writeln('');
    writeln('<html><head><title>FPCBot chatlog</title></head>');
    writeln('<body><table>');
    open;
    while not eof do
      begin
      writeln('<tr><td>'+Copy(fieldbyname('logtime').asstring, 1, 19)+'</td><td>'+fieldbyname('sender').asstring+'</td><td>'+ fieldbyname('msg').asstring+'</td></tr>');
      Next;
      end;
    close;
    writeln('</table></body></html>');
    end;
  LogFBConnection.Close;
  LogFBConnection.free;
  LogTransaction.free;
  LogQuery.free;
end.

