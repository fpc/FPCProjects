program dbdump;

{$mode objfpc}{$H+}

{$i baseinc.inc}

{$ifndef noDB}
uses
  Classes, SqlDB, IBConnection;
  
const
  {$i hiddeninc.inc}

var
  f: TextFile;
  
procedure DumpDB;
var
  List: TStringList;
  Query: TSQLQuery;
  Con: TSQLConnection;
  Tran: TSQLTransaction;
  
  procedure InitDB;
  begin
    Con := tIBConnection.Create(nil);
    with Con do begin
      DatabaseName := DBPath;
      UserName := CgiDBName;
      Password := CgiDBPass;
    end;

    Tran := tsqltransaction.create(nil);
    Tran.database := Con;

    Query := tsqlquery.Create(nil);
    with Query do begin
      DataBase := Con;
      transaction := Tran;
      ReadOnly:=True;
    end;
  end;
  
  procedure AppendToFile(var aList: TStringList);
  var
    i: Longint;
  begin
    Write('.');
    for i:=0 to List.Count-1 do
      Writeln(f, List[i]);
    List.Clear;
  end;
  
begin
  List:=TStringList.Create;
  InitDB;
  with Query do begin
    sql.clear;
    sql.add('select * from tbl_loglines where sender <> ''' + BotName + '''');
    open;
    while not eof do begin
      List.Add(FieldByName('msg').AsString);
      if List.Count > 10000 then    // we don't want to ruin the ram do we?
        AppendToFile(List);
      Next;
    end;
    close;
  end;
  List.Free;
end;

begin
  AssignFile(f, 'dbdump.txt');
  Rewrite(f);
  Write('Dumping DB...');
  DumpDB;
  Writeln(' done.');
  CloseFile(f);
end.

{$else}
//*******************************NODB*********************************

begin
  Writeln('Compile with DB support if you want to dump the database into a textfile');
end.

//*******************************NODB*********************************
{$endif}
