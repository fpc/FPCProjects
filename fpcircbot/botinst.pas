program BotInst;

{$mode objfpc}{$H+}

uses
  Crt, Classes, SysUtils, Process, SQLDB, PQConnection;
  
const
  Version = 'v0.5';
  {$ifdef win32}
  CopyCommand = 'copy';
  {$else}
  CopyCommand = 'cp -f';
  {$endif}

var
  List: TStringList;
  Proc: TProcess;
  s   : string;
  isql, gsec: string;

procedure QuitInst;
begin
  try
    List.Free;
    Proc.Free;
  except on e: Exception do
    Writeln(e.message);
  end;
  Writeln('Installation canceled');
  Halt(1);
end;
  
function ReadString: string;
var
  c: Char;
  Quit: Boolean;
  MinX: Longint;
begin
  Quit:=False;
  Result:='';
  c:=#0;
  MinX:=WhereX;
  while not Quit do begin
    if KeyPressed then begin
      c:=ReadKey;
      case c of
        #8  : if WhereX > MinX then begin
                GotoXY(WhereX-1, WhereY);
                Write(' ');
                GotoXY(WhereX-1, WhereY);
                Delete(Result, Length(Result), 1);
              end;
        #13 : Quit:=True;
        #27 : QuitInst;
      else
        Write(c);
        Result:=Result + c;
      end;
    end;
    Delay(1);
  end;
  Writeln;
end;

function Query(const Question, Defaults: string): string;
begin
  Write(Question, ' [', Defaults, ']: ');
  Result:=ReadString;
  if Length(Result) = 0 then
    Result:=Defaults;
  s:=Result;
end;
  
procedure GetInfo(const FieldName, RepStr: string);
var
  QResult: string;
begin
  QResult:=Query(FieldName, StringReplace(RepStr, '$', '', [rfReplaceAll]));
  List.Text:=StringReplace(List.Text, RepStr, QResult, [rfReplaceAll]);
end;

procedure Confirm;
begin
  if Pos('n', Query('About to save data to disk, are you sure you want to continue?', 'y')) > 0 then
    QuitInst;
end;

procedure ExecuteSQL;
var
  CON: TSQLConnection;
  TRN: TSQLTransaction;
  QUE: TSQLQuery;
begin
  Writeln('Adding DB, password required');
  
  Proc.CommandLine:=gsec + ' fpcbot';
  Proc.Execute;
  while Proc.Active do Sleep(1);
  
  CON:=TPQConnection.Create(nil);
  with CON do begin
    DatabaseName:='fpcbot';
    HostName:='127.0.0.1';
    UserName := Query('DB admin name', GetEnvironmentVariable('USER'));
    Password := Query('DB admin password', '');
  end;

  TRN:=TSQLTransaction.create(nil);
  TRN.DataBase:=CON;

  QUE:=TSQLQuery.Create(nil);
  with QUE do begin
    DataBase:=CON;
    Transaction:=TRN;
    SQL:=List;
  end;
  
  Writeln('Executing SQL commands');
  
  try
    QUE.ExecSQL;
    TRN.Commit;
  except
    on e: Exception do begin
      Writeln('DB ERROR: ' + e.Message);
      QuitInst;
    end;
  end;
end;
  
var
  f: TextFile;
  CgiBin, BotDBName, BotDBPass, CGIDBName, CGIDBPass: string;
begin
  isql:='';
  gsec:='';
  List:=TStringList.Create;
  Proc:=TProcess.Create(nil);
  Proc.Options:=[poUsePipes, poNoConsole];
  if ParamCount < 1 then begin
    try
      List.LoadFromFile('include' + PathDelim + 'hiddeninc.inc.default');
    except
      Writeln('Unable to load file include' + PathDelim + 'hiddeninc.inc.default');
      Halt;
    end;
    // INTRO
    Writeln('FPCBot installer ', Version);
    Writeln('This installer will try to automaticly (with your input)');
    Writeln('install the required binaries and database setup for FPCBot.');
    Writeln('WARNING: if you want DB support, you must run this as a db user with createdb and createuser rights!');
    Writeln('You can quit the installer at any time by pressing escape.');
    Writeln;
    Writeln('------------------------------BOT STATS---------------------------------');

    // BOTSTATS
    GetInfo('BotName', '$botname');
    GetInfo('LogUrl', '$logurl');
    GetInfo('NickPass', '$nickpass');

    Writeln('------------------------------DB STATS----------------------------------');
    if Pos('y', Query('Do you want DB support? (web server and pgSQL required)', 'y')) > 0 then begin
      AssignFile(f, 'include' + PathDelim + 'baseinc.inc');
      Rewrite(f);
      CloseFile(f);
      
{      isql:=Query('PostgreSQL interpreter binary', '/usr/bin/psql');
      while not FileExists(isql) do begin
        Writeln('Invalid file or file not exist, try again');
        isql:=Query('PostgreSQL interpreter binary', '/usr/bin/psql');
      end;}

      gsec:=Query('createdb binary', '/usr/bin/createdb');
      while not FileExists(gsec) do begin
        Writeln('Invalid file or file not exist, try again');
        gsec:=Query('createdb binary', '/usr/bin/createdb');
      end;

      GetInfo('BotDBName', '$fpcbot');
      BotDBName:=s;
      GetInfo('BotDBPass', '$botpass');
      BotDBPass:=s;
      GetInfo('CgiDBName', '$cgifpc');
      CGIDBName:=s;
      GetInfo('CgiDBPass', '$cgipass');
      CGIDBPass:=s;

      CgiBin:=Query('Path to your cgi-bin directory', '/usr/lib/cgi-bin');
      while not DirectoryExists(CgiBin) do begin
        Write('Directory does not exist, try again: ');
        CgiBin:=Query('Path to your cgi-bin directory', '/usr/lib/cgi-bin');
      end;
      
      Confirm;

      Writeln('Writing hiddeninc.inc to hard disk');
      try
        List.SaveToFile('include' + PathDelim + 'hiddeninc.inc');
      except
        on e: Exception do begin
          Writeln(e.message);
          Writeln('Unable to save include file!');
        end;
      end;

      Writeln('Creating DB construction script');
      List.Clear;
      List.LoadFromFile('common' + PathDelim + 'create_database_script.sql');
      List.Text:=StringReplace(List.Text, '$botdbname', BotDBName, [rfReplaceall]);
      List.Text:=StringReplace(List.Text, '$cgidbname', CGIDBName, [rfReplaceall]);
      List.Text:=StringReplace(List.Text, '$botdbpass', BotDBPass, [rfReplaceall]);
      List.Text:=StringReplace(List.Text, '$cgidbpass', CGIDBPass, [rfReplaceall]);
      List.SaveToFile('dbscr.sql');

      ExecuteSQL;
      
      List.Clear;
      List.Add(CgiBin);
      List.SaveToFile('cgibin_path.ipt');

    end else begin // {$define noDB}
      Confirm;
      AssignFile(f, 'include' + PathDelim + 'baseinc.inc');
      Rewrite(f);
      Writeln(f, '{$define noDB}');
      CloseFile(f);
      Writeln('Writing hiddeninc.inc to hard disk');
      try
        List.SaveToFile('include' + PathDelim + 'hiddeninc.inc');
      except
        on e: Exception do begin
          Writeln(e.message);
          Writeln('Unable to save include file!');
        end;
      end;
    end;

    // CONFIG FILE
    List.Clear;
    List.LoadFromFile('bot' + PathDelim + 'botconfig.cfg.default');
    GetInfo('FPCBot root user', '$root');
    GetInfo('FPCBot root channel', '$channel');
    List.SaveToFile('bot' + PathDelim + 'botconfig.cfg');

  end else begin  // INSTALL
    if FileExists('cgibin_path.ipt') then begin
      // CPYING OF FILES
      List.LoadFromFile('cgibin_path.ipt');
      try
        CgiBin:=List[0];
	if not FileExists(CgiBin) then
	  raise Exception.Create('Path does not exist');
      except
        on e: Exception do begin
          Writeln('Error determining install path! Installaction canceled');
          Writeln(e.Message);
          Halt(1);
	end;
      end;
      Writeln('Copying files...');
      try
        Proc.CommandLine:=CopyCommand + ' cgi' + PathDelim + 'cgifpcbot ' + CgiBin;
        Proc.Execute;
        Proc.CommandLine:=CopyCommand + ' cgi' + PathDelim + 'PWU.conf ' + CgiBin;
        Proc.Execute;
        if not DirectoryExists(CgiBin + PathDelim + 'html') then
          CreateDir(CgiBin + PathDelim + 'html');
        Proc.CommandLine:=CopyCommand + ' cgi' + PathDelim + 'html' + PathDelim + '* ' +
                                        CgiBin + PathDelim + 'html';
        Proc.Execute;
        Writeln('Installation complete.');
      except
        on e: Exception do
          Writeln(e.message);
      end;
    end else Writeln('No DB support selected. Installation not required');
  end;
  Proc.Free;
  List.Free;
end.

