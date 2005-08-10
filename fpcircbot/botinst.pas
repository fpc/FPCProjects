program BotInst;

{$mode objfpc}{$H+}

uses
  Crt, Classes, SysUtils, Process;
  
const
  Version = 'v0.1';
  {$ifdef win32}
  CopyCommand = 'copy';
  {$else}
  CopyCommand = 'cp -f';
  {$endif}

var
  List: TStringList;
  Proc: TProcess;
  s   : string;
  
procedure QuitInst;
begin
  try
    List.Free;
    Proc.Free;
  except on e: Exception do
    Writeln(e.message);
  end;
  Writeln('Installation canceled');
  Halt;
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
  s:=Result;
  Writeln;
end;
  
procedure GetInfo(const FieldName, RepStr: string);
begin
  Write(FieldName, ': ');
  List.Text:=StringReplace(List.Text, RepStr, ReadString, [rfReplaceAll]);
end;
  
var
  isql, gsec: string;
  f: TextFile;
  CgiBin, DBFile, BotDBName, BotDBPass, CGIDBName, CGIDBPass, SysDBPass: string;
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
    Writeln('You can quit the installer at any time by pressing escape.');
    Writeln;
    Writeln('------------------------------BOT STATS---------------------------------');

    // BOTSTATS
    GetInfo('BotName', '$botname');
    GetInfo('LogUrl', '$logurl');
    GetInfo('NickPass', '$nickpass');

    Writeln('------------------------------DB STATS----------------------------------');
    Write('Do you want DB support? (you need to have a web server and firebird2 installed) [y/n] ');
    if Pos('y', LowerCase(ReadString)) > 0 then begin
      AssignFile(f, 'include' + PathDelim + 'baseinc.inc');
      Rewrite(f);
      CloseFile(f);
      
      Write('FireBird binary (example: /usr/bin/isql-fb): ');
      isql:=ReadString;
      while not FileExists(isql) do begin
        Writeln('Invalid file or file not exist, try again');
        Write('FireBird binary (example: /usr/bin/isql-fb): ');
        isql:=ReadString;
      end;

      Write('gsec binary (example: /usr/bin/gsec): ');
      gsec:=ReadString;
      while not FileExists(isql) do begin
        Writeln('Invalid file or file not exist, try again');
        Write('gsec binary (example: /usr/bin/gsec): ');
        gsec:=ReadString;
      end;

      GetInfo('DB sysdba password', '$bogus');
      SysDBPass:=s;
      GetInfo('BotDBName', '$fpcbot');
      BotDBName:=s;
      GetInfo('BotDBPass', '$botpass');
      BotDBPass:=s;
      GetInfo('CgiDBName', '$cgifpc');
      CGIDBName:=s;
      GetInfo('CgiDBPass', '$cgipass');
      CGIDBPass:=s;
      GetInfo('Path to db file (with filename, example: /var/firebird/fpcbot.fdb)', '$dbpath');
      DBFile:=s;

      Write('Path to your cgi-bin directory (example: /usr/lib/cgi-bin): ');
      CgiBin:=ReadString;
      while not DirectoryExists(CgiBin) do begin
        Write('Directory does not exist, try again: ');
        CgiBin:=ReadString;
      end;
      
      // DB CREATION
      if FileExists(DBFile) then begin
        Writeln('WARNING WARNING WARNING');
        Writeln('There is an already existing DB file in: ', DBFile);
        Writeln('If you want to recreate(deletes ALL content) your DB, continue');
        Writeln('otherwise press escape and chose another location');
        ReadString;
      end;

      Writeln('Writing hiddeninc.inc to hard disk');
      try
        List.SaveToFile('include' + PathDelim + 'hiddeninc.inc');
      except
        on e: Exception do begin
          Writeln(e.message);
          Writeln('Unable to save include file!');
        end;
      end;

      Writeln('Creating DB..');
      Proc.CommandLine:='rm -f ' + DBFile;
      Proc.Execute;
      while Proc.Active do Delay(1);
      Proc.CommandLine:=gsec + ' -user sysdba -password ' +
                        SysDBPass + ' -delete ' + BotDBName;
      Proc.Execute;
      while Proc.Active do Delay(1);
      Proc.CommandLine:=gsec + ' -user sysdba -password ' +
                        SysDBPass + ' -delete ' + CgiDBName;
      Proc.Execute;
      while Proc.Active do Delay(1);

      List.Clear;
      List.LoadFromFile('cgi' + PathDelim + 'create_database_script.sql');
      List.Text:=StringReplace(List.Text, '$dbpath', DBFile, [rfReplaceall]);
      List.Text:=StringReplace(List.Text, '$sysdbapass', SysDBPass, [rfReplaceall]);
      List.Text:=StringReplace(List.Text, '$botdbname', BotDBName, [rfReplaceall]);
      List.Text:=StringReplace(List.Text, '$cgidbname', CGIDBName, [rfReplaceall]);
      List.SaveToFile('dbscr.sql');

      Proc.CommandLine:=gsec + ' -user sysdba -password ' + SysDBPass + ' -add ' +
                        BotDBName + ' -pw ' + BotDBPass + ' -fname ' + BotDBName;
      Proc.Execute;
      while Proc.Active do Delay(1);
      Proc.CommandLine:=gsec + ' -user sysdba -password ' + SysDBPass + ' -add ' +
                        CgiDBName + ' -pw ' + CgiDBPass + ' -fname ' + CgiDBName;
      Proc.Execute;
      while Proc.Active do Delay(1);
      List.Clear;
      List.Add(isql + ' < dbscr.sql');
      List.SaveToFile('makedb.sh');
      Proc.CommandLine:='sh makedb.sh';
      Proc.Execute;
      while Proc.Active do Delay(1);
      List.Clear;
      List.Add(CgiBin);
      List.SaveToFile('cgibin_path.ipt');

    end else begin // {$define noDB}
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
      Writeln('Copying files...');
      try
        List.LoadFromFile('cgibin_path.ipt');
        CgiBin:=List[0];
        if not DirectoryExists(CgiBin) then begin
          Writeln('Invalid install directory, this is a bug, contact author');
          Halt;
        end;
        Proc.CommandLine:=CopyCommand + ' cgi' + PathDelim + 'cgifpcbot ' + CgiBin;
        Proc.Execute;
        Proc.CommandLine:=CopyCommand + ' cgi' + PathDelim + 'psp.ini ' + CgiBin;
        Proc.Execute;
        if not DirectoryExists(CgiBin + PathDelim + 'html') then
          CreateDir(CgiBin + PathDelim + 'html');
        Proc.CommandLine:=CopyCommand + ' cgi' + PathDelim + 'html' + PathDelim + 'footer.html ' +
                                        CgiBin + PathDelim + 'html';
        Proc.Execute;
        Proc.CommandLine:=CopyCommand + ' cgi' + PathDelim + 'html' + PathDelim + 'header.html ' +
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

