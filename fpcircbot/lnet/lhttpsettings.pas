unit lHTTPSettings;

{$mode objfpc}{$H+}

interface

  function GetMimeFile: string;
  function GetPort: Word;
  function GetHTTPPath: string;
  function GetCGIPath: string;
  function GetCGIRoot: string;
  function GetScriptPathPrefix: string;
  function GetPHPCGIBinary: string;

implementation

uses
  Classes, SysUtils, IniFiles;

const
  DEF_PORT = '3880';
  
var
  SettingsFile: TIniFile;
  HomeDir: string;
  CurDir: string;
  
function CreateDefaultIni(const aFilePath: string): TIniFile;

  procedure AddPath(const aName, aPath: string; const CD: Boolean = True);
  begin
    Result.WriteString('PATH', aName, aPath);
    if CD and not DirectoryExists(aPath) then
      if not CreateDir(aPath) then
        Writeln('Unable to create directory: ', aPath);
  end;
  
  procedure CopyFile(const FromF, ToF: string);
  const
    MAX_SIZE = 65536;
  var
    f1, f2: TFileStream;
    n: Integer;
    Buf: array[0..MAX_SIZE-1] of Byte;
  begin
    f1:=TFileStream.Create(FromF, fmOpenRead);
    f2:=TFileStream.Create(ToF, fmCreate);
    
    while f1.Position < f1.Size do begin
      n:=f1.Read(Buf, MAX_SIZE);
      f2.Write(Buf, n);
    end;
    
    f1.Free;
    f2.Free;
  end;
  
  procedure AddFile(const aName, aFile: string);
  begin
    Result.WriteString('PATH', aName, aFile);
    if not FileExists(aFile) then begin
      if FileExists(CurDir + 'mime.types') then
        CopyFile(CurDir + 'mime.types', aFile)
      else if FileExists('/etc/mime.types') then
        CopyFile('/etc/mime.types', aFile)
      else
        Writeln('Warning! File does not exist: ', aFile);
    end;
  end;

begin
  Writeln('Creating default configuration file in: ', aFilePath);
  if not DirectoryExists(ExtractFilePath(aFilePath)) then
    CreateDir(ExtractFilePath(aFilePath));
  Result:=TIniFile.Create(aFilePath);
  
  AddPath('httpdir', HomeDir + PathDelim + 'http_docs');
  AddPath('cgiroot', HomeDir + PathDelim + 'cgi-bin');
 {$ifndef MSWINDOWS}
  AddPath('cgipath', '/usr/local/bin:/usr/bin:/bin:' + HomeDir + '/bin', False);
 {$else}
  AddPath('cgipath', HomeDir + PathDelim + 'cgi-bin', False);
 {$endif}
  AddFile('mimetypes', HomeDir + PathDelim + 'mime.types');
  Result.WriteString('PATH', 'cgiprefix', 'cgi-bin' + PathDelim);
 {$ifndef MSWINDOWS}
  Result.WriteString('PATH', 'phpcgibin', '/usr/lib/cgi-bin/php');
 {$else}
  Result.WriteString('PATH', 'phpcgibin', 'php');
 {$endif}
  Result.WriteString('NET', 'port', DEF_PORT);
  Result.UpdateFile;
end;
  
procedure InitSettings;
const
  INI_NAME = 'fphttpd.ini';
var
  SearchPaths: TStringList;
  i: Integer;
begin
  SearchPaths:=TStringList.Create;
  SearchPaths.Add(HomeDir);
  SearchPaths.Add('/etc');
  SearchPaths.Add('/usr/local/etc');
  SearchPaths.Add(ExtractFilePath(ParamStr(0)));

  for i:=0 to SearchPaths.Count-1 do
    if FileExists(SearchPaths[i] + PathDelim + INI_NAME) then begin
      Writeln('Loading settings from file: ', SearchPaths[i] + PathDelim + INI_NAME);
      SettingsFile:=TIniFile.Create(SearchPaths[i] + PathDelim + INI_NAME);
      SearchPaths.Free;
      Exit;
    end;
  // no file found, create default one in home
  SettingsFile:=CreateDefaultIni(GetEnvironmentVariable('HOME') + PathDelim +
                                 '.fphttpd' + PathDelim + INI_NAME);
  SearchPaths.Free;
end;

procedure FreeSettings;
begin
  SettingsFile.Free;
end;

function GetMimeFile: string;
begin
  Result:=SettingsFile.ReadString('PATH', 'mimetypes', '');
end;

function GetPort: Word;
begin
  Result:=Word(StrToInt(SettingsFile.ReadString('NET', 'port', DEF_PORT)));
end;

function GetHTTPPath: string;
begin
  Result:=SettingsFile.ReadString('PATH', 'httpdir', '') + PathDelim;
end;

function GetCGIPath: string;
begin
  Result:=SettingsFile.ReadString('PATH', 'cgipath', '') + PathDelim;
end;

function GetCGIRoot: string;
begin
  Result:=SettingsFile.ReadString('PATH', 'cgiroot', '') + PathDelim;
end;

function GetScriptPathPrefix: string;
begin
  Result:=SettingsFile.ReadString('PATH', 'cgiprefix', '') + PathDelim;
end;

function GetPHPCGIBinary: string;
begin
  Result:=SettingsFile.ReadString('PATH', 'phpcgibin', '') + PathDelim;
end;
  
initialization
  CurDir:=ExtractFilePath(ParamStr(0));
  HomeDir:=GetEnvironmentVariable('HOME') + PathDelim + '.fphttpd';
  InitSettings;

finalization
  FreeSettings;

end.

