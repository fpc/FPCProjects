{ Web server settings

  Copyright (C) 2006 Micha Nelissen

  This library is Free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is diStributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; withOut even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a Copy of the GNU Library General Public License
  along with This library; if not, Write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
  
  This license has been modified. See file LICENSE.ADDON for more information.
  Should you find these sources without a LICENSE File, please contact
  me at ales@chello.sk
}

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
  
  AddPath('httpdir', HomeDir + 'http_docs');
  AddPath('cgiroot', HomeDir + 'cgi-bin');
 {$ifndef MSWINDOWS}
  AddPath('cgipath', '/usr/local/bin:/usr/bin:/bin:' + HomeDir + '/bin', False);
 {$else}
  AddPath('cgipath', HomeDir + 'cgi-bin', False);
 {$endif}
  AddFile('mimetypes', HomeDir + 'mime.types');
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
  {$ifndef MSWINDOWS}
  SearchPaths.Add('/etc/');
  SearchPaths.Add('/usr/local/etc/');
  {$endif}
  SearchPaths.Add(ExtractFilePath(ParamStr(0)) + PathDelim);

  for i:=0 to SearchPaths.Count-1 do
    if FileExists(SearchPaths[i] + INI_NAME) then begin
      Writeln('Loading settings from file: ', SearchPaths[i] + INI_NAME);
      SettingsFile:=TIniFile.Create(SearchPaths[i] + INI_NAME);
      SearchPaths.Free;
      Exit;
    end;
  // no file found, create default one in home
  SettingsFile:=CreateDefaultIni(HomeDir + INI_NAME);
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
  {$ifdef MSWINDOWS}
  HomeDir:=GetEnvironmentVariable('HOMEDRIVE') + PathDelim +
           GetEnvironmentVariable('HOMEPATH') + PathDelim + 'fphttpd' + PathDelim;
  {$else}
  HomeDir:=GetEnvironmentVariable('HOME') + PathDelim + '.fphttpd' + PathDelim;
  {$endif}
  InitSettings;

finalization
  FreeSettings;

end.

