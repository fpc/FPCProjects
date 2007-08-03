{*******************************************************************************

                           PSP/PWU File sharing

********************************************************************************
  Functions for safe file sharing.

  Copyright (c) 2003-2006 by PSP devel team. See PSP License for information.

  Authors/Credits: Trustmaster (Vladimir Sibirov), L505 (Lars Olson)
********************************************************************************}

unit fileshare;

{$IFDEF FPC}{$MODE OBJFPC}{$H+}
    {$IFDEF EXTRA_SECURE}
     {$R+}{$Q+}{$CHECKPOINTER ON}
    {$ENDIF}
{$ENDIF}

interface

function FileMarkRead(const fname: string; var key: word): boolean;
function FileMarkWrite(const fname: string): boolean;
function FileUnmarkRead(const fname: string; key: word): boolean;
function FileUnmarkWrite(const fname: string): boolean;

implementation

uses
  fileutil,
  sysutils
 {$ifdef win32} ,
  windows  // DWORD declaration, SLEEP
 {$endif}
 ;

const
  GC_TIMEOUT = 3.0; // If flag older than 3 minutes - it is garbage left after error


{$IFDEF WIN32}
//procedure sleep(dwMilliseconds:DWORD); stdcall; external 'kernel32' name 'Sleep'; // caused error in Delphi, use Windows.pas
{$ENDIF}
{$IFDEF UNIX}
function sleep(seconds: longint): longint; cdecl; external 'c' name 'sleep';
{$ENDIF}



// Suspends the execution until writing operations are done
function FileSuspendRead(const fname: string): boolean;
begin
  // Garbage collection
  if FileExists_readwrite(fname + '.lfw')
    and ((abs(Now - FileDateToDateTime(FileAge(fname + '.lfw'))) * 1440)
    >= GC_TIMEOUT)
  then
    DeleteFile(pchar(fname + '.lfw'));
  // Suspending
  while FileExists_plain(fname + '.lfw') do
  begin
   {$IFDEF WIN32}
    sleep(10);
   {$ENDIF}
   {$IFDEF UNIX}
    sleep(1);
   {$ENDIF}
  end;
  result:= true;
end;

// Suspends the execution until the file is unlocked
function FileSuspendWrite(const fname: string): boolean;
var
  sr: TSearchRec;
begin
  // First waiting for writing operations finish
  result := FileSuspendRead(fname);

  while sysutils.FindFirst(fname + '.lfr*', faAnyFile, sr) = 0 do
  begin
    // Garbage collection
    repeat
      if (abs(Now - FileDateToDateTime(sr.Time)) * 1440) >= GC_TIMEOUT then
      begin
        if pos('\', fname) > 0 then
          DeleteFile(pchar(ExtractFilePath(fname) + '\' + sr.Name))
        else if pos('/', fname) > 0 then
          DeleteFile(pchar(ExtractFilePath(fname) + '/' + sr.Name))
        else
          DeleteFile(pchar(sr.Name));
      end;
    until FindNext(sr) <> 0;
    sysutils.Findclose(sr);
   {$IFDEF WIN32}
    sleep(10);
   {$ENDIF}
   {$IFDEF UNIX}
    sleep(1);
   {$ENDIF}
  end;
  sysutils.FindClose(sr); 
  result := true;
end;

// Creates unique file reading flag
function FileMarkRead(const fname: string; var key: word): boolean;
var
  fh: file of byte;
  lex: string;
begin
  result:= false;
  lex := '';
  FileSuspendRead(fname);
  if not FileExists_plain(fname) then exit; 
  repeat
    randomize;
    key := word(random(65534));
    str(key, lex);
  until not FileExists_plain(fname + '.lfr' + lex);
  assign(fh, fname + '.lfr' + lex);
  rewrite(fh);
  close(fh);
  if ioresult = 0 then result:= true;
end;

// Creates file writing flag
function FileMarkWrite(const fname: string): boolean;
var
  fh: file of byte;
begin
  result:= false;
  FileSuspendWrite(fname);
  if FileExists_readwrite(fname) and (not FileExists_plain(fname + '.lfw')) then
  begin
    assign(fh, fname + '.lfw');
    rewrite(fh);
    close(fh);
    result := true;
  end;
end;

// Removes unique file reading flag
function FileUnmarkRead(const fname: string; key: word): boolean;
var
  fh: file of byte;
  lex: string;
begin
  result:= false;
  lex := '';
  if not FileExists_read(fname) then exit;
  str(key, lex);
  if not FileExists_read(fname + '.lfr' + lex) then exit;
  assign(fh, fname + '.lfr' + lex);
  erase(fh);
  if ioresult = 0 then result := true;
end;

// Removes file writing flag
function FileUnmarkWrite(const fname: string): boolean;
var
  fh: file of byte;
begin
  result:= false;
  if FileExists_plain(fname + '.lfw') then
  begin                                               
    assign(fh, fname + '.lfw');
    erase(fh);
    if ioresult = 0 then result := true;
  end;
end;

end.

