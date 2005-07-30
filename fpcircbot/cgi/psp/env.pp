{$H+}{$MODE OBJFPC}
{
 This is just an auxilliary unit for web.pp.
 It contains more proper functions for getting environment variables than dos unit.
 It solves the problem of 255 char limit in all Win32, UNIX and OS/2 platforms.
 
 Code for OS/2 and WIN32 is taken from dos unit for these platforms but with some
 fixes for AnsiString usage.
 
 Vladimir Sibirov a.k.a. Trustmaster.
}
{$IFDEF WIN32}{$LINKLIB kernel32}{$ENDIF}
{$IFDEF UNIX}{$LINKLIB c}{$ENDIF}
unit env;

interface

{$IFDEF OS2}
type    PPchar=^Pchar;

var EnvC: longint; external name '_envc';
    EnvP: ppchar; external name '_environ';
{$ENDIF}

{$IFDEF UNIX}
function getenv(const envvar: string): string;
{$ENDIF}

{$IFDEF OS2}
function getenv(const envvar : string) : string;
{$ENDIF}

{$IFDEF WIN32}
Function GetEnv(envvar: string): string;
{$ENDIF}

implementation

// Found here and there: replacement for dos unit usage. Prevents from reaching
// 255 char GET and COOKIE limit.

{$IFDEF UNIX}

uses
  cmem;
  
function c_getenv(const name: PChar): PChar; cdecl; external 'c' name 'getenv';

function getenv(const envvar : string) : string;
var
  p: pChar;
begin
  p := c_getenv(PChar(envvar));
  result := p;
  cmem.Free(p);
end;

{$ENDIF}

{$IFDEF OS2}
// From dos unit for OS/2

function envs:PPchar;assembler;

asm
    movl envp,%eax
end ['EAX'];

function envcount:longint;assembler;
asm
    movl envc,%eax
end ['EAX'];

function envstr(index : longint) : string;

var hp:Pchar;

begin
    if (index<=0) or (index>envcount) then
        begin
            envstr:='';
            exit;
        end;
    hp:=envs[index-1];
    envstr:=AnsiString(hp);
end;

function getenv(const envvar : string) : string;

var hs,_envvar : string;
    eqpos,i : longint;

begin
    _envvar:=upcase(envvar);
    getenv:='';
    for i:=1 to envcount do
        begin
            hs:=envstr(i);
            eqpos:=pos('=',hs);
            if copy(hs,1,eqpos-1)=_envvar then
                begin
                    getenv:=copy(hs,eqpos+1,length(hs)-eqpos);
                    exit;
                end;
        end;
end;

{$ENDIF}

{$IFDEF WIN32}
// From dos unit for Windows
// Tuned for AnsiStrings

function GetEnvironmentStrings : pchar; stdcall; external 'kernel32' name 'GetEnvironmentStringsA';
function FreeEnvironmentStrings(p : pchar) : longbool; stdcall; external 'kernel32' name 'FreeEnvironmentStringsA';


function envcount : longint;
var
   hp,p : pchar;
   count : longint;
begin
   p:=GetEnvironmentStrings;
   hp:=p;
   count:=0;
   while  hp^<>#0 do
     begin
        { next string entry}
        hp:=hp+strlen(hp)+1;
        inc(count);
     end;
   FreeEnvironmentStrings(p);
   envcount:=count;
end;


Function  EnvStr(index: integer): string;
var
   hp,p : pchar;
   count,i : longint;
begin
   { envcount takes some time in win32 }
   count:=envcount;

   { range checking }
   if (index<=0) or (index>count) then
     begin
        envstr:='';
        exit;
     end;
   p:=GetEnvironmentStrings;
   hp:=p;

   { retrive the string with the given index }
   for i:=2 to index do
     hp:=hp+strlen(hp)+1;

   envstr:=AnsiString(hp);
   FreeEnvironmentStrings(p);
end;


Function  GetEnv(envvar: string): string;
var
   s : string;
   i : longint;
   hp,p : pchar;
begin
   getenv:='';
   p:=GetEnvironmentStrings;
   hp:=p;
   while hp^<>#0 do
     begin
        s:=AnsiString(hp);
        i:=pos('=',s);
        if upcase(copy(s,1,i-1))=upcase(envvar) then
          begin
             getenv:=copy(s,i+1,length(s)-i);
             break;
          end;
        { next string entry}
        hp:=hp+strlen(hp)+1;
     end;
   FreeEnvironmentStrings(p);
end;

{$ENDIF}

end.
