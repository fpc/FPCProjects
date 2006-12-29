unit tstutil;
// MvdV 2006-12-29 this unit was not included with the tests. Provided a
//  trivial implementation to get tests compiling

interface
{$mode delphi} 
uses sysutils;


procedure openlog(name:string);
procedure writelog(const s: string);
procedure closelog;
function randomstr(i:integer):string;
function gettickcount : longint;

procedure WriteLogStrDump(S:string);

implementation

{$ifdef win32}
  uses windows;
{$endif}

var flog : text;

procedure openlog(name:string);
begin
 assign(flog,name);
 rewrite(flog);
end;

procedure writelog(const s: string);
begin
  writeln(flog,s);
end;

procedure closelog;
begin
 close(flog);
end;

function randomstr(i:integer):string;

var j : integer;
begin
  setlength(result,i);
  for j:=1 to i do
    result[j]:=chr(64+random(32+26)); // roughly till end of small chars.
end;

{$IFDEF VirtualPascal}
function gettickcount : longint;
begin
  gettickcount := VPUtils.GetTimemSec;
end;
{$ENDIF}

{$ifdef FPC}
function gettickcount : longint;
begin
  gettickcount :=0;  // no reliable crossplatform one atm.
end;
{$endif}

procedure WriteLogStrDump(S:string);
var s2:string;
   i : integer;
begin
  s2:='';
  for i:=1 to length(s) do
   s2:=s2+inttohex(ord(s[i]),2) +' ';
  writelog(s2);   
end;

end.