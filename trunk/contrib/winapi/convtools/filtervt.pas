program filtervt;
{
usage:
 QoD Script that generates makefile.fpc dependancies from -va output.
 The batchfiles to generate the output are included (finddep.bat which
 calls docomp2.bat internally). This will generate about a 70 MB testlog.log
 file.

 Then use  (cygwin tools or unix)
 grep -i "load from" | sort |uniq | filtervt 

 -va output |grep -i "load from" |sort|uniq |thisprogram

Note that a file disallowed.txt with RTL unit names (units not to include
in the dependancies in uppercase) should exist.
}

{$mode Delphi}

uses Classes;

Type UnitClass = class
                    Name : String;
                    deps : TStringList;
                   end;


function consume(var origin:string):String;

var i,j : Integer;

begin
 while (length(Origin)>0) and (Origin[1] IN [#9,' ']) do Delete(origin,1,1);
 i:=Pos(#9,Origin);
 j:=Pos(' ',Origin);
 If (i=0) and (j=0) Then
   begin
     result:=Origin;
     origin:='';
   end
 else
  begin
    if (i=0) or (j<i) Then
      i:=j;
    result:=Copy(Origin,1,j-1);
    If result[1]='(' Then delete(result,1,1);
    if (length(result)>0) and (result[length(result)]=')') then Delete(result,length(result),1);
    Delete(Origin,1,j);
    while (length(Origin)>0) and (Origin[1] IN [#9,' ']) do Delete(origin,1,1);
  end
end;

var Disallowed : TStringList;

procedure processpair(TheList:TList;Name,Dep:String);

function createnew():unitclass;

var u2 : unitclass;


begin
 u2:=unitclass.create;
 u2.name:=name;
 u2.deps:=TStringList.Create;
 u2.deps.add(dep);
 result:=u2;
end;

var u : unitclass;
    found : boolean;
    i : integer;

begin
  if (disallowed.indexof(name)<>-1) or (disallowed.indexof(dep)<>-1) then
    exit; // we don't have to register these.
  found:=false;
  If thelist.count=0 Then
    thelist.add(createnew())
  else
   begin
     For i:=0 to thelist.count-1 Do
       begin
         u:=unitclass(thelist.items[i]);
         if name=u.name Then
           begin
             if u.deps.IndexOf(dep)=-1 Then
                u.deps.add(dep);
             found:=true;
           end;
        end;
     if not found then
       thelist.add(createnew());
   end;
end;


Var S,s2,S3 : String;
    TheList:TList;
    i,j:integer;
    u : unitclass;

begin
  Disallowed:=TStringList.Create;
  disallowed.LoadFromFile('disallowed.txt');
  TheList:=TList.Create;

  while not EOF(input) DO
    begin
      readln(S);
      If (length(s)>10) Then
        begin
          if S[1]='(' Then
            begin
              Consume(S);
              consume(s);
              consume(s);
              s2:=lowercase(consume(s));
              consume(s);
              // bepaal s2[2]='m' of s2[2]='n'
              consume(s);
              s3:=lowercase(consume(s));
              processpair(thelist,s2,s3);
            end;
        end;
    end;

  If thelist.count<>0 Then
     For i:=0 to thelist.count-1 Do
       begin
         u:=unitclass(thelist.items[i]);
         write(u.name,'$(PPUEXT) : ');
         if u.deps.count>0 then
           for j:=0 to u.deps.count-1 Do
             write(u.deps[j],'$(PPUEXT) ');

         writeln;
       end;

end.
