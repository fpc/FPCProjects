
{$mode objfpc}
uses classes,lzs;

type 
     TLZTest=class(tlzbase)
             F1,F2:File;
             procedure testcompr;
             procedure testuncompr;
             function  ParentRead(var Data;MaxBytes:Longint):Longint; override;
             function  ParentWrite(COnst Data;Bytes:Longint):Boolean; override;
            end;



function  TLZTest.ParentRead(var Data;MaxBytes:Longint):Longint;

var i  :integer;

begin
 BlockRead(F1,Data,maxBytes,i);
 parentread:=i;
end;

function  TLZTest.parentWrite(Const Data;Bytes:Longint):boolean;
Begin
 BlockWrite(F2,Data,bytes);
 parentwrite:=True;
end;

procedure TLZTest.testcompr;

begin
 Assign (F1,'configure');
 reset(F1,1);
 Assign (F2,'configure.comp');
 Rewrite(f2,1);
 Compress(filesize(f1));
 Close(F1);
 Close(F2);
end;


procedure TLZTest.testuncompr;

begin
 Assign (F1,'configure.comp');
 reset(F1,1);
 Assign (F2,'configure.2');
 Rewrite(f2,1);
 expand;
 Close(F1);
 Close(F2);
end;

var lzt : TLZTest;

begin
 lzt := TLZTest.create;
 lzt.testcompr;
 lzt.testuncompr;
 lzt.free;
end.
 
