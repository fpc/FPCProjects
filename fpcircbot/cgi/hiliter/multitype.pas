unit multitype;
{$mode objfpc} {$h+}
interface

type
  TMultiType = record
    aString: string;
    aFile: file;
    aTextFile: text;
    mtype: (mtString, mtFile, mtTextFile);
  end;


implementation

end.
