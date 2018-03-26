unit fprDeleteTree;

{$mode objfpc}{$H+}

interface

uses
  sysutils;

function DeleteTree(const DirectoryName: string; OnlyChildren: boolean): boolean;

implementation

function DeleteTree(const DirectoryName: string; OnlyChildren: boolean): boolean;
const
  //Don't follow symlinks on *nix, just delete them
  DeleteMask = faAnyFile {$ifdef unix} or faSymLink{%H-} {$endif unix};
var
  FileInfo: TSearchRec;
  CurSrcDir: String;
  CurFilename: String;
begin
  Result:=false;
  CurSrcDir:=IncludeTrailingPathDelimiter(DirectoryName);
  if FindFirst(CurSrcDir+AllFilesMask,DeleteMask,FileInfo)=0 then begin
    repeat
      // check if special file
      if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='') then
        continue;
      CurFilename:=CurSrcDir+FileInfo.Name;
      if ((FileInfo.Attr and faDirectory)>0)
         {$ifdef unix} and ((FileInfo.Attr and faSymLink{%H-})=0) {$endif unix} then begin
        if not DeleteTree(CurFilename,false) then exit;
      end else begin
        if not DeleteFile(CurFilename) then exit;
      end;
    until FindNext(FileInfo)<>0;
  end;
  FindClose(FileInfo);
  if (not OnlyChildren) and (not RemoveDir(CurSrcDir)) then exit;
  Result:=true;
end;


end.

