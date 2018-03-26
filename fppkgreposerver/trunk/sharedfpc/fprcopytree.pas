unit fprCopyTree;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  FileUtil,
  SysUtils;

function CopyTree(const SrcName, DestName: string): boolean;


implementation

function HasTrailingPathDelimiter(const Name: string): Boolean;
begin
  Result := IsPathDelimiter(Name, Length(Name));
end;

function CopyTree(const SrcName, DestName: string): boolean;
const
  //Don't follow symlinks on *nix, just copy them
  DeleteMask = faAnyFile {$ifdef unix} or faSymLink{%H-} {$endif unix};
var
  FileInfo: TSearchRec;
  CurSrcDir: String;
  CurDestDir: string;
  CurFilename: String;
begin
  Result:=false;

  // src trailing delim     dest trailing delim    action
  //     Yes                      Yes              Copy all files in src into dest. Dest must exist.
  //     No                       Yes              Copy src into dest. Dest must exist.
  //     Yes                      No               If dest does not exist, create it. Copy all files in src into dest.
  //     No                       No               Dest may not exist. Create dest and copy all files in src into dest.

  if HasTrailingPathDelimiter(DestName) then
    begin
    if not DirectoryExists(DestName) then
      Exit;

    if HasTrailingPathDelimiter(SrcName) then
      begin
      CurSrcDir := SrcName;
      CurDestDir := DestName;
      end
    else
      begin
      CurDestDir := DestName + ExtractFileName(SrcName);
      CreateDir(CurDestDir);
      CurSrcDir := SrcName + PathDelim;
      end;
    end
  else
    begin
    CurDestDir := DestName + PathDelim;
    if HasTrailingPathDelimiter(SrcName) then
      begin
      if not FileExists(CurDestDir) then
        CreateDir(CurDestDir);
      CurSrcDir := SrcName;
      end
    else
      begin
      if FileExists(CurDestDir) then
        Exit;
      CreateDir(CurDestDir);
      CurSrcDir := SrcName + PathDelim;
      end;
    end;

  if FindFirst(CurSrcDir+AllFilesMask,DeleteMask,FileInfo)=0 then
    begin
    repeat
      // check if special file
      if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='') then
        continue;
      CurFilename:=CurSrcDir+FileInfo.Name;
      if ((FileInfo.Attr and faDirectory)>0)
         {$ifdef unix} and ((FileInfo.Attr and faSymLink{%H-})=0) {$endif unix} then
        begin
        if not CopyTree(CurFilename, CurDestDir+FileInfo.Name) then exit;
        end
      else
        if not CopyFile(CurFilename, CurDestDir+FileInfo.Name, True) then exit;
    until FindNext(FileInfo)<>0;
    end;
  FindClose(FileInfo);
  Result:=true;
end;

end.

