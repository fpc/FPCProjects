unit LazProfSelectFiles;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, ButtonPanel, CheckLst;

type

  { TSelectFilesForm }

  TSelectFilesForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    CheckListBox1: TCheckListBox;
  private
    { private declarations }
  public
    { public declarations }
    function SearchFilesInPath(DirList: TStrings; var List: TStrings): boolean;
  end;

var
  SelectFilesForm: TSelectFilesForm;

function ShowSelectFilesForm: TStrings;

implementation

uses
  LazProfResourceStings;

{ TSelectFilesForm }

function ShowSelectFilesForm: TStrings;
var
  ListItems: TStrings;
  FolderItems: TStrings;
  i: integer;
begin
  try
    SelectFilesForm := TSelectFilesForm.Create(nil);
    FolderItems := TStringList.Create;
    FolderItems.Add('C:\Documents and Settings\dblaszyk\My Documents\develop\fpprofiler\trunk\lazprof');

    SelectFilesForm.SearchFilesInPath(FolderItems, ListItems);

    if Assigned(ListItems) then
    begin
      SelectFilesForm.CheckListBox1.Items.AddStrings(ListItems);
      ListItems.Free;
    end;

    FolderItems.Free;

    SelectFilesForm.ShowModal;

    Result := TStringList.Create;
    for i := 0 to SelectFilesForm.CheckListBox1.Items.Count - 1 do
      if SelectFilesForm.CheckListBox1.Selected[i] then
        Result.Add(SelectFilesForm.CheckListBox1.Items[i]);
  finally
    SelectFilesForm.Free;
  end;
end;

function TSelectFilesForm.SearchFilesInPath(DirList: TStrings; var List: TStrings): boolean;

  function FileMatches(const Filename: string): boolean;
  var
    FileExtension: string;
  begin
    Result := False;
    FileExtension := ExtractFileExt(Filename);

    Result := (FileExtension = '.pas') or
              (FileExtension = '.pp') or
              (FileExtension = '.inc');
  end;

  function SearchInDirectory(const MainDirectory: string; Lvl: integer): boolean;
  var
    FileInfo: TSearchRec;
    FullFilename: string;
  begin
    Result := False;
    if (not DirPathExists(MainDirectory)) or (Lvl > 20) then
      exit;
    if FindFirstUTF8(MainDirectory + GetAllFilesMask,
      faAnyFile, FileInfo) = 0 then
    begin
      repeat
        // check if special file
        if (FileInfo.Name = '.') or (FileInfo.Name = '..') or (FileInfo.Name = '') then
          continue;
        FullFilename := MainDirectory + FileInfo.Name;

        if (FileInfo.Attr and faDirectory) > 0 then
        begin
          //disable recursive searching
          //if SubDirsCheckbox.Checked then
          // begin
          //   // search recursively
          //   if not SearchInDirectory(AppendPathDelim(FullFilename), Lvl + 1) then
          //     break;
          // end;
         end
        else
        begin
          if FileMatches(FullFilename) then
            List.Add(FullFilename);
        end;
      until FindNextUTF8(FileInfo) <> 0;
    end;
    FindCloseUTF8(FileInfo);
    Result := True;
  end;

var
  Directory: string;
  i: integer;
begin
  Result := False;
  List := nil;

  try
    for i := 0 to DirList.Count - 1 do
    begin
      Directory := AppendPathDelim(DirList[i]);

      // search files
      List := TStringList.Create;
      if not SearchInDirectory(Directory, 0) then
        exit;
    end;

    Result := True;
  finally
    if not Result then
    begin
      List.Free;
      List := nil;
    end;
  end;
end;

initialization
  {$I lazprofselectfiles.lrs}

end.

