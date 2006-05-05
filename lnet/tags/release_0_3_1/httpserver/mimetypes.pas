unit mimetypes;

{$mode objfpc}{$h+}

interface

uses
  classes, sysutils, strutils;

type
  TStringObject = class(TObject)
    Str: string;
  end;

var
  MimeList: TStringList;

implementation

const
  MimeFileName: string = '/etc/mime.types';

procedure InitMimeList;
var
  MimeFile: Text;
  lPos, lNextPos: integer;
  lLine, lName: string;
  lStrObj: TStringObject;
begin
  MimeList := TStringList.Create;
  if FileExists(MimeFileName) then
  begin
    Assign(MimeFile, MimeFileName);
    Reset(MimeFile);
    while not Eof(MimeFile) do
    begin
      ReadLn(MimeFile, lLine);
      if (Length(lLine) = 0) or (lLine[1] = '#') then
        continue;

      lPos := Pos(#9, lLine);
      if lPos = 0 then
        continue;
      lName := Copy(lLine, 1, lPos-1);

      while (lPos <= Length(lLine)) and (lLine[lPos] in [#9,' ']) do
        Inc(lPos);
      if lPos > Length(lLine) then
        continue;
      repeat
        lNextPos := PosEx(' ', lLine, lPos);
        if lNextPos = 0 then
          lNextPos := Length(lLine)+1;
        lStrObj := TStringObject.Create;
        lStrObj.Str := lName;
        MimeList.AddObject(Copy(lLine, lPos, lNextPos-lPos), lStrObj);
        lPos := lNextPos+1;
      until lPos > Length(lLine);
    end;
    close(MimeFile);
  end;
  MimeList.Sorted := true;
end;

initialization
  InitMimeList;
finalization
  FreeAndNil(MimeList);
end.
