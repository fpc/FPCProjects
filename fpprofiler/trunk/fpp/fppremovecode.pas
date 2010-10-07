unit fppRemoveCode;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPPEnvironment, FPPUtils;

type

  { TfppRemoveCode }

  TfppRemoveCode = class(TObject)
  private
    Environment: TEnvironment;

    procedure RemoveProfilingCode(FileList: TStrings);
    procedure RemoveProfilingCode(const FileName: string); overload;
  public
    constructor Create(Env: TEnvironment);

    procedure Run;
  end;

implementation

{ TfppRemoveCode }

procedure TfppRemoveCode.RemoveProfilingCode(FileList: TStrings);
var
  i: integer;
begin
  //restore the original files
  for i := 0 to FileList.Count - 1 do
    if FileExists(FileList[i]) then
      RemoveProfilingCode(FileList[i]);
end;

procedure TfppRemoveCode.RemoveProfilingCode(const FileName: string);
var
  NewFileName: string;
begin
  NewFileName := Copy(FileName, 1, Length(FileName) - Length(FPPROF_EXT));

  Environment.Write('revert: ' + NewFileName);

  DeleteFile(NewFileName);
  if RenameFile(FileName, NewFileName) then
    Environment.writeln(' .......... OK')
  else
    Environment.writeln(' .......... FAIL');
end;

constructor TfppRemoveCode.Create(Env: TEnvironment);
begin
  inherited Create;

  Environment := Env;
end;

procedure TfppRemoveCode.Run;
var
  fileList: TStrings;
begin
  try
    fileList := Environment.FileList('.fpprof');
    RemoveProfilingCode(fileList);
  finally
    fileList.Free;
  end;
end;

end.

