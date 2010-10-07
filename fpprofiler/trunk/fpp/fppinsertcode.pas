unit fppInsertCode;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPPEnvironment, FPPUtils, PScanner, fppWriter;

type

  { TfppInsertCode }

  TfppInsertCode = class(TObject)
  private
    Environment: TEnvironment;
    procedure InsertProfilingCode(FileList: TStrings);
    procedure ModifyCode(AFileName: string; tokenlist: TPasTokenList);
  public
    constructor Create(Env: TEnvironment);

    procedure Run;
  end;

implementation

{ TfppInsertCode }

procedure TfppInsertCode.ModifyCode(AFileName: string; tokenlist: TPasTokenList
  );
var
  i: integer;
  begin_count: integer;
  is_record: boolean;
  function_start: boolean;
  procedure_start: boolean;
  linenum: string;

  procedure InsertFPProfUnit;
  var
    i: integer;
  begin
    //find uses clause and insert unit
    for i := tokenlist.Count - 1 downto 0 do
    begin
      if (tokenlist[i].token = tkUses) then
      begin
        //insert fpprof unit (with whitespace and comma)
        tokenlist.Insert(i - 1, tkIdentifier, ' fpprof, ', -1);
        Exit;
      end;
    end;

    //unit not found, find program / unit keyword
    for i := tokenlist.Count - 1 downto 0 do
      if (tokenlist[i].token = tkProgram) or (tokenlist[i].token = tkUnit) then
      begin
        //insert fpprof unit (with uses keyword)
        tokenlist.Insert(i - 5, tkIdentifier, 'uses fpprof;', -1);
        Exit;
      end;

    //just try and insert it at the beginning
    tokenlist.Insert(Tokenlist.Count - 2, tkIdentifier, 'uses fpprof;', -1);
  end;

begin
  //insert fpprof unit
  if ExtractFileExt(AFileName) <> '.inc' then
    InsertFPProfUnit;

  //insert function fpprof_info after each tkBegin and before each tkEnd
  begin_count := 0;
  is_record := False;
  procedure_start := False;
  function_start := False;
  for i := tokenlist.Count - 1 downto 0 do
  begin
    str(tokenlist[i].line, linenum);

    case tokenlist[i].token of
      tkAsm: if not function_start and not procedure_start then
          Inc(begin_count);
      tkBegin:
      begin
        function_start := False;
        procedure_start := False;

        Inc(begin_count);

        if begin_count = 1 then
        begin
          tokenlist.Insert(i - 1, tkIdentifier, ' fpprof_entry_profile(' +
            linenum + '); ' + LineEnding, -1);
        end;
      end;
      tkCase: if not is_record then
          Inc(begin_count);
      tkEnd:
      begin
        if (begin_count = 1) and not is_record then
        begin
          tokenlist.Insert(i + 1, tkIdentifier, ' ;fpprof_exit_profile(' +
            linenum + '); ' + LineEnding, -1);
        end;

        is_record := False;

        if begin_count > 0 then
          Dec(begin_count);
      end;
      tkExcept: Inc(begin_count);
      tkFinally: Inc(begin_count);
      tkFunction: function_start := True;
      tkProcedure: procedure_start := True;
      tkRecord:
      begin
        Inc(begin_count);
        is_record := True;
      end;
    end; { case }
  end; { while }

  //save result for debuging
  //tokenlist.SaveToFile('test.debug.pp');
end;

procedure TfppInsertCode.InsertProfilingCode(FileList: TStrings);
var
  i: integer;
  PasTokenList: TPasTokenList;
  Success: boolean;
  writer: TFPPWriter;
begin
  PasTokenList := TPasTokenList.Create;

  writer := TFPPWriter.Create;
  writer.CreateIgnored;

  //make a copy of the original files and process them
  for i := 0 to FileList.Count - 1 do
  begin
    //skip if file is already converted or belongs to the profiling units
    {$note the profiling files should be determined at compile time?}
    if not FileExists(FileList[i] + FPPROF_EXT) and
      (ExtractFileName(FileList[i]) <> 'fpprof.pp') and
      (ExtractFileName(FileList[i]) <> 'fpputils.pas') and
      (ExtractFileName(FileList[i]) <> 'fppwriter.pas') and
      (ExtractFileName(FileList[i]) <> 'systemtime.inc') and
      (ExtractFileName(FileList[i]) <> 'win32systemtime.inc') then
    begin
      Environment.Write('insert: ' + FileList[i]);

      try
        Success := PasTokenList.ParseSource(FileList[i]);

        Success := Success and RenameFile(FileList[i], FileList[i] + FPPROF_EXT);

        //perform the code modification
        ModifyCode(FileList[i], PasTokenList);

        PasTokenList.SaveToFile(FileList[i]);

        if Success then
          Environment.WriteLn(' .......... OK')
      except
        Environment.WriteLn(' .......... FAIL');
        writer.AddIgnoredFile(FileList[i]);
      end;

      PasTokenList.Clear;
    end
    else
      Environment.WriteLn('skipping: ' + FileList[i]);
  end;

  writer.Save;
  writer.Free;

  PasTokenList.Free;
end;

constructor TfppInsertCode.Create(Env: TEnvironment);
begin
  inherited Create;

  Environment := Env;
end;

procedure TfppInsertCode.Run;
var
  fileList: TStrings;
begin
  try
    fileList := Environment.FileList('.pp;.pas;.inc;.lpr');
    InsertProfilingCode(fileList);
  finally
    fileList.Free
  end;
end;

end.

