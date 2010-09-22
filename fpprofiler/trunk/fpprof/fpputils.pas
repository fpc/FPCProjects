{
    This file is part of the Free Pascal Profiler.
    Copyright (c) 2007 by Darius Blaszyk

    Profiling helper functions and classes

    See the file COPYING.GPL, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit fpputils;

{$mode objfpc}{$H+}
{ $define debug}

interface

uses
  pscanner, PasTree, SysUtils, Classes, FPPWriter;

const
  FPPROF_EXT = '.fpprof';

type
  TPasToken = record
    token: TToken;
    value: string;
    line: integer;
  end;
  PPasToken = ^TPasToken;

  { TPasTokenList }

  TPasTokenList = class(TObject)
    FFileName: string;
  private
    FList: TFPList;

    function GetList(index: integer): TPasToken;
    procedure SetFileName(const AValue: string);
    procedure SetList(AIndex: integer; const AValue: TPasToken);

  public
    constructor Create;
    destructor Destroy; override;

    function Count: integer;
    function ParseSource(AFileName: string): boolean;
    procedure Add(AToken: TToken; AValue: string; ALineNumber: integer);
    procedure Clear;
    procedure Insert(APos: integer; AToken: TToken; AValue: string; ALineNumber: integer);
    procedure SaveToFile(const AFileName: string);
    property FileName: string read FFileName write SetFileName;
    property List[index: integer]: TPasToken read GetList write SetList; default;
  end;

  TModTokenProc = procedure(AFileName: string; tokenlist: TPasTokenList);

procedure FileSearch(SearchDir: string; ExtensionMask: string; var FileList: TStrings; Recursive: boolean = False);
procedure InsertProfilingCode(FileList: TStrings; ModTokenProc: TModTokenProc);
procedure RemoveProfilingCodeFromFile(const FileName: string);
procedure RemoveProfilingCode(FileList: TStrings);
procedure BackupProfilingCode(FileList: TStrings);

implementation

procedure FileSearch(SearchDir: string; ExtensionMask: string;
  var FileList: TStrings; Recursive: boolean = False);
var
  Info: TSearchRec;
  ExtensionList: TStrings;
begin
  SearchDir := IncludeTrailingPathDelimiter(SearchDir);

  ExtensionList := TStringList.Create;
  ExtensionList.Delimiter := ';';
  ExtensionList.DelimitedText := ExtensionMask;

  if FindFirst(SearchDir + AllFilesMask, faAnyFile and faDirectory, Info) = 0 then
  begin
    repeat
      if Recursive then
        if ((Info.Attr and faDirectory) = faDirectory) and (Info.Name <> '.') and (Info.Name <> '..') then
          FileSearch(SearchDir + Info.Name, ExtensionMask, FileList, Recursive);

      if ExtensionList.IndexOf(ExtractFileExt(Info.Name)) <> -1 then
      begin
        if FileList.IndexOf(SearchDir + Info.Name) = -1 then
          FileList.Add(SearchDir + Info.Name);
      end;
    until FindNext(Info) <> 0;
  end;
  FindClose(Info);

  ExtensionList.Free;
end;

procedure InsertProfilingCode(FileList: TStrings; ModTokenProc: TModTokenProc);
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
      Write('insert: ', FileList[i]);

      try
        Success := PasTokenList.ParseSource(FileList[i]);

        Success := Success and RenameFile(FileList[i], FileList[i] + FPPROF_EXT);

        //perform the code modification
        if Assigned(ModTokenProc) then
          ModTokenProc(FileList[i], PasTokenList);

        PasTokenList.SaveToFile(FileList[i]);

        if Success then
          writeln(' .......... OK')
      except
        writeln(' .......... FAIL');
        writer.AddIgnoredFile(FileList[i]);
      end;

      PasTokenList.Clear;
    end
    else
      WriteLn('skipping: ', FileList[i]);
  end;

  writer.Save;
  writer.Free;

  PasTokenList.Free;
  FileList.Free;
end;

procedure RemoveProfilingCodeFromFile(const FileName: string);
var
  NewFileName: string;
begin
  NewFileName := Copy(FileName, 1, Length(FileName) - Length(FPPROF_EXT));

  Write('revert: ', NewFileName);

  DeleteFile(NewFileName);
  if RenameFile(FileName, NewFileName) then
    writeln(' .......... OK')
  else
    writeln(' .......... FAIL');
end;

procedure RemoveProfilingCode(FileList: TStrings);
var
  i: integer;
begin
  //restore the original files
  for i := 0 to FileList.Count - 1 do
    if FileExists(FileList[i]) then
      RemoveProfilingCodeFromFile(FileList[i]);
end;

procedure BackupProfilingCode(FileList: TStrings);
var
  i: integer;
var
  SourceFileName: string;
begin
  //backup files
  for i := 0 to FileList.Count - 1 do
  begin
    SourceFileName := Copy(FileList[i], 1, Length(FileList[i]) - Length(FPPROF_EXT));

    if FileExists(SourceFileName) then
    begin
      Write('backup: ', SourceFileName);

      if RenameFile(SourceFileName, SourceFileName + '.bak') then
        writeln(' .......... OK')
      else
        writeln(' .......... FAIL');
    end;
  end;
end;

{ TPasTokenList }

function TPasTokenList.GetList(index: integer): TPasToken;
begin
  Result := TPasToken(FList[index]^);
end;

procedure TPasTokenList.SetFileName(const AValue: string);
begin
  if FFileName = AValue then
    exit;
  FFileName := AValue;

end;

procedure TPasTokenList.SetList(AIndex: integer; const AValue: TPasToken);
begin
  FList.Add(@AValue);
end;

constructor TPasTokenList.Create;
begin
  FList := TFPList.Create;
end;

destructor TPasTokenList.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

procedure TPasTokenList.Clear;
var
  i: integer;
begin
  for i := 0 to FList.Count - 1 do
    Dispose(PPasToken(FList[i]));

  FList.Clear;
end;

procedure TPasTokenList.Insert(APos: integer; AToken: TToken; AValue: string; ALineNumber: integer);
var
  pt: ^TPasToken;
begin
  New(pt);
  pt^.token := AToken;
  pt^.value := AValue;
  pt^.line := ALineNumber;
  FList.Insert(APos, pt);
end;

procedure TPasTokenList.SaveToFile(const AFileName: string);
var
  t: Text;
  i: integer;
begin
  Assign(t, AFileName);
  rewrite(t);

  for i := FList.Count - 1 downto 0 do
  begin
    case TPasToken(FList[i]^).token of
      tkWhitespace: Write(t, ' ');
      tkString: Write(t, TPasToken(FList[i]^).value);
      tkBraceOpen: Write(t, '(');
      tkBraceClose: Write(t, ')');
      tkMul: Write(t, '*');
      tkPlus: Write(t, '+');
      tkComma: Write(t, ',');
      tkMinus: Write(t, '-');
      tkDot: Write(t, '.');
      tkDivision: Write(t, '/');
      tkColon: Write(t, ':');
      tkSemicolon: Write(t, ';');
      tkLessThan: Write(t, '<');
      tkEqual: Write(t, '=');
      tkGreaterThan: Write(t, '>');
      tkAt: Write(t, '@');
      tkSquaredBraceOpen: Write(t, '[');
      tkSquaredBraceClose: Write(t, ']');
      tkCaret: Write(t, '^');
      tkDotDot: Write(t, '..');
      tkAssign: Write(t, ':=');
      tkNotEqual: Write(t, '<>');
      tkLessEqualThan: Write(t, '<=');
      tkGreaterEqualThan: Write(t, '>=');
      tkPower: Write(t, '**');
      tkSymmetricalDifference: Write(t, '><');
      tkLineEnding: Write(t, LineEnding);
      tkTab: Write(t, #9);
      tkDirective, tkDefine, tkInclude: write(t, '{', TPasToken(FList[i]^).value, '}');
      tkComment: write(t, '{', TPasToken(FList[i]^).value, '}');
    else
      Write(t, TPasToken(FList[i]^).value);
    end;
  end;

  Close(t);
end;

function TPasTokenList.ParseSource(AFileName: string): boolean;
var
  pas: TPascalScanner;
  fr: TFileResolver;
  index: integer;
  token: TToken;
begin
  Result := False;

  fr := TFileResolver.Create;
  pas := TPascalScanner.Create(fr);
  pas.Options := [po_SkipIncludeFiles, po_DontEatDefines, po_MonoLithicASMBlocks];

  try
    pas.OpenFile(AFileName);

    index := 0;
    repeat
      Inc(index);

      token := pas.FetchToken;

      Add(token, pas.CurTokenString, pas.CurRow);

      {$ifdef debug}
      //writeln(token, '>', pas.CurTokenString);
      {$endif}
    until pas.CurToken = tkEOF;

    Result := True;
  finally
    pas.Free;
    fr.Free;
  end;
end;

procedure TPasTokenList.Add(AToken: TToken; AValue: string; ALineNumber: integer);
begin
  Insert(0, AToken, AValue, ALineNumber);
end;

function TPasTokenList.Count: integer;
begin
  Result := FList.Count;
end;

end.