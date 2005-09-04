
program fillmarkov;

{$mode objfpc}{$H+}
// define UseTimer, to use tom's cpu unit to print some timing results
//{$DEFINE UseTimer}

uses
  Classes, SysUtils,
{$IFDEF UseTimer}
  Cpu,
{$ENDIF}
  Markov;

var
  MyMarkov: TMarkov;
{$IFDEF UseTimer}
  MyTimer: TTimer;
{$ENDIF}

procedure CheckFile(const FileName: string);
begin
  if not FileExists(FileName) then begin
    writeln(FileName, ' doesn''t exist');
    halt;
  end;
end;

procedure init;
begin
{$IFDEF UseTimer}
  MyTimer:= TZenTimer.Create;
  MyTimer.AutoReset := true;
{$ENDIF}
  writeln('Initializing Markov');
  MyMarkov := TMarkov.Create(ParamStr(2), 15, 65);
end;

procedure done;
begin
  MyMarkov.Free;
{$IFDEF UseTimer}
  MyTimer.Free;
{$ENDIF}
end;

procedure ReadDump;
var
  DumpText: TextFile;
  line: string;
  lineNr: integer;
  LinesAdded: integer;
  FSList: TStringList;
  
  function SepString(s: string): TStringList;
  var
    i: Longint;
  begin
    Result:=nil;
    FSList.Clear;
    if Length(s) > 0 then begin
      s:=StringReplace(s, ':', ' ', [rfReplaceAll]);
      s:=StringReplace(s, ';', ' ', [rfReplaceAll]);
      s:=StringReplace(s, ')', ' ', [rfReplaceAll]);
      s:=StringReplace(s, '(', ' ', [rfReplaceAll]);
      s:=StringReplace(s, '.', ' ', [rfReplaceAll]);
      s:=StringReplace(s, ',', ' ', [rfReplaceAll]);
      s:=StringReplace(s, '"', ' ', [rfReplaceAll]);
      s:=StringReplace(s, '-', ' ', [rfReplaceAll]);
      s:=StringReplace(s, '_', ' ', [rfReplaceAll]);
      s:=StringReplace(s, '=', ' ', [rfReplaceAll]);
      s:=StringReplace(s, '+', ' ', [rfReplaceAll]);
      s:=StringReplace(s, '!', ' ', [rfReplaceAll]);
      s:=StringReplace(s, '?', ' ', [rfReplaceAll]);
      s:=StringReplace(s, '/', ' ', [rfReplaceAll]);
      s:=StringReplace(s, '\', ' ', [rfReplaceAll]);
      s:=StringReplace(s, '>', ' ', [rfReplaceAll]);
      s:=StringReplace(s, '<', ' ', [rfReplaceAll]);
      s:=StringReplace(s, '[', ' ', [rfReplaceAll]);
      s:=StringReplace(s, ']', ' ', [rfReplaceAll]);
      s:=StringReplace(s, '{', ' ', [rfReplaceAll]);
      s:=StringReplace(s, '}', ' ', [rfReplaceAll]);

      FSList.CommaText:=StringReplace(s, ' ', ',', [rfReplaceAll]);
      if FSList.Count > 0 then
        for i:=FSList.Count-1 downto 0 do
          if FSList[i] = '' then
            FSList.Delete(i)
          else
            FSList[i]:=Trim(LowerCase(FSList[i]));
      if FSList.Count > 0 then
        Result:=FSList;
    end;
  end;

begin
  assignfile(DumpText, ParamStr(1));
  reset(DumpText);
  lineNr := 0;
  FSList := TStringList.Create;
{$IFDEF UseTimer}
  MyTimer.Start;
{$ENDIF}
  while not EOF(DumpText) do begin
    inc(lineNr);
    readln(DumpText, line);
    line := LowerCase(line);
    if (pos('joins #', line)=0) and
      (pos('.freenode.net',line)=0) and
      (pos('fpcbot',line)<>1) and // likely fpcbot commands
      (pos('quits',line)=0) then begin
      MyMarkov.TalkTo(SepString(line));
      inc(LinesAdded);
    end;
    if (lineNr mod 10000)=0 then
      writeln(lineNr, ' lines read');
  end;
  writeln('Lines read: ', lineNr, ' Lines added: ', LinesAdded);
{$IFDEF UseTimer}
  writeln('Reading: ', MyTimer.Stop * MyTimer.Resolution:8:6);
{$ENDIF}
  writeln('Words: ', MyMarkov.DictionaryWords,
    ' Markov entries: ', MyMarkov.EntriesMarkov);
end;

begin
  if paramcount<>2 then begin
    writeln('Usage: fillmarkov <dump.txt> <newmarkov.txt>');
    halt;
  end;
  CheckFile(ParamStr(1));
  init;
  ReadDump;
  done;
end.

