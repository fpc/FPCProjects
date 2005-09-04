program convertmarkov;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils;
  
var
  Dict: TStringList;
  
procedure CheckFile(const FileName: string);
begin
  if not FileExists(FileName) then begin
    writeln(FileName, 'doesn''t exist');
    halt;
  end;
end;

procedure AddTransition(w1, w2, hitcount: ptrint);
var
  Transitions: TStringList;
begin
  if Dict.Objects[w1]=nil then
    Dict.Objects[w1] := TStringList.Create;
  Transitions:=Dict.Objects[w1] as TStringList;
  Transitions.AddObject(Dict[w2], TObject(hitcount));
end;
  
procedure ReadInputFiles(const dictfile, markovfile: string);
var
  MarkovTable: TStringList;
  i: integer;
  w1, w2: integer;
  hitcount: integer;
begin
  Dict := TStringList.Create;
  Dict.LoadFromFile(dictfile);
  MarkovTable := TStringList.Create;
  MarkovTable.LoadFromFile(markovfile);
  i := 0;
  writeln('Interpreting markov table');
  while (i<MarkovTable.Count) do begin
    w1 := StrToInt(MarkovTable[i]);
    w2 := StrToInt(MarkovTable[i+1]);
    hitcount := StrToInt(MarkovTable[i+2]);
    inc(i,3);
    AddTransition(w1,w2, hitcount);
  end;
  MarkovTable.Free;
  writeln('Sorting dictionary');
  Dict.Sort;
end;

procedure WriteOutput(const OutputFilename: string);
var
  w1, w2: integer;
  MarkovText: TextFile;
  Transitions: TStringList;
begin
  writeln('Writing new markov data');
  assign(MarkovText, OutputFileName);
  rewrite(MarkovText);
  for w1 := 0 to Dict.Count-1 do begin
    writeln(MarkovText, Dict[w1]);
    Transitions:=Dict.Objects[w1] as TStringList;
    if assigned(Transitions) then begin
      for w2 := 0 to Transitions.Count -1 do begin
        writeln(MarkovText, Transitions[w2]);
        writeln(MarkovText, ptrint(Transitions.Objects[w2]));
      end;
    end;
    writeln(MarkovText);
  end;
end;

begin
  if paramcount<>3 then begin
    writeln('Usage: convertmarkov <dict.txt> <markovtable.txt> <newmarkov.txt>');
    halt;
  end;
  CheckFile(ParamStr(1));
  CheckFile(ParamStr(2));
  ReadInputFiles(ParamStr(1), ParamStr(2));
  WriteOutput(ParamStr(3));
  Dict.Free;
end.

