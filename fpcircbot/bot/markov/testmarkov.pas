program testmarkov;

{$mode objfpc}{$H+}
{$DEFINE UseTimer}

uses
  Classes,
{$IFDEF UseTimer}
  Cpu,
{$ENDIF}
  Markov;
  
var
  MyMarkov: TMarkov;
{$IFDEF UseTimer}
  MyTimer: TTimer;
{$ENDIF}

procedure init;
begin
{$IFDEF UseTimer}
  MyTimer:= TZenTimer.Create;
  MyTimer.AutoReset := true;
{$ENDIF}
  writeln('Start loading');
{$IFDEF UseTimer}
  MyTimer.Start;
{$ENDIF}
  MyMarkov := TMarkov.Create('data\markovdata.txt', 15, 65);
{$IFDEF UseTimer}
  writeln('Loading: ', MyTimer.Stop * MyTimer.Resolution:8:6);
{$ENDIF}
end;
  
procedure test;
var
  line: TStringList;
  answer: string;
begin
  line := TStringList.Create;
  line.Delimiter:=' ';
  line.DelimitedText := 'Hallo ik ben Vincent';
  MyMarkov.TalkTo(line);
  line.DelimitedText := 'Hoe gaat het ermee';
{$IFDEF UseTimer}
  MyTimer.Start;
{$ENDIF}
  answer := MyMarkov.Talk(line);
  //answer := MyMarkov.TalkFrom();
  writeln(answer);
{$IFDEF UseTimer}
  writeln('Talking: ', MyTimer.Stop * MyTimer.Resolution:8:6);
{$ENDIF}
  line.Free;
end;

procedure conversation;
var
  line: string;
  words: TStringList;
  answer: string;
begin
  words := TStringList.Create;
  words.Delimiter:=' ';
  readln(line);
  while line<>'' do begin
    words.DelimitedText := line;
{$IFDEF UseTimer}
  MyTimer.Start;
{$ENDIF}
    answer:=MyMarkov.Talk(words);
{$IFDEF UseTimer}
    writeln('Talking: ', MyTimer.Stop * MyTimer.Resolution:8:6);
{$ENDIF}
    writeln(answer);
    readln(line);
  end;
  words.Free;
end;

begin
  randomize;
  init;
  test;
  conversation;
  MyMarkov.Free;
{$IFDEF UseTimer}
  MyTimer.Free;
{$ENDIF}
end.

