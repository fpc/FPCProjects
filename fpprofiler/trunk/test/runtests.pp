{
    This file is part of the Free Pascal Profiler.
    Copyright (c) 2010 by Darius Blaszyk

    Test runner application

    See the file COPYING.GPL, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

program runtests;

uses
  Process,
  SysUtils,
  Crt,
  FileUtil;

const
{$IFDEF MSWINDOWS}
  FPPROFDIR = '-Fu..\fpprof';
{$ELSE}
  FPPROFDIR = '-Fu../fpprof';
{$ENDIF}

type
  TTest = record
    Name: string;
    Compile: boolean;
    Execute: boolean;
  end;

var
  Info: TSearchRec;
  Count: longint;
  i: integer;
  TestList: array of TTest;

  procedure RunTest(var Test: TTest);
  var
    AProcess: TProcess;
    ExeFileName: string;
  begin
    ExeFileName := ChangeFileExt(Test.Name, GetExeExt);
    if not FileExists(ExeFileName) or DeleteFile(ExeFileName) then
    begin
      try
        AProcess := TProcess.Create(nil);

        //compile the test
        AProcess.CommandLine := 'fpp --backup ' + Test.Name + ' ' + FPPROFDIR;
        AProcess.Options := AProcess.Options + [poWaitOnExit];
        AProcess.Execute;

        Test.Compile := (AProcess.ExitStatus = 0) and FileExists(ExeFileName);

        //run the test
        if FileExists(ExeFileName) then
        begin
          AProcess.CommandLine := ExeFileName;
          AProcess.Options := AProcess.Options + [poWaitOnExit];
          AProcess.Execute;

          Test.Execute := AProcess.ExitStatus = 0;
        end;
      finally
        if Assigned(AProcess) then
          AProcess.Free;
      end;
    end;
  end;

begin
  Count := 0;
  if FindFirst('test_*.pp', faAnyFile and faDirectory, Info) = 0 then
  begin
    repeat
      //add a test to the list
      Inc(Count);
      SetLength(TestList, Count);
      with TestList[Count - 1] do
      begin
        Name := Info.Name;
        Compile := False;
        Execute := False;
      end;

      //execute test
      RunTest(TestList[Count - 1]);
    until FindNext(info) <> 0;
  end;
  FindClose(Info);

  //print test report
  ClrScr;
  Writeln;
  Writeln('Executed ', Count, ' tests');
  Writeln;
  Writeln;
  Writeln('Name                 Compile Execute');
  Writeln('-------------------- ------- -------');
  for i := 0 to Count - 1 do
  begin
    Write(TestList[i].Name: 20);

    if TestList[i].Compile then
      TextColor(Green)
    else
      TextColor(Red);
    Write(TestList[i].Compile: 7);

    if TestList[i].Execute then
      TextColor(Green)
    else
      TextColor(Red);
    Write(TestList[i].Execute: 7);

    Writeln;
    TextColor(LightGray);
  end;
  Writeln;
  Writeln('done.');
  Readln;
end.

