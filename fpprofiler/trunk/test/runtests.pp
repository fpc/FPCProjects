program runtests;

uses
  Process,
  SysUtils,
  Crt;

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
  begin
    try
      AProcess := TProcess.Create(nil);

      //compile the test
      AProcess.CommandLine := 'fpp ' + Test.Name + ' ' + FPPROFDIR;
      AProcess.Options := AProcess.Options + [poWaitOnExit];
      AProcess.Execute;

      Test.Compile := AProcess.ExitStatus = 0;

      //run the test
    finally
      AProcess.Free;
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
end.

