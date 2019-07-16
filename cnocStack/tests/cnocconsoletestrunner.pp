program FPCUnitProject1;

{$mode objfpc}{$H+}

uses
  {$IFDEF Unix}
  CThreads,
  {$ENDIF}
  Classes,
  consoletestrunner,
  testmessageformat,
  connectiontests;

var
  Application: TTestRunner;

begin
  Application := TTestRunner.Create(nil);
  Application.Initialize;
  Application.Title := 'Cnoc Stack Console test runner';
  Application.Run;
  Application.Free;
end.
