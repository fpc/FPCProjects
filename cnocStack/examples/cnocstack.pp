program cnocStack;

{$mode objfpc}{$H+}

uses
  {$IFDEF Unix}
  Cthreads,
  {$ENDIF}
  Classes,
  pthreads,
  Sysutils,
  Custapp,
  Generics.Collections,
  TLoggerUnit,
  TRollingFileAppenderUnit,
  cnocStackInternalStack,
  cnocStackBinaryListener,
  cnocStackInternalSenderHandler;

type

  { TcnocStack }

  TcnocStack = class(TCustomApplication)
  Protected
    procedure Dorun; Override;
  Public
    procedure Writehelp; Virtual;
  end;

{ TcnocStack }

procedure TcnocStack.Dorun;
var
  Errormsg: String;
  Binarylistener: TcnocStackBinaryListener;
  Stack: TcnocStackInternalStack;
  StackList: TcnocStackInternalStackList;
  SenderHandler: TcnocStackInternalSenderHandler;
  Appender: TRollingFileAppender;
begin
  // quick check parameters
  Errormsg := Checkoptions('h', 'help');
  if Errormsg <> '' then begin
    Showexception(Exception.Create(Errormsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if Hasoption('h', 'help') then begin
    Writehelp;
    Terminate;
    Exit;
  end;

  TLoggerUnit.Initialize;
  Appender  := TRollingFileAppender.Create(ChangeFileExt(ApplicationName, '.log'));
  Appender.SetMaxFileSize(16000);
  TLogger.GetInstance().AddAppender(Appender);

  StackList := TcnocStackInternalStackList.Create([]);
  try
    Stack := TcnocStackInternalStack.create();
    StackList.Add('TestStack', Stack);

    SenderHandler := TcnocStackInternalSenderHandler.Create;
    try
      BinaryListener := TcnocStackBinaryListener.Create('localhost', 5425, StackList, SenderHandler);
      try
        repeat
          sleep(10000);
        until Terminated;
      finally
        Binarylistener.Free;
      end;
    finally
      SenderHandler.Free;
    end;
  finally
    StackList.Free;
  end;

  // stop program loop
  Terminate;
end;

procedure TcnocStack.Writehelp;
begin
  Writeln('Usage: ', Exename, ' -h');
end;

var
  Application: TcnocStack;
begin
  Randomize;
  Application := TcnocStack.Create(nil);
  Application.Title := 'CNOC Stack';
  Application.Run;
  Application.Free;
end.

