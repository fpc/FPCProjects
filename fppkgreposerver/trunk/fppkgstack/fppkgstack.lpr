program fppkgstack;

{$mode objfpc}{$H+}

uses
  Cthreads,
  Classes,
  pthreads,
  Sysutils,
  Custapp,
  Generics.Collections,
  cnocStackInternalStack,
  cnocStackBinaryListener,
  TLoggerUnit,
  TLevelUnit,
  dcsGlobalSettings,
  TRollingFileAppenderUnit,
  fprSetupLogging,
  cnocStackInternalSenderHandler;

type

  { TfppkgStack }

  TfppkgStack = class(TCustomApplication)
  Protected
    procedure Dorun; Override;
  Public
    constructor Create(Theowner: TComponent); Override;
    destructor Destroy; Override;
    procedure Writehelp; Virtual;
  end;

{ TfppkgStack }

procedure TfppkgStack.Dorun;
var
  Errormsg: String;
  Binarylistener: TcnocStackBinaryListener;
  Stack: TcnocStackInternalStack;
  StackList: TcnocStackInternalStackList;
  SenderHandler: TcnocStackInternalSenderHandler;
  Appender: TRollingFileAppender;

  ConfigFileStream: TFileStream;
  ConfigFilename: string;
  GlobalSettings: TDCSGlobalSettings;
  SettingTemplate: TDCSSettingTemplate;
  i: Integer;
begin
  // quick check parameters
  Errormsg := Checkoptions('he', 'help');
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

  AddLoggingSettings;

  GlobalSettings := TDCSGlobalSettings.GetInstance;
  GlobalSettings.AddSetting('TCPPort', 'TCP', 'Port', '', #0, dcsPHasParameter);

  GlobalSettings.AddSettingTemplate('Stack_', 'Name', 'StackName', 'NoName');

  ConfigFilename := ChangeFileExt(ParamStr(0), '.ini');
  if FileExists(ConfigFilename) then
  begin
    ConfigFileStream := TFileStream.Create(ConfigFilename, fmOpenRead);
    try
      GlobalSettings.LoadSettingsFromIniStream(ConfigFileStream);
    finally
      ConfigFileStream.Free;
    end;
  end;

  if HasOption('e') then
  begin
    GlobalSettings.LoadSettingsFromEnvironment();
  end;
  GlobalSettings.EvaluateProgramParameters(Self);

  SetupLogging;

  StackList := TcnocStackInternalStackList.Create([]);
  try
    SettingTemplate := GlobalSettings.SettingTemplateList.FindSettingTemplate('Stack_');
    for i := 0 to SettingTemplate.Values.Count -1 do
      begin
      Stack := TcnocStackInternalStack.create();
      StackList.Add(GlobalSettings.GetSettingAsString('StackName'+SettingTemplate.Values[i]), Stack);
      end;

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

constructor TfppkgStack.Create(Theowner: TComponent);
begin
  inherited Create(Theowner);
end;

destructor TfppkgStack.Destroy;
begin
  inherited Destroy;
end;

procedure TfppkgStack.Writehelp;
begin
  Writeln('Usage: ', Exename, ' -h -e');
end;

var
  Application: TfppkgStack;
begin
  Randomize;
  Application := TfppkgStack.Create(nil);
  Application.Title := 'Fp Stack';
  Application.Run;
  Application.Free;
end.

