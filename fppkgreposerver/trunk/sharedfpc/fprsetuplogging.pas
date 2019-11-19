unit fprSetupLogging;

{$mode objfpc}{$H+}

interface

uses
  sysutils,
  dcsGlobalSettings,
  TLoggerUnit,
  TAppenderUnit,
  TEventLogAppenderUnit,
  TConsoleAppenderUnit,
  TLevelUnit,
  TRollingFileAppenderUnit;

procedure AddLoggingSettings;
procedure SetupLogging;

implementation

procedure AddLoggingSettings;
var
  GlobalSettings: TDCSGlobalSettings;
begin
  GlobalSettings := TDCSGlobalSettings.GetInstance;
  GlobalSettings.AddSettingTemplate('Log_', 'Level', 'LogLevel_', 'INFO');
  GlobalSettings.AddSettingTemplate('Log_', 'Format', 'LogFormat_', 'Console');
  GlobalSettings.AddSettingTemplate('Log_', 'Filename', 'LogFilename_', '');
  GlobalSettings.AddSettingTemplate('Log_', 'Directory', 'LogDirectory_', '');
  GlobalSettings.AddSettingTemplate('Log_', 'MaxFilesize', 'LogMaxFilesize_', '');
  GlobalSettings.AddSettingTemplate('Log_', 'MaxFileCount', 'LogMaxFileCount_', '');
end;

procedure SetupLogging;
var
  GlobalSettings: TDCSGlobalSettings;
  TemplateList: TDCSSettingTemplate;
  i: Integer;
  LogFormat: String;
  LogFilename: string;
  LogDirectory, LogLevel: string;
  Appender: TAppender;
  LogMaxFilesize: Longint;
  LogMaxFileCount: LongInt;

begin
  TLoggerUnit.Initialize;

  GlobalSettings := TDCSGlobalSettings.GetInstance;
  TemplateList := GlobalSettings.SettingTemplateList.FindSettingTemplate('Log_');
  for i := 0 to TemplateList.Values.Count -1 do
  begin
    LogFormat := GlobalSettings.GetSettingAsString('LogFormat_'+TemplateList.Values[i]);
    if SameText(LogFormat, 'Console') then
      begin
      Appender  := TConsoleAppender.Create();
      TLogger.GetInstance().AddAppender(Appender)
      end
    else if SameText(LogFormat, 'File') then
      begin
      LogFilename := GlobalSettings.GetSettingAsString('LogFilename_'+TemplateList.Values[i]);
      if LogFilename='' then
        LogFilename := ChangeFileExt(ApplicationName, '.log');
      LogDirectory := GlobalSettings.GetSettingAsString('LogDirectory_'+TemplateList.Values[i]);
      if LogDirectory<>'' then
        LogFilename := ConcatPaths([LogDirectory, LogFilename]);
      LogMaxFilesize := StrToIntDef(GlobalSettings.GetSettingAsString('LogMaxFilesize_'+TemplateList.Values[i]), -1);
      LogMaxFileCount := StrToIntDef(GlobalSettings.GetSettingAsString('LogMaxFileCount_'+TemplateList.Values[i]), -1);

      Appender  := TRollingFileAppender.Create(LogFilename);
      if LogMaxFilesize > -1 then
        TRollingFileAppender(Appender).SetMaxFileSize(LogMaxFilesize);
      if LogMaxFileCount > -1 then
        TRollingFileAppender(Appender).SetMaxBackupIndex(LogMaxFileCount-1);

      TLogger.GetInstance().AddAppender(Appender)
      end
    else if SameText(LogFormat, 'Syslog') then
      begin
      Appender  := TEventLogAppender.Create();

      TLogger.GetInstance().AddAppender(Appender)
      end
    else
      raise Exception.CreateFmt('Invalid log-format [%s]. Possible values are: Console, File and Syslog.', [LogFormat]);

    LogLevel := GlobalSettings.GetSettingAsString('LogLevel_'+TemplateList.Values[i]);
    if LogLevel <> '' then
      Appender.SetThreshold(toLevel(LogLevel));
  end;
end;

end.

