unit fprErrorHandling;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  HTTPDefs,
  fphttpserver,
  TLoggerUnit;


type
  EJsonWebException = class(EHTTP);

procedure fprOnShowRequestException(AResponse: TResponse; AnException: Exception; var handled: boolean);

implementation

procedure fprOnShowRequestException(AResponse: TResponse; AnException: Exception; var handled: boolean);
var
  ExcMessage: string;
  BackTrace: string;
  Frames: PPointer;
  i: Integer;
begin
  if AnException is EHTTP then
    begin
    if AnException is EJsonWebException then
      begin
      ExcMessage := StringReplace(AnException.Message, LineEnding, '\n', [rfReplaceAll]);
      AResponse.ContentType := 'application/json';
      AResponse.Content := Format('{"error":{"msg":"%s"}}', [StringReplace(ExcMessage, '"', '\"', [rfReplaceAll])]);
      end
    else
      AResponse.Content := AnException.Message;
    AResponse.Code := EHTTP(AnException).StatusCode;
    if AResponse.Code = 0 then
      AResponse.Code := 500;
    AResponse.CodeText := EHTTP(AnException).StatusText;
    if AResponse.CodeText = '' then
      AResponse.CodeText := GetStatusCode(AResponse.Code);
    end
  else
    begin
    ExcMessage := StringReplace(AnException.Message, LineEnding, '\n', [rfReplaceAll]);
    AResponse.Content := Format('{"error":{"msg":"%s"}}', [StringReplace(ExcMessage, '"', '\"', [rfReplaceAll])]);
    AResponse.ContentType := 'application/json';
    AResponse.Code := 500;
    AResponse.CodeText := GetStatusCode(AResponse.Code);
    end;

  BackTrace := '';
  if AnException<>nil then
    BackTrace := 'Exception class: ' + Exception.ClassName + LineEnding + 'Message: ' + AnException.Message + LineEnding;
  BackTrace := BackTrace + BacktraceStrFunc(ExceptAddr);
  Frames := ExceptFrames;
  for i := 0 to ExceptFrameCount -1 do
    begin
    BackTrace := BackTrace + LineEnding + BacktraceStrFunc(Frames[i]);
    end;
  TLogger.GetInstance.Error(BackTrace);

  TLogger.GetInstance.Trace('Respond with [' + IntToStr(AResponse.Code) + ':' + AResponse.CodeText + ']. Content: [' + Trim(Copy(AResponse.Content,1,100)) + ']');
  handled := True;
end;


end.

