unit fprErrorHandling;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  HTTPDefs,
  fphttpserver;


type
  EJsonWebException = class(EHTTP);

procedure fprOnShowRequestException(AResponse: TResponse; AnException: Exception; var handled: boolean);

implementation

procedure fprOnShowRequestException(AResponse: TResponse; AnException: Exception; var handled: boolean);
var
  ExcMessage: string;
begin
  if AnException is EHTTP then
    begin
    if AnException is EJsonWebException then
      begin
      ExcMessage := StringReplace(AnException.Message, LineEnding, '\n', [rfReplaceAll]);
      AResponse.Content := Format('{"error":{"msg":%s}}', [AnsiQuotedStr(ExcMessage, '"')]);
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
    AResponse.Content := Format('{"error":{"msg":%s}}', [AnsiQuotedStr(ExcMessage, '"')]);
    AResponse.ContentType := 'application/json';
    AResponse.Code := 200;
    AResponse.CodeText := 'OK';
    end;
  handled := True;
end;


end.

