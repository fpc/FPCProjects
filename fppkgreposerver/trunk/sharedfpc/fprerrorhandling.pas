unit fprErrorHandling;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  HTTPDefs,
  fphttpserver;


type
  EJsonWebException = class(Exception);

procedure pmOnShowRequestException(AResponse: TResponse; AnException: Exception; var handled: boolean);

implementation

procedure pmOnShowRequestException(AResponse: TResponse; AnException: Exception; var handled: boolean);
begin
  if AnException is EHTTP then
    begin
    AResponse.Content := AnException.Message;
    AResponse.Code := EHTTP(AnException).StatusCode;
    AResponse.CodeText := EHTTP(AnException).StatusText;
    if AResponse.CodeText = '' then
      AResponse.CodeText := GetStatusCode(AResponse.Code);
    end
  else
    begin
    AResponse.Content := Format('{error:{msg:%s}}', [QuotedStr(AnException.Message)]);
    AResponse.Code := 200;
    AResponse.CodeText := 'OK';
    end;
  handled := True;
end;


end.

