unit pmErrorHandling;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  HTTPDefs;


type
  EJsonWebException = class(Exception);

procedure pmOnShowRequestException(AResponse: TResponse; AnException: Exception; var handled: boolean);

implementation

procedure pmOnShowRequestException(AResponse: TResponse; AnException: Exception; var handled: boolean);
begin
  if AnException is EJsonWebException then
    begin
    AResponse.Content := Format('{error:{msg:%s}}', [QuotedStr(AnException.Message)]);
    AResponse.Code := 200;
    AResponse.CodeText := 'OK';
    handled := True;;
    end;
end;


end.

