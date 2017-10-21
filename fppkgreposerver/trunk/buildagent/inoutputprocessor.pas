unit InOutputProcessor;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  typinfo,
  dcsInOutputProcessor,
  RepoTestCommand,
  pkgglobals,
  RepoController;

type

  { TJSONInOutputProcessor }

  TJSONInOutputProcessor = class(TDCSJSonInOutputProcessor)
  protected
    function StringToEnum(TypeInfo: PTypeInfo; Value: string): integer; override;
  end;


implementation

{ TJSONInOutputProcessor }

function TJSONInOutputProcessor.StringToEnum(TypeInfo: PTypeInfo; Value: string): integer;
var
  LogLevel: TLogLevel;
  PackageSource: TrepoPackageSource;
begin
  Result := -1;
  if TypeInfo^.Name='TLogLevel' then
    begin
    for LogLevel := Low(TLogLevel) to High(TLogLevel) do
      begin
      if SameText(Value, SLogLevel[LogLevel]) then
        begin
        Result := integer(LogLevel);
        Exit;
        end;
      end;
    end
  else if TypeInfo^.Name='TrepoPackageSource' then
    begin
    for PackageSource := Low(TrepoPackageSource) to High(TrepoPackageSource) do
      begin
      if SameText(Value, SrepoPackageSource[PackageSource]) then
        begin
        Result := integer(PackageSource);
        Exit;
        end;
      end;
    end
  else
    Result := inherited StringToEnum(TypeInfo, Value);
end;

end.

