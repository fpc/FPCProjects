unit dcsThreadCommandFactory;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  dcsHandler;

type

  { TDCSThreadCommandFactory }

  TDCSThreadCommandFactory = class
  private
    class var FList: TFPList;
  public
    class constructor Create();
    class destructor Destroy;
    class function GetCommandClassByName(ATextName: string): TDCSThreadCommandClass;
    class procedure RegisterCommandClass(ACommandClass: TDCSThreadCommandClass);
  end;


implementation

{ TDCSThreadCommandFactory }

class constructor TDCSThreadCommandFactory.Create;
begin
  FList := TFPList.Create;
end;

class destructor TDCSThreadCommandFactory.Destroy;
begin
  FList.Destroy;
end;

class function TDCSThreadCommandFactory.GetCommandClassByName(ATextName: string): TDCSThreadCommandClass;
var
  i: Integer;
begin
  result := nil;
  for i := 0 to FList.Count -1 do
    begin
    if TDCSThreadCommandClass(FList[i]).TextName=ATextName then
      result := TDCSThreadCommandClass(FList[i]);
    end;
end;

class procedure TDCSThreadCommandFactory.RegisterCommandClass(ACommandClass: TDCSThreadCommandClass);
begin
  FList.Add(ACommandClass);
end;

end.

