unit fprGCollection;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  contnrs;

type
  { TcnocGCollection }

  generic TcnocGCollection<T: TCollectionItem> = class(TCollection)
  protected
    function GetItem(Index: Integer): T;
    procedure SetItem(Index: Integer; Value: T);
  public
    constructor Create();
    function Add: T;
    property Items[Index: Integer]: T read GetItem write SetItem;
  end;

implementation

{ TcnocGCollection }

function TcnocGCollection.GetItem(Index: Integer): T;
begin
  Result := T(inherited GetItem(Index));
end;

procedure TcnocGCollection.SetItem(Index: Integer; Value: T);
begin
  inherited SetItem(Index, Value)
end;

constructor TcnocGCollection.Create;
begin
  inherited Create(T);
end;

function TcnocGCollection.Add: T;
begin
  Result := T(Inherited Add);
end;

end.

