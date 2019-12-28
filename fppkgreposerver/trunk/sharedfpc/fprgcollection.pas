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

  { TcnocGOwnedCollection }

  generic TcnocGOwnedCollection<T: TCollectionItem> = class(TOwnedCollection)
  protected
    function GetItem(Index: Integer): T;
    procedure SetItem(Index: Integer; Value: T);
  public
    constructor Create(AOwner: TPersistent);
    function Add: T;
    property Items[Index: Integer]: T read GetItem write SetItem;
  end;


  { TcnocSingleton }

  generic TcnocSingleton<T: TObject> = class
  private
    class var FInstance: T;
    class function GetInstance: T; static;
  public
    class destructor Destroy;
    class property Instance: T read GetInstance;
  end;


implementation

{ TcnocGOwnedCollection }

constructor TcnocGOwnedCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, T);
end;

function TcnocGOwnedCollection.Add: T;
begin
  Result := T(Inherited Add);
end;

function TcnocGOwnedCollection.GetItem(Index: Integer): T;
begin
  Result := T(inherited GetItem(Index));
end;

procedure TcnocGOwnedCollection.SetItem(Index: Integer; Value: T);
begin
  inherited SetItem(Index, Value)
end;

{ TcnocGSingletonCollection }

class destructor TcnocSingleton.Destroy;
begin
  FInstance.Free;
end;

class function TcnocSingleton.GetInstance: T;
begin
  if not Assigned(FInstance) then
    FInstance := T.Create();

  Result := FInstance;
end;

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

