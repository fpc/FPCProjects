// TLConnection header file
// Automaticly created by listmaker
  TLConnectionList = class(TObjectList)
   private
    function GetItem(Index: Integer): TLConnection;
    Procedure SetItem(Index: Integer; AItem: TLConnection);
   public
    constructor Create(const Owns: boolean = true);
    destructor Destroy; override;
    function Add(AItem: TLConnection): Integer;
    function Remove(AItem: TLConnection): Integer;
    function IndexOf(AItem: TLConnection): Integer;
    function First: TLConnection;
    function Last: TLConnection;
    property Items[Index: Integer]: TLConnection read GetItem write SetItem; default;
  end;
