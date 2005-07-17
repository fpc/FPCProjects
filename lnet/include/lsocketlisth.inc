  TLSocketList = class(TObjectList)
   private
    function GetItem(Index: Integer): TLSocket;
    Procedure SetItem(Index: Integer; ASocket: TLSocket);
   public
    constructor Create;
    destructor Destroy; override;
    function Add(ASocket: TLSocket): Integer;
    function Remove(ASocket: TLSocket): Integer;
    function IndexOf(ASocket: TLSocket): Integer;
    function First: TLSocket;
    function Last: TLSocket;
    property Items[Index: Integer]: TLSocket read GetItem write SetItem; default;
  end;

