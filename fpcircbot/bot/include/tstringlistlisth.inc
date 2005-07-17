// TStringList header file
// Automaticly created by listmaker
  TStringListList = class(TFPObjectList)
   private
    function GetItem(Index: Integer): TStringList; {$ifdef HASINLINE} inline; {$endif}
    Procedure SetItem(Index: Integer; AItem: TStringList); {$ifdef HASINLINE} inline; {$endif}
   public
    constructor Create(Owns: Boolean = True);
    destructor Destroy; override;
    function Add(AItem: TStringList): Integer; {$ifdef HASINLINE} inline; {$endif}
    function Remove(AItem: TStringList): Integer;
    function IndexOf(AItem: TStringList): Integer;
    function First: TStringList;
    function Last: TStringList;
    property Items[Index: Integer]: TStringList read GetItem write SetItem; default;
  end;
