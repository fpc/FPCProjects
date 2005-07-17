// TLCommand header file
// Automaticly created by listmaker
  TLCommandList = class(TFPObjectList)
   private
    function GetItem(Index: Integer): TLCommand; {$ifdef HASINLINE} inline; {$endif}
    Procedure SetItem(Index: Integer; AItem: TLCommand); {$ifdef HASINLINE} inline; {$endif}
   public
    constructor Create(Owns: Boolean = True);
    destructor Destroy; override;
    function Add(AItem: TLCommand): Integer; {$ifdef HASINLINE} inline; {$endif}
    function Remove(AItem: TLCommand): Integer;
    function IndexOf(AItem: TLCommand): Integer;
    function First: TLCommand;
    function Last: TLCommand;
    property Items[Index: Integer]: TLCommand read GetItem write SetItem; default;
  end;
