{
    $Id$

    fpGUI  -  Free Pascal Graphical User Interface
    Copyright (C) 2000 - 2001 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    Basic events and widget declarations

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


// -------------------------------------------------------------------
//   Basic event objects
// -------------------------------------------------------------------

  {$M+}
  TEventObj = class
  private
    RefCount: LongInt;
    FSender: TObject;
  public
    constructor Create(ASender: TObject);
    procedure AddRef;
    procedure Release;
    function SendToChild(AChild: TWidget): Boolean; virtual;
  published
    property Sender: TObject read FSender write FSender;
  end;
  {$M-}

  TEventClass = class of TEventObj;

  // Miscellaneous events

  TLayoutingEventObj = class(TEventObj);

  TCalcSizesEventObj = class(TLayoutingEventObj);

  TResizedEventObj = class(TLayoutingEventObj)
  private
    FIsForced: Boolean;
  public
    constructor Create(ASender: TObject; AIsForced: Boolean);
  published
    property IsForced: Boolean read FIsForced write FIsForced;
  end;


  TDestroyEventObj = class(TEventObj);

  TEnabledChangeEventObj = class(TEventObj);
  TVisibilityChangeEventObj = class(TEventObj);

  TPaintingEventObj = class(TEventObj)
  private
    FCanvas: TGfxCanvas;
  public
    constructor Create(ASender: TObject; ACanvas: TGfxCanvas);
  published
    property Canvas: TGfxCanvas read FCanvas write FCanvas;
  end;

  TPreparePaintEventObj = class(TPaintingEventObj)
  public
    // To prevent invisible children to be processed:
    function SendToChild(AChild: TWidget): Boolean; override;
  end;

  TPaintEventObj = class(TPaintingEventObj)
  public
    // To adapt the clipping region and add a translation:
    function SendToChild(AChild: TWidget): Boolean; override;
  end;


  // Mouse events

  TMouseEventObj = class(TEventObj)
  private
    FShift: TShiftState;
    FPosition: TPoint;
  public
    constructor Create(ASender: TObject; AShift: TShiftState;
      APosition: TPoint);
    function SendToChild(AChild: TWidget): Boolean; override;
    property Position: TPoint read FPosition write FPosition;
  published
    property Shift: TShiftState read FShift write FShift;
  end;

  TMouseButtonEventObj = class(TMouseEventObj)
  private
    FButton: TMouseButton;
  public
    constructor Create(ASender: TObject; AButton: TMouseButton;
      AShift: TShiftState; APosition: TPoint);
  published
    property Button: TMouseButton read FButton write FButton;
  end;

  TMousePressedEventObj = class(TMouseButtonEventObj);
  TMouseReleasedEventObj = class(TMouseButtonEventObj);

  TMouseMoveEventObj = class(TMouseEventObj)
  public
    function SendToChild(AChild: TWidget): Boolean; override;
  end;


  TMouseEnterEventObj = class(TMouseEventObj)
  public
    NewCursor: TGfxCursor;
  end;

  TMouseLeaveEventObj = class(TEventObj)
  public
    function SendToChild(AChild: TWidget): Boolean; override;
  end;

  TMouseLeaveCheckEventObj = class(TMouseEventObj)
  public
    function SendToChild(AChild: TWidget): Boolean; override;
    NewCursor: TGfxCursor;
  end;


  TMouseWheelEventObj = class(TMouseEventObj)
  private
    FWheelDelta: Single;
  public
    constructor Create(ASender: TObject; AShift: TShiftState;
      AWheelDelta: Single; APosition: TPoint);
  published
    property WheelDelta: Single read FWheelDelta write FWheelDelta;
  end;


// -------------------------------------------------------------------
//   TWidget
// -------------------------------------------------------------------

  TWidgetStyle = set of (wsCaptureMouse, wsClickable, wsOpaque);

  TContainerWidget = class;

  TWidget = class(TComponent)
  private
    FParent: TWidget;
    FOnClick: TNotifyEvent;
    // Property access
    procedure SetParent(AParent: TWidget);
    function GetBoundsRect: TRect;
    function GetLeft: Integer;
    function GetTop: Integer;
    function GetWidth: Integer;
    function GetHeight: Integer;
    procedure SetEnabled(AEnabled: Boolean);
    procedure SetVisible(AVisible: Boolean);
    // Event handling
    function EvCalcSizes(Event: TCalcSizesEventObj): Boolean;
    function EvEnabledChange(Event: TEnabledChangeEventObj): Boolean;
    function EvVisibilityChange(Event: TVisibilityChangeEventObj): Boolean;
    function EvMousePressed(Event: TMousePressedEventObj): Boolean;
    function EvMouseReleased(Event: TMouseReleasedEventObj): Boolean;
    function EvMouseEnter(Event: TMouseEnterEventObj): Boolean;
    function EvMouseLeave(Event: TMouseLeaveEventObj): Boolean;
    function EvMouseLeaveCheck(Event: TMouseLeaveCheckEventObj): Boolean;
  protected
    FCursor: TGfxCursor;
    FText: String;
    FStyle: TStyle;
    FCanExpandHeight: Boolean;
    FCanExpandWidth: Boolean;
    FEnabled: Boolean;
    FVisible: Boolean;

    FOrigin: TPoint;
    FBoundsSize: TSize;
    FClientRect: TRect;
    FMinSize, FMaxSize, FDefSize: TSize;
    WidgetStyle: TWidgetStyle;
    WidgetState: TWidgetState;

    procedure Loaded; override;
    procedure Click; dynamic;
    procedure Paint(Canvas: TGfxCanvas); virtual;
    procedure SetParentComponent(AParent: TComponent); override;
    // Layouting
    procedure CalcSizes; virtual; abstract;
    procedure Resized; virtual;
    // Events:
    function ProcessEvent(Event: TEventObj): Boolean; virtual;
    function DistributeEvent(Event: TEventObj): Boolean; virtual;
    procedure EvFocusChanged; dynamic;		// Widget got or lost focus
    procedure EvKeyPressed(Key: Word; Shift: TShiftState); dynamic;
    procedure EvKeyReleased(Key: Word; Shift: TShiftState); dynamic;
    procedure EvKeyChar(KeyChar: Char); dynamic;
    procedure EvTextChanged; dynamic;

    function DoMouseEnter(AShift: TShiftState; AMousePos: TPoint): Boolean;

    // Properties
    function GetStyle: TStyle;
    procedure SetCanExpandWidth(allow: Boolean);
    procedure SetCanExpandHeight(allow: Boolean);
    procedure SetText(const AText: String);
    property CanExpandWidth: Boolean read FCanExpandWidth write SetCanExpandWidth default False;
    property CanExpandHeight: Boolean read FCanExpandHeight write SetCanExpandHeight default False;
    property Cursor: TGfxCursor read FCursor write FCursor default crDefault;
    property Text: String read FText write SetText;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function SendEvent(Event: TEventObj): Boolean;
    function FindForm: TCustomForm;
    procedure SetEmbeddedParent(AParent: TWidget);
    procedure SetBounds(APosition: TPoint; ASize: TSize);
    function WidgetToClient(const APoint: TPoint): TPoint; virtual;
    function ClientToWidget(const APoint: TPoint): TPoint; virtual;
    function ClientToScreen(const APoint: TPoint): TPoint; virtual;
    procedure Show; dynamic;
    procedure Hide; dynamic;
    procedure Redraw;
    procedure Redraw(const ARect: TRect);
    procedure Scroll(const ARect: TRect; DeltaX, DeltaY: Integer);
    procedure SetFocus;
    procedure Update;
    property Parent: TWidget read FParent write SetParent;
    property Origin: TPoint read FOrigin;
    property BoundsSize: TSize read FBoundsSize;
    property BoundsRect: TRect read GetBoundsRect;
    property Left: Integer read GetLeft;
    property Top: Integer read GetTop;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    property MinSize: TSize read FMinSize;
    property MaxSize: TSize read FMaxSize;
    property DefSize: TSize read FDefSize;
    property ClientRect: TRect read FClientRect;
    property Style: TStyle read GetStyle;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;


{
  $Log$
  Revision 1.4  2001/02/14 23:14:45  sg
  * Adaptions to fpGFX changes; added much more usage of TSize and TPoint
    here as well
  * Heavy improvements of the layouting algorithm

  Revision 1.3  2001/02/09 20:50:20  sg
  * The usual bunch of improvements and bug fixes...

  Revision 1.2  2001/01/11 23:30:12  sg
  *** empty log message ***

  Revision 1.1  2000/12/23 23:20:15  sg
  * First public CVS version...

}
