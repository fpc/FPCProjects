{
    $Id$

    fpGUI  -  Free Pascal Graphical User Interface
    Copyright (C) 2000 by
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

  TUpdateEventObj = class(TEventObj);

  TResizeEventObj = class(TEventObj)
  public
    // To block the propagation of this event to children:
    function SendToChild(AChild: TWidget): Boolean; override;
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
    FMouseX, FMouseY: Integer;
  public
    constructor Create(ASender: TObject; AShift: TShiftState;
      AMouseX, AMouseY: Integer);
    function SendToChild(AChild: TWidget): Boolean; override;
  published
    property Shift: TShiftState read FShift write FShift;
    property MouseX: Integer read FMouseX write FMouseX;
    property MouseY: Integer read FMouseY write FMouseY;
  end;

  TMouseButtonEventObj = class(TMouseEventObj)
  private
    FButton: TMouseButton;
  public
    constructor Create(ASender: TObject; AButton: TMouseButton;
      AShift: TShiftState; AMouseX, AMouseY: Integer);
  published
    property Button: TMouseButton read FButton write FButton;
  end;

  TMousePressedEventObj = class(TMouseButtonEventObj);
  TMouseReleasedEventObj = class(TMouseButtonEventObj);

  TMouseMoveEventObj = class(TMouseEventObj)
  public
    function SendToChild(AChild: TWidget): Boolean; override;
  end;


  TMouseEnterEventObj = class(TMouseEventObj);

  TMouseLeaveEventObj = class(TEventObj)
  public
    function SendToChild(AChild: TWidget): Boolean; override;
  end;


  TMouseWheelEventObj = class(TMouseEventObj)
  private
    FWheelDelta: Single;
  public
    constructor Create(ASender: TObject; AShift: TShiftState;
      AWheelDelta: Single; AMouseX, AMouseY: Integer);
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
    FLeft: Integer;
    FTop: Integer;
    FWidth: Integer;
    FHeight: Integer;
    FClientRect: TRect;
    FOnClick: TNotifyEvent;
    // Property access
    procedure SetParent(AParent: TWidget);
    procedure SetEnabled(AEnabled: Boolean);
    // Event handling
    function EvUpdate(Event: TUpdateEventObj): Boolean;
    function EvEnabledChange(Event: TEnabledChangeEventObj): Boolean;
    function EvVisibilityChange(Event: TVisibilityChangeEventObj): Boolean;
    function EvMousePressed(Event: TMousePressedEventObj): Boolean;
    function EvMouseReleased(Event: TMouseReleasedEventObj): Boolean;
    function EvMouseEnter(Event: TMouseEnterEventObj): Boolean;
    function EvMouseLeave(Event: TMouseLeaveEventObj): Boolean;
  protected
    FText: String;
    FStyle: TStyle;
    FCanExpandHeight: Boolean;
    FCanExpandWidth: Boolean;
    FEnabled: Boolean;
    DefW, DefH, MinW, MinH, MaxW, MaxH: Integer;
    WidgetStyle: TWidgetStyle;
    WidgetState: TWidgetState;

    procedure Loaded; override;
    procedure Click; dynamic;
    procedure Paint(Canvas: TGfxCanvas); virtual;
    procedure SetParentComponent(AParent: TComponent); override;
    // Events:
    function ProcessEvent(Event: TEventObj): Boolean; virtual;
    function DistributeEvent(Event: TEventObj): Boolean; virtual;
    procedure EvFocusChanged; dynamic;		// Widget got or lost focus
    procedure EvKeyPressed(Key: Word; Shift: TShiftState); dynamic;
    procedure EvKeyReleased(Key: Word; Shift: TShiftState); dynamic;
    procedure EvKeyChar(KeyChar: Char); dynamic;
    procedure EvRecalcLayout; dynamic;		// Recalc sizes
    procedure EvTextChanged; dynamic;
    // Properties
    function GetStyle: TStyle;
    procedure SetCanExpandWidth(allow: Boolean);
    procedure SetCanExpandHeight(allow: Boolean);
    procedure SetText(const AText: String);
    property CanExpandWidth: Boolean read FCanExpandWidth write SetCanExpandWidth default False;
    property CanExpandHeight: Boolean read FCanExpandHeight write SetCanExpandHeight default False;
    property Text: String read FText write SetText;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function SendEvent(Event: TEventObj): Boolean;
    function FindForm: TCustomForm;
    procedure SetEmbeddedParent(AParent: TWidget);
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
    function ClientToScreen(const APoint: TPoint): TPoint; virtual;
    procedure Update;
    procedure RecalcLayout;
    procedure Show; dynamic;
    procedure Hide; dynamic;
    procedure Redraw;
    procedure Redraw(const ARect: TRect);
    procedure Scroll(const ARect: TRect; DeltaX, DeltaY: Integer);
    procedure SetFocus;
    property Parent: TWidget read FParent write SetParent;
    property Left: Integer read FLeft;
    property Top: Integer read FTop;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property ClientRect: TRect read FClientRect;
    property Style: TStyle read GetStyle;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
  end;


{
  $Log$
  Revision 1.2  2001/01/11 23:30:12  sg
  *** empty log message ***

  Revision 1.1  2000/12/23 23:20:15  sg
  * First public CVS version...

}
