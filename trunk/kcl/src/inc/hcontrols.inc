{
    $Id$

    KCL  -  Kassandra Component Library
    Copyright (C) 1999 - 2000  by the KCL team
      see file AUTHORS in base directory of this distribution

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


// ===================================================================
//   Base classes for controls
// ===================================================================

// Helper function:
function ClipMinMax(val, min, max: Integer): Integer;


type

// -------------------------------------------------------------------
//   Control Canvas
// -------------------------------------------------------------------

  TControlCanvas = class(TCanvas)
  private
    priv: TControlCanvas_private;
    FHandle: TControlCanvasHandle;
    FControl: TControl;
  protected
    procedure SetColor(AColor: TColor); override;
    procedure RequiredState(ReqState: TCanvasState); override;
  public
    constructor Create(AHandle: TControlCanvasHandle);
    constructor Create(AControl: TControl);
    destructor Destroy; override;

    procedure MoveTo(x, y: Integer); override;
    procedure LineTo(x, y: Integer); override;
    procedure FillRect(const rect: TRect); override;
    procedure Text(x, y: Integer; const AText: String); override;
    procedure CopyRect(dx, dy, dw, dh, sx, sy: Integer); override;
    procedure DrawBitmap(ADestX, ADestY: Integer; ABitmap: TBitmap;
      ASourceX, ASourceY, ASourceW, ASourceH: Integer); override;

    function TextWidth(const AText: String): Integer; override;
    function TextHeight(const AText: String): Integer; override;

    // Special extensions: Drawing of basic controls
    procedure DrawButton(AControl: TWinControl; const ARect: TRect; IsPressed, IsGrayed: Boolean;
      AAlign: TAlignment; const AText: String); override;

    property Handle: TControlCanvasHandle read FHandle;
    property Control: TControl read FControl;
  end;


// -------------------------------------------------------------------
//   TControl
// -------------------------------------------------------------------

  // Events for TControl

  TChangeEventObj = class(TEventObj);

  TEnabledChangedEventObj = class(TChangeEventObj);
  THintChangedEventObj = class(TChangeEventObj);
  TVisibleChangedEventObj = class(TChangeEventObj);
  TTextChangedEventObj = class(TChangeEventObj);


  TControlCreationState = (wcsNotCreated, wcsCreationInProgress, wcsCreationDone);

  TControl = class(TKCLComponent)
  private
    FCreationState: TControlCreationState;

    FCanExpandHeight: Boolean;
    FCanExpandWidth: Boolean;
    FEnabled: Boolean;
    FHeight: Integer;
    FHint: String;
    FLeft: Integer;
    FParent: TWinControl;
    FParentShowHint: Boolean;
    FShowHint: Boolean;
    FText: String;
    FTop: Integer;
    FVisible: Boolean;
    FWidth: Integer;
    FOnClick: TNotifyEvent;
    FOnDblClick: TNotifyEvent;
    FOnEnter: TNotifyEvent;
    FOnExit: TNotifyEvent;
    FOnKeyDown: TKeyEvent;
    FOnKeyPress: TKeyPressEvent;
    FOnKeyUp: TKeyEvent;
    FOnMouseDown: TMouseEvent;
    FOnMouseMove: TMouseMoveEvent;
    FOnMouseUp: TMouseEvent;
    procedure SetCanExpandWidth(allow: Boolean);
    procedure SetCanExpandHeight(allow: Boolean);
    procedure SetEnabled(AEnabled: Boolean);
    procedure SetHint(const AHint: String);
    procedure SetText(const AText: String);
    procedure SetVisible(AVisible: Boolean);
  protected
    BlockDrawingCounter: LongInt;
    DefW, DefH, MinW, MinH, MaxW, MaxH: Integer;

    FOnFocusIn: TNotifyEvent;
    FOnFocusOut: TNotifyEvent;

    procedure Click; dynamic;
    function ProcessEvent(Event: TEventObj): Boolean; virtual;
    procedure SetParent(AParent: TWinControl); virtual;

    procedure DoRecalcLayout; virtual;

    property CanExpandWidth: Boolean read FCanExpandWidth write SetCanExpandWidth default True;
    property CanExpandHeight: Boolean read FCanExpandHeight write SetCanExpandHeight default True;
    // !!!: property Color: TColor read FColor write SetColor default clDefault;
    // !!!: property Font: TFont read FFont;
    property ParentShowHint: Boolean read FParentShowHint write FParentShowHint default True;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
    property OnEnter: TNotifyEvent read FOnEnter write FOnEnter;
    property OnExit: TNotifyEvent read FOnExit write FOnExit;
    property OnKeyDown: TKeyEvent read FOnKeyDown write FOnKeyDown;
    property OnKeyPress: TKeyPressEvent read FOnKeyPress write FOnKeyPress;
    property OnKeyUp: TKeyEvent read FOnKeyUp write FOnKeyUp;
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Hide;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); virtual;
    procedure Show;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Height: Integer read FHeight;
    property Hint: String read FHint write SetHint;
    property Left: Integer read FLeft;
    property ShowHint: Boolean read FShowHint write FShowHint;
    property Top: Integer read FTop;
    property Visible: Boolean read FVisible write SetVisible default True;
    property Width: Integer read FWidth;


    // !!!: Clean this up:

    procedure RecalcLayout;		// called at initialization or by a child

    procedure BlockDrawing;
    procedure UnblockDrawing;
    {procedure Redraw(AtOnce: Boolean);
    procedure Redraw(const rect: TRect; AtOnce: Boolean);
    procedure Redraw(ax, ay, aw, ah: Integer; AtOnce: Boolean);}
     
    function  GetText: String; virtual; 
    function  IsRadioGroup: Boolean; dynamic;

    property Parent: TWinControl read FParent write SetParent;
    property CreationState: TControlCreationState read FCreationState default wcsNotCreated;
    property Text: String read GetText write SetText;

    // Events
    property OnFocusIn: TNotifyEvent read FOnFocusIn write FOnFocusIn;
    property OnFocusOut: TNotifyEvent read FOnFocusOut write FOnFocusOut;
  end;


// -------------------------------------------------------------------
//   TWinControl
// -------------------------------------------------------------------

  TWinControl = class(TControl)
  private
    FHandle: TWinControlHandle;
  protected
    function  GetControlCount: Integer; virtual;
    function  GetControls(AIndex: Integer): TControl; virtual; abstract;

    // Property change handlers
    function ProcessEvent(Event: TEventObj): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // KCL specific:

    procedure OnFinishCreation; virtual;
    procedure LayoutChildren; virtual;


    // VCL compatible:

    function HandleAllocated: Boolean;
    procedure HandleNeeded;
    procedure InsertControl(AControl: TControl);
    procedure RemoveControl(AControl: TControl);
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    property ControlCount: Integer read GetControlCount;
    property Controls[AIndex: Integer]: TControl read GetControls;
    property Handle: TWinControlHandle read FHandle;
  end;


// -------------------------------------------------------------------
//   TGraphicControl
// -------------------------------------------------------------------

  TGraphicControl = class(TControl)
  private
    FCanvas: TCanvas;
  protected
    procedure Paint; virtual; {!!!:} abstract;
    property Canvas: TCanvas read FCanvas;
  end;


// -------------------------------------------------------------------
//   TCustomControl
// -------------------------------------------------------------------

  TCustomControl = class(TWinControl)
  private
    FCanvas: TCanvas;
  protected
    procedure Paint; virtual; {!!!:} abstract;
    // !!!: procedure PaintWindow(dc: HDC); override;
    property Canvas: TCanvas;
  end;


{
  $Log$
  Revision 1.1  2000/03/19 00:29:31  sg
  * Huge shift towards more VCL compatiblity (NOTE: Many widgets/controls
    are not working correctly at the moment!)

}
