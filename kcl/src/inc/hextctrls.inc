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
//   Extended Controls:
//     TShape
//     TPaintBox
//     TImage
//     TBevel
//     TTimer
//     T(Custom)Panel
//     TPage
//     TNotebook
//     THeader
//     T(Custom)RadioGroup
//     TSplitter
//     T(Custom)ControlBar
// ===================================================================

type

// -------------------------------------------------------------------
//   Shape
// -------------------------------------------------------------------

// Nothing here yet...


// -------------------------------------------------------------------
//   Paint Box
// -------------------------------------------------------------------

  TPaintBox = class(TGraphicControl)
{  private
    FOnPaint: TNotifyEvent;
  protected
    procedure Paint; override;
  public
    property Canvas;
  published
    // !!!: property Color;
    property Enabled;
    // !!!: property Font;
    // !!!: property ParentColor;
    // !!!: property ParentFont;
    property ParentShowHint;
    // !!!: property PopupMenu;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;}
  end;


// -------------------------------------------------------------------
//   Image
// -------------------------------------------------------------------

// Nothing here yet...


// -------------------------------------------------------------------
//   Bevel
// -------------------------------------------------------------------

// Nothing here yet...


// -------------------------------------------------------------------
//   Timer
// -------------------------------------------------------------------

  TTimer = class(TKCLComponent)
  private
    Handle: TTimerHandle;
    FEnabled: Boolean;
    FInterval: Cardinal;
    FOnTimer: TNotifyEvent;

    procedure Update;
    procedure SetEnabled(AEnabled: Boolean); dynamic;
    procedure SetInterval(AInterval: Cardinal); dynamic;
  protected
    procedure Timer; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Interval: Cardinal read FInterval write SetInterval default 1000;
    property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
  end;


// -------------------------------------------------------------------
//   Panel
// -------------------------------------------------------------------

// Nothing here yet...


// -------------------------------------------------------------------
//   Page
// -------------------------------------------------------------------

{  TPage = class(TCustomControl)
  protected
    priv: TPage_private;
    FContent: TControl;
    FBorderWidth: Integer;
    procedure SetText(const AText: String);
    procedure SetContent(AContent: TControl);
  published
    property Text;
    property Content: TControl read FContent write SetContent;
    property BorderWidth: Integer read FBorderWidth write FBorderWidth default 0;
  end;}


// -------------------------------------------------------------------
//   Notebook
// -------------------------------------------------------------------

  TPageChangeEvent = procedure(Sender: TControl; OldPage, NewPage: Integer) of object;

{  TNotebook = class(TCustomControl)
  protected
    FrameBorders: TRect;
    FPages: TCollection;
    FOnPageChange: TPageChangeEvent;

    function  GetPages(AIndex: Integer): TNotebookPage;
    function  GetCurPage: TNotebookPage;
    function  GetPageIndex: Integer;
    procedure SetPageIndex(AIndex: Integer);
    procedure OnFinishCreation; override;
    procedure DoRecalcLayout; override;
    procedure LayoutChildren; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function  AddPage(AText: String; AChild: TControl): Integer;
    procedure RemovePage(AIndex: Integer); virtual;
    property  Pages[AIndex: Integer]: TNotebookPage read GetPages;
    property  CurPage: TNotebookPage read GetCurPage;
  published
    property CanExpandWidth;
    property CanExpandHeight;
    property PageIndex: Integer read GetPageIndex write SetPageIndex;
    property OnPageChange: TPageChangeEvent read FOnPageChange write FOnPageChange;
  end;}


// -------------------------------------------------------------------
//   Header
// -------------------------------------------------------------------

// Nothing here yet...


// -------------------------------------------------------------------
//   Radio Group
// -------------------------------------------------------------------

// Nothing here yet...


// -------------------------------------------------------------------
//   Splitter
// -------------------------------------------------------------------

  TSplitterOrientation = (soHorizontal, soVertical);
  TSplitterResizePolicy = (srProportional, srFixedPane1, srFixedPane2);

  TSplitter = class(TWinControl)
  protected
    FPane1, FPane2: TControl;
    FOrientation: TSplitterOrientation;
    FResizePolicy: TSplitterResizePolicy;
    FSplitterPos, NextPos: Integer;
    procedure SetPane1(APane1: TControl);
    procedure SetPane2(APane2: TControl);
    procedure SetOrientation(AOrientation: TSplitterOrientation);
    procedure SetResizePolicy(APolicy: TSplitterResizePolicy);
    procedure SetSplitterPos(ASplitterPos: Integer);
    procedure OnFinishCreation; override;
    procedure DoRecalcLayout; override;
    procedure LayoutChildren; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Pane1: TControl read FPane1 write SetPane1;
    property Pane2: TControl read FPane2 write SetPane2;
    property Orientation: TSplitterOrientation
      read FOrientation write SetOrientation default soHorizontal;
    property ResizePolicy: TSplitterResizePolicy
      read FResizePolicy write SetResizePolicy default srProportional;
    property SplitterPos: Integer read FSplitterPos write SetSplitterPos default -1;
  end;


// -------------------------------------------------------------------
//   Control Bar
// -------------------------------------------------------------------

// Nothing here yet...


{
  $Log$
  Revision 1.1  2000/03/19 00:29:31  sg
  * Huge shift towards more VCL compatiblity (NOTE: Many widgets/controls
    are not working correctly at the moment!)

}
