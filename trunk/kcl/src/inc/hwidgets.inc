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
//   Widgets
//
//      Note: TWidget declaration is in hguibase.inc
// ===================================================================

type

// -------------------------------------------------------------------
//   Label
// -------------------------------------------------------------------

  TLabel = class(TWidget)
  protected
    procedure OnFinishCreation; override;
    procedure DoRecalcLayout; override;
    procedure SetText(const AText: String); override;
  published
    property Text;
  end;


// -------------------------------------------------------------------
//   Edit Control
// -------------------------------------------------------------------

  TEdit = class(TWidget)
  protected
    FMultiLine: Boolean;
    FDefaultWidth: Integer;
    procedure OnFinishCreation; override;
    procedure DoRecalcLayout; override;
    procedure SetText(const AText: String); override;
  published
    property Text;
    property MultiLine: Boolean read FMultiLine write FMultiLine default False;
    property DefaultWidth: Integer read FDefaultWidth write FDefaultWidth;
  end;


// -------------------------------------------------------------------
//   Separator
// -------------------------------------------------------------------

  TSeparator = class(TWidget)
  protected
    procedure OnFinishCreation; override;
  end;


// -------------------------------------------------------------------
//   Buttons
// -------------------------------------------------------------------

  TGenericButton = class(TWidget)
  protected
    FOnClick: TNotifyEvent;
  public
    procedure DoClick;
  published
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;

  TButton = class(TGenericButton)
  private
    FDefault: Boolean;
  protected
    procedure OnFinishCreation; override;
    procedure DoRecalcLayout; override;
    procedure SetText(const AText: String); override;
  published
    property Text;
    property Default: Boolean read FDefault write FDefault default False;
  end;

  TImageButton = class(TGenericButton)
  protected
    FImageList: TImageList;
    FImageIndex: Integer;
    procedure OnFinishCreation; override;
  published
    property ImageList: TImageList read FImageList write FImageList;
    property ImageIndex: Integer read FImageIndex write FImageIndex;
  end;


  TCheckBoxState = (cbUnchecked, cbChecked, cbGrayed);

  TCheckBox = class(TGenericButton)
  protected
    FState: TCheckBoxState;
    function  GetState: TCheckBoxState;
    procedure SetState(AState: TCheckBoxState);
    procedure OnFinishCreation; override;
    procedure DoRecalcLayout; override;
    procedure SetText(const AText: String); override;
  published
    property Text;
    property State: TCheckBoxState read GetState write SetState default cbUnchecked;
  end;

  TRadioButton = class(TGenericButton)
  protected
    FChecked: Boolean;
    procedure SetChecked(AChecked: Boolean);
    procedure OnFinishCreation; override;
    procedure DoRecalcLayout; override;
    procedure SetText(const AText: String); override;
  published
    property Text;
    property Checked: Boolean read FChecked write SetChecked;
  end;


// -------------------------------------------------------------------
//   TGroupBox
// -------------------------------------------------------------------

  TCustomGroupBox = class(TWidget)
  protected
    FContent: TWidget;
    FFixedWidth, FFixedHeight: Boolean;

    constructor Create(AOwner: TComponent); override;
    function  GetWidgetCount: Integer; override;
    function  GetWidgets(AIndex: Integer): TWidget; override;
    procedure DoRecalcLayout; override;
    procedure ApplySize; override;
    procedure OnFinishCreation; override;
    procedure SetText(const AText: String); override;

    property  Content: TWidget read FContent write FContent;
    property  FixedWidth: Boolean read FFixedWidth write FFixedWidth default True;
    property  FixedHeight: Boolean read FFixedHeight write FFixedHeight default True;
  public
    function  IsRadioGroup: Boolean; override;
  end;

  TGroupBox = class(TCustomGroupBox)
  published
    property Text;
    property Content;
    property FixedWidth;
    property FixedHeight;
  end;


// -------------------------------------------------------------------
//   TListBox
// -------------------------------------------------------------------

  TCustomListBox = class(TWidget)
  private
    FOnDblClick: TNotifyEvent;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
  protected
    priv: TCustomListBox_private;
    FItems: TStrings;
    function GetItemIndex: Integer;
    procedure OnFinishCreation; override;
    procedure DoRecalcLayout; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    property Items: TStrings read FItems write FItems;
    property ItemIndex: Integer read GetItemIndex;
  end;

  TListBox = class(TCustomListBox)
  published
    property Items;
    property ItemIndex;
    property OnDblClick;
  end;


// -------------------------------------------------------------------
//   TComboBox
// -------------------------------------------------------------------

  TComboBoxStyle = (csDropDown, csDropDownList);

  TCustomComboBox = class(TWidget)
  private
    function GetItemIndex: Integer;
    procedure SetItemIndex(AIndex: Integer);
  protected
    FStyle: TComboBoxStyle;
    FItems: TStrings;
    procedure OnFinishCreation; override;
    procedure DoRecalcLayout; override;
    procedure SetText(const AText: String); override;
    property Style: TComboBoxStyle read FStyle write FStyle default csDropDown;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    property Items: TStrings read FItems write FItems;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
  end;

  TComboBox = class(TCustomComboBox)
  published
    property Style;
    property Items;
  end;


// -------------------------------------------------------------------
//   TColList: Columned list
// -------------------------------------------------------------------

  TColList = class(TWidget)
  protected
    FColCount: Integer;
    FLineCount: Integer;
    procedure OnFinishCreation; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property ColCount: Integer read FColCount write FColCount default 1;
    property LineCount: Integer read FLineCount write FLineCount;
  end;


// -------------------------------------------------------------------
//   TTreeView
// -------------------------------------------------------------------

  TTreeView = class(TWidget)
  protected
    procedure OnFinishCreation; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;


// -------------------------------------------------------------------
//   Scroll Bars, Scroll Box
// -------------------------------------------------------------------

  TRange = class(TPersistent)
  protected
    FOnValueChange: TNotifyEvent;
    function  GetMaxValue: Integer; virtual; abstract;
    procedure SetMaxValue(AMaxValue: Integer); virtual; abstract;
    function  GetCurValue: Integer; virtual; abstract;
    procedure SetCurValue(ACurValue: Integer); virtual; abstract;
    function  GetPageSize: Integer; virtual; abstract;
    procedure SetPageSize(APageSize: Integer); virtual; abstract;
  published
  //property MinValue: Integer;
    property MaxValue: Integer read GetMaxValue write SetMaxValue default 200;
    property CurValue: Integer read GetCurValue write SetCurValue;
    property PageSize: Integer read GetPageSize write SetPageSize default 100;
    property OnValueChange: TNotifyEvent read FOnValueChange write FOnValueChange;
  end;

  THorzScrollBar = class(TWidget)
  protected
    FRange: TRange;
    procedure OnFinishCreation; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Range: TRange read FRange;
  end;

  TVertScrollBar = class(TWidget)
  protected
    FRange: TRange;
    procedure OnFinishCreation; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Range: TRange read FRange;
  end;

  TAbstractScrollingWidget = class(TWidget)
  protected
    FContent: TWidget;
    FHorzRange, FVertRange: TRange;
    procedure SetContent(AContent: TWidget); virtual; abstract;
    property Content: TWidget read FContent write SetContent;
    property HorzRange: TRange read FHorzRange;
    property VertRange: TRange read FVertRange;
  end;

  TScrollBox = class(TAbstractScrollingWidget)
  protected
    priv: TScrollBox_private;
    procedure OnFinishCreation; override;
    procedure DoRecalcLayout; override;
    procedure SetContent(AContent: TWidget); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Content;
    property HorzRange;
    property VertRange;
  end;

  TScrollers = class(TAbstractScrollingWidget)
  protected
    priv: TScrollers_private;
    procedure OnFinishCreation; override;
    procedure DoRecalcLayout; override;
    procedure ApplySize; override;
    procedure SetContent(AContent: TWidget); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Content;
    property HorzRange;
    property VertRange;
  end;


// -------------------------------------------------------------------
//   Paint Box
// -------------------------------------------------------------------

  TCustomPaintBox = class(TWidget)
  protected
    FOnPaint: TPaintEvent;
    FOnKey: TKeyEvent;
    FOnMouseButtonDown: TMouseEvent;
    FOnMouseButtonUp: TMouseEvent;
    FOnMouseMove: TMouseMoveEvent;
    procedure OnFinishCreation; override;
  public
    constructor Create(AOwner: TComponent); override;
    function  CreateCanvas: TCanvas;
    procedure ScrollBy(dx, dy: Integer; DoClear: Boolean);
    procedure DoPaint(ACanvas: TCanvas; const ARect: TRect);
    function  DoKey(AKey: Char; AKeyCode: LongWord; AShift: TShiftState): Boolean;
    procedure DoMouseButtonDown(AButton: TMouseButton; AShift: TShiftState;
      AMouseX, AMouseY: Integer);
    procedure DoMouseButtonUp(AButton: TMouseButton; AShift: TShiftState;
      AMouseX, AMouseY: Integer);
    procedure DoMouseMove(AShift: TShiftState; AMouseX, AMouseY: Integer);

    property OnPaint: TPaintEvent read FOnPaint write FOnPaint;
    property OnKey: TKeyEvent read FOnKey write FOnKey;
    property OnMouseButtonDown: TMouseEvent read FOnMouseButtonDown
      write FOnMouseButtonDown;
    property OnMouseButtonUp: TMouseEvent read FOnMouseButtonUp
      write FOnMouseButtonUp;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
  end;

  TPaintBox = class(TCustomPaintBox)
  published
    property OnPaint;
    property OnKey;
    property OnMouseButtonDown;
    property OnMouseButtonUp;
    property OnMouseMove;
  end;


// -------------------------------------------------------------------
//   Notebook
// -------------------------------------------------------------------

  TNotebook = class;

  TNotebookPage = class(TCollectionItem)
  protected
    priv: TNotebookPage_private;
    FNotebook: TNotebook;
    FText: String;
    FContent: TWidget;
    FBorderWidth: Integer;
    procedure SetText(const AText: String);
    procedure SetContent(AContent: TWidget);
  public
    property Notebook: TNotebook read FNotebook;
  published
    property Text: String read FText write SetText;
    property Content: TWidget read FContent write SetContent;
    property BorderWidth: Integer read FBorderWidth write FBorderWidth default 0;
  end;

  TPageChangeEvent = procedure(Sender: TWidget; OldPage, NewPage: Integer) of object;

  TNotebook = class(TWidget)
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
    procedure ApplySize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function  AddPage(AText: String; AChild: TWidget): Integer;
    procedure RemovePage(AIndex: Integer); virtual;
    property  Pages[AIndex: Integer]: TNotebookPage read GetPages;
    property  CurPage: TNotebookPage read GetCurPage;
  published
    property PageIndex: Integer read GetPageIndex write SetPageIndex;
    property OnPageChange: TPageChangeEvent read FOnPageChange write FOnPageChange;
  end;


// -------------------------------------------------------------------
//   Status Bar
// -------------------------------------------------------------------

  TStatusBar = class(TWidget)
  protected
    procedure OnFinishCreation; override;
    procedure DoRecalcLayout; override;
    procedure SetText(const AText: String); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Text;
  end;


// -------------------------------------------------------------------
//   Splitter
// -------------------------------------------------------------------

  TSplitterOrientation = (soHorizontal, soVertical);
  TSplitterResizePolicy = (srProportional, srFixedPane1, srFixedPane2);

  TSplitter = class(TWidget)
  protected
    FPane1, FPane2: TWidget;
    FOrientation: TSplitterOrientation;
    FResizePolicy: TSplitterResizePolicy;
    FSplitterPos, NextPos: Integer;
    procedure SetPane1(APane1: TWidget);
    procedure SetPane2(APane2: TWidget);
    procedure SetOrientation(AOrientation: TSplitterOrientation);
    procedure SetResizePolicy(APolicy: TSplitterResizePolicy);
    procedure SetSplitterPos(ASplitterPos: Integer);
    procedure OnFinishCreation; override;
    procedure DoRecalcLayout; override;
    procedure ApplySize; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Pane1: TWidget read FPane1 write SetPane1;
    property Pane2: TWidget read FPane2 write SetPane2;
    property Orientation: TSplitterOrientation
      read FOrientation write SetOrientation default soHorizontal;
    property ResizePolicy: TSplitterResizePolicy
      read FResizePolicy write SetResizePolicy default srProportional;
    property SplitterPos: Integer read FSplitterPos write SetSplitterPos default -1;
  end;


// -------------------------------------------------------------------
//   ToolBar
// -------------------------------------------------------------------

  TToolBar = class;

  TToolButtonStyle = (tbsButton, tbsCheck, tbsDropDown, tbsSeparator,
    tbsDivider);

  TToolButton = class(TKCLComponent)
  protected
    FToolBar: TToolBar;
    FStyle: TToolButtonStyle;
    FCommand: TCommand;
    priv: TToolButton_private;

    procedure SetParentComponent(AParent: TComponent); override;

    procedure SetToolBar(AToolBar: TToolBar);
    procedure SetCommand(ACommand: TCommand);
  public
    function HasParent: Boolean; override;
    function GetParentComponent: TComponent; override;
  published
    property Style: TToolButtonStyle read FStyle write FStyle default tbsButton;
    property Command: TCommand read FCommand write SetCommand;
  end;


  TToolBar = class(TWidget)
  protected
    Items: TList;		// All buttons and widgets in the toolbar
    priv: TToolBar_private;

    procedure GetChildren(AProc: TGetChildProc; ARoot: TComponent); override;
    procedure Notification(AComponent: TComponent;
      AOperation: TOperation); override;
    procedure OnFinishCreation; override;
    procedure DoRecalcLayout; override;

    function  CommandChangeFilter(AEvent: TEventObj): Boolean;
    procedure InternalAddItem(AItem: TKCLComponent);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function  AddChildWidget(AWidget: TWidget): Boolean; override;
    procedure RemoveChildWidget(AWidget: TWidget); override;
    function AddButton(AOwner: TComponent): TToolButton;
    function CalcItemsWidth: Integer;
  end;


{
  $Log$
  Revision 1.9  2000/02/19 19:09:00  sg
  * Fixed layouting bugs; minor improvements

  Revision 1.8  2000/02/18 22:18:57  sg
  * The "SetText" methods of all relevant classes are now overwritten

  Revision 1.7  2000/02/18 18:21:07  sg
  * Introduced TAbstractScrollWidget
  * Added TScrollers

  Revision 1.6  2000/02/17 22:25:42  sg
  * Cleaned up TListBox
  * Added TComboBox, TRadioButton, TSeparator
  * Added several new properties to existing widgets

  Revision 1.5  2000/02/10 18:38:32  sg
  * Some widgets need their own DoRecalcLayout

  Revision 1.4  2000/01/24 00:08:04  sg
  * Added property "MultiLine" to TEdit
  * Added TImageButton
  * Added TToolBar and TToolButton

  Revision 1.3  2000/01/10 09:54:11  peter
    * listbox added

  Revision 1.2  2000/01/05 19:15:58  sg
  * Moved all published properties of TCustomPaintBox to TPaintBox

  Revision 1.1.1.1  1999/12/30 21:33:20  sg
  Initial import

}
