{
    $Id$

    KCL  -  Kassandra Component Library
    Copyright (C) 1999  by the KCL team
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
//	Note: TWidget declaration is in hguibase.inc
// ===================================================================

// -------------------------------------------------------------------
//   Label
// -------------------------------------------------------------------

  TLabel = class(TWidget)
  protected
    procedure OnFinishCreation; override;
  published
    property Text;
  end;


// -------------------------------------------------------------------
//   Edit Control
// -------------------------------------------------------------------

  TEdit = class(TWidget)
  protected
    procedure OnFinishCreation; override;
  published
    property Text;
  end;


// -------------------------------------------------------------------
//   Buttons
// -------------------------------------------------------------------

  TButtonWidget = class(TWidget)
  protected
    FOnClick: TNotifyEvent;
  published
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;

  TButton = class(TButtonWidget)
  protected
    procedure OnFinishCreation; override;
  published
    property Text;
  end;


  TCheckBoxState = (cbUnchecked, cbChecked, cbGrayed);

  TCheckBox = class(TButtonWidget)
  protected
    FState: TCheckBoxState;
    function  GetState: TCheckBoxState;
    procedure SetState(AState: TCheckBoxState);
    procedure OnFinishCreation; override;
  published
    property State: TCheckBoxState
      read GetState write SetState default cbUnchecked;
    property Text;
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
  end;


// -------------------------------------------------------------------
//   Scroll Bars, Scroll Box
// -------------------------------------------------------------------


  TRange = class(TPersistent)
  protected
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

  TScrollBox = class(TWidget)
  protected
    FHorzRange, FVertRange: TRange;
    FContent: TWidget;

    procedure OnFinishCreation; override;
    procedure OnRecalcLayout; override;
    procedure OnSetSize; override;
    procedure OnSizeChanged; override;
    procedure SetContent(AContent: TWidget); virtual;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property  Content: TWidget read FContent write SetContent;
    property  HorzRange: TRange read FHorzRange;
    property  VertRange: TRange read FVertRange;
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
    FOnMouseMove: TMouseEvent;
  public
    function  CreateCanvas: TCanvas;
  published
    property OnPaint: TPaintEvent read FOnPaint write FOnPaint;
    property OnKey: TKeyEvent read FOnKey write FOnKey;
    property OnMouseButtonDown: TMouseEvent read FOnMouseButtonDown
      write FOnMouseButtonDown;
    property OnMouseButtonUp: TMouseEvent read FOnMouseButtonUp
      write FOnMouseButtonUp;
    property OnMouseMove: TMouseEvent read FOnMouseMove write FOnMouseMove;
  end;

  TPaintBox = class(TCustomPaintBox)
  protected
    procedure OnFinishCreation; override;
    procedure OnSetSize; override;
  end;


// -------------------------------------------------------------------
//   Notebook
// -------------------------------------------------------------------

  TNotebookPage = class(TCollectionItem)
  protected
    priv: TNotebookPage_private;
    FText: String;
    FContent: TWidget;
    procedure SetText(const AText: String);
    procedure SetContent(AContent: TWidget);
  published
    property Text: String read FText write SetText;
    property Content: TWidget read FContent write SetContent;
  end;

  TPageChangeEvent = procedure(Sender: TWidget; OldPage, NewPage: Integer) of object;

  TNotebook = class(TWidget)
  protected
    FPages: TCollection;
    FOnPageChange: TPageChangeEvent;

    function  GetPage(AIndex: Integer): TNotebookPage;
    function  GetCurPage: TNotebookPage;
    function  GetCurPageIndex: Integer;
    procedure SetCurPageIndex(AIndex: Integer);
    procedure OnFinishCreation; override;
    procedure OnSizeChanged; override;
  public
    constructor Create(AOwner: TComponent); override;
    function  AddPage(AText: String; AChild: TWidget): Integer;
    procedure RemovePage(AIndex: Integer); virtual;
    property  Page[AIndex: Integer]: TNotebookPage read GetPage;
    property  CurPage: TNotebookPage read GetCurPage;
  published
    property Pages: TCollection read FPages;
    property  CurPageIndex: Integer read GetCurPageIndex write SetCurPageIndex;
    property  OnPageChange: TPageChangeEvent read FOnPageChange
      write FOnPageChange;
  end;


// -------------------------------------------------------------------
//   Status Bar
// -------------------------------------------------------------------

  TStatusBar = class(TWidget)
  protected
    procedure OnFinishCreation; override;
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
    procedure SetPane1(APane1: TWidget);
    procedure SetPane2(APane2: TWidget);
    procedure SetOrientation(AOrientation: TSplitterOrientation);
    procedure SetResizePolicy(APolicy: TSplitterResizePolicy);
    procedure OnFinishCreation; override;
  published
    property Pane1: TWidget read FPane1 write SetPane1;
    property Pane2: TWidget read FPane2 write SetPane2;
    property Orientation: TSplitterOrientation
      read FOrientation write SetOrientation default soHorizontal;
    property ResizePolicy: TSplitterResizePolicy
      read FResizePolicy write SetResizePolicy default srProportional;
  end;


{
  $Log$
  Revision 1.1  1999/12/30 21:33:20  sg
  Initial revision

}
