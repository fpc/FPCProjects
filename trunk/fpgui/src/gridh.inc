{
    $Id$

    fpGUI  -  Free Pascal Graphical User Interface
    Copyright (C) 2000 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    Grid widgets declarations

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


// -------------------------------------------------------------------
//   TCustomGrid
// -------------------------------------------------------------------

  EInvalidGridOperation = class(Exception);

  TGridDrawState = set of (gdSelected, gdFocused, gdFixed);


  PIntegerArray = ^TIntegerArray;
  TIntegerArray = array[0..(MAXINT div SizeOf(Integer))-1] of Integer;


  TCustomGrid = class(TWidget)
  private
    FColCount: Integer;
    FRowCount: Integer;
    FFixedCols: Integer;
    FFixedRows: Integer;
    FDefaultColWidth: Integer;
    FDefaultRowHeight: Integer;
    FGridWidth: Integer;
    FGridHeight: Integer;
    FFixedWidth: Integer;
    FFixedHeight: Integer;
    FColWidths, FRowHeights: PIntegerArray;
    procedure SetColCount(AColCount: Integer);
    procedure SetRowCount(ARowCount: Integer);
    procedure SetFixedCols(AFixedCols: Integer);
    procedure SetFixedRows(AFixedRows: Integer);
    procedure SetDefaultColWidth(AWidth: Integer);
    procedure SetDefaultRowHeight(AHeight: Integer);
    function GetColWidths(ACol: Integer): Integer;
    procedure SetColWidths(ACol, AWidth: Integer);
    function GetRowHeights(ARow: Integer): Integer;
    procedure SetRowHeights(ARow, AHeight: Integer);
    procedure HorzScrollBarScroll(Sender: TObject; var APosition: Integer);
    procedure VertScrollBarScroll(Sender: TObject; var APosition: Integer);
  protected
    ScrollingSupport: TScrollingSupport;

    procedure Paint(Canvas: TGfxCanvas); override;
    function ProcessEvent(Event: TEventObj): Boolean; override;
    function DistributeEvent(Event: TEventObj): Boolean; override;
    procedure EvRecalcLayout; override;

    procedure ColWidthsChanged; dynamic;
    procedure DrawCell(ACanvas: TGfxCanvas; ACol, ARow: Integer; ARect: TRect;
     
 AState: TGridDrawState); virtual; abstract;
    procedure RowHeightsChanged; dynamic;
    procedure SizeChanged(OldColCount, OldRowCount: Integer); dynamic;
    property CanExpandWidth default True;
    property CanExpandHeight default True;
    property ColCount: Integer read FColCount write SetColCount default 5;
    property RowCount: Integer read FRowCount write SetRowCount default 5;
    property FixedCols: Integer read FFixedCols write SetFixedCols default 1;
    property FixedRows: Integer read FFixedRows write SetFixedRows default 1;
    property GridWidth: Integer read FGridWidth;
    property GridHeight: Integer read FGridHeight;
    property FixedWidth: Integer read FFixedWidth;
    property FixedHeight: Integer read FFixedHeight;
    property DefaultColWidth: Integer read FDefaultColWidth write SetDefaultColWidth default 64;
    property DefaultRowHeight: Integer read FDefaultRowHeight write SetDefaultRowHeight default 24;
    property ColWidths[ACol: Integer]: Integer read GetColWidths write SetColWidths;
    property RowHeights[ARow: Integer]: Integer read GetRowHeights write SetRowHeights;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;


// -------------------------------------------------------------------
//   TDrawGrid
// -------------------------------------------------------------------

  TDrawCellEvent = procedure(Sender: TObject; ACanvas: TGfxCanvas;
    ACol, ARow: Integer; Rect: TRect; State: TGridDrawState) of object;


  TDrawGrid = class(TCustomGrid)
  private
    FOnDrawCell: TDrawCellEvent;
  protected
    procedure DrawCell(ACanvas: TGfxCanvas; ACol, ARow: Integer;
      ARect: TRect;
 AState: TGridDrawState); override;
  public
    function CellRect(ACol, ARow: Integer): TRect;
    property ColWidths;
    property RowHeights;
  published
    property ColCount;
    property RowCount;
    property FixedCols;
    property FixedRows;
    property DefaultColWidth;
    property DefaultRowHeight;
    property OnDrawCell: TDrawCellEvent read FOnDrawCell write FOnDrawCell;
  end;


// -------------------------------------------------------------------
//   TStringGrid
// -------------------------------------------------------------------

  PCells = ^TCells;
  TCells = array[0..(1 shl 30) div SizeOf(AnsiString)] of AnsiString;

  TStringGrid = class(TDrawGrid)
  private
    CellStrings: PCells;
    function GetCells(ACol, ARow: Integer): String;
    procedure SetCells(ACol, ARow: Integer; const AValue: String);
  protected
    //function GetEditText(ACol, ARow: Integer): String; override;
    //procedure SetEditText(ACol, ARow: Integer; const AValue: String); override;
    procedure SizeChanged(OldColCount, OldRowCount: Integer); override;
    //procedure ColumnMoved(AFrom, ATo: Integer); override;
    //procedure RowMoved(AFrom, ATo: Integer); override;
    procedure DrawCell(ACanvas: TGfxCanvas; ACol, ARow: Integer;
      ARect: TRect;
 AState: TGridDrawState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Cells[ACol, ARow: Integer]: String read GetCells write SetCells;
    property Cols[Index: Integer]: TStrings;
    property Objects[ACol, ARow: Integer]: TObject;
    property Rows[Index: Integer]: TStrings;
  end;


{
  $Log$
  Revision 1.4  2001/01/17 21:36:55  sg
  * Scrolling improvements

  Revision 1.3  2001/01/11 23:30:12  sg
  *** empty log message ***

  Revision 1.2  2000/12/24 13:18:12  sg
  * Some small fixes (improved speed, and better scrollbar handling where
    appropiate)

  Revision 1.1  2000/12/23 23:20:15  sg
  * First public CVS version...

}
