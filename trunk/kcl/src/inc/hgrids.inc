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
//   Grid Widgets
// ===================================================================

type

// -------------------------------------------------------------------
//   TCustomGrid
// -------------------------------------------------------------------

  EInvalidGridOperation = class(Exception);

  TGridDrawState = set of (gdSelected, gdFocused, gdFixed);


  PIntegerArray = ^TIntegerArray;
  TIntegerArray = array[0..(MAXINT div SizeOf(Integer))-1] of Integer;


  TCustomGrid = class(TCustomControl)
  private
    FColCount, FRowCount, FFixedCols, FFixedRows: Integer;
    FDefaultColWidth, FDefaultRowHeight: Integer;
    FColWidths, FRowHeights: PIntegerArray;
    PaintBox: TPaintBox;
    procedure SetColCount(AColCount: Integer);
    procedure SetRowCount(ARowCount: Integer);
    procedure SetDefaultColWidth(AWidth: Integer);
    procedure SetDefaultRowHeight(AHeight: Integer);
    function GetColWidths(ACol: Integer): Integer;
    procedure SetColWidths(ACol, AWidth: Integer);
    function GetRowHeights(ARow: Integer): Integer;
    procedure SetRowHeights(ARow, AHeight: Integer);

    procedure PaintBoxPaint(Sender: TObject; ACanvas: TCanvas; const ARect: TRect);
  protected
    // Virtual methods
    procedure DrawCell(ACol, ARow: Integer; ARect: TRect;
     
 AState: TGridDrawState); virtual; abstract;
    procedure ColWidthsChanged; dynamic;
    procedure RowHeightsChanged; dynamic;
    procedure SizeChanged(OldColCount, OldRowCount: Longint); dynamic;
    // Properties
    property ColCount: Integer read FColCount write SetColCount default 5;
    property RowCount: Integer read FRowCount write SetRowCount default 5;
    property FixedCols: Integer read FFixedCols write FFixedCols default 1;
    property FixedRows: Integer read FFixedRows write FFixedRows default 1;
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

  TDrawCellEvent = procedure(Sender: TObject; ACol, ARow: Longint; Rect: TRect;
    State: TGridDrawState) of object;


  TDrawGrid = class(TCustomGrid)
  private
    FOnDrawCell: TDrawCellEvent;
  protected
    procedure DrawCell(ACol, ARow: Integer; ARect: TRect;
 AState: TGridDrawState); override;
  public
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
    procedure SizeChanged(OldColCount, OldRowCount: Longint); override;
    //procedure ColumnMoved(AFrom, ATo: Integer); override;
    //procedure RowMoved(AFrom, ATo: Integer); override;
    procedure DrawCell(ACol, ARow: Integer; ARect: TRect;
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
  Revision 1.1  2000/03/19 00:29:31  sg
  * Huge shift towards more VCL compatiblity (NOTE: Many widgets/controls
    are not working correctly at the moment!)

}
