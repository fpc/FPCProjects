{
    $Id$

    fpGUI  -  Free Pascal Graphical User Interface
    Copyright (C) 2000 - 2001 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    Layouts widgets declarations

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


// ===================================================================
//   Layouts
// ===================================================================

type

  ELayoutError = class(Exception);

  THorzAlign = (horzLeft, horzCenter, horzRight, horzFill);
  TVertAlign = (vertTop, vertCenter, vertBottom, vertFill);

  TLayoutItem = class(TCollectionItem)
  private
    FWidget: TWidget;
  published
    property Widget: TWidget read FWidget write FWidget;
  end;

  TWidgetArrayInfo = record
    min, def, max: Integer;
    MinFlag, MaxFlag: Boolean;
  end;

  TWidgetArrayInfoArray = array[0..(1 shl 30) div SizeOf(TWidgetArrayInfo) - 1] of TWidgetArrayInfo;
  PWidgetArrayInfoArray = ^TWidgetArrayInfoArray;



  TLayout = class(TContainerWidget)
  protected
    FWidgets: TCollection;
    FBorderSpacing: Integer;
    IsRecalcingLayout: Boolean;

    function GetChildCount: Integer; override;
    function GetChild(Index: Integer): TWidget; override;
    procedure AddFixedChild(AChild: TWidget);
    property BorderSpacing: Integer read FBorderSpacing write FBorderSpacing;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ContainsChild(AChild: TWidget): Boolean; override;
  end;


// -------------------------------------------------------------------
//   FixedLayout
// -------------------------------------------------------------------

  TFixedItem = class(TLayoutItem)
  public
    Left, Top: Integer;
  end;

  TFixedLayout = class(TLayout)
  protected
    procedure CalcSizes; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AddWidget(AWidget: TWidget; ALeft, ATop: Integer);
    procedure MoveWidget(AWidget: TWidget; ALeft, ATop: Integer);
  published
    property Enabled;
  end;


// -------------------------------------------------------------------
//   DockingLayout
// -------------------------------------------------------------------

  TDockingMode = (dmTop, dmBottom, dmLeft, dmRight, dmClient, dmUndocked);

  TDockingItem = class(TLayoutItem)
  public
    Left, Top: Integer;
    DockingMode: TDockingMode;
  end;

  TDockingLayout = class(TLayout)
  protected
    procedure CalcSizes; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AddWidget(AWidget: TWidget; ADockingMode: TDockingMode);
    procedure AddWidget(AWidget: TWidget; ALeft, ATop: Integer);
  published
    property Enabled;
  end;


// -------------------------------------------------------------------
//   BoxLayout
// -------------------------------------------------------------------

  TCustomBoxLayout = class(TLayout)
  private
    FHorzAlign: THorzAlign;
    FVertAlign: TVertAlign;
    FOrientation: TOrientation;
    FSpacing: Integer;
    procedure SetOrientation(AOrientation: TOrientation);
  protected
    procedure CalcSizes; override;
    procedure Resized; override;
    property HorzAlign: THorzAlign read FHorzAlign write FHorzAlign default horzFill;
    property VertAlign: TVertAlign read FVertAlign write FVertAlign default vertFill;
    property Orientation: TOrientation read FOrientation write SetOrientation default Horizontal;
    property Spacing: Integer read FSpacing write FSpacing default 4;
  public
    constructor Create(AOwner: TComponent); override;
    procedure InsertChild(AChild: TWidget); override;
    procedure RemoveChild(AChild: TWidget); override;
  end;

  TBoxLayout = class(TCustomBoxLayout)
  published
    property CanExpandWidth;
    property CanExpandHeight;
    property Enabled;
    property BorderSpacing;
    property HorzAlign;
    property Orientation;
    property Spacing;
    property VertAlign;
  end;


// -------------------------------------------------------------------
//   GridLayout
// -------------------------------------------------------------------

  TGridItem = class(TLayoutItem)
  private
    FX, FY, FWidth, FHeight: Integer;
  public
    constructor Create(ACollection: TCollection); override;
  published
    property x: Integer read FX write FX default 1;
    property y: Integer read FY write FY default 1;
    property Width: Integer read FWidth write FWidth;
    property Height: Integer read FHeight write FHeight;
  end;

  TCustomGridLayout = class(TLayout)
  private
    FColCount, FRowCount: Integer;
    FColSpacing, FRowSpacing: Integer;
    procedure SetColCount(AColCount: Integer);
    procedure SetRowCount(ARowCount: Integer);
    procedure SetColSpacing(AColSpacing: Integer);
    procedure SetRowSpacing(ARowSpacing: Integer);
  protected
    procedure InitSizeInfos(var ColInfos, RowInfos: PWidgetArrayInfoArray);
    procedure CalcSizes; override;
    procedure Resized; override;

    property GridPositions: TCollection read FWidgets write FWidgets;
    property ColCount: Integer read FColCount write SetColCount default 2;
    property RowCount: Integer read FRowCount write SetRowCount default 2;
    property ColSpacing: Integer read FColSpacing write SetColSpacing default 4;
    property RowSpacing: Integer read FRowSpacing write SetRowSpacing default 4;
  public
    constructor Create(AOwner: TComponent); override;
    procedure InsertChild(AChild: TWidget); override;
    procedure AddWidget(AWidget: TWidget; x, y, w, h: Integer);
    procedure MoveWidget(AWidget: TWidget; x, y, w, h: Integer);
  end;

  TGridLayout = class(TCustomGridLayout)
  published
    property Enabled;
    property ColCount;
    property RowCount;
    property ColSpacing;
    property RowSpacing;
    property GridPositions;
  end;


{
  $Log$
  Revision 1.3  2001/02/14 23:14:44  sg
  * Adaptions to fpGFX changes; added much more usage of TSize and TPoint
    here as well
  * Heavy improvements of the layouting algorithm

  Revision 1.2  2001/01/11 23:30:12  sg
  *** empty log message ***

  Revision 1.1  2000/12/23 23:20:15  sg
  * First public CVS version...

}
