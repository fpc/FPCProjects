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
//   Layouts
// ===================================================================

type

  THorzAlign = (horzLeft, horzCenter, horzRight);
  TVertAlign = (vertTop, vertCenter, vertBottom);

  TLayout = class(TWidget)
  protected
    FWidgets: TCollection;
    IsRecalcingLayout: Boolean;
    procedure PutChild(AChild: TWidget; ax, ay: LongInt);	// Initial
    procedure MoveChild(AChild: TWidget; ax, ay: LongInt);	// After init
    procedure OnFinishCreation; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Widgets: TCollection read FWidgets;
  end;


// -------------------------------------------------------------------
//   FixedLayout
// -------------------------------------------------------------------

  TFixedItem = class(TCollectionItem)
  protected
    FLeft, FTop: LongInt;
    FWidget: TWidget;
  published
    property Left: LongInt read FLeft write FLeft;
    property Top: LongInt read FTop write FTop;
    property Widget: TWidget read FWidget write FWidget;
  end;

  TFixedLayout = class(TLayout)
  protected
    procedure OnFinishCreation; override;
  published
    constructor Create(AOwner: TComponent); override;
    procedure AddWidget(AWidget: TWidget; ALeft, ATop: LongInt);
    procedure MoveWidget(AWidget: TWidget; ALeft, ATop: LongInt);
  end;


// -------------------------------------------------------------------
//   DockingLayout
// -------------------------------------------------------------------

  TDockingMode = (dmTop, dmBottom, dmLeft, dmRight, dmClient, dmUndocked);

  TDockingItem = class(TCollectionItem)
  protected
    Left, Top: LongInt;
    FDockingMode: TDockingMode;
    FWidget: TWidget;
  published
    property DockingMode: TDockingMode read FDockingMode write FDockingMode;
    property Widget: TWidget read FWidget write FWidget;
  end;

  TDockingLayout = class(TLayout)
  protected
    procedure OnFinishCreation; override;
    procedure DoRecalcLayout; override;
    procedure ApplySize; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AddWidget(AWidget: TWidget; ADockingMode: TDockingMode);
    procedure AddWidget(AWidget: TWidget; ALeft, ATop: LongInt);
  end;


// -------------------------------------------------------------------
//   GridLayout
// -------------------------------------------------------------------

  TGridItem = class(TCollectionItem)
  protected
    Fx, Fy, FWidth, FHeight: LongInt;
    FWidget: TWidget;
  public
    property x: LongInt read Fx write Fx;
    property y: LongInt read Fy write Fy;
    property Width: LongInt read FWidth write FWidth;
    property Height: LongInt read FHeight write FHeight;
    property Widget: TWidget read FWidget write FWidget;
  end;

  TGridLayout = class(TLayout)
  protected
    FRows, FColumns: LongInt;
    FSameSizeCells: Boolean;
    procedure FSetSameSizeCells(ASameSize: Boolean);

    procedure OnFinishCreation; override;
//    procedure OnRecalcLayout; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AddWidget(AWidget: TWidget; x, y, w, h: LongInt);
  published
    property  Rows: LongInt read FRows write FRows default 2;
    property  Columns: LongInt read FColumns write FColumns default 2;
    property  SameSizeCells: Boolean read FSameSizeCells
      write FSetSameSizeCells default True;
  end;


// -------------------------------------------------------------------
//   BoxLayout
// -------------------------------------------------------------------

  TBoxItem = class(TCollectionItem)
  protected
    FWidget: TWidget;
  published
    property Widget: TWidget read FWidget write FWidget;
  end;

  TBoxOrientation = (boxHorz, boxVert);

  TBoxLayout = class(TLayout)
  protected
    FHorzAlign: THorzAlign;
    FVertAlign: TVertAlign;
    FOrientation: TBoxOrientation;
    procedure OnFinishCreation; override;
//    procedure OnRecalcLayout; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AddWidget(AWidget: TWidget);
  published
    property HorzAlign: THorzAlign read FHorzAlign write FHorzAlign;
    property VertAlign: TVertAlign read FVertAlign write FVertAlign;
    property Orientation: TBoxOrientation read FOrientation write FOrientation;
  end;


{
  $Log$
  Revision 1.3  2000/02/10 18:35:55  sg
  * Adapted to new layouting methods

  Revision 1.2  2000/01/24 00:08:49  sg
  * Extended copyright to year 2000...

  Revision 1.1.1.1  1999/12/30 21:33:18  sg
  Initial import

}
