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

  THorzAlign = (horzLeft, horzCenter, horzRight, horzFixed, horzFill);
  TVertAlign = (vertTop, vertCenter, vertBottom, vertFixed, vertFill);

  TLayoutItem = class(TCollectionItem)
  public
    Widget: TWidget;
  end;

  TLayout = class(TWidget)
  protected
    FWidgets: TCollection;
    IsRecalcingLayout: Boolean;

    function  GetWidgetCount: Integer; override;
    function  GetWidgets(AIndex: Integer): TWidget; override;
    procedure PutChild(AChild: TWidget; ax, ay: LongInt);	// Initial
    procedure MoveChild(AChild: TWidget; ax, ay: LongInt);	// After init
    procedure OnFinishCreation; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;


// -------------------------------------------------------------------
//   FixedLayout
// -------------------------------------------------------------------

  TFixedItem = class(TLayoutItem)
  public
    Left, Top: LongInt;
  end;

  TFixedLayout = class(TLayout)
  protected
    procedure OnFinishCreation; override;
    procedure DoRecalcLayout; override;
  published
    constructor Create(AOwner: TComponent); override;
    procedure AddWidget(AWidget: TWidget; ALeft, ATop: LongInt);
    procedure MoveWidget(AWidget: TWidget; ALeft, ATop: LongInt);
  end;


// -------------------------------------------------------------------
//   DockingLayout
// -------------------------------------------------------------------

  TDockingMode = (dmTop, dmBottom, dmLeft, dmRight, dmClient, dmUndocked);

  TDockingItem = class(TLayoutItem)
  public
    Left, Top: LongInt;
    DockingMode: TDockingMode;
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

  TGridItem = class(TLayoutItem)
  public
    x, y, Width, Height: LongInt;
  end;

  TGridLayout = class(TLayout)
  protected
    FRows, FColumns: LongInt;
    FSameSizeCells: Boolean;
    FHorzSpacing, FVertSpacing: Integer;

    procedure FSetSameSizeCells(ASameSize: Boolean);

    procedure OnFinishCreation; override;
    procedure DoRecalcLayout; override;
    procedure ApplySize; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AddWidget(AWidget: TWidget; x, y, w, h: LongInt);
  published
    property  Rows: LongInt read FRows write FRows default 2;
    property  Columns: LongInt read FColumns write FColumns default 2;
    property  SameSizeCells: Boolean read FSameSizeCells
      write FSetSameSizeCells default True;
    property HorzSpacing: Integer read FHorzSpacing write FHorzSpacing;
    property VertSpacing: Integer read FVertSpacing write FVertSpacing;
  end;


// -------------------------------------------------------------------
//   BoxLayout
// -------------------------------------------------------------------

  TBoxOrientation = (boxHorz, boxVert);

  TBoxLayout = class(TLayout)
  protected
    FHorzAlign: THorzAlign;
    FVertAlign: TVertAlign;
    FOrientation: TBoxOrientation;
    FSpacing: Integer;

    procedure OnFinishCreation; override;
    procedure DoRecalcLayout; override;
    procedure ApplySize; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AddWidget(AWidget: TWidget);
  published
    property HorzAlign: THorzAlign read FHorzAlign write FHorzAlign default horzFixed;
    property VertAlign: TVertAlign read FVertAlign write FVertAlign default vertFixed;
    property Orientation: TBoxOrientation read FOrientation write FOrientation default boxHorz;
    property Spacing: Integer read FSpacing write FSpacing;
  end;


{
  $Log$
  Revision 1.5  2000/02/18 18:20:30  sg
  * Added "DoRecalcLayout" for TFixedLayout

  Revision 1.4  2000/02/17 22:23:57  sg
  * Added spacing properties to TGridLayout and TBoxLayout
  * Cleaned up the child widget collection item classes

  Revision 1.3  2000/02/10 18:35:55  sg
  * Adapted to new layouting methods

  Revision 1.2  2000/01/24 00:08:49  sg
  * Extended copyright to year 2000...

  Revision 1.1.1.1  1999/12/30 21:33:18  sg
  Initial import

}
