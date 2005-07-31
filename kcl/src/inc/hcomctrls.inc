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
//   Common Controls:
//     T(Custom)TabControl
//     TTabSheet
//     TPageControl
//     TStatusBar (with TStatusPanel and TStatusPanels)
//     THeaderControl (with THeaderSection and THeaderSections)
//     T(Custom)TreeView (with TTreeNode and TTreeNodes)
//     TTrackBar
//     TProgressBar
//     T(Custom)RichEdit
//     T(Custom)UpDown
//     THotKey
//     T(Custom)ListView (and associated classes)
//     TAnimate
//     TToolBar (with TToolButton)
//     TCoolBar
//     T(Common)Calendar
//     TMonthCalendar
//     TDateTimePicker
//     TPageScroller
// ===================================================================

type

// -------------------------------------------------------------------
//   Tab Control
// -------------------------------------------------------------------

// Nothing here yet...


// -------------------------------------------------------------------
//   Tab Sheet
// -------------------------------------------------------------------

// Nothing here yet...


// -------------------------------------------------------------------
//   Page Control
// -------------------------------------------------------------------

// Nothing here yet...


// -------------------------------------------------------------------
//   Status Bar
// -------------------------------------------------------------------

  TStatusBar = class(TWinControl)
  protected
    procedure OnFinishCreation; override;
    procedure DoRecalcLayout; override;
    function ProcessEvent(Event: TEventObj): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Text;
  end;


// -------------------------------------------------------------------
//   Header Control
// -------------------------------------------------------------------

// Nothing here yet...


// -------------------------------------------------------------------
//   Tree View
// -------------------------------------------------------------------

  TTreeView = class(TWinControl)
  protected
    procedure OnFinishCreation; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property CanExpandWidth;
    property CanExpandHeight;
  end;


// -------------------------------------------------------------------
//   Track Bar
// -------------------------------------------------------------------

// Nothing here yet...


// -------------------------------------------------------------------
//   Progress Bar
// -------------------------------------------------------------------

// Nothing here yet...


// -------------------------------------------------------------------
//   Rich Edit
// -------------------------------------------------------------------

// Nothing here yet...


// -------------------------------------------------------------------
//   Up/Down
// -------------------------------------------------------------------

// Nothing here yet...


// -------------------------------------------------------------------
//   Hot Key
// -------------------------------------------------------------------

// Nothing here yet...


// -------------------------------------------------------------------
//   List View
// -------------------------------------------------------------------

// Nothing here yet...


// -------------------------------------------------------------------
//   Animate
// -------------------------------------------------------------------

// Nothing here yet...


// -------------------------------------------------------------------
//   Tool Bar
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


  TToolBar = class(TWinControl)
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
    function AddButton(AOwner: TComponent): TToolButton;
    function CalcItemsWidth: Integer;
  end;


// -------------------------------------------------------------------
//   Cool Bar
// -------------------------------------------------------------------

// Nothing here yet...


// -------------------------------------------------------------------
//   Calendar
// -------------------------------------------------------------------

// Nothing here yet...


// -------------------------------------------------------------------
//   Month Calendar
// -------------------------------------------------------------------

// Nothing here yet...


// -------------------------------------------------------------------
//   Date/Time Picker
// -------------------------------------------------------------------

// Nothing here yet..


// -------------------------------------------------------------------
//   Page Scroller
// -------------------------------------------------------------------

// Nothing here yet...


{
  $Log$
  Revision 1.1  2000/03/19 00:29:31  sg
  * Huge shift towards more VCL compatiblity (NOTE: Many widgets/controls
    are not working correctly at the moment!)

}
