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
//   Main Definitions for TWidget
// ===================================================================

// Helper function:
function ClipMinMax(val, min, max: Integer): Integer;

type

  TWidget = class;
  TForm = class;


  TWidgetCreationState = (wcsNotCreated, wcsCreationInProgress, wcsCreationDone);

  TWidget = class(TKCLComponent)
  protected
    BlockDrawingCounter: LongInt;

    FHandle: TWidgetHandle;
    FParent: TWidget;
    FVisible: Boolean;
    FEnabled: Boolean;
    DefW, DefH, MinW, MinH, MaxW, MaxH: Integer;
    FWidth, FHeight: Integer;
    FText: String;
    FCreationState: TWidgetCreationState;
    FParentShowHint, FShowHint: Boolean;
    FHint: String;

    FOnCreate: TNotifyEvent;
    FOnFocusIn: TNotifyEvent;
    FOnFocusOut: TNotifyEvent;

    function  GetWidgetCount: Integer; virtual;
    function  GetWidgets(AIndex: Integer): TWidget; virtual; abstract;
    procedure SetSize(AWidth, AHeight: Integer);
    procedure RecalcLayout;		// called at initialization or by a child
    procedure SetVisible(AVisible: Boolean);

    // Platform specific:
    procedure Show;
    procedure Hide;
    procedure SetEnabled(AEnabled: Boolean);
    procedure SetText(const AText: String); dynamic;
    procedure SetHint(const AHint: String);
    procedure OnFinishCreation; virtual;
    procedure DoRecalcLayout; virtual;
    procedure ApplySize; virtual;	// apply current size (FWidth and FHeight); adjust children

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function  AddChildWidget(AWidget: TWidget): Boolean; dynamic;
    procedure RemoveChildWidget(AWidget: TWidget); dynamic; abstract;

    {"FinishCreation" should be called when all properties have been set (the
     underlying window objects are created in FinishCreation). Note that some
     functions like TApplication.Run may call FinishCreation automatically.
     Before this function terminates, it will set 'CreationState' to
     wcsCreationDone.}
    procedure FinishCreation;

    procedure BlockDrawing;
    procedure UnblockDrawing;
    procedure Redraw;
    procedure Redraw(const rect: TRect);
    procedure Redraw(ax, ay, aw, ah: Integer);

    procedure SetFocus;
    function  HasFocus: Boolean;

    function  IsRadioGroup: Boolean; dynamic;

    property Handle: TWidgetHandle read FHandle;
    property Parent: TWidget read FParent;
    property WidgetCount: Integer read GetWidgetCount;
    property Widgets[AIndex: Integer]: TWidget read GetWidgets;
    property CreationState: TWidgetCreationState read FCreationState default wcsNotCreated;
    property Text: String read FText write SetText;
    // State
    property Visible: Boolean read FVisible write SetVisible default True;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    // Size
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    // Hints
    property Hint: String read FHint write SetHint;
    property ShowHint: Boolean read FShowHint write FShowHint;
    property ParentShowHint: Boolean read FParentShowHint write FParentShowHint default True;
    // Events
    property OnCreate: TNotifyEvent read FOnCreate write FOnCreate;
    property OnFocusIn: TNotifyEvent read FOnFocusIn write FOnFocusIn;
    property OnFocusOut: TNotifyEvent read FOnFocusOut write FOnFocusOut;
  end;


{
  $Log$
  Revision 1.4  2000/02/17 22:24:41  sg
  * Added properties "Widgets" and "WidgetCount"
  * Added "IsRadioGroup" (needed for TRadioButton under GTK)

  Revision 1.3  2000/02/10 18:37:43  sg
  * Heavy interface changes for the new layouting algorithm

  Revision 1.2  2000/01/24 00:07:15  sg
  * Added AddChildWidget and RemoveChildWidget
  * "Handle" is now a read-only property
  * Added "Enabled" property

  Revision 1.1.1.1  1999/12/30 21:33:17  sg
  Initial import

}
