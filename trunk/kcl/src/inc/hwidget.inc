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
//   Main Definitions for TWidget
// ===================================================================

type

  TWidget = class;
  TForm = class;


  TWidgetCreationState = (wcsNotCreated, wcsCreationInProgress, wcsCreationDone);

  TSizeFlags = set of (sizeAuto, sizeFixed);

  TWidget = class(TKCLComponent)
  protected
    BlockDrawingCounter: LongInt;
    BlockRelayoutCounter: LongInt;

    FParent: TWidget;
    FVisible: Boolean;
    FWidth, FHeight: LongInt;
    FMinWidth, FMinHeight, FMaxWidth, FMaxHeight: LongInt;
    FSizeFlags: TSizeFlags;
    FText: String;
    FCreationState: TWidgetCreationState;
    FParentShowHint, FShowHint: Boolean;
    FHint: String;

    FOnCreate: TNotifyEvent;
    FOnFocusIn: TNotifyEvent;
    FOnFocusOut: TNotifyEvent;

    procedure SetVisible(AVisible: Boolean);
    procedure SetWidth(AWidth: LongInt);
    procedure SetHeight(AHeight: LongInt);
    procedure SetMinWidth(AWidth: LongInt);
    procedure SetMinHeight(AHeight: LongInt);
    procedure SetMaxWidth(AWidth: LongInt);
    procedure SetMaxHeight(AHeight: LongInt);
    procedure SetText(const AText: String); virtual;
    procedure SetHint(const AHint: String);

    procedure OnFinishCreation; virtual;
    procedure OnRecalcLayout; virtual;
    procedure OnSetSize; virtual;
    procedure OnSizeChanged; virtual;
    procedure OnMinMaxSizeChanged; virtual;
  public
    Handle: TWidgetHandle;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    {"FinishCreation" should be called when all properties have been set (the
     underlying window objects are created in FinishCreation). Note that some
     functions like TApplication.Run may call FinishCreation automatically.
     Before this function terminates, it will set 'CreationState' to
     wcsCreationDone.}
    procedure FinishCreation; virtual;
    procedure RecalcLayout;    // Mainly used for layout classes
    procedure ParentRecalcLayout;

    procedure SetSize(AWidth, AHeight: LongInt);
    procedure SetDefaultSize(AWidth, AHeight: LongInt);
    procedure SetFixedSize(AWidth, AHeight: LongInt);
    procedure CheckSize(var AWidth, AHeight: LongInt);

    procedure BlockDrawing;
    procedure UnblockDrawing;
    procedure Redraw;
    procedure Redraw(const rect: TRect);
    procedure Redraw(ax, ay, aw, ah: Integer);

    procedure SetFocus;
    function  HasFocus: Boolean;


    property Parent: TWidget read FParent;
    property CreationState: TWidgetCreationState read FCreationState
      default wcsNotCreated;
    property Text: String read FText write SetText;
    property ParentShowHint: Boolean
      read FParentShowHint write FParentShowHint default True;
  published
    property Visible: Boolean read FVisible write SetVisible default True;
    property Width: LongInt read FWidth write SetWidth;
    property Height: LongInt read FHeight write SetHeight;
    property MinWidth: LongInt read FMinWidth write SetMinWidth;
    property MinHeight: LongInt read FMinHeight write SetMinHeight;
    property MaxWidth: LongInt read FMaxWidth write SetMaxWidth
      default InfiniteSize;
    property MaxHeight: LongInt read FMaxHeight write SetMaxHeight
      default InfiniteSize;
    property SizeFlags: TSizeFlags read FSizeFlags write FSizeFlags;
    property Hint: string read FHint write SetHint;
    property ShowHint: Boolean read FShowHint write FShowHint;

    property OnCreate: TNotifyEvent read FOnCreate write FOnCreate;
    property OnFocusIn: TNotifyEvent read FOnFocusIn write FOnFocusIn;
    property OnFocusOut: TNotifyEvent read FOnFocusOut write FOnFocusOut;
  end;


{
  $Log$
  Revision 1.1  1999/12/30 21:33:17  sg
  Initial revision

}
