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
//   Event Definitions
// ===================================================================

{For each event, there should be two declarations:
  - procedure/function of object, for Delphi-style notifiers
  - event object, which contains the arguments from the according
    notifier as class members.
}


type


// -------------------------------------------------------------------
//   Generic
// -------------------------------------------------------------------

{$M+}
  TEventObj = class
  private
    RefCount: LongInt;
    FSender: TObject;
  public
    constructor Create(ASender: TObject);
    procedure AddRef;
    procedure Release;
  published
    property Sender: TObject read FSender write FSender;
  end;
{$M-}


// -------------------------------------------------------------------
//   Keyboard
// -------------------------------------------------------------------

  TKeyEvent = function(Sender: TObject; var Key: Word; Shift: TShiftState): Boolean of object;
  TKeyPressEvent = procedure(Sender: TObject; var Key: Word) of object;

  TKeyEventObj = class(TEventObj)
  private
    FKey: Word;
    FShift: TShiftState;
  public
    constructor Create(ASender: TObject; AKey: Word; AShift: TShiftState);
  published
    property Key: Word read FKey write FKey;
    property Shift: TShiftState read FShift write FShift;
  end;

  TKeyPressEventObj = class(TEventObj)
  private
    FKey: Word;
  public
    constructor Create(ASender: TObject; AKey: Word);
  published
    property Key: Word read FKey write FKey;
  end;


// -------------------------------------------------------------------
//   Mouse
// -------------------------------------------------------------------

  TMouseButton = (mbLeft, mbRight, mbMiddle);

  TClickEvent = TNotifyEvent;
  TClickEventObj = class(TEventObj) end;

  TMouseEvent = procedure(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; MouseX, MouseY: Integer) of object;

  TMouseEventObj = class(TEventObj)
  private
    FButton: TMouseButton;
    FShift: TShiftState;
    FMouseX, FMouseY: Integer;
  public
    constructor Create(ASender: TObject; AButton: TMouseButton;
      AShift: TShiftState; AMouseX, AMouseY: Integer);
  published
    property Button: TMouseButton read FButton write FButton;
    property Shift: TShiftState read FShift write FShift;
    property MouseX: Integer read FMouseX write FMouseX;
    property MouseY: Integer read FMouseY write FMouseY;
  end;

  TMouseButtonDownEventObj = type TMouseEventObj;
  TMouseButtonUpEventObj = type TMouseEventObj;


  TMouseMoveEvent = procedure(Sender: TObject; Shift: TShiftState;
    MouseX, MouseY: Integer) of object;

  TMouseMoveEventObj = class(TEventObj)
  private
    FShift: TShiftState;
    FMouseX, FMouseY: Integer;
  public
    constructor Create(ASender: TObject; AShift: TShiftState;
      AMouseX, AMouseY: Integer);
  published
    property Shift: TShiftState read FShift write FShift;
    property MouseX: Integer read FMouseX write FMouseX;
    property MouseY: Integer read FMouseY write FMouseY;
  end;


// -------------------------------------------------------------------
//   Painting
// -------------------------------------------------------------------

  TCanvas = class;

  TPaintEvent = procedure(Sender: TObject; ACanvas: TCanvas;
    const ARect: TRect) of object;

  TPaintEventObj = class(TEventObj)
  private
    FCanvas: TCanvas;
    FRect: TRect;
  public
    constructor Create(ASender: TObject; ACanvas: TCanvas;
      const ARect: TRect);
    property Rect: TRect read FRect write FRect;
  published
    property Canvas: TCanvas read FCanvas write FCanvas;
  end;


// -------------------------------------------------------------------
//   Commands
// -------------------------------------------------------------------

  TCmdExecuteEventObj = type TEventObj;


  TCommand = class;

  TCmdChangeType = (cctEnabled);

  TCmdChangeEventObj = class(TEventObj)
  private
    FChangeType: TCmdChangeType;
  public
    constructor Create(ASender: TCommand; AChangeType: TCmdChangeType);
  published
    property ChangeType: TCmdChangeType read FChangeType write FChangeType;
  end;


// -------------------------------------------------------------------
//   Owner drawing
// -------------------------------------------------------------------

  TOwnerDrawState = set of (odSelected, odGrayed, odDisabled, odChecked, odFocused);

  TDrawItemEvent = procedure(Control: TWinControl; Index: Integer; Rect: TRect;
    State: TOwnerDrawState) of object;

  TDrawItemEventObj = class(TEventObj)
  private
    FIndex: Integer;
    FLeft, FTop, FRight, FBottom: Integer;
    FState: TOwnerDrawState;
  public
    constructor Create(AControl: TWinControl; AIndex, ALeft, ATop, ARight, ABottom: Integer;
      AState: TOwnerDrawState);
  published
    property Index: Integer read FIndex write FIndex;
    property Left: Integer read FLeft write FLeft;
    property Top: Integer read FTop write FTop;
    property Right: Integer read FRight write FRight;
    property Bottom: Integer read FBottom write FBottom;
  end;


  TMeasureItemEvent = procedure(Control: TWinControl; Index: Integer;
    var Height: Integer) of object;

  TMeasureItemEventObj = class(TEventObj)
  private
    FIndex: Integer;
    FHeight: Integer;
  public
    constructor Create(AControl: TWinControl; AIndex, AHeight: Integer);
  published
    property Index: Integer read FIndex write FIndex;
    property Height: Integer read FHeight write FHeight;
  end;


{
  $Log$
  Revision 1.5  2000/03/19 00:29:31  sg
  * Huge shift towards more VCL compatiblity (NOTE: Many widgets/controls
    are not working correctly at the moment!)

  Revision 1.4  2000/02/10 18:33:57  sg
  * Added reference counting (AddRef, Release)

  Revision 1.3  2000/01/24 00:04:34  sg
  * TKeyEvent is now a function
  * Added event objects for TCommand

  Revision 1.2  2000/01/05 19:14:15  sg
  * Made mouse events VCL compatible
  * Added mouse move event

  Revision 1.1.1.1  1999/12/30 21:33:20  sg
  Initial import

}
