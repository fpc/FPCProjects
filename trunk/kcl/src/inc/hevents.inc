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
  protected
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

  TKeyEvent = function(Sender: TObject; Key: Char; KeyCode: LongWord;
    Shift: TShiftState): Boolean of object;

  TKeyEventObj = class(TEventObj)
  private
    FKey: Char;
    FKeyCode: LongWord;
    FShift: TShiftState;
  public
    constructor Create(ASender: TObject; AKey: Char; AKeyCode: LongWord;
      AShift: TShiftState);
  published
    property Key: Char read FKey write FKey;
    property KeyCode: LongWord read FKeyCode write FKeyCode;
    property Shift: TShiftState read FShift write FShift;
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
  protected
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
  protected
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
  protected
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
  protected
    FChangeType: TCmdChangeType;
  public
    constructor Create(ASender: TCommand; AChangeType: TCmdChangeType);
  published
    property ChangeType: TCmdChangeType read FChangeType write FChangeType;
  end;


{
  $Log$
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
