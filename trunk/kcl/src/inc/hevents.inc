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
    FSender: TObject;
  public
    constructor Create(ASender: TObject);
  published
    property Sender: TObject read FSender write FSender;
  end;
{$M-}


// -------------------------------------------------------------------
//   Keyboard
// -------------------------------------------------------------------

  TKeyEvent = procedure(Sender: TObject; Key: Char; KeyCode: LongWord;
    ShiftState: TShiftState) of object;

  TKeyEventObj = class(TEventObj)
  private
    FKey: Char;
    FKeyCode: LongWord;
    FShiftState: TShiftState;
  public
    constructor Create(ASender: TObject; AKey: Char; AKeyCode: LongWord;
      AShiftState: TShiftState);
  published
    property Key: Char read FKey write FKey;
    property KeyCode: LongWord read FKeyCode write FKeyCode;
    property ShiftState: TShiftState read FShiftState write FShiftState;
  end;


// -------------------------------------------------------------------
//   Mouse
// -------------------------------------------------------------------

  TClickEvent = TNotifyEvent;
  TClickEventObj = class(TEventObj) end;

  TMouseEvent = procedure(Sender: TObject; MouseX, MouseY: Integer;
    ShiftState: TShiftState) of object;

  TMouseEventObj = class(TEventObj)
  protected
    FMouseX, FMouseY: Integer;
    FShiftState: TShiftState;
  public
    constructor Create(ASender: TObject; AMouseX, AMouseY: Integer;
      AShiftState: TShiftState);
  published
    property MouseX: Integer read FMouseX write FMouseX;
    property MouseY: Integer read FMouseY write FMouseY;
    property ShiftState: TShiftState read FShiftState write FShiftState;
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


{
  $Log$
  Revision 1.1  1999/12/30 21:33:20  sg
  Initial revision

}
