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
//   Implementation of constructors for event objects
// ===================================================================

constructor TEventObj.Create(ASender: TObject);
begin
  Sender := ASender;
end;

constructor TKeyEventObj.Create(ASender: TObject; AKey: Char;
  AKeyCode: LongWord; AShiftState: TShiftState);
begin
  inherited Create(ASender);
  Key := AKey;
  KeyCode := AKeyCode;
  ShiftState := AShiftState;
end;

constructor TMouseEventObj.Create(ASender: TObject; AMouseX, AMouseY: Integer;
  AShiftState: TShiftState);
begin
  inherited Create(ASender);
  MouseX := AMouseX;
  MouseY := AMouseY;
  ShiftState := AShiftState;
end;

constructor TPaintEventObj.Create(ASender: TObject; ACanvas: TCanvas;
  const ARect: TRect);
begin
  inherited Create(ASender);
  Canvas := ACanvas;
  Rect := ARect;
end;


{
  $Log$
  Revision 1.1  1999/12/30 21:33:12  sg
  Initial revision

}
