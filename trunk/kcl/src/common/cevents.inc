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
//   Implementation of constructors for event objects
// ===================================================================

constructor TEventObj.Create(ASender: TObject);
begin
  Sender := ASender;
end;

constructor TKeyEventObj.Create(ASender: TObject; AKey: Char;
  AKeyCode: LongWord; AShift: TShiftState);
begin
  inherited Create(ASender);
  Key := AKey;
  KeyCode := AKeyCode;
  Shift := AShift;
end;

constructor TMouseEventObj.Create(ASender: TObject; AButton: TMouseButton;
  AShift: TShiftState; AMouseX, AMouseY: Integer);
begin
  inherited Create(ASender);
  Button := AButton;
  Shift := AShift;
  MouseX := AMouseX;
  MouseY := AMouseY;
end;

constructor TMouseMoveEventObj.Create(ASender: TObject; AShift: TShiftState;
  AMouseX, AMouseY: Integer);
begin
  inherited Create(ASender);
  Shift := AShift;
  MouseX := AMouseX;
  MouseY := AMouseY;
end;

constructor TPaintEventObj.Create(ASender: TObject; ACanvas: TCanvas;
  const ARect: TRect);
begin
  inherited Create(ASender);
  Canvas := ACanvas;
  Rect := ARect;
end;

constructor TCmdChangeEventObj.Create(ASender: TCommand; AChangeType: TCmdChangeType);
begin
  inherited Create(ASender);
  ChangeType := AChangeType;
end;


{
  $Log$
  Revision 1.3  2000/01/24 00:18:53  sg
  * Added TCmdChangeEventObj

  Revision 1.2  2000/01/05 19:11:12  sg
  * Made mouse event VCL compatible
  * Added mouse move event

  Revision 1.1.1.1  1999/12/30 21:33:12  sg
  Initial import

}
