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
//   Common implementation for some methods of T(Custom)PaintBox
// ===================================================================

// -------------------------------------------------------------------
//   TCustomPaintBox
// -------------------------------------------------------------------

procedure TCustomPaintBox.DoPaint(ACanvas: TCanvas; const ARect: TRect);
begin
  if CallEventFilters(TPaintEventObj.Create(Self, ACanvas, ARect)) then
    if Assigned(FOnPaint) then
      FOnPaint(Self, ACanvas, ARect);
end;

function TCustomPaintBox.DoKey(AKey: Char; AKeyCode: LongWord;
  AShift: TShiftState): Boolean;
begin
  Result := CallEventFilters(TKeyEventObj.Create(Self, AKey, AKeyCode, AShift));
  if Result and Assigned(FOnKey) then
    Result := FOnKey(Self, AKey, AKeyCode, AShift);
end;

procedure TCustomPaintBox.DoMouseButtonDown(AButton: TMouseButton;
  AShift: TShiftState; AMouseX, AMouseY: Integer);
begin
  if CallEventFilters(TMouseButtonDownEventObj.
    Create(Self, AButton, AShift, AMouseX, AMouseY)) then
    if Assigned(FOnMouseButtonDown) then
      FOnMouseButtonDown(Self, AButton, AShift, AMouseX, AMouseY);
end;

procedure TCustomPaintBox.DoMouseButtonUp(AButton: TMouseButton;
  AShift: TShiftState; AMouseX, AMouseY: Integer);
begin
  if CallEventFilters(TMouseButtonUpEventObj.
    Create(Self, AButton, AShift, AMouseX, AMouseY)) then
    if Assigned(FOnMouseButtonUp) then
      FOnMouseButtonUp(Self, AButton, AShift, AMouseX, AMouseY);
end;

procedure TCustomPaintBox.DoMouseMove(AShift: TShiftState;
  AMouseX, AMouseY: Integer);
begin
  if CallEventFilters(TMouseMoveEventObj.
    Create(Self, AShift, AMouseX, AMouseY)) then
    if Assigned(FOnMouseMove) then
      FOnMouseMove(Self, AShift, AMouseX, AMouseY);
end;

constructor TCustomPaintBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;


{
  $Log$
  Revision 1.4  2000/02/19 19:09:00  sg
  * Fixed layouting bugs; minor improvements

  Revision 1.3  2000/02/10 18:29:38  sg
  * Added TPaintBox.Create

  Revision 1.2  2000/01/24 00:20:43  sg
  * Changed "DoKey" to function; it now returns if the event has been handled
    successfully or not

  Revision 1.1  2000/01/05 19:12:00  sg
  * First version

  Revision 1.1.1.1  1999/12/30 21:33:12  sg
  Initial import

}
