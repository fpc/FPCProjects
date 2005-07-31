{
    $Id$

    fpGUI  -  Free Pascal Graphical User Interface
    Copyright (C) 2000 - 2001 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    Button widgets implementations

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

// ===================================================================
//   TGenericButton
// ===================================================================

constructor TGenericButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WidgetStyle := WidgetStyle + [wsCaptureMouse, wsClickable, wsOpaque];
end;

procedure TGenericButton.Paint(Canvas: TGfxCanvas);
var
  Flags: TButtonFlags;
begin
  if Embedded then
    Flags := [btnIsEmbedded]
  else
    Flags := [];
  if (wsClicked in WidgetState) and (wsMouseInside in WidgetState) then
    Include(Flags, btnIsPressed);
  if (wsHasFocus in WidgetState) and not Embedded then
  begin
    Include(Flags, btnIsSelected);
    if FindForm.IsActive then
      Include(Flags, btnHasFocus);
  end;

  Style.DrawButtonFace(Canvas, Rect(0, 0, BoundsSize.cx, BoundsSize.cy), Flags);
end;


// ===================================================================
//   TCustomButton
// ===================================================================

procedure TCustomButton.Paint(Canvas: TGfxCanvas);
var
  Pt: TPoint;
  x, y: Integer;
  Borders: TRect;
begin
  inherited Paint(Canvas);

  Borders := Style.GetButtonBorders;
  Canvas.IntersectClipRect(Rect(Borders.Left, Borders.Top,
    BoundsSize.cx - Borders.Right, BoundsSize.cy - Borders.Bottom));

  Canvas.SetColor(Style.GetUIColor(clBtnText));
  Pt.x := (BoundsSize.cx - Canvas.TextWidth(Text)) div 2;
  Pt.y := (BoundsSize.cy - Canvas.FontCellHeight) div 2;
  if (wsClicked in WidgetState) and (wsMouseInside in WidgetState) then
    Pt := Pt + Point(1, 1);
  Style.DrawText(Canvas, Pt, Text, WidgetState);
end;

procedure TCustomButton.CalcSizes;
var
  Borders: TRect;
begin
  LAYOUTTRACE('TCustomButton.CalcSizes for %s:%s', [Name, ClassName]);
  Borders := Style.GetButtonBorders;
  with FindForm.Wnd.Canvas do
  begin
    FMinSize.cx := Borders.Left + Borders.Right + TextWidth(Text);
    FMinSize.cy := Borders.Left + Borders.Right + FontCellHeight;
  end;
  FDefSize := FMinSize + Size(20, 2);
end;


{
  $Log$
  Revision 1.4  2001/02/14 23:14:43  sg
  * Adaptions to fpGFX changes; added much more usage of TSize and TPoint
    here as well
  * Heavy improvements of the layouting algorithm

  Revision 1.3  2001/02/09 20:50:19  sg
  * The usual bunch of improvements and bug fixes...

  Revision 1.2  2001/01/11 23:30:11  sg
  *** empty log message ***

  Revision 1.1  2000/12/23 23:20:14  sg
  * First public CVS version...

}
