{
    $Id$

    fpGUI  -  Free Pascal Graphical User Interface
    Copyright (C) 2000 by
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
  Flags := [];
  if (wsClicked in WidgetState) and (wsMouseInside in WidgetState) then
    Include(Flags, btnIsPressed);
  if (wsHasFocus in WidgetState) and not Embedded then
  begin
    Include(Flags, btnIsSelected);
    if FindForm.IsActive then
      Include(Flags, btnHasFocus);
  end;

  Style.DrawButtonFace(Canvas, Rect(0, 0, Width, Height), Flags);
end;


// ===================================================================
//   TCustomButton
// ===================================================================

procedure TCustomButton.Paint(Canvas: TGfxCanvas);
var
  x, y: Integer;
  Borders: TRect;
begin
  inherited Paint(Canvas);

  Borders := Style.GetButtonBorders;
  Canvas.IntersectClipRect(Rect(Borders.Left, Borders.Top,
    Width - Borders.Right, Height - Borders.Bottom));

  Canvas.SetColor(Style.MapGUIColor(Canvas, clBtnText));
  x := (Width - Canvas.TextWidth(Text)) div 2;
  y := (Height - Canvas.FontCellHeight) div 2;
  if (wsClicked in WidgetState) and (wsMouseInside in WidgetState) then
  begin
    Inc(x);
    Inc(y);
  end;
  Style.DrawText(Canvas, x, y, Text, WidgetState);
end;

procedure TCustomButton.EvRecalcLayout;
var
  Borders: TRect;
begin
  LAYOUTTRACE('TCustomButton.EvRecalcLayout for %s:%s', [Name, ClassName]);
  Borders := Style.GetButtonBorders;
  with FindForm.Wnd.Canvas do
  begin
    MinW := Borders.Left + Borders.Right + TextWidth(Text);
    MinH := Borders.Left + Borders.Right + FontCellHeight;
  end;
  DefW := MinW + 20;
  DefH := MinH + 2;
end;


{
  $Log$
  Revision 1.1  2000/12/23 23:20:14  sg
  * First public CVS version...

}
