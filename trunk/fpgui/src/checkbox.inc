{
    $Id$

    fpGUI  -  Free Pascal Graphical User Interface
    Copyright (C) 2000 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    Checkbox widget implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


// ===================================================================
//   TCustomCheckBox
// ===================================================================

constructor TCustomCheckBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WidgetStyle := WidgetStyle + [wsCaptureMouse, wsClickable, wsOpaque];
end;

procedure TCustomCheckBox.Click;
begin
  FChecked := not FChecked;
  inherited Click;
end;

procedure TCustomCheckBox.Paint(Canvas: TGfxCanvas);
var
  FontHeight: Integer;
  LabelRect: TRect;
  Flags: TCheckboxFlags;
begin
  FontHeight := Canvas.FontCellHeight;
  LabelRect.Left := FLabelX;
  LabelRect.Top := FLabelY + (Height - MinH) div 2;
  LabelRect.Right := LabelRect.Left + Canvas.TextWidth(Text);
  LabelRect.Bottom := LabelRect.Top + FontHeight;

  Flags := [];
  if (wsClicked in WidgetState) and (wsMouseInside in WidgetState) then
    Include(Flags, cbIsPressed);
  if (wsHasFocus in WidgetState) and FindForm.IsActive then
    Include(Flags, cbHasFocus);
  if wsEnabled in WidgetState then
    Include(Flags, cbIsEnabled);
  if Checked then
    Include(Flags, cbIsChecked);

  Style.DrawCheckbox(Canvas, Rect(0, 0, Width, Height), LabelRect, Flags);
  Canvas.SetColor(Style.GetUIColor(clWindowText));
  Style.DrawText(Canvas, LabelRect.Left, LabelRect.Top, Text, WidgetState);
end;

procedure TCustomCheckBox.EvRecalcLayout;
begin
  with FindForm.Wnd.Canvas do
    Style.GetCheckBoxLayout(TextWidth(Text), FontCellHeight,
      MinW, MinH, FLabelX, FLabelY);
end;

procedure TCustomCheckBox.SetChecked(AChecked: Boolean);
begin
  if AChecked <> Checked then
  begin
    FChecked := AChecked;
    Redraw;
  end;
end;


{
  $Log$
  Revision 1.2  2001/02/09 20:50:19  sg
  * The usual bunch of improvements and bug fixes...

  Revision 1.1  2000/12/23 23:20:14  sg
  * First public CVS version...

}
