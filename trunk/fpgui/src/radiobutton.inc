{
    $Id$

    fpGUI  -  Free Pascal Graphical User Interface
    Copyright (C) 2000 - 2001 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    Radiobutton widget implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


// ===================================================================
//   TCustomRadioButton
// ===================================================================

constructor TCustomRadioButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WidgetStyle := WidgetStyle + [wsCaptureMouse, wsClickable];
end;

procedure TCustomRadioButton.Click;
begin
  if not Checked then
    SetChecked(True);
  inherited Click;
end;

procedure TCustomRadioButton.Paint(Canvas: TGfxCanvas);
var
  FontHeight: Integer;
  LabelRect: TRect;
  Flags: TCheckboxFlags;
begin
  FontHeight := Canvas.FontCellHeight;
  LabelRect.Left := FLabelPos.x;
  LabelRect.Top := FLabelPos.y + (Height - MinSize.cy) div 2;
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

  Style.DrawRadioButton(Canvas, Rect(0, 0, Width, Height), LabelRect, Flags);
  Canvas.SetColor(Style.GetUIColor(clWindowText));
  Style.DrawText(Canvas, LabelRect.TopLeft, Text, WidgetState);
end;

procedure TCustomRadioButton.CalcSizes;
begin
  with FindForm.Wnd.Canvas do
    Style.GetRadioButtonLayout(Size(TextWidth(Text), FontCellHeight),
      FMinSize, FLabelPos);
end;

procedure TCustomRadioButton.SetChecked(AChecked: Boolean);
var
  i: Integer;
  Child: TWidget;
begin
  if AChecked <> Checked then
  begin
    FChecked := AChecked;
    Redraw;

    if Checked and Assigned(Parent) and
      Parent.InheritsFrom(TContainerWidget) then
      for i := 0 to TContainerWidget(Parent).ChildCount - 1 do
      begin
        Child := TContainerWidget(Parent).Children[i];
	if (Child <> Self) and Child.InheritsFrom(TCustomRadioButton) then
	  TCustomRadioButton(Child).Checked := False;
      end;
  end;
end;


{
  $Log$
  Revision 1.3  2001/02/14 23:14:44  sg
  * Adaptions to fpGFX changes; added much more usage of TSize and TPoint
    here as well
  * Heavy improvements of the layouting algorithm

  Revision 1.2  2001/02/09 20:50:20  sg
  * The usual bunch of improvements and bug fixes...

  Revision 1.1  2000/12/23 23:20:15  sg
  * First public CVS version...

}
