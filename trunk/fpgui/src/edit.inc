{
    $Id$

    fpGUI  -  Free Pascal Graphical User Interface
    Copyright (C) 2000 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    Edit widget implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


// ===================================================================
//   TCustomEdit
// ===================================================================

constructor TCustomEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WidgetStyle := WidgetStyle + [wsCaptureMouse, wsClickable, wsOpaque];
end;

procedure TCustomEdit.Paint(Canvas: TGfxCanvas);
var
  Borders: TRect;
  s: String;
  x: Integer;
begin
  Borders := Style.GetEditBoxBorders;

  Style.DrawEditBox(Canvas, Rect(0, 0, Width, Height));
  Canvas.IntersectClipRect(Rect(Borders.Left + 1, Borders.Top + 1,
    Width - Borders.Right - 1, Height - Borders.Bottom - 1));
  if wsEnabled in WidgetState then
    Style.SetUIColor(Canvas, clBtnText)
  else
    Style.SetUIColor(Canvas, clGrayText);

  if PasswordChar = #0 then
    s := PChar(Text)
  else
  begin
    SetLength(s, Length(Text));
    if Length(Text) > 0 then
      FillChar(s[1], Length(Text), PasswordChar);
  end;

  Canvas.TextOut(Borders.Left + 1, Borders.Top + 1, s);

  if wsHasFocus in WidgetState then
  begin
    x := Borders.Left + 1 + Canvas.TextWidth(Copy(s, 1, CursorPos));
    Canvas.DrawLine(x, Borders.Top, x, Height - Borders.Bottom);
  end;
end;

procedure TCustomEdit.EvKeyPressed(Key: Word; Shift: TShiftState);
begin
  if Shift * [ssShift, ssAlt, ssCtrl,
    ssMeta, ssSuper, ssHyper, ssAltGr] = [] then
    case Key of
      keyLeft, keyUp:
        if CursorPos > 0 then
	  CursorPos := CursorPos - 1;
      keyRight, keyDown:
	if CursorPos < Length(Text) then
	  CursorPos := CursorPos + 1;
      keyHome:
        CursorPos := 0;
      keyEnd:
        CursorPos := Length(Text);
      else
        inherited EvKeyPressed(Key, Shift);
    end
  else
    inherited EvKeyPressed(Key, Shift);
end;

procedure TCustomEdit.EvKeyChar(KeyChar: Char);
begin
  case KeyChar of
    #8:
      if CursorPos > 0 then
      begin
        Text := Copy(Text, 1, CursorPos - 1) +
	  Copy(Text, CursorPos + 1, Length(Text));
        CursorPos := CursorPos - 1;
      end;
    #127:
      if CursorPos < Length(Text) then
      begin
        Text := Copy(Text, 1, CursorPos) + Copy(Text, CursorPos + 2, Length(Text));
	Redraw;
      end;
    #32..#126, #128..#255:
      begin
        Text := Copy(Text, 1, CursorPos) + KeyChar +
          Copy(Text, CursorPos + 1, Length(Text));
        CursorPos := CursorPos + 1;
      end;
    else
      inherited EvKeyChar(KeyChar);
  end;
end;

procedure TCustomEdit.EvRecalcLayout;
var
  Borders: TRect;
begin
  Borders := Style.GetEditBoxBorders;
  MinW := 50;
  MinH := Borders.Top + Borders.Bottom +
    FindForm.Wnd.Canvas.FontCellHeight + 2;
end;

procedure TCustomEdit.EvTextChanged;
begin
  Redraw;
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TCustomEdit.SetPasswordChar(APasswordChar: Char);
begin
  if APasswordChar <> PasswordChar then
  begin
    FPasswordChar := APasswordChar;
    Redraw;
  end;
end;

procedure TCustomEdit.SetCursorPos(ACursorPos: Integer);
begin
  if ACursorPos <> CursorPos then
  begin
    FCursorPos := ACursorPos;
    Redraw;
  end;
end;


{
  $Log$
  Revision 1.1  2000/12/23 23:20:15  sg
  * First public CVS version...

}
