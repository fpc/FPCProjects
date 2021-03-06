{
    $Id$

    fpGUI  -  Free Pascal Graphical User Interface
    Copyright (C) 2000 - 2001 by
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
  FCanExpandWidth := True;
  FCursor := crIBeam;
end;

procedure TCustomEdit.Paint(Canvas: TGfxCanvas);
var
  Borders: TRect;
  s: String;
  x: Integer;
begin
  Borders := Style.GetEditBoxBorders;

  Style.DrawEditBox(Canvas, Rect(0, 0, BoundsSize.cx, BoundsSize.cy));

  if not Canvas.IntersectClipRect(Rect(Borders.Left + 1, Borders.Top + 1,
    BoundsSize.cx - Borders.Right - 1, BoundsSize.cy - Borders.Bottom - 1)) then
    exit;

  if wsEnabled in WidgetState then
    Canvas.SetColor(Style.GetUIColor(clWindowText))
  else
    Canvas.SetColor(Style.GetUIColor(clGrayText));

  if PasswordChar = #0 then
    s := PChar(Text)
  else
  begin
    SetLength(s, Length(Text));
    if Length(Text) > 0 then
      FillChar(s[1], Length(Text), PasswordChar);
  end;

  Canvas.TextOut(Borders.TopLeft + Point(1, 1), s);

  if wsHasFocus in WidgetState then
  begin
    x := Borders.Left + 1 + Canvas.TextWidth(Copy(s, 1, CursorPos));
    Canvas.DrawLine(Point(x, Borders.Top), Point(x, BoundsSize.cy - Borders.Bottom));
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

procedure TCustomEdit.CalcSizes;
var
  Borders: TRect;
begin
  Borders := Style.GetEditBoxBorders;
  FMinSize := Size(50, Borders.Top + Borders.Bottom +
    FindForm.Wnd.Canvas.FontCellHeight + 2);
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
  Revision 1.5  2001/02/14 23:14:44  sg
  * Adaptions to fpGFX changes; added much more usage of TSize and TPoint
    here as well
  * Heavy improvements of the layouting algorithm

  Revision 1.4  2001/02/09 20:50:20  sg
  * The usual bunch of improvements and bug fixes...

  Revision 1.3  2001/01/11 23:30:12  sg
  *** empty log message ***

  Revision 1.2  2000/12/24 13:18:12  sg
  * Some small fixes (improved speed, and better scrollbar handling where
    appropiate)

  Revision 1.1  2000/12/23 23:20:15  sg
  * First public CVS version...

}
