{
    $Id$

    fpGUI  -  Free Pascal Graphical User Interface
    Copyright (C) 2000 - 2001 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    Groupbox widget implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


// ===================================================================
//   TCustomGroupBox
// ===================================================================

procedure TCustomGroupBox.Paint(Canvas: TGfxCanvas);
begin
  Style.DrawGroupBox(Canvas, Rect(0, 0, Width, Height), Text, WidgetState);
end;

procedure TCustomGroupBox.CalcSizes;
var
  Borders: TRect;
  LabelWidth: Integer;
begin
  Borders := Style.GetGroupBoxBorders(FindForm.Wnd.Canvas, Text, LabelWidth);
  FMinSize.cx := Borders.Left + Borders.Right + LabelWidth;
  FMinSize.cy := Borders.Top + Borders.Bottom;
  if Assigned(Child) then
  begin
    if Child.MinSize.cx > LabelWidth then
      FMinSize.cx := Borders.Left + Borders.Right + Child.MinSize.cx;
    Inc(FMinSize.cy, Child.MinSize.cy);
    if Child.DefSize.cx > LabelWidth then
      FDefSize.cx := Borders.Left + Borders.Right + Child.MinSize.cx;
    Inc(FDefSize.cy, Child.DefSize.cy);
    if Child.MaxSize.cx > LabelWidth then
      FMaxSize.cx := Min(Borders.Left + Borders.Right + Child.MaxSize.cx, InfiniteSize);
    FMaxSize.cy := Min(MaxSize.cy + Child.MaxSize.cy, InfiniteSize);
  end;
end;

procedure TCustomGroupBox.Resized;
var
  LabelWidth: Integer;
begin
  if Assigned(Child) then
    with Style.GetGroupBoxBorders(FindForm.Wnd.Canvas, Text, LabelWidth) do
      Child.SetBounds(TopLeft, Size(Width - Left - Right, Height - Top - Bottom));
end;


{
  $Log$
  Revision 1.2  2001/02/14 23:14:44  sg
  * Adaptions to fpGFX changes; added much more usage of TSize and TPoint
    here as well
  * Heavy improvements of the layouting algorithm

  Revision 1.1  2000/12/23 23:20:15  sg
  * First public CVS version...

}
