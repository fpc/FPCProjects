{
    $Id$

    fpGUI  -  Free Pascal Graphical User Interface
    Copyright (C) 2000 by
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

function TCustomGroupBox.ProcessEvent(Event: TEventObj): Boolean;
begin
  if Event.InheritsFrom(TResizeEventObj) then
    Resized;

  Result := inherited ProcessEvent(Event);
end;

procedure TCustomGroupBox.Paint(Canvas: TGfxCanvas);
begin
  Style.DrawGroupBox(Canvas, Rect(0, 0, Width, Height), Text, WidgetState);
end;

procedure TCustomGroupBox.EvRecalcLayout;
var
  Borders: TRect;
  LabelWidth: Integer;
begin
  Borders := Style.GetGroupBoxBorders(FindForm.Wnd.Canvas, Text, LabelWidth);
  MinW := Borders.Left + Borders.Right + LabelWidth;
  MinH := Borders.Top + Borders.Bottom;
  if Assigned(Child) then
  begin
    if Child.MinW > LabelWidth then
      MinW := Borders.Left + Borders.Right + Child.MinW;
    Inc(MinH, Child.MinH);
    if Child.DefW > LabelWidth then
      DefW := Borders.Left + Borders.Right + Child.MinW;
    Inc(DefH, Child.DefH);
    if Child.MaxW > LabelWidth then
      MaxW := Min(Borders.Left + Borders.Right + Child.MaxW, InfiniteSize);
    MaxH := Min(MaxH + Child.MaxH, InfiniteSize);
  end;
end;

procedure TCustomGroupBox.Resized;
var
  LabelWidth: Integer;
begin
  if Assigned(Child) then
    with Style.GetGroupBoxBorders(FindForm.Wnd.Canvas, Text, LabelWidth) do
      Child.SetBounds(Left, Top, Width - Left - Right, Height - Top - Bottom);
end;


{
  $Log$
  Revision 1.1  2000/12/23 23:20:15  sg
  * First public CVS version...

}
