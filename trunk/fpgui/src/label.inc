{
    $Id$

    fpGUI  -  Free Pascal Graphical User Interface
    Copyright (C) 2000 - 2001 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    Label widget implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


// ===================================================================
//   TCustomLabel
// ===================================================================

procedure TCustomLabel.Paint(Canvas: TGfxCanvas);
var
  x: Integer;
begin
  Canvas.SetColor(Style.GetUIColor(clWindowText));
  case Alignment of
    taLeftJustify: x := 0;
    taCenter: x := (BoundsSize.cx - Canvas.TextWidth(Text)) div 2;
    taRightJustify: x := BoundsSize.cx - Canvas.TextWidth(Text);
  end;
  Style.DrawText(Canvas, Point(x,
    (BoundsSize.cy - Canvas.FontCellHeight) div 2), Text, WidgetState);
end;

procedure TCustomLabel.CalcSizes;
begin
  with FindForm.Wnd.Canvas do
    FMinSize := Size(TextWidth(Text), FontCellHeight);
end;

procedure TCustomLabel.SetAlignment(AAlignment: TAlignment);
begin
  if AAlignment <> Alignment then
  begin
    FAlignment := AAlignment;
    Redraw;
  end;
end;


{
  $Log$
  Revision 1.4  2001/02/14 23:14:44  sg
  * Adaptions to fpGFX changes; added much more usage of TSize and TPoint
    here as well
  * Heavy improvements of the layouting algorithm

  Revision 1.3  2001/02/09 20:50:20  sg
  * The usual bunch of improvements and bug fixes...

  Revision 1.2  2001/01/11 23:30:12  sg
  *** empty log message ***

  Revision 1.1  2000/12/23 23:20:15  sg
  * First public CVS version...

}
