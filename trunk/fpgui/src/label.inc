{
    $Id$

    fpGUI  -  Free Pascal Graphical User Interface
    Copyright (C) 2000 by
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
begin
  Canvas.SetColor(Style.MapGUIColor(Canvas, clBtnText));
  Style.DrawText(Canvas, (Width - Canvas.TextWidth(Text)) div 2,
    (Height - Canvas.FontCellHeight) div 2, Text, WidgetState);
end;

procedure TCustomLabel.EvRecalcLayout;
begin
  with FindForm.Wnd.Canvas do
  begin
    MinW := TextWidth(Text);
    MinH := FontCellHeight;
  end;
  DefW := MinW;
  DefH := MinH;
end;


{
  $Log$
  Revision 1.1  2000/12/23 23:20:15  sg
  * First public CVS version...

}
