{
    $Id$

    fpGUI  -  Free Pascal Graphical User Interface
    Copyright (C) 2000 - 2001 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    Separator widget implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


// ===================================================================
//   TCustomSeparator
// ===================================================================

constructor TCustomSeparator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanExpandWidth := True;
  FSpacing := 4;
end;

procedure TCustomSeparator.Paint(Canvas: TGfxCanvas);
begin
  Style.DrawSeparator(Canvas, Rect(0, 0, Width, Height), Orientation);
end;

procedure TCustomSeparator.CalcSizes;
begin
  if Orientation = Horizontal then
  begin
    FCanExpandWidth := True;
    FCanExpandHeight := False;
    FMinSize.cy := Style.GetSeparatorSize + 2 * Spacing
  end else
  begin
    FCanExpandWidth := False;
    FCanExpandHeight := True;
    FMinSize.cx := Style.GetSeparatorSize + 2 * Spacing;
  end;
end;

procedure TCustomSeparator.SetOrientation(AOrientation: TOrientation);
begin
  if AOrientation <> Orientation then
  begin
    FOrientation := AOrientation;
    Update;
  end;
end;

procedure TCustomSeparator.SetSpacing(ASpacing: Integer);
begin
  if ASpacing <> Spacing then
  begin
    FSpacing := ASpacing;
    Update;
  end;
end;


{
  $Log$
  Revision 1.3  2001/02/14 23:14:45  sg
  * Adaptions to fpGFX changes; added much more usage of TSize and TPoint
    here as well
  * Heavy improvements of the layouting algorithm

  Revision 1.2  2001/01/11 23:30:12  sg
  *** empty log message ***

  Revision 1.1  2000/12/23 23:20:15  sg
  * First public CVS version...

}
