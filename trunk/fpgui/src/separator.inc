{
    $Id$

    fpGUI  -  Free Pascal Graphical User Interface
    Copyright (C) 2000 by
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

procedure TCustomSeparator.EvRecalcLayout;
begin
  if Orientation = Horizontal then
  begin
    FCanExpandWidth := True;
    FCanExpandHeight := False;
    MaxW := InfiniteSize;
    MinH := Style.GetSeparatorSize + 2 * Spacing
  end else
  begin
    FCanExpandWidth := False;
    FCanExpandHeight := True;
    MinW := Style.GetSeparatorSize + 2 * Spacing;
    MaxH := InfiniteSize;
  end;
end;

procedure TCustomSeparator.SetOrientation(AOrientation: TOrientation);
begin
  if AOrientation <> Orientation then
  begin
    FOrientation := AOrientation;
    RecalcLayout;
  end;
end;

procedure TCustomSeparator.SetSpacing(ASpacing: Integer);
begin
  if ASpacing <> Spacing then
  begin
    FSpacing := ASpacing;
    RecalcLayout;
  end;
end;


{
  $Log$
  Revision 1.2  2001/01/11 23:30:12  sg
  *** empty log message ***

  Revision 1.1  2000/12/23 23:20:15  sg
  * First public CVS version...

}
