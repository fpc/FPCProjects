{
    $Id$

    KCL  -  Kassandra Component Library
    Copyright (C) 1999 - 2000  by the KCL team
      see file AUTHORS in base directory of this distribution

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


// ===================================================================
//   TCustomGroupBox implementation for GTK+
// ===================================================================

procedure TCustomGroupBox.OnFinishCreation;
begin
  FHandle := gtk_frame_new(PChar(FText));
  inherited OnFinishCreation;

  if Assigned(FContent) then begin
    FContent.FinishCreation;
    if Assigned(FContent.FHandle) then
      gtk_container_add(PGtkContainer(FHandle), FContent.FHandle);
  end;
end;

procedure TCustomGroupBox.DoRecalcLayout;
var
  BorderWidth, ContentDefW, ContentDefH,
    ContentMinW, ContentMinH, ContentMaxW, ContentMaxH: Integer;
begin
  BorderWidth := PGtkContainer(FHandle)^.flag0 and bm_TGtkContainer_border_width;
  MinW := BorderWidth + PGtkStyle(FHandle^.thestyle)^.klass^.xthickness * 2;
  MinH := BorderWidth + PGtkFrame(FHandle)^.label_height +
    PGtkStyle(FHandle^.thestyle)^.klass^.ythickness;
  DefW := MinW;
  DefH := MinH;
  MaxW := MinW;
  MaxH := MinH;
  if Assigned(FContent) then begin
    ContentDefW := FContent.DefW;
    ContentDefH := FContent.DefH;
    ContentMinW := FContent.MinW;
    ContentMinH := FContent.MinH;
    ContentMaxW := FContent.MaxW;
    ContentMaxH := FContent.MaxH;
  end else begin
    ContentDefW := 50;
    ContentDefH := 50;
    ContentMinW := 20;
    ContentMinH := 20;
    ContentMaxW := InfiniteSize;
    ContentMaxH := InfiniteSize;
  end;

  if ContentDefW > PGtkFrame(FHandle)^.label_width then
    Inc(DefW, ContentDefW)
  else
    Inc(DefW, PGtkFrame(FHandle)^.label_width);

  if ContentMinW > PGtkFrame(FHandle)^.label_width then
    Inc(MinW, ContentMinW)
  else
    Inc(MinW, PGtkFrame(FHandle)^.label_width);

  Inc(DefH, ContentDefH);
  Inc(MinH, ContentMinH);

  if ContentMaxW > PGtkFrame(FHandle)^.label_width then
    MaxW := Min(InfiniteSize, MaxW + ContentMaxW)
  else
    Inc(MaxW, PGtkFrame(FHandle)^.label_width);

  MaxH := Min(InfiniteSize, MaxH + ContentMaxH);
end;

procedure TCustomGroupBox.LayoutChildren;
begin
end;

procedure TCustomGroupBox.SetText(const AText: String);
begin
  if Assigned(FHandle) then
    gtk_frame_set_label(PGtkFrame(FHandle), PChar(AText));
  inherited SetText(AText);
end;


{
  $Log$
  Revision 1.4  2000/02/24 13:39:32  sg
  * More layout fixes...

  Revision 1.3  2000/02/22 14:40:32  sg
  * More layout fixes; minor improvements

  Revision 1.2  2000/02/18 22:19:49  sg
  * Implemented SetText

  Revision 1.1  2000/02/17 22:32:24  sg
  * First implementation

}
