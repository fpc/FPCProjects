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
//   Label widget implementation for GTK+
// ===================================================================


procedure TLabel.OnFinishCreation;
begin
  FHandle := gtk_label_new(PChar(FText));
  gtk_label_set_justify(PGtkLabel(FHandle), GTK_JUSTIFY_LEFT);
  inherited OnFinishCreation;
end;

procedure TLabel.DoRecalcLayout;
begin
  inherited DoRecalcLayout;
  DefW := gdk_string_width(PGtkStyle(FHandle^.thestyle)^.font, PChar(FText));
  MinW := DefW;
end;

procedure TLabel.SetText(const AText: String);
begin
  if Assigned(FHandle) then
    gtk_label_set_text(PGtkLabel(FHandle), PChar(AText));
  inherited SetText(AText);
end;


{
  $Log$
  Revision 1.3  2000/02/17 22:28:36  sg
  * Implemented "DoRecalcLayout"
  * Label text can now be changed after creation

  Revision 1.2  2000/01/24 00:30:55  sg
  * Extended copyright to year 2000...
  * Modifications to reflect the changed definition of TWidgetHandle

  Revision 1.1.1.1  1999/12/30 21:33:25  sg
  Initial import

}
