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
//   Status bar widget implementation for GTK+
// ===================================================================

procedure TStatusBar.OnFinishCreation;
begin
  FHandle := gtk_statusbar_new;
  inherited OnFinishCreation;
  gtk_statusbar_push(PGtkStatusbar(FHandle), 1, PChar(FText));
end;

procedure TStatusBar.DoRecalcLayout;
begin
  MinW := DefH;
  MaxH := DefH;
  DefW := DefH * 4;
end;

procedure TStatusBar.SetText(const AText: String);
begin
  FText := AText;
  if Assigned(FHandle) then begin
    gtk_statusbar_pop(PGtkStatusbar(FHandle), 1);
    gtk_statusbar_push(PGtkStatusbar(FHandle), 1, PChar(FText));
  end;
end;


{
  $Log$
  Revision 1.5  2000/02/24 13:39:32  sg
  * More layout fixes...

  Revision 1.4  2000/02/17 22:30:34  sg
  * The statusbar text now can be changed after creation

  Revision 1.3  2000/02/10 18:47:38  sg
  * Added layout calculation to TStatusBar

  Revision 1.2  2000/01/24 00:30:55  sg
  * Extended copyright to year 2000...
  * Modifications to reflect the changed definition of TWidgetHandle

  Revision 1.1.1.1  1999/12/30 21:33:50  sg
  Initial import

}
