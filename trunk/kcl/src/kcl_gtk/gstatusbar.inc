{
    $Id$

    KCL  -  Kassandra Component Library
    Copyright (C) 1999  by the KCL team
      see file AUTHORS in base directory of this distribution

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


procedure TStatusBar.OnFinishCreation;
begin
  Handle.Widget := gtk_statusbar_new;
  inherited OnFinishCreation;
  gtk_statusbar_push(PGtkStatusbar(Handle.Widget), 1, PChar(FText));
end;


{
  $Log$
  Revision 1.1  1999/12/30 21:33:50  sg
  Initial revision

}
