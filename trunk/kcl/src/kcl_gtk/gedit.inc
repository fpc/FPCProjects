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
// Edit widget implementation for GTK+
// ===================================================================

procedure TEdit.OnFinishCreation;
begin
  if FMultiLine then begin
    FHandle := gtk_text_new(nil, nil);
    gtk_text_set_editable(PGtkText(FHandle), True);
  end else begin
    FHandle := gtk_entry_new;
    if Length(FText) > 0 then
      gtk_entry_set_text(PGtkEntry(FHandle), PChar(FText));
  end;
  inherited OnFinishCreation;
end;


{
  $Log$
  Revision 1.2  2000/01/24 00:23:14  sg
  * Implemented multi-line support (switchable via property "MultiLine")

  Revision 1.1.1.1  1999/12/30 21:33:33  sg
  Initial import

}
