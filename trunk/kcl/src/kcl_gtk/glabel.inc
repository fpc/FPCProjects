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

procedure TCustomLabel.SetAlignment(AAlignment: TAlignment);
var
  x: Single;
begin
  FAlignment := AAlignment;
  if Assigned(FHandle) then begin
    case FAlignment of
      taLeftJustify : x := 0;
      taRightJustify: x := 1.0;
      taCenter	  : x := 0.5;
    end;
    gtk_misc_set_alignment(PGtkMisc(FHandle), x, 0.5);
  end;
end;

procedure TCustomLabel.OnFinishCreation;
begin
  FHandle := gtk_label_new(nil);
  gtk_label_parse_uline(PGtkLabel(FHandle), PChar(ConvertUnderlines(FText)));
  SetAlignment(FAlignment);
  inherited OnFinishCreation;
end;

procedure TCustomLabel.DoRecalcLayout;
begin
end;

procedure TCustomLabel.SetText(const AText: String);
begin
  if Assigned(FHandle) then
    gtk_label_parse_uline(PGtkLabel(FHandle), PChar(ConvertUnderlines(AText)));
  inherited SetText(AText);
end;


{
  $Log$
  Revision 1.5  2000/02/24 14:56:16  sg
  * Implemented "Alignment" property
  * TCustomLabel now supports "&" in "Text" property to mark shortcuts

  Revision 1.4  2000/02/24 13:39:32  sg
  * More layout fixes...

  Revision 1.3  2000/02/17 22:28:36  sg
  * Implemented "DoRecalcLayout"
  * Label text can now be changed after creation

  Revision 1.2  2000/01/24 00:30:55  sg
  * Extended copyright to year 2000...
  * Modifications to reflect the changed definition of TWidgetHandle

  Revision 1.1.1.1  1999/12/30 21:33:25  sg
  Initial import

}
