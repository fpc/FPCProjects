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
//   TCustomComboBox implementation for GTK+
// ===================================================================

procedure TCustomComboBox_TextChanged(edit: PGtkEditable; combo: TCustomComboBox); cdecl;
begin
  combo.FText := gtk_entry_get_text(PGtkEntry(PGtkCombo(combo.FHandle)^.entry));
end;


function TCustomComboBox.GetItemIndex: Integer;
var
  i: Integer;
begin
  for i := 0 to FItems.Count - 1 do
    if FText = FItems[i] then begin
      Result := i;
      break;
    end;
end;

procedure TCustomComboBox.SetItemIndex(AIndex: Integer);
begin
  if (AIndex >= 0) and (AIndex < FItems.Count) then
    FText := FItems[AIndex]
  else
    SetLength(FText, 0);

  if Assigned(FHandle) then
    gtk_entry_set_text(PGtkEntry(PGtkCombo(FHandle)^.entry), PChar(FText));
end;

procedure TCustomComboBox.OnFinishCreation;
var
  i: integer;
  list: PGList;
begin
  FHandle := gtk_combo_new;
  if FStyle = csDropDownList then
    gtk_entry_set_editable(PGtkEntry(PGtkCombo(FHandle)^.entry), False);

  list := nil;
  for i := 0 to FItems.Count - 1 do
    list := g_list_append(list, PChar(FItems[i]));
  gtk_combo_set_popdown_strings(PGtkCombo(FHandle), list);

  gtk_entry_set_text(PGtkEntry(PGtkCombo(FHandle)^.entry), PChar(FText));

  inherited OnFinishCreation;

  gtk_signal_connect(PGtkObject(PGtkCombo(FHandle)^.entry), 'changed',
    GTK_SIGNAL_FUNC(@TCustomComboBox_TextChanged), Self);
end;

procedure TCustomComboBox.DoRecalcLayout;
var
  ButtonReq: TGtkRequisition;
begin
  inherited DoRecalcLayout;

  PGtkWidgetClass(gtk_type_class(GTK_OBJECT_TYPE(PGtkObject(PGtkCombo(FHandle)^.button))))^.
    size_request(PGtkCombo(FHandle)^.button, @ButtonReq);

  MinW := ButtonReq.Width + 20;
  DefW := ButtonReq.Width + 80;
  MaxH := DefH;
end;

procedure TCustomComboBox.SetText(const AText: String);
begin
  if Assigned(FHandle) then
    gtk_entry_set_text(PGtkEntry(PGtkCombo(FHandle)^.entry), PChar(AText));
  inherited SetText(AText);
end;

procedure TCustomComboBox.Clear;
begin
  if Assigned(FHandle) then
    gtk_combo_set_popdown_strings(PGtkCombo(FHandle), nil);
  FItems.Clear;
end;


{
  $Log$
  Revision 1.3  2000/02/19 19:09:00  sg
  * Fixed layouting bugs; minor improvements

  Revision 1.2  2000/02/18 22:19:49  sg
  * Implemented SetText

  Revision 1.1  2000/02/17 22:32:24  sg
  * First implementation

}
