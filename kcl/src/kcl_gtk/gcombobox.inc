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

  if HandleAllocated then
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
  if Assigned(list) then
    gtk_combo_set_popdown_strings(PGtkCombo(FHandle), list);

  gtk_entry_set_text(PGtkEntry(PGtkCombo(FHandle)^.entry), PChar(FText));

  inherited OnFinishCreation;

  gtk_signal_connect(PGtkObject(PGtkCombo(FHandle)^.entry), 'changed',
    GTK_SIGNAL_FUNC(@TCustomComboBox_TextChanged), Self);
end;

procedure TCustomComboBox.DoRecalcLayout;
begin
  MinW := MinH;
  MaxH := DefH;
end;


function TCustomComboBox.ProcessEvent(Event: TEventObj): Boolean;
begin
  if Event.ClassType = TTextChangedEventObj then
    if HandleAllocated then
      gtk_entry_set_text(PGtkEntry(PGtkCombo(FHandle)^.entry), PChar(FText));

  Result := inherited ProcessEvent(Event);
end;


procedure TCustomComboBox.Clear;
begin
  if HandleAllocated then
    gtk_combo_set_popdown_strings(PGtkCombo(FHandle), nil);
  FItems.Clear;
end;


{
  $Log$
  Revision 1.5  2000/03/19 00:29:31  sg
  * Huge shift towards more VCL compatiblity (NOTE: Many widgets/controls
    are not working correctly at the moment!)

  Revision 1.4  2000/02/24 13:39:32  sg
  * More layout fixes...

  Revision 1.3  2000/02/19 19:09:00  sg
  * Fixed layouting bugs; minor improvements

  Revision 1.2  2000/02/18 22:19:49  sg
  * Implemented SetText

  Revision 1.1  2000/02/17 22:32:24  sg
  * First implementation

}
