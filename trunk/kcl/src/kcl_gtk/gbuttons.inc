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


// ===================================================================
//   Button widgets implementation for GTK+
// ===================================================================

// -------------------------------------------------------------------
//   TButton
// -------------------------------------------------------------------

procedure TButton_Clicked(GtkButton: PGtkButton; button: TButton); cdecl;
begin
  if Assigned(button.OnClick) then
    button.OnClick(button);
end;

procedure TButton.OnFinishCreation;
begin
  Handle.Widget := gtk_button_new_with_label(PChar(FText));
  inherited OnFinishCreation;
  gtk_signal_connect(PGtkObject(Handle.Widget), 'clicked',
    GTK_SIGNAL_FUNC(@TButton_Clicked), self);
end;


// -------------------------------------------------------------------
//   TCheckBox
//	Looks like GTK doesn't support 3state check boxes, therefore
//      the state 'cbGrayed' is currently not supported.  (sg)
// -------------------------------------------------------------------

function TCheckBox.GetState: TCheckBoxState;
begin
  if gtk_toggle_button_get_active(PGtkToggleButton(Handle.Widget)) then
    Result := cbChecked
  else
    Result := cbUnchecked;
end;

procedure TCheckBox.SetState(AState: TCheckBoxState);
begin
  gtk_toggle_button_set_active(PGtkToggleButton(Handle.Widget),
    AState = cbChecked);
end;

procedure TCheckBox.OnFinishCreation;
begin
  Handle.Widget := gtk_check_button_new_with_label(PChar(FText));
  inherited OnFinishCreation;
  gtk_signal_connect(PGtkObject(Handle.Widget), 'clicked',
    GTK_SIGNAL_FUNC(@TButton_Clicked), self);
end;


{
  $Log$
  Revision 1.1  1999/12/30 21:33:24  sg
  Initial revision

}
