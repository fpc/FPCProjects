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
//   Button widgets implementation for GTK+
// ===================================================================

procedure TGenericButton_Clicked(GtkButton: PGtkButton;
  button: TGenericButton); cdecl;
begin
  button.DoClick;
end;


// -------------------------------------------------------------------
//   TButton
// -------------------------------------------------------------------

procedure TButton.OnFinishCreation;
begin
  FHandle := gtk_button_new_with_label(PChar(FText));
  inherited OnFinishCreation;
  gtk_signal_connect(PGtkObject(FHandle), 'clicked',
    GTK_SIGNAL_FUNC(@TGenericButton_Clicked), self);
end;

procedure TButton.DoRecalcLayout;
begin
  inherited DoRecalcLayout;
  Inc(DefW, 16);
  Inc(DefH, 4);
end;


// -------------------------------------------------------------------
//   TImageButton
// -------------------------------------------------------------------

procedure TImageButton.OnFinishCreation;
var
  item: PImageListItem;
  pixmap: PGtkWidget;
begin
  ASSERT(Assigned(FImageList));
  ASSERT((FImageIndex >= 0) and (FImageIndex < FImageList.priv.Count));
  FHandle := gtk_button_new;
  // Get the GtkImage from the ImageList and use it as child for the button
  item := PImageListItem(FImageList.priv.Items[FImageIndex]);
  pixmap := gtk_pixmap_new(item^.ImageCanvas.pixmap, item^.Mask);
  gtk_container_add(PGtkContainer(FHandle), PGtkWidget(pixmap));

//###  SetFixedSize(FImageList.Width + 8, FImageList.Height + 8);
  inherited OnFinishCreation;
  gtk_signal_connect(PGtkObject(FHandle), 'clicked',
    GTK_SIGNAL_FUNC(@TGenericButton_Clicked), self);
  gtk_widget_show(pixmap);
end;


// -------------------------------------------------------------------
//   TCheckBox
//	Looks like GTK doesn't support 3state check boxes, therefore
//      the state 'cbGrayed' is currently not supported. ### (sg)
// -------------------------------------------------------------------

function TCheckBox.GetState: TCheckBoxState;
begin
  if gtk_toggle_button_get_active(PGtkToggleButton(FHandle)) then
    Result := cbChecked
  else
    Result := cbUnchecked;
end;

procedure TCheckBox.SetState(AState: TCheckBoxState);
begin
  gtk_toggle_button_set_active(PGtkToggleButton(FHandle),
    AState = cbChecked);
end;

procedure TCheckBox.OnFinishCreation;
begin
  FHandle := gtk_check_button_new_with_label(PChar(FText));
  inherited OnFinishCreation;
  gtk_signal_connect(PGtkObject(FHandle), 'clicked',
    GTK_SIGNAL_FUNC(@TGenericButton_Clicked), self);
end;


{
  $Log$
  Revision 1.3  2000/02/10 18:40:16  sg
  * Added layout calculations to TButton

  Revision 1.2  2000/01/24 00:22:24  sg
  * Added TImageButton

  Revision 1.1.1.1  1999/12/30 21:33:24  sg
  Initial import

}
