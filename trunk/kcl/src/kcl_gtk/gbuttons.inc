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
  if FDefault then begin
    GTK_WIDGET_SET_FLAGS(FHandle, GTK_CAN_DEFAULT);
    gtk_widget_grab_default(FHandle);
  end;

  inherited OnFinishCreation;

  gtk_signal_connect(PGtkObject(FHandle), 'clicked',
    GTK_SIGNAL_FUNC(@TGenericButton_Clicked), self);
end;

procedure TButton.DoRecalcLayout;
var
  font: PGdkFont;
  BorderSize, xthickness, ythickness: Integer;
begin
  inherited DoRecalcLayout;

  font := PGtkStyle(PGtkBin(FHandle)^.child^.thestyle)^.font;

  BorderSize := PGtkContainer(FHandle)^.flag0 and bm_TGtkContainer_border_width;
  xthickness := PGtkStyle(FHandle^.thestyle)^.klass^.xthickness;
  ythickness := PGtkStyle(FHandle^.thestyle)^.klass^.ythickness;

  MinH := font^.ascent + font^.descent + 2 * (BorderSize + ythickness + 1);
  DefW := MinW + 16;
  DefH := MinH + 4;
end;

procedure TButton.SetText(const AText: String);
begin
  if Assigned(FHandle) then
    gtk_label_set_text(PGtkLabel(PGtkBin(FHandle)^.child), PChar(AText));
  inherited SetText(AText);
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

procedure TCheckBox.DoRecalcLayout;
begin
  inherited DoRecalcLayout;
  MaxH := DefH;
end;

procedure TCheckBox.SetText(const AText: String);
begin
  if Assigned(FHandle) then
    gtk_label_set_text(PGtkLabel(PGtkBin(FHandle)^.child), PChar(AText));
  inherited SetText(AText);
end;


// -------------------------------------------------------------------
//   TRadioButton
// -------------------------------------------------------------------

procedure TRadioButton.SetChecked(AChecked: Boolean);
begin
  FChecked := AChecked;
  gtk_toggle_button_set_active(PGtkToggleButton(FHandle), AChecked);
end;

procedure TRadioButton.OnFinishCreation;

  function FindOtherRadioButton(p: TWidget): PGtkRadioButton;
  var
    i: Integer;
    r: PGtkRadioButton;
  begin
    if (p <> Self) and (p is TRadioButton) then
      Result := PGtkRadioButton(p.FHandle)
    else begin
      for i := 0 to p.WidgetCount - 1 do begin
	r := FindOtherRadioButton(p.Widgets[i]);
	if Assigned(r) then begin
	  Result := r;
	  exit;
	end;
      end;
      Result := nil;
    end;
  end;

var
  p: TWidget;
  other: PGtkRadioButton;
begin
  // We must find other radio buttons within the same radio group
  p := FParent;
  other := nil;
  while Assigned(p) do begin
    other := FindOtherRadioButton(p);
    if Assigned(other) or p.IsRadioGroup then break;
    p := p.FParent;
  end;

  FHandle := gtk_radio_button_new_with_label_from_widget(other, PChar(FText));
  inherited OnFinishCreation;
end;

procedure TRadioButton.DoRecalcLayout;
begin
  inherited DoRecalcLayout;
  MaxH := DefH;
end;

procedure TRadioButton.SetText(const AText: String);
begin
  if Assigned(FHandle) then
    gtk_label_set_text(PGtkLabel(PGtkBin(FHandle)^.child), PChar(AText));
  inherited SetText(AText);
end;


{
  $Log$
  Revision 1.7  2000/02/22 14:40:32  sg
  * More layout fixes; minor improvements

  Revision 1.6  2000/02/19 19:09:00  sg
  * Fixed layouting bugs; minor improvements

  Revision 1.5  2000/02/18 22:19:49  sg
  * Implemented SetText

  Revision 1.4  2000/02/17 22:26:38  sg
  * Added TRadioButton

  Revision 1.3  2000/02/10 18:40:16  sg
  * Added layout calculations to TButton

  Revision 1.2  2000/01/24 00:22:24  sg
  * Added TImageButton

  Revision 1.1.1.1  1999/12/30 21:33:24  sg
  Initial import

}
