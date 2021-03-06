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

// -------------------------------------------------------------------
//   TButtonControl
// -------------------------------------------------------------------

function TButtonControl.ProcessEvent(Event: TEventObj): Boolean;
begin
  if Event.ClassType = TTextChangedEventObj then
    if HandleAllocated then
      gtk_label_set_text(PGtkLabel(PGtkBin(FHandle)^.child), PChar(FText));

  Result := inherited ProcessEvent(Event);
end;


// -------------------------------------------------------------------
//   TButton
// -------------------------------------------------------------------

procedure TButton_OnMap(GtkButton: PGtkButton; button: TButton); cdecl;
begin
  if button.FDefault then begin
    GTK_WIDGET_SET_FLAGS(button.FHandle, GTK_CAN_DEFAULT);
    gtk_widget_grab_default(button.FHandle);
  end;
end;


procedure TButton.OnFinishCreation;
var
  lbl: PGtkWidget;
begin
  FHandle := gtk_button_new;
  lbl := gtk_label_new(nil);
  gtk_label_parse_uline(PGtkLabel(lbl), PChar(ConvertUnderlines(FText)));
  gtk_widget_show(lbl);
  gtk_container_add(PGtkContainer(FHandle), lbl);

  inherited OnFinishCreation;

  gtk_signal_connect(PGtkObject(FHandle), 'clicked', GTK_SIGNAL_FUNC(@TWinControl_Clicked), self);

  gtk_signal_connect(PGtkObject(FHandle), 'map',
    GTK_SIGNAL_FUNC(@TButton_OnMap), self);
end;

procedure TButton.DoRecalcLayout;
begin
  DefW := MinW + 16;
  DefH := MinH + 4;
end;




// -------------------------------------------------------------------
//   TImageButton
// -------------------------------------------------------------------

{procedure TImageButton.OnFinishCreation;
var
  item: PImageListItem;
  pixmap: PGtkWidget;
begin
  ASSERT(Assigned(FImageList));
  ASSERT((FImageIndex >= 0) and (FImageIndex < FImageList.priv.Count));
  FHandle := gtk_button_new;
  // Get the GtkImage from the ImageList and use it as child for the button
  item := PImageListItem(FImageList.priv.Items[FImageIndex]);
  pixmap := gtk_pixmap_new(item^.ImageCanvas.Handle, item^.Mask);
  gtk_container_add(PGtkContainer(FHandle), PGtkWidget(pixmap));

  inherited OnFinishCreation;
  gtk_signal_connect(PGtkObject(FHandle), 'clicked',
    GTK_SIGNAL_FUNC(@TGenericButton_Clicked), self);
  gtk_widget_show(pixmap);
end;}


// -------------------------------------------------------------------
//   TCheckBox
//	Looks like GTK doesn't support 3state check boxes, therefore
//      the state 'cbGrayed' is currently not supported. ### (sg)
// -------------------------------------------------------------------

function TCustomCheckBox.GetState: TCheckBoxState;
begin
  if gtk_toggle_button_get_active(PGtkToggleButton(FHandle)) then
    Result := cbChecked
  else
    Result := cbUnchecked;
end;

procedure TCustomCheckBox.SetState(AState: TCheckBoxState);
begin
  gtk_toggle_button_set_active(PGtkToggleButton(FHandle),
    AState = cbChecked);
end;

function TCustomCheckBox.GetChecked: Boolean;
begin
    Result := (State = cbChecked);
end;

procedure TCustomCheckBox.SetChecked(Value: Boolean);
begin
  if Value then
    State := cbChecked
  else
    State := cbUnchecked;
end;

procedure TCustomCheckBox.OnFinishCreation;
var
  lbl: PGtkWidget;
begin
  FHandle := gtk_check_button_new;
  lbl := gtk_label_new(nil);
  gtk_misc_set_alignment(PGtkMisc(lbl), 0, 0.5);
  gtk_label_parse_uline(PGtkLabel(lbl), PChar(ConvertUnderlines(FText)));
  gtk_widget_show(lbl);
  gtk_container_add(PGtkContainer(FHandle), lbl);

  inherited OnFinishCreation;

  gtk_signal_connect(PGtkObject(FHandle), 'toggled', GTK_SIGNAL_FUNC(@TWinControl_Clicked), self);
end;


procedure TCustomCheckBox.DoRecalcLayout;
begin
  MaxH := DefH;
end;


// -------------------------------------------------------------------
//   TRadioButton
// -------------------------------------------------------------------

procedure TRadioButton.SetChecked(AChecked: Boolean);
begin
  FChecked := AChecked;
  gtk_toggle_button_set_active(PGtkToggleButton(FHandle), AChecked);
end;

Function TRadioButton.GetChecked : Boolean;
begin
  FChecked:=gtk_toggle_button_get_active(PGtkToggleButton(FHandle));
  Result:=FChecked;
end;

procedure TRadioButton.OnFinishCreation;

  function FindOtherRadioButton(p: TWinControl): PGtkRadioButton;
  var
    i: Integer;
    r: PGtkRadioButton;
  begin
    {if (p <> Self) and (p is TRadioButton) then
      Result := PGtkRadioButton(p.FHandle)
    else begin
      for i := 0 to p.ControlCount - 1 do begin
        if p.Controls[i].IsRadioGroup then continue;
	r := FindOtherRadioButton(p.Controls[i]);
	if Assigned(r) then begin
	  Result := r;
	  exit;
	end;
      end;
      Result := nil;
    end;}
    Result := nil;
  end;

var
  p: TWinControl;
  other: PGtkRadioButton;
  lbl: PGtkWidget;
begin
  // We must find other radio buttons within the same radio group
  p := FParent;
  other := nil;
  while Assigned(p) do begin
    other := FindOtherRadioButton(p);
    if Assigned(other) or p.IsRadioGroup then break;
    p := p.Parent;
  end;

  FHandle := gtk_radio_button_new_from_widget(other);
  lbl := gtk_label_new(nil);
  gtk_misc_set_alignment(PGtkMisc(lbl), 0, 0.5);
  gtk_label_parse_uline(PGtkLabel(lbl), PChar(ConvertUnderlines(FText)));
  gtk_widget_show(lbl);
  gtk_container_add(PGtkContainer(FHandle), lbl);

  inherited OnFinishCreation;

  gtk_signal_connect(PGtkObject(FHandle), 'toggled', GTK_SIGNAL_FUNC(@TWinControl_Clicked), self);
end;

procedure TRadioButton.DoRecalcLayout;
begin
  MaxH := DefH;
end;


{
  $Log$
  Revision 1.13  2000/03/19 00:29:31  sg
  * Huge shift towards more VCL compatiblity (NOTE: Many widgets/controls
    are not working correctly at the moment!)

  Revision 1.12  2000/03/01 22:25:41  michael
  inc/hwidgets.inc

  Revision 1.11  2000/02/25 15:12:56  sg
  * TButton.Default now works correctly (but not changing this value after
    TButton.FinishCreation...)
  * Simplified size calculations for TButton

  Revision 1.10  2000/02/25 11:40:28  michael
  + Added Checked reader to TRadioButton
  + Started support for ShowModal

  Revision 1.9  2000/02/24 14:55:38  sg
  * TButton, TCheckBox and TRadioButton now support "&" to mark shortcuts

  Revision 1.8  2000/02/24 13:39:32  sg
  * More layout fixes...

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
