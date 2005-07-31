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
//   Form classes implementations for GTK+
// ===================================================================

// -------------------------------------------------------------------
//   TCustomForm
// -------------------------------------------------------------------

procedure TCustomForm_OnMap(GtkWidget: PGtkWidget; ev: PGdkEvent; AForm: TCustomForm); cdecl;
begin
  if (AForm.FPositionX >= 0) and (AForm.FPositionY >= 0) then
    AForm.SetPosition(AForm.FPositionX, AForm.FPositionY);
end;

function TCustomForm_OnDelete(GtkWidget: PGtkWidget; ev: PGdkEvent;
  AForm: TCustomForm): Boolean; cdecl;
begin
  Result := False;
  AForm.Close;
end;


procedure TCustomForm.SetContent(AContent: TControl);
begin
  FContent := AContent;
  FContent.Parent := Self;

{!!!:  if HandleAllocated and Assigned(FContent) and FContent.HandleAllocated then
    gtk_container_add(PGtkContainer(FHandle), FContent.FHandle);}
end;

function TCustomForm.GetPositionX: Integer;
begin
  if HandleAllocated and Assigned(FHandle^.window) then
    gdk_window_get_position(FHandle^.window, @FPositionX, @FPositionY);
  Result := FPositionX;
end;

function TCustomForm.GetPositionY: Integer;
begin
  if HandleAllocated and Assigned(FHandle^.window) then
    gdk_window_get_position(FHandle^.window, @FPositionX, @FPositionY);
  Result := FPositionY;
end;

procedure TCustomForm.ApplyMinMaxSize;
var
  geometry: TGdkGeometry;
begin
  ASSERT(HandleAllocated);
  geometry.min_width := MinW;
  geometry.min_height := MinH;
  geometry.max_width := MaxW;
  geometry.max_height := MaxH;
  gtk_window_set_geometry_hints(PGtkWindow(FHandle), nil,
    @geometry, GDK_HINT_MIN_SIZE or GDK_HINT_Max_SIZE);
end;

procedure TCustomForm.OnFinishCreation;
begin
  if not HandleAllocated then    // -> derived class can create their own form
    if fsModal in FFormState then
      FHandle := gtk_window_new(GTK_WINDOW_DIALOG)
    else
      FHandle := gtk_window_new(GTK_WINDOW_TOPLEVEL);

  gtk_window_set_policy(PGtkWindow(FHandle), 1, 1, 0);
  gtk_container_set_border_width(PGtkContainer(FHandle), FBorderWidth);

  gtk_window_add_accel_group(PGtkWindow(FHandle), priv.AccelGroup);

  if Assigned(FContent) and FContent.InheritsFrom(TWinControl) then
    TWinControl(FContent).HandleNeeded;
  
  inherited OnFinishCreation;

  if Length(FText) = 0 then
    gtk_window_set_title(PGtkWindow(FHandle), PChar(Application.Title))
  else
    gtk_window_set_title(PGtkWindow(FHandle), PChar(FText));

  gtk_signal_connect(PGtkObject(FHandle), 'map_event',
    GTK_SIGNAL_FUNC(@TCustomForm_OnMap), Self);

  gtk_signal_connect(PGtkObject(FHandle), 'delete-event',
    GTK_SIGNAL_FUNC(@TCustomForm_OnDelete), Self);
end;

procedure TCustomForm.LayoutChildren;
begin
end;

constructor TCustomForm.Create(AOwner: TComponent);
begin
  FFormState := [fsCreating];

  inherited Create(AOwner);


  FDefaultWidth := -1;
  FDefaultHeight := -1;
  FPositionX := -1;
  FPositionY := -1;

  priv.AccelGroup := gtk_accel_group_new;

  if Assigned(FOnCreate) then
    FOnCreate(Self);

  FFormState := [];
end;

procedure TCustomForm.Close;
var
  CloseAction: TCloseAction;
begin
  if (not (csDestroying in ComponentState)) and CloseQuery then begin
    // Save the last used window position!
    gdk_window_get_root_origin(FHandle^.window, @FPositionX, @FPositionY);

    if Assigned(OnClose) then begin
      CloseAction := caFree;
      OnClose(Self, CloseAction);
    end;
    if fsModal in FFormState then
      gtk_main_quit
    else
      Self.Free;
  end;
end;

procedure TCustomForm.Show;
begin
  HandleNeeded;
  Include(FFormState, fsVisible);
  gtk_widget_show(FHandle);
end;

function TCustomForm.ShowModal: Integer;
begin
  Include(FFormState, fsModal);
  HandleNeeded;
  Include(FFormState, fsVisible);
  gtk_window_set_modal(PGtkWindow(FHandle), True);
  gtk_widget_show(FHandle);
  gtk_main;
  Result := FModalResult;
end;

procedure TCustomForm.SetPosition(x, y: Integer);
begin
  if HandleAllocated and Assigned(FHandle^.window) then
//    gdk_window_move(FHandle^.window, x, y)
    gtk_widget_set_uposition(FHandle, x, y)
  else begin
    FPositionX := x;
    FPositionY := y;
  end;
end;


{
  $Log$
  Revision 1.11  2000/03/19 00:29:31  sg
  * Huge shift towards more VCL compatiblity (NOTE: Many widgets/controls
    are not working correctly at the moment!)

  Revision 1.10  2000/02/25 12:44:29  michael
  + ShowModal support finished

  Revision 1.9  2000/02/25 11:40:28  michael
  + Added Checked reader to TRadioButton
  + Started support for ShowModal

  Revision 1.8  2000/02/24 13:39:32  sg
  * More layout fixes...

  Revision 1.7  2000/02/22 14:40:32  sg
  * More layout fixes; minor improvements

  Revision 1.6  2000/02/18 18:22:39  sg
  * Fixed resizing when a border is set (BorderWidth > 0)

  Revision 1.5  2000/02/17 22:28:06  sg
  * Improved behaviour on form closing
  * Added BorderWidth property
}
