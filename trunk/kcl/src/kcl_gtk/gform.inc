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

procedure TCustomForm_SizeAllocate(widget: PGtkWidget; alloc: PGtkAllocation;
  AForm: TCustomForm); cdecl;
begin
  //WriteLn('Form: Size allocated for ', AForm.ClassName, ': ', alloc^.Width, 'x', alloc^.Height);
  if (AForm.FWidth <> alloc^.width) or (AForm.FHeight <> alloc^.height) then begin
    AForm.FWidth := alloc^.width;
    AForm.FHeight := alloc^.height;
    if Assigned(AForm.FContent) then
      AForm.FContent.SetSize(AForm.FWidth, AForm.FHeight);
  end;
end;

procedure TCustomForm_OnMap(GtkWidget: PGtkWidget; ev: PGdkEvent; AForm: TCustomForm); cdecl;
begin
  if (AForm.FPositionX >= 0) and (AForm.FPositionY >= 0) then
    AForm.SetPosition(AForm.FPositionX, AForm.FPositionY);
end;

function TCustomForm_OnDelete(GtkWidget: PGtkWidget; ev: PGdkEvent;
  AForm: TCustomForm): Boolean; cdecl;
begin
  Result := False;
  if not (csDestroying in AForm.ComponentState) then begin
    if not AForm.CanClose then exit;

    // Save the last used window position!
    gdk_window_get_root_origin(AForm.FHandle^.window, @AForm.FPositionX, @AForm.FPositionY);

    if Assigned(AForm.OnClose) then
      AForm.OnClose(AForm);

    AForm.Free;
  end;
  Result := True;
end;


procedure TCustomForm.SetContent(AContent: TWidget);
begin
  FContent := AContent;
  FContent.FParent := Self;

  if Assigned(FHandle) and Assigned(FContent) and Assigned(FContent.FHandle) then
    gtk_container_add(PGtkContainer(FHandle), FContent.FHandle);
end;

function TCustomForm.GetPositionX: Integer;
begin
  if Assigned(FHandle) and Assigned(FHandle^.window) then
    gdk_window_get_position(FHandle^.window, @FPositionX, @FPositionY);
  Result := FPositionX;
end;

function TCustomForm.GetPositionY: Integer;
begin
  if Assigned(FHandle) and Assigned(FHandle^.window) then
    gdk_window_get_position(FHandle^.window, @FPositionX, @FPositionY);
  Result := FPositionY;
end;

procedure TCustomForm.ApplyMinMaxSize;
var
  geometry: TGdkGeometry;
begin
  ASSERT(Assigned(FHandle));
  geometry.min_width := MinW;
  geometry.min_height := MinH;
  geometry.max_width := MaxW;
  geometry.max_height := MaxH;
  gtk_window_set_geometry_hints(PGtkWindow(FHandle), nil,
    @geometry, GDK_HINT_MIN_SIZE or GDK_HINT_Max_SIZE);
end;

procedure TCustomForm.OnFinishCreation;
begin
  if not Assigned(FHandle) then    // -> derived class can create their own form
    FHandle := gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_policy(PGtkWindow(FHandle), 1, 1, 0);

  gtk_window_add_accel_group(PGtkWindow(FHandle), priv.AccelGroup);

  if Assigned(FContent) then begin
    FContent.FinishCreation;
    if Assigned(FContent.FHandle) then
      gtk_container_add(PGtkContainer(FHandle), FContent.FHandle);
  end;

  inherited OnFinishCreation;

  RecalcLayout;

  gtk_signal_connect(PGtkObject(FHandle), 'size_allocate',
    GTK_SIGNAL_FUNC(@TCustomForm_SizeAllocate), Self);

  gtk_signal_connect(PGtkObject(FHandle), 'map_event',
    GTK_SIGNAL_FUNC(@TCustomForm_OnMap), Self);

  gtk_window_set_title(PGtkWindow(FHandle), PChar(FText));
  gtk_signal_connect(PGtkObject(FHandle), 'delete-event',
    GTK_SIGNAL_FUNC(@TCustomForm_OnDelete), Self);
  ApplySize;
end;

procedure TCustomForm.ApplySize;
begin
  if Assigned(FHandle) then
    gtk_window_set_default_size(PGtkWindow(FHandle), FWidth, FHeight);
  if Assigned(FContent) then
    FContent.SetSize(FWidth, FHeight);
end;

constructor TCustomForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FDefaultWidth := -1;
  FDefaultHeight := -1;
  FPositionX := -1;
  FPositionY := -1;

  priv.AccelGroup := gtk_accel_group_new;
end;

procedure TCustomForm.Show;
begin
  ASSERT(Assigned(FHandle));
  gtk_widget_show(FHandle);
end;

procedure TCustomForm.Close;
begin
  // ###
end;

procedure TCustomForm.SetPosition(x, y: Integer);
begin
  if Assigned(FHandle) and Assigned(FHandle^.window) then
    gdk_window_move(FHandle^.window, x, y)
  else begin
    FPositionX := x;
    FPositionY := y;
  end;
end;


{
  $Log$
  Revision 1.4  2000/02/10 18:41:23  sg
  * Adapted to new TCustomForm and TForm interfaces

  Revision 1.3  2000/01/24 00:30:55  sg
  * Extended copyright to year 2000...
  * Modifications to reflect the changed definition of TWidgetHandle

  Revision 1.2  2000/01/06 23:01:14  sg
  * Added Accelerator Group creation to TGenericForm

  Revision 1.1.1.1  1999/12/30 21:33:25  sg
  Initial import

}
