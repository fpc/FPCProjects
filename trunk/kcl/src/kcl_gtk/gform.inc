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

constructor TGenericForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // Set a default size
  FWidth := 100;
  FHeight := 100;

  priv.AccelGroup := gtk_accel_group_new;
end;

function TGenericForm_OnDestroy(GtkObject: PGtkObject; AForm: TGenericForm):
  Boolean; cdecl;
begin
  Result := False;
  if not AForm.CanClose then exit;
  if Assigned(AForm.OnClose) then
    AForm.OnClose(AForm);
  AForm.Free;
  Result := True;
end;

procedure TForm.SetContent(AContent: TWidget);
begin
  FContent := AContent;
  FContent.FParent := Self;

  if sizeFixed in FSizeFlags then
    AContent.SizeFlags := AContent.SizeFlags + [sizeFixed];

  if Assigned(FHandle) and Assigned(FContent.FHandle) then
    gtk_container_add(PGtkContainer(FHandle), FContent.FHandle);
end;

procedure TForm.OnFinishCreation;
begin
  if not Assigned(FHandle) then    // -> derived class can create their own form
    FHandle := gtk_window_new(GTK_WINDOW_TOPLEVEL);

  gtk_window_add_accel_group(PGtkWindow(FHandle), priv.AccelGroup);

  inherited OnFinishCreation;

  if Assigned(FContent) then begin
    if sizeFixed in FSizeFlags then
      FContent.SizeFlags := FContent.SizeFlags + [sizeFixed];
    FContent.FinishCreation;
    if Assigned(FContent.FHandle) then
      gtk_container_add(PGtkContainer(FHandle), FContent.FHandle);
    if sizeAuto in FSizeFlags then begin
      // WriteLn('Setting form size to ', FContent.FWidth, 'x', FContent.FHeight);
      SetSize(FContent.FWidth, FContent.FHeight);
    end;
  end;

  gtk_window_set_title(PGtkWindow(FHandle), PChar(FText));
  gtk_signal_connect(PGtkObject(FHandle), 'destroy',
    GTK_SIGNAL_FUNC(@TGenericForm_OnDestroy), Self);
end;

procedure TForm.OnSetSize;
begin
  if Assigned(FHandle) then begin
    gtk_window_set_default_size(PGtkWindow(FHandle), FWidth, FHeight);
    if sizeFixed in FSizeFlags then begin
      gtk_window_set_policy(PGtkWindow(FHandle), 0, 0, 0);
      gtk_widget_set_usize(FHandle, FWidth, FHeight);
      OnMinMaxSizeChanged;
    end;
  end;
  if Assigned(FContent) then
    FContent.SetSize(FWidth, FHeight);
end;

procedure TForm.OnSizeChanged;
begin
  inherited OnSizeChanged;
end;

procedure TForm.Show;
begin
  ASSERT(Assigned(FHandle));
  gtk_widget_show(FHandle);
end;

procedure TForm.Close;
begin
  // ###
end;


{
  $Log$
  Revision 1.3  2000/01/24 00:30:55  sg
  * Extended copyright to year 2000...
  * Modifications to reflect the changed definition of TWidgetHandle

  Revision 1.2  2000/01/06 23:01:14  sg
  * Added Accelerator Group creation to TGenericForm

  Revision 1.1.1.1  1999/12/30 21:33:25  sg
  Initial import

}
