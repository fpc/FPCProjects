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
//   Implementations of dialog box classes of all kind
// ===================================================================

// -------------------------------------------------------------------
//   TDialog
// -------------------------------------------------------------------

constructor TDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

procedure TDialog.Ok(AObj: TObject);
begin
end;

procedure TDialog.Cancel(AObj: TObject);
begin
end;

procedure TDialog.OnFinishCreation;
begin
  Handle.Form := PGtkWindow(gtk_window_new(GTK_WINDOW_DIALOG));

  inherited OnFinishCreation;

{  if Assigned(FContent) then begin
    if sizeFixed in FSizeFlags then
      FContent.SizeFlags := FContent.SizeFlags + [sizeFixed];
    FContent.FinishCreation;
    if Assigned(FContent.Handle.Widget) then
      gtk_container_add(PGtkContainer(Handle.Form), FContent.Handle.Widget);
    if sizeAuto in FSizeFlags then begin
      // WriteLn('Setting dialog size to ', FContent.FWidth, 'x', FContent.FHeight);
      SetSize(FContent.FWidth, FContent.FHeight);
    end;
  end;

  gtk_window_set_title(Handle.Form, PChar(FText));}
end;


function TDialog.Run: Boolean;
begin
  FinishCreation;
  Result := False;
end;


// -------------------------------------------------------
//   TStdBtnDialog
// -------------------------------------------------------


constructor TStdBtnDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FButtons := [btnOk];
end;

procedure TStdBtnDialog.SetButtons(AButtons: TStdDlgButtons);
begin
  Buttons := AButtons;
end;

procedure TStdBtnDialog.SetContent(AContent: TWidget);
begin
  RealContent := AContent;
  AContent.SetDefaultSize(100, 100);
end;

procedure TStdBtnDialog.OnFinishCreation;

  procedure AddBtn(AText: String);
  var
    btn: TButton;
  begin
    btn := TButton.Create(Self);
    btn.Text := AText;
    BtnLayout.AddWidget(btn);
  end;

begin
  MainLayout := TDockingLayout.Create(Self);
  MainLayout.AddWidget(RealContent, dmClient);

  BtnLayout := TBoxLayout.Create(Self);
  BtnLayout.Orientation := boxHorz;
  BtnLayout.HorzAlign := horzRight;

  if btnOk in FButtons then AddBtn('OK');
  if btnYes in FButtons then AddBtn('Yes');
  if btnNo in FButtons then AddBtn('No');
  if btnCancel in FButtons then AddBtn('Cancel');
  if btnApply in FButtons then AddBtn('Apply');
  if btnHelp in FButtons then AddBtn('Help');

  MainLayout.AddWidget(BtnLayout, dmBottom);
  inherited SetContent(MainLayout);
  inherited OnFinishCreation;
end;


// -------------------------------------------------------
//   TFileDialog
// -------------------------------------------------------

procedure FileDialog_OK(GtkWidget: PGtkWidget; dlg: TFileDialog); cdecl;
begin
  dlg.ResultCode := True;
  dlg.FFilename := gtk_file_selection_get_filename(
    PGtkFileSelection(dlg.Handle.Widget));
  gtk_widget_destroy(dlg.Handle.Widget);
end;

procedure FileDialog_Destroy(GtkWidget: PGtkWidget; dlg: TFileDialog); cdecl;
begin
  gtk_main_quit;
end;

procedure TFileDialog.OnFinishCreation;
begin
  // Special OnFinishCreation handling!
  Handle.Widget := gtk_file_selection_new('File Selection');

  gtk_window_set_modal(PGtkWindow(Handle.Widget), True);

  gtk_signal_connect(PGtkObject(Handle.Widget), 'destroy',
    GTK_SIGNAL_FUNC(@FileDialog_Destroy), Self);

  gtk_signal_connect(
    PGtkObject(PGtkFileSelection(Handle.Widget)^.ok_button),
    'clicked', GTK_SIGNAL_FUNC(@FileDialog_OK), Self);

  gtk_signal_connect_object(
    PGtkObject(PGtkFileSelection(Handle.Widget)^.cancel_button),
    'clicked', GTK_SIGNAL_FUNC(@gtk_widget_destroy), PGtkObject(Handle.Widget));

  FCreationState := wcsCreationDone;
end;

function TFileDialog.Run: Boolean;
begin
  FinishCreation;
  gtk_widget_show(Handle.Widget);
  FFilename := '';
  gtk_main;
  Result := ResultCode;
end;


{
  $Log$
  Revision 1.1  1999/12/30 21:33:45  sg
  Initial revision

}
