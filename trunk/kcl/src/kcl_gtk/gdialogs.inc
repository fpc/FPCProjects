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
//   Implementations of dialog box classes of all kind
// ===================================================================

// -------------------------------------------------------------------
//   TCustomDialog
// -------------------------------------------------------------------

procedure TCustomDialog.Ok(AObj: TObject);
begin
end;

procedure TCustomDialog.Cancel(AObj: TObject);
begin
end;

procedure TCustomDialog.OnFinishCreation;
begin
  FHandle := gtk_window_new(GTK_WINDOW_DIALOG);
  inherited OnFinishCreation;
end;

procedure TCustomDialog.DoClose;
begin
  gtk_widget_destroy(FHandle);
  FHandle := nil;
  gtk_main_quit;
end;

constructor TCustomDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

function TCustomDialog.Run: Boolean;
begin
  FinishCreation;
  gtk_main;
  Result := False;
end;


// -------------------------------------------------------------------
//   TStdBtnDialog
// -------------------------------------------------------------------

constructor TStdBtnDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FButtons := [btnOk, btnCancel];
  FBorderWidth := 8;
end;

procedure TStdBtnDialog.SetButtons(AButtons: TStdDlgButtons);
begin
  FButtons := AButtons;
end;

procedure TStdBtnDialog.StdBtnClicked(Sender: TObject);
begin
  FResultWidget := TWidget(Sender);
  Close;
end;

procedure TStdBtnDialog.OnFinishCreation;

  function AddBtn(AText: String; ADefault: Boolean): TButton;
  begin
    Result := TButton.Create(Self);
    Result.Text := AText;
//    Result.Default := ADefault;
    Result.OnClick := @StdBtnClicked;
    BtnLayout.AddWidget(Result);
  end;

begin
  MainLayout := TBoxLayout.Create(Self);
  MainLayout.Orientation := boxVert;
  MainLayout.Spacing := 8;

  if Assigned(FDlgContent) then
    MainLayout.AddWidget(FDlgContent);

  Separator := TSeparator.Create(Self);
  MainLayout.AddWidget(Separator);

  BtnLayout := TBoxLayout.Create(Self);
  BtnLayout.Orientation := boxHorz;
  BtnLayout.HorzAlign := horzRight;
  BtnLayout.VertAlign := vertCenter;
  BtnLayout.CanExpandHeight := False;
  BtnLayout.Spacing := 8;

  if btnOk in FButtons then AddBtn(btnText_OK, True);
  if btnYes in FButtons then AddBtn(btnText_Yes, False);
  if btnNo in FButtons then AddBtn(btnText_No, False);
  if btnCancel in FButtons then AddBtn(btnText_Cancel, False);
  if btnAbort in FButtons then AddBtn(btnText_Abort, False);
  if btnApply in FButtons then AddBtn(btnText_Apply, False);
  if btnHelp in FButtons then AddBtn(btnText_Help, False);

  MainLayout.AddWidget(BtnLayout);

  inherited SetContent(MainLayout);
  inherited OnFinishCreation;
end;


// -------------------------------------------------------------------
//   TFileDialog
// -------------------------------------------------------------------

procedure FileDialog_OK(GtkWidget: PGtkWidget; dlg: TFileDialog); cdecl;
begin
  dlg.ResultCode := True;
  dlg.FFilename := gtk_file_selection_get_filename(
    PGtkFileSelection(dlg.FHandle));
  gtk_widget_destroy(dlg.FHandle);
end;

procedure FileDialog_Destroy(GtkWidget: PGtkWidget; dlg: TFileDialog); cdecl;
begin
  gtk_main_quit;
end;

procedure TFileDialog.OnFinishCreation;
begin
  // Special OnFinishCreation handling!
  FHandle := gtk_file_selection_new(PChar(dlgText_FileDialog));

  gtk_window_set_modal(PGtkWindow(FHandle), True);

  gtk_signal_connect(PGtkObject(FHandle), 'destroy',
    GTK_SIGNAL_FUNC(@FileDialog_Destroy), Self);

  gtk_signal_connect(
    PGtkObject(PGtkFileSelection(FHandle)^.ok_button),
    'clicked', GTK_SIGNAL_FUNC(@FileDialog_OK), Self);

  gtk_signal_connect_object(
    PGtkObject(PGtkFileSelection(FHandle)^.cancel_button),
    'clicked', GTK_SIGNAL_FUNC(@gtk_widget_destroy), PGtkObject(FHandle));

  FCreationState := wcsCreationDone;
end;

function TFileDialog.Run: Boolean;
begin
  FinishCreation;
  gtk_widget_show(FHandle);
  FFilename := '';
  gtk_main;
  FHandle := nil;		// The GTK widget has already been destroyed!
  Result := ResultCode;
end;


{
  $Log$
  Revision 1.6  2000/02/22 14:40:32  sg
  * More layout fixes; minor improvements

  Revision 1.5  2000/02/17 22:27:01  sg
  * Made dialogs working again; TStdBtnDialog is now functional

  Revision 1.4  2000/02/10 18:40:56  sg
  * Small fixes

  Revision 1.3  2000/01/24 00:30:55  sg
  * Extended copyright to year 2000...
  * Modifications to reflect the changed definition of TWidgetHandle

  Revision 1.2  2000/01/05 19:17:46  sg
  * Added 'Abort' button to StdBtnDialog
  * StdBtnDialog now uses resourestrings for the button texts
  * The default caption of TFileDialog is now a resourcestring

  Revision 1.1.1.1  1999/12/30 21:33:45  sg
  Initial import

}
