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
//   TStandardDialog
// -------------------------------------------------------------------

constructor TStandardDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FButtons := [mbOk, mbCancel];
  FBorderWidth := 4;
end;

procedure TStandardDialog.SetButtons(AButtons: TMsgDlgButtons);
begin
  FButtons := AButtons;
end;

procedure TStandardDialog.StdBtnClicked(Sender: TObject);
begin
  Close;
end;

procedure TStandardDialog.OnFinishCreation;

  function AddBtn(AText: String; ADefault: Boolean): TButton;
  begin
    Result := TButton.Create(Self);
    Result.Text := AText;
    Result.Default := ADefault;
    Result.OnClick := @StdBtnClicked;
    BtnLayout.AddControl(Result);
  end;

begin
  MainLayout := TBoxLayout.Create(Self);
  MainLayout.Orientation := boxVert;

  if Assigned(FDlgContent) then
    MainLayout.AddControl(FDlgContent);

{!!!:  Separator := TSeparator.Create(Self);
  MainLayout.AddControl(Separator);}

  BtnLayout := TBoxLayout.Create(Self);
  BtnLayout.Orientation := boxHorz;
  BtnLayout.HorzAlign := horzRight;
  BtnLayout.VertAlign := vertCenter;
  BtnLayout.CanExpandHeight := False;

  if mbYes      in FButtons then AddBtn(mbText_Yes, False);
  if mbNo       in FButtons then AddBtn(mbText_No, False);
  if mbOk       in FButtons then AddBtn(mbText_OK, True);
  if mbCancel   in FButtons then AddBtn(mbText_Cancel, False);
  if mbApply    in FButtons then AddBtn(mbText_Apply, False);
  if mbAbort    in FButtons then AddBtn(mbText_Abort, False);
  if mbRetry    in FButtons then AddBtn(mbText_Retry, False);
  if mbIgnore   in FButtons then AddBtn(mbText_Ignore, False);
  if mbAll      in FButtons then AddBtn(mbText_All, False);
  if mbNoToAll  in FButtons then AddBtn(mbText_NoToAll, False);
  if mbYesToAll in FButtons then AddBtn(mbText_YesToAll, False);
  if mbHelp     in FButtons then AddBtn(mbText_Help, False);

  MainLayout.AddControl(BtnLayout);

  inherited SetContent(MainLayout);
  inherited OnFinishCreation;
end;


// -------------------------------------------------------------------
//   TFileDialog
// -------------------------------------------------------------------

procedure FileDialog_OK(GtkWidget: PGtkWidget; dlg: TFileDialog); cdecl;
begin
  dlg.ResultCode := True;
{!!!:  dlg.FFilename := gtk_file_selection_get_filename(
    PGtkFileSelection(dlg.FHandle));
  gtk_widget_destroy(dlg.FHandle);}
end;

procedure FileDialog_Destroy(GtkWidget: PGtkWidget; dlg: TFileDialog); cdecl;
begin
  gtk_main_quit;
end;

procedure TFileDialog.OnFinishCreation;
begin
  // Special OnFinishCreation handling!
{!!!:  FHandle := gtk_file_selection_new(PChar(dlgText_FileDialog));

  gtk_window_set_modal(PGtkWindow(FHandle), True);

  gtk_signal_connect(PGtkObject(FHandle), 'destroy',
    GTK_SIGNAL_FUNC(@FileDialog_Destroy), Self);

  gtk_signal_connect(
    PGtkObject(PGtkFileSelection(FHandle)^.ok_button),
    'clicked', GTK_SIGNAL_FUNC(@FileDialog_OK), Self);

  gtk_signal_connect_object(
    PGtkObject(PGtkFileSelection(FHandle)^.cancel_button),
    'clicked', GTK_SIGNAL_FUNC(@gtk_widget_destroy), PGtkObject(FHandle));

  FCreationState := wcsCreationDone;}
end;

function TFileDialog.Run: Boolean;
begin
{!!!:  HandleNeeded;
  gtk_widget_show(FHandle);
  FFilename := '';
  gtk_main;
  FHandle := nil;		// The GTK widget has already been destroyed!
  Result := ResultCode;}
end;


{
  $Log$
  Revision 1.7  2000/03/19 00:29:31  sg
  * Huge shift towards more VCL compatiblity (NOTE: Many widgets/controls
    are not working correctly at the moment!)

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
