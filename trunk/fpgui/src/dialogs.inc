{
    $Id$

    fpGUI  -  Free Pascal Graphical User Interface
    Copyright (C) 2000 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    Dialogs implementations

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


// ===================================================================
//   TCustomStandardDialog
// ===================================================================

constructor TCustomStandardDialog.Create(AOwner: TComponent);

  function AddBtn(const AText: String; ADefault: Boolean): TButton;
  begin
    Result := TButton.Create(Self);
    Result.Text := AText;
//    Result.Default := ADefault;
    Result.OnClick := @StdBtnClicked;
    Result.Parent := BtnLayout;
  end;

begin
  inherited Create(AOwner);
  FButtons := [mbOk, mbCancel];
  FBorderWidth := 4;

  MainLayout := TBoxLayout.Create(Self);
  MainLayout.Orientation := Vertical;

  if Assigned(FDlgChild) then
    FDlgChild.Parent := MainLayout;

{!!!:  Separator := TSeparator.Create(Self);
  MainLayout.AddControl(Separator);}

  BtnLayout := TBoxLayout.Create(Self);
  BtnLayout.Orientation := Horizontal;
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

  BtnLayout.Parent := MainLayout;

  TCustomForm(Self).Child := MainLayout;
end;

procedure TCustomStandardDialog.SetButtons(AButtons: TMsgDlgButtons);
begin
  FButtons := AButtons;
end;

procedure TCustomStandardDialog.StdBtnClicked(Sender: TObject);
begin
  Close;
end;


{
  $Log$
  Revision 1.1  2000/12/23 23:20:15  sg
  * First public CVS version...

}
