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

// public methods

constructor TCustomStandardDialog.Create(AOwner: TComponent);

  function AddBtn(const AText: String; ADefault: Boolean): TButton;
  begin
    Result := TButton.Create(Self);
    Result.Text := AText;
    // Result.Default := ADefault;
    Result.OnClick := @StdBtnClicked;
    Result.Parent := BtnLayout;
  end;

begin
  inherited Create(AOwner);
  FButtons := [mbOk, mbCancel];
  FBorderWidth := 4;

  MainLayout := TBoxLayout.Create(Self);
  MainLayout.Orientation := Vertical;
  MainLayout.SetEmbeddedParent(Self);

  Separator := TSeparator.Create(Self);
  Separator.Parent := MainLayout;

  BtnLayout := TBoxLayout.Create(Self);
  BtnLayout.Orientation := Horizontal;
  BtnLayout.HorzAlign := horzRight;
  BtnLayout.VertAlign := vertCenter;
  BtnLayout.CanExpandHeight := False;
  BtnLayout.Parent := MainLayout;

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

end;


// protected methods

function TCustomStandardDialog.ProcessEvent(Event: TEventObj): Boolean;
begin
  if Event.InheritsFrom(TResizeEventObj) then
  begin
    if Assigned(Child) then
      Child.SetBounds(BorderWidth, BorderWidth, Width - 2 * BorderWidth,
        Height - MainLayout.DefH - 2 * BorderWidth);
    MainLayout.SetBounds(BorderWidth, Height - MainLayout.DefH - BorderWidth,
      Width - 2 * BorderWidth, MainLayout.DefH - BorderWidth);
    Result := False;
  end else
    Result := MainLayout.ProcessEvent(Event) or inherited ProcessEvent(Event);
end;

function TCustomStandardDialog.DistributeEvent(Event: TEventObj): Boolean;
begin
  Result := Event.SendToChild(MainLayout) or inherited DistributeEvent(Event);
end;

procedure TCustomStandardDialog.EvRecalcLayout;
begin
  if Assigned(Child) then
  begin
    MinW := Child.MinW + 2 * BorderWidth;
    MinH := Child.MinH + 2 * BorderWidth;
    DefW := Child.DefW + 2 * BorderWidth;
    DefH := Child.DefH + 2 * BorderWidth;
    MaxW := Min(InfiniteSize, Child.MaxW + 2 * BorderWidth);
    MaxH := Min(InfiniteSize, Child.MaxH + 2 * BorderWidth);
  end;

  MinW := Max(MinW, MainLayout.MinW + 2 * BorderWidth);
  Inc(MinH, MainLayout.DefH + BorderWidth);
  DefW := Max(DefW, MainLayout.DefW + 2 * BorderWidth);
  DefH := Min(InfiniteSize, DefH);
  MaxW := Min(MaxW, MainLayout.MaxW + 2 * BorderWidth);
  MaxH := Min(InfiniteSize, MaxH + MainLayout.DefH);

  ApplyNewLayout;
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
  Revision 1.2  2001/01/11 23:30:11  sg
  *** empty log message ***

  Revision 1.1  2000/12/23 23:20:15  sg
  * First public CVS version...

}
