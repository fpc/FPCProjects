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
//   Dialog implementations
// ===================================================================

constructor TDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

procedure TDialog.OnFinishCreation;
begin
  inherited OnFinishCreation;
end;

procedure TDialog.Ok(AObj: TObject);
begin
end;

procedure TDialog.Cancel(AObj: TObject);
begin
end;

function TDialog.Run: Boolean;
begin
  Show;

  Result := False;
end;


// -------------------------------------------------------------------
//   TStdBtnDialog
// -------------------------------------------------------------------


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



                      
procedure TFileDialog.OnFinishCreation;
begin
end;

function TFileDialog.Run: Boolean;
var o: OPENFILENAME;
    c: array[0..512] of char;
begin
  StrPCopy(@c, FFilename+#0);
  with o do
  begin
    lStructSize := SizeOf(o);
    if parent<>nil then hWndOwner := TWidget(parent).Handle.Window
    else hWndOwner := GetActiveWindow;
    hInstance :=  system.MainInstance; 
    lpstrFilter := nil;
    lpstrCustomFilter := PChar('All Files (*.*)'+#0+'*.*'+#0+#0);
    nMaxCustFilter := StrLen(lpstrCustomFilter);
    nFilterIndex := 0;
    lpstrFile := PChar(@c);
    nMaxFile := 512;
    lpstrFileTitle := nil;
    nMaxFileTitle := StrLen(lpstrFileTitle);
    lpstrInitialDir := nil;
    lpstrTitle := nil;
    Flags := OFN_HIDEREADONLY;
    nFileOffset := 0;
    nFileExtension := 0;
    lpstrDefExt := nil;
    lCustData := 0;          
    lpfnHook := nil;
    lpTemplateName := nil;   
  end;
  result := GetOpenFileName(@o);
  if result then
     FFilename := StrPas(@c);
end;


{
  $Log$
  Revision 1.1  1999/12/30 21:33:55  sg
  Initial revision

}
