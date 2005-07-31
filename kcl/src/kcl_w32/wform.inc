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
// Form classes implementations for Win32
// ===================================================================

function TForm_ProcessMessages(Widget: Pointer; AMessage, WParam, LParam: Longint): Longint; 
var
  form: TForm;
begin
  result := 0;
  form := TForm(widget);
  case AMessage of
    wm_Destroy:
    begin
      if not Form.CanClose then exit;
      if Assigned(Form.OnClose) then Form.OnClose(Form);
      Application.RemoveForm(Form);
    end;
  end;
end;


constructor TGenericForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // Set a default size
  FWidth := 100;
  FHeight := 100;
end;

procedure TForm.SetContent(AContent: TWidget);
begin
  FContent := AContent;
  FContent.FParent := Self;

  if sizeFixed in FSizeFlags then
    AContent.SizeFlags := AContent.SizeFlags + [sizeFixed];
        
end;

procedure TForm.OnFinishCreation;
begin
  if CreationState = wcsCreationDone then exit;
  FCreationState := wcsCreationInProgress;
 
  CreateWin32(self, 'KCLWIDGET', WS_EX_CONTROLPARENT, ws_OverlappedWindow);
  Handle.ProcessMessages := @TForm_ProcessMessages;

  if FContent<>nil then FContent.FinishCreation;

  inherited OnFinishCreation;

  writeln('form finish creation');
end;

procedure TForm.OnSetSize;
var 
  r: Windows.Rect;
begin
  inherited OnSetSize;
  if FContent<>nil then 
  begin
    GetClientRect(Handle.Window, @r);
    FContent.SetSize(r.right, r.bottom);
  end;          
end;

procedure TForm.OnSizeChanged;
var 
  r: Windows.Rect;
begin
  inherited OnSizeChanged;
  if FContent<>nil then 
  begin
    GetClientRect(Handle.Window, @r);
    FContent.SetSize(r.right, r.bottom);
  end;      
end;

procedure TForm.Show;
begin
  FinishCreation;
end;

procedure TForm.Close;
begin
  // ###
end;


{
  $Log$
  Revision 1.1  1999/12/30 21:33:58  sg
  Initial revision

}
