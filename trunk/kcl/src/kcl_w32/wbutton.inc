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

// -------------------------------------------------------------------
//   TButton widget implementation for Win32
// -------------------------------------------------------------------


function TButtonWidget_ProcessMessages(widget: TWidget; AMessage, WParam, LParam: Longint): Longint; 
var
   button: TButtonWidget;
begin
  result := 0;
  button := TButtonWidget(widget);
  case AMessage of
    wm_Command:
      if IsSignalUsed(@button.FOnClick) then button.FOnClick(button);     
  end;   
end;

procedure TButton.OnFinishCreation;
begin
  if CreationState = wcsCreationDone then exit;
  FCreationState := wcsCreationInProgress;

  CreateWin32(self, 'BUTTON', 0, ws_Child or ws_TabStop);
  Handle.ProcessMessages := @TButtonWidget_ProcessMessages;

  inherited OnFinishCreation;
end;


// -------------------------------------------------------------------
//   TCheckBox
// -------------------------------------------------------------------

function TCheckBox.GetState: TCheckBoxState;
var
  msgresult: longint;
begin
  msgresult := SendMessage(Handle.Window, bm_GetCheck, 0, 0);
  case msgresult of
    bst_Checked       : result := cbChecked;
    bst_Indeterminate : result := cbGrayed
    else result := cbUnchecked;
  end;
end;

procedure TCheckBox.SetState(AState: TCheckBoxState);
var
  value: word;
begin
  case AState of
    cbChecked: value := bst_Checked;
    cbGrayed : value := bst_Indeterminate
    else       value := bst_Unchecked;
  end;
  PostMessage(Handle.Window, bm_SetCheck, value, 0);
end;

procedure TCheckBox.OnFinishCreation;
begin
  if CreationState = wcsCreationDone then exit;
  FCreationState := wcsCreationInProgress;

  CreateWin32(self, 'BUTTON', 0, ws_Child or ws_TabStop or bs_AutoCheckbox);
  Handle.ProcessMessages := @TButtonWidget_ProcessMessages;

  inherited OnFinishCreation;
end;


{
  $Log$
  Revision 1.1  1999/12/30 21:33:51  sg
  Initial revision

}
