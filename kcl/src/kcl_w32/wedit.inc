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
//   Edit widget implementation for Win32
// ===================================================================

procedure TEdit.OnFinishCreation;
begin
  if CreationState = wcsCreationDone then exit;
  FCreationState := wcsCreationInProgress;
     
  CreateWin32(self, 'EDIT', ws_ex_clientedge, WS_CHILD or WS_BORDER or ES_AUTOHSCROLL or ES_AUTOVSCROLL or WS_TABSTOP);
     
  inherited OnFinishCreation;
end;


{
  $Log$
  Revision 1.1  1999/12/30 21:33:55  sg
  Initial revision

}
