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
//   Layout implementation for Win32
// ===================================================================


procedure TLayout.PutChild(AChild: TWidget; ax, ay: Longint);
begin
  SetWindowPos(AChild.Handle.Window, -1, ax, ay, 0, 0, swp_NoZOrder or swp_NoSize);
end;

procedure TLayout.MoveChild(AChild: TWidget; ax, ay: Longint);
begin
  SetWindowPos(AChild.Handle.Window, -1, ax, ay, 0, 0, swp_NoZOrder or swp_NoSize);
end;

procedure TLayout.SetChildSize(AChild: TWidget; AWidth, AHeight: Longint);
begin
  AChild.SetSize(AWidth, AHeight);
end;

procedure TLayout.FinishCreation;
begin
  FCreationState := wcsCreationInProgress;
  CreateWin32(self, 'KCLWIDGET', WS_EX_CONTROLPARENT, WS_CHILD or WS_TABSTOP);
  inherited FinishCreation;
end;


{
  $Log$
  Revision 1.1  1999/12/30 21:33:58  sg
  Initial revision

}
