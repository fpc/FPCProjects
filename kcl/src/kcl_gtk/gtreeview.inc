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
//   TTreeView widget implementation for GTK
// ===================================================================

constructor TTreeView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

procedure TTreeView.OnFinishCreation;
begin
  FHandle := gtk_tree_new;
  inherited OnFinishCreation;
end;


{
  $Log$
  Revision 1.3  2000/02/10 18:48:27  sg
  * Added TTreeView.Create

  Revision 1.2  2000/01/24 00:30:55  sg
  * Extended copyright to year 2000...
  * Modifications to reflect the changed definition of TWidgetHandle

  Revision 1.1.1.1  1999/12/30 21:33:27  sg
  Initial import

}
