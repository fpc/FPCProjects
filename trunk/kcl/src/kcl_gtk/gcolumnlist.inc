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
//   Column list widget implementation for GTK+
// ===================================================================

constructor TColList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColCount := 1;
end;

procedure TColList.OnFinishCreation;
begin
  Handle.Widget := gtk_clist_new(FColCount);
  inherited OnFinishCreation;
end;


{
  $Log$
  Revision 1.1  1999/12/30 21:33:45  sg
  Initial revision

}
