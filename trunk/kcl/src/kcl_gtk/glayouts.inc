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


procedure TLayout.PutChild(AChild: TWidget; ax, ay: LongInt);
begin
  gtk_fixed_put(PGtkFixed(FHandle), AChild.FHandle, ax, ay);
end;

procedure TLayout.MoveChild(AChild: TWidget; ax, ay: LongInt);
begin
  gtk_widget_set_uposition(AChild.FHandle, ax, ay);
end;

procedure TLayout.SetChildSize(AChild: TWidget; AWidth, AHeight: LongInt);
begin
  AChild.SetSize(AWidth, AHeight);
end;

procedure TLayout.FinishCreation;
begin
  FHandle := gtk_fixed_new;
  inherited FinishCreation;
end;


{
  $Log$
  Revision 1.2  2000/01/24 00:30:55  sg
  * Extended copyright to year 2000...
  * Modifications to reflect the changed definition of TWidgetHandle

  Revision 1.1.1.1  1999/12/30 21:33:25  sg
  Initial import

}
