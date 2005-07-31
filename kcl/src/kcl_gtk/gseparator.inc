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
//   Separator widget implementation for GTK+
// ===================================================================

procedure TSeparator.OnFinishCreation;
begin
  FHandle := gtk_hseparator_new;
  inherited OnFinishCreation;
end;

procedure TSeparator.DoRecalcLayout;
begin
  MaxH := DefH;
end;


{
  $Log$
  Revision 1.3  2000/02/24 13:39:32  sg
  * More layout fixes...

  Revision 1.2  2000/02/22 14:40:32  sg
  * More layout fixes; minor improvements

  Revision 1.1  2000/02/17 22:32:24  sg
  * First implementation

}
