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
//   Common implementation of TStatusBar methods
// ===================================================================

constructor TStatusBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;


{
  $Log$
  Revision 1.1  2000/02/10 18:51:59  sg
  * First implementation; most methods have been moved from the GTK target
    implementation to this file.

}
