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
//   Common implementation of TCustomListBox methods
// ===================================================================

destructor TCustomListBox.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;


{
  $Log$
  Revision 1.2  2000/02/22 14:36:45  sg
  * Removed common constructor

  Revision 1.1  2000/02/17 22:33:19  sg
  * First implementation

}
