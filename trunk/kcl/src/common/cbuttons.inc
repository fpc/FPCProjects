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
//   Common implementation of TxxxButton methods
// ===================================================================

procedure TGenericButton.DoClick;
begin
  if CallEventFilters(TClickEventObj.Create(Self)) then
    if Assigned(FOnClick) then
      FOnClick(Self);
end;


{
  $Log$
  Revision 1.1  2000/01/24 00:14:28  sg
  * First version

}
