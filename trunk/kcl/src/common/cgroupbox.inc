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
//   Common implementation of TCustomGroupBox methods
// ===================================================================

function TCustomGroupBox.GetControlCount: Integer;
begin
  if Assigned(FContent) then
    Result := 1
  else
    Result := 0;
end;

function TCustomGroupBox.GetControls(AIndex: Integer): TControl;
begin
  ASSERT((AIndex = 0) and Assigned(FContent));
  Result := FContent
end;

function TCustomGroupBox.IsRadioGroup: Boolean;
begin
  Result := True;
end;


{
  $Log$
  Revision 1.3  2000/03/19 00:29:31  sg
  * Huge shift towards more VCL compatiblity (NOTE: Many widgets/controls
    are not working correctly at the moment!)

  Revision 1.2  2000/02/22 14:35:29  sg
  * Removed common constructor

  Revision 1.1  2000/02/17 22:33:19  sg
  * First implementation

}
