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
//   Common implementation for some menu related stuff
// ===================================================================

// -------------------------------------------------------------------
//   TMenu
// -------------------------------------------------------------------

constructor TMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TList.Create;
end;

destructor TMenu.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;


// -------------------------------------------------------------------
//   TMenuBar
// -------------------------------------------------------------------

constructor TMenuBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TList.Create;
end;

destructor TMenuBar.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;


{
  $Log$
  Revision 1.2  2000/01/24 00:19:54  sg
  * Added item list initalization to TMenu and TMenuBar

  Revision 1.1.1.1  1999/12/30 21:33:12  sg
  Initial import

}
