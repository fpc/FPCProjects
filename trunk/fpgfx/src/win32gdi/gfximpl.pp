{
    $Id$

    fpGFX  -  Free Pascal Graphics Library
    Copyright (C) 2000 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    Default target selection unit for Win32 GDI target

    See the file COPYING, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


unit GFXImpl;

interface

uses GFX_GDI;


type

  TDefCanvas = TGDICanvas;
  TDefScreen = TGDIScreen;
  TDefDisplay = TGDIDisplay;
  TDefWindow = TGDIWindow;


implementation

end.


{
  $Log$
  Revision 1.2  2001/02/09 20:45:33  sg
  * Added TDefCanvas and TDefScreen

  Revision 1.1  2000/10/28 20:30:50  sg
  * First version (NOT compilable at the moment, as the sources haven't been
    adapted to recent interfaces changes yet!)

}
