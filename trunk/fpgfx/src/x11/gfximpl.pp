{
    $Id$

    fpGFX  -  Free Pascal Graphics Library
    Copyright (C) 2000 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    Default target selection unit for X11 target

    See the file COPYING, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


unit GFXImpl;

interface

uses GFX_X11;


type

  TDefDisplay = TXDisplay;
  TDefWindow = TXWindow;


implementation

end.


{
  $Log$
  Revision 1.2  2000/10/28 20:27:33  sg
  * Changed handling of offscreen stuff to the concept of Bitmaps and
    Images

  Revision 1.1  2000/08/04 21:05:53  sg
  * First version in CVS

}
