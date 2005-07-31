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
//   Master Include File for KCL Interface
// ===================================================================


{$IFDEF Debug}
{$ASSERTIONS On}
{$ENDIF}

type
 
  TControl = class;
  TWinControl = class;


// Non-Visual Stuff:
{$INCLUDE hkeys.inc}		// Keyboard definitions (keycoded etc.)
{$INCLUDE hevents.inc}		// Definitions of some event types
{$INCLUDE hcomponent.inc}	// TKCLComponent
{$INCLUDE happlication.inc}	// TApplication
{$INCLUDE hclipboard.inc}	// Clipboard class

// Visual Stuff:
{$INCLUDE hgraphics.inc}	// Graphics support (drawing, bitmaps etc.)
{$INCLUDE himagelist.inc}	// TImageList
{$INCLUDE hcommands.inc}	// TCommand, TCommandList
{$INCLUDE hcontrols.inc}	// TControl, TWinControl
{$INCLUDE hlayouts.inc}		// GUI layout classes
{$INCLUDE hforms.inc}		// Forms
{$INCLUDE hdialogs.inc}		// Dialog box support
{$INCLUDE hstdctrls.inc}	// Standard controls
{$INCLUDE hextctrls.inc}	// Extended controls
{$INCLUDE hcomctrls.inc}	// Common controls
{$INCLUDE hmenus.inc}		// Menu classes (menu bars, items, popups...)
{$INCLUDE hgrids.inc}		// Grid widgets


function  ShortcutDisplayName(const AShortCut: String): String;

procedure Register;		// Register all components (-> persistence)

var
  Application: TApplication;


{
  $Log$
  Revision 1.3  2000/03/19 00:29:31  sg
  * Huge shift towards more VCL compatiblity (NOTE: Many widgets/controls
    are not working correctly at the moment!)

  Revision 1.2  2000/01/24 00:01:06  sg
  * Added himagelist and hcommands, renamed hdrawing to hgraphics

  Revision 1.1.1.1  1999/12/30 21:33:22  sg
  Initial import

  Revision 1.1  1999/12/23 17:40:25  sg
  * Added KCL to CVS tree (but without Win32 target yet)

}
