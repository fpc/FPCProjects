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
//   Master Include File for all KCL Implementations
// ===================================================================

{$INCLUDE cevents.inc}
{$INCLUDE ccomponent.inc}
{$INCLUDE ctimer.inc}
{$INCLUDE cgraphics.inc}
{$INCLUDE ccommands.inc}
{$INCLUDE cwidget.inc}
{$INCLUDE cform.inc}
{$INCLUDE clayouts.inc}
{$INCLUDE cmenus.inc}
{$INCLUDE cbuttons.inc}
{$INCLUDE cpaintbox.inc}
{$INCLUDE cnotebook.inc}
{$INCLUDE cstatusbar.inc}
{$INCLUDE csplitter.inc}
{$INCLUDE ctoolbar.inc}


procedure Register;
var
  InvComponents: array[0..1] of TComponentClass;
  BaseComponents: array[0..17] of TComponentClass;
  LayoutComponents: array[0..3] of TComponentClass;
  DialogComponents: array[0..2] of TComponentClass;
begin
  InvComponents[0] := TForm;
  InvComponents[1] := TMenuItem;
  RegisterNoIcon(InvComponents);

  BaseComponents[0]  := TLabel;
  BaseComponents[1]  := TEdit;
  BaseComponents[2]  := TButton;
  BaseComponents[3]  := TImageButton;
  BaseComponents[4]  := TListBox;
  BaseComponents[5]  := TColList;
  BaseComponents[6]  := TTreeView;
  BaseComponents[7]  := THorzScrollBar;
  BaseComponents[8]  := TVertScrollBar;
  BaseComponents[9]  := TScrollBox;
  BaseComponents[10]  := TPaintBox;
  BaseComponents[11] := TNotebook;
  BaseComponents[12] := TToolBar;
  BaseComponents[13] := TStatusBar;
  BaseComponents[14] := TSplitter;
  BaseComponents[15] := TMenuBar;
  BaseComponents[16] := TMenu;
  BaseComponents[17] := TToolBar;
  RegisterComponents('Base Widgets', BaseComponents);

  LayoutComponents[0]  := TFixedLayout;
  LayoutComponents[1]  := TDockingLayout;
  LayoutComponents[2]  := TGridLayout;
  LayoutComponents[3]  := TBoxLayout;
  RegisterComponents('Layouts', LayoutComponents);

  DialogComponents[0]  := TDialog;
  DialogComponents[1]  := TStdBtnDialog;
  DialogComponents[2]  := TFileDialog;
  RegisterComponents('Dialogs', DialogComponents);
end;


{
  $Log$
  Revision 1.4  2000/02/10 18:26:51  sg
  * Added more include files

  Revision 1.3  2000/01/24 00:11:44  sg
  * Added more widgets to the component registration function

  Revision 1.2  2000/01/05 19:09:15  sg
  * Added cpaintbox.inc to list of included files

  Revision 1.1.1.1  1999/12/30 21:33:12  sg
  Initial import

  Revision 1.1  1999/12/23 17:40:25  sg
  * Added KCL to CVS tree (but without Win32 target yet)

}
