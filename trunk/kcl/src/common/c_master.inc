{
    $Id$

    KCL  -  Kassandra Component Library
    Copyright (C) 1999  by the KCL team
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

type

  PByteArray = ^TByteArray;
  TByteArray = array[LongWord] of Byte;

{$INCLUDE cevents.inc}
{$INCLUDE ccomponent.inc}
{$INCLUDE ctimer.inc}
{$INCLUDE cguibase.inc}
{$INCLUDE clayouts.inc}
{$INCLUDE cmenus.inc}


procedure Register;
var
  InvComponents: array[0..1] of TComponentClass;
  BaseComponents: array[0..13] of TComponentClass;
  LayoutComponents: array[0..3] of TComponentClass;
  DialogComponents: array[0..2] of TComponentClass;
begin
  InvComponents[0] := TForm;
  InvComponents[1] := TMenuItem;
  RegisterNoIcon(InvComponents);

  BaseComponents[0]  := TLabel;
  BaseComponents[1]  := TEdit;
  BaseComponents[2]  := TButton;
  BaseComponents[3]  := TColList;
  BaseComponents[4]  := TTreeView;
  BaseComponents[5]  := THorzScrollBar;
  BaseComponents[6]  := TVertScrollBar;
  BaseComponents[7]  := TScrollBox;
  BaseComponents[8]  := TPaintBox;
  BaseComponents[9]  := TNotebook;
  BaseComponents[10] := TStatusBar;
  BaseComponents[11] := TSplitter;
  BaseComponents[12] := TMenuBar;
  BaseComponents[13] := TMenu;
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
  Revision 1.1  1999/12/30 21:33:12  sg
  Initial revision

  Revision 1.1  1999/12/23 17:40:25  sg
  * Added KCL to CVS tree (but without Win32 target yet)

}
