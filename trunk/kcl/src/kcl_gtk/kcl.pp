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


unit KCL;

{$MODE objfpc}
{$H+}  // Use AnsiStrings

{$DEFINE kcl_gtk}

{$IFDEF Debug}
{$DEFINE DebugEvents}
{$ENDIF}

// Currently required, until direct X clipboard access is implemented
{$DEFINE KCL_USEINTERNALCLIPBOARD}


interface

uses classes, GDK, GTK;

const

  InfiniteSize = 16383;

type

  TTimerHandle = LongWord;

  TWidgetHandle = record
      case Byte of
        0: (Form: PGtkWindow);
        1: (Widget: PGtkWidget);
	2: (Layout: PGtkLayout);
      end;

  TMenuItemHandle = record
    item, submenu: PGtkWidget;
  end;

  THorzScrollBar_private = PGtkAdjustment;
  TVertScrollBar_private = PGtkAdjustment;

  TFont_private = record
    GdkFont: PGdkFont;
  end;

  TCanvas_handle = PGdkWindow;
  TCanvas_private = record
    GC: PGdkGC;
  end;

  TNotebookPage_private = record
    LabelWidget: PGtkLabel;
  end;

{$INCLUDE h_master.inc}

// ============================================================================

implementation

uses SysUtils, TypInfo;

var
  gToolTips: PGtkToolTips;


{$INCLUDE c_master.inc}
{$INCLUDE cclipboard.inc}

{$INCLUDE gapplication.inc}
{$INCLUDE gtimer.inc}
{$INCLUDE gdrawing.inc}
{$INCLUDE gform.inc}
{$INCLUDE glayouts.inc}
{$INCLUDE gdialogs.inc}
{$INCLUDE gmenus.inc}

// Widgets:
{$INCLUDE gwidget.inc}
{$INCLUDE glabel.inc}
{$INCLUDE gedit.inc}
{$INCLUDE gbutton.inc}
{$INCLUDE gcolumnlist.inc}
{$INCLUDE gtreeview.inc}
{$INCLUDE gscroll.inc}
{$INCLUDE gpaintbox.inc}
{$INCLUDE gnotebook.inc}
{$INCLUDE gstatusbar.inc}
{$INCLUDE gsplitter.inc}


begin
  Application := TApplication.Create(nil);
//  ASSERT(False);
end.


{
  $Log$
  Revision 1.1  1999/12/30 21:33:33  sg
  Initial revision

}
