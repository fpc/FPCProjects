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

//{$APPTYPE GUI}

{$DEFINE kcl_w32}

interface

uses Windows, Classes;

const

  InfiniteSize = 32000;       

type

  ProcessMessagesProc = function(widget: Pointer; AMessage, WParam, LParam: Longint): Longint;

  TWidgetHandle = record
    Window: hWnd;       //windows handle
    ProcessMessages: pointer;
    StdProc: pointer;            
    Tag: Pointer;      
  end;

  TMenuItemHandle = record
    ID: integer;
    Menu: hMenu;
  end;

  TFont_private = record
    Font:  hFont;
    oldFont: hFont;
  end;

  TCanvas_handle = record
    DC: hDC;
    Pen, oldPen: hPen;
    Brush, oldBrush: hBrush;
  end;

  TCanvas_private = hWnd;

  TNotebookPage_private = record
    Notebook: Pointer;
  end;

  TTimerHandle = hWnd;

{$INCLUDE kcl_h.inc}

// ============================================================================

implementation

uses TypInfo, SysUtils, SigSlot;

{$INCLUDE kcl_impl.inc}

var
  _fontDC: HDC;            //desktop dc for gettextwidth/height in TFont
  _systemFont: TFont;      //standard system font (ms sans serif, 8)
  _syscurSizeNS,
  _syscurSizeWE,
  _syscurDefault: hCursor; //system cursors for the splitter-widget

{$INCLUDE cclipboard.inc}

{$INCLUDE wdrawing.inc}
{$INCLUDE wwidget.inc}
{$INCLUDE wapplication.inc}
{$INCLUDE wform.inc}
{$INCLUDE wlayouts.inc}
{$INCLUDE wdialogs.inc}
{$INCLUDE wmenus.inc}
{$INCLUDE wtimer.inc}

// Widgets:
{$INCLUDE wlabel.inc}
{$INCLUDE wbutton.inc}
{$INCLUDE wedit.inc}
{$INCLUDE wscroll.inc}
{$INCLUDE wnotebook.inc}
{$INCLUDE wpaintbox.inc}
{$INCLUDE wstatusbar.inc}
{$INCLUDE wcolumnlist.inc}
{$INCLUDE wsplitter.inc}

end.


{
  $Log$
  Revision 1.1  1999/12/30 21:33:50  sg
  Initial revision

}
