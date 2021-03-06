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


unit KCL;

{$MODE objfpc}
{$H+}  // Use AnsiStrings

{$DEFINE kcl_gtk}

// Currently required, until direct X clipboard access is implemented
{$DEFINE KCL_USEINTERNALCLIPBOARD}


interface

uses SysUtils, Classes, GLib, GDK, GTK;

const

  InfiniteSize = 16383;

type

  TTimerHandle = LongWord;

  TControlCanvas = class;

  PImageListItem = ^TImageListItem;
  TImageListItem = record
    ImageCanvas: TControlCanvas;
    Mask: PGdkPixmap;
  end;

  TImageList_private = TList;	// List of elements of type PImageListItem

  TWinControlHandle = PGtkWidget;

  TApplication_private = record
    IdleHandle: Integer;
  end;

  TGenericForm_private = record
    AccelGroup: PGtkAccelGroup;
  end;

  TMenuItemHandle = PGtkWidget;
  TMenuItem_private = record
    Item, SubMenu, Packer, Pixmap: PGtkWidget;
    AccelGroup: PGtkAccelGroup;
    TextLabel, AccelLabel: PGtkWidget;
  end;

  TMenu_private = record
    TearoffItem: PGtkWidget;
  end;

  THorzScrollBar_private = PGtkAdjustment;
  TVertScrollBar_private = PGtkAdjustment;

  TControlCanvasHandle = PGdkDrawable;
  TControlCanvas_private = record
    GC: PGdkGC;			// The underlying graphics context
    GdkFont: PGdkFont;		// The currently set GDK font
    FontManagerUsed: Boolean;
    PenX, PenY: Integer;	// Last pen position (for MoveTo/LineTo)
  end;

  TBitmap_private = record
    Data, ConvData: Pointer;
  end;

  TCustomListBox_private = record
    list: PGtkWidget;	// the 'real' listbox widget. FHandle is a GtkScrolledWindow!
  end;

  TScrollBox_private = record
    viewport: PGtkWidget;
  end;

  TScrollers_private = record
    frame: PGtkWidget;				// PGtkFrame
    HorzScrollbar, VertScrollbar: PGtkWidget;	// PGtkHScrollbar, PGtkVScrollbar
  end;

  TPage_private = record
    LabelWidget: PGtkWidget;
    BorderWidget: PGtkWidget;
  end;

  TToolButton_private = record
    Widget: PGtkWidget;
  end;

  TToolBar_private = record
    ToolBar: PGtkToolBar;
  end;

{$INCLUDE h_master.inc}

// ============================================================================

implementation

uses TypInfo;

type

  TPixmapCanvas = class(TControlCanvas)
  public
    constructor Create(GdkWindow: PGdkWindow; w, h, ColorDepth: Integer);
  end;


var

  gToolTips: PGtkToolTips;
  gBitmapWnd: PGtkWidget;
  gBitmapGC: PGdkGC;
  gGdkRGBInitialized: Boolean;



function ReplaceStr(const s, OldStr, NewStr: String): String;
var
  i: Integer;
begin
  Result := s;
  i := Pos(OldStr, s);
  if i = 0 then exit;
  Result := Copy(s, 1, i - 1) + NewStr + Copy(s, i + Length(OldStr), Length(s));
end;

function ShortcutDisplayName(const AShortCut: String): String;
var
  accel: LongWord;
  mods: TGdkModifierType;
begin
  gtk_accelerator_parse(PChar(AShortCut), @accel, @mods);
  Result := Trim(UpperCase(gtk_accelerator_name(accel, mods)));
  Result := ReplaceStr(Result, '<SHIFT>', 'Shift+');
  Result := ReplaceStr(Result, '<CONTROL>', 'Ctrl+');
  Result := ReplaceStr(Result, '<ALT>', 'Alt+');
end;


procedure BitmapWndNeeded;
begin
  if not Assigned(gBitmapWnd) then begin
    gBitmapWnd := gtk_window_new(GTK_WINDOW_POPUP);
    gtk_widget_set_usize(gBitmapWnd, 0, 0);
    gtk_widget_show(gBitmapWnd);
    gtk_widget_hide(gBitmapWnd);
    gBitmapGC := gdk_gc_new(gBitmapWnd^.window);
  end;
end;

procedure GdkRGBNeeded;
begin
  if not gGdkRGBInitialized then begin
    gdk_rgb_init;
    gGdkRGBInitialized := True;
  end;
end;


// Replace all "&" in AmpersandStr with "_" (the underline character in GTK)
function ConvertUnderlines(const AmpersandStr: String): String;
var
  i: Integer;
begin
  SetLength(Result, 0);
  for i := 1 to Length(AmpersandStr) do
    case AmpersandStr[i] of
      '&': Result := Result + '_';
      '_': Result := Result + '__';
      else Result := Result + AmpersandStr[i];
    end;
end;


{$INCLUDE c_master.inc}
{$INCLUDE cclipboard.inc}

{$INCLUDE gapplication.inc}
{$INCLUDE gtimer.inc}
{$INCLUDE ggraphics.inc}
{$INCLUDE gform.inc}
{$INCLUDE glayouts.inc}
{$INCLUDE gdialogs.inc}
{$INCLUDE gmenus.inc}
{$INCLUDE gimagelist.inc}

// Controls:
{$INCLUDE gcontrols.inc}
{$INCLUDE glabel.inc}
{$INCLUDE gedit.inc}
//{$INCLUDE gseparator.inc}
{$INCLUDE gbuttons.inc}
{$INCLUDE ggroupbox.inc}
{$INCLUDE glistbox.inc}
{$INCLUDE gcombobox.inc}
//{$INCLUDE gcolumnlist.inc}
{$INCLUDE gtreeview.inc}
//{$INCLUDE gscroll.inc}
//{$INCLUDE gpaintbox.inc}
//{$INCLUDE gnotebook.inc}
{$INCLUDE gstatusbar.inc}
{$INCLUDE gsplitter.inc}
{$INCLUDE gtoolbar.inc}


initialization
  Application := TApplication.Create(nil);
  Application.Name := 'Application';
finalization
  if Assigned(GlobalClipboard) then
    GlobalClipboard.Free;
  Application.Free;
end.


{
  $Log$
  Revision 1.10  2000/03/19 00:29:31  sg
  * Huge shift towards more VCL compatiblity (NOTE: Many widgets/controls
    are not working correctly at the moment!)

  Revision 1.9  2000/02/19 19:09:00  sg
  * Fixed layouting bugs; minor improvements

  Revision 1.8  2000/02/18 18:30:15  sg
  * Added TScrollers_private

  Revision 1.7  2000/02/17 22:31:37  sg
  * Added gseparator.inc, gcombobox.inc, ggroupbox.inc

  Revision 1.6  2000/02/10 18:49:22  sg
  * The global clipboard object is now freed on KCL exit

  Revision 1.5  2000/01/24 00:29:29  sg
  * Restructured some of the *_private records
  * TWidgetHandle is now a simple PGtkWidget
  * Added some helper functions

  Revision 1.4  2000/01/10 09:54:11  peter
    * listbox added

  Revision 1.3  2000/01/06 23:04:11  sg
  * Added TGenericForm_private; its only use until now is the support of
    an attached AccelGroup (for the underlined menu accelerators; full
    accelerator support (will be added later) will need this, too.

  Revision 1.2  2000/01/05 19:21:01  sg
  * The symbol "DebugEvent" isn't defined automatically anymore in DEBUG mode

  Revision 1.1.1.1  1999/12/30 21:33:33  sg
  Initial import

}
