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
//   Graphics support implementation for GTK+
// ===================================================================

// -------------------------------------------------------------------
//   TCanvas and derived classes
// -------------------------------------------------------------------

constructor TCanvas.Create;
begin
  inherited Create;
end;


type

  TGdkCanvas = class(TCanvas)
    constructor Create(GdkWindow: PGdkWindow);
    constructor Create(AWidget: TWidget);
    destructor  Destroy; override;
  end;

  TGdkExposeCanvas = class(TGdkCanvas)
    constructor Create(AWidget: TWidget);
  end;

  TGdkPixmapCanvas = class(TGdkCanvas)
    pixmap: PGdkPixmap;		// Same value as FHandle
    constructor Create(GdkWindow: PGdkWindow; w, h, ColorDepth: Integer);
    destructor Destroy; override;
  end;


constructor TGdkCanvas.Create(GdkWindow: PGdkWindow);
begin
  FHandle := GdkWindow;
  priv.GC := gdk_gc_new(FHandle);
end;

destructor TGdkCanvas.Destroy;
begin
  gdk_gc_destroy(priv.GC);
  inherited Destroy;
end;


constructor TGdkCanvas.Create(AWidget: TWidget);
begin
  FHandle := gtk_widget_get_parent_window(AWidget.FHandle);
  priv.GC := gdk_gc_new(FHandle);
end;


constructor TGdkExposeCanvas.Create(AWidget: TWidget);
begin
  FHandle := AWidget.FHandle^.window;
  priv.GC := gdk_gc_new(FHandle);
  gdk_gc_copy(priv.GC, PGtkStyle(AWidget.FHandle^.thestyle)^.
    fg_gc[AWidget.FHandle^.state]);
end;


constructor TGdkPixmapCanvas.Create(GdkWindow: PGdkWindow;
  w, h, ColorDepth: Integer);
begin
  pixmap := gdk_pixmap_new(GdkWindow, w, h, ColorDepth);
  inherited Create(pixmap);
end;

destructor TGdkPixmapCanvas.Destroy;
begin
  gdk_pixmap_unref(pixmap);
  inherited Destroy;
end;


procedure TCanvas.SetColor(AColor: TColor);
var
  c: TGdkColor;
begin
  if AColor <> FColor then begin
    if Assigned(priv.gc) then begin
      c.pixel := 0;
      c.red   := (((AColor shr 16) and 255) * 65535) div 255;
      c.green := (((AColor shr 8) and 255) * 65535) div 255;
      c.blue  := ((AColor and 255) * 65535) div 255;
      gdk_colormap_alloc_color(gdk_colormap_get_system, @c, False, True);
      gdk_gc_set_foreground(priv.gc, @c);
    end;
    FColor := AColor;
  end;    
end;

procedure TCanvas.SetFont(AFont: TFont);
begin
  FFont := AFont;
end;

procedure TCanvas.Line(x1, y1, x2, y2: Integer);
begin
  gdk_draw_line(PGdkDrawable(FHandle), priv.GC, x1, y1, x2, y2);
end;

procedure TCanvas.FillRect(const rect: TRect);
begin
  gdk_draw_rectangle(PGdkDrawable(FHandle), priv.GC, 1,
    rect.Left, rect.Top, rect.Right - rect.Left, rect.Bottom - rect.Top);
end;

procedure TCanvas.FillRect(ax, ay, aw, ah: Integer);
begin
  gdk_draw_rectangle(PGdkDrawable(FHandle), priv.GC, 1, ax, ay, aw, ah);
end;

procedure TCanvas.Text(x, y: Integer; const AText: String);
var
  ReleaseFont: Boolean;
begin
  if AText = '' then exit;

  if Assigned(FFont) then
    ReleaseFont := False
  else begin
    ReleaseFont := True;
    FFont := TFont.Create;
  end;

  FFont.Allocate;
  gdk_draw_text(PGdkDrawable(FHandle), FFont.Handle.GdkFont, priv.GC,
    x, y + Trunc(FFont.FHeight), PChar(AText), Length(AText));

  if ReleaseFont then begin
    FFont.Free;
    FFont := nil;
  end;
end;

procedure TCanvas.CopyRect(dx, dy, dw, dh, sx, sy: Integer);
begin
  gdk_window_copy_area(FHandle, priv.GC, dx, dy, FHandle, sx, sy, dw, dh);
end;

procedure TCanvas.DrawBitmap(ADestX, ADestY: Integer; ABitmap: TBitmap;
  ASourceX, ASourceY, ASourceW, ASourceH: Integer);
var
  p: Pointer;
begin
  ASSERT(Assigned(ABitmap.FData));
  GdkRGBNeeded;
  if ASourceX + ASourceW > ABitmap.FWidth then
    ASourceW := ABitmap.FWidth - ASourceX;
  if ASourceY + ASourceH > ABitmap.FHeight then
    ASourceH := ABitmap.FHeight - ASourceY;
  if (ASourceW <= 0) or (ASourceH <= 0) then exit;

  case ABitmap.FBitsPerPixel of
    // 1, 4, 8: Draw indexed ###
    24: begin
      // ### COMPILER BUG!!! The following 3 lines cannot be written as a simple addition!!!
      p := ABitmap.FData;
      Inc(p, ASourceY * ABitmap.FStride);
      Inc(p, ASourceX * 3);
      gdk_draw_rgb_image(PGdkDrawable(FHandle), priv.GC,
        ADestX, ADestY, ASourceW, ASourceH,
	GDK_RGB_DITHER_NORMAL,
	p,
	ABitmap.FStride);
      end;
    // 32: ###
    else ASSERT(False);
  end;
end;


// -------------------------------------------------------------------
//   TFont
// -------------------------------------------------------------------

constructor TFont.Create;
begin
  Modified := True;
  FHeight := 12;
end;

destructor TFont.Destroy;
begin
  if Assigned(Handle.GdkFont) then
    gdk_font_unref(Handle.GdkFont);
  inherited Destroy;
end;

procedure TFont.FSetFontName(const AFontName: String);
begin
  if AFontName <> FFontName then begin
    FFontName := AFontName;
    Modified := True;
  end;
end;

procedure TFont.FSetWidth(AWidth: Single);
begin
  if AWidth <> FWidth then begin
    FWidth := AWidth;
    Modified := True;
  end;
end;

procedure TFont.FSetHeight(AHeight: Single);
begin
  if AHeight <> FHeight then begin
    FHeight := AHeight;
    Modified := True;
  end;
end;

procedure TFont.FSetBold(AIsBold: Boolean);
begin
  if AIsBold <> FBold then begin
    FBold := AIsBold;
    Modified := True;
  end;
end;

procedure TFont.FSetItalics(AIsItalics: Boolean);
begin
  if AIsItalics <> FItalics then begin
    FItalics := AIsItalics;
    Modified := True;
  end;
end;

procedure TFont.Allocate;
var
  lfd: String;    // Logical font descriptor
begin
  if not Modified then exit;
  lfd := '-*-';
  if FFontName = '' then lfd := lfd + '*'
  else lfd := lfd + FFontName;
  lfd := lfd + '-';
  if FBold then lfd := lfd + 'bold'
  else lfd := lfd + 'medium';
  lfd := lfd + '-';
  if FItalics then lfd := lfd + 'i'
  else lfd := lfd + 'r';
  lfd := lfd + '-normal--' + IntToStr(Trunc(FHeight)) + '-*-*-*-*-*-iso8859-1';
  Handle.GdkFont := gdk_font_load(PChar(lfd));
  Modified := False;
end;

function TFont.GetTextWidth(const s: String): Integer;
begin
  Allocate;
  if Length(s) = 1 then
    Result := gdk_char_width(Handle.GdkFont, s[1])
  else
    Result := gdk_text_width(Handle.GdkFont, PChar(s), Length(s));
end;

function TFont.GetTextHeight(const s: String): Integer;
begin
  Allocate;
  if Length(s) = 1 then
    Result := gdk_char_height(Handle.GdkFont, s[1])
  else
    Result := gdk_text_height(Handle.GdkFont, PChar(s), Length(s));
end;


{
  $Log$
  Revision 1.4  2000/02/19 19:09:00  sg
  * Fixed layouting bugs; minor improvements

  Revision 1.3  2000/02/10 18:42:43  sg
  * All classes now call the inherited destructor in Destroy

  Revision 1.2  2000/01/24 00:23:53  sg
  * Added bitmap drawing methods to TCanvas

  Revision 1.1.1.1  1999/12/30 21:33:24  sg
  Initial import

}
