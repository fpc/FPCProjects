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
//   TGdkFontManager
// -------------------------------------------------------------------

type

  TGdkFontData = class
    Font: TFont;
    Handle: PGdkFont;
    // Charset: TFontCharset;
    Height: Integer;
    Name: TFontName;
    Pitch: TFontPitch;
    Style: TFontStyles;
  end;


  TGdkFontManager = class
  private
    Fonts: TList;		// Array of TGdkFontData objects
    procedure FontNotify(Sender: TGraphicsObject;
      AOperation: TGraphicsObjectOperation; UserData: TObject);
  public
    RefCount: Integer;		// Used only by [Need|Release]GdkFontManager!
    destructor Destroy; override;
    procedure AddFont(AFont: TFont; AGdkFont: PGdkFont);
    function  GetGdkFont(AFont: TFont): PGdkFont;
  end;


procedure TGdkFontManager.FontNotify(Sender: TGraphicsObject;
  AOperation: TGraphicsObjectOperation; UserData: TObject);
begin
end;


destructor TGdkFontManager.Destroy;
var
  font: TGdkFontData;
  i: Integer;
begin
  if Assigned(Fonts) then begin
    for i := 0 to Fonts.Count - 1 do begin
      font := TGdkFontData(Fonts[i]);
      font.Font.RemoveNotifier(@FontNotify);
      font.Free;
    end;
    Fonts.Free;
  end;
  inherited Destroy;
end;


procedure TGdkFontManager.AddFont(AFont: TFont; AGdkFont: PGdkFont);
var
  font: TGdkFontData;
begin
  font := TGdkFontData.Create;
  
  Fonts.Add(font);
end;


function TGdkFontManager.GetGdkFont(AFont: TFont): PGdkFont;
var
  font: TGdkFontData;

  procedure CreateGdkFont;
  var
    lfd: String;    // Logical font descriptor
  begin
    lfd := '-*-';
    if font.Name = '' then
      lfd := lfd + '*'
    else
      lfd := lfd + font.Name;
    lfd := lfd + '-';
    if fsBold in font.Style then
      lfd := lfd + 'bold'
    else
      lfd := lfd + 'medium';
    lfd := lfd + '-';
    if fsItalic in font.Style then
      lfd := lfd + 'i'
    else
      lfd := lfd + 'r';
    lfd := lfd + '-normal--' + IntToStr(font.Height) + '-*-*-*-*-*-iso8859-1';
// !!!:    font.GdkFont := gdk_font_load(PChar(lfd));
  end;

var
  i: Integer;
begin
  // Is AFont already in our list?
  if Assigned(Fonts) then
    for i := 0 to Fonts.Count - 1 do begin
      font := TGdkFontData(Fonts[i]);
      if font.Font = AFont then begin
        // Check if the font has been modified since we created the GDK font
{!!!:        if (not Assigned(font.GdkFont) or ({(font.Charset <> AFont.Charset) or}
	   (font.Height <> AFont.Height) or (font.Pitch <> AFont.Pitch) or
	   (font.Style <> AFont.Style) or (font.Name <> AFont.Name)) then begin
	  if Assigned(priv.GdkFont) then
  	    gdk_font_unref(priv.GdkFont);
	  CreateGdkFont;
	end;
        Result := font.GdkFont;}
	exit;
      end;
    end;


  // Apparently not, if we got here...

  if not Assigned(Fonts) then
    Fonts := TList.Create;

  font := TGdkFontData.Create;
  font.Font := AFont;
  // font.Charset := AFont.Charset;
  font.Height := AFont.Height;
  font.Name := AFont.Name;
  font.Pitch := AFont.Pitch;
  font.Style := AFont.Style;
  Fonts.Add(font);
  CreateGdkFont;
  // !!!:  Result := font.GdkFont;
end;


var

  GdkFontManager: TGdkFontManager;


procedure NeedGdkFontManager;
begin
  if not Assigned(GdkFontManager) then
  begin
    GdkFontManager := TGdkFontManager.Create;
    GdkFontManager.RefCount := 1;
  end else
    Inc(GdkFontManager.RefCount);
end;


procedure ReleaseGdkFontManager;
begin
  Dec(GdkFontManager.RefCount);
  if GdkFontManager.RefCount = 0 then begin
    GdkFontManager.Free;
    GdkFontManager := nil;
  end;
end;


// -------------------------------------------------------------------
//   TControlCanvas
// -------------------------------------------------------------------

{procedure TControlCanvas.SetFont(AFont: TFont);
begin
  FFont.Assign(AFont);
end;}


constructor TControlCanvas.Create(AHandle: TControlCanvasHandle);
begin
  FHandle := AHandle;
  priv.GC := gdk_gc_new(FHandle);
  NeedGdkFontManager;
//  FFont := TMyGdkFont.Create;
  inherited Create;
end;


constructor TControlCanvas.Create(AControl: TControl);
begin
{  Create(gtk_widget_get_parent_window(AWidget.FHandle));
  gdk_gc_copy(priv.GC, PGtkStyle(AWidget.FHandle^.thestyle)^.
    fg_gc[AWidget.FHandle^.state]);
  priv.GdkFont := PGtkStyle(AWidget.FHandle^.thestyle)^.font;
  gdk_font_ref(priv.GdkFont);
  FFont.FHeight := priv.GdkFont^.ascent + priv.GdkFont^.descent;}
end;


destructor TControlCanvas.Destroy;
begin
  FFont.Free;
  ReleaseGdkFontManager;
  gdk_gc_destroy(priv.GC);
  inherited Destroy;
end;


procedure TControlCanvas.SetColor(AColor: TColor);
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


procedure TControlCanvas.RequiredState(ReqState: TCanvasState);
begin
  if csFontValid in ReqState then begin
    if FFont.Modified or not Assigned(priv.GdkFont) then begin
      FFont.Modified := False;
    end;
  end;
end;


procedure TControlCanvas.MoveTo(x, y: Integer);
begin
  priv.PenX := x;
  priv.PenY := y;
end;


procedure TControlCanvas.LineTo(x, y: Integer);
begin
  gdk_draw_line(PGdkDrawable(FHandle), priv.GC, priv.PenX, priv.PenY, x, y);
  priv.PenX := x;
  priv.PenY := y;
end;


procedure TControlCanvas.FillRect(const rect: TRect);
begin
  gdk_draw_rectangle(PGdkDrawable(FHandle), priv.GC, 1,
    rect.Left, rect.Top, rect.Right - rect.Left, rect.Bottom - rect.Top);
end;


procedure TControlCanvas.Text(x, y: Integer; const AText: String);
begin
  if Length(AText) > 0 then begin
    RequiredState([csHandleValid, csFontValid]);
    gdk_draw_text(PGdkDrawable(FHandle), priv.GdkFont, priv.GC,
      x, y + Trunc(FFont.FHeight), PChar(AText), Length(AText));
  end;
end;


procedure TControlCanvas.CopyRect(dx, dy, dw, dh, sx, sy: Integer);
begin
  gdk_window_copy_area(FHandle, priv.GC, dx, dy, FHandle, sx, sy, dw, dh);
end;


procedure TControlCanvas.DrawBitmap(ADestX, ADestY: Integer; ABitmap: TBitmap;
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


function TControlCanvas.TextWidth(const AText: String): Integer;
begin
  RequiredState([csFontValid]);
  if Length(AText) = 1 then
    Result := gdk_char_width(priv.GdkFont, AText[1])
  else
    Result := gdk_text_width(priv.GdkFont, PChar(AText), Length(AText));
end;


function TControlCanvas.TextHeight(const AText: String): Integer;
begin
  RequiredState([csFontValid]);
  if Length(AText) = 1 then
    Result := gdk_char_height(priv.GdkFont, AText[1])
  else
    Result := gdk_text_height(priv.GdkFont, PChar(AText), Length(AText));
end;


procedure TControlCanvas.DrawButton(AControl: TWinControl; const ARect: TRect;
  IsPressed, IsGrayed: Boolean; AAlign: TAlignment; const AText: String);
var
  state: TGtkStateType;
  area: TGdkRectangle;
  WidgetFont: PGdkFont;
  x, y: Integer;
begin
  if IsPressed then
    state := GTK_STATE_SELECTED
  else
    state := 0;
  if IsGrayed then
    state := state or GTK_STATE_INSENSITIVE;

  area.x := ARect.Left;
  area.y := ARect.Top;
  area.width := ARect.Right - ARect.Left;
  area.height := ARect.Bottom - ARect.Top;

  gtk_paint_box(PGtkStyle(AControl.FHandle^.thestyle), FHandle, state, GTK_SHADOW_OUT,
    @area, AControl.FHandle, 'button',
    ARect.Left, ARect.Top, ARect.Right - ARect.Left, ARect.Bottom - ARect.Top);

  WidgetFont := PGtkStyle(AControl.FHandle^.thestyle)^.font;
  x := ARect.Left + (area.width - gdk_text_width(WidgetFont, PChar(AText), Length(AText))) div 2;
  y := ARect.Top + (area.height + Trunc(WidgetFont^.ascent + WidgetFont^.descent)) div 2;
  gdk_draw_text(PGdkDrawable(FHandle), WidgetFont, priv.GC, x, y, PChar(AText), Length(AText));
end;


// -------------------------------------------------------------------
//   TPixmapCanvas  (private KCL/Gtk class)
// -------------------------------------------------------------------

constructor TPixmapCanvas.Create(GdkWindow: PGdkWindow; w, h, ColorDepth: Integer);
begin
  inherited Create(gdk_pixmap_new(GdkWindow, w, h, ColorDepth));
end;


{
  $Log$
  Revision 1.5  2000/03/19 00:29:31  sg
  * Huge shift towards more VCL compatiblity (NOTE: Many widgets/controls
    are not working correctly at the moment!)

  Revision 1.4  2000/02/19 19:09:00  sg
  * Fixed layouting bugs; minor improvements

  Revision 1.3  2000/02/10 18:42:43  sg
  * All classes now call the inherited destructor in Destroy

  Revision 1.2  2000/01/24 00:23:53  sg
  * Added bitmap drawing methods to TCanvas

  Revision 1.1.1.1  1999/12/30 21:33:24  sg
  Initial import

}
