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

type

  TW32Canvas = class(TCanvas)
  protected
    mywnd: TWidget;
    ps: PaintStruct;
    startfont: TFont;
  public
    redrawrect: TRect;
    constructor Create(wnd: TWidget);
    destructor Destroy; override;
  end;

  TColorCacheItem = class
    color: TColor;
    pen: hPen;
    brush: hBrush;
    constructor Create(acolor: TColor);
    procedure Select(dc: HDC);
    destructor Destroy; override;
  end;

var
  colcachecount: integer;
  colcache: array[0..19] of TColorCacheItem;



function RectToTRect(r: Windows.Rect): TRect;
begin
  result := Rect(r.left, r.top, r.right, r.bottom);
end;

function TRectToRect(r: TRect): Windows.Rect;
begin
  result.left := r.left;
  result.top := r.top;
  result.right := r.right;
  result.bottom := r.bottom;
end;



// -------------------------------------------------------------------
//   TColorCacheItem
// -------------------------------------------------------------------

constructor TColorCacheItem.Create(acolor: TColor);
var
  col: ColorRef;
begin
  color := acolor;
  col := rgb(byte(color shr 16), byte(color shr 8), byte(color));
  Pen := CreatePen(ps_Solid, 0, col);
  Brush := CreateSolidBrush(col);
end;

procedure TColorCacheItem.Select(dc: HDC);
var
  col: ColorRef;
begin
  col := rgb(byte(color shr 16), byte(color shr 8), byte(color));
  SelectObject(dc, pen);
  SelectObject(dc, brush);
  SetTextColor(DC, col);
end;

destructor TColorCacheItem.Destroy;
begin
  //dürfen nicht selected sein!!!
  DeleteObject(pen);
  DeleteObject(brush);
end;

// -------------------------------------------------------------------
//   TW32Canvas
// -------------------------------------------------------------------


constructor TW32Canvas.Create(wnd: TWidget);
begin
  inherited Create;
  FFont := TFont.Create;
  with FFont do
  begin
    FontName := 'MS Sans Serif';
    Height := 8;
  end;
  startfont := FFont;
  FFont.Allocate;
  mywnd := wnd;

  with FHandle do
    DC := beginpaint(wnd.handle.Window, @ps);
  FColor := colWhite;
  Color := colBlack;

  FFont.Handle.OldFont := SelectObject(FHandle.DC, FFont.Handle.Font);
  setbkmode(FHandle.DC, transparent);
  redrawRect := RectToTRect(ps.rcPaint);
end;

destructor TW32Canvas.Destroy;
begin
  with FHandle do
  begin
    SelectObject(DC, OldPen);
    //if not DeleteObject(Pen) then writeln('pen not deleted!');
    SelectObject(DC, OldBrush);
    //if not DeleteObject(Brush) then writeln('pen not deleted!');
    SelectObject(DC, startfont.handle.OldFont);
  end;
  startfont.Free;
  EndPaint(mywnd.Handle.Window, ps);
  inherited Destroy;
end;


// -------------------------------------------------------------------
//   TCanvas
// -------------------------------------------------------------------


constructor TCanvas.Create;
begin
 // if colcache=nil then colcache := TList.Create; 
 //wird bis jetzt noch nicht freigegeben!!
end;

procedure TCanvas.SetColor(aColor: TColor);
var 
  col: ColorRef;
  cache: TColorCacheItem;
  i: integer;
begin
  if AColor <> FColor then 
  begin
    FColor := AColor;
  
    //find cached pen & brush:
    for i:=0 to colcachecount-1 do
      if colcache[i].Color = AColor then
      begin
        colcache[i].Select(FHandle.DC);
        exit;
      end;

    //allocate new color:
    cache := TColorCacheItem.Create(AColor);

    //is it the first color?
    if colcachecount = 0 then
    begin
      col := rgb(byte(acolor shr 16), byte(acolor shr 8), byte(acolor));
      FHandle.OldPen := SelectObject(FHandle.dc, cache.pen);
      FHandle.OldBrush := SelectObject(FHandle.dc, cache.brush);
      SetTextColor(FHandle.dc, col);
    end
    else cache.Select(FHandle.DC);

    //max. 20 cached colors:
    if colcachecount>=20 then 
    begin
      colcachecount := 19;
      colcache[19].Free;
    end;
    colcache[colcachecount] := cache;
    inc(colcachecount);

  end;    
end;

procedure TCanvas.SetFont(aFont: TFont);
begin
  FFont := aFont;
  if FFont<>nil then 
  begin
    FFont.Allocate;
    FFont.FHandle.OldFont := SelectObject(FHandle.DC, FFont.Fhandle.Font);   
  end
  else 
  begin
    FFont := TW32Canvas(self).startFont;
    SelectObject(FHandle.DC, FFont.Fhandle.Font);   
  end;
end;

procedure TCanvas.Line(x1, y1, x2, y2: Integer);
begin
  movetoex(FHandle.DC, X1, Y1, nil);
  lineto(FHandle.DC, X2, Y2);
end;

procedure TCanvas.FillRect(const rect: TRect);
var r: _Rect;
begin
  r.left := rect.left;
  r.top := rect.top;
  r.right := rect.right;
  r.bottom := rect.bottom;
  windows.fillrect(FHandle.DC, r, FHandle.Brush);
end;

procedure TCanvas.Text(x, y: Integer; const aText: String);
begin
  if FFont.Modified then
  begin
    SelectObject(FHandle.DC, FFont.Handle.OldFont);
    FFont.Allocate;
    SelectObject(FHandle.DC, FFont.Handle.Font);
  end;
  SetBkMode(FHandle.DC, Transparent);
  textout(FHandle.DC, x,y, PChar(aText), Length(aText));
end;


// -------------------------------------------------------------------
//   TFont
// -------------------------------------------------------------------


constructor TFont.Create;
begin
  Modified := True;
  FHeight := 12;
  FHandle.Font := 0;
end;

destructor TFont.Destroy;
begin
  //darf aber nicht selected sein!!!!
  if FHandle.Font>0 then
    if not DeleteObject(FHandle.Font) then writeln('font not deleted!');
end;

procedure TFont.FSetFontName(const AFontName: String);
begin
  FFontName := AFontName;
  Modified := True;
end;

procedure TFont.FSetWidth(AWidth: Single);
begin
  if FWidth<>AWidth then
  begin
    FWidth := AWidth;
    Modified := True;
  end;
end;

procedure TFont.FSetHeight(AHeight: Single);
begin
  if FHeight<>AHeight then
  begin
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
  w: integer;
  lf: LogFont;
begin       
  if Modified then
  begin
    if FHandle.Font>0 then
      if not DeleteObject(FHandle.Font) then writeln('font not deleted!');

    w := fw_Normal;
    if FBold then w := fw_Bold;

    with lf do
    begin
      lfHeight := -round(FHeight);
      lfWidth := round(FWidth);
      lfEscapement := 0;
      lfOrientation := 0;
      lfWeight := w;
      if FItalics then lfItalic := 1
      else lfItalic := 0;
      lfUnderline := 0;
      lfStrikeout := 0;
      lfCharset := Ansi_Charset;
      lfOutPrecision := Out_Default_Precis;
      lfClipPrecision := Clip_Default_Precis;
      lfQuality := Default_Quality;
      lfPitchAndFamily := Default_Pitch;
      lfFacename := PChar(FFontname);
    end;

    FHandle.Font := CreateFontIndirect(lf);
    Modified := False;                
  end;    
end;

function TFont.GetTextWidth(const s: String): Integer;
var asize: size;
  old: hFont;
begin
  Allocate; 
  old := SelectObject(_fontDC, FHandle.Font);  
  GetTextExtentPoint32(_fontDC, PChar(s), length(s), @asize);
  SelectObject(_fontDC, old);  
  result := asize.cx;                                        
end;

function TFont.GetTextHeight(const s: String): Integer;
var asize: size;
  old: hFont;
begin
  Allocate;
  old := SelectObject(_fontDC, FHandle.Font);  
  GetTextExtentPoint32(_fontDC, PChar(s), length(s), @asize);
  SelectObject(_fontDC, old);  
  result := asize.cy;                                        
end;


{
  $Log$
  Revision 1.1  1999/12/30 21:33:55  sg
  Initial revision

}
