{
    $Id$

    fpGUI  -  Free Pascal Graphical User Interface
    Copyright (C) 2000 - 2001 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    Default style implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


// ===================================================================
//   TStyle
// ===================================================================

constructor TStyle.Create(ADisplay: TGfxDisplay);
begin
  inherited Create;
  UpdateUIColorCache;
end;

procedure TStyle.UpdateUIColorCache;
var
  i: TColor;
begin
  for i := 0 to $18 do
    UIColorCache[i] := GetGUIColor(TColor(i + clScrollBar));
end;

function TStyle.GetUIColor(AColor: TColor): TGfxColor;
begin
  if (AColor >= clScrollBar) and (AColor <= clScrollBar + $18) then
    Result := UIColorCache[TColor(AColor - clScrollBar)]
  else
    Result := colBlack;
end;

procedure TStyle.SetUIColor(Canvas: TGfxCanvas; Color: TColor);
begin
  if (Color >= clScrollBar) and (Color <= clScrollBar + $18) then
    Canvas.SetColor(UIColorCache[TColor(Color - clScrollBar)])
  else
    Canvas.SetColor(colBlack);
end;


// ===================================================================
//   TDefaultStyle
// ===================================================================

const
  rgbaWhite: TGfxColor = (Red: $ffff; Green: $ffff; Blue: $ffff; Alpha: 0);
  rgbaDkWhite: TGfxColor = (Red: $e000; Green: $e000; Blue: $e000; Alpha: 0);
  rgbaLtGray: TGfxColor = (Red: $c000; Green: $c000; Blue: $c000; Alpha: 0);
  rgbaGray: TGfxColor = (Red: $8000; Green: $8000; Blue: $8000; Alpha: 0);
  rgbaBlack: TGfxColor = (Red: $0000; Green: $0000; Blue: $0000; Alpha: 0);
  rgbaBlue: TGfxColor = (Red: $0000; Green: $0000; Blue: $ffff; Alpha: 0);
  rgbaDkBlue: TGfxColor = (Red: $0000; Green: $0000; Blue: $8000; Alpha: 0);
  rgbaLtYellow: TGfxColor = (Red: $ffff; Green: $ffff; Blue: $e100; Alpha: 0);


{$IFDEF Win32}
  {$INCLUDE win32/defstyle.inc}
{$ENDIF}

{$INCLUDE defimpl/defstyle.inc}


constructor TDefaultStyle.Create(ADisplay: TGfxDisplay);
const
  // 60x12 pixel 4bpp bitmap
  RadioBitmapData: array[0..359] of Byte = (
    $00, $00, $22, $22, $00, $00, $00, $00, $22, $22, $00, $00, $00, $00, $22, $22, $00, $00, $00, $00, $22, $22, $00, $00, $00, $00, $22, $22, $00, $00,
    $00, $22, $11, $11, $22, $00, $00, $22, $11, $11, $22, $00, $00, $22, $11, $11, $22, $00, $00, $22, $11, $11, $22, $00, $00, $22, $11, $11, $22, $00,
    $02, $11, $77, $77, $11, $50, $02, $11, $77, $77, $11, $50, $02, $11, $33, $33, $11, $50, $02, $11, $33, $33, $11, $50, $02, $11, $33, $33, $11, $50,
    $02, $17, $77, $77, $74, $50, $02, $17, $77, $77, $74, $50, $02, $13, $33, $33, $34, $50, $02, $13, $33, $33, $34, $50, $02, $13, $33, $33, $34, $50,
    $21, $77, $77, $77, $77, $45, $21, $77, $76, $67, $77, $45, $21, $33, $33, $33, $33, $45, $21, $33, $36, $63, $33, $45, $21, $33, $32, $23, $33, $45,
    $21, $77, $77, $77, $77, $45, $21, $77, $66, $66, $77, $45, $21, $33, $33, $33, $33, $45, $21, $33, $66, $66, $33, $45, $21, $33, $22, $22, $33, $45,
    $21, $77, $77, $77, $77, $45, $21, $77, $66, $66, $77, $45, $21, $33, $33, $33, $33, $45, $21, $33, $66, $66, $33, $45, $21, $33, $22, $22, $33, $45,
    $21, $77, $77, $77, $77, $45, $21, $77, $76, $67, $77, $45, $21, $33, $33, $33, $33, $45, $21, $33, $36, $63, $33, $45, $21, $33, $32, $23, $33, $45,
    $02, $17, $77, $77, $74, $50, $02, $17, $77, $77, $74, $50, $02, $13, $33, $33, $34, $50, $02, $13, $33, $33, $34, $50, $02, $13, $33, $33, $34, $50,
    $02, $44, $77, $77, $44, $50, $02, $44, $77, $77, $44, $50, $02, $44, $33, $33, $44, $50, $02, $44, $33, $33, $44, $50, $02, $44, $33, $33, $44, $50,
    $00, $55, $44, $44, $55, $00, $00, $55, $44, $44, $55, $00, $00, $55, $44, $44, $55, $00, $00, $55, $44, $44, $55, $00, $00, $55, $44, $44, $55, $00,
    $00, $00, $55, $55, $00, $00, $00, $00, $55, $55, $00, $00, $00, $00, $55, $55, $00, $00, $00, $00, $55, $55, $00, $00, $00, $00, $55, $55, $00, $00
  );

  // 12x12 pixel monochrome bitmap
  RadioMaskData: array[0..23] of Byte = ($0f, $00, $3f, $c0, $7f, $e0, $7f,
    $e0, $ff, $f0, $ff, $f0, $ff, $f0, $ff, $f0, $7f, $e0, $7f, $e0, $3f, $c0,
    $0f, $00);

  // 65x13 pixel 4bpp bitmap
  CheckBoxBitmapData: array[0..428] of Byte = (
    $22, $22, $22, $22, $22, $22, $52, $22, $22, $22, $22, $22, $25, $22, $22, $22, $22, $22, $22, $52, $22, $22, $22, $22, $22, $25, $22, $22, $22, $22, $22, $22, $50,
    $21, $11, $11, $11, $11, $14, $52, $11, $11, $11, $11, $11, $45, $21, $11, $11, $11, $11, $14, $52, $11, $11, $11, $11, $11, $45, $21, $11, $11, $11, $11, $14, $50,
    $21, $77, $77, $77, $77, $74, $52, $17, $77, $77, $77, $77, $45, $21, $33, $33, $33, $33, $34, $52, $13, $33, $33, $33, $33, $45, $21, $33, $33, $33, $33, $34, $50,
    $21, $77, $77, $77, $77, $74, $52, $17, $77, $77, $77, $67, $45, $21, $33, $33, $33, $33, $34, $52, $13, $33, $33, $33, $63, $45, $21, $33, $33, $33, $32, $34, $50,
    $21, $77, $77, $77, $77, $74, $52, $17, $77, $77, $76, $67, $45, $21, $33, $33, $33, $33, $34, $52, $13, $33, $33, $36, $63, $45, $21, $33, $33, $33, $22, $34, $50,
    $21, $77, $77, $77, $77, $74, $52, $17, $67, $77, $66, $67, $45, $21, $33, $33, $33, $33, $34, $52, $13, $63, $33, $66, $63, $45, $21, $32, $33, $32, $22, $34, $50,
    $21, $77, $77, $77, $77, $74, $52, $17, $66, $76, $66, $77, $45, $21, $33, $33, $33, $33, $34, $52, $13, $66, $36, $66, $33, $45, $21, $32, $23, $22, $23, $34, $50,
    $21, $77, $77, $77, $77, $74, $52, $17, $66, $66, $67, $77, $45, $21, $33, $33, $33, $33, $34, $52, $13, $66, $66, $63, $33, $45, $21, $32, $22, $22, $33, $34, $50,
    $21, $77, $77, $77, $77, $74, $52, $17, $76, $66, $77, $77, $45, $21, $33, $33, $33, $33, $34, $52, $13, $36, $66, $33, $33, $45, $21, $33, $22, $23, $33, $34, $50,
    $21, $77, $77, $77, $77, $74, $52, $17, $77, $67, $77, $77, $45, $21, $33, $33, $33, $33, $34, $52, $13, $33, $63, $33, $33, $45, $21, $33, $32, $33, $33, $34, $50,
    $21, $77, $77, $77, $77, $74, $52, $17, $77, $77, $77, $77, $45, $21, $33, $33, $33, $33, $34, $52, $13, $33, $33, $33, $33, $45, $21, $33, $33, $33, $33, $34, $50,
    $24, $44, $44, $44, $44, $44, $52, $44, $44, $44, $44, $44, $45, $24, $44, $44, $44, $44, $44, $52, $44, $44, $44, $44, $44, $45, $24, $44, $44, $44, $44, $44, $50,
    $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $50
  );

  // 64x8 pixel 4bpp bitmap
  ArrowBitmapData: array[0..255] of Byte = (
    $33, $33, $33, $33, $33, $33, $33, $33, $33, $33, $13, $33, $33, $31, $33, $33, $33, $33, $33, $33, $33, $33, $33, $33, $33, $33, $23, $33, $33, $23, $33, $33,
    $33, $33, $33, $33, $33, $33, $33, $33, $33, $31, $13, $33, $33, $31, $13, $33, $33, $33, $33, $33, $33, $33, $33, $33, $33, $32, $25, $33, $33, $22, $33, $33,
    $33, $31, $33, $33, $11, $11, $11, $13, $33, $11, $13, $33, $33, $31, $11, $33, $33, $32, $33, $33, $22, $22, $22, $23, $33, $22, $25, $33, $33, $22, $23, $33,
    $33, $11, $13, $33, $31, $11, $11, $33, $31, $11, $13, $33, $33, $31, $11, $13, $33, $22, $23, $33, $32, $22, $22, $55, $32, $22, $25, $33, $33, $22, $22, $33,
    $31, $11, $11, $33, $33, $11, $13, $33, $33, $11, $13, $33, $33, $31, $11, $33, $32, $22, $22, $33, $33, $22, $25, $53, $33, $22, $25, $33, $33, $22, $25, $53,
    $11, $11, $11, $13, $33, $31, $33, $33, $33, $31, $13, $33, $33, $31, $13, $33, $22, $22, $22, $23, $33, $32, $55, $33, $33, $32, $25, $33, $33, $22, $55, $33,
    $33, $33, $33, $33, $33, $33, $33, $33, $33, $33, $13, $33, $33, $31, $33, $33, $35, $55, $55, $55, $33, $33, $53, $33, $33, $33, $25, $33, $33, $25, $53, $33,
    $33, $33, $33, $33, $33, $33, $33, $33, $33, $33, $33, $33, $33, $33, $33, $33, $33, $33, $33, $33, $33, $33, $33, $33, $33, $33, $35, $33, $33, $35, $33, $33
  );

var
  PalData: array[0..7] of TGfxColor;
  Palette: TGfxPalette;
  Image: TGfxImage;
begin
  inherited Create(ADisplay);

  PalData[0] := colMagenta;
  PalData[1] := GetGUIColor(cl3DDkShadow);
  PalData[2] := GetGUIColor(cl3DShadow);
  PalData[3] := GetGUIColor(cl3DFace);
  PalData[4] := GetGUIColor(cl3DLight);
  PalData[5] := GetGUIColor(cl3DHighlight);
  PalData[6] := GetGUIColor(clWindowText);
  PalData[7] := GetGUIColor(clWindow);

  { The image canvas contains 3 horizontal stripes:
    - y=0..11: Radio button images (5x 12x12 pixel)
    - y=12..24: Check box images (5x 13x13 pixel)
    - y=25..32: Arrow images (8x 8x8 pixel) }

  ImageCanvas := ADisplay.DefaultScreen.CreateBitmap(65, 33);
  MaskCanvas := ADisplay.DefaultScreen.CreateMonoBitmap(12, 12);
  Palette := ADisplay.DefaultScreen.CreatePalette(8, @PalData);

  Image := ADisplay.CreateImage(60, 12, PixelFormatPal4);
  Image.Palette := Palette;
  Image.SetPixelsFromData(@RadioBitmapData, 30);
  ImageCanvas.DrawImage(Image, Point(0, 0));
  Image.Free;

  Image := ADisplay.CreateImage(12, 12, PixelFormatMono);
  Image.SetPixelsFromData(@RadioMaskData, 2);
  MaskCanvas.DrawImage(Image, Point(0, 0));
  Image.Free;

  Image := ADisplay.CreateImage(66, 13, PixelFormatPal4); {!!!: width is 65, but fpGFX emulayer has a problem with odd widths}
  Image.Palette := Palette;
  Image.SetPixelsFromData(@CheckBoxBitmapData, 33);
  ImageCanvas.DrawImage(Image, Point(0, 12));
  Image.Free;

  Image := ADisplay.CreateImage(64, 8, PixelFormatPal4);
  Image.Palette := Palette;
  Image.SetPixelsFromData(@ArrowBitmapData, 32);
  ImageCanvas.DrawImage(Image, Point(0, 25));
  Image.Free;

  Palette.Release;
end;

destructor TDefaultStyle.Destroy;
begin
  MaskCanvas.Free;
  ImageCanvas.Free;
  inherited Destroy;
end;


// helper functions


{ Draws a 3D frame, its thickness is 2 pixels. The 4 given colors are used in
  this way:
  Color1: Outer frame left & top
  Color2: Inner frame left & top
  Color3: Outer frame right & bottom
  Color4: Inner frame right & bottom
}

procedure TDefaultStyle.Draw3DFrame(Canvas: TGfxCanvas; const ARect: TRect;
  Color1, Color2, Color3, Color4: TColor);
begin
  with ARect do
  begin
    SetUIColor(Canvas, Color1);
    {Canvas.DrawPolyLine([Point(Left, Bottom - 2), TopLeft,
      Point(Right - 1, Top)]);}
    Canvas.DrawLine(Point(Left, Bottom - 2), TopLeft);
    Canvas.DrawLine(TopLeft, Point(Right - 1, Top));
    SetUIColor(Canvas, Color2);
    {Canvas.DrawPolyLine([Point(Left + 1, Bottom - 3), Point(Left + 1, Top + 1),
      Point(Right - 2, Top + 1)]);}
    Canvas.DrawLine(Point(Left + 1, Bottom - 3), Point(Left + 1, Top + 1));
    Canvas.DrawLine(Point(Left + 1, Top + 1), Point(Right - 2, Top + 1));
    SetUIColor(Canvas, Color3);
    {Canvas.DrawPolyLine([Point(Left, Bottom - 1), Point(Right - 1, Bottom - 1),
      Point(Right - 1, Top - 1)]);}
    Canvas.DrawLine(Point(Left, Bottom - 1), Point(Right - 1, Bottom - 1));
    Canvas.DrawLine(Point(Right - 1, Bottom - 1), Point(Right - 1, Top - 1));
    SetUIColor(Canvas, Color4);
    {Canvas.DrawPolyLine([Point(Left + 1, Bottom - 2),
      Point(Right - 2, Bottom - 2), Point(Right - 2, Top)]);}
    Canvas.DrawLine(Point(Left + 1, Bottom - 2), Point(Right - 2, Bottom - 2));
    Canvas.DrawLine(Point(Right - 2, Bottom - 2), Point(Right - 2, Top));
  end;
end;

procedure TDefaultStyle.DrawSunkenOuterBorder(Canvas: TGfxCanvas;
  const ARect: TRect);
begin
  with ARect do
  begin
    SetUIColor(Canvas, cl3DShadow);
    // Canvas.DrawPolyLine([Point(Left, Bottom - 2), TopLeft, Point(Right, Top)]);
    Canvas.DrawLine(Point(Left, Bottom - 2), TopLeft);
    Canvas.DrawLine(TopLeft, Point(Right, Top));
    SetUIColor(Canvas, cl3DHighlight);
    {Canvas.DrawPolyLine([Point(Left, Bottom - 1), Point(Right - 1, Bottom - 1),
      Point(Right - 1, Top - 1)]);}
    Canvas.DrawLine(Point(Left, Bottom - 1), Point(Right - 1, Bottom - 1));
    Canvas.DrawLine(Point(Right - 1, Bottom - 1), Point(Right - 1, Top - 1));
  end;
end;


// style engine implementation

procedure TDefaultStyle.DrawText(Canvas: TGfxCanvas; const APosition: TPoint;
  const AText: String; State: TWidgetState);
begin
  if not (wsEnabled in State) then
  begin
    SetUIColor(Canvas, cl3DHighlight);
    Canvas.TextOut(APosition + Point(1, 1), AText);
    SetUIColor(Canvas, cl3DShadow);
  end;
  Canvas.TextOut(APosition, AText);
end;

procedure TDefaultStyle.DrawItemBefore(Canvas: TGfxCanvas; const ARect: TRect;
  Flags: TItemFlags);
begin
  if ifSelected in Flags then
  begin
    SetUIColor(Canvas, clHighlight);
    Canvas.FillRect(ARect);
    SetUIColor(Canvas, clHighlightText);
  end;
end;

procedure TDefaultStyle.DrawItemAfter(Canvas: TGfxCanvas; const ARect: TRect;
  Flags: TItemFlags);
begin
  if ifFocused in Flags then
    DrawFocusRect(Canvas, ARect);
end;

procedure TDefaultStyle.DrawWindowBackground(Canvas: TGfxCanvas;
  const ARect: TRect);
begin
  SetUIColor(Canvas, cl3DFace);
  Canvas.FillRect(ARect);
end;

procedure TDefaultStyle.DrawButtonFace(Canvas: TGfxCanvas; const ARect: TRect;
  Flags: TButtonFlags);
var
  r: TRect;
begin
  r := ARect;

  if btnIsSelected in Flags then
  begin
    SetUIColor(Canvas, cl3DDkShadow);
    Canvas.DrawRect(r);
    Inc(r.Left);
    Inc(r.Top);
    Dec(r.Right);
    Dec(r.Bottom);
  end;

  if btnIsPressed in Flags then
  begin
    SetUIColor(Canvas, cl3DShadow);
    Canvas.DrawRect(r);
    Inc(r.Left);
    Inc(r.Top);
    Dec(r.Right);
    Dec(r.Bottom);
  end else
  begin
    if btnIsEmbedded in Flags then
      Draw3DFrame(Canvas, r, cl3DLight, cl3DHighlight, cl3DDkShadow, cl3DShadow)
    else
      Draw3DFrame(Canvas, r, cl3DHighlight, cl3DLight, cl3DDkShadow, cl3DShadow);
    Inc(r.Left, 2);
    Inc(r.Top, 2);
    Dec(r.Right, 2);
    Dec(r.Bottom, 2);
  end;

  SetUIColor(Canvas, cl3DFace);
  Canvas.FillRect(r);

  if btnHasFocus in Flags then
  begin
    r.Left := ARect.Left + 4;
    r.Top := ARect.Top + 4;
    r.Right := ARect.Right - 4;
    r.Bottom := ARect.Bottom - 4;
    DrawFocusRect(Canvas, r);
  end;
end;

function TDefaultStyle.GetButtonBorders: TRect;
begin
  Result := Rect(5, 5, 5, 5);
end;

function TDefaultStyle.GetSeparatorSize: Integer;
begin
  Result := 2;
end;

procedure TDefaultStyle.DrawSeparator(Canvas: TGfxCanvas; const ARect: TRect;
  AOrientation: TOrientation);
var
  r: TRect;
begin
  with ARect do
    if AOrientation = Horizontal then
      r := Rect(Left, Top + (Bottom - Top) div 2 - 1, Right,
        Top + (Bottom - Top) div 2 + 1)
    else
      r := Rect(Left + (Right - Left) div 2 - 1, Top,
        Left + (Right - Left) div 2 + 1, Bottom);
  DrawSunkenOuterBorder(Canvas, r);
end;


procedure TDefaultStyle.DrawGroupBox(Canvas: TGfxCanvas; const ARect: TRect;
  const ALabel: String; WidgetState: TWidgetState);
var
  TitleWidth, TitleHeight, TopLine: Integer;
begin
  TitleWidth := Canvas.TextWidth(ALabel);
  TitleHeight := Canvas.FontCellHeight;
  TopLine := ARect.Top + TitleHeight div 3;

  SetUIColor(Canvas, cl3DHighlight);
{  with ARect do
    Canvas.DrawPolyLine([Left + TitleWidth + 10, TopLine + 1,
      Right - 1, TopLine + 1,
      Right - 1, Bottom - 1,
      Left + 1, Bottom - 1,
      Left + 1, TopLine + 1,
      Left + 8, TopLine + 1]);}
  with ARect do
  begin
    Canvas.DrawLine(Point(Left + TitleWidth + 10, TopLine + 1), Point(Right - 1, TopLine + 1));
    Canvas.DrawLine(Point(Right - 1, TopLine + 1), Point(Right - 1, Bottom - 1));
    Canvas.DrawLine(Point(Right - 1, Bottom - 1), Point(Left + 1, Bottom - 1));
    Canvas.DrawLine(Point(Left + 1, Bottom - 1), Point(Left + 1, TopLine + 1));
    Canvas.DrawLine(Point(Left + 1, TopLine + 1), Point(Left + 8, TopLine + 1));
  end;

  SetUIColor(Canvas, cl3DShadow);
{  with ARect do
    Canvas.DrawPolyLine([Left + TitleWidth + 10, TopLine,
      Right - 2, TopLine,
      Right - 2, Bottom - 2,
      Left, Bottom - 2,
      Left, TopLine,
      Left + 8, TopLine]);}
  with ARect do
  begin
    Canvas.DrawLine(Point(Left + TitleWidth + 10, TopLine), Point(Right - 2, TopLine));
    Canvas.DrawLine(Point(Right - 2, TopLine), Point(Right - 2, Bottom - 2));
    Canvas.DrawLine(Point(Right - 2, Bottom - 2), Point(Left, Bottom - 2));
    Canvas.DrawLine(Point(Left, Bottom - 2), Point(Left, TopLine));
    Canvas.DrawLine(Point(Left, TopLine), Point(Left + 8, TopLine));
  end;

  SetUIColor(Canvas, clWindowText);
  DrawText(Canvas, ARect.TopLeft + Point(9, 0), ALabel, WidgetState);
end;

function TDefaultStyle.GetGroupBoxBorders(Canvas: TGfxCanvas;
  const ALabel: String; var LabelWidth: Integer): TRect;
begin
  Result := Rect(6, Canvas.FontCellHeight + 4, 6, 6);
  LabelWidth := Canvas.TextWidth(ALabel) + 6;
end;

procedure TDefaultStyle.DrawEditBox(Canvas: TGfxCanvas; const ARect: TRect);
begin
  Draw3DFrame(Canvas, ARect, cl3DShadow, cl3DDkShadow, cl3DHighlight, cl3DFace);
  SetUIColor(Canvas, clWindow);
  with ARect do
    Canvas.FillRect(Rect(Left + 2, Top + 2, Right - 2, Bottom - 2));
end;

function TDefaultStyle.GetEditBoxBorders: TRect;
begin
  Result := Rect(2, 2, 2, 2);
end;

procedure TDefaultStyle.DrawCheckBox(Canvas: TGfxCanvas;
  const ARect, LabelRect: TRect; Flags: TCheckboxFlags);
var
  Index, BoxY: Integer;
begin
  SetUIColor(Canvas, cl3DFace);
  Canvas.FillRect(ARect);
  with ARect do
  begin
    BoxY := Top + (Bottom - Top - 13) div 2;
    if cbIsEnabled in Flags then
    begin
      Index := Ord(cbIsChecked in Flags);
      if cbIsPressed in Flags then
        Inc(Index, 2);
    end else
      Index := 2 + Ord(cbIsChecked in Flags) * 2;
    Canvas.CopyRect(ImageCanvas, Rect(Index * 13, 12, (Index + 1) * 13, 25),
      Point(Left, BoxY));
  end;

  if cbHasFocus in Flags then
    with LabelRect do
      DrawFocusRect(Canvas, Rect(Left - 2, Top - 2, Right + 2, Bottom + 2));
end;

procedure TDefaultStyle.GetCheckBoxLayout(const LabelSize: TSize;
  var TotalSize: TSize; var LabelPos: TPoint);
begin
  TotalSize := Size(LabelSize.cx + 21, Max(13, LabelSize.cy + 4));
  LabelPos := Point(19, (TotalSize.cy - LabelSize.cy) div 2);
end;

procedure TDefaultStyle.DrawRadioButton(Canvas: TGfxCanvas;
  const ARect, LabelRect: TRect; Flags: TCheckboxFlags);
var
  Index, BtnY: Integer;
begin
  with ARect do
  begin
    BtnY := Top + (Bottom - Top - 12) div 2;
    if cbIsEnabled in Flags then
    begin
      Index := Ord(cbIsChecked in Flags);
      if cbIsPressed in Flags then
        Inc(Index, 2);
    end else
      Index := 2 + Ord(cbIsChecked in Flags) * 2;
    Canvas.MaskedCopyRect(ImageCanvas, MaskCanvas,
      Rect(Index * 12, 0, (Index + 1) * 12, 12),
      Point(0, 0), Point(Left, BtnY));
  end;
  if cbHasFocus in Flags then
    with LabelRect do
      DrawFocusRect(Canvas, Rect(Left - 2, Top - 2, Right + 2, Bottom + 2));
end;

procedure TDefaultStyle.GetRadioButtonLayout(const LabelSize: TSize;
  var TotalSize: TSize; var LabelPos: TPoint);
begin
  TotalSize := Size(LabelSize.cx + 20, Max(12, LabelSize.cy + 4));
  LabelPos := Point(18, (TotalSize.cy - LabelSize.cy) div 2);
end;

function TDefaultStyle.GetComboBoxArrowSize: TSize;
begin
  Result := Size(16, 17);
end;

procedure TDefaultStyle.DrawComboBoxArrow(Canvas: TGfxCanvas;
  const ARect: TRect; IsPressed, IsEnabled: Boolean);
var
  Pt: TPoint;
  Index: Integer;
begin
  with ARect do
    Pt := TopLeft + Point((Right - Left - 8) div 2, (Bottom - Top - 8) div 2);

  if IsPressed then
    Pt := Pt + Point(1, 1);

  Index := 1 + Ord(not IsEnabled) * 4;
  Canvas.CopyRect(ImageCanvas, Rect(Index * 8, 25, (Index + 1) * 8, 33), Pt);
end;

function TDefaultStyle.GetScrollBarBorders(Orientation: TOrientation): TRect;
begin
  Result := Rect(1, 1, 1, 1);
end;

function TDefaultStyle.GetScrollBarBtnSize(Orientation: TOrientation): TSize;
begin
  Result.cx := 16;
  Result.cy := 16;
end;

function TDefaultStyle.GetScrollBarBtnMinSize: Integer;
begin
  Result := 8;
end;

procedure TDefaultStyle.DrawScrollBarBorder(Canvas: TGfxCanvas;
  const ARect: TRect);
begin
  with ARect do
  begin
    SetUIColor(Canvas, cl3DShadow);
{    Canvas.DrawPolyLine([Left, Bottom - 1, Left, Top, Right, Top]);}
    Canvas.DrawLine(Point(Left, Bottom - 1), TopLeft);
    Canvas.DrawLine(TopLeft, Point(Right, Top));
    SetUIColor(Canvas, cl3DHighlight);
{    Canvas.DrawPolyLine([Left + 1, Bottom - 1, Right - 1, Bottom - 1,
      Right - 1, Top]);}
    Canvas.DrawLine(Point(Left + 1, Bottom - 1), Point(Right - 1, Bottom - 1));
    Canvas.DrawLine(Point(Right - 1, Bottom - 1), Point(Right - 1, Top));
    SetUIColor(Canvas, cl3DFace);
    Canvas.FillRect(Rect(Left, Bottom - 1, Left + 1, Bottom));
    Canvas.FillRect(Rect(Right - 1, Top, Right, Top + 1));
  end;
end;

procedure TDefaultStyle.DrawScrollBarButton(Canvas: TGfxCanvas;
  const ARect: TRect; Direction: TArrowDirection;
  IsPressed, IsEnabled: Boolean);
var
  Index: Integer;
begin
  Index := Ord(Direction) + Ord(not IsEnabled) * 4;
  with ARect do
    Canvas.CopyRect(
      ImageCanvas,
      Rect(Index * 8, 25, (Index + 1) * 8, 33),
      TopLeft + Point((Right - Left - 8) div 2, (Bottom - Top - 8) div 2));
end;

function TDefaultStyle.GetScrollBoxBorders: TRect;
begin
  Result := Rect(2, 2, 2, 2);
end;

procedure TDefaultStyle.DrawScrollBoxBorder(Canvas: TGfxCanvas;
  const ARect: TRect);
begin
  Draw3DFrame(Canvas, ARect, cl3DShadow, cl3DDkShadow,
    cl3DHighlight, cl3DLight);
end;


{
  $Log$
  Revision 1.4  2001/02/14 23:14:45  sg
  * Adaptions to fpGFX changes; added much more usage of TSize and TPoint
    here as well
  * Heavy improvements of the layouting algorithm

  Revision 1.3  2001/02/09 20:50:20  sg
  * The usual bunch of improvements and bug fixes...

  Revision 1.2  2001/01/11 23:30:12  sg
  *** empty log message ***

  Revision 1.1  2000/12/23 23:20:15  sg
  * First public CVS version...

}
