{
    $Id$

    fpGUI  -  Free Pascal Graphical User Interface
    Copyright (C) 2000 by
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

constructor TStyle.Create(ADevice: TGfxDevice);
begin
  inherited Create;
end;

function TStyle.MapGUIColor(Canvas: TGfxCanvas; Color: TColor): TGfxPixel;
begin
  Result := Canvas.MapColor(GetGUIColor(Color));
end;

procedure TStyle.SetUIColor(Canvas: TGfxCanvas; Color: TColor);
begin
  Canvas.SetColor(MapGUIColor(Canvas, Color));
end;


// ===================================================================
//   TDefaultStyle
// ===================================================================

const
  rgbaWhite: TGfxColor = (Red: $ffff; Green: $ffff; Blue: $ffff; Alpha: 0);
  rgbaDkWhite: TGfxColor = (Red: $e000; Green: $e000; Blue: $e000; Alpha: 0);
  rgbaLtLtGray: TGfxColor = (Red: $d800; Green: $d800; Blue: $d800; Alpha: 0);
  rgbaLtGray: TGfxColor = (Red: $c000; Green: $c000; Blue: $c000; Alpha: 0);
  rgbaGray: TGfxColor = (Red: $8000; Green: $8000; Blue: $8000; Alpha: 0);
  rgbaBlack: TGfxColor = (Red: $0000; Green: $0000; Blue: $0000; Alpha: 0);
  rgbaBlue: TGfxColor = (Red: $0000; Green: $0000; Blue: $ffff; Alpha: 0);
  rgbaLtYellow: TGfxColor = (Red: $ffff; Green: $ffff; Blue: $e100; Alpha: 0);


constructor TDefaultStyle.Create(ADevice: TGfxDevice);

  function CreateMonoBitmap(Data: Pointer; w, h: Integer): TGfxImage;
  begin
    Result := ADevice.CreateImage(w, h, PixelFormatMono);
    Result.SetPixelsFromData(Data, (w + 7) div 8);
  end;

const
  RadioBitmap0Data: array[0..11] of Word = 
    ($0f0, $30c, $002, $002, $001, $001, $001, $001, $002, $002, $000, $000);
  RadioBitmap1Data: array[0..11] of Word = 
    ($000, $0f0, $30c, $004, $002, $002, $002, $002, $004, $000, $000, $000);
  RadioBitmap2Data: array[0..11] of Word = 
    ($000, $000, $400, $400, $800, $800, $800, $800, $400, $400, $30c, $0f0);
  RadioBitmap3Data: array[0..11] of Word = 
    ($000, $000, $000, $200, $400, $400, $400, $400, $200, $30c, $0f0, $000);
  RadioBitmap4Data: array[0..11] of Word = 
    ($000, $000, $0f0, $1f8, $3fc, $3fc, $3fc, $3fc, $1f8, $0f0, $000, $000);
  RadioBitmap5Data: array[0..11] of Word = 
    ($000, $000, $000, $000, $060, $0f0, $0f0, $060, $000, $000, $000, $000);
  CheckBitmapData: array[0..7] of Byte = ($00, $80, $c0, $e2, $76, $3e, $1c, $08);
  ArrowUpBitmapData: array[0..3] of Byte = ($08, $1c, $3e, $7f);
  ArrowDownBitmapData: array[0..3] of Byte = ($7f, $3e, $1c, $08);
  ArrowLeftBitmapData: array[0..6] of Byte = ($08, $0c, $0e, $0f, $0e, $0c, $08);
  ArrowRightBitmapData: array[0..6] of Byte = ($01, $03, $07, $0f, $07, $03, $01);
begin
  inherited Create(ADevice);
  RadioBitmaps[0] := CreateMonoBitmap(@RadioBitmap0Data, 12, 12);
  RadioBitmaps[1] := CreateMonoBitmap(@RadioBitmap1Data, 12, 12);
  RadioBitmaps[2] := CreateMonoBitmap(@RadioBitmap2Data, 12, 12);
  RadioBitmaps[3] := CreateMonoBitmap(@RadioBitmap3Data, 12, 12);
  RadioBitmaps[4] := CreateMonoBitmap(@RadioBitmap4Data, 12, 12);
  RadioBitmaps[5] := CreateMonoBitmap(@RadioBitmap5Data, 12, 12);
  CheckBitmap := CreateMonoBitmap(@CheckBitmapData, 8, 8);
  ArrowBitmaps[arrowUp] := CreateMonoBitmap(@ArrowUpBitmapData, 7, 4);
  ArrowBitmaps[arrowDown] := CreateMonoBitmap(@ArrowDownBitmapData, 7, 4);
  ArrowBitmaps[arrowLeft] := CreateMonoBitmap(@ArrowLeftBitmapData, 4, 7);
  ArrowBitmaps[arrowRight] := CreateMonoBitmap(@ArrowRightBitmapData, 4, 7);
end;

destructor TDefaultStyle.Destroy;
var
  i: Integer;
begin
  for i := Ord(Low(TArrowDirection)) to Ord(High(TArrowDirection)) do
    ArrowBitmaps[TArrowDirection(i)].Free;
  CheckBitmap.Free;
  for i := 0 to 5 do
    RadioBitmaps[i].Free;
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
    Canvas.DrawPolyLine([Left, Bottom - 2, Left, Top, Right - 1, Top]);
    SetUIColor(Canvas, Color2);
    Canvas.DrawPolyLine([Left + 1, Bottom - 3, Left + 1, Top + 1, Right - 2, Top + 1]);
    SetUIColor(Canvas, Color3);
    Canvas.DrawPolyLine([Left, Bottom - 1, Right - 1, Bottom - 1, Right - 1, Top - 1]);
    SetUIColor(Canvas, Color4);
    Canvas.DrawPolyLine([Left + 1, Bottom - 2, Right - 2, Bottom - 2, Right - 2, Top]);
  end;
end;

procedure TDefaultStyle.DrawFocusRect(Canvas: TGfxCanvas; const ARect: TRect);
begin
//  Canvas.SetColor(GC.MapColor(rgbaBlack));
//  Canvas.SetLineStyle(lsDot);	// !!!: Disabled as long a a certain nVidia X-Server has bugs with this...
  Canvas.SetColor(Canvas.MapColor(rgbaGray));
  Canvas.DrawRect(ARect);
  Canvas.SetLineStyle(lsSolid);
end;

procedure TDefaultStyle.DrawSunkenOuterBorder(Canvas: TGfxCanvas;
  const ARect: TRect);
begin
  with ARect do
  begin
    SetUIColor(Canvas, cl3DShadow);
    Canvas.DrawPolyLine([Left, Bottom - 2, Left, Top, Right, Top]);
    SetUIColor(Canvas, cl3DHighlight);
    Canvas.DrawPolyLine([Left, Bottom - 1, Right - 1, Bottom - 1,
      Right - 1, Top - 1]);
  end;
end;


// theme engine implementation

function TDefaultStyle.GetGUIColor(Color: TColor): TGfxColor;
begin
  case Color of
//    clScrollBar: Result := rgbaLtGray;
    clScrollBar: Result := rgbaLtLtGray;
    clMenu: Result := rgbaLtGray;
    clWindow: Result := rgbaWhite;
    clMenuText: Result := rgbaBlack;
    clWindowText: Result := rgbaBlack;
    clAppWorkSpace: Result := rgbaGray;
    clHighlight: Result := rgbaBlue;
    clHighlightText: Result := rgbaWhite;
    cl3DFace: Result := rgbaLtGray;
    cl3DShadow: Result := rgbaGray;
    clGrayText: Result := rgbaGray;
    clBtnText: Result := rgbaBlack;
    cl3DHighlight: Result := rgbaWhite;
    cl3DDkShadow: Result := rgbaBlack;
    cl3DLight: Result := rgbaDkWhite;
    clInfoText: Result := rgbaBlack;
    clInfoBk: Result := rgbaLtYellow;
    else Result := rgbaWhite;
  end;
end;

procedure TDefaultStyle.DrawText(Canvas: TGfxCanvas; x, y: Integer;
  const AText: String; State: TWidgetState);
begin
  if not (wsEnabled in State) then
  begin
    SetUIColor(Canvas, cl3DHighlight);
    Canvas.TextOut(x + 1, y + 1, AText);
    SetUIColor(Canvas, cl3DShadow);
  end;
  Canvas.TextOut(x, y, AText);
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
  with ARect do
    Canvas.DrawPolyLine([Left + TitleWidth + 10, TopLine + 1,
      Right - 1, TopLine + 1,
      Right - 1, Bottom - 1,
      Left + 1, Bottom - 1,
      Left + 1, TopLine + 1,
      Left + 8, TopLine + 1]);

  SetUIColor(Canvas, cl3DShadow);
  with ARect do
    Canvas.DrawPolyLine([Left + TitleWidth + 10, TopLine,
      Right - 2, TopLine,
      Right - 2, Bottom - 2,
      Left, Bottom - 2,
      Left, TopLine,
      Left + 8, TopLine]);

  Canvas.SetColor(Canvas.MapColor(rgbaBlack));
  DrawText(Canvas, ARect.Left + 9, ARect.Top, ALabel, WidgetState);
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
  BoxY: Integer;
begin
  SetUIColor(Canvas, cl3DFace);
  Canvas.FillRect(ARect);
  with ARect do
  begin
    BoxY := Top + (Bottom - Top - 13) div 2;
    Draw3DFrame(Canvas, Rect(Left, BoxY, Left + 13, BoxY + 13),
      cl3DShadow, cl3DDkShadow, cl3DHighlight, cl3DFace);
    if (not (cbIsEnabled in Flags)) or (cbIsPressed in Flags) then
      SetUIColor(Canvas, cl3DFace)
    else
      SetUIColor(Canvas, clWindow);
    Canvas.FillRect(Rect(Left + 2, BoxY + 2, Left + 11, BoxY + 11));
    if cbIsChecked in Flags then
    begin
      if cbIsEnabled in Flags then
        SetUIColor(Canvas, clWindowText)
      else
        SetUIColor(Canvas, cl3DShadow);
      Canvas.DrawImage(CheckBitmap, Left + 2, BoxY + 2);
    end;
  end;
  if cbHasFocus in Flags then
    with LabelRect do
      DrawFocusRect(Canvas, Rect(Left - 2, Top - 2, Right + 2, Bottom + 2));
end;

procedure TDefaultStyle.GetCheckBoxLayout(LabelW, LabelH: Integer;
  var TotalW, TotalH, LabelX, LabelY: Integer);
begin
  TotalW := LabelW + 21;
  TotalH := Max(13, LabelH + 4);
  LabelX := 19;
  LabelY := (TotalH - LabelH) div 2;
end;

procedure TDefaultStyle.DrawRadioButton(Canvas: TGfxCanvas;
  const ARect, LabelRect: TRect; Flags: TCheckboxFlags);
const
  Colors: array[0..3] of TColor = (cl3DShadow, cl3DDkShadow, cl3DHighlight, cl3DFace);
var
  i, BtnY: Integer;
begin
  SetUIColor(Canvas, cl3DFace);
  Canvas.FillRect(ARect);
  with ARect do
  begin
    BtnY := Top + (Bottom - Top - 12) div 2;
    for i := 0 to 3 do
    begin
      SetUIColor(Canvas, Colors[i]);
      Canvas.DrawImage(RadioBitmaps[i], Left, BtnY);
    end;
    if (cbIsPressed in Flags) or not (cbIsEnabled in Flags) then
      SetUIColor(Canvas, cl3DFace)
    else
      SetUIColor(Canvas, clWindow);
    Canvas.DrawImage(RadioBitmaps[4], Left, BtnY);
    if cbIsChecked in Flags then
    begin
      if cbIsEnabled in Flags then
        SetUIColor(Canvas, clWindowText)
      else
        SetUIColor(Canvas, cl3DShadow);
      Canvas.DrawImage(RadioBitmaps[5], Left, BtnY);
    end;
  end;
  if cbHasFocus in Flags then
    with LabelRect do
      DrawFocusRect(Canvas, Rect(Left - 2, Top - 2, Right + 2, Bottom + 2));
end;

procedure TDefaultStyle.GetRadioButtonLayout(LabelW, LabelH: Integer;
  var TotalW, TotalH, LabelX, LabelY: Integer);
begin
  TotalW := LabelW + 20;
  TotalH := Max(12, LabelH + 4);
  LabelX := 18;
  LabelY := (TotalH - LabelH) div 2;
end;

function TDefaultStyle.GetComboBoxArrowSize: TSize;
begin
  Result.cx := 16;
  Result.cy := 17;
end;

procedure TDefaultStyle.DrawComboBoxArrow(Canvas: TGfxCanvas;
  const ARect: TRect; IsPressed, IsEnabled: Boolean);
var
  x, y: Integer;
begin
  with ARect do
  begin
    x := Left + (Right - Left - 7) div 2;
    y := Top + (Bottom - Top - 4) div 2;
  end;
  if IsPressed then
  begin
    Inc(x);
    Inc(y);
  end;

  if IsEnabled then
    SetUIColor(Canvas, clBtnText)
  else
  begin
    SetUIColor(Canvas, cl3DHighlight);
    Canvas.DrawImage(ArrowBitmaps[arrowDown], x + 1, y + 1);
    SetUIColor(Canvas, cl3DShadow);
  end;
    Canvas.DrawImage(ArrowBitmaps[arrowDown], x, y);
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
    Canvas.DrawPolyLine([Left, Bottom - 1, Left, Top, Right, Top]);
    SetUIColor(Canvas, cl3DHighlight);
    Canvas.DrawPolyLine([Left + 1, Bottom - 1, Right - 1, Bottom - 1,
      Right - 1, Top]);
    SetUIColor(Canvas, cl3DFace);
    Canvas.FillRect(Rect(Left, Bottom - 1, Left + 1, Bottom));
    Canvas.FillRect(Rect(Right - 1, Top, Right, Top + 1));
  end;
end;

procedure TDefaultStyle.DrawScrollBarButton(Canvas: TGfxCanvas;
  const ARect: TRect; Direction: TArrowDirection;
  IsPressed, IsEnabled: Boolean);
const
  Widths: array[TArrowDirection] of Integer = (7, 7, 4, 4);
  Heights: array[TArrowDirection] of Integer = (4, 4, 7, 7);
var
  x, y: Integer;
begin
  with ARect do
  begin
    x := Left + (Right - Left - Widths[Direction]) div 2;
    y := Top + (Bottom - Top - Heights[Direction]) div 2;
  end;

  if IsEnabled then
    SetUIColor(Canvas, clBtnText)
  else
  begin
    SetUIColor(Canvas, cl3DHighlight);
    Canvas.DrawImage(ArrowBitmaps[Direction], x + 1, y + 1);
    SetUIColor(Canvas, cl3DShadow);
  end;
    Canvas.DrawImage(ArrowBitmaps[Direction], x, y);
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
  Revision 1.1  2000/12/23 23:20:15  sg
  * First public CVS version...

}
