{
    $Id$

    fpGFX  -  Free Pascal Graphics Library
    Copyright (C) 2000 - 2001 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    Win32 GDI target implementation

    See the file COPYING, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


unit GFX_GDI;

interface

uses
  Windows,
  SysUtils, Classes,	// FPC units
  GfxBase;		// fpGUI units

const
  WM_MOUSEWHEEL = $020a;

type

  TMessage = packed record
    Msg: Cardinal;
    case Integer of
      0: (
	wParam: WPARAM;
	lParam: LPARAM;
	Result: LRESULT);
      1: (
	wParamLo, wParamHi: Word;
	lParamLo, lParamHi: Word;
	ResultLo, ResultHi: Word);
  end;


  EGDIError = class(EGfxError);

  TGDICanvas = class;
  TGDIDisplay = class;

  TGDIFont = class(TGfxFont)
  public
    constructor Create;
    destructor Destroy; override;
  end;

  PGDICanvasState = ^TGDICanvasState;
  TGDICanvasState = record
    Prev: PGDICanvasState;
    Matrix: TGfxMatrix;
    Color, PenColor, FontColor: TGfxPixel;
  end;

  TGDICanvas = class(TGfxCanvas)
  private
    FHandle: HDC;
    FColor, FBrushColor, FPenColor, FFontColor: TGfxPixel;
    FBrush, FOldBrush: HBRUSH;
    FPen, FOldPen: HPEN;
    FFontMetrics: TTextMetric;
    FStateStackpointer: PGDICanvasState;
    procedure Resized(NewWidth, NewHeight: Integer);
  protected
    procedure NeedBrush;
    procedure NeedPen;
    procedure NeedFont(ANeedFontColor: Boolean);
    procedure NeedFontColor;
  public
    constructor Create(AHandle: HDC);
    destructor Destroy; override;

    procedure SaveState; override;
    procedure RestoreState; override;
    procedure EmptyClipRect; override;
    function ExcludeClipRect(const ARect: TRect): Boolean; override;
    function IntersectClipRect(const ARect: TRect): Boolean; override;
    function UnionClipRect(const ARect: TRect): Boolean; override;
    function GetClipRect: TRect; override;
    function MapColor(const AColor: TGfxColor): TGfxPixel; override;
    procedure SetColor(AColor: TGfxPixel); override;
    procedure SetFont(AFont: TGfxFont); override;
    procedure SetLineStyle(ALineStyle: TGfxLineStyle); override;

    procedure DrawArc(const Rect: TRect; StartAngle, EndAngle: Single); override;
    procedure DrawCircle(const Rect: TRect); override;
    procedure DrawLine(x1, y1, x2, y2: Integer); override;
    procedure FillRect(const Rect: TRect); override;
    function FontCellHeight: Integer; override;
    function TextExtent(const AText: String): TSize; override;
    procedure TextOut(x, y: Integer; const AText: String); override;

    procedure CopyRect(ASource: TGfxCanvas; const ASourceRect: TRect;
      ADestX, ADestY: Integer); override;
    procedure MaskedCopyRect(ASource, AMask: TGfxCanvas;
      const ASourceRect: TRect;
      AMaskX, AMaskY, ADestX, ADestY: Integer); override;
    procedure DrawImageRect(AImage: TGfxImage; ASourceRect: TRect;
      ADestX, ADestY: Integer); override;

    property Handle: HDC read FHandle;
  end;

  TGDIWindowCanvas = class(TGDICanvas)
  private
    FWnd: HWND;
  public
    constructor Create(AWnd: HWND);
    destructor Destroy; override;
  end;

  TGDIBitmapCanvas = class(TGDICanvas)
  private
    FBitmap, FOldBitmap: HBITMAP;
  public
    constructor Create(ABitmap: HBITMAP; AWidth, AHeight: Integer);
    destructor Destroy; override;
    property Bitmap: HBITMAP read FBitmap;
  end;


  TGDIImage = class(TGfxImage)
  private
    FHandle: HBITMAP;
    {$IFDEF Debug}
    IsLocked: Boolean;
    {$ENDIF}
  protected
    FStride: LongWord;
    FData: Pointer;
  public
    constructor Create(AWidth, AHeight: Integer; APixelFormat: TGfxPixelFormat);
    destructor Destroy; override;
    procedure Lock(var AData: Pointer; var AStride: LongWord); override;
    {$IFDEF Debug}
    procedure Unlock; override;
    {$ENDIF}
    property Handle: HBITMAP read FHandle;
    property Stride: LongWord read FStride;
    property Data: Pointer read FData;
  end;


  TGDIScreen = class(TGfxScreen)
  private
    FDisplay: TGDIDisplay;
  public
    constructor Create(ADisplay: TGDIDisplay);
    function CreateBitmap(AWidth, AHeight: Integer): TGfxCanvas; override;
    function CreateMonoBitmap(AWidth, AHeight: Integer): TGfxCanvas; override;
    function CreateWindow(AParent: TGfxWindow;
      AWindowType: TGfxWindowType): TGfxWindow; override;
    property Display: TGDIDisplay read FDisplay;
  end;


  TGDIWindow = class;

  TGDIDisplay = class(TGfxDisplay)
  private
    DoBreakRun: Boolean;
    FWindows: TList;
  public
    constructor Create;
    destructor Destroy; override;
    function CreateFont(const Descriptor: String): TGfxFont; override;
    function CreateImage(AWidth, AHeight: Integer;
      APixelFormat: TGfxPixelFormat): TGfxImage; override;
    procedure Run; override;
    procedure BreakRun; override;
  end;


  TGDIWindow = class(TGfxWindow)
  private
    FHandle: HWND;
    FMinWidth, FMinHeight, FMaxWidth, FMaxHeight: Integer;
    // Messages:
    procedure WMCreate(var Msg: TMessage); message WM_CREATE;
    procedure WMDestroy(var Msg: TMessage); message WM_DESTROY;
    procedure WMGetMinMaxInfo(var Msg: TMessage); message WM_GETMINMAXINFO;
    procedure WMActivate(var Msg: TMessage); message WM_ACTIVATE;
    procedure WMPaint(var Msg: TMessage); message WM_PAINT;
    procedure WMShowWindow(var Msg: TMessage); message WM_SHOWWINDOW;
    procedure WMSize(var Msg: TMessage); message WM_SIZE;
    // Input messages:
    procedure WMLButtonDown(var Msg: TMessage); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Msg: TMessage); message WM_LBUTTONUP;
    procedure WMRButtonDown(var Msg: TMessage); message WM_RBUTTONDOWN;
    procedure WMRButtonUp(var Msg: TMessage); message WM_RBUTTONUP;
    procedure WMMButtonDown(var Msg: TMessage); message WM_MBUTTONDOWN;
    procedure WMMButtonUp(var Msg: TMessage); message WM_MBUTTONUP;
    procedure WMMouseMove(var Msg: TMessage); message WM_MOUSEMOVE;
    procedure WMMouseWheel(var Msg: TMessage); message WM_MOUSEWHEEL;
    procedure WMKeyDown(var Msg: TMessage); message WM_KEYDOWN;
    procedure WMKeyUp(var Msg: TMessage); message WM_KEYUP;
    procedure WMChar(var Msg: TMessage); message WM_CHAR;
  protected
    WindowClass: TWndClass; {!!!: static;}
    FWindowStyle, FWindowStyleEx: LongWord;	// values used for creation
    FMouseInWindow, FHasMouseCapture: Boolean;
    function GetTitle: String; override;
    procedure SetTitle(const ATitle: String); override;
    function DoMouseEnterLeaveCheck(const Msg: TMessage): Boolean;
  private
    constructor Create(AScreen: TGDIScreen; AParent: TGDIWindow;
      AWindowType: TGfxWindowType);
  public
    destructor Destroy; override;
    procedure DefaultHandler(var Message); override;

    procedure SetSize(AWidth, AHeight: Integer); override;
    procedure SetClientSize(AWidth, AHeight: Integer); override;
    procedure SetMinMaxSize(AMinWidth, AMinHeight,
      AMaxWidth, AMaxHeight: Integer); override;
        procedure Show; override;
    procedure Invalidate(const ARect: TRect); override;
    procedure CaptureMouse; override;
    procedure ReleaseMouse; override;


    property Handle: HWND read FHandle;
  end;


function VirtKeyToKeycode(VirtKey: Byte): Word;
function GetKeyboardShiftState: TShiftState;


// ===================================================================
// ===================================================================

implementation

// -------------------------------------------------------------------
//   TGDIFont
// -------------------------------------------------------------------

constructor TGDIFont.Create;
begin
  inherited Create;
  // !!!: Implement this
  WriteLn('Not implemented yet: TGDIFont.Create');
end;

destructor TGDIFont.Destroy;
begin
  WriteLn('Not implemented yet: TGDIFont.Destroy');
  // !!!: Implement this
  inherited Destroy;
end;


// -------------------------------------------------------------------
//   TGDICanvas
// -------------------------------------------------------------------

constructor TGDICanvas.Create(AHandle: HDC);
begin
  inherited Create;
  FHandle := AHandle;
  ASSERT(Handle <> 0);
  Windows.SelectObject(Handle, Windows.GetStockObject(DEFAULT_GUI_FONT));
  Windows.GetTextMetrics(Handle, @FFontMetrics);
  Windows.SetBkMode(Handle, TRANSPARENT);
end;

destructor TGDICanvas.Destroy;
begin
  if FBrush <> 0 then
  begin
    Windows.SelectObject(Handle, FOldBrush);
    Windows.DeleteObject(FBrush);
  end;
  if FPen <> 0 then
  begin
    Windows.SelectObject(Handle, FOldPen);
    Windows.DeleteObject(FPen);
  end;
  inherited Destroy;
end;

procedure TGDICanvas.SaveState;
var
  SavedState: PGDICanvasState;
  NewRegion: HRGN;
begin
  New(SavedState);
  SavedState^.Prev := FStateStackpointer;
  SavedState^.Matrix := Matrix;
  SavedState^.Color := FColor;
  SavedState^.PenColor := FPenColor;
  SavedState^.FontColor := FFontColor;
  FStateStackpointer := SavedState;
  Windows.SaveDC(Handle);
end;

procedure TGDICanvas.RestoreState;
var
  SavedState: PGDICanvasState;
begin
  Windows.RestoreDC(Handle, -1);

  SavedState := FStateStackpointer;
  FStateStackpointer := SavedState^.Prev;
  Matrix := SavedState^.Matrix;
  FFontColor := SavedState^.FontColor;
  FPenColor := SavedState^.PenColor;
  FColor := SavedState^.Color;
  Dispose(SavedState);
end;

procedure TGDICanvas.EmptyClipRect;
begin
  Windows.IntersectClipRect(Handle, 0, 0, 0, 0);
end;

function TGDICanvas.ExcludeClipRect(const ARect: TRect): Boolean;
var
  x1, y1, x2, y2: Integer;
begin
  Transform(ARect.Left, ARect.Top, x1, y1);
  Transform(ARect.Right, ARect.Bottom, x2, y2);
  Result := Windows.ExcludeClipRect(Handle, x1, y1, x2, y2) <> NULLREGION;
end;

function TGDICanvas.IntersectClipRect(const ARect: TRect): Boolean;
var
  x1, y1, x2, y2: Integer;
begin
  Transform(ARect.Left, ARect.Top, x1, y1);
  Transform(ARect.Right, ARect.Bottom, x2, y2);

  if (x2 > x1) and (y2 > y1) then
    Result := Windows.IntersectClipRect(Handle, x1, y1, x2, y2) <> NULLREGION
  else
    Result := False;
end;

function TGDICanvas.UnionClipRect(const ARect: TRect): Boolean;
var
  x1, y1, x2, y2: Integer;
  Region: HRGN;
begin
  Transform(ARect.Left, ARect.Top, x1, y1);
  Transform(ARect.Right, ARect.Bottom, x2, y2);

  if (x2 > x1) and (y2 > y1) then
  begin
    Region := Windows.CreateRectRgn(x1, y1, x2, y2);
    Result := Windows.ExtSelectClipRgn(Handle, Region, RGN_OR) <> NULLREGION;
    Windows.DeleteObject(Region);
  end else
    Result := False;
end;

function TGDICanvas.GetClipRect: TRect;
var
  Rect: Windows.Rect;
begin
  Windows.GetClipBox(Handle, Rect);
  ReverseTransform(Rect.Left, Rect.Top, Result.Left, Result.Top);
  ReverseTransform(Rect.Right, Rect.Bottom, Result.Right, Result.Bottom);
end;

function TGDICanvas.MapColor(const AColor: TGfxColor): TGfxPixel;
begin
{  Result := Windows.GetNearestColor(Handle, RGB(AColor.Red div 257,
    AColor.Green div 257, AColor.Blue div 257));}
  Result := RGB(AColor.Red div 257, AColor.Green div 257, AColor.Blue div 257);
end;

procedure TGDICanvas.SetColor(AColor: TGfxPixel);
begin
  FColor := AColor;
end;

procedure TGDICanvas.SetFont(AFont: TGfxFont);
begin
  WriteLn('Not implemented yet: TGDICanvas.SetFont');
  // !!!: Implement this
end;

procedure TGDICanvas.SetLineStyle(ALineStyle: TGfxLineStyle);
begin
  WriteLn('Not implemented yet: TGDICanvas.SetLineStyle');
  // !!!: Implement this
end;

procedure TGDICanvas.DrawArc(const Rect: TRect; StartAngle, EndAngle: Single);
var
  x1, y1, x2, y2: Integer;
begin
  Transform(Rect.Left, Rect.Top, x1, y1);
  Transform(Rect.Right, Rect.Bottom, x2, y2);
  WriteLn('Not implemented yet: TGDICanvas.DrawArc');
  // !!!: Implement this
end;

procedure TGDICanvas.DrawCircle(const Rect: TRect);
var
  x1, y1, x2, y2: Integer;
begin
  Transform(Rect.Left, Rect.Top, x1, y1);
  Transform(Rect.Right, Rect.Bottom, x2, y2);
  WriteLn('Not implemented yet: TGDICanvas.DrawCircle');
  // !!!: Implement this
end;

procedure TGDICanvas.DrawLine(x1, y1, x2, y2: Integer);
begin
  Transform(x1, y1, x1, y1);
  Transform(x2, y2, x2, y2);
  NeedPen;
  Windows.MoveToEx(Handle, x1, y1, nil);
  Windows.LineTo(Handle, x2, y2);
end;

procedure TGDICanvas.FillRect(const Rect: TRect);
var
  r: Windows.Rect;
begin
  Transform(Rect.Left, Rect.Top, r.Left, r.Top);
  Transform(Rect.Right, Rect.Bottom, r.Right, r.Bottom);
  NeedBrush;
  Windows.FillRect(Handle, r, FBrush);
end;

function TGDICanvas.FontCellHeight: Integer;
begin
  Result := FFontMetrics.tmHeight;
end;

function TGDICanvas.TextExtent(const AText: String): TSize;
begin
  Windows.GetTextExtentPoint32(Handle, PChar(AText), Length(AText), @Result);
end;

procedure TGDICanvas.TextOut(x, y: Integer; const AText: String);
begin
  Transform(x, y, x, y);
  NeedFont(True);
  Windows.TextOut(Handle, x, y, PChar(AText), Length(AText));
end;

procedure TGDICanvas.CopyRect(ASource: TGfxCanvas; const ASourceRect: TRect;
  ADestX, ADestY: Integer);
var
  SourceLeft, SourceTop, SourceRight, SourceBottom: Integer;
begin
  if not ASource.InheritsFrom(TGDICanvas) then
    raise EGDIError.CreateFmt(SIncompatibleCanvasForBlitting,
      [ASource.ClassName, Self.ClassName]);

  ASource.Transform(ASourceRect.Left, ASourceRect.Top, SourceLeft, SourceTop);
  ASource.Transform(ASourceRect.Right, ASourceRect.Bottom,
    SourceRight, SourceBottom);
  Transform(ADestX, ADestY, ADestX, ADestY);

  Windows.BitBlt(
    Handle, ADestX, ADestY, SourceRight - SourceLeft, SourceBottom - SourceTop,
    TGDICanvas(ASource).Handle, SourceLeft, SourceTop,
    SRCCOPY);
end;

procedure TGDICanvas.MaskedCopyRect(ASource, AMask: TGfxCanvas;
  const ASourceRect: TRect; AMaskX, AMaskY, ADestX, ADestY: Integer);
var
  SourceLeft, SourceTop, SourceRight, SourceBottom, w, h: Integer;
  SourceBitmap, AndObjectBitmap, AndMemBitmap, SaveBitmap,
    OldSourceBitmap, OldAndObjectBitmap, OldAndMemBitmap,
    OldSaveBitmap: HBITMAP;
  SourceDC, MemDC, ObjectDC, SaveDC: HDC;
begin
  if not ASource.InheritsFrom(TGDICanvas) then
    raise EGDIError.CreateFmt(SIncompatibleCanvasForBlitting,
      [ASource.ClassName, Self.ClassName]);

  if not AMask.InheritsFrom(TGDICanvas) then
    raise EGDIError.CreateFmt(SIncompatibleCanvasForBlitting,
      [AMask.ClassName, Self.ClassName]);

  ASource.Transform(ASourceRect.Left, ASourceRect.Top, SourceLeft, SourceTop);
  ASource.Transform(ASourceRect.Right, ASourceRect.Bottom,
    SourceRight, SourceBottom);
  AMask.Transform(AMaskX, AMaskY, AMaskX, AMaskY);
  Transform(ADestX, ADestY, ADestX, ADestY);
  w := SourceRight - SourceLeft;
  h := SourceBottom - SourceTop;

  // See http://support.microsoft.com/support/kb/articles/Q79/2/12.ASP

  SourceDC := Windows.CreateCompatibleDC(Handle);
  ObjectDC := Windows.CreateCompatibleDC(Handle);
  MemDC := Windows.CreateCompatibleDC(Handle);
  SourceBitmap := Windows.CreateCompatibleBitmap(Handle, w, h);
  AndObjectBitmap := Windows.CreateCompatibleBitmap(ObjectDC, w, h);
  AndMemBitmap := Windows.CreateCompatibleBitmap(Handle, w, h);
  OldSourceBitmap := Windows.SelectObject(SourceDC, SourceBitmap);
  OldAndObjectBitmap := Windows.SelectObject(ObjectDC, AndObjectBitmap);
  OldAndMemBitmap := Windows.SelectObject(MemDC, AndMemBitmap);

  Windows.BitBlt(SourceDC, 0, 0, w, h,
    TGDICanvas(ASource).Handle, SourceLeft, SourceTop, SRCCOPY);
  Windows.BitBlt(MemDC, 0, 0, w, h, Handle, ADestX, ADestY, SRCCOPY);

  // !!!: Find a ROP for replacing the following 2 Blits with a single one:
  Windows.BitBlt(ObjectDC, 0, 0, w, h,
    TGDICanvas(AMask).Handle, AMaskX, AMaskY, NOTSRCCOPY);
  Windows.BitBlt(MemDC, 0, 0, w, h, ObjectDC, 0, 0, SRCAND);

  Windows.BitBlt(SourceDC, 0, 0, w, h,
    TGDICanvas(AMask).Handle, AMaskX, AMaskY, SRCAND);
  Windows.BitBlt(MemDC, 0, 0, w, h, SourceDC, 0, 0, SRCPAINT);
  // Copy the result to the screen
  Windows.BitBlt(Handle, ADestX, ADestY, w, h, MemDC, 0, 0, SRCCOPY);

  // Clean up
  Windows.DeleteObject(Windows.SelectObject(ObjectDC, OldAndObjectBitmap));
  Windows.DeleteObject(Windows.SelectObject(MemDC, OldAndMemBitmap));
  Windows.DeleteObject(Windows.SelectObject(SourceDC, OldSourceBitmap));
  Windows.DeleteDC(MemDC);
  Windows.DeleteDC(ObjectDC);
  Windows.DeleteDC(SourceDC);
end;

procedure TGDICanvas.DrawImageRect(AImage: TGfxImage; ASourceRect: TRect;
  ADestX, ADestY: Integer);
var
  MemDC: HDC;
  OldBitmap: HBITMAP;
  GDIPal: PRGBQUAD;
  i: Integer;
begin
  ASSERT(AImage.InheritsFrom(TGDIImage));
  {$IFDEF Debug}
  ASSERT(not TGDIImage(AImage).IsLocked);
  {$ENDIF}

  Transform(ADestX, ADestY, ADestX, ADestY);

  MemDC := Windows.CreateCompatibleDC(Handle);
  OldBitmap := Windows.SelectObject(MemDC, TGDIImage(AImage).Handle);

  // Set the color palette, if present
  if Assigned(AImage.Palette) then
  begin
    GetMem(GDIPal, AImage.Palette.EntryCount * SizeOf(RGBQUAD));
    for i := 0 to AImage.Palette.EntryCount - 1 do
      with AImage.Palette.Entries[i] do
      begin
        GDIPal[i].rgbRed := Red div 257;
        GDIPal[i].rgbGreen := Green div 257;
        GDIPal[i].rgbBlue := Blue div 257;
        GDIPal[i].rgbReserved := 0;
      end;
    Windows.SetDIBColorTable(MemDC, 0, AImage.Palette.EntryCount, GDIPal^);
    FreeMem(GDIPal);
  end;

  with ASourceRect do
    Windows.BitBlt(Handle, ADestX, ADestY, Right - Left, Bottom - Top,
      MemDC, Left, Top, SRCCOPY);

  Windows.SelectObject(MemDC, OldBitmap);
  Windows.DeleteDC(MemDC);
end;


procedure TGDICanvas.NeedBrush;
begin
  if (FBrush = 0) or (FBrushColor <> FColor) then
  begin
    if FBrush <> 0 then
    begin
      SelectObject(Handle, FOldBrush);
      DeleteObject(FBrush);
    end;
    FBrushColor := FColor;
    FBrush := CreateSolidBrush(FBrushColor);
    FOldBrush := SelectObject(Handle, FBrush);
  end;
end;

procedure TGDICanvas.NeedPen;
begin
  if (FPen = 0) or (FPenColor <> FColor) then
  begin
    if FPen <> 0 then
    begin
      SelectObject(Handle, FOldPen);
      DeleteObject(FPen);
    end;
    FPenColor := FColor;
    FPen := CreatePen(PS_SOLID, 0, FPenColor);
    FOldPen := SelectObject(Handle, FPen);
  end;
end;

procedure TGDICanvas.NeedFont(ANeedFontColor: Boolean);
begin
  if ANeedFontColor then
    NeedFontColor;
end;

procedure TGDICanvas.NeedFontColor;
begin
  if FFontColor <> FColor then
  begin
    FFontColor := FColor;
    SetTextColor(Handle, FFontColor);
  end;
end;

procedure TGDICanvas.Resized(NewWidth, NewHeight: Integer);
begin
  FWidth := NewWidth;
  FHeight := NewHeight;
end;


// -------------------------------------------------------------------
//   TGDIWindowCanvas
// -------------------------------------------------------------------

constructor TGDIWindowCanvas.Create(AWnd: HWND);
begin
  FWnd := AWnd;
  inherited Create(Windows.GetDC(FWnd));
end;

destructor TGDIWindowCanvas.Destroy;
begin
  inherited Destroy;
  if Handle <> 0 then
    Windows.ReleaseDC(FWnd, Handle);
end;


// -------------------------------------------------------------------
//   TGDIBitmapCanvas
// -------------------------------------------------------------------

constructor TGDIBitmapCanvas.Create(ABitmap: HBITMAP; AWidth, AHeight: Integer);
begin
  ASSERT(ABitmap <> 0);
  FBitmap := ABitmap;
  inherited Create(Windows.CreateCompatibleDC(0));
  FWidth := AWidth;
  FHeight := AHeight;
  FOldBitmap := Windows.SelectObject(Handle, Bitmap);
end;

destructor TGDIBitmapCanvas.Destroy;
begin
  Windows.SelectObject(Handle, FOldBitmap);
  Windows.DeleteObject(Bitmap);
  Windows.DeleteDC(Handle);
  inherited Destroy;
end;


// -------------------------------------------------------------------
//   TGDIImage
// -------------------------------------------------------------------

constructor TGDIImage.Create(AWidth, AHeight: Integer;
  APixelFormat: TGfxPixelFormat);
var
  BitmapInfo: PBitmapInfo;
  Color: PRGBQUAD;
begin
  inherited Create(AWidth, AHeight, APixelFormat);

  case APixelFormat.FormatType of
    ftMono:
      begin
        FStride := (AWidth + 7) shr 3;
	GetMem(BitmapInfo, SizeOf(TBitmapInfoHeader) + 2 * SizeOf(RGBQUAD));
	BitmapInfo^.bmiHeader.biClrUsed := 2;
	Color := @BitmapInfo^.bmiColors[0];
	Color^.rgbRed := 0;
	Color^.rgbGreen := 0;
	Color^.rgbBlue := 0;
	Color^.rgbReserved := 0;
	Inc(Color);
	Color^.rgbRed := 255;
	Color^.rgbGreen := 255;
	Color^.rgbBlue := 255;
	Color^.rgbReserved := 0;
      end;
    ftPal4, ftPal4A:
      begin
        FStride := (AWidth + 1) shr 1;
        GetMem(BitmapInfo, SizeOf(TBitmapInfoHeader) + 16 * SizeOf(RGBQUAD));
        BitmapInfo^.bmiHeader.biClrUsed := 0;
      end;
    ftPal8, ftPal8A:
      begin
        FStride := AWidth;
        GetMem(BitmapInfo, SizeOf(TBitmapInfoHeader) + 256 * SizeOf(RGBQUAD));
        BitmapInfo^.bmiHeader.biClrUsed := 0;
      end;
    else
    begin
      FStride := AWidth * (FormatTypeBPPTable[APixelFormat.FormatType] shr 3);
      GetMem(BitmapInfo, SizeOf(TBitmapInfoHeader));
      BitmapInfo^.bmiHeader.biClrUsed := 0;
    end;
  end;
  // The stride is always a multiple of 4
  FStride := (FStride + 3) and not 3;

  with BitmapInfo^.bmiHeader do
  begin
    biSize := SizeOf(TBitmapInfoHeader);
    biWidth := AWidth;
    biHeight := -AHeight;
    biPlanes := 1;
    biBitCount := FormatTypeBPPTable[APixelFormat.FormatType];
    biCompression := 0;
    biSizeImage := 0;
    biXPelsPerMeter := 0;
    biYPelsPerMeter := 0;
    biClrImportant := 0;
  end;

  FData := nil;
  FHandle := Windows.CreateDIBSection(0, BitmapInfo^, DIB_RGB_COLORS,
    FData, 0, 0);
  FreeMem(BitmapInfo);
end;

destructor TGDIImage.Destroy;
begin
  if Handle <> 0 then
    Windows.DeleteObject(Handle);
  inherited Destroy;
end;

procedure TGDIImage.Lock(var AData: Pointer; var AStride: LongWord);
begin
  {$IFDEF Debug}
  ASSERT(not IsLocked);
  IsLocked := True;
  {$ENDIF}
  AData := Data;
  AStride := Stride;
  Windows.GdiFlush;
end;

{$IFDEF Debug}
procedure TGDIImage.Unlock;
begin
  ASSERT(IsLocked);
  IsLocked := False;
end;
{$ENDIF}


// -------------------------------------------------------------------
//   TGDIScreen
// -------------------------------------------------------------------

constructor TGDIScreen.Create(ADisplay: TGDIDisplay);
begin
  inherited Create(ADisplay);
  FDisplay := ADisplay;
end;

function TGDIScreen.CreateBitmap(AWidth, AHeight: Integer): TGfxCanvas;
var
  TempDC: HDC;
begin
  TempDC := Windows.GetDC(0);
  Result := TGDIBitmapCanvas.Create(
    Windows.CreateCompatibleBitmap(TempDC, AWidth, AHeight), AWidth, AHeight);
  Windows.ReleaseDC(0, TempDC);
end;

function TGDIScreen.CreateMonoBitmap(AWidth, AHeight: Integer): TGfxCanvas;
var
  TempDC: HDC;
begin
  TempDC := Windows.CreateCompatibleDC(0);
  Result := TGDIBitmapCanvas.Create(
    Windows.CreateCompatibleBitmap(TempDC, AWidth, AHeight), AWidth, AHeight);
  Windows.DeleteDC(TempDC);
end;

function TGDIScreen.CreateWindow(AParent: TGfxWindow;
  AWindowType: TGfxWindowType): TGfxWindow;
begin
  Result := TGDIWindow.Create(Self, AParent as TGDIWindow, AWindowType);
  if not Assigned(Display.FWindows) then
    Display.FWindows := TList.Create;
  Display.FWindows.Add(Result);
end;


// -------------------------------------------------------------------
//   TGDIDisplay
// -------------------------------------------------------------------

constructor TGDIDisplay.Create;
begin
  inherited Create;
  FDefaultScreen := TGDIScreen.Create(Self);
end;

destructor TGDIDisplay.Destroy;
var
  i: Integer;
begin
  if Assigned(FWindows) then
  begin
    for i := 0 to FWindows.Count - 1 do
      TGDIWindow(FWindows[i]).Free;
    FWindows.Free;
  end;
  DefaultScreen.Free;
  inherited Destroy;
end;

function TGDIDisplay.CreateFont(const Descriptor: String): TGfxFont;
begin
  Result := TGDIFont.Create;
end;

function TGDIDisplay.CreateImage(AWidth, AHeight: Integer;
  APixelFormat: TGfxPixelFormat): TGfxImage;
begin
  Result := TGDIImage.Create(AWidth, AHeight, APixelFormat);
end;

procedure TGDIDisplay.Run;
var
  Msg: TMsg;
begin
  if not Assigned(FWindows) then
    exit;

  DoBreakRun := False;
  while Windows.GetMessage(@Msg, 0, 0, 0) and not DoBreakRun do
  begin
    Windows.TranslateMessage(@msg);
    Windows.DispatchMessage(@msg);
  end;
end;

procedure TGDIDisplay.BreakRun;
begin
  DoBreakRun := True;
end;


// -------------------------------------------------------------------
//   TGDIWindow
// -------------------------------------------------------------------

function fpGFXWindowProc(hwnd: HWND; uMsg: UINT; wParam: WPARAM;
  lParam: LPARAM): LRESULT; stdcall;
var
  Window: TGDIWindow;
  Msg: TMessage;
begin
  if uMsg = WM_CREATE then
  begin
    Window := TGDIWindow(PCreateStruct(lParam)^.lpCreateParams);
    Window.FHandle := hwnd;
    Windows.SetWindowLong(hwnd, GWL_USERDATA, LongWord(Window));
  end else
    Window := TGDIWindow(Windows.GetWindowLong(hwnd, GWL_USERDATA));

  if Assigned(Window) then
  begin
    Msg.msg := uMsg;
    Msg.wParam := wParam;
    Msg.lParam := lParam;
    Msg.Result := 0;
    Window.Dispatch(Msg);
    Result := Msg.Result;
  end else
    Result := Windows.DefWindowProc(hwnd, uMsg, wParam, lParam);
end;

function TGDIWindow.GetTitle: String;
var
  l: Integer;
begin
  l := Windows.GetWindowTextLength(Handle);
  SetLength(Result, l);
  Windows.GetWindowText(Handle, @Result[1], l);
end;

procedure TGDIWindow.SetTitle(const ATitle: String);
begin
  Windows.SetWindowText(Handle, PChar(ATitle));
end;

function TGDIWindow.DoMouseEnterLeaveCheck(const Msg: TMessage): Boolean;
begin
  if not FMouseInWindow then
  begin
    FMouseInWindow := True;
    Windows.SetCapture(Handle);
    if Assigned(OnMouseEnter) then
      OnMouseEnter(Self, GetKeyboardShiftState, Msg.lParamLo, Msg.lParamHi);
    Result := Msg.Msg <> WM_MOUSEMOVE;
  end else
  begin
    if (Msg.lParamLo < 0) or (Msg.lParamHi < 0) or
      (Msg.lParamLo >= ClientWidth) or (Msg.lParamHi >= ClientHeight) then
      FMouseInWindow := False;

    if (not FHasMouseCapture) and (not FMouseInWindow) then
    begin
      Windows.ReleaseCapture;
      if Assigned(OnMouseLeave) then
        OnMouseLeave(Self);
      Result := False;
    end else
      Result := True;
  end;
end;

constructor TGDIWindow.Create(AScreen: TGDIScreen; AParent: TGDIWindow;
  AWindowType: TGfxWindowType);
const
  WindowStyleTable: array[TGfxWindowType] of LongWord = (
    WS_OVERLAPPEDWINDOW,	// wtWindow
    WS_OVERLAPPED,		// wtBorderlessWindow
    WS_POPUPWINDOW,		// wtPopup
    WS_POPUP,			// wtBorderlessPopup
    WS_OVERLAPPED,		// wtToolWindow
    WS_CHILDWINDOW);		// wtChild

  WindowStyleExTable: array[TGfxWindowType] of LongWord = (
    WS_EX_APPWINDOW,		// wtWindow
    WS_EX_APPWINDOW,		// wtBorderlessWindow
    0,				// wtPopup
    0,				// wtBorderlessPopup
    WS_EX_PALETTEWINDOW,	// wtToolWindow
    0);				// wtChild

var
  ParentHandle: HWND;
begin
  inherited Create;
  FScreen := AScreen;

  // Initialize a window class, if necessary
  if not Assigned(WindowClass.lpfnWndProc) then
  begin
    with WindowClass do
    begin
      style := CS_HREDRAW or
 CS_VREDRAW;
      lpfnWndProc := WndProc(@fpGFXWindowProc);
      hInstance := MainInstance;
      hIcon := LoadIcon(0, IDI_APPLICATION);
      hCursor := LoadCursor(0, IDC_ARROW);
      hbrBackground := 0;
      lpszClassName := 'fpGFX';
    end;
    Windows.RegisterClass(@WindowClass);
  end;

  if Assigned(AParent) then
    ParentHandle := AParent.Handle
  else
    ParentHandle := 0;

  FWindowStyle := WindowStyleTable[AWindowType];
  FWindowStyleEx := WindowStyleExTable[AWindowType];

  FHandle := Windows.CreateWindowEx(
    FWindowStyleEx,			// extended window style
    'fpGFX',				// registered class name
    'fpGFX Window',			// window name
    FWindowStyle,			// window style
    CW_USEDEFAULT,			// horizontal position of window
    CW_USEDEFAULT,			// vertical position of window
    CW_USEDEFAULT,			// window width
    CW_USEDEFAULT,			// window height
    ParentHandle,			// handle to parent or owner window
    0,					// menu handle or child identifier
    MainInstance,			// handle to application instance
    Self);				// window-creation data

  FCanvas := TGDIWindowCanvas.Create(Handle);
end;

destructor TGDIWindow.Destroy;
var
  OldHandle: HWND;
begin
  if Assigned(OnClose) then
    OnClose(Self);

  Canvas.Free;

  if Handle <> 0 then
  begin
    OldHandle := Handle;
    FHandle := 0;
    Windows.DestroyWindow(OldHandle);
  end;

  TGDIDisplay(Screen.Display).FWindows.Remove(Self);

  // Are we the last window for our owning display?
  if TGDIDisplay(Screen.Display).FWindows.Count = 0 then
    Windows.PostQuitMessage(0);

  inherited Destroy;
end;

procedure TGDIWindow.DefaultHandler(var Message);
begin
  TMessage(Message).Result := Windows.DefWindowProc(Handle,
    TMessage(Message).Msg, TMessage(Message).wParam, TMessage(Message).lParam);
end;

procedure TGDIWindow.SetSize(AWidth, AHeight: Integer);
begin
  if (AWidth <> Width) or (AHeight <> Height) then
    Windows.SetWindowPos(Handle, 0, 0, 0, AWidth, AHeight,
      SWP_NOMOVE or SWP_NOZORDER);
end;

procedure TGDIWindow.SetClientSize(AWidth, AHeight: Integer);
var
  Rect: Windows.Rect;
begin
  if (AWidth <> ClientWidth) or (AHeight <> ClientHeight) then
  begin
    Rect.Left := 0;
    Rect.Top := 0;
    Rect.Right := AWidth;
    Rect.Bottom := AHeight;
    Windows.AdjustWindowRectEx(Rect, FWindowStyle, False, FWindowStyleEx);
    SetSize(Rect.Right - Rect.Left, Rect.Bottom - Rect.Top);
  end;
end;

procedure TGDIWindow.SetMinMaxSize(AMinWidth, AMinHeight,
  AMaxWidth, AMaxHeight: Integer);
begin
  FMinWidth := AMinWidth;
  FMinHeight := AMinHeight;
  FMaxWidth := AMaxWidth;
  FMaxHeight := AMaxHeight;
end;

procedure TGDIWindow.Show;
begin
  Windows.ShowWindow(Handle, SW_SHOWNORMAL);
  Windows.UpdateWindow(Handle);
end;

procedure TGDIWindow.Invalidate(const ARect: TRect);
var
  Rect: Windows.Rect;
begin
  Rect.Left := ARect.Left;
  Rect.Top := ARect.Top;
  Rect.Right := ARect.Right;
  Rect.Bottom := ARect.Bottom;
  Windows.InvalidateRect(Handle, Rect, False);
end;

procedure TGDIWindow.CaptureMouse;
begin
  if FHasMouseCapture then
    exit;

  FHasMouseCapture := True;

  if not FMouseInWindow then
  begin
    FMouseInWindow := True;
    Windows.SetCapture(Handle);
  end;
end;

procedure TGDIWindow.ReleaseMouse;
begin
  if FHasMouseCapture then
  begin
    FHasMouseCapture := False;
    if not FMouseInWindow then
      Windows.ReleaseCapture;
  end;
end;


// Message handlers

procedure TGDIWindow.WMCreate(var Msg: TMessage);
begin
  if Assigned(OnCreate) then
    OnCreate(Self);
end;

procedure TGDIWindow.WMDestroy(var Msg: TMessage);
begin
  if Handle <> 0 then
    Self.Free;
end;

procedure TGDIWindow.WMGetMinMaxInfo(var Msg: TMessage);
begin
  with PMinMaxInfo(Msg.lParam)^ do
  begin
    if FMinWidth > 0 then
      ptMinTrackSize.x := FMinWidth;
    if FMinHeight > 0 then
      ptMinTrackSize.y := FMinHeight;
    if FMaxWidth > 0 then
      ptMaxTrackSize.x := FMaxWidth;
    if FMaxHeight > 0 then
      ptMaxTrackSize.y := FMaxHeight;
  end;
end;

procedure TGDIWindow.WMActivate(var Msg: TMessage);
begin
  if Msg.wParam = WA_INACTIVE then
  begin
    if Assigned(OnFocusOut) then
      OnFocusOut(Self);
  end else
    if Assigned(OnFocusIn) then
      OnFocusIn(Self);
end;

procedure TGDIWindow.WMPaint(var Msg: TMessage);
var
  PaintStruct: TPaintStruct;
  r: TRect;
  OldCanvas: TGfxCanvas;
begin
  Windows.BeginPaint(Handle, @PaintStruct);
  if Assigned(OnPaint) then
  begin
    with PaintStruct.rcPaint do
    begin
      r.Left := Left;
      r.Top := Top;
      r.Right := Right;
      r.Bottom := Bottom;
    end;
    OldCanvas := Canvas;
    FCanvas := TGDICanvas.Create(PaintStruct.hdc);
    OnPaint(Self, r);
    Canvas.Free;
    FCanvas := OldCanvas;
  end;
  Windows.EndPaint(Handle, @PaintStruct);
end;

procedure TGDIWindow.WMShowWindow(var Msg: TMessage);
begin
  if Msg.wParam <> 0 then
  begin
    if Assigned(OnFocusIn) then
      OnFocusIn(Self);
    if Assigned(OnShow) then
      OnShow(Self);
  end else
    if Assigned(OnHide) then
      OnHide(Self);
end;

procedure TGDIWindow.WMSize(var Msg: TMessage);
var
  r: Windows.Rect;
begin
  if (ClientWidth <> Msg.lParamLo) or (ClientHeight <> Msg.lParamHi) then
  begin
    Windows.GetWindowRect(Handle, r);
    FWidth := r.Right - r.Left;
    FHeight := r.Bottom - r.Top;
    Windows.GetClientRect(Handle, r);
    FClientWidth := Msg.lParamLo;
    FClientHeight := Msg.lParamHi;
    TGDICanvas(Canvas).Resized(FWidth, FHeight);
    if Assigned(OnResize) then
      OnResize(Self);
  end;
end;

procedure TGDIWindow.WMLButtonDown(var Msg: TMessage);
begin
  if DoMouseEnterLeaveCheck(Msg) and Assigned(OnMousePressed) then
    OnMousePressed(Self, mbLeft, GetKeyboardShiftState,
      Msg.lParamLo, Msg.lParamHi);
end;

procedure TGDIWindow.WMLButtonUp(var Msg: TMessage);
begin
  if DoMouseEnterLeaveCheck(Msg) and Assigned(OnMouseReleased) then
    OnMouseReleased(Self, mbLeft, GetKeyboardShiftState,
      Msg.lParamLo, Msg.lParamHi);
end;

procedure TGDIWindow.WMRButtonDown(var Msg: TMessage);
begin
  if DoMouseEnterLeaveCheck(Msg) and Assigned(OnMousePressed) then
    OnMousePressed(Self, mbRight, GetKeyboardShiftState,
      Msg.lParamLo, Msg.lParamHi);
end;

procedure TGDIWindow.WMRButtonUp(var Msg: TMessage);
begin
  if DoMouseEnterLeaveCheck(Msg) and Assigned(OnMouseReleased) then
    OnMouseReleased(Self, mbRight, GetKeyboardShiftState,
      Msg.lParamLo, Msg.lParamHi);
end;

procedure TGDIWindow.WMMButtonDown(var Msg: TMessage);
begin
  if DoMouseEnterLeaveCheck(Msg) and Assigned(OnMousePressed) then
    OnMousePressed(Self, mbMiddle, GetKeyboardShiftState,
      Msg.lParamLo, Msg.lParamHi);
end;

procedure TGDIWindow.WMMButtonUp(var Msg: TMessage);
begin
  if DoMouseEnterLeaveCheck(Msg) and Assigned(OnMouseReleased) then
    OnMouseReleased(Self, mbMiddle, GetKeyboardShiftState,
      Msg.lParamLo, Msg.lParamHi);
end;

procedure TGDIWindow.WMMouseMove(var Msg: TMessage);
begin
  if DoMouseEnterLeaveCheck(Msg) and Assigned(OnMouseMove) then
    OnMouseMove(Self, GetKeyboardShiftState, Msg.lParamLo, Msg.lParamHi);
end;

procedure TGDIWindow.WMMouseWheel(var Msg: TMessage);
begin
  if DoMouseEnterLeaveCheck(Msg) and Assigned(OnMouseWheel) then
    OnMouseWheel(Self, GetKeyboardShiftState, SmallInt(Msg.wParamHi) / -120.0,
      Msg.lParamLo, Msg.lParamHi);
end;

procedure TGDIWindow.WMKeyDown(var Msg: TMessage);
begin
  if Assigned(OnKeyPressed) then
    OnKeyPressed(Self, VirtKeyToKeycode(Msg.wParam), GetKeyboardShiftState);
end;

procedure TGDIWindow.WMKeyUp(var Msg: TMessage);
begin
  if Assigned(OnKeyReleased) then
    OnKeyReleased(Self, VirtKeyToKeycode(Msg.wParam), GetKeyboardShiftState);
end;

procedure TGDIWindow.WMChar(var Msg: TMessage);
begin
  if Assigned(OnKeyChar) then
    OnKeyChar(Self, Chr(Msg.wParam));
end;


// -------------------------------------------------------------------
//   Helpers
// -------------------------------------------------------------------

{$INCLUDE gdikeys.inc}


end.


{
  $Log$
  Revision 1.4  2001/01/18 15:00:14  sg
  * Added TGfxWindowType and implemented support for it

  Revision 1.3  2001/01/11 23:07:24  sg
  *** empty log message ***

  Revision 1.2  2000/12/23 23:07:24  sg
  *** empty log message ***

  Revision 1.1  2000/10/28 20:30:50  sg
  * First version (NOT compilable at the moment, as the sources haven't been
    adapted to recent interfaces changes yet!)

}
