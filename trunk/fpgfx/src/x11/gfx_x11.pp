{
    $Id$

    fpGFX  -  Free Pascal Graphics Library
    Copyright (C) 2000 - 2001 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    X11/XLib target implementation

    See the file COPYING, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


unit GFX_X11;

{$IFDEF Debug}
{$ASSERTIONS On}
{$ENDIF}

interface

uses
  SysUtils, Classes,	// FPC units
  X, XLib, XUtil,	// X11 units
  GfxBase,		// fpGFX units
  GELDirty;		// fpGFX emulation layer

resourcestring
  // X11 exception strings
  SGCCreationFailed = 'Creation of X11 graphics context failed';
  SXCanvasInvalidFontClass = 'Tried to set font of class "%s" into X11 context; only TXFont is allowed.';
  SOpenDisplayFailed = 'Opening of display "%s" failed';
  SWindowCreationFailed = 'Creation of X11 window failed';
  SWindowUnsupportedPixelFormat = 'Window uses unsupported pixel format: %d bits per pixel';
  SNoDefaultFont = 'Unable to load default font';
  SIncompatibleCanvasForBlitting = 'Cannot blit from %s to %s';

type

  EX11Error = class(EGfxError);

  TXCanvas = class;
  TXDisplay = class;

  TXFont = class(TGfxFont)
  private
    FDisplayHandle: PDisplay;
    FFontStruct: PXFontStruct;
  public
    constructor Create(ADisplayHandle: PDisplay; const Descriptor: String);
    destructor Destroy; override;
    property DisplayHandle: PDisplay read FDisplayHandle;
    property FontStruct: PXFontStruct read FFontStruct;
  end;

  PXCanvasState = ^TXCanvasState;
  TXCanvasState = record
    Prev: PXCanvasState;
    Matrix: TGfxMatrix;
    Region: TRegion;
    Color: TGfxPixel;
    Font: TGfxFont;
  end;

  TXCanvas = class(TGfxCanvas)
  private
    FDisplay: TXDisplay;
    FDisplayHandle: PDisplay;
    FHandle: X.TDrawable;
    FGC: TGC;
    FVisual: PVisual;
    FRegion: TRegion;
    FDefaultFont: PXFontStruct;
    FFontStruct: PXFontStruct;
    FStateStackpointer: PXCanvasState;
    FColormap: TColormap;
    FCurColor: TGfxPixel;
    FFont: TGfxFont;
    procedure Resized(NewWidth, NewHeight: Integer);
  public
    constructor Create(AColormap: TColormap; ADisplay: TXDisplay;
      AXDrawable: X.TDrawable; ADefaultFont: PXFontStruct);
    destructor Destroy; override;

    procedure SaveState; override;
    procedure RestoreState; override;
    procedure EmptyClipRect; override;
    function ExcludeClipRect(const ARect: TRect): Boolean; override;
    function IntersectClipRect(const ARect: TRect): Boolean; override;
    function UnionClipRect(const ARect: TRect): Boolean; override;
    function GetClipRect: TRect; override;
    function MapColor(const AColor: TGfxColor): TGfxPixel; override;
    procedure SetColor_(AColor: TGfxPixel); override;
    procedure SetFont(AFont: TGfxFont); override;
    procedure SetLineStyle(ALineStyle: TGfxLineStyle); override;

    procedure DrawArc(const Rect: TRect; StartAngle, EndAngle: Single); override;
    procedure DrawCircle(const Rect: TRect); override;
    procedure DrawLine(x1, y1, x2, y2: Integer); override;
    procedure DrawPolyLine(const Coords: array of Integer); override;
    procedure DrawRect(const Rect: TRect); override;
    procedure FillRect(const Rect: TRect); override;
    function FontCellHeight: Integer; override;
    function TextExtent(const AText: String): TSize; override;
    function TextWidth(const AText: String): Integer; override;
    procedure TextOut(x, y: Integer; const AText: String); override;

    procedure CopyRect(ASource: TGfxCanvas; const ASourceRect: TRect;
      ADestX, ADestY: Integer); override;
    procedure MaskedCopyRect(ASource, AMask: TGfxCanvas; const ASourceRect: TRect;
      AMaskX, AMaskY, ADestX, ADestY: Integer); override;
    procedure DrawImageRect(AImage: TGfxImage; ASourceRect: TRect;
      ADestX, ADestY: Integer); override;

    property DisplayHandle: PDisplay read FDisplayHandle;
    property Handle: X.TDrawable read FHandle;
    property GC: TGC read FGC;
    property Visual: PVisual read FVisual;
    property Colormap: TColormap read FColormap;
    property Region: TRegion read FRegion;
  end;

  TXWindowCanvas = class(TXCanvas)
  public
    constructor Create(AColormap: TColormap; ADisplay: TXDisplay;
      AXDrawable: X.TDrawable; ADefaultFont: PXFontStruct);
  end;

  TXPixmapCanvas = class(TXCanvas)
  public
    constructor Create(AColormap: TColormap; ADisplay: TXDisplay;
      AHandle: TPixmap; APixelFormat: TGfxPixelFormat);
    destructor Destroy; override;
  end;

  TXMonoPixmapCanvas = class(TXPixmapCanvas)
    constructor Create(AColormap: TColormap; ADisplay: TXDisplay;
      AHandle: TPixmap);
  end;


  TXImage = class(TGfxImage)
  private
    FDisplayHandle: PDisplay;
    {$IFDEF Debug}
    IsLocked: Boolean;
    {$ENDIF}
  protected
    FStride: LongWord;
    FData: Pointer;
  public
    constructor Create(ADisplayHandle: PDisplay; AWidth, AHeight: Integer;
      APixelFormat: TGfxPixelFormat);
    destructor Destroy; override;
    procedure Lock(var AData: Pointer; var AStride: LongWord); override;
    {$IFDEF Debug}
    procedure Unlock; override;
    {$ENDIF}
    property DisplayHandle: PDisplay read FDisplayHandle;
    property Stride: LongWord read FStride;
    property Data: Pointer read FData;
  end;


  TXScreen = class(TGfxScreen)
  private
    FDisplayHandle: PDisplay;
    FScreenIndex: Integer;
    FScreenInfo: PScreen;
  public
    constructor Create(ADisplay: TXDisplay; AScreenIndex: Integer);
    function CreateBitmap(AWidth, AHeight: Integer): TGfxCanvas; override;
    function CreateMonoBitmap(AWidth, AHeight: Integer): TGfxCanvas; override;
    function CreateWindow(AParent: TGfxWindow;
      AWindowType: TGfxWindowType): TGfxWindow; override;
    property DisplayHandle: PDisplay read FDisplayHandle;
    property ScreenIndex: Integer read FScreenIndex;
    property ScreenInfo: PScreen read FScreenInfo;
  end;


  TXWindow = class;

  PXWindowListEntry = ^TXWindowListEntry;
  TXWindowListEntry = record
    GfxWindow: TXWindow;
    XWindowID: X.TWindow;
  end;

  TXDisplay = class(TGfxDisplay)
  private
    DoBreakRun: Boolean;
    FDirtyList: TDirtyList;
    FDefaultFont: PXFontStruct;
    FDisplayName: String;
    FHandle: PDisplay;
    FWindows: TList;			// List of PXWindowListEntry records
    FWMProtocols: TAtom;		// Atom for "WM_PROTOCOLS"
    FWMDeleteWindow: TAtom;		// Atom for "WM_DELETE_WINDOW"
    FWMHints: TAtom;			// Atom for "_MOTIF_WM_HINTS"
    function GetHandle: PDisplay;
    property DirtyList: TDirtyList read FDirtyList;
  public
    constructor Create;
    destructor Destroy; override;
    function CreateFont(const Descriptor: String): TGfxFont; override;
    function CreateImage(AWidth, AHeight: Integer;
      APixelFormat: TGfxPixelFormat): TGfxImage; override;
    procedure Run; override;
    procedure BreakRun; override;

    function FindWindowByXID(XWindowID: X.TWindow): PXWindowListEntry;
    property Handle: PDisplay read GetHandle;
    property DisplayName: String read FDisplayName write FDisplayName;
  end;


  TXWindow = class(TGfxWindow)
  private
    FHandle: X.TWindow;
    FDisplay: TXDisplay;
    FParent: TGfxWindow;
    FComposeStatus: TXComposeStatus;
    FComposeBuffer: String[32];
    FCurCursorHandle: X.TCursor;
    function StartComposing(const Event: TXKeyEvent): TKeySym;
    procedure EndComposing;
    procedure KeyPressed(var Event: TXKeyPressedEvent); message X.KeyPress;
    procedure KeyReleased(var Event: TXKeyReleasedEvent); message X.KeyRelease;
    procedure ButtonPressed(var Event: TXButtonPressedEvent); message X.ButtonPress;
    procedure ButtonReleased(var Event: TXButtonReleasedEvent); message X.ButtonRelease;
    procedure EnterWindow(var Event :TXEnterWindowEvent); message X.EnterNotify;
    procedure LeaveWindow(var Event :TXLeaveWindowEvent); message X.LeaveNotify;
    procedure PointerMoved(var Event: TXPointerMovedEvent); message X.MotionNotify;
    procedure Expose(var Event: TXExposeEvent); message X.Expose;
    procedure FocusIn(var Event: TXFocusInEvent); message X.FocusIn;
    procedure FocusOut(var Event: TXFocusOutEvent); message X.FocusOut;
    procedure Map(var Event: TXMapEvent); message X.MapNotify;
    procedure Unmap(var Event: TXUnmapEvent); message X.UnmapNotify;
    procedure Reparent(var Event: TXReparentEvent); message X.ReparentNotify;
    procedure Configure(var Event: TXConfigureEvent); message X.ConfigureNotify;
    procedure ClientMessage(var Event: TXClientMessageEvent); message X.ClientMessage;
  protected
    IsExposing: Boolean;
    CanMaximize: Boolean;
    function GetTitle: String; override;
    procedure SetTitle(const ATitle: String); override;
    procedure DoSetCursor; override;
    function ConvertShiftState(AState: Cardinal): TShiftState;
    function KeySymToKeycode(KeySym: TKeySym): Word;
    procedure UpdateMotifWMHints;
  public
    constructor Create(AScreen: TXScreen; AParent: TGfxWindow;
      AWindowType: TGfxWindowType);
    destructor Destroy; override;
    procedure DefaultHandler(var Message); override;

    procedure SetPosition(ALeft, ATop: Integer); override;
    procedure SetSize(AWidth, AHeight: Integer); override;
    procedure SetMinMaxSize(AMinWidth, AMinHeight,
      AMaxWidth, AMaxHeight: Integer); override;
    procedure SetClientSize(AWidth, AHeight: Integer); override;
    procedure SetMinMaxClientSize(AMinWidth, AMinHeight,
      AMaxWidth, AMaxHeight: Integer); override;
    procedure Show; override;
    procedure Invalidate(const ARect: TRect); override;
    procedure PaintInvalidRegion; override;
    procedure CaptureMouse; override;
    procedure ReleaseMouse; override;

    function DisplayHandle: PDisplay; {!!!: inline;	will crash with current compiler}
    property Handle: X.TWindow read FHandle;
    property Display: TXDisplay read FDisplay;
  end;


var
  LeaderWindow: X.TWindow;
  ClientLeaderAtom: TAtom;


function GetXEventName(Event: LongInt): String;


// ===================================================================
// ===================================================================

implementation

uses GELImage;


// -------------------------------------------------------------------
//   TXFont
// -------------------------------------------------------------------

constructor TXFont.Create(ADisplayHandle: PDisplay; const Descriptor: String);
begin
  inherited Create;
  FDisplayHandle := ADisplayHandle;
  FFontStruct := XLoadQueryFont(DisplayHandle, PChar(Descriptor));
end;

destructor TXFont.Destroy;
begin
  if Assigned(FontStruct) then
  begin
    if FontStruct^.fid <> 0 then
      XUnloadFont(DisplayHandle, FontStruct^.fid);
    XFreeFontInfo(nil, FontStruct, 0);
  end;
  inherited Destroy;
end;


// -------------------------------------------------------------------
//   TXCanvas
// -------------------------------------------------------------------

constructor TXCanvas.Create(AColormap: TColormap; ADisplay: TXDisplay;
  AXDrawable: X.TDrawable; ADefaultFont: PXFontStruct);
var
  DummyWnd: PWindow;
  DummyInt: LongInt;
  GCValues: XLib.TXGCValues;
begin
  inherited Create;
  FColormap := AColormap;
  FDisplay := ADisplay;
  FDisplayHandle := ADisplay.Handle;
  FHandle := AXDrawable;
  FDefaultFont := ADefaultFont;
  XGetGeometry(DisplayHandle, Handle, @DummyWnd, @DummyInt, @DummyInt,
    @FWidth, @FHeight, @DummyInt, @DummyInt);

  GCValues.graphics_exposures := False;
  FGC := XCreateGC(DisplayHandle, Handle, GCGraphicsExposures, @GCValues);
  if not Assigned(GC) then
    raise EX11Error.Create(SGCCreationFailed);

  XSetLineAttributes(DisplayHandle, GC, 0,
    LineSolid, CapNotLast, JoinMiter);

  FFontStruct := FDefaultFont;
  if Assigned(FFontStruct) then
    XSetFont(DisplayHandle, GC, FFontStruct^.FID);

  FRegion := XCreateRegion;
  Resized(Width, Height);	// Set up a proper clipping region
end;

destructor TXCanvas.Destroy;
begin
  XDestroyRegion(Region);
  if Assigned(GC) then
    XFreeGC(DisplayHandle, GC);
  inherited Destroy;
end;

procedure TXCanvas.SaveState;
var
  SavedState: PXCanvasState;
  NewRegion: TRegion;
begin
  New(SavedState);
  SavedState^.Prev := FStateStackpointer;
  SavedState^.Matrix := Matrix;
  SavedState^.Region := Region;
  NewRegion := XCreateRegion;
  XUnionRegion(Region, NewRegion, NewRegion);
  FRegion := NewRegion;
  SavedState^.Color := FCurColor;
  SavedState^.Font := FFont;
  FStateStackpointer := SavedState;
end;

procedure TXCanvas.RestoreState;
var
  SavedState: PXCanvasState;
begin
  SavedState := FStateStackpointer;
  FStateStackpointer := SavedState^.Prev;
  Matrix := SavedState^.Matrix;

  XDestroyRegion(Region);
  FRegion := SavedState^.Region;
  XSetRegion(DisplayHandle, GC, Region);

  SetColor_(SavedState^.Color);
  SetFont(SavedState^.Font);

  Dispose(SavedState);
end;

procedure TXCanvas.EmptyClipRect;
begin
  XDestroyRegion(Region);
  FRegion := XCreateRegion;
  XSetRegion(DisplayHandle, GC, Region);
end;

function TXCanvas.ExcludeClipRect(const ARect: TRect): Boolean;
var
  x1, y1, x2, y2: Integer;
  RectRegion: TRegion;
  XRect: TXRectangle;
begin
  Transform(ARect.Left, ARect.Top, x1, y1);
  Transform(ARect.Right, ARect.Bottom, x2, y2);

  if (x2 > x1) and (y2 > y1) then
  begin
    XRect.x := x1;
    XRect.y := y1;
    XRect.Width := x2 - x1;
    XRect.Height := y2 - y1;
    RectRegion := XCreateRegion;
    XUnionRectWithRegion(@XRect, RectRegion, RectRegion);
    XSubtractRegion(Region, RectRegion, Region);
    XDestroyRegion(RectRegion);
    XSetRegion(DisplayHandle, GC, Region);
    Result := XEmptyRegion(Region) = 0;
  end else
    Result := False;
end;

function TXCanvas.IntersectClipRect(const ARect: TRect): Boolean;
var
  x1, y1, x2, y2: Integer;
  RectRegion: TRegion;
  XRect: TXRectangle;
begin
  Transform(ARect.Left, ARect.Top, x1, y1);
  Transform(ARect.Right, ARect.Bottom, x2, y2);

  if (x2 > x1) and (y2 > y1) then
  begin
    XRect.x := x1;
    XRect.y := y1;
    XRect.Width := x2 - x1;
    XRect.Height := y2 - y1;
    RectRegion := XCreateRegion;
    XUnionRectWithRegion(@XRect, RectRegion, RectRegion);
    XIntersectRegion(Region, RectRegion, Region);
    XDestroyRegion(RectRegion);
    XSetRegion(DisplayHandle, GC, Region);
    Result := XEmptyRegion(Region) = 0;
  end else
    Result := False;
end;

function TXCanvas.UnionClipRect(const ARect: TRect): Boolean;
var
  x1, y1, x2, y2: Integer;
  XRect: TXRectangle;
begin
  Transform(ARect.Left, ARect.Top, x1, y1);
  Transform(ARect.Right, ARect.Bottom, x2, y2);

  if (x2 > x1) and (y2 > y1) then
  begin
    XRect.x := x1;
    XRect.y := y1;
    XRect.Width := x2 - x1;
    XRect.Height := y2 - y1;
    XUnionRectWithRegion(@XRect, Region, Region);
    XSetRegion(DisplayHandle, GC, Region);
  end;
  Result := XEmptyRegion(Region) = 0;
end;

function TXCanvas.GetClipRect: TRect;
var
  XRect: TXRectangle;
begin
  XClipBox(Region, @XRect);
  ReverseTransform(XRect.x, XRect.y, Result.Left, Result.Top);
  ReverseTransform(XRect.x + XRect.Width, XRect.y + XRect.Height,
    Result.Right, Result.Bottom);
end;

function TXCanvas.MapColor(const AColor: TGfxColor): TGfxPixel;
var
  Color: TXColor;
begin
  Color.Pixel := 0;
  Color.Red := AColor.Red;
  Color.Green := AColor.Green;
  Color.Blue := AColor.Blue;
  XAllocColor(DisplayHandle, Colormap, @Color);
  Result := Color.Pixel;
end;

procedure TXCanvas.SetColor_(AColor: TGfxPixel);
begin
  if AColor <> FCurColor then
  begin
    XSetForeground(DisplayHandle, GC, AColor);
    FCurColor := AColor;
  end;
end;

procedure TXCanvas.SetFont(AFont: TGfxFont);
begin
  if AFont = FFont then
    exit;

  FFont := AFont;

  if not Assigned(AFont) then
  begin
    if FFontStruct = FDefaultFont then
      exit;
    FFontStruct := FDefaultFont;
  end else
  begin
    if not AFont.InheritsFrom(TXFont) then
      raise EGfxError.CreateFmt(SXCanvasInvalidFontClass, [AFont.ClassName]);
    if TXFont(AFont).FontStruct = FFontStruct then
      exit;
    FFontStruct := TXFont(AFont).FontStruct;
  end;
  XSetFont(DisplayHandle, GC, FFontStruct^.FID);
end;

procedure TXCanvas.SetLineStyle(ALineStyle: TGfxLineStyle);
const
  DotDashes: array[0..1] of Char = #1#1;
begin
  case ALineStyle of
    lsSolid:
      XSetLineAttributes(DisplayHandle, GC, 0,
        LineSolid, CapNotLast, JoinMiter);
    lsDot:
      begin
        XSetLineAttributes(DisplayHandle, GC, 0,
          LineOnOffDash, CapNotLast, JoinMiter);
        XSetDashes(DisplayHandle, GC, 0, DotDashes, 2);
      end;
  end;
end;

procedure TXCanvas.DrawArc(const Rect: TRect; StartAngle, EndAngle: Single);
var
  x1, y1, x2, y2: Integer;
begin
  Transform(Rect.Left, Rect.Top, x1, y1);
  Transform(Rect.Right, Rect.Bottom, x2, y2);
  XDrawArc(DisplayHandle, Handle, GC,
    x1, y1, x2 - x1 - 1, y2 - y1 - 1,
    Round(StartAngle * 64), Round((EndAngle - StartAngle) * 64));
end;

procedure TXCanvas.DrawCircle(const Rect: TRect);
var
  x1, y1, x2, y2: Integer;
begin
  Transform(Rect.Left, Rect.Top, x1, y1);
  Transform(Rect.Right, Rect.Bottom, x2, y2);
  XDrawArc(DisplayHandle, Handle, GC,
    x1, y1, x2 - x1 - 1, y2 - y1 - 1, 0, 23040);
end;

procedure TXCanvas.DrawLine(x1, y1, x2, y2: Integer);
begin
  Transform(x1, y1, x1, y1);
  Transform(x2, y2, x2, y2);
  XDrawLine(DisplayHandle, Handle, GC, x1, y1, x2, y2);
end;

procedure TXCanvas.DrawPolyLine(const Coords: array of Integer);
var
  Points: PXPoint;
  CoordsIndex, PointsIndex, x, y: Integer;
begin
  GetMem(Points, (High(Coords) - Low(Coords) + 1) * SizeOf(TXPoint));
  CoordsIndex := Low(Coords);
  PointsIndex := 0;
  while CoordsIndex < High(Coords) do
  begin
    Transform(Coords[CoordsIndex], Coords[CoordsIndex + 1], x, y);
    Points[PointsIndex].x := x;
    Points[PointsIndex].y := y;
    Inc(CoordsIndex, 2);
    Inc(PointsIndex);
  end;

  XDrawLines(DisplayHandle, Handle, GC, Points, PointsIndex, CoordModeOrigin);

  FreeMem(Points);
end;

procedure TXCanvas.DrawRect(const Rect: TRect);
var
  x1, y1, x2, y2: Integer;
begin
  Transform(Rect.Left, Rect.Top, x1, y1);
  Transform(Rect.Right, Rect.Bottom, x2, y2);
  if (Rect.Right > Rect.Left) and (Rect.Bottom > Rect.Top) then
    XDrawRectangle(DisplayHandle, Handle, GC, x1, y1, x2 - x1 - 1, y2 - y1 - 1);
end;

procedure TXCanvas.FillRect(const Rect: TRect);
var
  r: TRect;
begin
  Transform(Rect.Left, Rect.Top, r.Left, r.Top);
  Transform(Rect.Right, Rect.Bottom, r.Right, r.Bottom);
  if (r.Right > r.Left) and (r.Bottom > r.Top) then
    XFillRectangle(DisplayHandle, Handle, GC, r.Left, r.Top,
      r.Right - r.Left, r.Bottom - r.Top);
end;

function TXCanvas.FontCellHeight: Integer;
begin
  Result := FFontStruct^.Ascent + FFontStruct^.Descent;
end;

function TXCanvas.TextExtent(const AText: String): TSize;
var
  Direction, FontAscent, FontDescent: LongInt;
  CharStruct: TXCharStruct;
begin
  if Length(AText) = 0 then
  begin
    Result.cx := 0;
    Result.cy := 0;
  end else
  begin
    XQueryTextExtents(DisplayHandle, XGContextFromGC(GC),
      PChar(AText), Length(AText),
      @Direction, @FontAscent, @FontDescent, @CharStruct);
    Result.cx := CharStruct.Width;
    Result.cy := CharStruct.Ascent + CharStruct.Descent;
  end;
end;

function TXCanvas.TextWidth(const AText: String): Integer;
var
  Direction, FontAscent, FontDescent: LongInt;
  CharStruct: TXCharStruct;
begin
  if Length(AText) = 0 then
    Result := 0
  else
  begin
    XQueryTextExtents(DisplayHandle, XGContextFromGC(GC),
      PChar(AText), Length(AText),
      @Direction, @FontAscent, @FontDescent, @CharStruct);
    Result := CharStruct.Width;
  end;
end;

procedure TXCanvas.TextOut(x, y: Integer; const AText: String);
begin
  Transform(x, y, x, y);
  XDrawString(DisplayHandle, Handle, GC, x, y + FFontStruct^.ascent,
    PChar(AText), Length(AText));
end;

procedure TXCanvas.CopyRect(ASource: TGfxCanvas; const ASourceRect: TRect;
  ADestX, ADestY: Integer);
var
  SourceLeft, SourceTop, SourceRight, SourceBottom, RealHeight: Integer;
begin
  if not ASource.InheritsFrom(TXCanvas) then
    raise EX11Error.CreateFmt(SIncompatibleCanvasForBlitting,
      [ASource.ClassName, Self.ClassName]);

  ASource.Transform(ASourceRect.Left, ASourceRect.Top, SourceLeft, SourceTop);
  ASource.Transform(ASourceRect.Right, ASourceRect.Bottom,
    SourceRight, SourceBottom);

  if (ASourceRect.Left >= ASourceRect.Right) or
    (ASourceRect.Top >= ASourceRect.Bottom) then
    exit;

  Transform(ADestX, ADestY, ADestX, ADestY);

  if (ASource <> Self) and (ASource.PixelFormat.FormatType = ftMono) then
  begin
    // !!!: This case will probably be removed completely very soon
    RealHeight := ASourceRect.Bottom - ASourceRect.Top;
    if ADestY + RealHeight > Height then
      RealHeight := Height - ADestY;
    XSetClipMask(DisplayHandle, GC, TXCanvas(ASource).Handle);
    XSetClipOrigin(DisplayHandle, GC, ADestX, ADestY);
    XFillRectangle(DisplayHandle, Handle, GC, ADestX, ADestY,
      ASource.Width, RealHeight);
    // Restore old clipping settings
    XSetClipOrigin(DisplayHandle, GC, 0, 0);
    XSetRegion(DisplayHandle, GC, Region);
  end else
    XCopyArea(DisplayHandle, TXCanvas(ASource).Handle, Handle, GC,
      ASourceRect.Left, ASourceRect.Top, ASourceRect.Right - ASourceRect.Left,
      ASourceRect.Bottom - ASourceRect.Top, ADestX, ADestY);
end;

procedure TXCanvas.MaskedCopyRect(ASource, AMask: TGfxCanvas;
  const ASourceRect: TRect; AMaskX, AMaskY, ADestX, ADestY: Integer);
var
  SourceLeft, SourceTop, SourceRight, SourceBottom,
    RectWidth, RectHeight: Integer;
begin
  if not ASource.InheritsFrom(TXCanvas) then
    raise EX11Error.CreateFmt(SIncompatibleCanvasForBlitting,
      [ASource.ClassName, Self.ClassName]);
  if not AMask.InheritsFrom(TXMonoPixmapCanvas) then
    raise EX11Error.CreateFmt(SIncompatibleCanvasForBlitting,
      [AMask.ClassName, Self.ClassName]);

  SourceLeft := ASourceRect.Left;
  SourceTop := ASourceRect.Top;
  SourceRight := ASourceRect.Right;
  SourceBottom := ASourceRect.Bottom;
  RectWidth := SourceRight - SourceLeft;
  RectHeight := SourceBottom - SourceTop;

  { !!!: Attention! The current implementation only clips to the ClipRect,
    i.e. the outer bounds of the current clipping region. In other words, the
    result is only correct for a simple rectangle clipping region. }
  with GetClipRect do
  begin
    if (ADestX + RectWidth <= Left) or (ADestY + RectHeight <= Top) then
      exit;

    if ADestX < Left then
    begin
      Inc(AMaskX, Left - ADestX);
      Inc(SourceLeft, Left - ADestX);
      ADestX := Left;
    end;
    if ADestY < Top then
    begin
      Inc(AMaskY, Top - ADestY);
      Inc(SourceTop, Top - ADestY);
      ADestY := Top;
    end;

    if (ADestX >= Right) or (ADestY >= Bottom) then
      exit;

    if ADestX + RectWidth > Right then
      RectWidth := Right - ADestX;
    if ADestY + RectHeight > Bottom then
      RectHeight := Bottom - ADestY;
  end;

  if (RectWidth <= 0) or (RectHeight <= 0) then
    exit;

  ASource.Transform(SourceLeft, SourceTop, SourceLeft, SourceTop);
  ASource.Transform(SourceRight, SourceBottom, SourceRight, SourceBottom);
  AMask.Transform(AMaskX, AMaskY, AMaskX, AMaskY);
  Transform(ADestX, ADestY, ADestX, ADestY);

  XSetClipMask(DisplayHandle, GC, TXCanvas(AMask).Handle);
  XSetClipOrigin(DisplayHandle, GC, ADestX - AMaskX, ADestY - AMaskY);

  XCopyArea(DisplayHandle, TXCanvas(ASource).Handle, Handle, GC,
    SourceLeft, SourceTop, RectWidth, RectHeight, ADestX, ADestY);

  // Restore old clipping settings
  XSetClipOrigin(DisplayHandle, GC, 0, 0);
  XSetRegion(DisplayHandle, GC, Region);
end;


function malloc(size: LongWord): Pointer; cdecl; external;

procedure TXCanvas.DrawImageRect(AImage: TGfxImage; ASourceRect: TRect;
  ADestX, ADestY: Integer);
var
  SourceRect: TRect;
  RealWidth, RealHeight: Integer;
  Image: XLib.PXImage;
begin
  ASSERT(AImage.InheritsFrom(TXImage));
  {$IFDEF Debug}
  ASSERT(not TXImage(AImage).IsLocked);
  {$ENDIF}

  Transform(ADestX, ADestY, ADestX, ADestY);

  SourceRect := ASourceRect;
  if SourceRect.Right > Width then
    SourceRect.Right := Width;
  if SourceRect.Bottom > Height then
    SourceRect.Bottom := Height;

  RealWidth := ASourceRect.Right - ASourceRect.Left;
  RealHeight := ASourceRect.Bottom - ASourceRect.Top;
  if (RealWidth <= 0) or (RealHeight <= 0) then
    exit;

  // !!!: Add support for XF86 4 and XShm etc. to speed this up!
  Image := XCreateImage(DisplayHandle, Visual,
    FormatTypeBPPTable[PixelFormat.FormatType], ZPixmap,
    0, nil, RealWidth, RealHeight, 8, 0);
  Image^.data := malloc(Image^.bytes_per_line * RealHeight);

  if (AImage.PixelFormat.FormatType = ftMono) and
    Self.InheritsFrom(TXMonoPixmapCanvas) then
    // mirror the bits within all image data bytes...:
    FlipMonoImageBits(ASourceRect, TXImage(AImage).Data,
      TXImage(AImage).Stride, 0, 0, Image^.data, Image^.bytes_per_line)
  else
    ConvertImage(ASourceRect, AImage.PixelFormat, AImage.Palette,
      TXImage(AImage).Data, TXImage(AImage).Stride,
      0, 0, PixelFormat, Image^.data, Image^.bytes_per_line);

  XPutImage(DisplayHandle, Handle, GC,
    Image, 0, 0, ADestX, ADestY, AImage.Width, AImage.Height);

  // !!!: Change to XDestroyImage when this macro gets supported by xutil.pp
  Image^.f.destroy_image(Image);
end;

procedure TXCanvas.Resized(NewWidth, NewHeight: Integer);
var
  XRect: TXRectangle;
begin
  FWidth := NewWidth;
  FHeight := NewHeight;

  XDestroyRegion(Region);
  XRect.x := 0;
  XRect.y := 0;
  XRect.Width := Width;
  XRect.Height := Height;
  FRegion := XCreateRegion;
  XUnionRectWithRegion(@XRect, Region, Region);
end;


// -------------------------------------------------------------------
//   TXWindowCanvas
// -------------------------------------------------------------------

constructor TXWindowCanvas.Create(AColormap: TColormap; ADisplay: TXDisplay;
  AXDrawable: X.TDrawable; ADefaultFont: PXFontStruct);
var
  Attr: XLib.TXWindowAttributes;
begin
  inherited Create(AColormap, ADisplay, AXDrawable, ADefaultFont);
  XGetWindowAttributes(DisplayHandle, Handle, @Attr);
  case Attr.Depth of
    1: PixelFormat.FormatType := ftMono;
    4: PixelFormat.FormatType := ftPal4;
    8: PixelFormat.FormatType := ftPal8;
    16: PixelFormat.FormatType := ftRGB16;
    24: PixelFormat.FormatType := ftRGB24;
    32: PixelFormat.FormatType := ftRGB32;
    else
      raise EX11Error.CreateFmt(SWindowUnsupportedPixelFormat, [Attr.Depth]);
  end;

  FVisual := Attr.Visual;

  if Attr.Depth >= 16 then
  begin
    PixelFormat.RedMask := Visual^.red_mask;
    PixelFormat.GreenMask := Visual^.green_mask;
    PixelFormat.BlueMask := Visual^.blue_mask;
  end;
end;


// -------------------------------------------------------------------
//   TXPixmapCanvas
// -------------------------------------------------------------------

constructor TXPixmapCanvas.Create(AColormap: TColormap; ADisplay: TXDisplay;
  AHandle: TPixmap; APixelFormat: TGfxPixelFormat);
begin
  inherited Create(AColormap, ADisplay, AHandle, nil);
  FPixelFormat := APixelFormat;
end;

destructor TXPixmapCanvas.Destroy;
begin
  XFreePixmap(DisplayHandle, Handle);
  inherited Destroy;
end;


// -------------------------------------------------------------------
//   TXMonoPixmapCanvas
// -------------------------------------------------------------------

constructor TXMonoPixmapCanvas.Create(AColormap: TColormap; ADisplay: TXDisplay;
  AHandle: TPixmap);
begin
  inherited Create(AColormap, ADisplay, AHandle, PixelFormatMono);
end;


// -------------------------------------------------------------------
//   TXImage
// -------------------------------------------------------------------

constructor TXImage.Create(ADisplayHandle: PDisplay; AWidth, AHeight: Integer;
  APixelFormat: TGfxPixelFormat);
begin
  inherited Create(AWidth, AHeight, APixelFormat);
  FDisplayHandle := ADisplayHandle;
  case APixelFormat.FormatType of
    ftMono:
      FStride := (AWidth + 7) shr 3;
    ftPal4, ftPal4A:
      FStride := (AWidth + 1) and not 1;
    else
      FStride := AWidth * (FormatTypeBPPTable[APixelFormat.FormatType] shr 3);
  end;
  GetMem(FData, FStride * Height);
end;

destructor TXImage.Destroy;
begin
  FreeMem(FData);
  inherited Destroy;
end;

procedure TXImage.Lock(var AData: Pointer; var AStride: LongWord);
begin
  {$IFDEF Debug}
  ASSERT(not IsLocked);
  IsLocked := True;
  {$ENDIF}
  AData := Data;
  AStride := Stride;
end;

{$IFDEF Debug}
procedure TXImage.Unlock;
begin
  ASSERT(IsLocked);
  IsLocked := False;
end;
{$ENDIF}


// -------------------------------------------------------------------
//   TXScreen
// -------------------------------------------------------------------

constructor TXScreen.Create(ADisplay: TXDisplay; AScreenIndex: Integer);
begin
  inherited Create(ADisplay);
  FDisplayHandle := ADisplay.Handle;
  FScreenIndex := AScreenIndex;

  FScreenInfo := XScreenOfDisplay(DisplayHandle, ScreenIndex);
end;

function TXScreen.CreateBitmap(AWidth, AHeight: Integer): TGfxCanvas;
var
  Depth: Integer;
  PixelFormat: TGfxPixelFormat;
begin
  Depth := XDefaultDepthOfScreen(ScreenInfo);
  case Depth of
    1: PixelFormat.FormatType := ftMono;
    4: PixelFormat.FormatType := ftPal4;
    8: PixelFormat.FormatType := ftPal8;
    16: PixelFormat.FormatType := ftRGB16;
    24: PixelFormat.FormatType := ftRGB24;
    32: PixelFormat.FormatType := ftRGB32;
    else
      raise EX11Error.CreateFmt(SWindowUnsupportedPixelFormat, [Depth]);
  end;

  if Depth >= 16 then
    with XDefaultVisualOfScreen(ScreenInfo)^ do
    begin
      PixelFormat.RedMask := red_mask;
      PixelFormat.GreenMask := green_mask;
      PixelFormat.BlueMask := blue_mask;
    end;

  Result := TXPixmapCanvas.Create(
    XDefaultColormapOfScreen(ScreenInfo), TXDisplay(Display),
    XCreatePixmap(DisplayHandle, XRootWindowOfScreen(ScreenInfo),
      AWidth, AHeight,
    Depth), PixelFormat);
end;

function TXScreen.CreateMonoBitmap(AWidth, AHeight: Integer): TGfxCanvas;
begin
  Result := TXMonoPixmapCanvas.Create(
    XDefaultColormap(DisplayHandle, ScreenIndex), TXDisplay(Display),
    XCreatePixmap(DisplayHandle, XRootWindowOfScreen(ScreenInfo),
      AWidth, AHeight, 1));
end;

function TXScreen.CreateWindow(AParent: TGfxWindow;
  AWindowType: TGfxWindowType): TGfxWindow;
var
  WindowListEntry: PXWindowListEntry;
begin
  Result := TXWindow.Create(Self, AParent, AWindowType);
  if not Assigned(TXDisplay(Display).FWindows) then
    TXDisplay(Display).FWindows := TList.Create;
  New(WindowListEntry);
  WindowListEntry^.GfxWindow := TXWindow(Result);
  WindowListEntry^.XWindowID := TXWindow(Result).Handle;
  TXDisplay(Display).FWindows.Add(WindowListEntry);
end;


// -------------------------------------------------------------------
//   TXDisplay
// -------------------------------------------------------------------

// public methods

constructor TXDisplay.Create;
begin
  inherited Create;
  FDirtyList := TDirtyList.Create;
  FDefaultScreen := TXScreen.Create(Self, XDefaultScreen(Handle));
end;

destructor TXDisplay.Destroy;
var
  i: Integer;
  WindowListEntry: PXWindowListEntry;
begin
  if Assigned(FWindows) then
  begin
    for i := 0 to FWindows.Count - 1 do
    begin
      WindowListEntry := PXWindowListEntry(FWindows[i]);
      WindowListEntry^.GfxWindow.Free;
      Dispose(WindowListEntry);
    end;
    FWindows.Free;
  end;

  DefaultScreen.Free;
  DirtyList.Free;

  if Assigned(FDefaultFont) then
  begin
    if FDefaultFont^.fid <> 0 then
      XUnloadFont(FHandle, FDefaultFont^.fid);
    XFreeFontInfo(nil, FDefaultFont, 0);
  end;

  if Assigned(FHandle) then
    XCloseDisplay(FHandle);

  inherited Destroy;
end;

function TXDisplay.CreateFont(const Descriptor: String): TGfxFont;
begin
  Result := TXFont.Create(Handle, Descriptor);
end;

function TXDisplay.CreateImage(AWidth, AHeight: Integer;
  APixelFormat: TGfxPixelFormat): TGfxImage;
begin
  Result := TXImage.Create(Handle, AWidth, AHeight, APixelFormat);
end;

procedure TXDisplay.Run;
var
  Event: TXEvent;
  WindowListEntry: PXWindowListEntry;
begin
  if not Assigned(FWindows) then
    exit;

  GetHandle;
  DoBreakRun := False;
  while (FWindows.Count > 0) and not DoBreakRun do
  begin
    if Assigned(OnIdle) or Assigned(DirtyList.First) then
    begin
      if not XCheckMaskEvent(FHandle, MaxInt, @Event) then
      begin
        if Assigned(DirtyList.First) then
	  DirtyList.PaintAll
	else
          if Assigned(OnIdle) then
	    OnIdle(Self);
	continue;
      end;
    end else
      XNextEvent(FHandle, @Event);

    // According to a comment in X.h, the valid event types start with 2!
    if Event._type >= 2 then
    begin
      WindowListEntry := FindWindowByXID(Event.XAny.Window);

      if Event._type = X.DestroyNotify then
      begin
	FWindows.Remove(WindowListEntry);
        Dispose(WindowListEntry);
      end else if Assigned(WindowListEntry) then
      begin
        if Assigned(WindowListEntry^.GfxWindow) then
          WindowListEntry^.GfxWindow.Dispatch(Event);
      end else
        WriteLn('fpGFX/X11: Received X event ''', GetXEventName(Event._type),
	  ''' for unknown window');
    end;
  end;
end;

procedure TXDisplay.BreakRun;
begin
  DoBreakRun := True;
end;

function TXDisplay.FindWindowByXID(XWindowID: X.TWindow): PXWindowListEntry;
var
  i: Integer;
begin
  for i := 0 to FWindows.Count - 1 do
  begin
    Result := PXWindowListEntry(FWindows[i]);
    if Result^.XWindowID = XWindowID then
      exit;
  end;
  Result := nil;
end;

function TXDisplay.GetHandle: PDisplay;
begin
  if not Assigned(FHandle) then
  begin
    if Length(DisplayName) = 0 then
      FDisplayName := XDisplayName(nil);
    FHandle := XOpenDisplay(PChar(DisplayName));
    if not Assigned(FHandle) then
      raise EX11Error.CreateFmt(SOpenDisplayFailed, [DisplayName]);

    FDefaultFont := XLoadQueryFont(FHandle,
      '-adobe-helvetica-medium-r-normal--*-120-*-*-*-*-iso8859-1');
    if not Assigned(FDefaultFont) then
    begin
      FDefaultFont := XLoadQueryFont(FHandle, 'fixed');
      if not Assigned(FDefaultFont) then
        raise EX11Error.Create(SNoDefaultFont);
    end;
  end;
  Result := FHandle;
end;


// -------------------------------------------------------------------
//   TXWindow
// -------------------------------------------------------------------

// public methods

constructor TXWindow.Create(AScreen: TXScreen; AParent: TGfxWindow;
  AWindowType: TGfxWindowType);
const
  WindowHints: TXWMHints = (
    flags: InputHint or StateHint or WindowGroupHint;
    input: True;
    initial_state: NormalState;
    icon_pixmap: 0;
    icon_window: 0;
    icon_x: 0;
    icon_y: 0;
    icon_mask: 0;
    window_group: 0;
  );
var
  Colormap: TColormap;
  Attr: TXSetWindowAttributes;
  SizeHints: TXSizeHints;
  ClassHint: PXClassHint;
begin
  inherited Create;

  FWindowType := AWindowType;
  FScreen := AScreen;
  FDisplay := TXDisplay(Screen.Display);
  FParent := AParent;
  FWindowType := AWindowType;

  if LeaderWindow = 0 then
  begin
    LeaderWindow := XCreateSimpleWindow(DisplayHandle,
      XDefaultRootWindow(DisplayHandle), 10, 10, 10, 10, 0, 0, 0);

    ClassHint := XAllocClassHint;
    ClassHint^.res_name := 'fpGFX'; // !!! use app name
    ClassHint^.res_class := 'FpGFX';
    XSetWMProperties(DisplayHandle, LeaderWindow, nil, nil, nil, 0, nil, nil,
      ClassHint);
    XFree(ClassHint);
    ClientLeaderAtom := XInternAtom(DisplayHandle, 'WM_CLIENT_LEADER', False);
  end;

  Colormap := XDefaultColormap(DisplayHandle, XDefaultScreen(DisplayHandle));

  Attr.event_mask := KeyPressMask or KeyReleaseMask or ButtonPressMask or
    ButtonReleaseMask or EnterWindowMask or LeaveWindowMask or
    PointerMotionMask or ExposureMask or FocusChangeMask or StructureNotifyMask;
  Attr.Colormap := Colormap;

  SizeHints.flags := PSize;
  SizeHints.x := 0;
  SizeHints.y := 0;
  SizeHints.width := 200;
  SizeHints.height := 200;

  FHandle := XCreateWindow(
    DisplayHandle,
    XDefaultRootWindow(DisplayHandle),	// create top-level window
    SizeHints.x, SizeHints.y,		// position
    SizeHints.width, SizeHints.height,	// size
    0,					// border size
    CopyFromParent,			// depth
    InputOutput,			// class
    CopyFromParent,			// visual
    CWEventMask or CWColormap,		// valuemask: What in "Attr" is valid?
    @Attr);

  if Handle = 0 then
    raise EX11Error.Create(SWindowCreationFailed);

  XSetStandardProperties(DisplayHandle, Handle, nil, nil, 0,
    argv, argc, @SizeHints);

  XSetWMNormalHints(DisplayHandle, Handle, @SizeHints);

  WindowHints.flags := WindowGroupHint;
  WindowHints.window_group := LeaderWindow;
  XSetWMHints(DisplayHandle, Handle, @WindowHints);

  XChangeProperty(DisplayHandle, Handle, ClientLeaderAtom, 33, 32,
    PropModeReplace, @LeaderWindow, 1);

  // We want to get a Client Message when the user tries to close this window
  if Display.FWMProtocols = 0 then
    Display.FWMProtocols := XInternAtom(DisplayHandle,
      'WM_PROTOCOLS', False);
  if Display.FWMDeleteWindow = 0 then
    Display.FWMDeleteWindow := XInternAtom(DisplayHandle,
      'WM_DELETE_WINDOW', False);

  XSetWMProtocols(DisplayHandle, FHandle, @Display.FWMDeleteWindow, 1);


  FCanvas := TXWindowCanvas.Create(Colormap, Display, Handle,
    Display.FDefaultFont);
end;

destructor TXWindow.Destroy;
begin
  if Assigned(OnClose) then
    OnClose(Self);

  Display.DirtyList.ClearQueueForWindow(Self);

  XDestroyWindow(DisplayHandle, Handle);
  Canvas.Free;

  Display.FindWindowByXID(Handle)^.GfxWindow := nil;

  if FCurCursorHandle <> 0 then
    XFreeCursor(DisplayHandle, FCurCursorHandle);

  inherited Destroy;
end;

procedure TXWindow.DefaultHandler(var Message);
begin
  WriteLn('fpGFX/X11: Unhandled X11 event received: ',
    GetXEventName(TXEvent(Message)._type));
end;

procedure TXWindow.SetPosition(ALeft, ATop: Integer);
var
  Supplied: LongInt;
  SizeHints: PXSizeHints;
begin
  SizeHints := XAllocSizeHints;
  XGetWMNormalHints(DisplayHandle, Handle, SizeHints, @Supplied);
  SizeHints^.flags := SizeHints^.flags or PPosition;
  SizeHints^.x := ALeft;
  SizeHints^.y := ATop;
  XSetWMNormalHints(DisplayHandle, Handle, SizeHints);
  XFree(SizeHints);
  XMoveWindow(DisplayHandle, Handle, ALeft, ATop);
end;

procedure TXWindow.SetSize(AWidth, AHeight: Integer);
begin
  // !!!: Implement this properly
  WriteLn('fpGFX/X11: TXWindow.SetSize is not properly implemented yet');
  SetClientSize(AWidth, AHeight);
end;

procedure TXWindow.SetMinMaxSize(AMinWidth, AMinHeight,
  AMaxWidth, AMaxHeight: Integer);
begin
  // !!!: Implement this properly
  WriteLn('fpGFX/X11: TXWindow.SetMinMaxSize is not properly implemented yet');
  SetMinMaxClientSize(AMinWidth, AMinHeight, AMaxWidth, AMaxHeight);
end;

procedure TXWindow.SetClientSize(AWidth, AHeight: Integer);
var
  ChangeMask: Cardinal;
  Changes: TXWindowChanges;
begin
  ChangeMask := 0;

  if AWidth <> ClientWidth then
  begin
    ChangeMask := CWWidth;
    Changes.Width := AWidth;
  end;

  if AHeight <> ClientHeight then
  begin
    ChangeMask := ChangeMask or CWHeight;
    Changes.Height := AHeight;
  end;

  if ChangeMask <> 0 then
    XConfigureWindow(DisplayHandle, Handle, ChangeMask, @Changes);
end;

procedure TXWindow.SetMinMaxClientSize(AMinWidth, AMinHeight,
  AMaxWidth, AMaxHeight: Integer);
var
  Supplied: LongInt;
  SizeHints: PXSizeHints;
begin
  CanMaximize := (AMaxWidth = 0) or (AMaxHeight = 0) or
    (AMaxWidth > AMinWidth) or (AMaxHeight > AMinHeight);
  UpdateMotifWMHints;

  SizeHints := XAllocSizeHints;
  XGetWMNormalHints(DisplayHandle, Handle, SizeHints, @Supplied);
  with SizeHints^ do
  begin
    if (AMinWidth > 0) or (AMinHeight > 0) then
    begin
      flags := flags or PMinSize;
      min_width := AMinWidth;
      min_height := AMinHeight;
    end else
      flags := flags and not PMinSize;

    if (AMaxWidth > 0) or (AMaxHeight > 0) then
    begin
      flags := flags or PMaxSize;
      if AMaxWidth > 0 then
        max_width := AMaxWidth
      else
        max_width := 32767;
      if AMaxHeight > 0 then
        max_height := AMaxHeight
      else
        max_height := 32767;
    end else
      flags := flags and not PMaxSize;
  end;

  XSetWMNormalHints(DisplayHandle, Handle, SizeHints);
  XFree(SizeHints);
end;

procedure TXWindow.Show;
begin
  XMapRaised(DisplayHandle, Handle);
end;

procedure TXWindow.Invalidate(const ARect: TRect);
begin
  Display.DirtyList.AddRect(Self, ARect);
end;

procedure TXWindow.PaintInvalidRegion;
begin
  Display.DirtyList.PaintQueueForWindow(Self);
end;

procedure TXWindow.CaptureMouse;
begin
  XGrabPointer(DisplayHandle, Handle, False, ButtonPressMask or
    ButtonReleaseMask or EnterWindowMask or LeaveWindowMask or
    PointerMotionMask, GrabModeAsync, GrabModeAsync, 0, 0, CurrentTime);
end;

procedure TXWindow.ReleaseMouse;
begin
  XUngrabPointer(DisplayHandle, CurrentTime);
end;

function TXWindow.DisplayHandle: PDisplay; {!!!: inline;}
begin
  Result := TXDisplay(Screen.Display).Handle;
end;


// protected methods

function TXWindow.GetTitle: String;
var
  s: PChar;
begin
  XFetchName(DisplayHandle, Handle, @s);
  Result := s;
  XFree(s);
end;

procedure TXWindow.SetTitle(const ATitle: String);
begin
  XStoreName(DisplayHandle, Handle, PChar(ATitle));
end;

procedure TXWindow.DoSetCursor;
const
  CursorTable: array[TGfxCursor] of Integer = (
    -1,			// crDefault
    -2,			// crNone	!!!: not implemented
    -1,			// crArrow
    34,			// crCross
    152,		// crIBeam
    52,			// crSize
    116,		// crSizeNS
    108,		// crSizeWE
    114,		// crUpArrow
    150,		// crHourGlass
    0,			// crNoDrop
    92);		// crHelp
var
  ID: Integer;
begin
  if FCurCursorHandle <> 0 then
    XFreeCursor(DisplayHandle, FCurCursorHandle);
  ID := CursorTable[Cursor];
  if ID = -1 then
    FCurCursorHandle := 0
  else
    FCurCursorHandle := XCreateFontCursor(DisplayHandle, ID);
  XDefineCursor(DisplayHandle, Handle, FCurCursorHandle);
end;

function TXWindow.ConvertShiftState(AState: Cardinal): TShiftState;
begin
  Result := [];
  if (AState and Button1Mask) <> 0 then
    Include(Result, ssLeft);
  if (AState and Button2Mask) <> 0 then
    Include(Result, ssMiddle);
  if (AState and Button3Mask) <> 0 then
    Include(Result, ssRight);
  if (AState and ShiftMask) <> 0 then
    Include(Result, ssShift);
  if (AState and LockMask) <> 0 then
    Include(Result, ssCaps);
  if (AState and ControlMask) <> 0 then
    Include(Result, ssCtrl);
  if (AState and Mod1Mask) <> 0 then
    Include(Result, ssAlt);
  if (AState and Mod2Mask) <> 0 then
    Include(Result, ssNum);
  if (AState and Mod4Mask) <> 0 then
    Include(Result, ssSuper);
  if (AState and Mod5Mask) <> 0 then
    Include(Result, ssScroll);
  if (AState and (1 shl 13)) <> 0 then
    Include(Result, ssAltGr);
end;

function TXWindow.KeySymToKeycode(KeySym: TKeySym): Word;
const
  Table_20aX: array[$20a0..$20ac] of Word = (keyEcuSign, keyColonSign,
    keyCruzeiroSign, keyFFrancSign, keyLiraSign, keyMillSign, keyNairaSign,
    keyPesetaSign, keyRupeeSign, keyWonSign, keyNewSheqelSign, keyDongSign,
    keyEuroSign);
  Table_feXX: array[$fe50..$fe60] of Word = (keyDeadGrave, keyDeadAcute,
    keyDeadCircumflex, keyDeadTilde, keyDeadMacron,keyDeadBreve,
    keyDeadAbovedot, keyDeadDiaeresis, keyDeadRing, keyDeadDoubleacute,
    keyDeadCaron, keyDeadCedilla, keyDeadOgonek, keyDeadIota,
    keyDeadVoicedSound, keyDeadSemivoicedSound, keyDeadBelowdot);
  Table_ff5X: array[$ff50..$ff58] of Word = (keyHome, keyLeft, keyUp, keyRight,
    keyDown, keyPrior, keyNext, keyEnd, keyBegin);
  Table_ff6X: array[$ff60..$ff6b] of Word = (keySelect, keyPrintScreen,
    keyExecute, keyNIL, keyInsert, keyUndo, keyRedo, keyMenu, keyFind,
    keyCancel, keyHelp, keyBreak);
  Table_ff9X: array[$ff91..$ff9f] of Word = (keyPF1, keyPF2, keyPF3, keyPF4,
    keyP7, keyP4, keyP8, keyP6, keyP2, keyP9, keyP3, keyP1, keyP5, keyP0,
    keyPDecimal);
  Table_ffeX: array[$ffe1..$ffee] of Word = (keyShiftL, keyShiftR, keyCtrlL,
    keyCtrlR, keyCapsLock, keyShiftLock, keyMetaL, keyMetaR, keyAltL, keyAltR,
    keySuperL, keySuperR, keyHyperL, keyHyperR);
begin
  case KeySym of
    0..Ord('a')-1, Ord('z')+1..$bf, $f7:
      Result := KeySym;
    Ord('a')..Ord('z'), $c0..$f6, $f8..$ff:
      Result := KeySym - 32;
    $20a0..$20ac: Result := Table_20aX[KeySym];
    $fe20: Result := keyTab;
    $fe50..$fe60: Result := Table_feXX[KeySym];
    $ff08: Result := keyBackspace;
    $ff09: Result := keyTab;
    $ff0a: Result := keyLinefeed;
    $ff0b: Result := keyClear;
    $ff0d: Result := keyReturn;
    $ff13: Result := keyPause;
    $ff14: Result := keyScrollLock;
    $ff15: Result := keySysRq;
    $ff1b: Result := keyEscape;
    $ff50..$ff58: Result := Table_ff5X[KeySym];
    $ff60..$ff6b: Result := Table_ff6X[KeySym];
    $ff7e: Result := keyModeSwitch;
    $ff7f: Result := keyNumLock;
    $ff80: Result := keyPSpace;
    $ff89: Result := keyPTab;
    $ff8d: Result := keyPEnter;
    $ff91..$ff9f: Result := Table_ff9X[KeySym];
    $ffaa: Result := keyPAsterisk;
    $ffab: Result := keyPPlus;
    $ffac: Result := keyPSeparator;
    $ffad: Result := keyPMinus;
    $ffae: Result := keyPDecimal;
    $ffaf: Result := keyPSlash;
    $ffb0..$ffb9: Result := keyP0 + KeySym - $ffb0;
    $ffbd: Result := keyPEqual;
    $ffbe..$ffe0: Result := keyF1 + KeySym - $ffbe;
    $ffe1..$ffee: Result := Table_ffeX[KeySym];
    $ffff: Result := keyDelete;
  else
    Result := keyNIL;
  end;
{$IFDEF Debug}
  if Result = keyNIL then
    WriteLn('fpGFX/X11: Unknown KeySym: $', IntToHex(KeySym, 4));
{$ENDIF}
end;

procedure TXWindow.UpdateMotifWMHints;
type
  PMotifWmHints = ^TMotifWmHints;
  TMotifWmHints = packed record
    Flags, Functions, Decorations: LongWord;
    InputMode: LongInt;
    Status: LongWord;
  end;
const
  MWM_HINTS_FUNCTIONS = 1;
  MWM_HINTS_DECORATIONS = 2;
  FuncAll = 1;
  FuncResize = 2;
  FuncMove = 4;
  FuncMinimize = 8;
  FuncMaximize = 16;
  FuncClose = 32;
  DecorAll = 1;
  DecorBorder = 2;
  DecorResizeH = 4;
  DecorTitle = 8;
  DecorMenu = 16;
  DecorMinimize = 32;
  DecorMaximize = 64;
var
  PropType: TAtom;
  PropFormat: LongInt;
  PropItemCount, PropBytesAfter: LongWord;
  Hints: PMotifWmHints;
  NewHints: TMotifWmHints;
begin
  if TXDisplay(Display).FWMHints = 0 then
    TXDisplay(Display).FWMHints :=
      XInternAtom(TXDisplay(Display).Handle, '_MOTIF_WM_HINTS', False);

  XGetWindowProperty(TXDisplay(Display).Handle, Handle,
    TXDisplay(Display).FWMHints, 0, 5, False, AnyPropertyType, @PropType,
    @PropFormat, @PropItemCount, @PropBytesAfter, @Hints);

  NewHints.Flags := MWM_HINTS_FUNCTIONS or MWM_HINTS_DECORATIONS;
  NewHints.Functions := FuncResize or FuncMove or FuncMinimize or FuncClose;

  if FWindowType in [wtWindow, wtPopup, wtToolWindow] then
    NewHints.Decorations := DecorBorder or DecorTitle or DecorMenu or DecorMinimize
  else
    NewHints.Decorations := 0;
  if CanMaximize then
  begin
    NewHints.Functions := NewHints.Functions or FuncMaximize;
    NewHints.Decorations := NewHints.Decorations or DecorMaximize;
  end;

  if Assigned(Hints) then
  begin
    Hints^.Flags := Hints^.Flags or NewHints.Flags;
    Hints^.Decorations := NewHints.Decorations;
    Hints^.Functions := NewHints.Functions;
  end else
    Hints := @NewHints;

  XChangeProperty(TXDisplay(Display).Handle, Handle,
    TXDisplay(Display).FWMHints, TXDisplay(Display).FWMHints,
    32, PropModeReplace, Pointer(Hints), 5);
  if Hints <> @NewHints then
    XFree(Hints);
end;


// private methods

const
  ButtonTable: array[1..3] of TMouseButton = (mbLeft, mbMiddle, mbRight);

function TXWindow.StartComposing(const Event: TXKeyEvent): TKeySym;
begin
  SetLength(FComposeBuffer,
    XLookupString(@Event, @FComposeBuffer[1],
      SizeOf(FComposeBuffer) - 1, @Result, @FComposeStatus));
end;

procedure TXWindow.EndComposing;
var
  i: Integer;
begin
  if Assigned(OnKeyChar) then
    for i := 1 to Length(FComposeBuffer) do
      OnKeyChar(Self, FComposeBuffer[i]);
end;

procedure TXWindow.KeyPressed(var Event: TXKeyPressedEvent);
var
  KeySym: TKeySym;
begin
  KeySym := StartComposing(Event);
  if Assigned(OnKeyPressed) then
    OnKeyPressed(Self, KeySymToKeycode(KeySym), ConvertShiftState(Event.State));

  if (Event.State and (ControlMask or Mod1Mask)) = 0 then
    EndComposing;
end;

procedure TXWindow.KeyReleased(var Event: TXKeyReleasedEvent);
var
  KeySym: TKeySym;
begin
  KeySym := StartComposing(Event);
  if Assigned(OnKeyReleased) then
    OnKeyReleased(Self, KeySymToKeycode(KeySym),
      ConvertShiftState(Event.State));
  // Do not call EndComposing, as this would generate duplicate KeyChar events!
end;

procedure TXWindow.ButtonPressed(var Event: TXButtonPressedEvent);
var
  Sum: Integer;
  NewEvent: TXEvent;
begin
  case Event.Button of
    Button1..Button3:
      if Assigned(OnMousePressed) then
        OnMousePressed(Self, ButtonTable[Event.Button],
          ConvertShiftState(Event.State), Event.x, Event.y);
    Button4, Button5:		// Mouse wheel message
      begin
        if Event.Button = Button4 then
          Sum := -1
        else
          Sum := 1;

	// Check for other mouse wheel messages in the queue
	while XCheckTypedWindowEvent(DisplayHandle, Handle,
	  X.ButtonPress, @NewEvent) do
	begin
	  if NewEvent.xbutton.Button = 4 then
	    Dec(Sum)
	  else if NewEvent.xbutton.Button = 5 then
	    Inc(Sum)
	  else
	  begin
	    XPutBackEvent(DisplayHandle, @NewEvent);
	    break;
	  end;
	end;

        if Assigned(OnMouseWheel) then
          OnMouseWheel(Self, ConvertShiftState(Event.State),
	    Sum, Event.x, Event.y);
      end;
  end;
end;

procedure TXWindow.ButtonReleased(var Event: TXButtonReleasedEvent);
begin
  if (Event.Button >= 1) and (Event.Button <= 3) and
    Assigned(OnMouseReleased) then
    OnMouseReleased(Self, ButtonTable[Event.Button],
      ConvertShiftState(Event.State), Event.x, Event.y);
end;

procedure TXWindow.EnterWindow(var Event: TXEnterWindowEvent);
begin
  if Assigned(OnMouseEnter) then
    OnMouseEnter(Self, ConvertShiftState(Event.State), Event.x, Event.y);
end;

procedure TXWindow.LeaveWindow(var Event: TXLeaveWindowEvent);
begin
  if Assigned(OnMouseEnter) then
    OnMouseLeave(Self);
end;

procedure TXWindow.PointerMoved(var Event: TXPointerMovedEvent);
begin
  if Assigned(OnMouseMove) then
    OnMouseMove(Self, ConvertShiftState(Event.State), Event.x, Event.y);
end;

procedure TXWindow.Expose(var Event: TXExposeEvent);
{var
  IsNotEmpty: Boolean;
begin
WriteLn('Expose');
  if Assigned(OnPaint) then
    with Event do
    begin
      if not IsExposing then
      begin
        IsExposing := True;
	Canvas.SaveState;
	Canvas.EmptyClipRect;
      end;
      IsNotEmpty := Canvas.UnionClipRect(Rect(x, y, x + Width, y + Height));
      if Count = 0 then
      begin
        if IsNotEmpty then
	  OnPaint(Self, Canvas.GetClipRect);
	IsExposing := False;
	Canvas.RestoreState;
      end;
    end;
end;}
var
  r: TRect;
begin
  with Event do
    r := Rect(x, y, x + Width, y + Height);
  Display.DirtyList.AddRect(Self, r);
end;

procedure TXWindow.FocusIn(var Event: TXFocusInEvent);
begin
  if Assigned(OnFocusIn) then
    OnFocusIn(Self);
end;

procedure TXWindow.FocusOut(var Event: TXFocusOutEvent);
begin
  if Assigned(OnFocusOut) then
    OnFocusOut(Self);
end;

procedure TXWindow.Map(var Event: TXMapEvent);
begin
  if Assigned(OnShow) then
    OnShow(Self);
end;

procedure TXWindow.Unmap(var Event: TXUnmapEvent);
begin
  if Assigned(OnHide) then
    OnHide(Self);
end;

procedure TXWindow.Reparent(var Event: TXReparentEvent);
begin
  if Assigned(OnCreate) then
    OnCreate(Self);
end;

procedure TXWindow.Configure(var Event: TXConfigureEvent);
begin
  while XCheckTypedWindowEvent(DisplayHandle, Handle,
    X.ConfigureNotify, @Event) do;

  if (Event.x <> Left) or (Event.y <> Top) then
  begin
    FLeft := Event.x;
    FTop := Event.y;
    if Assigned(OnMove) then
      OnMove(Self);
  end;
  if (Event.Width <> Width) or (Event.Height <> Height) then
  begin
  // !!!: The following 2 lines are _quite_ wrong... :)
    FWidth := Event.Width;
    FHeight := Event.Height;
    FClientWidth := Event.Width;
    FClientHeight := Event.Height;
    TXCanvas(Canvas).Resized(ClientWidth, ClientHeight);
    if Assigned(OnResize) then
      OnResize(Self);
  end;
end;

procedure TXWindow.ClientMessage(var Event: TXClientMessageEvent);
begin
  if Event.message_type = TXDisplay(Display).FWMProtocols then
    if Event.Data.l[0] = TXDisplay(Display).FWMDeleteWindow then
    begin
      if CanClose then
        Free;
    end else
      WriteLn('fpGFX/X11: Unknown client protocol message: ', Event.Data.l[0])
  else
    WriteLn('fpGFX/X11: Unknown client message: ', Event.message_type);
end;


// -------------------------------------------------------------------
//   Global utility functions
// -------------------------------------------------------------------

function GetXEventName(Event: LongInt): String;
const
  EventNames: array[2..34] of String = (
    'KeyPress', 'KeyRelease', 'ButtonPress', 'ButtonRelease', 'MotionNotify',
    'EnterNotify', 'LeaveNotify', 'FocusIn', 'FocusOut', 'KeymapNotify',
    'Expose', 'GraphicsExpose', 'NoExpose', 'VisibilityNotify', 'CreateNotify',
    'DestroyNotify', 'UnmapNotify', 'MapNotify', 'MapRequest', 'ReparentNotify',
    'ConfigureNotify', 'ConfigureRequest', 'GravityNotify', 'ResizeRequest',
    'CirculateNotify', 'CirculateRequest', 'PropertyNotify', 'SelectionClear',
    'SelectionRequest', 'SelectionNotify', 'ColormapNotify', 'ClientMessage',
    'MappingNotify');
begin
  if (Event >= Low(EventNames)) and (Event <= High(EventNames)) then
    Result := EventNames[Event]
  else
    Result := '#' + IntToStr(Event);
end;


end.


{
  $Log$
  Revision 1.9  2001/02/09 20:47:25  sg
  * Better mouse wheel support
  * Implemented new fpGFX interface methods (DefaultFontNames, cursors...)
  * Minor bugfixes

  Revision 1.8  2001/01/18 15:00:14  sg
  * Added TGfxWindowType and implemented support for it

  Revision 1.7  2001/01/17 21:29:03  sg
  * Implemented dirty rectangle list
  * Minor improvements in window list management

  Revision 1.6  2001/01/11 23:07:24  sg
  *** empty log message ***

  Revision 1.5  2000/12/31 16:32:04  sg
  * Implemented TXWindow.SetClientSize and SetMinMaxClientSize
  * Adapted to new X11 units
  * Clipping bugfixes

  Revision 1.4  2000/12/24 13:15:29  sg
  * Implemented TXCanvas.EmptyClipRect and TXCanvas.UnionClipRect
  * Vastly improved handling of expose events: Successive expositions are
    gathered and delivered as a single event; the clipping region of the
    canvas will be set accordingly.
}
