{
    $Id$

    fpGFX  -  Free Pascal Graphics Library
    Copyright (C) 2000  by Sebastian Guenther, sg@freepascal.org

    X11/XLib target implementation

    See the file COPYING, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


unit GFX_X11;

interface

uses
  SysUtils, Classes,	// FPC units
  X, XLib, XUtil,	// X11 units
  GfxBase;		// fpGFX units

resourcestring
  // X11 exception strings
  SGCCreationFailed = 'Creation of X11 graphics context failed';
  SXContextInvalidFontClass = 'Tried to set font of class "%s" into X11 context; only TXFont is allowed.';
  SOpenDisplayFailed = 'Opening of display "%s" failed';
  SWindowCreationFailed = 'Creation of X11 window failed';

type

  EX11Error = class(Exception);

  TXDrawable = class;
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

  PXContextState = ^TXContextState;
  TXContextState = record
    Prev: PXContextState;
    Matrix: TGfxMatrix;
    Region: TRegion;
  end;

  TXContext = class(TGfxContext)
  private
    FHandle: TGC;
    FDefaultFontStruct: PXFontStruct;
    FFontStruct: PXFontStruct;
    FRegion: TRegion;
    FStateStackpointer: PXContextState;
    function GetDrawable: TXDrawable;
    function GetDisplayHandle: PDisplay;
  public
    constructor Create(ADrawable: TXDrawable);
    destructor Destroy; override;

    function CreateMemoryDrawable(AWidth, AHeight: Integer;
      const APixelFormat: TGfxPixelFormat;
      const AData: TGfxPixelData): TGfxDrawable; override;
    procedure SaveState; override;
    procedure RestoreState; override;
    function ExcludeClipRect(const ARect: TRect): Boolean; override;
    function IntersectClipRect(const ARect: TRect): Boolean; override;
    function GetClipRect: TRect; override;
    function MapColor(const AColor: TGfxColor): TGfxPixel; override;
    procedure SetColor(AColor: TGfxPixel); override;
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

    procedure CopyRect(ASource: TGfxDrawable; const ASourceRect: TRect;
      DestX, DestY: Integer); override;

    property Drawable: TXDrawable read GetDrawable;
    property DisplayHandle: PDisplay read GetDisplayHandle;
    property Handle: TGC read FHandle;
    property Region: TRegion read FRegion;
  end;


  TXDrawable = class(TGfxDrawable)
  private
    FColormap: TColormap;
    FDefaultFont: PXFontStruct;
    FDisplay: TXDisplay;
    FHandle: X.TDrawable;
    procedure Resized(NewWidth, NewHeight: Integer);
  public
    constructor Create(AColormap: TColormap; ADisplay: TXDisplay;
      AHandle: X.TDrawable; ADefaultFont: PXFontStruct);
    function CreateContext: TGfxContext; override;
    property Colormap: TColormap read FColormap;
    property Display: TXDisplay read FDisplay;
    property Handle: X.TDrawable read FHandle;
  end;

  TXPixmapDrawable = class(TXDrawable)
  public
    constructor Create(ADisplay: TXDisplay; AHandle: TPixmap;
      APixelFormat: TGfxPixelFormat);
  end;


  TXWindow = class;

  TXDisplay = class(TGfxDisplay)
  private
    FDefaultFont: PXFontStruct;
    FDisplayName: String;
    FHandle: PDisplay;
    FRootWindow: X.TWindow;
    FWindows: TList;
    FWMProtocols: TAtom;		// Atom for "WM_PROTOCOLS"
    FWMDeleteWindow: TAtom;		// Atom for "WM_DELETE_WINDOW"
    function GetHandle: PDisplay;
  public
    destructor Destroy; override;
    function CreateFont(const Descriptor: String): TGfxFont; override;
    function CreateWindow: TGfxWindow; override;
    procedure Run; override;

    function FindWindowByXID(XWindowID: X.TWindow): TXWindow;
    property Handle: PDisplay read GetHandle;
    property DisplayName: String read FDisplayName write FDisplayName;
  end;


  TXWindow = class(TGfxWindow)
  private
    FHandle: X.TWindow;
    FComposeStatus: TXComposeStatus;
    FComposeBuffer: String[32];
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
    procedure DestroyWindow(var Event: TXDestroyWindowEvent); message X.DestroyNotify;
    procedure Configure(var Event: TXConfigureEvent); message X.ConfigureNotify;
    procedure ClientMessage(var Event: TXClientMessageEvent); message X.ClientMessage;
  protected
    function ConvertShiftState(AState: Cardinal): TShiftState;
    function KeySymToKeycode(KeySym: TKeySym): Word;
    function GetTitle: String; override;
    procedure SetTitle(const ATitle: String); override;
  private
    constructor Create(ADisplay: TXDisplay);
  public
    destructor Destroy; override;
    procedure DefaultHandler(var Message); override;

    procedure SetSize(AWidth, AHeight: Integer); override;
    procedure SetMinMaxSize(AMinWidth, AMinHeight,
      AMaxWidth, AMaxHeight: Integer); override;
    procedure Show; override;
    procedure Invalidate(const ARect: TRect); override;
    procedure CaptureMouse; override;
    procedure ReleaseMouse; override;

    function DisplayHandle: PDisplay; {!!!: inline;	will crash with current compiler}
    property Handle: X.TWindow read FHandle;
  end;


// ===================================================================
// ===================================================================

implementation


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
//   TXContext
// -------------------------------------------------------------------

constructor TXContext.Create(ADrawable: TXDrawable);
begin
  inherited Create(ADrawable);
  FHandle := XCreateGC(DisplayHandle, Drawable.Handle, 0, nil);
  if not Assigned(Handle) then
    raise EX11Error.Create(SGCCreationFailed);

  XSetLineAttributes(DisplayHandle, Handle, 0,
    LineSolid, CapNotLast, JoinMiter);

  FDefaultFontStruct := Drawable.FDefaultFont;
  FFontStruct := FDefaultFontStruct;
  XSetFont(DisplayHandle, Handle, FFontStruct^.FID);
end;

destructor TXContext.Destroy;
begin
  if Assigned(Region) then
    XDestroyRegion(Region);
  if Assigned(Handle) then
    XFreeGC(DisplayHandle, Handle);
  inherited Destroy;
end;

function TXContext.CreateMemoryDrawable(AWidth, AHeight: Integer;
  const APixelFormat: TGfxPixelFormat;
  const AData: TGfxPixelData): TGfxDrawable;
var
  Pixmap: TPixmap;
begin
  if (APixelFormat.BitsPerPixel = 1) and
    (AData.Stride = (AWidth + 7) div 8) then
    Pixmap := XCreateBitmapFromData(DisplayHandle, Drawable.Handle,
      AData.Data, AWidth, AHeight)
  else
    // !!!: Pixel format is not supported yet
    raise EGfxError.Create(SUnsupportedPixelFormat);
  Result := TXPixmapDrawable.Create(Drawable.Display, Pixmap, APixelFormat);
end;

procedure TXContext.SaveState;
var
  SavedState: PXContextState;
  NewRegion: TRegion;
begin
  New(SavedState);
  SavedState^.Prev := FStateStackpointer;
  SavedState^.Matrix := Matrix;
  SavedState^.Region := Region;
  if Assigned(Region) then
  begin
    NewRegion := XCreateRegion;
    XUnionRegion(Region, NewRegion, NewRegion);
    SavedState^.Region := NewRegion;
  end else
    SavedState^.Region := nil;
  FStateStackpointer := SavedState;
end;

procedure TXContext.RestoreState;
var
  SavedState: PXContextState;
begin
  SavedState := FStateStackpointer;
  FStateStackpointer := SavedState^.Prev;
  Matrix := SavedState^.Matrix;

  if Assigned(Region) then
    XDestroyRegion(Region);
  FRegion := SavedState^.Region;
  if Assigned(Region) then
    XSetRegion(DisplayHandle, Handle, Region);

  Dispose(SavedState);
end;

function TXContext.ExcludeClipRect(const ARect: TRect): Boolean;
var
  x1, y1, x2, y2: Integer;
  RectRegion: TRegion;
  XRect: TXRectangle;
begin

  Transform(ARect.Left, ARect.Top, x1, y1);
  Transform(ARect.Right, ARect.Bottom, x2, y2);

  if (x2 > x1) and (y2 > y1) then
  begin
    if not Assigned(Region) then
    begin
      XRect.x := 0;
      XRect.y := 0;
      XRect.Width := Drawable.Width;
      XRect.Height := Drawable.Height;
      FRegion := XCreateRegion;
      XUnionRectWithRegion(@XRect, Region, Region);
    end;
    XRect.x := x1;
    XRect.y := y1;
    XRect.Width := x2 - x1;
    XRect.Height := y2 - y1;
    RectRegion := XCreateRegion;
    XUnionRectWithRegion(@XRect, RectRegion, RectRegion);
    XSubtractRegion(Region, RectRegion, Region);
    XDestroyRegion(RectRegion);
    XSetRegion(DisplayHandle, Handle, Region);

    Result := not XEmptyRegion(Region);
  end else
    Result := False;
end;

function TXContext.IntersectClipRect(const ARect: TRect): Boolean;
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

    if Assigned(Region) then
    begin
      XIntersectRegion(Region, RectRegion, Region);
      XDestroyRegion(RectRegion);
    end else
      FRegion := RectRegion;

    XSetRegion(DisplayHandle, Handle, Region);

    Result := not XEmptyRegion(Region);
  end else
    Result := False;
end;

function TXContext.GetClipRect: TRect;
var
  XRect: TXRectangle;
begin
  XClipBox(Region, @XRect);
  Result.Left := XRect.x;
  Result.Top := XRect.y;
  Result.Right := XRect.x + XRect.Width;
  Result.Bottom := XRect.y + XRect.Height;
end;

function TXContext.MapColor(const AColor: TGfxColor): TGfxPixel;
var
  Color: TXColor;
begin
  Color.Pixel := 0;
  Color.Red := AColor.Red;
  Color.Green := AColor.Green;
  Color.Blue := AColor.Blue;
  XAllocColor(DisplayHandle, Drawable.Colormap, @Color);
  Result := Color.Pixel;
end;

procedure TXContext.SetColor(AColor: TGfxPixel);
begin
  XSetForeground(DisplayHandle, Handle, AColor);
end;

procedure TXContext.SetFont(AFont: TGfxFont);
begin
  if AFont = nil then
  begin
    if FFontStruct = FDefaultFontStruct then
      exit;
    FFontStruct := FDefaultFontStruct;
  end else
  begin
    if not AFont.InheritsFrom(TXFont) then
      raise EGfxError.Create(SXContextInvalidFontClass);
    if TXFont(AFont).FontStruct = FFontStruct then
      exit;
    FFontStruct := TXFont(AFont).FontStruct;
  end;
  XSetFont(DisplayHandle, Handle, FFontStruct^.FID);
end;

procedure TXContext.SetLineStyle(ALineStyle: TGfxLineStyle);
const
  DotDashes: array[0..1] of Char = #1#1;
begin
  case ALineStyle of
    lsSolid:
      XSetLineAttributes(DisplayHandle, Handle, 0,
        LineSolid, CapNotLast, JoinMiter);
    lsDot:
      begin
        XSetLineAttributes(DisplayHandle, Handle, 0,
          LineOnOffDash, CapNotLast, JoinMiter);
        XSetDashes(DisplayHandle, Handle, 0, DotDashes, 2);
      end;
  end;
end;

procedure TXContext.DrawArc(const Rect: TRect; StartAngle, EndAngle: Single);
var
  x1, y1, x2, y2: Integer;
begin
  Transform(Rect.Left, Rect.Top, x1, y1);
  Transform(Rect.Right, Rect.Bottom, x2, y2);
  XDrawArc(DisplayHandle, Drawable.Handle, Handle,
    x1, y1, x2 - x1 - 1, y2 - y1 - 1,
    Round(StartAngle * 64), Round((EndAngle - StartAngle) * 64));
end;

procedure TXContext.DrawCircle(const Rect: TRect);
var
  x1, y1, x2, y2: Integer;
begin
  Transform(Rect.Left, Rect.Top, x1, y1);
  Transform(Rect.Right, Rect.Bottom, x2, y2);
  XDrawArc(DisplayHandle, Drawable.Handle, Handle,
    x1, y1, x2 - x1 - 1, y2 - y1 - 1, 0, 23040);
end;

procedure TXContext.DrawLine(x1, y1, x2, y2: Integer);
begin
  Transform(x1, y1, x1, y1);
  Transform(x2, y2, x2, y2);
  XDrawLine(DisplayHandle, Drawable.Handle, Handle, x1, y1, x2, y2);
end;

procedure TXContext.DrawPolyLine(const Coords: array of Integer);
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

  XDrawLines(DisplayHandle, Drawable.Handle, Handle, Points, PointsIndex, CoordModeOrigin);

  FreeMem(Points);
end;

procedure TXContext.DrawRect(const Rect: TRect);
var
  x1, y1, x2, y2: Integer;
begin
  Transform(Rect.Left, Rect.Top, x1, y1);
  Transform(Rect.Right, Rect.Bottom, x2, y2);
  XDrawRectangle(DisplayHandle, Drawable.Handle, Handle, x1, y1, x2 - x1 - 1, y2 - y1 - 1);
end;

procedure TXContext.FillRect(const Rect: TRect);
var
  r: TRect;
begin
  Transform(Rect.Left, Rect.Top, r.Left, r.Top);
  Transform(Rect.Right, Rect.Bottom, r.Right, r.Bottom);
  XFillRectangle(DisplayHandle, Drawable.Handle, Handle, r.Left, r.Top,
    r.Right - r.Left, r.Bottom - r.Top);
end;

function TXContext.FontCellHeight: Integer;
begin
  Result := FFontStruct^.Ascent + FFontStruct^.Descent;
end;

function TXContext.TextExtent(const AText: String): TSize;
var
  Direction, FontAscent, FontDescent: LongInt;
  CharStruct: TXCharStruct;
begin
  XQueryTextExtents(DisplayHandle, Handle^.GID, PChar(AText), Length(AText),
    @Direction, @FontAscent, @FontDescent, @CharStruct);
  Result.cx := CharStruct.Width;
  Result.cy := CharStruct.Ascent + CharStruct.Descent;
end;

function TXContext.TextWidth(const AText: String): Integer;
var
  Direction, FontAscent, FontDescent: LongInt;
  CharStruct: TXCharStruct;
begin
  XQueryTextExtents(DisplayHandle, Handle^.GID, PChar(AText), Length(AText),
    @Direction, @FontAscent, @FontDescent, @CharStruct);
  Result := CharStruct.Width;
end;

procedure TXContext.TextOut(x, y: Integer; const AText: String);
begin
  Transform(x, y, x, y);
  XDrawString(DisplayHandle, Drawable.Handle, Handle,
    x, y + FFontStruct^.ascent,
    PChar(AText), Length(AText));
end;

procedure TXContext.CopyRect(ASource: TGfxDrawable; const ASourceRect: TRect;
  DestX, DestY: Integer);
begin
  Transform(DestX, DestY, DestX, DestY);

  if (ASource.PixelFormat.BitsPerPixel = 1) and
    ASource.InheritsFrom(TXPixmapDrawable) then
  begin
    XSetClipMask(DisplayHandle, Handle, TXPixmapDrawable(ASource).Handle);
    XSetClipOrigin(DisplayHandle, Handle, DestX, DestY);
    XFillRectangle(DisplayHandle, Drawable.Handle, Handle, DestX, DestY,
      DestX + ASource.Width, DestY + ASource.Height);
    XSetClipMask(DisplayHandle, Handle, 0);
  end;
  { XCopyArea(DisplayHandle, Pixmap, Drawable.Handle, Handle, 0, 0,
    ABitmap.Width, ABitmap.Height, DestX, DestY); }
end;

{procedure TXContext.DrawBitmap(ABitmap: TGfxBitmap; DestX, DestY: Integer);
var
  Pixmap: TPixmap;
begin
  Transform(DestX, DestY, DestX, DestY);

  if ABitmap.BitsPerPixel = 1 then
  begin
    Pixmap := XCreateBitmapFromData(Drawable.Display, Drawable.Handle,
      ABitmap.Data, ABitmap.Width, ABitmap.Height);

    XSetClipMask(Drawable.Display, Handle, Pixmap);
    XSetClipOrigin(Drawable.Display, Handle, DestX, DestY);
    XFillRectangle(Drawable.Display, Drawable.Handle, Handle, DestX, DestY,
      DestX + ABitmap.Width, DestY + ABitmap.Height);
    XSetClipMask(Drawable.Display, Handle, 0);

    XFreePixmap(Drawable.Display, Pixmap);
  end;
  { XCopyArea(Drawable.Display, Pixmap, Drawable.Handle, Handle, 0, 0,
    ABitmap.Width, ABitmap.Height, DestX, DestY); }
end;}

function TXContext.GetDrawable: TXDrawable;
begin
  Result := TXDrawable(FDrawable);
end;

function TXContext.GetDisplayHandle: PDisplay;
begin
  Result := Drawable.Display.Handle;
end;


// -------------------------------------------------------------------
//   TXDrawable
// -------------------------------------------------------------------

constructor TXDrawable.Create(AColormap: TColormap; ADisplay: TXDisplay;
  AHandle: X.TDrawable; ADefaultFont: PXFontStruct);
var
  DummyWnd: PWindow;
  DummyInt: LongInt;
begin
  inherited Create(ADisplay);
  FColormap := AColormap;
  FDisplay := ADisplay;
  FHandle := AHandle;
  FDefaultFont := ADefaultFont;
  XGetGeometry(Display.Handle, Handle, @DummyWnd, @DummyInt, @DummyInt,
    @FWidth, @FHeight, @DummyInt, @DummyInt);
end;

function TXDrawable.CreateContext: TGfxContext;
begin
  Result := TXContext.Create(Self);
end;

procedure TXDrawable.Resized(NewWidth, NewHeight: Integer);
begin
  FWidth := NewWidth;
  FHeight := NewHeight;
end;


// -------------------------------------------------------------------
//   TXPixmapDrawable
// -------------------------------------------------------------------

constructor TXPixmapDrawable.Create(ADisplay: TXDisplay; AHandle: TPixmap;
  APixelFormat: TGfxPixelFormat);
begin
  inherited Create(0, ADisplay, AHandle, nil);
  FPixelFormat := APixelFormat;
end;


// -------------------------------------------------------------------
//   TXDisplay
// -------------------------------------------------------------------

function TXDisplay.GetHandle: PDisplay;
begin
  if not Assigned(FHandle) then
  begin
    if Length(DisplayName) = 0 then
      FDisplayName := XDisplayName(nil);
    FHandle := XOpenDisplay(PChar(DisplayName));
    if not Assigned(FHandle) then
      raise EX11Error.CreateFmt(SOpenDisplayFailed, [DisplayName]);
    FRootWindow := XDefaultRootWindow(FHandle);

    FDefaultFont := XLoadQueryFont(FHandle,
      '-adobe-helvetica-medium-r-normal-*-*-120-*-*-p-*-iso8859-1');
  end;
  Result := FHandle;
end;

destructor TXDisplay.Destroy;
begin
  if Assigned(FDefaultFont) then
  begin
    if FDefaultFont^.fid <> 0 then
      XUnloadFont(FHandle, FDefaultFont^.fid);
    XFreeFontInfo(nil, FDefaultFont, 0);
  end;

  if Assigned(FHandle) then
    XCloseDisplay(FHandle);
  if Assigned(FWindows) then
    FWindows.Free;
  inherited Destroy;
end;

function TXDisplay.CreateFont(const Descriptor: String): TGfxFont;
begin
  Result := TXFont.Create(Handle, Descriptor);
end;

function TXDisplay.CreateWindow: TGfxWindow;
begin
  Result := TXWindow.Create(Self);
  if not Assigned(FWindows) then
    FWindows := TList.Create;
  FWindows.Add(Result);
end;

procedure TXDisplay.Run;
var
  Event: TXEvent;
  Window: TXWindow;
begin
  if not Assigned(FWindows) then
    exit;

  GetHandle;
  WriteLn('fpGFX/X11: Entering X11 message loop...');
  while FWindows.Count > 0 do
  begin
    XNextEvent(FHandle, @Event);

    // According to a comment in X.h, the valid event types start with 2!
    if Event.EventType >= 2 then
    begin
      Window := FindWindowByXID(Event.XAny.Window);

      if Assigned(Window) then
        Window.Dispatch(Event)
      else
        WriteLn('fpGFX/X11: Received X event for unknown window: ', Event.EventType);
    end;
  end;
end;

function TXDisplay.FindWindowByXID(XWindowID: X.TWindow): TXWindow;
var
  i: Integer;
begin
  for i := 0 to FWindows.Count - 1 do
  begin
    Result := TXWindow(FWindows[i]);
    if Result.Handle = XWindowID then
      exit;
  end;
  Result := nil;
end;


// -------------------------------------------------------------------
//   TXWindow
// -------------------------------------------------------------------

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
  EndComposing;
end;

procedure TXWindow.ButtonPressed(var Event: TXButtonPressedEvent);
begin
  case Event.Button of
    Button1..Button3:
      if Assigned(OnMousePressed) then
        OnMousePressed(Self, ButtonTable[Event.Button],
          ConvertShiftState(Event.State), Event.x, Event.y);
    Button4:
      if Assigned(OnMouseWheel) then
        OnMouseWheel(Self, ConvertShiftState(Event.State),
	  -1, Event.x, Event.y);
    Button5:
      if Assigned(OnMouseWheel) then
        OnMouseWheel(Self, ConvertShiftState(Event.State),
	  1, Event.x, Event.y);
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
begin
  if Assigned(OnPaint) then
    with Event do
      OnPaint(Self, Rect(x, y, x + Width, y + Height));
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
  WriteLn('fpGFX/X11: XWindow: Map');
end;

procedure TXWindow.Unmap(var Event: TXUnmapEvent);
begin
  WriteLn('fpGFX/X11: XWindow: Unmap');
end;

procedure TXWindow.Reparent(var Event: TXReparentEvent);
begin
  WriteLn('fpGFX/X11: XWindow: Reparent');
  if Assigned(OnCreate) then
    OnCreate(Self);
end;

procedure TXWindow.DestroyWindow(var Event: TXDestroyWindowEvent);
begin
  WriteLn('fpGFX/X11: XWindow: DestroyWindow');
end;

procedure TXWindow.Configure(var Event: TXConfigureEvent);
begin
  FLeft := Event.x;
  FTop := Event.y;
  if (FWidth <> Event.Width) or (FHeight <> Event.Height) then
  begin
    FWidth := Event.Width;
    FHeight := Event.Height;
    TXDrawable(Drawable).Resized(FWidth, FHeight);
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

function TXWindow.DisplayHandle: PDisplay; {!!!: inline;}
begin
  Result := TXDisplay(Display).Handle;
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
begin
  case KeySym of
    0..Ord('a')-1, Ord('z')+1..$bf, $f7:
      Result := KeySym;
    Ord('a')..Ord('z'), $c0..$f6, $f8..$ff:
      Result := KeySym - 32;
    $20a0: Result := keyEcuSign;
    $20a1: Result := keyColonSign;
    $20a2: Result := keyCruzeiroSign;
    $20a3: Result := keyFFrancSign;
    $20a4: Result := keyLiraSign;
    $20a5: Result := keyMillSign;
    $20a6: Result := keyNairaSign;
    $20a7: Result := keyPesetaSign;
    $20a8: Result := keyRupeeSign;
    $20a9: Result := keyWonSign;
    $20aa: Result := keyNewSheqelSign;
    $20ab: Result := keyDongSign;
    $20ac: Result := keyEuroSign;
    $fe50: Result := keyDeadGrave;
    $fe51: Result := keyDeadAcute;
    $fe52: Result := keyDeadCircumflex;
    $fe53: Result := keyDeadTilde;
    $fe54: Result := keyDeadMacron;
    $fe55: Result := keyDeadBreve;
    $fe56: Result := keyDeadAbovedot;
    $fe57: Result := keyDeadDiaeresis;
    $fe58: Result := keyDeadRing;
    $fe59: Result := keyDeadDoubleacute;
    $fe5a: Result := keyDeadCaron;
    $fe5b: Result := keyDeadCedilla;
    $fe5c: Result := keyDeadOgonek;
    $fe5d: Result := keyDeadIota;
    $fe5e: Result := keyDeadVoicedSound;
    $fe5f: Result := keyDeadSemivoicedSound;
    $fe60: Result := keyDeadBelowdot;
    $ff08: Result := keyBackspace;
    $ff09: Result := keyTab;
    $ff0a: Result := keyLinefeed;
    $ff0b: Result := keyClear;
    $ff0d: Result := keyReturn;
    $ff13: Result := keyPause;
    $ff14: Result := keyScrollLock;
    $ff15: Result := keySysRq;
    $ff1b: Result := keyEscape;
    $ff50: Result := keyHome;
    $ff51: Result := keyLeft;
    $ff52: Result := keyUp;
    $ff53: Result := keyRight;
    $ff54: Result := keyDown;
    $ff55: Result := keyPrior;
    $ff56: Result := keyNext;
    $ff57: Result := keyEnd;
    $ff58: Result := keyBegin;
    $ff60: Result := keySelect;
    $ff61: Result := keyPrintScreen;
    $ff62: Result := keyExecute;
    $ff63: Result := keyInsert;
    $ff65: Result := keyUndo;
    $ff66: Result := keyRedo;
    $ff67: Result := keyMenu;
    $ff68: Result := keyFind;
    $ff69: Result := keyCancel;
    $ff6a: Result := keyHelp;
    $ff6b: Result := keyBreak;
    $ff7e: Result := keyModeSwitch;
    $ff7f: Result := keyNumLock;
    $ff80: Result := keyPSpace;
    $ff89: Result := keyPTab;
    $ff8d: Result := keyPEnter;
    $ff91: Result := keyPF1;
    $ff92: Result := keyPF2;
    $ff93: Result := keyPF3;
    $ff94: Result := keyPF4;
    $ff95: Result := keyP7;
    $ff96: Result := keyP4;
    $ff97: Result := keyP8;
    $ff98: Result := keyP6;
    $ff99: Result := keyP2;
    $ff9a: Result := keyP9;
    $ff9b: Result := keyP3;
    $ff9c: Result := keyP1;
    $ff9d: Result := keyP5;
    $ff9e: Result := keyP0;
    $ff9f: Result := keyPDecimal;
    $ffaa: Result := keyPAsterisk;
    $ffab: Result := keyPPlus;
    $ffac: Result := keyPSeparator;
    $ffad: Result := keyPMinus;
    $ffae: Result := keyPDecimal;
    $ffaf: Result := keyPSlash;
    $ffb0: Result := keyP0;
    $ffb1: Result := keyP1;
    $ffb2: Result := keyP2;
    $ffb3: Result := keyP3;
    $ffb4: Result := keyP4;
    $ffb5: Result := keyP5;
    $ffb6: Result := keyP6;
    $ffb7: Result := keyP7;
    $ffb8: Result := keyP8;
    $ffb9: Result := keyP9;
    $ffbd: Result := keyPEqual;
    $ffbe: Result := keyF1;
    $ffbf: Result := keyF2;
    $ffc0: Result := keyF3;
    $ffc1: Result := keyF4;
    $ffc2: Result := keyF5;
    $ffc3: Result := keyF6;
    $ffc4: Result := keyF7;
    $ffc5: Result := keyF8;
    $ffc6: Result := keyF9;
    $ffc7: Result := keyF10;
    $ffc8: Result := keyF11;
    $ffc9: Result := keyF12;
    $ffca: Result := keyF13;
    $ffcb: Result := keyF14;
    $ffcc: Result := keyF15;
    $ffcd: Result := keyF16;
    $ffce: Result := keyF17;
    $ffcf: Result := keyF18;
    $ffd0: Result := keyF19;
    $ffd1: Result := keyF20;
    $ffd2: Result := keyF21;
    $ffd3: Result := keyF22;
    $ffd4: Result := keyF23;
    $ffd5: Result := keyF24;
    $ffd6: Result := keyF25;
    $ffd7: Result := keyF26;
    $ffd8: Result := keyF27;
    $ffd9: Result := keyF28;
    $ffda: Result := keyF29;
    $ffdb: Result := keyF30;
    $ffdc: Result := keyF31;
    $ffdd: Result := keyF32;
    $ffde: Result := keyF33;
    $ffdf: Result := keyF34;
    $ffe0: Result := keyF35;
    $ffe1: Result := keyShiftL;
    $ffe2: Result := keyShiftR;
    $ffe3: Result := keyCtrlL;
    $ffe4: Result := keyCtrlR;
    $ffe5: Result := keyCapsLock;
    $ffe6: Result := keyShiftLock;
    $ffe7: Result := keyMetaL;
    $ffe8: Result := keyMetaR;
    $ffe9: Result := keyAltL;
    $ffea: Result := keyAltR;
    $ffeb: Result := keySuperL;
    $ffec: Result := keySuperR;
    $ffed: Result := keyHyperL;
    $ffee: Result := keyHyperR;
    $ffff: Result := keyDelete;
  else
    begin
      WriteLn('fpGFX/X11: Unknown KeySym: $', IntToHex(KeySym, 4));
      Result := keyNIL;
    end;
  end;
  // WriteLn('KeySym translated to $', IntToHex(Result, 4));	
end;

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

constructor TXWindow.Create(ADisplay: TXDisplay);
const
  WindowHints: TXWMHints = (
    flags: InputHint or StateHint;
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
begin
  inherited Create;

  FDisplay := ADisplay;

  Colormap := XDefaultColormap(ADisplay.Handle, XDefaultScreen(ADisplay.Handle));

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
    XDefaultRootWindow(DisplayHandle),	// parent: Create a top-level window
    SizeHints.x, SizeHints.y,		// position
    SizeHints.width, SizeHints.height,	// size
    0,					// border size
    CopyFromParent,			// depth
    CopyFromParent,			// class
    CopyFromParent,			// visual,
    CWEventMask or CWColormap,		// valuemask: What in "Attr" is valid?
    @Attr);

  if FHandle = 0 then
    raise EX11Error.Create(SWindowCreationFailed);

  XSetStandardProperties(DisplayHandle, Handle, nil, nil, 0,
    argv, argc, @SizeHints);

  XSetWMHints(DisplayHandle, Handle, @WindowHints);

  // We want to get a Client Message when the user tries to close this window
  if ADisplay.FWMProtocols = 0 then
    ADisplay.FWMProtocols := XInternAtom(ADisplay.Handle,
      'WM_PROTOCOLS', False);
  if ADisplay.FWMDeleteWindow = 0 then
    ADisplay.FWMDeleteWindow := XInternAtom(ADisplay.Handle,
      'WM_DELETE_WINDOW', False);

  XSetWMProtocols(ADisplay.Handle, FHandle, @ADisplay.FWMDeleteWindow, 1);


  FDrawable := TXDrawable.Create(Colormap, ADisplay, Handle,
    ADisplay.FDefaultFont);
end;

destructor TXWindow.Destroy;
begin
  if Assigned(OnClose) then
    OnClose(Self);

  XDestroyWindow(DisplayHandle, Handle);
  Drawable.Free;

  TXDisplay(Display).FWindows.Remove(Self);
  inherited Destroy;
end;

procedure TXWindow.DefaultHandler(var Message);
begin
  WriteLn('fpGFX/X11: Unhandled X11 event received: ', TXEvent(Message).EventType);
end;

procedure TXWindow.SetSize(AWidth, AHeight: Integer);
var
  ChangeMask: Cardinal;
  Changes: TXWindowChanges;
begin
  ChangeMask := 0;

  if AWidth <> Width then
  begin
    ChangeMask := CWWidth;
    Changes.Width := AWidth;
  end;

  if AHeight <> Height then
  begin
    ChangeMask := ChangeMask or CWHeight;
    Changes.Height := AHeight;
  end;

  if ChangeMask <> 0 then
    XConfigureWindow(DisplayHandle, Handle, ChangeMask, @Changes);
end;

procedure TXWindow.SetMinMaxSize(AMinWidth, AMinHeight,
  AMaxWidth, AMaxHeight: Integer);
var
  Supplied: LongInt;
  SizeHints: PXSizeHints;
begin
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
var
  Event: TXExposeEvent;
begin
  Event.EventType := X.Expose;
  Event.Window := FHandle;
  Event.x := ARect.Left;
  Event.y := ARect.Top;
  Event.Width := ARect.Right - ARect.Left;
  Event.Height := ARect.Bottom - ARect.Top;
  XSendEvent(DisplayHandle, Handle, False, 0, @Event);
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


end.


{
  $Log$
  Revision 1.1  2000/08/04 21:05:53  sg
  * First version in CVS

}
