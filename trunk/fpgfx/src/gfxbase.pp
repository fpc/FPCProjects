{
    $Id$

    fpGFX  -  Free Pascal Graphics Library
    Copyright (C) 2000 - 2001 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    fpGFX basic declarations

    See the file COPYING, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


unit GfxBase;

{$IFDEF Debug}
{$ASSERTIONS On}
{$ENDIF}

interface

uses SysUtils, Classes;

{$INCLUDE keys.inc}

resourcestring
  // Exception message strings, to be used by target implementations
  SUnsupportedPixelFormat = 'Pixel format (%d bits/pixel) is not supported';
  SIncompatibleCanvasForBlitting = 'Cannot blit from %s to %s';

type

  TSize = record
    cx, cy: Integer;
  end;


  { We use a special 3x3 matrix for transformations of coordinates. As the
    only allowed transformations are translations and scalations, we need a
    matrix with the following content ([x,y] is a variable):
        [0,0]   0   [2,0]
          0   [1,1] [2,1]
          0     0     1
    [0,0]: X scalation
    [2,0]: X translation
    [1,1]: Y scalation
    [2,1]: Y translation
    NOTE: This may change in the future! Don't assume anything about the
	  structure of TGfxMatrix! TGfxCanvas only allows you to read and
	  write the current transformation matrix so that the matrix can
	  easily be saved and restored.
  }

  TGfxMatrix = record
    _00, _20, _11, _21: Integer;
  end;

const
  GfxIdentityMatrix: TGfxMatrix = (_00: 1; _20: 0; _11: 1; _21: 0);

type

  PGfxColor = ^TGfxColor;
  TGfxColor = packed record
    Red, Green, Blue, Alpha: Word;
  end;

  PGfxPixel = ^TGfxPixel;
  TGfxPixel = LongWord;

  TGfxFormatType = (
    ftInvalid,
    ftMono,		// Monochrome
    ftPal4,		// 4 bpp using palette
    ftPal4A,		// 4 bpp using palette with alpha values > 0
    ftPal8,		// 8 bpp using palette
    ftPal8A,		// 8 bpp using palette with alpha values > 0
    ftRGB16,		// 15/16 bpp RGB
    ftRGBA16,		// 16 bpp RGBA
    ftRGB24,		// 24 bpp RGB
    ftRGB32,		// 32 bpp RGB
    ftRGBA32);		// 32 bpp RGBA

  TGfxPixelFormat = record
    case FormatType: TGfxFormatType of
      ftRGB16, ftRGBA16, ftRGB24, ftRGB32, ftRGBA32: (
	RedMask: TGfxPixel;
	GreenMask: TGfxPixel;
        BlueMask: TGfxPixel;
        AlphaMask: TGfxPixel);	// only used for RGBA types
    end;


const

  FormatTypeBPPTable: array[TGfxFormatType] of Integer =
    (0, 1, 4, 4, 8, 8, 16, 16, 24, 32, 32);

  // Some predefined colors:

  colTransparent: TGfxColor	= (Red: $0000; Green: $0000; Blue: $0000; Alpha: $ffff);
  colBlack: TGfxColor		= (Red: $0000; Green: $0000; Blue: $0000; Alpha: $0000);
  colBlue: TGfxColor		= (Red: $0000; Green: $0000; Blue: $ffff; Alpha: $0000);
  colGreen: TGfxColor		= (Red: $0000; Green: $ffff; Blue: $0000; Alpha: $0000);
  colCyan: TGfxColor		= (Red: $0000; Green: $ffff; Blue: $ffff; Alpha: $0000);
  colRed: TGfxColor		= (Red: $ffff; Green: $0000; Blue: $0000; Alpha: $0000);
  colMagenta: TGfxColor		= (Red: $ffff; Green: $0000; Blue: $ffff; Alpha: $0000);
  colYellow: TGfxColor		= (Red: $ffff; Green: $ffff; Blue: $0000; Alpha: $0000);
  colWhite: TGfxColor		= (Red: $ffff; Green: $ffff; Blue: $ffff; Alpha: $0000);
  colGray: TGfxColor		= (Red: $8000; Green: $8000; Blue: $8000; Alpha: $0000);
  colLtGray: TGfxColor		= (Red: $c000; Green: $c000; Blue: $c000; Alpha: $0000);
  colDkBlue: TGfxColor		= (Red: $0000; Green: $0000; Blue: $8000; Alpha: $0000);
  colDkGreen: TGfxColor		= (Red: $0000; Green: $8000; Blue: $0000; Alpha: $0000);
  colDkCyan: TGfxColor		= (Red: $0000; Green: $8000; Blue: $8000; Alpha: $0000);
  colDkRed: TGfxColor		= (Red: $8000; Green: $0000; Blue: $0000; Alpha: $0000);
  colDkMagenta: TGfxColor	= (Red: $8000; Green: $0000; Blue: $8000; Alpha: $0000);
  colDkYellow: TGfxColor	= (Red: $8000; Green: $8000; Blue: $0000; Alpha: $0000);

  webBlack: TGfxColor		= (Red: $0000; Green: $0000; Blue: $0000; Alpha: $0000);
  webMaroon: TGfxColor		= (Red: $8000; Green: $0000; Blue: $0000; Alpha: $0000);
  webGreen: TGfxColor		= (Red: $0000; Green: $8000; Blue: $0000; Alpha: $0000);
  webOlive: TGfxColor		= (Red: $8000; Green: $8000; Blue: $0000; Alpha: $0000);
  webNavy: TGfxColor		= (Red: $0000; Green: $0000; Blue: $8000; Alpha: $0000);
  webPurple: TGfxColor		= (Red: $8000; Green: $0000; Blue: $8000; Alpha: $0000);
  webTeal: TGfxColor		= (Red: $0000; Green: $8000; Blue: $8000; Alpha: $0000);
  webGray: TGfxColor		= (Red: $8000; Green: $8000; Blue: $8000; Alpha: $0000);
  webSilver: TGfxColor		= (Red: $c000; Green: $c000; Blue: $c000; Alpha: $0000);
  webRed: TGfxColor		= (Red: $ffff; Green: $0000; Blue: $0000; Alpha: $0000);
  webLime: TGfxColor		= (Red: $0000; Green: $ffff; Blue: $0000; Alpha: $0000);
  webYellow: TGfxColor		= (Red: $ffff; Green: $ffff; Blue: $0000; Alpha: $0000);
  webBlue: TGfxColor		= (Red: $0000; Green: $0000; Blue: $ffff; Alpha: $0000);
  webFuchsia: TGfxColor		= (Red: $ffff; Green: $0000; Blue: $ffff; Alpha: $0000);
  webAqua: TGfxColor		= (Red: $0000; Green: $ffff; Blue: $ffff; Alpha: $0000);
  webWhite: TGfxColor		= (Red: $ffff; Green: $ffff; Blue: $ffff; Alpha: $0000);


  // Some predefined pixel formats:

  PixelFormatMono: TGfxPixelFormat = (
    FormatType: ftMono);

  PixelFormatPal4: TGfxPixelFormat = (
    FormatType: ftPal4);

  PixelFormatPal4A: TGfxPixelFormat = (
    FormatType: ftPal4A);

  PixelFormatPal8: TGfxPixelFormat = (
    FormatType: ftPal8);

  PixelFormatPal8A: TGfxPixelFormat = (
    FormatType: ftPal8A);

  PixelFormatRGB24: TGfxPixelFormat = (
    FormatType:	ftRGB24;
    RedMask:	$0000ff;
    GreenMask:	$00ff00;
    BlueMask:	$ff0000;
    AlphaMask:	0);

  PixelFormatBGR24: TGfxPixelFormat = (
    FormatType:	ftRGB24;
    RedMask:	$ff0000;
    GreenMask:	$00ff00;
    BlueMask:	$0000ff;
    AlphaMask:	0);

  PixelFormatRGB32: TGfxPixelFormat = (
    FormatType:	ftRGB32;
    RedMask:	$0000ff;
    GreenMask:	$00ff00;
    BlueMask:	$ff0000;
    AlphaMask:	0);

  PixelFormatRGBA32: TGfxPixelFormat = (
    FormatType: ftRGB32;
    RedMask:	$000000ff;
    GreenMask:	$0000ff00;
    BlueMask:	$00ff0000;
    AlphaMask:	$ff000000);

  PixelFormatARGB32: TGfxPixelFormat = (
    FormatType: ftRGB32;
    RedMask:	$0000ff00;
    GreenMask:	$00ff0000;
    BlueMask:	$ff000000;
    AlphaMask:	$000000ff);

  PixelFormatBGR32: TGfxPixelFormat = (
    FormatType:	ftRGB32;
    RedMask:	$ff0000;
    GreenMask:	$00ff00;
    BlueMask:	$0000ff;
    AlphaMask:	0);

  PixelFormatBGRA32: TGfxPixelFormat = (
    FormatType:	ftRGB32;
    RedMask:	$00ff0000;
    GreenMask:	$0000ff00;
    BlueMask:	$000000ff;
    AlphaMask:	$ff000000);

  PixelFormatABGR32: TGfxPixelFormat = (
    FormatType:	ftRGB32;
    RedMask:	$ff000000;
    GreenMask:	$00ff0000;
    BlueMask:	$0000ff00;
    AlphaMask:	$000000ff);

type

  EGfxError = class(Exception);

  EGfxUnsupportedPixelFormat = class(EGfxError)
    constructor Create(const APixelFormat: TGfxPixelFormat);
  end;


  TGfxImage = class;
  TGfxDevice = class;
  TGfxDisplay = class;
  TGfxWindow = class;

  TGfxWindowType = (wtWindow, wtBorderlessWindow, wtPopup, wtBorderlessPopup,
    wtToolWindow, wtChild);

  TGfxCursor = (crDefault, crNone, crArrow, crCross, crIBeam, crSize, crSizeNS,
    crSizeWE, cpUpArrow, crHourGlass, crNoDrop, crHelp);


  // Fonts

  TGfxFontClass = (fcSerif, fcSansSerif, fcTypewriter, fcDingbats);

  TGfxFont = class
    { This class doesn't define anything... Create it from a canvas and
      destroy it with Free, as usual. }
  end;


  // Palettes

  TGfxPalette = class
  private
    FRefCount: LongInt;
    FEntryCount: Integer;
    FEntries: PGfxColor;
    function GetEntry(AIndex: Integer): TGfxColor;
  protected
    constructor Create(AEntryCount: Integer; AEntries: PGfxColor);
  public
    destructor Destroy; override;
    procedure AddRef;
    procedure Release;
    property EntryCount: Integer read FEntryCount;
    property Entries[AIndex: Integer]: TGfxColor read GetEntry;
  end;


  // Canvasses

  TGfxLineStyle = (lsSolid, lsDot);

  TGfxCanvas = class
  private
    FMatrix: TGfxMatrix;
  protected
    FWidth: Integer;
    FHeight: Integer;
    FPixelFormat: TGfxPixelFormat;
    constructor Create;
  public
    // Transformations
    procedure Transform(x, y: Integer; var OutX, OutY: Integer);
    procedure ReverseTransform(x, y: Integer; var OutX, OutY: Integer);
    procedure AppendTranslation(dx, dy: Integer);

    // Graphics state
    procedure SaveState; virtual; abstract;
    procedure RestoreState; virtual; abstract;
    procedure EmptyClipRect; virtual;
    function ExcludeClipRect(const ARect: TRect): Boolean; virtual; abstract;
    function IntersectClipRect(const ARect: TRect): Boolean; virtual; abstract;
    function UnionClipRect(const ARect: TRect): Boolean; virtual; abstract;
    function GetClipRect: TRect; virtual; abstract;
    function MapColor(const AColor: TGfxColor): TGfxPixel; virtual; abstract;
    procedure SetColor_(AColor: TGfxPixel); virtual; abstract;
    procedure SetColor(AColor: TGfxColor); virtual;
    procedure SetFont(AFont: TGfxFont); virtual; abstract;
    procedure SetLineStyle(ALineStyle: TGfxLineStyle); virtual; abstract;

    // Drawing functions
    procedure DrawArc(const Rect: TRect; StartAngle, EndAngle: Single); virtual; abstract;
    procedure DrawCircle(const Rect: TRect); virtual; abstract;
    procedure DrawLine(x1, y1, x2, y2: Integer); virtual; abstract;
    procedure DrawPolyLine(const Coords: array of Integer); virtual;
    procedure DrawRect(const Rect: TRect); virtual;
    procedure FillRect(const Rect: TRect); virtual; abstract;

    // Fonts
    function FontCellHeight: Integer; virtual; abstract;
    function TextExtent(const AText: String): TSize; virtual;
    function TextWidth(const AText: String): Integer; virtual;
    procedure TextOut(x, y: Integer; const AText: String); virtual; abstract;

    // Bit block transfers
    procedure Copy(ASource: TGfxCanvas; DestX, DestY: Integer); virtual;
    procedure CopyRect(ASource: TGfxCanvas; const ASourceRect: TRect;
      ADestX, ADestY: Integer); virtual; abstract;
    procedure MaskedCopy(ASource, AMask: TGfxCanvas; DestX, DestY: Integer); virtual;
{!!!:    procedure MaskedCopyRect(ASource, AMask: TGfxCanvas; const ASourceRect: TRect;
      ADestX, ADestY: Integer); virtual;}
    procedure MaskedCopyRect(ASource, AMask: TGfxCanvas; const ASourceRect: TRect;
      AMaskX, AMaskY, ADestX, ADestY: Integer); virtual; abstract;

    // Image drawing
    procedure DrawImage(AImage: TGfxImage; ADestX, ADestY: Integer); virtual;
    procedure DrawImageRect(AImage: TGfxImage; ASourceRect: TRect;
      ADestX, ADestY: Integer); virtual; abstract;


    // Properties
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property PixelFormat: TGfxPixelFormat read FPixelFormat;
    property Matrix: TGfxMatrix read FMatrix write FMatrix;
  end;


  TGfxImage = class
  private
    FWidth, FHeight: Integer;
    FPixelFormat: TGfxPixelFormat;
    FPalette: TGfxPalette;
    procedure SetPalette(APalette: TGfxPalette);
  protected
    constructor Create(AWidth, AHeight: Integer; APixelFormat: TGfxPixelFormat);
  public
    destructor Destroy; override;
    procedure Lock(var AData: Pointer; var AStride: LongWord); virtual; abstract;
    procedure Unlock; virtual;
    procedure SetPixelsFromData(AData: Pointer; AStride: LongWord);
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property PixelFormat: TGfxPixelFormat read FPixelFormat;
    property Palette: TGfxPalette read FPalette write SetPalette;
  end;


  TGfxScreen = class
  private
    FDisplay: TGfxDisplay;
  public
    constructor Create(ADisplay: TGfxDisplay);
    function CreatePalette(AEntryCount: Integer;
      AEntries: PGfxColor): TGfxPalette; virtual;
    function CreateBitmap(AWidth, AHeight: Integer): TGfxCanvas; virtual; abstract;
    function CreateMonoBitmap(AWidth, AHeight: Integer): TGfxCanvas; virtual; abstract;
    function CreateWindow: TGfxWindow;
    function CreateWindow(AParent: TGfxWindow;
      AWindowType: TGfxWindowType): TGfxWindow; virtual; abstract;
    property Display: TGfxDisplay read FDisplay;
  end;


  TGfxDevice = class
  public
    function CreateFont(const Descriptor: String): TGfxFont; virtual; abstract;
    function GetDefaultFontName(const AFontClass: TGfxFontClass): String; virtual;
    function CreateImage(AWidth, AHeight: Integer;
      APixelFormat: TGfxPixelFormat): TGfxImage; virtual; abstract;
  end;


  TGfxDisplay = class(TGfxDevice)
  private
    FOnIdle: TNotifyEvent;
  protected
    FDefaultScreen: TGfxScreen;
  public
    procedure Run; virtual; abstract;
    procedure BreakRun; virtual; abstract;
    property DefaultScreen: TGfxScreen read FDefaultScreen;
    property OnIdle: TNotifyEvent read FOnIdle write FOnIdle;
  end;


// Windowing support

  // Lifetime handling
  TGfxCanCloseEvent = function(Sender: TObject): Boolean of object;
  // Keyboard
  TGfxKeyEvent = procedure(Sender: TObject; Key: Word;
    Shift: TShiftState) of object;
  TGfxKeyCharEvent = procedure(Sender: TObject; KeyChar: Char) of object;
  // Mouse
  TMouseButton = (mbLeft, mbRight, mbMiddle);
  TGfxMouseButtonEvent = procedure(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; x, y: Integer) of object;
  TGfxMouseMoveEvent = procedure(Sender: TObject; Shift: TShiftState;
    x, y: Integer) of object;
  TGfxMouseWheelEvent = procedure(Sender: TObject; Shift: TShiftState;
    WheelDelta: Single; x, y: Integer) of object;
  // Painting
  TGfxPaintEvent = procedure(Sender: TObject; const Rect: TRect) of object;

  TGfxWindow = class
  private
    FCursor: TGfxCursor;
    FOnCreate: TNotifyEvent;
    FOnCanClose: TGfxCanCloseEvent;
    FOnClose: TNotifyEvent;
    FOnFocusIn: TNotifyEvent;
    FOnFocusOut: TNotifyEvent;
    FOnHide: TNotifyEvent;
    FOnKeyPressed: TGfxKeyEvent;
    FOnKeyReleased: TGfxKeyEvent;
    FOnKeyChar: TGfxKeyCharEvent;
    FOnMouseEnter: TGfxMouseMoveEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnMousePressed: TGfxMouseButtonEvent;
    FOnMouseReleased: TGfxMouseButtonEvent;
    FOnMouseMove: TGfxMouseMoveEvent;
    FOnMouseWheel: TGfxMouseWheelEvent;
    FOnPaint: TGfxPaintEvent;
    FOnMove: TNotifyEvent;
    FOnResize: TNotifyEvent;
    FOnShow: TNotifyEvent;
    procedure SetWidth(AWidth: Integer);
    procedure SetHeight(AHeight: Integer);
    procedure SetCursor(ACursor: TGfxCursor);
  protected
    FWindowType: TGfxWindowType;
    FScreen: TGfxScreen;
    FCanvas: TGfxCanvas;
    FLeft: Integer;
    FTop: Integer;
    FWidth: Integer;
    FHeight: Integer;
    FClientWidth: Integer;
    FClientHeight: Integer;
    function GetTitle: String; virtual;
    procedure SetTitle(const ATitle: String); virtual;
    procedure DoSetCursor; virtual; abstract;
  public
    function CanClose: Boolean; virtual;
    procedure SetPosition(ALeft, ATop: Integer); virtual;
    procedure SetSize(AWidth, AHeight: Integer); virtual;
    procedure SetMinMaxSize(AMinWidth, AMinHeight,
      AMaxWidth, AMaxHeight: Integer); virtual;
    procedure SetFixedSize(AWidth, AHeight: Integer);
    procedure SetClientSize(AWidth, AHeight: Integer); virtual;
    procedure SetMinMaxClientSize(AMinWidth, AMinHeight,
      AMaxWidth, AMaxHeight: Integer); virtual;
    procedure SetFixedClientSize(AWidth, AHeight: Integer);
    procedure Show; virtual; abstract;
    procedure Invalidate(const ARect: TRect); virtual; abstract;
    procedure PaintInvalidRegion; virtual; abstract;
    procedure CaptureMouse; virtual; abstract;
    procedure ReleaseMouse; virtual; abstract;

    property WindowType: TGfxWindowType read FWindowType;
    property Screen: TGfxScreen read FScreen;
    property Canvas: TGfxCanvas read FCanvas;
    // Window state
    property Left: Integer read FLeft;
    property Top: Integer read FTop;
    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;
    property ClientWidth: Integer read FClientWidth;
    property ClientHeight: Integer read FClientHeight;
    property Cursor: TGfxCursor read FCursor write SetCursor;
    property Title: String read GetTitle write SetTitle;
    // Event handlers
    property OnCreate: TNotifyEvent read FOnCreate write FOnCreate;
    property OnCanClose: TGfxCanCloseEvent read FOnCanClose write FOnCanClose;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnFocusIn: TNotifyEvent read FOnFocusIn write FOnFocusIn;
    property OnFocusOut: TNotifyEvent read FOnFocusOut write FOnFocusOut;
    property OnHide: TNotifyEvent read FOnHide write FOnHide;
    property OnKeyPressed: TGfxKeyEvent read FOnKeyPressed write FOnKeyPressed;
    property OnKeyReleased: TGfxKeyEvent read FOnKeyReleased write FOnKeyReleased;
    property OnKeyChar: TGfxKeyCharEvent read FOnKeyChar write FOnKeyChar;
    property OnMouseEnter: TGfxMouseMoveEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnMousePressed: TGfxMouseButtonEvent read FOnMousePressed write FOnMousePressed;
    property OnMouseReleased: TGfxMouseButtonEvent read FOnMouseReleased write FOnMouseReleased;
    property OnMouseMove: TGfxMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseWheel: TGfxMouseWheelEvent read FOnMouseWheel write FOnMouseWheel;
    property OnPaint: TGfxPaintEvent read FOnPaint write FOnPaint;
    property OnMove: TNotifyEvent read FOnMove write FOnMove;
    property OnResize: TNotifyEvent read FOnResize write FOnResize;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
  end;


// Some helpers:

operator = (const AColor1, AColor2: TGfxColor) b: Boolean;

function GetAvgColor(const AColor1, AColor2: TGfxColor): TGfxColor;

function KeycodeToText(Key: Word; ShiftState: TShiftState): String;


implementation


// ===================================================================
//   Exceptions
// ===================================================================

constructor EGfxUnsupportedPixelFormat.Create(const
  APixelFormat: TGfxPixelFormat);
begin
  inherited CreateFmt(SUnsupportedPixelFormat,
    [FormatTypeBPPTable[APixelFormat.FormatType]]);
end;


// ===================================================================
//   TGfxPalette
// ===================================================================

destructor TGfxPalette.Destroy;
begin
  if Assigned(FEntries) then
    FreeMem(FEntries);
  inherited Destroy;
end;

procedure TGfxPalette.AddRef;
begin
  Inc(FRefCount);
end;

procedure TGfxPalette.Release;
begin
  if FRefCount <= 0 then
    Free
  else
    Dec(FRefCount);
end;

constructor TGfxPalette.Create(AEntryCount: Integer; AEntries: PGfxColor);
begin
  inherited Create;
  FEntryCount := AEntryCount;
  GetMem(FEntries, EntryCount * SizeOf(TGfxColor));
  if Assigned(AEntries) then
    Move(AEntries^, FEntries^, EntryCount * SizeOf(TGfxColor));
end;

function TGfxPalette.GetEntry(AIndex: Integer): TGfxColor;
begin
  if (AIndex >= 0) and (AIndex < EntryCount) then
    Result := FEntries[AIndex]
  else
    Result := colBlack;
end;


// ===================================================================
//   TGfxCanvas
// ===================================================================

constructor TGfxCanvas.Create;
begin
  inherited Create;
  Matrix := GfxIdentityMatrix;
end;

procedure TGfxCanvas.Transform(x, y: Integer; var OutX, OutY: Integer);
begin
  OutX := Matrix._00 * x + Matrix._20;
  OutY := Matrix._11 * y + Matrix._21;
end;

procedure TGfxCanvas.ReverseTransform(x, y: Integer; var OutX, OutY: Integer);
begin
  OutX := (x - Matrix._20) div Matrix._00;
  OutY := (y - Matrix._21) div Matrix._11;
end;

procedure TGfxCanvas.AppendTranslation(dx, dy: Integer);
begin
  // Append a translation to the existing transformation matrix
  Inc(FMatrix._20, FMatrix._00 * dx);
  Inc(FMatrix._21, FMatrix._11 * dy);
end;

procedure TGfxCanvas.EmptyClipRect;
begin
  IntersectClipRect(Rect(0, 0, 0, 0));
end;

procedure TGfxCanvas.SetColor(AColor: TGfxColor);
begin
  SetColor_(MapColor(AColor));
end;

procedure TGfxCanvas.DrawPolyLine(const Coords: array of Integer);
var
  i: Integer;
begin
  i := Low(Coords);
  while i <= High(Coords) - 3 do
  begin
    DrawLine(Coords[i], Coords[i + 1], Coords[i + 2], Coords[i + 3]);
    Inc(i, 2);
  end;
end;

procedure TGfxCanvas.DrawRect(const Rect: TRect);
begin
  DrawPolyLine(
    [Rect.Left, Rect.Top,
     Rect.Right - 1, Rect.Top,
     Rect.Right - 1, Rect.Bottom - 1,
     Rect.Left, Rect.Bottom - 1,
     Rect.Left, Rect.Top]);
end;

function TGfxCanvas.TextExtent(const AText: String): TSize;
begin
  Result.cx := TextWidth(AText);
  Result.cy := FontCellHeight;
end;

function TGfxCanvas.TextWidth(const AText: String): Integer;
begin
  Result := TextExtent(AText).cx;
end;

procedure TGfxCanvas.Copy(ASource: TGfxCanvas; DestX, DestY: Integer);
begin
  ASSERT(Assigned(ASource));
  CopyRect(ASource, Rect(0, 0, ASource.Width, ASource.Height), DestX, DestY);
end;

procedure TGfxCanvas.MaskedCopy(ASource, AMask: TGfxCanvas; DestX, DestY: Integer);
begin
  MaskedCopyRect(ASource, AMask, Rect(0, 0, ASource.Width, ASource.Height),
    0, 0, DestX, DestY);
end;

{!!!:procedure TGfxCanvas.MaskedCopyRect(ASource, AMask: TGfxCanvas;
  const ASourceRect: TRect; ADestX, ADestY: Integer);
begin
  MaskedCopyRect(ASource, AMask, ASourceRect,
    ASourceRect.Left, ASourceRect.Top, ADestX, ADestY);
end;}

procedure TGfxCanvas.DrawImage(AImage: TGfxImage; ADestX, ADestY: Integer);
var
  r: TRect;
begin
  r.Left := 0;
  r.Top := 0;
  r.Right := AImage.Width;
  r.Bottom := AImage.Height;
  DrawImageRect(AImage, r, ADestX, ADestY);
end;


// ===================================================================
//   TGfxImage
// ===================================================================

// public methods

destructor TGfxImage.Destroy;
begin
  if Assigned(Palette) then
    Palette.Release;
  inherited Destroy;
end;

procedure TGfxImage.Unlock;
begin
  // Default implementation: Do nothing...
end;

procedure TGfxImage.SetPixelsFromData(AData: Pointer; AStride: LongWord);
var
  DestData: Pointer;
  DestStride, BytesPerScanline: LongWord;
  y: Integer;
begin
  if Height <= 0 then
    exit;

  Lock(DestData, DestStride);
  try
    if DestStride = AStride then
      Move(AData^, DestData^, AStride * Height)
    else
    begin
      if DestStride > AStride then
        BytesPerScanline := AStride
      else
        BytesPerScanline := DestStride;

      y := 0;
      while True do
      begin
        Move(AData^, DestData^, BytesPerScanline);
	Inc(y);
	if y = Height then
	  break;
	Inc(AData, AStride);
	Inc(DestData, DestStride);
      end;
    end;
  finally
    Unlock;
  end;
end;


// protected methods

constructor TGfxImage.Create(AWidth, AHeight: Integer;
  APixelFormat: TGfxPixelFormat);
begin
  FWidth := AWidth;
  FHeight := AHeight;
  FPixelFormat := APixelFormat;
end;


// private methods

procedure TGfxImage.SetPalette(APalette: TGfxPalette);
begin
  if APalette <> Palette then
  begin
    if Assigned(Palette) then
      Palette.Release;

    FPalette := APalette;

    if Assigned(Palette) then
      Palette.AddRef;
  end;
end;


// ===================================================================
//   TGfxScreen
// ===================================================================

constructor TGfxScreen.Create(ADisplay: TGfxDisplay);
begin
  inherited Create;
  FDisplay := ADisplay;
end;

function TGfxScreen.CreatePalette(AEntryCount: Integer;
  AEntries: PGfxColor): TGfxPalette;
begin
  Result := TGfxPalette.Create(AEntryCount, AEntries);
end;

function TGfxScreen.CreateWindow: TGfxWindow;
begin
  Result := CreateWindow(nil, wtWindow);
end;


// ===================================================================
//   TGfxDevice
// ===================================================================

function TGfxDevice.GetDefaultFontName(
  const AFontClass: TGfxFontClass): String;
const
  FontNames: array[TGfxFontClass] of String = (
    'times', 'helvetica', 'courier', 'dingbats');
begin
  Result := FontNames[AFontClass];
end;


// ===================================================================
//   TGfxWindow
// ===================================================================

// public methods

function TGfxWindow.CanClose: Boolean;
begin
  if Assigned(OnCanClose) then
    Result := OnCanClose(Self)
  else
    Result := True;
end;

procedure TGfxWindow.SetPosition(ALeft, ATop: Integer);
begin
  // Empty
end;

procedure TGfxWindow.SetSize(AWidth, AHeight: Integer);
begin
  // Empty
end;

procedure TGfxWindow.SetMinMaxSize(AMinWidth, AMinHeight,
  AMaxWidth, AMaxHeight: Integer);
begin
  // Empty
end;

procedure TGfxWindow.SetFixedSize(AWidth, AHeight: Integer);
begin
  SetSize(AWidth, AHeight);
  SetMinMaxSize(AWidth, AHeight, AWidth, AHeight);
end;

procedure TGfxWindow.SetClientSize(AWidth, AHeight: Integer);
begin
  // Empty
end;

procedure TGfxWindow.SetMinMaxClientSize(AMinWidth, AMinHeight,
  AMaxWidth, AMaxHeight: Integer);
begin
  // Empty
end;

procedure TGfxWindow.SetFixedClientSize(AWidth, AHeight: Integer);
begin
  SetClientSize(AWidth, AHeight);
  SetMinMaxClientSize(AWidth, AHeight, AWidth, AHeight);
end;


// protected methods

function TGfxWindow.GetTitle: String;
begin
  SetLength(Result, 0);
end;

procedure TGfxWindow.SetTitle(const ATitle: String);
begin
  // Empty
end;


// private methods

procedure TGfxWindow.SetWidth(AWidth: Integer);
begin
  SetSize(AWidth, Height);
end;

procedure TGfxWindow.SetHeight(AHeight: Integer);
begin
  SetSize(Width, AHeight);
end;

procedure TGfxWindow.SetCursor(ACursor: TGfxCursor);
begin
  if ACursor <> Cursor then
  begin
    FCursor := ACursor;
    DoSetCursor;
  end;
end;


// ===================================================================
//   Global functions
// ===================================================================

// Color functions

operator = (const AColor1, AColor2: TGfxColor) b: Boolean;
begin
  b := (AColor1.Red = AColor2.Red) and (AColor1.Green = AColor2.Green) and
    (AColor1.Blue = AColor2.Blue) and (AColor1.Alpha = AColor2.Alpha);
end;

function GetAvgColor(const AColor1, AColor2: TGfxColor): TGfxColor;
begin
  Result.Red := AColor1.Red + (AColor2.Red - AColor1.Red) div 2;
  Result.Green := AColor1.Green + (AColor2.Green - AColor1.Green) div 2;
  Result.Blue := AColor1.Blue + (AColor2.Blue - AColor1.Blue) div 2;
  Result.Alpha := AColor1.Alpha + (AColor2.Alpha - AColor1.Alpha) div 2;
end;


// Keyboard functions

function KeycodeToText(Key: Word; ShiftState: TShiftState): String;

  function GetASCIIText: String;
  var
    c: Char;
  begin
    c := Chr(Key and $ff);
    case c of
      #13: Result := Result + 'Enter';
      #127: Result := Result + 'Del';
      '+': Result := Result + 'Plus'
      else
        Result := Result + c;
    end;
  end;

var
  s: String;
begin
  SetLength(Result, 0);

  if ssShift in ShiftState then
    Result := 'Shift+';
  if ssCtrl in ShiftState then
    Result := 'Ctrl+';
  if ssAlt in ShiftState then
    Result := 'Alt+';

  if (Key > Ord(' ')) and (Key < 255) then
  begin
    Result := Result + GetASCIIText;
    exit;
  end;

  case Key of
    keyNul: s := 'Null';
    keyBackSpace: s := 'Backspace';
    keyTab: s := 'Tab';
    keyLinefeed: s := 'Linefeed';
    keyReturn: s := 'Enter';
    keyEscape: s := 'Esc';
    Ord(' '): s := 'Space';
    keyDelete: s := 'Del';
    keyVoid: s := 'Void';
    keyBreak: s := 'Break';
    keyScrollForw: s := 'ScrollForw';
    keyScrollBack: s := 'ScrollBack';
    keyBoot: s := 'Boot';
    keyCompose: s := 'Compose';
    keySAK: s := 'SAK';
    keyUndo: s := 'Undo';
    keyRedo: s := 'Redo';
    keyMenu: s := 'Menu';
    keyCancel: s := 'Cancel';
    keyPrintScreen: s := 'PrtScr';
    keyExecute: s := 'Exec';
    keyFind: s := 'Find';
    keyBegin: s := 'Begin';
    keyClear: s := 'Clear';
    keyInsert: s := 'Ins';
    keySelect: s := 'Select';
    keyMacro: s := 'Macro';
    keyHelp: s := 'Help';
    keyDo: s := 'Do';
    keyPause: s := 'Pause';
    keySysRq: s := 'SysRq';
    keyModeSwitch: s := 'ModeSw';
    keyUp: s := 'Up';
    keyDown: s := 'Down';
    keyLeft: s := 'Left';
    keyRight: s := 'Right';
    keyPrior: s := 'PgUp';
    keyNext: s := 'PgDown';
    keyHome: s := 'Home';
    keyEnd: s := 'End';
    keyF0..keyF64: s := 'F' + IntToStr(Key - keyF0);
    keyP0..keyP9: s := 'KP' + Chr(Key - keyP0 + Ord('0'));
    keyPA..keyPF: s := 'KP' + Chr(Key - keyPA + Ord('A'));
    keyPPlus, keyPMinus, keyPSlash, keyPStar, keyPEqual, keyPSeparator,
      keyPDecimal, keyPParenLeft, keyPParenRight, keyPSpace, keyPEnter,
      keyPTab: s := 'KP' + GetASCIIText;
    keyPPlusMinus: s := 'KPPlusMinus';
    keyPBegin: s := 'KPBegin';
    keyPF1..keyPF9: s := 'KPF' + IntToStr(Key - keyPF1);
    keyShiftL: s := 'ShiftL';
    keyShiftR: s := 'ShiftR';
    keyCtrlL: s := 'CtrlL';
    keyCtrlR: s := 'CtrlR';
    keyAltL: s := 'AltL';
    keyAltR: s := 'AltR';
    keyMetaL: s := 'MetaL';
    keyMetaR: s := 'MetaR';
    keySuperL: s := 'SuperL';
    keySuperR: s := 'SuperR';
    keyHyperL: s := 'HyperL';
    keyHyperR: s := 'HyperR';
    keyAltGr: s := 'AltGr';
    keyCaps: s := 'Caps';
    keyNum: s := 'Num';
    keyScroll: s := 'Scroll';
    keyShiftLock: s := 'ShiftLock';
    keyCtrlLock: s := 'CtrlLock';
    keyAltLock: s := 'AltLock';
    keyMetaLock: s := 'MetaLock';
    keySuperLock: s := 'SuperLock';
    keyHyperLock: s := 'HyperLock';
    keyAltGrLock: s := 'AltGrLock';
    keyCapsLock: s := 'CapsLock';
    keyNumLock: s := 'NumLock';
    keyScrollLock: s := 'ScrollLock';
    keyDeadRing: s := 'DeadRing';
    keyDeadCaron: s := 'DeadCaron';
    keyDeadOgonek: s := 'DeadOgonek';
    keyDeadIota: s := 'DeadIota';
    keyDeadDoubleAcute: s := 'DeadDoubleAcute';
    keyDeadBreve: s := 'DeadBreve';
    keyDeadAboveDot: s := 'DeadAboveDot';
    keyDeadBelowDot: s := 'DeadBelowDot';
    keyDeadVoicedSound: s := 'DeadVoicedSound';
    keyDeadSemiVoicedSound: s := 'DeadSemiVoicedSound';
    keyDeadAcute: s := 'DeadAcute';
    keyDeadCedilla: s := 'DeadCedilla';
    keyDeadCircumflex: s := 'DeadCircumflex';
    keyDeadDiaeresis: s := 'DeadDiaeresis';
    keyDeadGrave: s := 'DeadGrave';
    keyDeadTilde: s := 'DeadTilde';
    keyDeadMacron: s := 'DeadMacron';

    keyEcuSign: s := 'Ecu';
    keyColonSign: s := 'Colon';
    keyCruzeiroSign: s := 'Cruzeiro';
    keyFFrancSign: s := 'FFranc';
    keyLiraSign: s := 'Lira';
    keyMillSign: s := 'Mill';
    keyNairaSign: s := 'Naira';
    keyPesetaSign: s := 'Peseta';
    keyRupeeSign: s := 'Rupee';
    keyWonSign: s := 'Won';
    keyNewSheqelSign: s := 'NewShequel';
    keyDongSign: s := 'Dong';
    keyEuroSign: s := 'Euro';
  else
    s := '#' + IntToHex(Key, 4);
  end;
  Result := Result + s;
end;


end.


{
  $Log$
  Revision 1.8  2001/02/09 20:43:05  sg
  * Added more color definitions
  * Overloaded TGfxCanvas.SetColor to accept a TGfxColor as well
  * Added support for setting the cursor type within a window
  * Added TGfxWindow.PaintInvalidRegion
  * Added GetAvgColor helper function

  Revision 1.7  2001/01/18 15:00:14  sg
  * Added TGfxWindowType and implemented support for it

  Revision 1.6  2001/01/11 23:07:24  sg
  *** empty log message ***

  Revision 1.5  2000/12/31 16:29:59  sg
  * Added TGfxWindow.SetClientSize and SetMinMaxClientSize

  Revision 1.4  2000/12/24 13:13:02  sg
  * Added TGfxCanvas.ReverseTransform and TGfxCanvas.EmptyClipRect
}
