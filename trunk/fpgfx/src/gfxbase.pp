{
    $Id$

    fpGFX  -  Free Pascal Graphics Library
    Copyright (C) 2000 by
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
  SUnsupportedPixelFormat = 'Pixel format is not supported';


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

  TGfxColor = packed record
    Red, Green, Blue, Alpha: Word;
  end;

  TGfxPixel = LongWord;

  TGfxFormatType = (
    ftInvalid,
    ftMono,		// Monochrome
    ftPal4,		// 4 bpp using palette
    ftPalA4,		// 4 bpp using palette with alpha values > 0
    ftPal8,		// 8 bpp using palette
    ftPalA8,		// 8 bpp using palette with alpha values > 0
    ftRGB16,		// 16 bpp RGB
    ftRGBA16,		// 16 bpp RGBA
    ftRGB24,		// 24 bpp RGB
    ftRGBA24,		// 24 bpp RGBA
    ftRGB32,		// 32 bpp RGB
    ftRGBA32);		// 32 bpp RGBA

  // !!!: Change this to dynamic array as soon as FPC supports it!
  TGfxPalette = array[0..255] of TGfxColor;

  TGfxPixelFormat = record
    case FormatType: TGfxFormatType of
      ftPal4, ftPalA4, ftPal8, ftPalA8:
        (Palette: TGfxPalette);
      ftRGB16, ftRGBA16, ftRGB24, ftRGBA24, ftRGB32, ftRGBA32: (
	RedMask: TGfxPixel;
	GreenMask: TGfxPixel;
        BlueMask: TGfxPixel;
        AlphaMask: TGfxPixel);	// only used for RGBA types
    end;


const

  FormatTypeBPPTable: array[TGfxFormatType] of Integer =
    (0, 1, 4, 4, 8, 8, 16, 16, 24, 24, 32, 32);

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


  // Some predefined pixel formats:

  PixelFormatMono: TGfxPixelFormat = (
    FormatType: ftMono);

  PixelFormatRGB24: TGfxPixelFormat = (
    FormatType: ftRGB24;
    RedMask: $0000ff;
    GreenMask: $00ff00;
    BlueMask: $ff0000;
    AlphaMask: 0);

  PixelFormatBGR24: TGfxPixelFormat = (
    FormatType: ftRGB24;
    RedMask: $ff0000;
    GreenMask: $00ff00;
    BlueMask: $0000ff;
    AlphaMask: 0);

  PixelFormatRGB32: TGfxPixelFormat = (
    FormatType: ftRGB32;
    RedMask: $0000ff;
    GreenMask: $00ff00;
    BlueMask: $ff0000;
    AlphaMask: 0);

  PixelFormatBGR32: TGfxPixelFormat = (
    FormatType: ftRGB32;
    RedMask: $ff0000;
    GreenMask: $00ff00;
    BlueMask: $0000ff;
    AlphaMask: 0);

type

  EGfxError = class(Exception);

  TGfxImage = class;
  TGfxDevice = class;
  TGfxWindow = class;

  // Fonts

  TGfxFont = class
    { This class doesn't define anything... Create it from a canvas and
      destroy it with Free, as usual. }
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
    function CreateBitmap(AWidth, AHeight: Integer): TGfxCanvas; virtual; abstract;
    function CreateMonoBitmap(AWidth, AHeight: Integer): TGfxCanvas; virtual; abstract;

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
    procedure SetColor(AColor: TGfxPixel); virtual; abstract;
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
  protected
    constructor Create(AWidth, AHeight: Integer; APixelFormat: TGfxPixelFormat);
  public
    procedure Lock(var AData: Pointer; var AStride: LongWord); virtual; abstract;
    procedure Unlock; virtual;
    procedure SetPixelsFromData(AData: Pointer; AStride: LongWord);
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property PixelFormat: TGfxPixelFormat read FPixelFormat;
  end;


  TGfxDevice = class
    function CreateFont(const Descriptor: String): TGfxFont; virtual; abstract;
    function CreateImage(AWidth, AHeight: Integer;
      APixelFormat: TGfxPixelFormat): TGfxImage; virtual; abstract;
  end;


  TGfxDisplay = class(TGfxDevice)
  private
    FOnIdle: TNotifyEvent;
  public
    function CreateWindow: TGfxWindow; virtual; abstract;
    procedure Run; virtual; abstract;
    procedure BreakRun; virtual; abstract;
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
    WheelDelta, x, y: Integer) of object;
  // Painting
  TGfxPaintEvent = procedure(Sender: TObject; const Rect: TRect) of object;

  TGfxWindow = class
  private
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
    FOnResize: TNotifyEvent;
    FOnShow: TNotifyEvent;
    procedure SetWidth(AWidth: Integer);
    procedure SetHeight(AHeight: Integer);
  protected
    FDisplay: TGfxDisplay;
    FCanvas: TGfxCanvas;
    FLeft: Integer;
    FTop: Integer;
    FWidth: Integer;
    FHeight: Integer;
    function GetTitle: String; virtual;
    procedure SetTitle(const ATitle: String); virtual;
  public
    function CanClose: Boolean; virtual;
    procedure SetSize(AWidth, AHeight: Integer); virtual;
    procedure SetMinMaxSize(AMinWidth, AMinHeight,
      AMaxWidth, AMaxHeight: Integer); virtual;
    procedure Show; virtual; abstract;
    procedure Invalidate(const ARect: TRect); virtual; abstract;
    procedure CaptureMouse; virtual; abstract;
    procedure ReleaseMouse; virtual; abstract;

    property Display: TGfxDisplay read FDisplay;
    property Canvas: TGfxCanvas read FCanvas;
    // Window state
    property Left: Integer read FLeft;
    property Top: Integer read FTop;
    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;
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
    property OnResize: TNotifyEvent read FOnResize write FOnResize;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
  end;


function KeycodeToText(Key: Word; ShiftState: TShiftState): String;


implementation


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
  DrawPolyLine([Rect.Left, Rect.Top, Rect.Right - 1, Rect.Top,
    Rect.Right - 1, Rect.Bottom - 1, Rect.Left, Rect.Bottom - 1, Rect.Left, Rect.Top]);
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

constructor TGfxImage.Create(AWidth, AHeight: Integer;
  APixelFormat: TGfxPixelFormat);
begin
  FWidth := AWidth;
  FHeight := AHeight;
  FPixelFormat := APixelFormat;
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


// ===================================================================
//   TGfxWindow
// ===================================================================

procedure TGfxWindow.SetWidth(AWidth: Integer);
begin
  SetSize(AWidth, Height);
end;

procedure TGfxWindow.SetHeight(AHeight: Integer);
begin
  SetSize(Width, AHeight);
end;

function TGfxWindow.GetTitle: String;
begin
  SetLength(Result, 0);
end;

{ Methods which do nothing because GFX target implementations are not
  required to implement this methods: }

procedure TGfxWindow.SetTitle(const ATitle: String);
begin
  // Empty
end;

function TGfxWindow.CanClose: Boolean;
begin
  if Assigned(OnCanClose) then
    Result := OnCanClose(Self)
  else
    Result := True;
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
  Revision 1.4  2000/12/24 13:13:02  sg
  * Added TGfxCanvas.ReverseTransform and TGfxCanvas.EmptyClipRect

  Revision 1.3  2000/12/23 23:07:24  sg
  *** empty log message ***

  Revision 1.2  2000/10/28 20:27:32  sg
  * Changed handling of offscreen stuff to the concept of Bitmaps and
    Images

  Revision 1.1  2000/08/04 21:05:52  sg
  * First version in CVS

}
