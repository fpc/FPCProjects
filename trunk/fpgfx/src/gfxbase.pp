{
    $Id$

    fpGFX  -  Free Pascal Graphics Library
    Copyright (C) 2000  by Sebastian Guenther, sg@freepascal.org

    fpGFX basic declarations

    See the file COPYING, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit GfxBase;

interface

uses SysUtils, Classes;

{$IFDEF Debug}
{$ASSERTIONS On}
{$ENDIF}

{$INCLUDE keys.inc}

resourcestring
  // Exception message strings, to be used by target implementations
  SUnsupportedPixelFormat = 'Pixel format is not supported';


type

  TSize = record
    cx, cy: LongInt;
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
	  structure of TGfxMatrix! TGfxContext only allows you to read and
	  write the current transformation matrix so that the matrix can
	  easily be saved and restored.
  }

  TGfxMatrix = record
    _00, _20, _11, _21: Integer;
  end;

const
  GfxIdentityMatrix: TGfxMatrix = (_00: 1; _20: 0; _11: 1; _21: 0);


type

  EGfxError = class(Exception);

  // Colors

  TGfxColor = packed record
    Red, Green, Blue, Alpha: Word;
  end;

  TGfxPixel = LongWord;


  TGfxPixelFormat = record
    BitsPerPixel: Integer;
    RedMask: TGfxPixel;
    RedShift: Integer;
    GreenMask: TGfxPixel;
    GreenShift: Integer;
    BlueMask: TGfxPixel;
    BlueShift: Integer;
    AlphaMask: TGfxPixel;
    AlphaShift: Integer;
  end;

  PGfxPixelData = ^TGfxPixelData;
  TGfxPixelData = record
    Stride: LongInt;	// Width of scan line in bytes of 0 for default
    Data: Pointer;
  end;


const

  // Some predefined pixel formats:

  PixelFormatMono: TGfxPixelFormat = (
    BitsPerPixel: 1;
    RedMask: 1;
    RedShift: 0;
    GreenMask: 1;
    GreenShift: 0;
    BlueMask: 1;
    BlueShift: 0;
    AlphaMask: 0;
    AlphaShift: 0);

  PixelFormatRGB24: TGfxPixelFormat = (
    BitsPerPixel: 24;
    RedMask: $0000ff;
    RedShift: 0;
    GreenMask: $00ff00;
    GreenShift: 8;
    BlueMask: $ff0000;
    BlueShift: 0;
    AlphaMask: 0;
    AlphaShift: 0);

  PixelFormatBGR24: TGfxPixelFormat = (
    BitsPerPixel: 24;
    RedMask: $ff0000;
    RedShift: 0;
    GreenMask: $00ff00;
    GreenShift: 8;
    BlueMask: $0000ff;
    BlueShift: 0;
    AlphaMask: 0;
    AlphaShift: 0);

type

  TGfxDrawable = class;
  TGfxDevice = class;
  TGfxWindow = class;

  // Fonts

  TGfxFont = class
    { This class doesn't define anything... Create it from a drawable and
      destroy it with Free, as usual. }
  end;


  // Graphics context


  TGfxLineStyle = (lsSolid, lsDot);

  TGfxContext = class
  private
    FMatrix: TGfxMatrix;
    function GetDevice: TGfxDevice;
  protected
    FDrawable: TGfxDrawable;
    constructor Create(ADrawable: TGfxDrawable);
  public
    function CreateMemoryDrawable(AWidth, AHeight: Integer;
      const APixelFormat: TGfxPixelFormat;
      const AData: TGfxPixelData): TGfxDrawable; virtual; abstract;

    // Transformations
    procedure Transform(x, y: Integer; var OutX, OutY: Integer);
    procedure AppendTranslation(dx, dy: Integer);

    // Graphics context state
    procedure SaveState; virtual; abstract;
    procedure RestoreState; virtual; abstract;
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
    procedure Copy(ASource: TGfxDrawable; DestX, DestY: Integer); virtual;
    procedure CopyRect(ASource: TGfxDrawable; const ASourceRect: TRect;
      DestX, DestY: Integer); virtual; abstract;

    // Properties
    property Drawable: TGfxDrawable read FDrawable;
    property Device: TGfxDevice read GetDevice;
    property Matrix: TGfxMatrix read FMatrix write FMatrix;
  end;


  // Drawable

  TGfxDrawable = class
  protected
    FDevice: TGfxDevice;
    FWidth: Integer;
    FHeight: Integer;
    FPixelFormat: TGfxPixelFormat;
    constructor Create(ADevice: TGfxDevice);
  public
    function CreateContext: TGfxContext; virtual; abstract;
    property Device: TGfxDevice read FDevice;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property PixelFormat: TGfxPixelFormat read FPixelFormat;
  end;


  TGfxDevice = class
    function CreateFont(const Descriptor: String): TGfxFont; virtual; abstract;
  end;

  TGfxDisplay = class(TGfxDevice)
  public
    function CreateWindow: TGfxWindow; virtual; abstract;
    procedure Run; virtual; abstract;
  end;


// Bitmap class (read-only bitmap which can be used for several targets
// simultaneously)

  TGfxBitmap = class
  private
    FWidth, FHeight: Integer;
    FPixelFormat: TGfxPixelFormat;
    FData: TGfxPixelData;
  public
    constructor Create(AWidth, AHeight: Integer;
      const APixelFormat: TGfxPixelFormat; const AData: TGfxPixelData);
    procedure Draw(AContext: TGfxContext; DestX, DestY: Integer);
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
    procedure SetWidth(AWidth: Integer);
    procedure SetHeight(AHeight: Integer);
  protected
    FDisplay: TGfxDisplay;
    FDrawable: TGfxDrawable;
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
    property Drawable: TGfxDrawable read FDrawable;
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
  end;


function KeycodeToText(Key: Word; ShiftState: TShiftState): String;


implementation


// ===================================================================
//   TGfxContext
// ===================================================================

constructor TGfxContext.Create(ADrawable: TGfxDrawable);
begin
  inherited Create;
  FDrawable := ADrawable;
  Matrix := GfxIdentityMatrix;
end;

procedure TGfxContext.Transform(x, y: Integer; var OutX, OutY: Integer);
begin
  OutX := Matrix._00 * x + Matrix._20;
  OutY := Matrix._11 * y + Matrix._21;
end;

procedure TGfxContext.AppendTranslation(dx, dy: Integer);
begin
  // Append a translation to the existing transformation matrix
  Inc(FMatrix._20, FMatrix._00 * dx);
  Inc(FMatrix._21, FMatrix._11 * dy);
end;

procedure TGfxContext.DrawPolyLine(const Coords: array of Integer);
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

procedure TGfxContext.DrawRect(const Rect: TRect);
begin
  DrawPolyLine([Rect.Left, Rect.Top, Rect.Right - 1, Rect.Top,
    Rect.Right - 1, Rect.Bottom - 1, Rect.Left, Rect.Bottom - 1, Rect.Left, Rect.Top]);
end;

function TGfxContext.TextExtent(const AText: String): TSize;
begin
  Result.cx := TextWidth(AText);
  Result.cy := FontCellHeight;
end;

function TGfxContext.TextWidth(const AText: String): Integer;
begin
  Result := TextExtent(AText).cx;
end;

procedure TGfxContext.Copy(ASource: TGfxDrawable; DestX, DestY: Integer);
begin
  ASSERT(Assigned(ASource));
  CopyRect(ASource, Rect(0, 0, ASource.Width, ASource.Height), DestX, DestY);
end;

function TGfxContext.GetDevice: TGfxDevice;
begin
  Result := Drawable.Device;
end;


// ===================================================================
//   TGfxBitmap
//	NOTE: The current implementation is extremely slow, as it
//	doesn't cache the bitmap data for the different targets!!!
// ===================================================================

constructor TGfxDrawable.Create(ADevice: TGfxDevice);
begin
  inherited Create;
  FDevice := ADevice;
end;


// ===================================================================
//   TGfxBitmap
//	NOTE: The current implementation is extremely slow, as it
//	doesn't cache the bitmap data for the different targets!!!
// ===================================================================

constructor TGfxBitmap.Create(AWidth, AHeight: Integer;
  const APixelFormat: TGfxPixelFormat; const AData: TGfxPixelData);
begin
  inherited Create;
  FWidth := AWidth;
  FHeight := AHeight;
  FPixelFormat := APixelFormat;
  FData := AData;
end;

procedure TGfxBitmap.Draw(AContext: TGfxContext; DestX, DestY: Integer);
var
  MemDrawable: TGfxDrawable;
begin
  MemDrawable :=
    AContext.CreateMemoryDrawable(FWidth, FHeight, FPixelFormat, FData);
  AContext.Copy(MemDrawable, DestX, DestY);
  MemDrawable.Free;
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
  Revision 1.1  2000/08/04 21:05:52  sg
  * First version in CVS

}
