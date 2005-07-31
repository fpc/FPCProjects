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
//   Graphics Support (Drawing, Bitmaps...)
// ===================================================================

type

  PRect = ^TRect;
  TColor = type LongInt;
  TBitmap = class;


const

  clBlack   = TColor(  $000000);
  clMaroon  = TColor(  $000080);
  clGreen   = TColor(  $008000);
  clOlive   = TColor(  $008080);
  clNavy    = TColor(  $800000);
  clPurple  = TColor(  $800080);
  clTeal    = TColor(  $808000);
  clGray    = TColor(  $808080);	// identical to clDkGray
  clSilver  = TColor(  $c0c0c0);	// identical to clLtGray
  clRed     = TColor(  $0000ff);
  clLime    = TColor(  $00ff00);
  clYellow  = TColor(  $00ffff);
  clBlue    = TColor(  $ff0000);
  clFuchsia = TColor(  $ff00ff);
  clAqua    = TColor(  $ffff00);
  clLtGray  = TColor(  $c0c0c0);
  clDkGray  = TColor(  $808080);
  clWhite   = TColor(  $ffffff);
  clNone    = TColor($1fffffff);
  clDefault = TColor($20000000);


type

// -------------------------------------------------------------------
//   TGraphicsObject
// -------------------------------------------------------------------

  {We use an extended TGraphicsObject in KCL for a simple reason: Under Win32,
   all canvasses have an underlying device context, and all graphic objects use
   the according Win32 graphics objects (TFont uses a HFont etc.). But this is
   not true for other platforms, where a canvas for e.g. printing may be a
   totally different canvas than those used for displaying graphics on the
   screen. Therefore KCL uses abstract graphics objects, and each distinct
   canvas implementation has to care about managing, caching etc. for itself.
   For this we need a small extension to TGraphicsObject: A list of notifiers;
   each notifier is called whenever a property of the object changes or when the
   object is being destroyed. They are used by canvas implementations to keep
   track of all changes, in order to enable them to change or free internally
   allocated resources.}


  TGraphicsObject = class;
  TGraphicsObjectOperation = (goaChanged, goaDeleted);
  TGraphicsObjectNotify = procedure(Sender: TGraphicsObject;
    AOperation: TGraphicsObjectOperation; UserData: TObject) of object;

  TGraphicsObject = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    NotifierList: TList;
  protected
    Modified: Boolean;
    procedure Changed; dynamic;
  public
    destructor Destroy; override;
    procedure AddNotifier(ANotifier: TGraphicsObjectNotify; UserData: TObject);
    procedure RemoveNotifier(ANotifier: TGraphicsObjectNotify);
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;


// -------------------------------------------------------------------
//   TFont
// -------------------------------------------------------------------

  TFontPitch = (fpDefault, fpVariable, fpFixed);
  TFontName = type String;

  TFontStyle = (fsBold, fsItalic, fsUnderline, fsStrikeOut);
  TFontStyles = set of TFontStyle;

  TFont = class(TGraphicsObject)
  private
    // FCharset: TFontCharset;
    FColor: TColor;
    FHeight: Integer;
    FName: TFontName;
    FPitch: TFontPitch;
    FSize: Integer;
    FStyle: TFontStyles;
    // procedure SetCharset(ACharset: TFontCharset);
    procedure SetColor(AColor: TColor);
    procedure SetHeight(AHeight: Integer);
    procedure SetName(const AName: TFontName);
    procedure SetPitch(APitch: TFontPitch);
    procedure SetSize(ASize: Integer);
    procedure SetStyle(AStyle: TFontStyles);
  public
    procedure Assign(Source: TPersistent); override;
  published
    // property Charset: TFontCharset read FCharset write SetCharset;
    property Color: TColor read FColor write SetColor;
    property Height: Integer read FHeight write SetHeight;
    property Name: TFontName read FName write SetName;
    property Pitch: TFontPitch read FPitch write SetPitch;
    property Size: Integer read FSize write SetSize;
    property Style: TFontStyles read FStyle write SetStyle;
  end;


// -------------------------------------------------------------------
//   TCanvas
// -------------------------------------------------------------------

  TCanvasStates = (csHandleValid, csFontValid, csPenValid, csBrushValid);
  TCanvasState = set of TCanvasStates;

  TCanvas = class
  protected
    FColor: TColor;
    FFont: TFont;

    procedure SetColor(AColor: TColor); virtual; abstract;
    procedure SetFont(AFont: TFont); virtual; abstract;

    procedure RequiredState(ReqState: TCanvasState); virtual; abstract;
  public
    property Color: TColor read FColor write SetColor;
    property Font: TFont read FFont write SetFont;

    procedure MoveTo(x, y: Integer); virtual; abstract;
    procedure LineTo(x, y: Integer); virtual; abstract;
    procedure FillRect(const rect: TRect); virtual; abstract;
    procedure Text(x, y: Integer; const AText: String); virtual; abstract;
    procedure CopyRect(dx, dy, dw, dh, sx, sy: Integer); virtual; abstract;
    procedure DrawBitmap(ADestX, ADestY: Integer; ABitmap: TBitmap;
      ASourceX, ASourceY, ASourceW, ASourceH: Integer); virtual; abstract;

    function TextWidth(const AText: String): Integer; virtual; abstract;
    function TextHeight(const AText: String): Integer; virtual; abstract;

    // Comfort functions
    procedure FillRect(ax, ay, aw, ah: Integer);
    procedure DrawBitmap(ADestX, ADestY: Integer; ABitmap: TBitmap);
    procedure DrawBitmap(ADestX, ADestY: Integer; ABitmap: TBitmap;
      ASourceW, ASourceH: Integer);

    // Special extensions: Drawing of basic controls
    procedure DrawButton(AControl: TWinControl; const ARect: TRect; IsPressed, IsGrayed: Boolean;
      AAlign: TAlignment; const AText: String); virtual; abstract;
  end;


// -------------------------------------------------------------------
//   TBitmap
// -------------------------------------------------------------------

  TBitmap = class
  protected
    FWidth, FHeight, FBitsPerPixel: Integer;
    FStride: LongWord;
    FData: Pointer;
    FDataChanged: Boolean;
    function GetRGB(x, y: Integer): TColor;
  public
    destructor Destroy; override;
    procedure LoadFromFile(const AFilename: String);
    procedure LoadFromStream(AStream: TStream);

    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property BitsPerPixel: Integer read FBitsPerPixel;
    property Stride: LongWord read FStride;
    property DataChanged: Boolean read FDataChanged write FDataChanged;
    property Data: Pointer read FData;
    property RGB[x, y: Integer]: TColor read GetRGB;
  end;


{
  $Log$
  Revision 1.4  2000/03/19 00:29:31  sg
  * Huge shift towards more VCL compatiblity (NOTE: Many widgets/controls
    are not working correctly at the moment!)

  Revision 1.3  2000/02/19 19:09:00  sg
  * Fixed layouting bugs; minor improvements

  Revision 1.2  2000/01/24 00:09:41  sg
  * Added TBitmap and bitmap drawing support to TCanvas

  Revision 1.1.1.1  1999/12/30 21:33:17  sg
  Initial import

}
