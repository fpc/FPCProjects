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
  TColor = LongWord;
  TBitmap = class;

const
  colBlack       = $000000;
  colDarkBlue    = $000080;
  colBlue        = $0000ff;
  colDarkGreen   = $008000;
  colGreen       = $00ff00;
  colDarkCyan    = $008080;
  colCyan        = $00ffff;
  colBrown       = $800000;
  colRed         = $ff0000;
  colDarkMagenta = $800080;
  colMagenta     = $ff00ff;
  colDarkYellow  = $808000;
  colYellow      = $ffff00;
  colGray        = $808080;
  colGrey        = colGray;
  colLightGray   = $c0c0c0;
  colLightGrey   = colLightGray;
  colWhite       = $ffffff;

  colDefault     = $ffffffff;

type

// -------------------------------------------------------------------
//   TFont
// -------------------------------------------------------------------

  TFont = class(TPersistent)
  protected
    FHandle: TFont_private;
    Modified: Boolean;		// Set if font needs to be regenerated
    FFontName: String;
    FWidth, FHeight: Single;
    FBold, FItalics: Boolean;
    procedure FSetFontName(const AFontName: String);
    procedure FSetWidth(AWidth: Single);
    procedure FSetHeight(AHeight: Single);
    procedure FSetBold(AIsBold: Boolean);
    procedure FSetItalics(AIsItalics: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Allocate;
    function  GetTextWidth(const s: String): Integer;
    function  GetTextHeight(const s: String): Integer;
    property  Handle: TFont_private read FHandle;
  published
    property FontName: String read FFontName write FSetFontName;
    property Width: Single read FWidth write FSetWidth;
    property Height: Single read FHeight write FSetHeight;
    property Bold: Boolean read FBold write FSetBold default False;
    property Italics: Boolean read FItalics write FSetItalics default False;
  end;


// -------------------------------------------------------------------
//   TCanvas
// -------------------------------------------------------------------

  TCanvas = class
  protected
    FHandle: TCanvas_Handle;
    priv: TCanvas_private;
    FColor: TColor;
    FFont: TFont;
    procedure SetColor(AColor: TColor);
    procedure SetFont(AFont: TFont);
    constructor Create;
  public
    property Handle: TCanvas_Handle read FHandle;
    property Color: TColor read FColor write SetColor;
    property Font: TFont read FFont write SetFont;

    procedure Line(x1, y1, x2, y2: Integer);
    procedure FillRect(const rect: TRect);
    procedure FillRect(ax, ay, aw, ah: Integer);
    procedure Text(x, y: Integer; const AText: String);
    procedure CopyRect(dx, dy, dw, dh, sx, sy: Integer);
    procedure DrawBitmap(ADestX, ADestY: Integer; ABitmap: TBitmap);
    procedure DrawBitmap(ADestX, ADestY: Integer; ABitmap: TBitmap;
      ASourceW, ASourceH: Integer);
    procedure DrawBitmap(ADestX, ADestY: Integer;
      ABitmap: TBitmap; ASourceX, ASourceY, ASourceW, ASourceH: Integer);
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
  Revision 1.3  2000/02/19 19:09:00  sg
  * Fixed layouting bugs; minor improvements

  Revision 1.2  2000/01/24 00:09:41  sg
  * Added TBitmap and bitmap drawing support to TCanvas

  Revision 1.1.1.1  1999/12/30 21:33:17  sg
  Initial import

}
