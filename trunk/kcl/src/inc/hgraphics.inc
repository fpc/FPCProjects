{
    $Id$

    KCL  -  Kassandra Component Library
    Copyright (C) 1999  by the KCL team
      see file AUTHORS in base directory of this distribution

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


// ===================================================================
//   Drawing/Painting Stuff
// ===================================================================

type

  PRect = ^TRect;
  TColor = LongWord;

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
  end;


{
  $Log$
  Revision 1.1  1999/12/30 21:33:17  sg
  Initial revision

}
