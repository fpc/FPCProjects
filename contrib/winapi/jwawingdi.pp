{******************************************************************************}
{                                                       	               }
{ Graphics Device Interface API interface Unit for Object Pascal               }
{                                                       	               }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{ 								               }
{ The original file is: wingdi.h, released June 2000. The original Pascal      }
{ code is: WinGDI.pas, released December 2000. The initial developer of the    }
{ Pascal code is Marcel van Brakel (brakelm@chello.nl).                        }
{                                                                              }
{ Portions created by Marcel van Brakel are Copyright (C) 1999-2001            }
{ Marcel van Brakel. All Rights Reserved.                                      }
{ 								               }
{ Obtained through: Joint Endeavour of Delphi Innovators (Project JEDI)        }
{								               }
{ You may retrieve the latest version of this file at the Project JEDI home    }
{ page, located at http://delphi-jedi.org or my personal homepage located at   }
{ http://members.chello.nl/m.vanbrakel2                                        }
{								               }
{ The contents of this file are used with permission, subject to the Mozilla   }
{ Public License Version 1.1 (the "License"); you may not use this file except }
{ in compliance with the License. You may obtain a copy of the License at      }
{ http://www.mozilla.org/MPL/MPL-1.1.html                                      }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ Alternatively, the contents of this file may be used under the terms of the  }
{ GNU Lesser General Public License (the  "LGPL License"), in which case the   }
{ provisions of the LGPL License are applicable instead of those above.        }
{ If you wish to allow use of your version of this file only under the terms   }
{ of the LGPL License and not to allow others to use your version of this file }
{ under the MPL, indicate your decision by deleting  the provisions above and  }
{ replace  them with the notice and other provisions required by the LGPL      }
{ License.  If you do not delete the provisions above, a recipient may use     }
{ your version of this file under either the MPL or the LGPL License.          }
{ 								               }
{ For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html }
{ 								               }
{******************************************************************************}

unit JwaWinGDI;

{$WEAKPACKAGEUNIT}

{$HPPEMIT ''}
{$HPPEMIT '#include "WinGDI.h"'}
{$HPPEMIT ''}

{$I WINDEFINES.INC}

interface

uses
  JwaWinNT, JwaWinType;

// Binary raster ops

const
  R2_BLACK       = 1; // 0
  R2_NOTMERGEPEN = 2; // DPon
  R2_MASKNOTPEN  = 3; // DPna
  R2_NOTCOPYPEN  = 4; // PN
  R2_MASKPENNOT  = 5; // PDna
  R2_NOT         = 6; // Dn
  R2_XORPEN      = 7; // DPx
  R2_NOTMASKPEN  = 8; // DPan
  R2_MASKPEN     = 9; // DPa
  R2_NOTXORPEN   = 10; // DPxn
  R2_NOP         = 11; // D
  R2_MERGENOTPEN = 12; // DPno
  R2_COPYPEN     = 13; // P
  R2_MERGEPENNOT = 14; // PDno
  R2_MERGEPEN    = 15; // DPo
  R2_WHITE       = 16; // 1
  R2_LAST        = 16;

// Ternary raster operations

  SRCCOPY     = DWORD($00CC0020); // dest = source
  SRCPAINT    = DWORD($00EE0086); // dest = source OR dest
  SRCAND      = DWORD($008800C6); // dest = source AND dest
  SRCINVERT   = DWORD($00660046); // dest = source XOR dest
  SRCERASE    = DWORD($00440328); // dest = source AND (NOT dest )
  NOTSRCCOPY  = DWORD($00330008); // dest = (NOT source)
  NOTSRCERASE = DWORD($001100A6); // dest = (NOT src) AND (NOT dest)
  MERGECOPY   = DWORD($00C000CA); // dest = (source AND pattern)
  MERGEPAINT  = DWORD($00BB0226); // dest = (NOT source) OR dest
  PATCOPY     = DWORD($00F00021); // dest = pattern
  PATPAINT    = DWORD($00FB0A09); // dest = DPSnoo
  PATINVERT   = DWORD($005A0049); // dest = pattern XOR dest
  DSTINVERT   = DWORD($00550009); // dest = (NOT dest)
  BLACKNESS   = DWORD($00000042); // dest = BLACK
  WHITENESS   = DWORD($00FF0062); // dest = WHITE

  NOMIRRORBITMAP = DWORD($80000000); // Do not Mirror the bitmap in this call
  CAPTUREBLT     = DWORD($40000000); // Include layered windows

// Quaternary raster codes

function MAKEROP4(Fore, Back: DWORD): DWORD;

const
  GDI_ERROR = DWORD($FFFFFFFF);
  HGDI_ERROR = HANDLE($FFFFFFFF);

// Region Flags

  ERROR         = 0;
  NULLREGION    = 1;
  SIMPLEREGION  = 2;
  COMPLEXREGION = 3;
  RGN_ERROR     = ERROR;

// CombineRgn() Styles

  RGN_AND  = 1;
  RGN_OR   = 2;
  RGN_XOR  = 3;
  RGN_DIFF = 4;
  RGN_COPY = 5;
  RGN_MIN  = RGN_AND;
  RGN_MAX  = RGN_COPY;

// StretchBlt() Modes

  BLACKONWHITE      = 1;
  WHITEONBLACK      = 2;
  COLORONCOLOR      = 3;
  HALFTONE          = 4;
  MAXSTRETCHBLTMODE = 4;

// New StretchBlt() Modes

  STRETCH_ANDSCANS    = BLACKONWHITE;
  STRETCH_ORSCANS     = WHITEONBLACK;
  STRETCH_DELETESCANS = COLORONCOLOR;
  STRETCH_HALFTONE    = HALFTONE;

// PolyFill() Modes

  ALTERNATE     = 1;
  WINDING       = 2;
  POLYFILL_LAST = 2;

// Layout Orientation Options

  LAYOUT_RTL                        = $00000001; // Right to left
  LAYOUT_BTT                        = $00000002; // Bottom to top
  LAYOUT_VBH                        = $00000004; // Vertical before horizontal
  LAYOUT_ORIENTATIONMASK            = (LAYOUT_RTL or LAYOUT_BTT or LAYOUT_VBH);
  LAYOUT_BITMAPORIENTATIONPRESERVED = $00000008;

// Text Alignment Options

  TA_NOUPDATECP = 0;
  TA_UPDATECP   = 1;

  TA_LEFT   = 0;
  TA_RIGHT  = 2;
  TA_CENTER = 6;

  TA_TOP      = 0;
  TA_BOTTOM   = 8;
  TA_BASELINE = 24;

  TA_RTLREADING = 256;

{$IFDEF WINVER_0400_GREATER}
  TA_MASK       = (TA_BASELINE + TA_CENTER + TA_UPDATECP + TA_RTLREADING);
{$ELSE}
  TA_MASK       = (TA_BASELINE + TA_CENTER + TA_UPDATECP);
{$ENDIF}

  VTA_BASELINE = TA_BASELINE;
  VTA_LEFT     = TA_BOTTOM;
  VTA_RIGHT    = TA_TOP;
  VTA_CENTER   = TA_CENTER;
  VTA_BOTTOM   = TA_RIGHT;
  VTA_TOP      = TA_LEFT;

  ETO_OPAQUE  = $0002;
  ETO_CLIPPED = $0004;

  ETO_GLYPH_INDEX    = $0010;
  ETO_RTLREADING     = $0080;
  ETO_NUMERICSLOCAL  = $0400;
  ETO_NUMERICSLATIN  = $0800;
  ETO_IGNORELANGUAGE = $1000;

  ETO_PDY = $2000;

  ASPECT_FILTERING = $0001;

// Bounds Accumulation APIs

  DCB_RESET      = $0001;
  DCB_ACCUMULATE = $0002;
  DCB_DIRTY      = DCB_ACCUMULATE;
  DCB_SET        = (DCB_RESET or DCB_ACCUMULATE);
  DCB_ENABLE     = $0004;
  DCB_DISABLE    = $0008;

// Metafile Functions

  META_SETBKCOLOR            = $0201;
  META_SETBKMODE             = $0102;
  META_SETMAPMODE            = $0103;
  META_SETROP2               = $0104;
  META_SETRELABS             = $0105;
  META_SETPOLYFILLMODE       = $0106;
  META_SETSTRETCHBLTMODE     = $0107;
  META_SETTEXTCHAREXTRA      = $0108;
  META_SETTEXTCOLOR          = $0209;
  META_SETTEXTJUSTIFICATION  = $020A;
  META_SETWINDOWORG          = $020B;
  META_SETWINDOWEXT          = $020C;
  META_SETVIEWPORTORG        = $020D;
  META_SETVIEWPORTEXT        = $020E;
  META_OFFSETWINDOWORG       = $020F;
  META_SCALEWINDOWEXT        = $0410;
  META_OFFSETVIEWPORTORG     = $0211;
  META_SCALEVIEWPORTEXT      = $0412;
  META_LINETO                = $0213;
  META_MOVETO                = $0214;
  META_EXCLUDECLIPRECT       = $0415;
  META_INTERSECTCLIPRECT     = $0416;
  META_ARC                   = $0817;
  META_ELLIPSE               = $0418;
  META_FLOODFILL             = $0419;
  META_PIE                   = $081A;
  META_RECTANGLE             = $041B;
  META_ROUNDRECT             = $061C;
  META_PATBLT                = $061D;
  META_SAVEDC                = $001E;
  META_SETPIXEL              = $041F;
  META_OFFSETCLIPRGN         = $0220;
  META_TEXTOUT               = $0521;
  META_BITBLT                = $0922;
  META_STRETCHBLT            = $0B23;
  META_POLYGON               = $0324;
  META_POLYLINE              = $0325;
  META_ESCAPE                = $0626;
  META_RESTOREDC             = $0127;
  META_FILLREGION            = $0228;
  META_FRAMEREGION           = $0429;
  META_INVERTREGION          = $012A;
  META_PAINTREGION           = $012B;
  META_SELECTCLIPREGION      = $012C;
  META_SELECTOBJECT          = $012D;
  META_SETTEXTALIGN          = $012E;
  META_CHORD                 = $0830;
  META_SETMAPPERFLAGS        = $0231;
  META_EXTTEXTOUT            = $0a32;
  META_SETDIBTODEV           = $0d33;
  META_SELECTPALETTE         = $0234;
  META_REALIZEPALETTE        = $0035;
  META_ANIMATEPALETTE        = $0436;
  META_SETPALENTRIES         = $0037;
  META_POLYPOLYGON           = $0538;
  META_RESIZEPALETTE         = $0139;
  META_DIBBITBLT             = $0940;
  META_DIBSTRETCHBLT         = $0b41;
  META_DIBCREATEPATTERNBRUSH = $0142;
  META_STRETCHDIB            = $0f43;
  META_EXTFLOODFILL          = $0548;
  META_SETLAYOUT             = $0149;
  META_DELETEOBJECT          = $01f0;
  META_CREATEPALETTE         = $00f7;
  META_CREATEPATTERNBRUSH    = $01F9;
  META_CREATEPENINDIRECT     = $02FA;
  META_CREATEFONTINDIRECT    = $02FB;
  META_CREATEBRUSHINDIRECT   = $02FC;
  META_CREATEREGION          = $06FF;

type
  PDrawPatRect = ^TDrawPatRect;
  _DRAWPATRECT = record
    ptPosition: POINT;
    ptSize: POINT;
    wStyle: WORD;
    wPattern: WORD;
  end;
  DRAWPATRECT = _DRAWPATRECT;
  TDrawPatRect = _DRAWPATRECT;

// GDI Escapes

const
  NEWFRAME           = 1;
  _ABORTDOC          = 2; // Underscore prfix by translator (nameclash)
  NEXTBAND           = 3;
  SETCOLORTABLE      = 4;
  GETCOLORTABLE      = 5;
  FLUSHOUTPUT        = 6;
  DRAFTMODE          = 7;
  QUERYESCSUPPORT    = 8;
  SETABORTPROC_      = 9;  // Underscore prfix by translator (nameclash)
  STARTDOC_          = 10; // Underscore prfix by translator (nameclash)
  ENDDOC_            = 11; // Underscore prfix by translator (nameclash)
  GETPHYSPAGESIZE    = 12;
  GETPRINTINGOFFSET  = 13;
  GETSCALINGFACTOR   = 14;
  MFCOMMENT          = 15;
  GETPENWIDTH        = 16;
  SETCOPYCOUNT       = 17;
  SELECTPAPERSOURCE  = 18;
  DEVICEDATA         = 19;
  PASSTHROUGH        = 19;
  GETTECHNOLGY       = 20;
  GETTECHNOLOGY      = 20;
  SETLINECAP         = 21;
  SETLINEJOIN        = 22;
  SETMITERLIMIT_     = 23; // underscore prefix by translator (nameclash)
  BANDINFO           = 24;
  DRAWPATTERNRECT    = 25;
  GETVECTORPENSIZE   = 26;
  GETVECTORBRUSHSIZE = 27;
  ENABLEDUPLEX       = 28;
  GETSETPAPERBINS    = 29;
  GETSETPRINTORIENT  = 30;
  ENUMPAPERBINS      = 31;
  SETDIBSCALING      = 32;
  EPSPRINTING        = 33;
  ENUMPAPERMETRICS   = 34;
  GETSETPAPERMETRICS = 35;
  POSTSCRIPT_DATA    = 37;
  POSTSCRIPT_IGNORE  = 38;
  MOUSETRAILS        = 39;
  GETDEVICEUNITS     = 42;

  GETEXTENDEDTEXTMETRICS = 256;
  GETEXTENTTABLE         = 257;
  GETPAIRKERNTABLE       = 258;
  GETTRACKKERNTABLE      = 259;
  EXTTEXTOUT_            = 512; // underscore prefix by translator (nameclash)
  GETFACENAME            = 513;
  DOWNLOADFACE           = 514;
  ENABLERELATIVEWIDTHS   = 768;
  ENABLEPAIRKERNING      = 769;
  SETKERNTRACK           = 770;
  SETALLJUSTVALUES       = 771;
  SETCHARSET             = 772;

  STRETCHBLT_ESCAPE       = 2048; // suffix _ESCAPE by translator because of 
                                  // name-clash with StretchBlt function
  METAFILE_DRIVER         = 2049;
  GETSETSCREENPARAMS      = 3072;
  QUERYDIBSUPPORT         = 3073;
  BEGIN_PATH              = 4096;
  CLIP_TO_PATH            = 4097;
  END_PATH                = 4098;
  EXT_DEVICE_CAPS         = 4099;
  RESTORE_CTM             = 4100;
  SAVE_CTM                = 4101;
  SET_ARC_DIRECTION       = 4102;
  SET_BACKGROUND_COLOR    = 4103;
  SET_POLY_MODE           = 4104;
  SET_SCREEN_ANGLE        = 4105;
  SET_SPREAD              = 4106;
  TRANSFORM_CTM           = 4107;
  SET_CLIP_BOX            = 4108;
  SET_BOUNDS              = 4109;
  SET_MIRROR_MODE         = 4110;
  OPENCHANNEL             = 4110;
  DOWNLOADHEADER          = 4111;
  CLOSECHANNEL            = 4112;
  POSTSCRIPT_PASSTHROUGH  = 4115;
  ENCAPSULATED_POSTSCRIPT = 4116;

  POSTSCRIPT_IDENTIFY  = 4117; // new escape for NT5 pscript driver
  POSTSCRIPT_INJECTION = 4118; // new escape for NT5 pscript driver

  CHECKJPEGFORMAT = 4119;
  CHECKPNGFORMAT  = 4120;

  GET_PS_FEATURESETTING = 4121; // new escape for NT5 pscript driver

  SPCLPASSTHROUGH2 = 4568; // new escape for NT5 pscript driver

//
// Parameters for POSTSCRIPT_IDENTIFY escape
//

  PSIDENT_GDICENTRIC = 0;
  PSIDENT_PSCENTRIC  = 1;

//
// Header structure for the input buffer to POSTSCRIPT_INJECTION escape
//

type
  PPsInjectData = ^TPsInjectData;
  _PSINJECTDATA = record
    DataBytes: DWORD;     // number of raw data bytes (NOT including this header)
    InjectionPoint: WORD; // injection point
    PageNumber: WORD;     // page number to apply the injection
                          // Followed by raw data to be injected
  end;
  PSINJECTDATA = _PSINJECTDATA;
  TPsInjectData = _PSINJECTDATA;

//
// Constants for PSINJECTDATA.InjectionPoint field
//

const
  PSINJECT_BEGINSTREAM = 1;
  PSINJECT_PSADOBE     = 2;
  PSINJECT_PAGESATEND  = 3;
  PSINJECT_PAGES       = 4;

  PSINJECT_DOCNEEDEDRES          = 5;
  PSINJECT_DOCSUPPLIEDRES        = 6;
  PSINJECT_PAGEORDER             = 7;
  PSINJECT_ORIENTATION           = 8;
  PSINJECT_BOUNDINGBOX           = 9;
  PSINJECT_DOCUMENTPROCESSCOLORS = 10;

  PSINJECT_COMMENTS                   = 11;
  PSINJECT_BEGINDEFAULTS              = 12;
  PSINJECT_ENDDEFAULTS                = 13;
  PSINJECT_BEGINPROLOG                = 14;
  PSINJECT_ENDPROLOG                  = 15;
  PSINJECT_BEGINSETUP                 = 16;
  PSINJECT_ENDSETUP                   = 17;
  PSINJECT_TRAILER                    = 18;
  PSINJECT_EOF                        = 19;
  PSINJECT_ENDSTREAM                  = 20;
  PSINJECT_DOCUMENTPROCESSCOLORSATEND = 21;

  PSINJECT_PAGENUMBER     = 100;
  PSINJECT_BEGINPAGESETUP = 101;
  PSINJECT_ENDPAGESETUP   = 102;
  PSINJECT_PAGETRAILER    = 103;
  PSINJECT_PLATECOLOR     = 104;

  PSINJECT_SHOWPAGE        = 105;
  PSINJECT_PAGEBBOX        = 106;
  PSINJECT_ENDPAGECOMMENTS = 107;

  PSINJECT_VMSAVE    = 200;
  PSINJECT_VMRESTORE = 201;

//
// Parameter for GET_PS_FEATURESETTING escape
//

  FEATURESETTING_NUP       = 0;
  FEATURESETTING_OUTPUT    = 1;
  FEATURESETTING_PSLEVEL   = 2;
  FEATURESETTING_CUSTPAPER = 3;
  FEATURESETTING_MIRROR    = 4;
  FEATURESETTING_NEGATIVE  = 5;
  FEATURESETTING_PROTOCOL  = 6;

//
// The range of selectors between FEATURESETTING_PRIVATE_BEGIN and
// FEATURESETTING_PRIVATE_END is reserved by Microsoft for private use
//

  FEATURESETTING_PRIVATE_BEGIN = $1000;
  FEATURESETTING_PRIVATE_END   = $1FFF;

//
// Information about output options
//

type
  PPsFeatureOutput = ^TPsFeatureOutput;
  _PSFEATURE_OUTPUT = record
    bPageIndependent: BOOL;
    bSetPageDevice: BOOL;
  end;
  PSFEATURE_OUTPUT = _PSFEATURE_OUTPUT;
  PPSFEATURE_OUTPUT = ^PSFEATURE_OUTPUT;
  TPsFeatureOutput = _PSFEATURE_OUTPUT;

//
// Information about custom paper size
//

  PPsFeatureCustPaper = ^TPsFeatureCustPaper;
  _PSFEATURE_CUSTPAPER = record
    lOrientation: LONG;
    lWidth: LONG;
    lHeight: LONG;
    lWidthOffset: LONG;
    lHeightOffset: LONG;
  end;
  PSFEATURE_CUSTPAPER = _PSFEATURE_CUSTPAPER;
  PPSFEATURE_CUSTPAPER = ^PSFEATURE_CUSTPAPER;
  TPsFeatureCustPaper = _PSFEATURE_CUSTPAPER;

// Value returned for FEATURESETTING_PROTOCOL

const
  PSPROTOCOL_ASCII  = 0;
  PSPROTOCOL_BCP    = 1;
  PSPROTOCOL_TBCP   = 2;
  PSPROTOCOL_BINARY = 3;

// Flag returned from QUERYDIBSUPPORT

  QDI_SETDIBITS   = 1;
  QDI_GETDIBITS   = 2;
  QDI_DIBTOSCREEN = 4;
  QDI_STRETCHDIB  = 8;

// Spooler Error Codes

  SP_NOTREPORTED = $4000;
  SP_ERROR       = DWORD(-1);
  SP_APPABORT    = DWORD(-2);
  SP_USERABORT   = DWORD(-3);
  SP_OUTOFDISK   = DWORD(-4);
  SP_OUTOFMEMORY = DWORD(-5);

  PR_JOBSTATUS = $0000;

// Object Definitions for EnumObjects()

  OBJ_PEN         = 1;
  OBJ_BRUSH       = 2;
  OBJ_DC          = 3;
  OBJ_METADC      = 4;
  OBJ_PAL         = 5;
  OBJ_FONT        = 6;
  OBJ_BITMAP      = 7;
  OBJ_REGION      = 8;
  OBJ_METAFILE    = 9;
  OBJ_MEMDC       = 10;
  OBJ_EXTPEN      = 11;
  OBJ_ENHMETADC   = 12;
  OBJ_ENHMETAFILE = 13;
  OBJ_COLORSPACE  = 14;

// xform stuff

  MWT_IDENTITY      = 1;
  MWT_LEFTMULTIPLY  = 2;
  MWT_RIGHTMULTIPLY = 3;

  MWT_MIN = MWT_IDENTITY;
  MWT_MAX = MWT_RIGHTMULTIPLY;

type
  PXform = ^TXform;
  tagXFORM = record
    eM11: FLOAT;
    eM12: FLOAT;
    eM21: FLOAT;
    eM22: FLOAT;
    eDx: FLOAT;
    eDy: FLOAT;
  end;
  XFORM = tagXFORM;
  LPXFORM = ^XFORM;
  TXform = XFORM;

// Bitmap Header Definition

  PBitmap = ^TBitmap;
  tagBITMAP = record
    bmType: LONG;
    bmWidth: LONG;
    bmHeight: LONG;
    bmWidthBytes: LONG;
    bmPlanes: WORD;
    bmBitsPixel: WORD;
    bmBits: LPVOID;
  end;
  BITMAP = tagBITMAP;
  LPBITMAP = ^BITMAP;
  NPBITMAP = ^BITMAP;
  TBitmap = BITMAP;

// #include <pshpack1.h>

  PRgbTriple = ^TRgbTriple;
  tagRGBTRIPLE = packed record
    rgbtBlue: BYTE;
    rgbtGreen: BYTE;
    rgbtRed: BYTE;
  end;
  RGBTRIPLE = tagRGBTRIPLE;
  TRgbTriple = RGBTRIPLE;

// #include <poppack.h>

  PRgbQuad = ^TRgbQuad;
  tagRGBQUAD = record
    rgbBlue: BYTE;
    rgbGreen: BYTE;
    rgbRed: BYTE;
    rgbReserved: BYTE;
  end;
  RGBQUAD = tagRGBQUAD;
  LPRGBQUAD = ^RGBQUAD;
  TRgbQuad = RGBQUAD;

// Image Color Matching color definitions

const
  CS_ENABLE           = $00000001;
  CS_DISABLE          = $00000002;
  CS_DELETE_TRANSFORM = $00000003;

// Logcolorspace signature

  LCS_SIGNATURE = 'PSOC';

// Logcolorspace lcsType values

  LCS_sRGB                = 'sRGB';
  LCS_WINDOWS_COLOR_SPACE = 'Win '; // Windows default color space

type
  LCSCSTYPE = LONG;

const
  LCS_CALIBRATED_RGB = $00000000;

type
  LCSGAMUTMATCH = LONG;

const
  LCS_GM_BUSINESS         = $00000001;
  LCS_GM_GRAPHICS         = $00000002;
  LCS_GM_IMAGES           = $00000004;
  LCS_GM_ABS_COLORIMETRIC = $00000008;

// ICM Defines for results from CheckColorInGamut()

  CM_OUT_OF_GAMUT = 255;
  CM_IN_GAMUT     = 0;

// UpdateICMRegKey Constants

  ICM_ADDPROFILE          = 1;
  ICM_DELETEPROFILE       = 2;
  ICM_QUERYPROFILE        = 3;
  ICM_SETDEFAULTPROFILE   = 4;
  ICM_REGISTERICMATCHER   = 5;
  ICM_UNREGISTERICMATCHER = 6;
  ICM_QUERYMATCH          = 7;

// Macros to retrieve CMYK values from a COLORREF

function GetKValue(cmyk: COLORREF): BYTE;
function GetYValue(cmyk: COLORREF): BYTE;
function GetMValue(cmyk: COLORREF): BYTE;
function GetCValue(cmyk: COLORREF): BYTE;
function CMYK(c, m, y, k: BYTE): COLORREF;

type
  FXPT16DOT16 = Longint;
  LPFXPT16DOT16 = ^FXPT16DOT16;

  FXPT2DOT30 = Longint;
  LPFXPT2DOT30 = ^FXPT2DOT30;

// ICM Color Definitions
// The following two structures are used for defining RGB's in terms of CIEXYZ.

  PCieXyz = ^TCieXyz;
  tagCIEXYZ = record
    ciexyzX: FXPT2DOT30;
    ciexyzY: FXPT2DOT30;
    ciexyzZ: FXPT2DOT30;
  end;
  CIEXYZ = tagCIEXYZ;
  LPCIEXYZ = ^CIEXYZ;
  TCieXyz = CIEXYZ;

  PCieXyzTriple = ^TCieXyzTriple;
  tagCIEXYZTRIPLE = record
    ciexyzRed: CIEXYZ;
    ciexyzGreen: CIEXYZ;
    ciexyzBlue: CIEXYZ;
  end;
  CIEXYZTRIPLE = tagCIEXYZTRIPLE;
  LPCIEXYZTRIPLE = ^CIEXYZTRIPLE;
  TCieXyzTriple = CIEXYZTRIPLE;

// The next structures the logical color space. Unlike pens and brushes,
// but like palettes, there is only one way to create a LogColorSpace.
// A pointer to it must be passed, its elements can't be pushed as
// arguments.

  PLogColorSpaceA = ^TLogColorSpaceA;
  tagLOGCOLORSPACEA = record
    lcsSignature: DWORD;
    lcsVersion: DWORD;
    lcsSize: DWORD;
    lcsCSType: LCSCSTYPE;
    lcsIntent: LCSGAMUTMATCH;
    lcsEndpoints: CIEXYZTRIPLE;
    lcsGammaRed: DWORD;
    lcsGammaGreen: DWORD;
    lcsGammaBlue: DWORD;
    lcsFilename: array [0..MAX_PATH - 1] of CHAR;
  end;
  LOGCOLORSPACEA = tagLOGCOLORSPACEA;
  LPLOGCOLORSPACEA = ^LOGCOLORSPACEA;
  TLogColorSpaceA = LOGCOLORSPACEA;

  PLogColorSpaceW = ^TLogColorSpaceW;
  tagLOGCOLORSPACEW = record
    lcsSignature: DWORD;
    lcsVersion: DWORD;
    lcsSize: DWORD;
    lcsCSType: LCSCSTYPE;
    lcsIntent: LCSGAMUTMATCH;
    lcsEndpoints: CIEXYZTRIPLE;
    lcsGammaRed: DWORD;
    lcsGammaGreen: DWORD;
    lcsGammaBlue: DWORD;
    lcsFilename: array [0..MAX_PATH - 1] of WCHAR;
  end;
  LOGCOLORSPACEW = tagLOGCOLORSPACEW;
  LPLOGCOLORSPACEW = ^LOGCOLORSPACEW;
  TLogColorSpaceW = LOGCOLORSPACEW;

{$IFDEF UNICODE}
  LOGCOLORSPACE = LOGCOLORSPACEW;
  LPLOGCOLORSPACE = LPLOGCOLORSPACEW;
  TLogColorSpace = TLogColorSpaceW;
  PLogColorSpace = PLogColorSpaceW;
{$ELSE}
  LOGCOLORSPACE = LOGCOLORSPACEA;
  LPLOGCOLORSPACE = LPLOGCOLORSPACEA;
  TLogColorSpace = TLogColorSpaceA;
  PLogColorSpace = PLogColorSpaceA;
{$ENDIF}

// structures for defining DIBs

  PBitmapCoreHeader = ^TBitmapCoreHeader;
  tagBITMAPCOREHEADER = record
    bcSize: DWORD;
    bcWidth: WORD;
    bcHeight: WORD;
    bcPlanes: WORD;
    bcBitCount: WORD;
  end;
  BITMAPCOREHEADER = tagBITMAPCOREHEADER;
  LPBITMAPCOREHEADER = ^BITMAPCOREHEADER;
  TBitmapCoreHeader = BITMAPCOREHEADER;

  PBitmapInfoHeader = ^TBitmapInfoHeader;
  tagBITMAPINFOHEADER = record
    biSize: DWORD;
    biWidth: LONG;
    biHeight: LONG;
    biPlanes: WORD;
    biBitCount: WORD;
    biCompression: DWORD;
    biSizeImage: DWORD;
    biXPelsPerMeter: LONG;
    biYPelsPerMeter: LONG;
    biClrUsed: DWORD;
    biClrImportant: DWORD;
  end;
  BITMAPINFOHEADER = tagBITMAPINFOHEADER;
  LPBITMAPINFOHEADER = ^BITMAPINFOHEADER;
  TBitmapInfoHeader = BITMAPINFOHEADER;

  PBitmapV4Header = ^TBitmapV4Header;
  BITMAPV4HEADER = record
    bV4Size: DWORD;
    bV4Width: LONG;
    bV4Height: LONG;
    bV4Planes: WORD;
    bV4BitCount: WORD;
    bV4V4Compression: DWORD;
    bV4SizeImage: DWORD;
    bV4XPelsPerMeter: LONG;
    bV4YPelsPerMeter: LONG;
    bV4ClrUsed: DWORD;
    bV4ClrImportant: DWORD;
    bV4RedMask: DWORD;
    bV4GreenMask: DWORD;
    bV4BlueMask: DWORD;
    bV4AlphaMask: DWORD;
    bV4CSType: DWORD;
    bV4Endpoints: CIEXYZTRIPLE;
    bV4GammaRed: DWORD;
    bV4GammaGreen: DWORD;
    bV4GammaBlue: DWORD;
  end;
  LPBITMAPV4HEADER = ^BITMAPV4HEADER;
  TBitmapV4Header = BITMAPV4HEADER;

  PBitmapV5Header = ^TBitmapV5Header;
  BITMAPV5HEADER = record
    bV5Size: DWORD;
    bV5Width: LONG;
    bV5Height: LONG;
    bV5Planes: WORD;
    bV5BitCount: WORD;
    bV5Compression: DWORD;
    bV5SizeImage: DWORD;
    bV5XPelsPerMeter: LONG;
    bV5YPelsPerMeter: LONG;
    bV5ClrUsed: DWORD;
    bV5ClrImportant: DWORD;
    bV5RedMask: DWORD;
    bV5GreenMask: DWORD;
    bV5BlueMask: DWORD;
    bV5AlphaMask: DWORD;
    bV5CSType: DWORD;
    bV5Endpoints: CIEXYZTRIPLE;
    bV5GammaRed: DWORD;
    bV5GammaGreen: DWORD;
    bV5GammaBlue: DWORD;
    bV5Intent: DWORD;
    bV5ProfileData: DWORD;
    bV5ProfileSize: DWORD;
    bV5Reserved: DWORD;
  end;
  LPBITMAPV5HEADER = ^BITMAPV5HEADER;
  TBitmapV5Header = BITMAPV5HEADER;

// Values for bV5CSType

const
  PROFILE_LINKED   = 'LINK';
  PROFILE_EMBEDDED = 'MBED';

// constants for the biCompression field

  BI_RGB       = 0;
  BI_RLE8      = 1;
  BI_RLE4      = 2;
  BI_BITFIELDS = 3;
  BI_JPEG      = 4;
  BI_PNG       = 5;

type
  PBitmapInfo = ^TBitmapInfo;
  tagBITMAPINFO = record
    bmiHeader: BITMAPINFOHEADER;
    bmiColors: array [0..0] of RGBQUAD;
  end;
  BITMAPINFO = tagBITMAPINFO;
  LPBITMAPINFO = ^BITMAPINFO;
  TBitmapInfo = BITMAPINFO;

  PBitmapCoreInfo = ^TBitmapCoreInfo;
  tagBITMAPCOREINFO = record
    bmciHeader: BITMAPCOREHEADER;
    bmciColors: array [0..0] of RGBTRIPLE;
  end;
  BITMAPCOREINFO = tagBITMAPCOREINFO;
  LPBITMAPCOREINFO = ^BITMAPCOREINFO;
  TBitmapCoreInfo = BITMAPCOREINFO;

// #include <pshpack2.h>

  PBitmapFileHeader = ^TBitmapFileHeader;
  tagBITMAPFILEHEADER = packed record
    bfType: WORD;
    bfSize: DWORD;
    bfReserved1: WORD;
    bfReserved2: WORD;
    bfOffBits: DWORD;
  end;
  BITMAPFILEHEADER = tagBITMAPFILEHEADER;
  LPBITMAPFILEHEADER = ^BITMAPFILEHEADER;
  TBitmapFileHeader = BITMAPFILEHEADER;

// #include <poppack.h>

function MAKEPOINTS(l: DWORD): POINTS;

type
  PFontSignature = ^TFontSignature;
  tagFONTSIGNATURE = record
    fsUsb: array [0..3] of DWORD;
    fsCsb: array [0..1] of DWORD;
  end;
  FONTSIGNATURE = tagFONTSIGNATURE;
  LPFONTSIGNATURE = ^FONTSIGNATURE;
  TFontSignature = FONTSIGNATURE;

  PCharSetInfo = ^TCharSetInfo;
  tagCHARSETINFO = record
    ciCharset: UINT;
    ciACP: UINT;
    fs: FONTSIGNATURE;
  end;
  CHARSETINFO = tagCHARSETINFO;
  LPCHARSETINFO = ^CHARSETINFO;
  NPCHARSETINFO = ^CHARSETINFO;
  TCharSetInfo = CHARSETINFO;

const
  TCI_SRCCHARSET  = 1;
  TCI_SRCCODEPAGE = 2;
  TCI_SRCFONTSIG  = 3;
  TCI_SRCLOCALE   = $1000;

type
  PLocaleSignature = ^TLocaleSignature;
  tagLOCALESIGNATURE = record
    lsUsb: array [0..3] of DWORD;
    lsCsbDefault: array [0..1] of DWORD;
    lsCsbSupported: array [0..1] of DWORD;
  end;
  LOCALESIGNATURE = tagLOCALESIGNATURE;
  LPLOCALESIGNATURE = ^LOCALESIGNATURE;
  TLocaleSignature = LOCALESIGNATURE;

// Clipboard Metafile Picture Structure

  PHandleTable = ^THandleTable;
  tagHANDLETABLE = record
    objectHandle: array [0..0] of HGDIOBJ;
  end;
  HANDLETABLE = tagHANDLETABLE;
  LPHANDLETABLE = ^HANDLETABLE;
  THandleTable = HANDLETABLE;

  PMetaRecord = ^TMetaRecord;
  tagMETARECORD = record
    rdSize: DWORD;
    rdFunction: WORD;
    rdParm: array [0..0] of WORD;
  end;
  METARECORD = tagMETARECORD;
  LPMETARECORD = ^METARECORD;
  TMetaRecord = METARECORD;

  PMetaFilePict = ^TMetaFilePict;
  tagMETAFILEPICT = record
    mm: LONG;
    xExt: LONG;
    yExt: LONG;
    hMF: HMETAFILE;
  end;
  METAFILEPICT = tagMETAFILEPICT;
  LPMETAFILEPICT = ^METAFILEPICT;
  TMetaFilePict = METAFILEPICT;

// #include <pshpack2.h>

  PMetaHeader = ^TMetaHeader;
  tagMETAHEADER = packed record
    mtType: WORD;
    mtHeaderSize: WORD;
    mtVersion: WORD;
    mtSize: DWORD;
    mtNoObjects: WORD;
    mtMaxRecord: DWORD;
    mtNoParameters: WORD;
  end;
  METAHEADER = tagMETAHEADER;
  LPMETAHEADER = ^METAHEADER;
  TMetaHeader = METAHEADER;

// #include <poppack.h>

// Enhanced Metafile structures

  PEnhMetaRecord = ^TEnhMetaRecord;
  tagENHMETARECORD = record
    iType: DWORD; // Record type EMR_XXX
    nSize: DWORD; // Record size in bytes
    dParm: array [0..0] of DWORD; // Parameters
  end;
  ENHMETARECORD = tagENHMETARECORD;
  LPENHMETARECORD = ^ENHMETARECORD;
  TEnhMetaRecord = ENHMETARECORD;

  PEnhMetaHeader = ^TEnhMetaHeader;
  tagENHMETAHEADER = record
    iType: DWORD;              // Record type EMR_HEADER
    nSize: DWORD;              // Record size in bytes.  This may be greater
                               // than the sizeof(ENHMETAHEADER).
    rclBounds: RECTL;          // Inclusive-inclusive bounds in device units
    rclFrame: RECTL;           // Inclusive-inclusive Picture Frame of metafile in .01 mm units
    dSignature: DWORD;         // Signature.  Must be ENHMETA_SIGNATURE.
    nVersion: DWORD;           // Version number
    nBytes: DWORD;             // Size of the metafile in bytes
    nRecords: DWORD;           // Number of records in the metafile
    nHandles: WORD;            // Number of handles in the handle table
                               // Handle index zero is reserved.
    sReserved: WORD;           // Reserved.  Must be zero.
    nDescription: DWORD;       // Number of chars in the unicode description string
                               // This is 0 if there is no description string
    offDescription: DWORD;     // Offset to the metafile description record.
                               // This is 0 if there is no description string
    nPalEntries: DWORD;        // Number of entries in the metafile palette.
    szlDevice: SIZEL;          // Size of the reference device in pels
    szlMillimeters: SIZEL;     // Size of the reference device in millimeters
    {$IFDEF WINVER_0400_GREATER}
    cbPixelFormat: DWORD;      // Size of PIXELFORMATDESCRIPTOR information
                               // This is 0 if no pixel format is set
    offPixelFormat: DWORD;     // Offset to PIXELFORMATDESCRIPTOR
                               // This is 0 if no pixel format is set
    bOpenGL: DWORD;            // TRUE if OpenGL commands are present in
                               // the metafile, otherwise FALSE
    {$ENDIF}
    {$IFDEF WINVER_0500_GREATER}
    szlMicrometers: SIZEL;     // Size of the reference device in micrometers
    {$ENDIF}
  end;
  ENHMETAHEADER = tagENHMETAHEADER;
  LPENHMETAHEADER = ^ENHMETAHEADER;
  TEnhMetaHeader = tagENHMETAHEADER;

// tmPitchAndFamily flags

const
  TMPF_FIXED_PITCH = $01;
  TMPF_VECTOR      = $02;
  TMPF_DEVICE      = $08;
  TMPF_TRUETYPE    = $04;

//
// BCHAR definition for APPs
//

type
{$IFDEF UNICODE}
  BCHAR = WCHAR;
{$ELSE}
  BCHAR = BYTE;
{$ENDIF}

type
  PTextMetricA = ^TTextMetricA;
  tagTEXTMETRICA = record
    tmHeight: LONG;
    tmAscent: LONG;
    tmDescent: LONG;
    tmInternalLeading: LONG;
    tmExternalLeading: LONG;
    tmAveCharWidth: LONG;
    tmMaxCharWidth: LONG;
    tmWeight: LONG;
    tmOverhang: LONG;
    tmDigitizedAspectX: LONG;
    tmDigitizedAspectY: LONG;
    tmFirstChar: BYTE;
    tmLastChar: BYTE;
    tmDefaultChar: BYTE;
    tmBreakChar: BYTE;
    tmItalic: BYTE;
    tmUnderlined: BYTE;
    tmStruckOut: BYTE;
    tmPitchAndFamily: BYTE;
    tmCharSet: BYTE;
  end;
  TEXTMETRICA = tagTEXTMETRICA;
  LPTEXTMETRICA = ^TEXTMETRICA;
  NPTEXTMETRICA = ^TEXTMETRICA;
  TTextMetricA = TEXTMETRICA;

  PTextMetricW = ^TTextMetricW;
  tagTEXTMETRICW = record
    tmHeight: LONG;
    tmAscent: LONG;
    tmDescent: LONG;
    tmInternalLeading: LONG;
    tmExternalLeading: LONG;
    tmAveCharWidth: LONG;
    tmMaxCharWidth: LONG;
    tmWeight: LONG;
    tmOverhang: LONG;
    tmDigitizedAspectX: LONG;
    tmDigitizedAspectY: LONG;
    tmFirstChar: WCHAR;
    tmLastChar: WCHAR;
    tmDefaultChar: WCHAR;
    tmBreakChar: WCHAR;
    tmItalic: BYTE;
    tmUnderlined: BYTE;
    tmStruckOut: BYTE;
    tmPitchAndFamily: BYTE;
    tmCharSet: BYTE;
  end;
  TEXTMETRICW = tagTEXTMETRICW;
  LPTEXTMETRICW = ^TEXTMETRICW;
  NPTEXTMETRICW = ^TEXTMETRICW;
  TTextMetricW = TEXTMETRICW;

{$IFDEF UNICODE}
  TEXTMETRIC = TEXTMETRICW;
  PTEXTMETRIC = PTEXTMETRICW;
  NPTEXTMETRIC = NPTEXTMETRICW;
  LPTEXTMETRIC = LPTEXTMETRICW;
  TTextMetric = TTextMetricW;
{$ELSE}
  TEXTMETRIC = TEXTMETRICA;
  NPTEXTMETRIC = NPTEXTMETRICA;
  LPTEXTMETRIC = LPTEXTMETRICA;
  TTextMetric = TTextMetricA;
{$ENDIF}

// ntmFlags field flags

const
  NTM_REGULAR = $00000040;
  NTM_BOLD    = $00000020;
  NTM_ITALIC  = $00000001;

// new in NT 5.0

  NTM_NONNEGATIVE_AC = $00010000;
  NTM_PS_OPENTYPE    = $00020000;
  NTM_TT_OPENTYPE    = $00040000;
  NTM_MULTIPLEMASTER = $00080000;
  NTM_TYPE1          = $00100000;
  NTM_DSIG           = $00200000;

// #include <pshpack4.h>

type
  PNewTextMetricA = ^TNewTextMetricA;
  tagNEWTEXTMETRICA = record
    tmHeight: LONG;
    tmAscent: LONG;
    tmDescent: LONG;
    tmInternalLeading: LONG;
    tmExternalLeading: LONG;
    tmAveCharWidth: LONG;
    tmMaxCharWidth: LONG;
    tmWeight: LONG;
    tmOverhang: LONG;
    tmDigitizedAspectX: LONG;
    tmDigitizedAspectY: LONG;
    tmFirstChar: BYTE;
    tmLastChar: BYTE;
    tmDefaultChar: BYTE;
    tmBreakChar: BYTE;
    tmItalic: BYTE;
    tmUnderlined: BYTE;
    tmStruckOut: BYTE;
    tmPitchAndFamily: BYTE;
    tmCharSet: BYTE;
    ntmFlags: DWORD;
    ntmSizeEM: UINT;
    ntmCellHeight: UINT;
    ntmAvgWidth: UINT;
  end;
  NEWTEXTMETRICA = tagNEWTEXTMETRICA;
  LPNEWTEXTMETRICA = ^NEWTEXTMETRICA;
  NPNEWTEXTMETRICA = ^NEWTEXTMETRICA;
  TNewTextMetricA = NEWTEXTMETRICA;

  PNewTextMetricW = ^TNewTextMetricW;
  tagNEWTEXTMETRICW = record
    tmHeight: LONG;
    tmAscent: LONG;
    tmDescent: LONG;
    tmInternalLeading: LONG;
    tmExternalLeading: LONG;
    tmAveCharWidth: LONG;
    tmMaxCharWidth: LONG;
    tmWeight: LONG;
    tmOverhang: LONG;
    tmDigitizedAspectX: LONG;
    tmDigitizedAspectY: LONG;
    tmFirstChar: WCHAR;
    tmLastChar: WCHAR;
    tmDefaultChar: WCHAR;
    tmBreakChar: WCHAR;
    tmItalic: BYTE;
    tmUnderlined: BYTE;
    tmStruckOut: BYTE;
    tmPitchAndFamily: BYTE;
    tmCharSet: BYTE;
    ntmFlags: DWORD;
    ntmSizeEM: UINT;
    ntmCellHeight: UINT;
    ntmAvgWidth: UINT;
  end;
  NEWTEXTMETRICW = tagNEWTEXTMETRICW;
  LPNEWTEXTMETRICW = ^NEWTEXTMETRICW;
  NPNEWTEXTMETRICW = ^NEWTEXTMETRICW;
  TNewTextMetricW = NEWTEXTMETRICW;

{$IFDEF UNICODE}
  NEWTEXTMETRIC = NEWTEXTMETRICW;
  PNEWTEXTMETRIC = PNEWTEXTMETRICW;
  NPNEWTEXTMETRIC = NPNEWTEXTMETRICW;
  LPNEWTEXTMETRIC = LPNEWTEXTMETRICW;
  TNewTextMetric = TNewTextMetricW;
{$ELSE}
  NEWTEXTMETRIC = NEWTEXTMETRICW;
  PNEWTEXTMETRIC = PNEWTEXTMETRICW;
  NPNEWTEXTMETRIC = NPNEWTEXTMETRICW;
  LPNEWTEXTMETRIC = LPNEWTEXTMETRICW;
  TNewTextMetric = TNewTextMetricW;
{$ENDIF}

// #include <poppack.h>

  PNewTextMetricExA = ^TNewTextMetricExA;
  tagNEWTEXTMETRICEXA = record
    ntmTm: NEWTEXTMETRICA;
    ntmFontSig: FONTSIGNATURE;
  end;
  NEWTEXTMETRICEXA = tagNEWTEXTMETRICEXA;
  TNewTextMetricExA = NEWTEXTMETRICEXA;

  PNewTextMetricExW = ^TNewTextMetricExW;
  tagNEWTEXTMETRICEXW = record
    ntmTm: NEWTEXTMETRICW;
    ntmFontSig: FONTSIGNATURE;
  end;
  NEWTEXTMETRICEXW = tagNEWTEXTMETRICEXW;
  TNewTextMetricExW = NEWTEXTMETRICEXW;

{$IFDEF UNICODE}
  NEWTEXTMETRICEX = NEWTEXTMETRICEXW;
  TNewTextMetricEx = TNewTextMetricExW;
  PNewTextMetricEx = PNewTextMetricExW;
{$ELSE}
  NEWTEXTMETRICEX = NEWTEXTMETRICEXA;
  TNewTextMetricEx = TNewTextMetricExA;
  PNewTextMetricEx = PNewTextMetricExA;
{$ENDIF}

// GDI Logical Objects:

// Pel Array

  PPelArray = ^TPelArray;
  tagPELARRAY = record
    paXCount: LONG;
    paYCount: LONG;
    paXExt: LONG;
    paYExt: LONG;
    paRGBs: BYTE;
  end;
  PELARRAY = tagPELARRAY;
  LPPELARRAY = ^PELARRAY;
  TPelArray = PELARRAY;

// Logical Brush (or Pattern)

  PLogBrush = ^TLogBrush;
  tagLOGBRUSH = record
    lbStyle: UINT;
    lbColor: COLORREF;
    lbHatch: ULONG_PTR; // Sundown: lbHatch could hold a HANDLE
  end;
  LOGBRUSH = tagLOGBRUSH;
  LPLOGBRUSH = ^LOGBRUSH;
  NPLOGBRUSH = ^LOGBRUSH;
  TLogBrush = LOGBRUSH;

  PLogBrush32 = ^TLogBrush32;
  tagLOGBRUSH32 = record
    lbStyle: UINT;
    lbColor: COLORREF;
    lbHatch: ULONG;
  end;
  LOGBRUSH32 = tagLOGBRUSH32;
  LPLOGBRUSH32 = ^LOGBRUSH32;
  NPLOGBRUSH32 = ^LOGBRUSH32;
  TLogBrush32 = LOGBRUSH32;

  PATTERN = LOGBRUSH;
  PPATTERN = ^PATTERN;
  LPPATTERN = ^PATTERN;
  NPPATTERN = ^PATTERN;

// Logical Pen

  PLogPen = ^TLogPen;
  tagLOGPEN = record
    lopnStyle: UINT;
    lopnWidth: POINT;
    lopnColor: COLORREF;
  end;
  LOGPEN = tagLOGPEN;
  LPLOGPEN = ^LOGPEN;
  NPLOGPEN = ^LOGPEN;
  TLogPen = LOGPEN;

  PExtLogPen = ^TExtLogPen;
  tagEXTLOGPEN = record
    elpPenStyle: DWORD;
    elpWidth: DWORD;
    elpBrushStyle: UINT;
    elpColor: COLORREF;
    elpHatch: ULONG_PTR; // Sundown: elpHatch could take a HANDLE
    elpNumEntries: DWORD;
    elpStyleEntry: array [0..0] of DWORD;
  end;
  EXTLOGPEN = tagEXTLOGPEN;
  LPEXTLOGPEN = ^EXTLOGPEN;
  NPEXTLOGPEN = ^EXTLOGPEN;
  TExtLogPen = EXTLOGPEN;

  PPaletteEntry = ^TPaletteEntry;
  tagPALETTEENTRY = record
    peRed: BYTE;
    peGreen: BYTE;
    peBlue: BYTE;
    peFlags: BYTE;
  end;
  PALETTEENTRY = tagPALETTEENTRY;
  LPPALETTEENTRY = ^PALETTEENTRY;
  TPaletteEntry = PALETTEENTRY;

// Logical Palette

  PLogPalette = ^TLogPalette;
  tagLOGPALETTE = record
    palVersion: WORD;
    palNumEntries: WORD;
    palPalEntry: array [0..0] of PALETTEENTRY;
  end;
  LOGPALETTE = tagLOGPALETTE;
  LPLOGPALETTE = ^LOGPALETTE;
  NPLOGPALETTE = ^LOGPALETTE;
  TLogPalette = LOGPALETTE;

// Logical Font

const
  LF_FACESIZE = 32;

type
  PLogFontA = ^TLogFontA;
  tagLOGFONTA = record
    lfHeight: LONG;
    lfWidth: LONG;
    lfEscapement: LONG;
    lfOrientation: LONG;
    lfWeight: LONG;
    lfItalic: BYTE;
    lfUnderline: BYTE;
    lfStrikeOut: BYTE;
    lfCharSet: BYTE;
    lfOutPrecision: BYTE;
    lfClipPrecision: BYTE;
    lfQuality: BYTE;
    lfPitchAndFamily: BYTE;
    lfFaceName: array [0..LF_FACESIZE - 1] of CHAR;
  end;
  LOGFONTA = tagLOGFONTA;
  LPLOGFONTA = ^LOGFONTA;
  NPLOGFONTA = ^LOGFONTA;
  TLogFontA = LOGFONTA;

  PLogFontW = ^TLogFontW;
  tagLOGFONTW = record
    lfHeight: LONG;
    lfWidth: LONG;
    lfEscapement: LONG;
    lfOrientation: LONG;
    lfWeight: LONG;
    lfItalic: BYTE;
    lfUnderline: BYTE;
    lfStrikeOut: BYTE;
    lfCharSet: BYTE;
    lfOutPrecision: BYTE;
    lfClipPrecision: BYTE;
    lfQuality: BYTE;
    lfPitchAndFamily: BYTE;
    lfFaceName: array [0..LF_FACESIZE - 1] of WCHAR;
  end;
  LOGFONTW = tagLOGFONTW;
  LPLOGFONTW = ^LOGFONTW;
  NPLOGFONTW = ^LOGFONTW;
  TLogFontW = LOGFONTW;

{$IFDEF UNICODE}
  LOGFONT = LOGFONTW;
  PLOGFONT = PLOGFONTW;
  NPLOGFONT = NPLOGFONTW;
  LPLOGFONT = LPLOGFONTW;
  TLogFont = TLogFontW;
{$ELSE}
  LOGFONT = LOGFONTA;
  PLOGFONT = PLOGFONTA;
  NPLOGFONT = NPLOGFONTA;
  LPLOGFONT = LPLOGFONTA;
  TLogFont = TLogFontA;
{$ENDIF}

const
  LF_FULLFACESIZE = 64;

// Structure passed to FONTENUMPROC

type
  PEnumLogFontA = ^TEnumLogFontA;
  tagENUMLOGFONTA = record
    elfLogFont: LOGFONTA;
    elfFullName: array [ 0..LF_FULLFACESIZE - 1] of BYTE;
    elfStyle: array [0..LF_FACESIZE - 1] of BYTE;
  end;
  ENUMLOGFONTA = tagENUMLOGFONTA;
  LPENUMLOGFONTA = ^ENUMLOGFONTA;
  TEnumLogFontA = ENUMLOGFONTA;

// Structure passed to FONTENUMPROC

  PEnumLogFontW = ^TEnumLogFontW;
  tagENUMLOGFONTW = record
    elfLogFont: LOGFONTW;
    elfFullName: array [0..LF_FULLFACESIZE - 1] of WCHAR;
    elfStyle: array [0..LF_FACESIZE - 1] of WCHAR;
  end;
  ENUMLOGFONTW = tagENUMLOGFONTW;
  LPENUMLOGFONTW = ^ENUMLOGFONTW;
  TEnumLogFontW = ENUMLOGFONTW;

{$IFDEF UNICODE}
  ENUMLOGFONT = ENUMLOGFONTW;
  LPENUMLOGFONT = LPENUMLOGFONTW;
  TEnumLogFont = TEnumLogFontW;
  PEnumLogFont = PEnumLogFontW;
{$ELSE}
  ENUMLOGFONT = ENUMLOGFONTA;
  LPENUMLOGFONT = LPENUMLOGFONTA;
  TEnumLogFont = TEnumLogFontA;
  PEnumLogFont = PEnumLogFontA;
{$ENDIF}

  PEnumLogFontExA = ^TEnumLogFontExA;
  tagENUMLOGFONTEXA = record
    elfLogFont: LOGFONTA;
    elfFullName: array [0..LF_FULLFACESIZE - 1] of BYTE;
    elfStyle: array [0..LF_FACESIZE - 1] of BYTE;
    elfScript: array [0..LF_FACESIZE - 1] of BYTE;
  end;
  ENUMLOGFONTEXA = tagENUMLOGFONTEXA;
  LPENUMLOGFONTEXA = ^ENUMLOGFONTEXA;
  TEnumLogFontExA = ENUMLOGFONTEXA;

  PEnumLogFontExW = ^TEnumLogFontExW;
  tagENUMLOGFONTEXW = record
    elfLogFont: LOGFONTW;
    elfFullName: array [0..LF_FULLFACESIZE - 1] of WCHAR;
    elfStyle: array [0..LF_FACESIZE - 1] of WCHAR;
    elfScript: array [0..LF_FACESIZE - 1] of WCHAR;
  end;
  ENUMLOGFONTEXW = tagENUMLOGFONTEXW;
  LPENUMLOGFONTEXW = ^ENUMLOGFONTEXW;
  TEnumLogFontExW = ENUMLOGFONTEXW;

{$IFDEF UNICODE}
  ENUMLOGFONTEX = ENUMLOGFONTEXW;
  LPENUMLOGFONTEX = LPENUMLOGFONTEXW;
  TEnumLogFontEx = TEnumLogFontExW;
  PEnumLogFontEx = PEnumLogFontExW;
{$ELSE}
  ENUMLOGFONTEX = ENUMLOGFONTEXA;
  LPENUMLOGFONTEX = LPENUMLOGFONTEXA;
  TEnumLogFontEx = TEnumLogFontExA;
  PEnumLogFontEx = PEnumLogFontExA;
{$ENDIF}

const
  OUT_DEFAULT_PRECIS        = 0;
  OUT_STRING_PRECIS         = 1;
  OUT_CHARACTER_PRECIS      = 2;
  OUT_STROKE_PRECIS         = 3;
  OUT_TT_PRECIS             = 4;
  OUT_DEVICE_PRECIS         = 5;
  OUT_RASTER_PRECIS         = 6;
  OUT_TT_ONLY_PRECIS        = 7;
  OUT_OUTLINE_PRECIS        = 8;
  OUT_SCREEN_OUTLINE_PRECIS = 9;
  OUT_PS_ONLY_PRECIS        = 10;

  CLIP_DEFAULT_PRECIS   = 0;
  CLIP_CHARACTER_PRECIS = 1;
  CLIP_STROKE_PRECIS    = 2;
  CLIP_MASK             = $f;
  CLIP_LH_ANGLES        = (1 shl 4);
  CLIP_TT_ALWAYS        = (2 shl 4);
  CLIP_DFA_DISABLE      = (4 shl 4);
  CLIP_EMBEDDED         = (8 shl 4);

  DEFAULT_QUALITY        = 0;
  DRAFT_QUALITY          = 1;
  PROOF_QUALITY          = 2;
  NONANTIALIASED_QUALITY = 3;
  ANTIALIASED_QUALITY    = 4;
  CLEARTYPE_QUALITY      = 5;

//#if (_WIN32_WINNT >= 0x0501)
  CLEARTYPE_NATURAL_QUALITY = 6;
//#endif

  DEFAULT_PITCH  = 0;
  FIXED_PITCH    = 1;
  VARIABLE_PITCH = 2;
  MONO_FONT      = 8;

  ANSI_CHARSET        = 0;
  DEFAULT_CHARSET     = 1;
  SYMBOL_CHARSET      = 2;
  SHIFTJIS_CHARSET    = 128;
  HANGEUL_CHARSET     = 129;
  HANGUL_CHARSET      = 129;
  GB2312_CHARSET      = 134;
  CHINESEBIG5_CHARSET = 136;
  OEM_CHARSET         = 255;
  JOHAB_CHARSET       = 130;
  HEBREW_CHARSET      = 177;
  ARABIC_CHARSET      = 178;
  GREEK_CHARSET       = 161;
  TURKISH_CHARSET     = 162;
  VIETNAMESE_CHARSET  = 163;
  THAI_CHARSET        = 222;
  EASTEUROPE_CHARSET  = 238;
  RUSSIAN_CHARSET     = 204;

  MAC_CHARSET    = 77;
  BALTIC_CHARSET = 186;

  FS_LATIN1      = $00000001;
  FS_LATIN2      = $00000002;
  FS_CYRILLIC    = $00000004;
  FS_GREEK       = $00000008;
  FS_TURKISH     = $00000010;
  FS_HEBREW      = $00000020;
  FS_ARABIC      = $00000040;
  FS_BALTIC      = $00000080;
  FS_VIETNAMESE  = $00000100;
  FS_THAI        = $00010000;
  FS_JISJAPAN    = $00020000;
  FS_CHINESESIMP = $00040000;
  FS_WANSUNG     = $00080000;
  FS_CHINESETRAD = $00100000;
  FS_JOHAB       = $00200000;
  FS_SYMBOL      = $80000000;

// Font Families

  FF_DONTCARE   = (0 shl 4); // Don't care or don't know.
  FF_ROMAN      = (1 shl 4); // Variable stroke width, serifed.
                             // Times Roman, Century Schoolbook, etc.
  FF_SWISS      = (2 shl 4); // Variable stroke width, sans-serifed.
                             // Helvetica, Swiss, etc.
  FF_MODERN     = (3 shl 4); // Constant stroke width, serifed or sans-serifed.
                             // Pica, Elite, Courier, etc.
  FF_SCRIPT     = (4 shl 4); // Cursive, etc.
  FF_DECORATIVE = (5 shl 4); // Old English, etc.

// Font Weights

  FW_DONTCARE   = 0;
  FW_THIN       = 100;
  FW_EXTRALIGHT = 200;
  FW_LIGHT      = 300;
  FW_NORMAL     = 400;
  FW_MEDIUM     = 500;
  FW_SEMIBOLD   = 600;
  FW_BOLD       = 700;
  FW_EXTRABOLD  = 800;
  FW_HEAVY      = 900;

  FW_ULTRALIGHT = FW_EXTRALIGHT;
  FW_REGULAR    = FW_NORMAL;
  FW_DEMIBOLD   = FW_SEMIBOLD;
  FW_ULTRABOLD  = FW_EXTRABOLD;
  FW_BLACK      = FW_HEAVY;

  PANOSE_COUNT              = 10;
  PAN_FAMILYTYPE_INDEX      = 0;
  PAN_SERIFSTYLE_INDEX      = 1;
  PAN_WEIGHT_INDEX          = 2;
  PAN_PROPORTION_INDEX      = 3;
  PAN_CONTRAST_INDEX        = 4;
  PAN_STROKEVARIATION_INDEX = 5;
  PAN_ARMSTYLE_INDEX        = 6;
  PAN_LETTERFORM_INDEX      = 7;
  PAN_MIDLINE_INDEX         = 8;
  PAN_XHEIGHT_INDEX         = 9;

  PAN_CULTURE_LATIN = 0;

type
  PPanose = ^TPanose;
  tagPANOSE = record
    bFamilyType: BYTE;
    bSerifStyle: BYTE;
    bWeight: BYTE;
    bProportion: BYTE;
    bContrast: BYTE;
    bStrokeVariation: BYTE;
    bArmStyle: BYTE;
    bLetterform: BYTE;
    bMidline: BYTE;
    bXHeight: BYTE;
  end;
  PANOSE = tagPANOSE;
  LPPANOSE = ^PANOSE;
  TPanose = PANOSE;

const
  PAN_ANY    = 0; // Any
  PAN_NO_FIT = 1; // No Fit

  PAN_FAMILY_TEXT_DISPLAY = 2; // Text and Display
  PAN_FAMILY_SCRIPT       = 3; // Script
  PAN_FAMILY_DECORATIVE   = 4; // Decorative
  PAN_FAMILY_PICTORIAL    = 5; // Pictorial

  PAN_SERIF_COVE               = 2; // Cove
  PAN_SERIF_OBTUSE_COVE        = 3; // Obtuse Cove
  PAN_SERIF_SQUARE_COVE        = 4; // Square Cove
  PAN_SERIF_OBTUSE_SQUARE_COVE = 5; // Obtuse Square Cove
  PAN_SERIF_SQUARE             = 6; // Square
  PAN_SERIF_THIN               = 7; // Thin
  PAN_SERIF_BONE               = 8; // Bone
  PAN_SERIF_EXAGGERATED        = 9; // Exaggerated
  PAN_SERIF_TRIANGLE           = 10; // Triangle
  PAN_SERIF_NORMAL_SANS        = 11; // Normal Sans
  PAN_SERIF_OBTUSE_SANS        = 12; // Obtuse Sans
  PAN_SERIF_PERP_SANS          = 13; // Prep Sans
  PAN_SERIF_FLARED             = 14; // Flared
  PAN_SERIF_ROUNDED            = 15; // Rounded

  PAN_WEIGHT_VERY_LIGHT = 2; // Very Light
  PAN_WEIGHT_LIGHT      = 3; // Light
  PAN_WEIGHT_THIN       = 4; // Thin
  PAN_WEIGHT_BOOK       = 5; // Book
  PAN_WEIGHT_MEDIUM     = 6; // Medium
  PAN_WEIGHT_DEMI       = 7; // Demi
  PAN_WEIGHT_BOLD       = 8; // Bold
  PAN_WEIGHT_HEAVY      = 9; // Heavy
  PAN_WEIGHT_BLACK      = 10; // Black
  PAN_WEIGHT_NORD       = 11; // Nord

  PAN_PROP_OLD_STYLE      = 2; // Old Style
  PAN_PROP_MODERN         = 3; // Modern
  PAN_PROP_EVEN_WIDTH     = 4; // Even Width
  PAN_PROP_EXPANDED       = 5; // Expanded
  PAN_PROP_CONDENSED      = 6; // Condensed
  PAN_PROP_VERY_EXPANDED  = 7; // Very Expanded
  PAN_PROP_VERY_CONDENSED = 8; // Very Condensed
  PAN_PROP_MONOSPACED     = 9; // Monospaced

  PAN_CONTRAST_NONE        = 2; // None
  PAN_CONTRAST_VERY_LOW    = 3; // Very Low
  PAN_CONTRAST_LOW         = 4; // Low
  PAN_CONTRAST_MEDIUM_LOW  = 5; // Medium Low
  PAN_CONTRAST_MEDIUM      = 6; // Medium
  PAN_CONTRAST_MEDIUM_HIGH = 7; // Mediim High
  PAN_CONTRAST_HIGH        = 8; // High
  PAN_CONTRAST_VERY_HIGH   = 9; // Very High

  PAN_STROKE_GRADUAL_DIAG = 2; // Gradual/Diagonal
  PAN_STROKE_GRADUAL_TRAN = 3; // Gradual/Transitional
  PAN_STROKE_GRADUAL_VERT = 4; // Gradual/Vertical
  PAN_STROKE_GRADUAL_HORZ = 5; // Gradual/Horizontal
  PAN_STROKE_RAPID_VERT   = 6; // Rapid/Vertical
  PAN_STROKE_RAPID_HORZ   = 7; // Rapid/Horizontal
  PAN_STROKE_INSTANT_VERT = 8; // Instant/Vertical

  PAN_STRAIGHT_ARMS_HORZ         = 2; // Straight Arms/Horizontal
  PAN_STRAIGHT_ARMS_WEDGE        = 3; // Straight Arms/Wedge
  PAN_STRAIGHT_ARMS_VERT         = 4; // Straight Arms/Vertical
  PAN_STRAIGHT_ARMS_SINGLE_SERIF = 5; // Straight Arms/Single-Serif
  PAN_STRAIGHT_ARMS_DOUBLE_SERIF = 6; // Straight Arms/Double-Serif
  PAN_BENT_ARMS_HORZ             = 7; // Non-Straight Arms/Horizontal
  PAN_BENT_ARMS_WEDGE            = 8; // Non-Straight Arms/Wedge
  PAN_BENT_ARMS_VERT             = 9; // Non-Straight Arms/Vertical
  PAN_BENT_ARMS_SINGLE_SERIF     = 10; // Non-Straight Arms/Single-Serif
  PAN_BENT_ARMS_DOUBLE_SERIF     = 11; // Non-Straight Arms/Double-Serif

  PAN_LETT_NORMAL_CONTACT     = 2; // Normal/Contact
  PAN_LETT_NORMAL_WEIGHTED    = 3; // Normal/Weighted
  PAN_LETT_NORMAL_BOXED       = 4; // Normal/Boxed
  PAN_LETT_NORMAL_FLATTENED   = 5; // Normal/Flattened
  PAN_LETT_NORMAL_ROUNDED     = 6; // Normal/Rounded
  PAN_LETT_NORMAL_OFF_CENTER  = 7; // Normal/Off Center
  PAN_LETT_NORMAL_SQUARE      = 8; // Normal/Square
  PAN_LETT_OBLIQUE_CONTACT    = 9; // Oblique/Contact
  PAN_LETT_OBLIQUE_WEIGHTED   = 10; // Oblique/Weighted
  PAN_LETT_OBLIQUE_BOXED      = 11; // Oblique/Boxed
  PAN_LETT_OBLIQUE_FLATTENED  = 12; // Oblique/Flattened
  PAN_LETT_OBLIQUE_ROUNDED    = 13; // Oblique/Rounded
  PAN_LETT_OBLIQUE_OFF_CENTER = 14; // Oblique/Off Center
  PAN_LETT_OBLIQUE_SQUARE     = 15; // Oblique/Square

  PAN_MIDLINE_STANDARD_TRIMMED = 2; // Standard/Trimmed
  PAN_MIDLINE_STANDARD_POINTED = 3; // Standard/Pointed
  PAN_MIDLINE_STANDARD_SERIFED = 4; // Standard/Serifed
  PAN_MIDLINE_HIGH_TRIMMED     = 5; // High/Trimmed
  PAN_MIDLINE_HIGH_POINTED     = 6; // High/Pointed
  PAN_MIDLINE_HIGH_SERIFED     = 7; // High/Serifed
  PAN_MIDLINE_CONSTANT_TRIMMED = 8; // Constant/Trimmed
  PAN_MIDLINE_CONSTANT_POINTED = 9; // Constant/Pointed
  PAN_MIDLINE_CONSTANT_SERIFED = 10; // Constant/Serifed
  PAN_MIDLINE_LOW_TRIMMED      = 11; // Low/Trimmed
  PAN_MIDLINE_LOW_POINTED      = 12; // Low/Pointed
  PAN_MIDLINE_LOW_SERIFED      = 13; // Low/Serifed

  PAN_XHEIGHT_CONSTANT_SMALL = 2; // Constant/Small
  PAN_XHEIGHT_CONSTANT_STD   = 3; // Constant/Standard
  PAN_XHEIGHT_CONSTANT_LARGE = 4; // Constant/Large
  PAN_XHEIGHT_DUCKING_SMALL  = 5; // Ducking/Small
  PAN_XHEIGHT_DUCKING_STD    = 6; // Ducking/Standard
  PAN_XHEIGHT_DUCKING_LARGE  = 7; // Ducking/Large

  ELF_VENDOR_SIZE = 4;

// The extended logical font
// An extension of the ENUMLOGFONT

type
  PExtLogFontA = ^TExtLogFontA;
  tagEXTLOGFONTA = record
    elfLogFont: LOGFONTA;
    elfFullName: array [0..LF_FULLFACESIZE - 1] of BYTE;
    elfStyle: array [0..LF_FACESIZE - 1] of BYTE;
    elfVersion: DWORD;
    elfStyleSize: DWORD;
    elfMatch: DWORD;
    elfReserved: DWORD;
    elfVendorId: array [0..ELF_VENDOR_SIZE - 1] of BYTE;
    elfCulture: DWORD;
    elfPanose: PANOSE;
  end;
  EXTLOGFONTA = tagEXTLOGFONTA;
  LPEXTLOGFONTA = ^EXTLOGFONTA;
  NPEXTLOGFONTA = ^EXTLOGFONTA;
  TExtLogFontA = EXTLOGFONTA;

  PExtLogFontW = ^TExtLogFontW;
  tagEXTLOGFONTW = record
    elfLogFont: LOGFONTW;
    elfFullName: array [0..LF_FULLFACESIZE - 1] of WCHAR;
    elfStyle: array [0..LF_FACESIZE - 1] of WCHAR;
    elfVersion: DWORD;
    elfStyleSize: DWORD;
    elfMatch: DWORD;
    elfReserved: DWORD;
    elfVendorId: array [0..ELF_VENDOR_SIZE - 1] of BYTE;
    elfCulture: DWORD;
    elfPanose: PANOSE;
  end;
  EXTLOGFONTW = tagEXTLOGFONTW;
  LPEXTLOGFONTW = ^EXTLOGFONTW;
  NPEXTLOGFONTW = ^EXTLOGFONTW;
  TExtLogFontW = EXTLOGFONTW;

{$IFDEF UNICODE}
  EXTLOGFONT = EXTLOGFONTW;
  PEXTLOGFONT = PEXTLOGFONTW;
  NPEXTLOGFONT = NPEXTLOGFONTW;
  LPEXTLOGFONT = LPEXTLOGFONTW;
  TExtLogFont = TExtLogFontW;
{$ELSE}
  EXTLOGFONT = EXTLOGFONTA;
  PEXTLOGFONT = PEXTLOGFONTA;
  NPEXTLOGFONT = NPEXTLOGFONTA;
  LPEXTLOGFONT = LPEXTLOGFONTA;
  TExtLogFont = TExtLogFontA;
{$ENDIF}

const
  ELF_VERSION       = 0;
  ELF_CULTURE_LATIN = 0;

// EnumFonts Masks

  RASTER_FONTTYPE   = $0001;
  DEVICE_FONTTYPE   = $002;
  TRUETYPE_FONTTYPE = $004;

function RGB(r, g, b: BYTE): COLORREF;
function PALETTERGB(r, g, b: BYTE): COLORREF;
function PALETTEINDEX(i: WORD): COLORREF;

// palette entry flags

const
  PC_RESERVED   = $01; // palette index used for animation
  PC_EXPLICIT   = $02; // palette index is explicit to device
  PC_NOCOLLAPSE = $04; // do not match color to system palette

function GetRValue(rgb: COLORREF): BYTE;
function GetGValue(rgb: COLORREF): BYTE;
function GetBValue(rgb: COLORREF): BYTE;

// Background Modes

const
  TRANSPARENT = 1;
  OPAQUE      = 2;
  BKMODE_LAST = 2;

// Graphics Modes

  GM_COMPATIBLE = 1;
  GM_ADVANCED   = 2;
  GM_LAST       = 2;

// PolyDraw and GetPath point types

  PT_CLOSEFIGURE = $01;
  PT_LINETO      = $02;
  PT_BEZIERTO    = $04;
  PT_MOVETO      = $06;

// Mapping Modes

  MM_TEXT        = 1;
  MM_LOMETRIC    = 2;
  MM_HIMETRIC    = 3;
  MM_LOENGLISH   = 4;
  MM_HIENGLISH   = 5;
  MM_TWIPS       = 6;
  MM_ISOTROPIC   = 7;
  MM_ANISOTROPIC = 8;

// Min and Max Mapping Mode values

  MM_MIN            = MM_TEXT;
  MM_MAX            = MM_ANISOTROPIC;
  MM_MAX_FIXEDSCALE = MM_TWIPS;

// Coordinate Modes

  ABSOLUTE = 1;
  RELATIVE = 2;

// Stock Logical Objects

  WHITE_BRUSH         = 0;
  LTGRAY_BRUSH        = 1;
  GRAY_BRUSH          = 2;
  DKGRAY_BRUSH        = 3;
  BLACK_BRUSH         = 4;
  NULL_BRUSH          = 5;
  HOLLOW_BRUSH        = NULL_BRUSH;
  WHITE_PEN           = 6;
  BLACK_PEN           = 7;
  NULL_PEN            = 8;
  OEM_FIXED_FONT      = 10;
  ANSI_FIXED_FONT     = 11;
  ANSI_VAR_FONT       = 12;
  SYSTEM_FONT         = 13;
  DEVICE_DEFAULT_FONT = 14;
  DEFAULT_PALETTE     = 15;
  SYSTEM_FIXED_FONT   = 16;

  DEFAULT_GUI_FONT = 17;

  DC_BRUSH = 18;
  DC_PEN   = 19;

{$IFDEF WIN32_WINNT_0500_GREATER}
  STOCK_LAST = 19;
{$ELSE}
{$IFDEF WINVER_0400_GREATER}
  STOCK_LAST = 17;
{$ELSE}
  STOCK_LAST = 16;
{$ENDIF}
{$ENDIF}

  CLR_INVALID = DWORD($FFFFFFFF);

// Brush Styles

  BS_SOLID         = 0;
  BS_NULL          = 1;
  BS_HOLLOW        = BS_NULL;
  BS_HATCHED       = 2;
  BS_PATTERN       = 3;
  BS_INDEXED       = 4;
  BS_DIBPATTERN    = 5;
  BS_DIBPATTERNPT  = 6;
  BS_PATTERN8X8    = 7;
  BS_DIBPATTERN8X8 = 8;
  BS_MONOPATTERN   = 9;

// Hatch Styles

  HS_HORIZONTAL = 0; // -----
  HS_VERTICAL   = 1; // |||||
  HS_FDIAGONAL  = 2; // \\\\\
  HS_BDIAGONAL  = 3; // /////
  HS_CROSS      = 4; // +++++
  HS_DIAGCROSS  = 5; // xxxxx

// Pen Styles

  PS_SOLID       = 0;
  PS_DASH        = 1; // -------
  PS_DOT         = 2; // .......
  PS_DASHDOT     = 3; // _._._._
  PS_DASHDOTDOT  = 4; // _.._.._
  PS_NULL        = 5;
  PS_INSIDEFRAME = 6;
  PS_USERSTYLE   = 7;
  PS_ALTERNATE   = 8;
  PS_STYLE_MASK  = $0000000F;

  PS_ENDCAP_ROUND  = $00000000;
  PS_ENDCAP_SQUARE = $00000100;
  PS_ENDCAP_FLAT   = $00000200;
  PS_ENDCAP_MASK   = $00000F00;

  PS_JOIN_ROUND = $00000000;
  PS_JOIN_BEVEL = $00001000;
  PS_JOIN_MITER = $00002000;
  PS_JOIN_MASK  = $0000F000;

  PS_COSMETIC  = $00000000;
  PS_GEOMETRIC = $00010000;
  PS_TYPE_MASK = $000F0000;

  AD_COUNTERCLOCKWISE = 1;
  AD_CLOCKWISE        = 2;

// Device Parameters for GetDeviceCaps()

  DRIVERVERSION = 0; // Device driver version
  TECHNOLOGY    = 2; // Device classification
  HORZSIZE      = 4; // Horizontal size in millimeters
  VERTSIZE      = 6; // Vertical size in millimeters
  HORZRES       = 8; // Horizontal width in pixels
  VERTRES       = 10; // Vertical height in pixels
  BITSPIXEL     = 12; // Number of bits per pixel
  PLANES        = 14; // Number of planes
  NUMBRUSHES    = 16; // Number of brushes the device has
  NUMPENS       = 18; // Number of pens the device has
  NUMMARKERS    = 20; // Number of markers the device has
  NUMFONTS      = 22; // Number of fonts the device has
  NUMCOLORS     = 24; // Number of colors the device supports
  PDEVICESIZE   = 26; // Size required for device descriptor
  CURVECAPS     = 28; // Curve capabilities
  LINECAPS      = 30; // Line capabilities
  POLYGONALCAPS = 32; // Polygonal capabilities
  TEXTCAPS      = 34; // Text capabilities
  CLIPCAPS      = 36; // Clipping capabilities
  RASTERCAPS    = 38; // Bitblt capabilities
  ASPECTX       = 40; // Length of the X leg
  ASPECTY       = 42; // Length of the Y leg
  ASPECTXY      = 44; // Length of the hypotenuse

  LOGPIXELSX = 88; // Logical pixels/inch in X
  LOGPIXELSY = 90; // Logical pixels/inch in Y

  SIZEPALETTE = 104; // Number of entries in physical palette
  NUMRESERVED = 106; // Number of reserved entries in palette
  COLORRES    = 108; // Actual color resolution

// Printing related DeviceCaps. These replace the appropriate Escapes

  PHYSICALWIDTH   = 110; // Physical Width in device units
  PHYSICALHEIGHT  = 111; // Physical Height in device units
  PHYSICALOFFSETX = 112; // Physical Printable Area x margin
  PHYSICALOFFSETY = 113; // Physical Printable Area y margin
  SCALINGFACTORX  = 114; // Scaling factor x
  SCALINGFACTORY  = 115; // Scaling factor y

// Display driver specific

  VREFRESH = 116;       // Current vertical refresh rate of the
                        // display device (for displays only) in Hz
  DESKTOPVERTRES = 117; // Horizontal width of entire desktop in
                        // pixels
  DESKTOPHORZRES = 118; // Vertical height of entire desktop in
                        // pixels
  BLTALIGNMENT = 119;   // Preferred blt alignment

  SHADEBLENDCAPS = 120; // Shading and blending caps
  COLORMGMTCAPS  = 121; // Color Management caps

// Device Capability Masks:

// Device Technologies

  DT_PLOTTER    = 0; // Vector plotter
  DT_RASDISPLAY = 1; // Raster display
  DT_RASPRINTER = 2; // Raster printer
  DT_RASCAMERA  = 3; // Raster camera
  DT_CHARSTREAM = 4; // Character-stream, PLP
  DT_METAFILE   = 5; // Metafile, VDM
  DT_DISPFILE   = 6; // Display-file

// Curve Capabilities

  CC_NONE       = 0; // Curves not supported
  CC_CIRCLES    = 1; // Can do circles
  CC_PIE        = 2; // Can do pie wedges
  CC_CHORD      = 4; // Can do chord arcs
  CC_ELLIPSES   = 8; // Can do ellipese
  CC_WIDE       = 16; // Can do wide lines
  CC_STYLED     = 32; // Can do styled lines
  CC_WIDESTYLED = 64; // Can do wide styled lines
  CC_INTERIORS  = 128; // Can do interiors
  CC_ROUNDRECT  = 256;

// Line Capabilities

  LC_NONE       = 0; // Lines not supported
  LC_POLYLINE   = 2; // Can do polylines
  LC_MARKER     = 4; // Can do markers
  LC_POLYMARKER = 8; // Can do polymarkers
  LC_WIDE       = 16; // Can do wide lines
  LC_STYLED     = 32; // Can do styled lines
  LC_WIDESTYLED = 64; // Can do wide styled lines
  LC_INTERIORS  = 128; // Can do interiors

// Polygonal Capabilities

  PC_NONE        = 0; // Polygonals not supported
  PC_POLYGON     = 1; // Can do polygons
  PC_RECTANGLE   = 2; // Can do rectangles
  PC_WINDPOLYGON = 4; // Can do winding polygons
  PC_TRAPEZOID   = 4; // Can do trapezoids
  PC_SCANLINE    = 8; // Can do scanlines
  PC_WIDE        = 16; // Can do wide borders
  PC_STYLED      = 32; // Can do styled borders
  PC_WIDESTYLED  = 64; // Can do wide styled borders
  PC_INTERIORS   = 128; // Can do interiors
  PC_POLYPOLYGON = 256; // Can do polypolygons
  PC_PATHS       = 512; // Can do paths

// Clipping Capabilities

  CP_NONE      = 0; // No clipping of output
  CP_RECTANGLE = 1; // Output clipped to rects
  CP_REGION    = 2; // obsolete

// Text Capabilities

  TC_OP_CHARACTER = $00000001; // Can do OutputPrecision   CHARACTER
  TC_OP_STROKE    = $00000002; // Can do OutputPrecision   STROKE
  TC_CP_STROKE    = $00000004; // Can do ClipPrecision     STROKE
  TC_CR_90        = $00000008; // Can do CharRotAbility    90
  TC_CR_ANY       = $00000010; // Can do CharRotAbility    ANY
  TC_SF_X_YINDEP  = $00000020; // Can do ScaleFreedom      X_YINDEPENDENT
  TC_SA_DOUBLE    = $00000040; // Can do ScaleAbility      DOUBLE
  TC_SA_INTEGER   = $00000080; // Can do ScaleAbility      INTEGER
  TC_SA_CONTIN    = $00000100; // Can do ScaleAbility      CONTINUOUS
  TC_EA_DOUBLE    = $00000200; // Can do EmboldenAbility   DOUBLE
  TC_IA_ABLE      = $00000400; // Can do ItalisizeAbility  ABLE
  TC_UA_ABLE      = $00000800; // Can do UnderlineAbility  ABLE
  TC_SO_ABLE      = $00001000; // Can do StrikeOutAbility  ABLE
  TC_RA_ABLE      = $00002000; // Can do RasterFontAble    ABLE
  TC_VA_ABLE      = $00004000; // Can do VectorFontAble    ABLE
  TC_RESERVED     = $00008000;
  TC_SCROLLBLT    = $00010000; // Don't do text scroll with blt

// Raster Capabilities

  RC_BITBLT       = 1; // Can do standard BLT.
  RC_BANDING      = 2; // Device requires banding support
  RC_SCALING      = 4; // Device requires scaling support
  RC_BITMAP64     = 8; // Device can support >64K bitmap
  RC_GDI20_OUTPUT = $0010; // has 2.0 output calls
  RC_GDI20_STATE  = $0020;
  RC_SAVEBITMAP   = $0040;
  RC_DI_BITMAP    = $0080; // supports DIB to memory
  RC_PALETTE      = $0100; // supports a palette
  RC_DIBTODEV     = $0200; // supports DIBitsToDevice
  RC_BIGFONT      = $0400; // supports >64K fonts
  RC_STRETCHBLT   = $0800; // supports StretchBlt
  RC_FLOODFILL    = $1000; // supports FloodFill
  RC_STRETCHDIB   = $2000; // supports StretchDIBits
  RC_OP_DX_OUTPUT = $4000;
  RC_DEVBITS      = $8000;

// Shading and blending caps

  SB_NONE          = $00000000;
  SB_CONST_ALPHA   = $00000001;
  SB_PIXEL_ALPHA   = $00000002;
  SB_PREMULT_ALPHA = $00000004;

  SB_GRAD_RECT = $00000010;
  SB_GRAD_TRI  = $00000020;

// Color Management caps

  CM_NONE       = $00000000;
  CM_DEVICE_ICM = $00000001;
  CM_GAMMA_RAMP = $00000002;
  CM_CMYK_COLOR = $00000004;

// DIB color table identifiers

  DIB_RGB_COLORS = 0; // color table in RGBs
  DIB_PAL_COLORS = 1; // color table in palette indices

// constants for Get/SetSystemPaletteUse()

  SYSPAL_ERROR       = 0;
  SYSPAL_STATIC      = 1;
  SYSPAL_NOSTATIC    = 2;
  SYSPAL_NOSTATIC256 = 3;

// constants for CreateDIBitmap

  CBM_INIT = $04; // initialize bitmap

// ExtFloodFill style flags

  FLOODFILLBORDER  = 0;
  FLOODFILLSURFACE = 1;

// size of a device name string

  CCHDEVICENAME = 32;

// size of a form name string

  CCHFORMNAME = 32;


{$IFDEF WINVER_0500_GREATER}
{$IFDEF WIN32_WINNT_0400_GREATER}
{$DEFINE WINVER_0500_OR_WIN32_WINNT_0400}
{$ENDIF}
{$ENDIF}

type
  TDmDisplayFlagsUnion = record
    case Integer of
      0: (
        dmDisplayFlags: DWORD);
      1: (
        dmNup: DWORD);
  end;

  _devicemodeA = record
    dmDeviceName: array [0..CCHDEVICENAME - 1] of BYTE;
    dmSpecVersion: WORD;
    dmDriverVersion: WORD;
    dmSize: WORD;
    dmDriverExtra: WORD;
    dmFields: DWORD;
    union1: record
    case Integer of
      // printer only fields
      0: (
        dmOrientation: Smallint;
        dmPaperSize: Smallint;
        dmPaperLength: Smallint;
        dmPaperWidth: Smallint;
        dmScale: Smallint;
        dmCopies: Smallint;
        dmDefaultSource: Smallint;
        dmPrintQuality: Smallint);
      // display only fields
      1: (
        dmPosition: POINTL;
        dmDisplayOrientation: DWORD;
        dmDisplayFixedOutput: DWORD);
    end;
    dmColor: Shortint;
    dmDuplex: Shortint;
    dmYResolution: Shortint;
    dmTTOption: Shortint;
    dmCollate: Shortint;
    dmFormName: array [0..CCHFORMNAME - 1] of BYTE;
    dmLogPixels: WORD;
    dmBitsPerPel: DWORD;
    dmPelsWidth: DWORD;
    dmPelsHeight: DWORD;
    dmDisplayFlags: TDmDisplayFlagsUnion;
    dmDisplayFrequency: DWORD;
    {$IFDEF WINVER_0400_GREATER}
    dmICMMethod: DWORD;
    dmICMIntent: DWORD;
    dmMediaType: DWORD;
    dmDitherType: DWORD;
    dmReserved1: DWORD;
    dmReserved2: DWORD;
    {$IFDEF WINVER_0500_OR_WIN32_WINNT_0400}
    dmPanningWidth: DWORD;
    dmPanningHeight: DWORD;
    {$ENDIF}
    {$ENDIF}
  end;
  DEVMODEA = _devicemodeA;
  PDEVMODEA = ^DEVMODEA;
  LPDEVMODEA = ^DEVMODEA;
  NPDEVMODEA = ^DEVMODEA;
  TDevModeA = _devicemodeA;

  _devicemodeW = record
    dmDeviceName: array [0..CCHDEVICENAME - 1] of WCHAR;
    dmSpecVersion: WORD;
    dmDriverVersion: WORD;
    dmSize: WORD;
    dmDriverExtra: WORD;
    dmFields: DWORD;
    union1: record
    case Integer of
      // printer only fields
      0: (
        dmOrientation: Smallint;
        dmPaperSize: Smallint;
        dmPaperLength: Smallint;
        dmPaperWidth: Smallint;
        dmScale: Smallint;
        dmCopies: Smallint;
        dmDefaultSource: Smallint;
        dmPrintQuality: Smallint);
      // display only fields
      1: (
        dmPosition: POINTL;
        dmDisplayOrientation: DWORD;
        dmDisplayFixedOutput: DWORD);
    end;
    dmColor: Shortint;
    dmDuplex: Shortint;
    dmYResolution: Shortint;
    dmTTOption: Shortint;
    dmCollate: Shortint;
    dmFormName: array [0..CCHFORMNAME - 1] of WCHAR;
    dmLogPixels: WORD;
    dmBitsPerPel: DWORD;
    dmPelsWidth: DWORD;
    dmPelsHeight: DWORD;
    dmDiusplayFlags: TDmDisplayFlagsUnion;
    dmDisplayFrequency: DWORD;
    {$IFDEF WINVER_0400_GREATER}
    dmICMMethod: DWORD;
    dmICMIntent: DWORD;
    dmMediaType: DWORD;
    dmDitherType: DWORD;
    dmReserved1: DWORD;
    dmReserved2: DWORD;
    {$IFDEF WINVER_0500_OR_WIN32_WINNT_0400}
    dmPanningWidth: DWORD;
    dmPanningHeight: DWORD;
    {$ENDIF}
    {$ENDIF}
  end;
  DEVMODEW = _devicemodeW;
  PDEVMODEW = ^DEVMODEW;
  LPDEVMODEW = ^DEVMODEW;
  NPDEVMODEW = ^DEVMODEW;
  TDevModeW = _devicemodeW;

{$IFDEF UNICODE}
  DEVMODE = DEVMODEW;
  PDEVMODE = PDEVMODEW;
  NPDEVMODE = NPDEVMODEW;
  LPDEVMODE = LPDEVMODEW;
  TDevMode = TDevModeW;
{$ELSE}
  DEVMODE = DEVMODEA;
  PDEVMODE = PDEVMODEA;
  NPDEVMODE = NPDEVMODEA;
  LPDEVMODE = LPDEVMODEA;
  TDevMode = TDevModeA;
{$ENDIF}

// current version of specification

const
{$IFDEF WINVER_0500_OR_WIN32_WINNT_0400}
  DM_SPECVERSION = $0401;
{$ELSE}
{$IFDEF WINVER_0400_GREATER}
  DM_SPECVERSION = $0400;
{$ELSE}
  DM_SPECVERSION = $0320;
{$ENDIF}
{$ENDIF}

{$UNDEF WINVER_0500_OR_WIN32_WINNT_0400}

// field selection bits

const
  DM_ORIENTATION      = $00000001;
  DM_PAPERSIZE        = $00000002;
  DM_PAPERLENGTH      = $00000004;
  DM_PAPERWIDTH       = $00000008;
  DM_SCALE            = $00000010;
  DM_POSITION         = $00000020;
  DM_NUP              = $00000040;
//#if(WINVER >= 0x0501)
  DM_DISPLAYORIENTATION = $00000080;
//#endif /* WINVER >= 0x0501 */
  DM_COPIES           = $00000100;
  DM_DEFAULTSOURCE    = $00000200;
  DM_PRINTQUALITY     = $00000400;
  DM_COLOR            = $00000800;
  DM_DUPLEX           = $00001000;
  DM_YRESOLUTION      = $00002000;
  DM_TTOPTION         = $00004000;
  DM_COLLATE          = $00008000;
  DM_FORMNAME         = $00010000;
  DM_LOGPIXELS        = $00020000;
  DM_BITSPERPEL       = $00040000;
  DM_PELSWIDTH        = $00080000;
  DM_PELSHEIGHT       = $00100000;
  DM_DISPLAYFLAGS     = $00200000;
  DM_DISPLAYFREQUENCY = $00400000;
  DM_ICMMETHOD        = $00800000;
  DM_ICMINTENT        = $01000000;
  DM_MEDIATYPE        = $02000000;
  DM_DITHERTYPE       = $04000000;
  DM_PANNINGWIDTH     = $08000000;
  DM_PANNINGHEIGHT    = $10000000;
//#if(WINVER >= 0x0501)
  DM_DISPLAYFIXEDOUTPUT = $20000000;
//#endif /* WINVER >= 0x0501 */

// orientation selections

  DMORIENT_PORTRAIT  = 1;
  DMORIENT_LANDSCAPE = 2;

// paper selections

  DMPAPER_LETTER                  = 1; // Letter 8 1/2 x 11 in
  DMPAPER_FIRST                   = DMPAPER_LETTER;
  DMPAPER_LETTERSMALL             = 2; // Letter Small 8 1/2 x 11 in
  DMPAPER_TABLOID                 = 3; // Tabloid 11 x 17 in
  DMPAPER_LEDGER                  = 4; // Ledger 17 x 11 in
  DMPAPER_LEGAL                   = 5; // Legal 8 1/2 x 14 in
  DMPAPER_STATEMENT               = 6; // Statement 5 1/2 x 8 1/2 in
  DMPAPER_EXECUTIVE               = 7; // Executive 7 1/4 x 10 1/2 in
  DMPAPER_A3                      = 8; // A3 297 x 420 mm
  DMPAPER_A4                      = 9; // A4 210 x 297 mm
  DMPAPER_A4SMALL                 = 10; // A4 Small 210 x 297 mm
  DMPAPER_A5                      = 11; // A5 148 x 210 mm
  DMPAPER_B4                      = 12; // B4 (JIS) 250 x 354
  DMPAPER_B5                      = 13; // B5 (JIS) 182 x 257 mm
  DMPAPER_FOLIO                   = 14; // Folio 8 1/2 x 13 in
  DMPAPER_QUARTO                  = 15; // Quarto 215 x 275 mm
  DMPAPER_10X14                   = 16; // 10x14 in
  DMPAPER_11X17                   = 17; // 11x17 in
  DMPAPER_NOTE                    = 18; // Note 8 1/2 x 11 in
  DMPAPER_ENV_9                   = 19; // Envelope #9 3 7/8 x 8 7/8
  DMPAPER_ENV_10                  = 20; // Envelope #10 4 1/8 x 9 1/2
  DMPAPER_ENV_11                  = 21; // Envelope #11 4 1/2 x 10 3/8
  DMPAPER_ENV_12                  = 22; // Envelope #12 4 \276 x 11
  DMPAPER_ENV_14                  = 23; // Envelope #14 5 x 11 1/2
  DMPAPER_CSHEET                  = 24; // C size sheet
  DMPAPER_DSHEET                  = 25; // D size sheet
  DMPAPER_ESHEET                  = 26; // E size sheet
  DMPAPER_ENV_DL                  = 27; // Envelope DL 110 x 220mm
  DMPAPER_ENV_C5                  = 28; // Envelope C5 162 x 229 mm
  DMPAPER_ENV_C3                  = 29; // Envelope C3  324 x 458 mm
  DMPAPER_ENV_C4                  = 30; // Envelope C4  229 x 324 mm
  DMPAPER_ENV_C6                  = 31; // Envelope C6  114 x 162 mm
  DMPAPER_ENV_C65                 = 32; // Envelope C65 114 x 229 mm
  DMPAPER_ENV_B4                  = 33; // Envelope B4  250 x 353 mm
  DMPAPER_ENV_B5                  = 34; // Envelope B5  176 x 250 mm
  DMPAPER_ENV_B6                  = 35; // Envelope B6  176 x 125 mm
  DMPAPER_ENV_ITALY               = 36; // Envelope 110 x 230 mm
  DMPAPER_ENV_MONARCH             = 37; // Envelope Monarch 3.875 x 7.5 in
  DMPAPER_ENV_PERSONAL            = 38; // 6 3/4 Envelope 3 5/8 x 6 1/2 in
  DMPAPER_FANFOLD_US              = 39; // US Std Fanfold 14 7/8 x 11 in
  DMPAPER_FANFOLD_STD_GERMAN      = 40; // German Std Fanfold 8 1/2 x 12 in
  DMPAPER_FANFOLD_LGL_GERMAN      = 41; // German Legal Fanfold 8 1/2 x 13 in
  DMPAPER_ISO_B4                  = 42; // B4 (ISO) 250 x 353 mm
  DMPAPER_JAPANESE_POSTCARD       = 43; // Japanese Postcard 100 x 148 mm
  DMPAPER_9X11                    = 44; // 9 x 11 in
  DMPAPER_10X11                   = 45; // 10 x 11 in
  DMPAPER_15X11                   = 46; // 15 x 11 in
  DMPAPER_ENV_INVITE              = 47; // Envelope Invite 220 x 220 mm
  DMPAPER_RESERVED_48             = 48; // RESERVED--DO NOT USE
  DMPAPER_RESERVED_49             = 49; // RESERVED--DO NOT USE
  DMPAPER_LETTER_EXTRA            = 50; // Letter Extra 9 \275 x 12 in
  DMPAPER_LEGAL_EXTRA             = 51; // Legal Extra 9 \275 x 15 in
  DMPAPER_TABLOID_EXTRA           = 52; // Tabloid Extra 11.69 x 18 in
  DMPAPER_A4_EXTRA                = 53; // A4 Extra 9.27 x 12.69 in
  DMPAPER_LETTER_TRANSVERSE       = 54; // Letter Transverse 8 \275 x 11 in
  DMPAPER_A4_TRANSVERSE           = 55; // A4 Transverse 210 x 297 mm
  DMPAPER_LETTER_EXTRA_TRANSVERSE = 56; // Letter Extra Transverse 9\275 x 12 in
  DMPAPER_A_PLUS                  = 57; // SuperA/SuperA/A4 227 x 356 mm
  DMPAPER_B_PLUS                  = 58; // SuperB/SuperB/A3 305 x 487 mm
  DMPAPER_LETTER_PLUS             = 59; // Letter Plus 8.5 x 12.69 in
  DMPAPER_A4_PLUS                 = 60; // A4 Plus 210 x 330 mm
  DMPAPER_A5_TRANSVERSE           = 61; // A5 Transverse 148 x 210 mm
  DMPAPER_B5_TRANSVERSE           = 62; // B5 (JIS) Transverse 182 x 257 mm
  DMPAPER_A3_EXTRA                = 63; // A3 Extra 322 x 445 mm
  DMPAPER_A5_EXTRA                = 64; // A5 Extra 174 x 235 mm
  DMPAPER_B5_EXTRA                = 65; // B5 (ISO) Extra 201 x 276 mm
  DMPAPER_A2                      = 66; // A2 420 x 594 mm
  DMPAPER_A3_TRANSVERSE           = 67; // A3 Transverse 297 x 420 mm
  DMPAPER_A3_EXTRA_TRANSVERSE     = 68; // A3 Extra Transverse 322 x 445 mm

  DMPAPER_DBL_JAPANESE_POSTCARD         = 69; // Japanese Double Postcard 200 x 148 mm
  DMPAPER_A6                            = 70; // A6 105 x 148 mm
  DMPAPER_JENV_KAKU2                    = 71; // Japanese Envelope Kaku #2
  DMPAPER_JENV_KAKU3                    = 72; // Japanese Envelope Kaku #3
  DMPAPER_JENV_CHOU3                    = 73; // Japanese Envelope Chou #3
  DMPAPER_JENV_CHOU4                    = 74; // Japanese Envelope Chou #4
  DMPAPER_LETTER_ROTATED                = 75; // Letter Rotated 11 x 8 1/2 11 in
  DMPAPER_A3_ROTATED                    = 76; // A3 Rotated 420 x 297 mm
  DMPAPER_A4_ROTATED                    = 77; // A4 Rotated 297 x 210 mm
  DMPAPER_A5_ROTATED                    = 78; // A5 Rotated 210 x 148 mm
  DMPAPER_B4_JIS_ROTATED                = 79; // B4 (JIS) Rotated 364 x 257 mm
  DMPAPER_B5_JIS_ROTATED                = 80; // B5 (JIS) Rotated 257 x 182 mm
  DMPAPER_JAPANESE_POSTCARD_ROTATED     = 81; // Japanese Postcard Rotated 148 x 100 mm
  DMPAPER_DBL_JAPANESE_POSTCARD_ROTATED = 82; // Double Japanese Postcard Rotated 148 x 200 mm
  DMPAPER_A6_ROTATED                    = 83; // A6 Rotated 148 x 105 mm
  DMPAPER_JENV_KAKU2_ROTATED            = 84; // Japanese Envelope Kaku #2 Rotated
  DMPAPER_JENV_KAKU3_ROTATED            = 85; // Japanese Envelope Kaku #3 Rotated
  DMPAPER_JENV_CHOU3_ROTATED            = 86; // Japanese Envelope Chou #3 Rotated
  DMPAPER_JENV_CHOU4_ROTATED            = 87; // Japanese Envelope Chou #4 Rotated
  DMPAPER_B6_JIS                        = 88; // B6 (JIS) 128 x 182 mm
  DMPAPER_B6_JIS_ROTATED                = 89; // B6 (JIS) Rotated 182 x 128 mm
  DMPAPER_12X11                         = 90; // 12 x 11 in
  DMPAPER_JENV_YOU4                     = 91; // Japanese Envelope You #4
  DMPAPER_JENV_YOU4_ROTATED             = 92; // Japanese Envelope You #4 Rotated
  DMPAPER_P16K                          = 93; // PRC 16K 146 x 215 mm
  DMPAPER_P32K                          = 94; // PRC 32K 97 x 151 mm
  DMPAPER_P32KBIG                       = 95; // PRC 32K(Big) 97 x 151 mm
  DMPAPER_PENV_1                        = 96; // PRC Envelope #1 102 x 165 mm
  DMPAPER_PENV_2                        = 97; // PRC Envelope #2 102 x 176 mm
  DMPAPER_PENV_3                        = 98; // PRC Envelope #3 125 x 176 mm
  DMPAPER_PENV_4                        = 99; // PRC Envelope #4 110 x 208 mm
  DMPAPER_PENV_5                        = 100; // PRC Envelope #5 110 x 220 mm
  DMPAPER_PENV_6                        = 101; // PRC Envelope #6 120 x 230 mm
  DMPAPER_PENV_7                        = 102; // PRC Envelope #7 160 x 230 mm
  DMPAPER_PENV_8                        = 103; // PRC Envelope #8 120 x 309 mm
  DMPAPER_PENV_9                        = 104; // PRC Envelope #9 229 x 324 mm
  DMPAPER_PENV_10                       = 105; // PRC Envelope #10 324 x 458 mm
  DMPAPER_P16K_ROTATED                  = 106; // PRC 16K Rotated
  DMPAPER_P32K_ROTATED                  = 107; // PRC 32K Rotated
  DMPAPER_P32KBIG_ROTATED               = 108; // PRC 32K(Big) Rotated
  DMPAPER_PENV_1_ROTATED                = 109; // PRC Envelope #1 Rotated 165 x 102 mm
  DMPAPER_PENV_2_ROTATED                = 110; // PRC Envelope #2 Rotated 176 x 102 mm
  DMPAPER_PENV_3_ROTATED                = 111; // PRC Envelope #3 Rotated 176 x 125 mm
  DMPAPER_PENV_4_ROTATED                = 112; // PRC Envelope #4 Rotated 208 x 110 mm
  DMPAPER_PENV_5_ROTATED                = 113; // PRC Envelope #5 Rotated 220 x 110 mm
  DMPAPER_PENV_6_ROTATED                = 114; // PRC Envelope #6 Rotated 230 x 120 mm
  DMPAPER_PENV_7_ROTATED                = 115; // PRC Envelope #7 Rotated 230 x 160 mm
  DMPAPER_PENV_8_ROTATED                = 116; // PRC Envelope #8 Rotated 309 x 120 mm
  DMPAPER_PENV_9_ROTATED                = 117; // PRC Envelope #9 Rotated 324 x 229 mm
  DMPAPER_PENV_10_ROTATED               = 118; // PRC Envelope #10 Rotated 458 x 324 mm

{$IFDEF WINVER_0500_GREATER}
  DMPAPER_LAST = DMPAPER_PENV_10_ROTATED;
{$ELSE}
{$IFDEF WINVER_0400_GREATER}
  DMPAPER_LAST = DMPAPER_A3_EXTRA_TRANSVERSE;
{$ELSE}
  DMPAPER_LAST = DMPAPER_FANFOLD_LGL_GERMAN;
{$ENDIF}
{$ENDIF}

  DMPAPER_USER = 256;

// bin selections

  DMBIN_UPPER         = 1;
  DMBIN_FIRST         = DMBIN_UPPER;
  DMBIN_ONLYONE       = 1;
  DMBIN_LOWER         = 2;
  DMBIN_MIDDLE        = 3;
  DMBIN_MANUAL        = 4;
  DMBIN_ENVELOPE      = 5;
  DMBIN_ENVMANUAL     = 6;
  DMBIN_AUTO          = 7;
  DMBIN_TRACTOR       = 8;
  DMBIN_SMALLFMT      = 9;
  DMBIN_LARGEFMT      = 10;
  DMBIN_LARGECAPACITY = 11;
  DMBIN_CASSETTE      = 14;
  DMBIN_FORMSOURCE    = 15;
  DMBIN_LAST          = DMBIN_FORMSOURCE;

  DMBIN_USER = 256; // device specific bins start here

// print qualities

  DMRES_DRAFT  = DWORD(-1);
  DMRES_LOW    = DWORD(-2);
  DMRES_MEDIUM = DWORD(-3);
  DMRES_HIGH   = DWORD(-4);

// color enable/disable for color printers

  DMCOLOR_MONOCHROME = 1;
  DMCOLOR_COLOR      = 2;

// duplex enable

  DMDUP_SIMPLEX    = 1;
  DMDUP_VERTICAL   = 2;
  DMDUP_HORIZONTAL = 3;

// TrueType options

  DMTT_BITMAP           = 1; // print TT fonts as graphics
  DMTT_DOWNLOAD         = 2; // download TT fonts as soft fonts
  DMTT_SUBDEV           = 3; // substitute device fonts for TT fonts
  DMTT_DOWNLOAD_OUTLINE = 4; // download TT fonts as outline soft fonts

// Collation selections

  DMCOLLATE_FALSE = 0;
  DMCOLLATE_TRUE  = 1;

//#if(WINVER >= 0x0501)

// DEVMODE dmDisplayOrientation specifiations

  DMDO_DEFAULT   = 0;
  DMDO_90        = 1;
  DMDO_180       = 2;
  DMDO_270       = 3;

// DEVMODE dmDisplayFixedOutput specifiations

  DMDFO_DEFAULT  = 0;
  DMDFO_STRETCH  = 1;
  DMDFO_CENTER   = 2;

//#endif /* WINVER >= 0x0501 */

// DEVMODE dmDisplayFlags flags

// #define DM_GRAYSCALE            0x00000001 /* This flag is no longer valid */
// #define DM_INTERLACED           0x00000002 /* This flag is no longer valid */

  DMDISPLAYFLAGS_TEXTMODE = $00000004;

// dmNup , multiple logical page per physical page options

  DMNUP_SYSTEM = 1;
  DMNUP_ONEUP  = 2;

// ICM methods

  DMICMMETHOD_NONE   = 1; // ICM disabled
  DMICMMETHOD_SYSTEM = 2; // ICM handled by system
  DMICMMETHOD_DRIVER = 3; // ICM handled by driver
  DMICMMETHOD_DEVICE = 4; // ICM handled by device

  DMICMMETHOD_USER = 256; // Device-specific methods start here

// ICM Intents

  DMICM_SATURATE         = 1; // Maximize color saturation
  DMICM_CONTRAST         = 2; // Maximize color contrast
  DMICM_COLORIMETRIC     = 3; // Use specific color metric
  DMICM_ABS_COLORIMETRIC = 4; // Use specific color metric

  DMICM_USER = 256; // Device-specific intents start here

// Media types

  DMMEDIA_STANDARD     = 1; // Standard paper
  DMMEDIA_TRANSPARENCY = 2; // Transparency
  DMMEDIA_GLOSSY       = 3; // Glossy paper

  DMMEDIA_USER = 256; // Device-specific media start here

// Dither types

  DMDITHER_NONE           = 1; // No dithering
  DMDITHER_COARSE         = 2; // Dither with a coarse brush
  DMDITHER_FINE           = 3; // Dither with a fine brush
  DMDITHER_LINEART        = 4; // LineArt dithering
  DMDITHER_ERRORDIFFUSION = 5; // LineArt dithering
  DMDITHER_RESERVED6      = 6; // LineArt dithering
  DMDITHER_RESERVED7      = 7; // LineArt dithering
  DMDITHER_RESERVED8      = 8; // LineArt dithering
  DMDITHER_RESERVED9      = 9; // LineArt dithering
  DMDITHER_GRAYSCALE      = 10; // Device does grayscaling

  DMDITHER_USER = 256; // Device-specific dithers start here

type
  PDisplayDeviceA = ^TDisplayDeviceA;
  _DISPLAY_DEVICEA = record
    cb: DWORD;
    DeviceName: array [0..32 - 1] of CHAR;
    DeviceString: array [0..128 - 1] of CHAR;
    StateFlags: DWORD;
    DeviceID: array [0..128 - 1] of CHAR;
    DeviceKey: array [0..128 - 1] of CHAR;
  end;
  DISPLAY_DEVICEA = _DISPLAY_DEVICEA;
  LPDISPLAY_DEVICEA = ^DISPLAY_DEVICEA;
  PDISPLAY_DEVICEA = ^DISPLAY_DEVICEA;
  TDisplayDeviceA = _DISPLAY_DEVICEA;

  PDisplayDeviceW = ^TDisplayDeviceW;
  _DISPLAY_DEVICEW = record
    cb: DWORD;
    DeviceName: array [0..32 - 1] of WCHAR;
    DeviceString: array [0..128 - 1] of WCHAR;
    StateFlags: DWORD;
    DeviceID: array [0..128 - 1] of WCHAR;
    DeviceKey: array [0..128 - 1] of WCHAR;
  end;
  DISPLAY_DEVICEW = _DISPLAY_DEVICEW;
  LPDISPLAY_DEVICEW = ^DISPLAY_DEVICEW;
  PDISPLAY_DEVICEW = ^DISPLAY_DEVICEW;
  TDisplayDeviceW = _DISPLAY_DEVICEW;

{$IFDEF UNICODE}
  DISPLAY_DEVICE = DISPLAY_DEVICEW;
  PDISPLAY_DEVICE = PDISPLAY_DEVICEW;
  LPDISPLAY_DEVICE = LPDISPLAY_DEVICEW;
  TDisplayDevice = TDisplayDeviceW;
  PDisplayDevice = PDisplayDeviceW;
{$ELSE}
  DISPLAY_DEVICE = DISPLAY_DEVICEA;
  PDISPLAY_DEVICE = PDISPLAY_DEVICEA;
  LPDISPLAY_DEVICE = LPDISPLAY_DEVICEA;
  TDisplayDevice = TDisplayDeviceA;
  PDisplayDevice = PDisplayDeviceA;
{$ENDIF}

const
  DISPLAY_DEVICE_ATTACHED_TO_DESKTOP = $00000001;
  DISPLAY_DEVICE_MULTI_DRIVER        = $00000002;
  DISPLAY_DEVICE_PRIMARY_DEVICE      = $00000004;
  DISPLAY_DEVICE_MIRRORING_DRIVER    = $00000008;
  DISPLAY_DEVICE_VGA_COMPATIBLE      = $00000010;
  DISPLAY_DEVICE_REMOVABLE           = $00000020;
  DISPLAY_DEVICE_MODESPRUNED         = $08000000;
  DISPLAY_DEVICE_REMOTE              = $04000000;
  DISPLAY_DEVICE_DISCONNECT          = $02000000;

// Child device state

  DISPLAY_DEVICE_ACTIVE              = $00000001;
  DISPLAY_DEVICE_ATTACHED            = $00000002;

// GetRegionData/ExtCreateRegion

  RDH_RECTANGLES = 1;

type
  PRgnDataHeader = ^TRgnDataHeader;
  _RGNDATAHEADER = record
    dwSize: DWORD;
    iType: DWORD;
    nCount: DWORD;
    nRgnSize: DWORD;
    rcBound: RECT;
  end;
  RGNDATAHEADER = _RGNDATAHEADER;
  TRgnDataHeader = _RGNDATAHEADER;

  PRgnData = ^TRgnData;
  _RGNDATA = record
    rdh: RGNDATAHEADER;
    Buffer: array [0..0] of Char;
  end;
  RGNDATA = _RGNDATA;
  LPRGNDATA = ^RGNDATA;
  NPRGNDATA = ^RGNDATA;
  TRgnData = _RGNDATA;

// for GetRandomRgn

const
  SYSRGN = 4;

type
  PAbc = ^TAbc;
  _ABC = record
    abcA: Integer;
    abcB: UINT;
    abcC: Integer;
  end;
  ABC = _ABC;
  LPABC = ^ABC;
  NPABC = ^ABC;
  TAbc = _ABC;

  PAbcFloat = ^TAbcFloat;
  _ABCFLOAT = record
    abcfA: FLOAT;
    abcfB: FLOAT;
    abcfC: FLOAT;
  end;
  ABCFLOAT = _ABCFLOAT;
  LPABCFLOAT = ^ABCFLOAT;
  NPABCFLOAT = ^ABCFLOAT;
  TAbcFloat = _ABCFLOAT;

  POutlineTextMetricA = ^TOutlineTextMetricA;
  _OUTLINETEXTMETRICA = record
    otmSize: UINT;
    otmTextMetrics: TEXTMETRICA;
    otmFiller: BYTE;
    otmPanoseNumber: PANOSE;
    otmfsSelection: UINT;
    otmfsType: UINT;
    otmsCharSlopeRise: Integer;
    otmsCharSlopeRun: Integer;
    otmItalicAngle: Integer;
    otmEMSquare: UINT;
    otmAscent: Integer;
    otmDescent: Integer;
    otmLineGap: UINT;
    otmsCapEmHeight: UINT;
    otmsXHeight: UINT;
    otmrcFontBox: RECT;
    otmMacAscent: Integer;
    otmMacDescent: Integer;
    otmMacLineGap: UINT;
    otmusMinimumPPEM: UINT;
    otmptSubscriptSize: POINT;
    otmptSubscriptOffset: POINT;
    otmptSuperscriptSize: POINT;
    otmptSuperscriptOffset: POINT;
    otmsStrikeoutSize: UINT;
    otmsStrikeoutPosition: Integer;
    otmsUnderscoreSize: Integer;
    otmsUnderscorePosition: Integer;
    otmpFamilyName: PSTR;
    otmpFaceName: PSTR;
    otmpStyleName: PSTR;
    otmpFullName: PSTR;
  end;
  OUTLINETEXTMETRICA = _OUTLINETEXTMETRICA;
  LPOUTLINETEXTMETRICA = ^OUTLINETEXTMETRICA;
  NPOUTLINETEXTMETRICA = ^OUTLINETEXTMETRICA;
  TOutlineTextMetricA = _OUTLINETEXTMETRICA;

  POutlineTextMetricW = ^TOutlineTextMetricW;
  _OUTLINETEXTMETRICW = record
    otmSize: UINT;
    otmTextMetrics: TEXTMETRICW;
    otmFiller: BYTE;
    otmPanoseNumber: PANOSE;
    otmfsSelection: UINT;
    otmfsType: UINT;
    otmsCharSlopeRise: Integer;
    otmsCharSlopeRun: Integer;
    otmItalicAngle: Integer;
    otmEMSquare: UINT;
    otmAscent: Integer;
    otmDescent: Integer;
    otmLineGap: UINT;
    otmsCapEmHeight: UINT;
    otmsXHeight: UINT;
    otmrcFontBox: RECT;
    otmMacAscent: Integer;
    otmMacDescent: Integer;
    otmMacLineGap: UINT;
    otmusMinimumPPEM: UINT;
    otmptSubscriptSize: POINT;
    otmptSubscriptOffset: POINT;
    otmptSuperscriptSize: POINT;
    otmptSuperscriptOffset: POINT;
    otmsStrikeoutSize: UINT;
    otmsStrikeoutPosition: Integer;
    otmsUnderscoreSize: Integer;
    otmsUnderscorePosition: Integer;
    otmpFamilyName: PSTR;
    otmpFaceName: PSTR;
    otmpStyleName: PSTR;
    otmpFullName: PSTR;
  end;
  OUTLINETEXTMETRICW = _OUTLINETEXTMETRICW;
  LPOUTLINETEXTMETRICW = ^OUTLINETEXTMETRICW;
  NPOUTLINETEXTMETRICW = ^OUTLINETEXTMETRICW;
  TOutlineTextMetricW = _OUTLINETEXTMETRICW;

{$IFDEF UNICODE}
  OUTLINETEXTMETRIC = OUTLINETEXTMETRICW;
  POUTLINETEXTMETRIC = POUTLINETEXTMETRICW;
  NPOUTLINETEXTMETRIC = NPOUTLINETEXTMETRICW;
  LPOUTLINETEXTMETRIC = LPOUTLINETEXTMETRICW;
  TOutlineTextMetric = TOutlineTextMetricW;
{$ELSE}
  OUTLINETEXTMETRIC = OUTLINETEXTMETRICA;
  POUTLINETEXTMETRIC = POUTLINETEXTMETRICA;
  NPOUTLINETEXTMETRIC = NPOUTLINETEXTMETRICA;
  LPOUTLINETEXTMETRIC = LPOUTLINETEXTMETRICA;
  TOutlineTextMetric = TOutlineTextMetricA;
{$ENDIF}

  PPolytextA = ^TPolytextA;
  tagPOLYTEXTA = record
    x: Integer;
    y: Integer;
    n: UINT;
    lpstr: LPCSTR;
    uiFlags: UINT;
    rcl: RECT;
    pdx: PINT;
  end;
  POLYTEXTA = tagPOLYTEXTA;
  LPPOLYTEXTA = ^POLYTEXTA;
  NPPOLYTEXTA = ^POLYTEXTA;
  TPolytextA = POLYTEXTA;

  PPolytextW = ^TPolytextW;
  tagPOLYTEXTW = record
    x: Integer;
    y: Integer;
    n: UINT;
    lpstr: LPCWSTR;
    uiFlags: UINT;
    rcl: RECT;
    pdx: PINT;
  end;
  POLYTEXTW = tagPOLYTEXTW;
  LPPOLYTEXTW = ^POLYTEXTW;
  NPPOLYTEXTW = ^POLYTEXTW;
  TPolytextW = POLYTEXTW;

{$IFDEF UNICODE}
  POLYTEXT = POLYTEXTW;
  PPOLYTEXT = PPOLYTEXTW;
  NPPOLYTEXT = NPPOLYTEXTW;
  LPPOLYTEXT = LPPOLYTEXTW;
  TPolyText = TPolyTextW;
{$ELSE}
  POLYTEXT = POLYTEXTA;
  PPOLYTEXT = PPOLYTEXTA;
  NPPOLYTEXT = NPPOLYTEXTA;
  LPPOLYTEXT = LPPOLYTEXTA;
  TPolyText = TPolyTextA;
{$ENDIF}

  PFixed = ^TFixed;
  _FIXED = record
    fract: WORD;
    value: short;
  end;
  FIXED = _FIXED;
  TFixed = _FIXED;

  PMat2 = ^TMat2;
  _MAT2 = record
    eM11: FIXED;
    eM12: FIXED;
    eM21: FIXED;
    eM22: FIXED;
  end;
  MAT2 = _MAT2;
  LPMAT2 = ^MAT2;
  TMat2 = _MAT2;

  PGlyphMetrics = ^TGlyphMetrics;
  _GLYPHMETRICS = record
    gmBlackBoxX: UINT;
    gmBlackBoxY: UINT;
    gmptGlyphOrigin: POINT;
    gmCellIncX: short;
    gmCellIncY: short;
  end;
  GLYPHMETRICS = _GLYPHMETRICS;
  LPGLYPHMETRICS = ^GLYPHMETRICS;
  TGlyphMetrics = _GLYPHMETRICS;

//  GetGlyphOutline constants

const
  GGO_METRICS = 0;
  GGO_BITMAP  = 1;
  GGO_NATIVE  = 2;
  GGO_BEZIER  = 3;

  GGO_GRAY2_BITMAP = 4;
  GGO_GRAY4_BITMAP = 5;
  GGO_GRAY8_BITMAP = 6;
  GGO_GLYPH_INDEX  = $0080;

  GGO_UNHINTED = $0100;

  TT_POLYGON_TYPE = 24;

  TT_PRIM_LINE    = 1;
  TT_PRIM_QSPLINE = 2;
  TT_PRIM_CSPLINE = 3;

type
  PPointFx = ^TPointFx;
  tagPOINTFX = record
    x: FIXED;
    y: FIXED;
  end;
  POINTFX = tagPOINTFX;
  LPPOINTFX = ^POINTFX;
  TPointFx = POINTFX;

  PTtPolyCurve = ^TTtPolyCurve;
  tagTTPOLYCURVE = record
    wType: WORD;
    cpfx: WORD;
    apfx: array [0..0] of POINTFX;
  end;
  TTPOLYCURVE = tagTTPOLYCURVE;
  LPTTPOLYCURVE = ^TTPOLYCURVE;
  TTtPolyCurve = TTPOLYCURVE;

  PTtPolygonHeader = ^TTtPolygonHeader;
  tagTTPOLYGONHEADER = record
    cb: DWORD;
    dwType: DWORD;
    pfxStart: POINTFX;
  end;
  TTPOLYGONHEADER = tagTTPOLYGONHEADER;
  LPTTPOLYGONHEADER = ^TTPOLYGONHEADER;
  TTtPolygonHeader = TTPOLYGONHEADER;

const
  GCP_DBCS       = $0001;
  GCP_REORDER    = $0002;
  GCP_USEKERNING = $0008;
  GCP_GLYPHSHAPE = $0010;
  GCP_LIGATE     = $0020;
////#define GCP_GLYPHINDEXING  0x0080
  GCP_DIACRITIC = $0100;
  GCP_KASHIDA   = $0400;
  GCP_ERROR     = $8000;
  FLI_MASK      = $103B;

  GCP_JUSTIFY = $00010000;
////#define GCP_NODIACRITICS   0x00020000L
  FLI_GLYPHS          = $00040000;
  GCP_CLASSIN         = $00080000;
  GCP_MAXEXTENT       = $00100000;
  GCP_JUSTIFYIN       = $00200000;
  GCP_DISPLAYZWG      = $00400000;
  GCP_SYMSWAPOFF      = $00800000;
  GCP_NUMERICOVERRIDE = $01000000;
  GCP_NEUTRALOVERRIDE = $02000000;
  GCP_NUMERICSLATIN   = $04000000;
  GCP_NUMERICSLOCAL   = $08000000;

  GCPCLASS_LATIN                  = 1;
  GCPCLASS_HEBREW                 = 2;
  GCPCLASS_ARABIC                 = 2;
  GCPCLASS_NEUTRAL                = 3;
  GCPCLASS_LOCALNUMBER            = 4;
  GCPCLASS_LATINNUMBER            = 5;
  GCPCLASS_LATINNUMERICTERMINATOR = 6;
  GCPCLASS_LATINNUMERICSEPARATOR  = 7;
  GCPCLASS_NUMERICSEPARATOR       = 8;
  GCPCLASS_PREBOUNDLTR            = $80;
  GCPCLASS_PREBOUNDRTL            = $40;
  GCPCLASS_POSTBOUNDLTR           = $20;
  GCPCLASS_POSTBOUNDRTL           = $10;

  GCPGLYPH_LINKBEFORE = $8000;
  GCPGLYPH_LINKAFTER  = $4000;

type
  PGcpResultsA = ^TGcpResultsA;
  tagGCP_RESULTSA = record
    lStructSize: DWORD;
    lpOutString: LPSTR;
    lpOrder: LPUINT;
    lpDx: PINT;
    lpCaretPos: PINT;
    lpClass: LPSTR;
    lpGlyphs: LPWSTR;
    nGlyphs: UINT;
    nMaxFit: Integer;
  end;
  GCP_RESULTSA = tagGCP_RESULTSA;
  LPGCP_RESULTSA = ^GCP_RESULTSA;
  TGcpResultsA = GCP_RESULTSA;

  PGcpResultsW = ^TGcpResultsW;
  tagGCP_RESULTSW = record
    lStructSize: DWORD;
    lpOutString: LPWSTR;
    lpOrder: LPUINT;
    lpDx: PINT;
    lpCaretPos: PINT;
    lpClass: LPSTR;
    lpGlyphs: LPWSTR;
    nGlyphs: UINT;
    nMaxFit: Integer;
  end;
  GCP_RESULTSW = tagGCP_RESULTSW;
  LPGCP_RESULTSW = ^GCP_RESULTSW;
  TGcpResultsW = GCP_RESULTSW;

{$IFDEF UNICODE}
  GCP_RESULTS = GCP_RESULTSW;
  LPGCP_RESULTS = LPGCP_RESULTSW;
  TGcpResults = TGcpResultsW;
  PGcpResults = PGcpResultsW;
{$ELSE}
  GCP_RESULTS = GCP_RESULTSA;
  LPGCP_RESULTS = LPGCP_RESULTSA;
  TGcpResults = TGcpResultsA;
  PGcpResults = PGcpResultsA;
{$ENDIF}

  PRasterizerStatus = ^TRasterizerStatus;
  _RASTERIZER_STATUS = record
    nSize: short;
    wFlags: short;
    nLanguageID: short;
  end;
  RASTERIZER_STATUS = _RASTERIZER_STATUS;
  LPRASTERIZER_STATUS = ^RASTERIZER_STATUS;
  TRasterizerStatus = _RASTERIZER_STATUS;

// bits defined in wFlags of RASTERIZER_STATUS

const
  TT_AVAILABLE = $0001;
  TT_ENABLED   = $0002;

// Pixel format descriptor

type
  PPixelFormatDescriptor = ^TPixelFormatDescriptor;
  tagPIXELFORMATDESCRIPTOR = record
    nSize: WORD;
    nVersion: WORD;
    dwFlags: DWORD;
    iPixelType: BYTE;
    cColorBits: BYTE;
    cRedBits: BYTE;
    cRedShift: BYTE;
    cGreenBits: BYTE;
    cGreenShift: BYTE;
    cBlueBits: BYTE;
    cBlueShift: BYTE;
    cAlphaBits: BYTE;
    cAlphaShift: BYTE;
    cAccumBits: BYTE;
    cAccumRedBits: BYTE;
    cAccumGreenBits: BYTE;
    cAccumBlueBits: BYTE;
    cAccumAlphaBits: BYTE;
    cDepthBits: BYTE;
    cStencilBits: BYTE;
    cAuxBuffers: BYTE;
    iLayerType: BYTE;
    bReserved: BYTE;
    dwLayerMask: DWORD;
    dwVisibleMask: DWORD;
    dwDamageMask: DWORD;
  end;
  PIXELFORMATDESCRIPTOR = tagPIXELFORMATDESCRIPTOR;
  LPPIXELFORMATDESCRIPTOR = ^PIXELFORMATDESCRIPTOR;
  TPixelFormatDescriptor = PIXELFORMATDESCRIPTOR;

// pixel types

const
  PFD_TYPE_RGBA       = 0;
  PFD_TYPE_COLORINDEX = 1;

// layer types

  PFD_MAIN_PLANE     = 0;
  PFD_OVERLAY_PLANE  = 1;
  PFD_UNDERLAY_PLANE = DWORD(-1);

// PIXELFORMATDESCRIPTOR flags

  PFD_DOUBLEBUFFER        = $00000001;
  PFD_STEREO              = $00000002;
  PFD_DRAW_TO_WINDOW      = $00000004;
  PFD_DRAW_TO_BITMAP      = $00000008;
  PFD_SUPPORT_GDI         = $00000010;
  PFD_SUPPORT_OPENGL      = $00000020;
  PFD_GENERIC_FORMAT      = $00000040;
  PFD_NEED_PALETTE        = $00000080;
  PFD_NEED_SYSTEM_PALETTE = $00000100;
  PFD_SWAP_EXCHANGE       = $00000200;
  PFD_SWAP_COPY           = $00000400;
  PFD_SWAP_LAYER_BUFFERS  = $00000800;
  PFD_GENERIC_ACCELERATED = $00001000;
  PFD_SUPPORT_DIRECTDRAW  = $00002000;

// PIXELFORMATDESCRIPTOR flags for use in ChoosePixelFormat only

  PFD_DEPTH_DONTCARE        = DWORD($20000000);
  PFD_DOUBLEBUFFER_DONTCARE = DWORD($40000000);
  PFD_STEREO_DONTCARE       = DWORD($80000000);

type
  OLDFONTENUMPROCA = function (lpelf: LPLOGFONTA; lpntm: LPTEXTMETRICA; FontType: DWORD; lParam: LPARAM): Integer; stdcall;
  OLDFONTENUMPROCW = function (lpelf: LPLOGFONTW; lpntm: LPTEXTMETRICW; FontType: DWORD; lParam: LPARAM): Integer; stdcall;
  {$IFDEF UNICODE}
  OLDFONTENUMPROC = function (lpelf: LPLOGFONTW; lpntm: LPTEXTMETRICW; FontType: DWORD; lParam: LPARAM): Integer; stdcall;
  {$ELSE}
  OLDFONTENUMPROC = function (lpelf: LPLOGFONTA; lpntm: LPTEXTMETRICA; FontType: DWORD; lParam: LPARAM): Integer; stdcall;
  {$ENDIF}

  FONTENUMPROCA = OLDFONTENUMPROCA;
  FONTENUMPROCW = OLDFONTENUMPROCW;
  {$IFDEF UNICODE}
  FONTENUMPROC = FONTENUMPROCW;
  {$ELSE}
  FONTENUMPROC = FONTENUMPROCA;
  {$ENDIF}

  GOBJENUMPROC = function(lpLogObject: LPVOID; lpData: LPARAM): Integer; stdcall;
  LINEDDAPROC = procedure (X, Y: Integer; lpData: LPARAM); stdcall;

function AddFontResourceA(lpszFileName: LPCSTR): Integer; stdcall;
function AddFontResourceW(lpszFileName: LPCWSTR): Integer; stdcall;

{$IFDEF UNICODE}
function AddFontResource(lpszFileName: LPCWSTR): Integer; stdcall;
{$ELSE}
function AddFontResource(lpszFileName: LPCSTR): Integer; stdcall;
{$ENDIF}

function AnimatePalette(hPal: HPALETTE; iStartIndex: UINT; cEntries: UINT; ppe: PPALETTEENTRY): BOOL; stdcall;
function Arc(hdc: HDC; nLeftRect, nTopRect, nRightRect, nBottomRect, nxStartArc, nyStartArc, nXEndArc, nYEndArc: Integer): BOOL; stdcall;
function BitBlt(hdcDEst: HDC; nXDest, nYDest, nWidth, nHeight: Integer; hdcSrc: HDC; nXSrc, nYSrc: Integer; dwRop: DWORD): BOOL; stdcall;
function CancelDC(hdc: HDC): BOOL; stdcall;
function Chord(hdc: HDC; nLeftRect, nTopRect, nRightRect, nBottomRect, nXRadial1, nYRadial1, nXRadial2, nYRadial2: Integer): BOOL; stdcall;
function ChoosePixelFormat(hdc: HDC; const ppfd: PIXELFORMATDESCRIPTOR): Integer; stdcall;
function CloseMetaFile(hdc: HDC): HMETAFILE; stdcall;
function CombineRgn(hrgnDest, hrgnSrc1, hrgnSrc2: HRGN; fnCombineMode: Integer): Integer; stdcall;
function CopyMetaFileA(hmfSrc: HMETAFILE; lpszFile: LPCSTR): HMETAFILE; stdcall;
function CopyMetaFileW(hmfSrc: HMETAFILE; lpszFile: LPCWSTR): HMETAFILE; stdcall;
{$IFDEF UNICODE}
function CopyMetaFile(hmfSrc: HMETAFILE; lpszFile: LPCWSTR): HMETAFILE; stdcall;
{$ELSE}
function CopyMetaFile(hmfSrc: HMETAFILE; lpszFile: LPCSTR): HMETAFILE; stdcall;
{$ENDIF}
function CreateBitmap(nWidth, nHeight: Integer; Cplanes, cBitsPerPel: UINT; lpvBits: PVOID): HBITMAP; stdcall;
function CreateBitmapIndirect(const lpbm: BITMAP): HBITMAP; stdcall;
function CreateBrushIndirect(const lplb: LOGBRUSH): HBRUSH; stdcall;
function CreateCompatibleBitmap(hdc: HDC; nWidth, nHeight: Integer): HBITMAP; stdcall;
function CreateDiscardableBitmap(hdc: HDC; nWidth, nHeight: Integer): HBITMAP; stdcall;
function CreateCompatibleDC(hdc: HDC): HDC; stdcall;
function CreateDCA(lpszDriver, lpszDevice, lpszOutput: LPCSTR; lpInitData: LPDEVMODEA): HDC; stdcall;
function CreateDCW(lpszDriver, lpszDevice, lpszOutput: LPCWSTR; lpInitData: LPDEVMODEW): HDC; stdcall;
{$IFDEF UNICODE}
function CreateDC(lpszDriver, lpszDevice, lpszOutput: LPCWSTR; lpInitData: LPDEVMODEW): HDC; stdcall;
{$ELSE}
function CreateDC(lpszDriver, lpszDevice, lpszOutput: LPCSTR; lpInitData: LPDEVMODEA): HDC; stdcall;
{$ENDIF}
function CreateDIBitmap(hdc: HDC; const lpbmih: BITMAPINFOHEADER; fdwInit: DWORD; lpbInit: PVOID; const lpbmi: BITMAPINFO; fuUsage: UINT): HBITMAP; stdcall;
function CreateDIBPatternBrush(hglbDIBPacked: HGLOBAL; fuColorSpec: UINT): HBRUSH; stdcall;
function CreateDIBPatternBrushPt(lpPackedDIB: PVOID; iUsage: UINT): HBRUSH; stdcall;
function CreateEllipticRgn(nLeftRect, nTopRect, nRightRect, nBottomRect: Integer): HRGN; stdcall;
function CreateEllipticRgnIndirect(const lprc: RECT): HRGN; stdcall;
function CreateFontIndirectA(const lplf: LOGFONTA): HFONT; stdcall;
function CreateFontIndirectW(const lplf: LOGFONTW): HFONT; stdcall;
{$IFDEF UNICODE}
function CreateFontIndirect(const lplf: LOGFONTW): HFONT; stdcall;
{$ELSE}
function CreateFontIndirect(const lplf: LOGFONTA): HFONT; stdcall;
{$ENDIF}
function CreateFontA(nHeight, nWidth, nEscapement, nOrientation, fnWeight: Integer; fdwItalic, fdwUnderline, fdwStrikeOut, fdwCharSet, fdwOutputPrecision, fdwClipPrecision, fdwQuality, fdwPitchAndFamily: DWORD; lpszFace: LPCSTR): HFONT; stdcall;
function CreateFontW(nHeight, nWidth, nEscapement, nOrientation, fnWeight: Integer; fdwItalic, fdwUnderline, fdwStrikeOut, fdwCharSet, fdwOutputPrecision, fdwClipPrecision, fdwQuality, fdwPitchAndFamily: DWORD; lpszFace: LPCWSTR): HFONT; stdcall;
{$IFDEF UNICODE}
function CreateFont(nHeight, nWidth, nEscapement, nOrientation, fnWeight: Integer; fdwItalic, fdwUnderline, fdwStrikeOut, fdwCharSet, fdwOutputPrecision, fdwClipPrecision, fdwQuality, fdwPitchAndFamily: DWORD; lpszFace: LPCWSTR): HFONT; stdcall;
{$ELSE}
function CreateFont(nHeight, nWidth, nEscapement, nOrientation, fnWeight: Integer; fdwItalic, fdwUnderline, fdwStrikeOut, fdwCharSet, fdwOutputPrecision, fdwClipPrecision, fdwQuality, fdwPitchAndFamily: DWORD; lpszFace: LPCSTR): HFONT; stdcall;
{$ENDIF}
function CreateHatchBrush(fnStyle: Integer; clrref: COLORREF): HBRUSH; stdcall;
function CreateICA(lpszDriver, lpszDevice, lpszOutput: LPCSTR; lpdvmInit: LPDEVMODEA): HDC; stdcall;
function CreateICW(lpszDriver, lpszDevice, lpszOutput: LPCWSTR; lpdvmInit: LPDEVMODEW): HDC; stdcall;
{$IFDEF UNICODE}
function CreateIC(lpszDriver, lpszDevice, lpszOutput: LPCWSTR; lpdvmInit: LPDEVMODEW): HDC; stdcall;
{$ELSE}
function CreateIC(lpszDriver, lpszDevice, lpszOutput: LPCSTR; lpdvmInit: LPDEVMODEA): HDC; stdcall;
{$ENDIF}
function CreateMetaFileA(lpszFile: LPCSTR): HDC; stdcall;
function CreateMetaFileW(lpszFile: LPCWSTR): HDC; stdcall;
{$IFDEF UNICODE}
function CreateMetaFile(lpszFile: LPCWSTR): HDC; stdcall;
{$ELSE}
function CreateMetaFile(lpszFile: LPCSTR): HDC; stdcall;
{$ENDIF}
function CreatePalette(const lplgpl: LOGPALETTE): HPALETTE; stdcall;
function CreatePen(fnPenStyle, nWidth: Integer; crColor: COLORREF): HPEN; stdcall;
function CreatePenIndirect(const lplgpn: LOGPEN): HPEN; stdcall;
function CreatePolyPolygonRgn(lppt: LPPOINT; lpPolyCounts: LPINT; nCount, fnPolyFillMode: Integer): HRGN; stdcall;
function CreatePatternBrush(hbmp: HBITMAP): HBRUSH; stdcall;
function CreateRectRgn(nLeftRect, nTopRect, nRightRect, nBottomRect: Integer): HRGN; stdcall;
function CreateRectRgnIndirect(const lprc: RECT): HRGN; stdcall;
function CreateRoundRectRgn(nLeftRect, nTopRect, nRightRect, nBottomRect, nWidthEllipse, nHeightEllipse: Integer): HRGN; stdcall;
function CreateScalableFontResourceA(fdwHidden: DWORD; lpszFontRes, lpszFontFile, lpszCurrentPath: LPCSTR): BOOL; stdcall;
function CreateScalableFontResourceW(fdwHidden: DWORD; lpszFontRes, lpszFontFile, lpszCurrentPath: LPCWSTR): BOOL; stdcall;
{$IFDEF UNICODE}
function CreateScalableFontResource(fdwHidden: DWORD; lpszFontRes, lpszFontFile, lpszCurrentPath: LPCWSTR): BOOL; stdcall;
{$ELSE}
function CreateScalableFontResource(fdwHidden: DWORD; lpszFontRes, lpszFontFile, lpszCurrentPath: LPCSTR): BOOL; stdcall;
{$ENDIF}
function CreateSolidBrush(crColor: COLORREF): HBRUSH; stdcall;
function DeleteDC(hdc: HDC): BOOL; stdcall;
function DeleteMetaFile(hmf: HMETAFILE): BOOL; stdcall;
function DeleteObject(hObject: HGDIOBJ): BOOL; stdcall;
function DescribePixelFormat(hdc: HDC; iPixelFormat: Integer; nBytes: UINT; ppfd: LPPIXELFORMATDESCRIPTOR): Integer; stdcall;

// mode selections for the device mode function

const
  DM_UPDATE = 1;
  DM_COPY   = 2;
  DM_PROMPT = 4;
  DM_MODIFY = 8;

  DM_IN_BUFFER   = DM_MODIFY;
  DM_IN_PROMPT   = DM_PROMPT;
  DM_OUT_BUFFER  = DM_COPY;
  DM_OUT_DEFAULT = DM_UPDATE;

// device capabilities indices

  DC_FIELDS            = 1;
  DC_PAPERS            = 2;
  DC_PAPERSIZE         = 3;
  DC_MINEXTENT         = 4;
  DC_MAXEXTENT         = 5;
  DC_BINS              = 6;
  DC_DUPLEX            = 7;
  DC_SIZE              = 8;
  DC_EXTRA             = 9;
  DC_VERSION           = 10;
  DC_DRIVER            = 11;
  DC_BINNAMES          = 12;
  DC_ENUMRESOLUTIONS   = 13;
  DC_FILEDEPENDENCIES  = 14;
  DC_TRUETYPE          = 15;
  DC_PAPERNAMES        = 16;
  DC_ORIENTATION       = 17;
  DC_COPIES            = 18;
  DC_BINADJUST         = 19;
  DC_EMF_COMPLIANT     = 20;
  DC_DATATYPE_PRODUCED = 21;
  DC_COLLATE           = 22;
  DC_MANUFACTURER      = 23;
  DC_MODEL             = 24;

  DC_PERSONALITY    = 25;
  DC_PRINTRATE      = 26;
  DC_PRINTRATEUNIT  = 27;
  PRINTRATEUNIT_PPM = 1;
  PRINTRATEUNIT_CPS = 2;
  PRINTRATEUNIT_LPM = 3;
  PRINTRATEUNIT_IPM = 4;
  DC_PRINTERMEM     = 28;
  DC_MEDIAREADY     = 29;
  DC_STAPLE         = 30;
  DC_PRINTRATEPPM   = 31;
  DC_COLORDEVICE    = 32;
  DC_NUP            = 33;
  DC_MEDIATYPENAMES = 34;
  DC_MEDIATYPES     = 35;

// bit fields of the return value (DWORD) for DC_TRUETYPE

  DCTT_BITMAP           = $0000001;
  DCTT_DOWNLOAD         = $0000002;
  DCTT_SUBDEV           = $0000004;
  DCTT_DOWNLOAD_OUTLINE = $0000008;

// return values for DC_BINADJUST

  DCBA_FACEUPNONE     = $0000;
  DCBA_FACEUPCENTER   = $0001;
  DCBA_FACEUPLEFT     = $0002;
  DCBA_FACEUPRIGHT    = $0003;
  DCBA_FACEDOWNNONE   = $0100;
  DCBA_FACEDOWNCENTER = $0101;
  DCBA_FACEDOWNLEFT   = $0102;
  DCBA_FACEDOWNRIGHT  = $0103;

function DeviceCapabilitiesA(pDevice, pPort: LPCSTR; fwCapability: WORD; pOutput: LPSTR; pDevMode: LPDEVMODEA): Integer; stdcall;
function DeviceCapabilitiesW(pDevice, pPort: LPCWSTR; fwCapability: WORD; pOutput: LPWSTR; pDevMode: LPDEVMODEW): Integer; stdcall;
{$IFDEF UNICODE}
function DeviceCapabilities(pDevice, pPort: LPCWSTR; fwCapability: WORD; pOutput: LPWSTR; pDevMode: LPDEVMODEW): Integer; stdcall;
{$ELSE}
function DeviceCapabilities(pDevice, pPort: LPCSTR; fwCapability: WORD; pOutput: LPSTR; pDevMode: LPDEVMODEA): Integer; stdcall;
{$ENDIF}
function DrawEscape(hdc: HDC; nEscape, cbInput: Integer; lpszInData: LPCSTR): Integer; stdcall;
function Ellipse(hdc: HDC; nLeftRect, nTopRect, nRightRect, nBottomRect: Integer): BOOL; stdcall;
function EnumFontFamiliesExA(hdc: HDC; lpLogFont: LPLOGFONTA; lpEnumFontFamExProc: FONTENUMPROCA; lParam: LPARAM; dwFlags: DWORD): Integer; stdcall;
function EnumFontFamiliesExW(hdc: HDC; lpLogFont: LPLOGFONTW; lpEnumFontFamExProc: FONTENUMPROCW; lParam: LPARAM; dwFlags: DWORD): Integer; stdcall;
{$IFDEF UNICODE}
function EnumFontFamiliesEx(hdc: HDC; lpLogFont: LPLOGFONTW; lpEnumFontFamExProc: FONTENUMPROCW; lParam: LPARAM; dwFlags: DWORD): Integer; stdcall;
{$ELSE}
function EnumFontFamiliesEx(hdc: HDC; lpLogFont: LPLOGFONTA; lpEnumFontFamExProc: FONTENUMPROCA; lParam: LPARAM; dwFlags: DWORD): Integer; stdcall;
{$ENDIF}

function EnumFontFamiliesA(hdc: HDC; lpszFamily: LPCSTR; lpEnumFontFamProc: FONTENUMPROCA; lParam: LPARAM): Integer; stdcall;
function EnumFontFamiliesW(hdc: HDC; lpszFamily: LPCWSTR; lpEnumFontFamProc: FONTENUMPROCW; lParam: LPARAM): Integer; stdcall;
{$IFDEF UNICODE}
function EnumFontFamilies(hdc: HDC; lpszFamily: LPCWSTR; lpEnumFontFamProc: FONTENUMPROCW; lParam: LPARAM): Integer; stdcall;
{$ELSE}
function EnumFontFamilies(hdc: HDC; lpszFamily: LPCSTR; lpEnumFontFamProc: FONTENUMPROCA; lParam: LPARAM): Integer; stdcall;
{$ENDIF}

function EnumFontsA(hdc: HDC; lpFaceName: LPCSTR; lpFontFunc: FONTENUMPROCA; lParam: LPARAM): Integer; stdcall;
function EnumFontsW(hdc: HDC; lpFaceName: LPCWSTR; lpFontFunc: FONTENUMPROCW; lParam: LPARAM): Integer; stdcall;
{$IFDEF UNICODE}
function EnumFonts(hdc: HDC; lpFaceName: LPCWSTR; lpFontFunc: FONTENUMPROCW; lParam: LPARAM): Integer; stdcall;
{$ELSE}
function EnumFonts(hdc: HDC; lpFaceName: LPCSTR; lpFontFunc: FONTENUMPROCA; lParam: LPARAM): Integer; stdcall;
{$ENDIF}

function EnumObjects(hdc: HDC; mObjectType: Integer; lpObjectFunc: GOBJENUMPROC; lParam: LPARAM): Integer; stdcall;

function EqualRgn(hSrcRgn1, hSrcRgn2: HRGN): BOOL; stdcall;
function Escape(hdc: HDC; nEscape, cbInput: Integer; lpvInData: LPCSTR; lpvOutData: LPVOID): Integer; stdcall;
function ExtEscape(hdc: HDC; nEscape, cbInput: Integer; lpszInData: LPCSTR; cbOutput: Integer; lpszOutData: LPSTR): Integer; stdcall;
function ExcludeClipRect(hdc: HDC; nLeftRect, nTopRect, nRightRect, nBottomRect: Integer): Integer; stdcall;
function ExtCreateRegion(lpXForm: LPXFORM; nCount: DWORD; lpRgnData: LPRGNDATA): HRGN; stdcall;
function ExtFloodFill(hdc: HDC; nXStart, nYStart: Integer; crColor: COLORREF; fuFillType: UINT): BOOL; stdcall;
function FillRgn(hdc: HDC; hrgn: HRGN; hbr: HBRUSH): BOOL; stdcall;
function FloodFill(hdc: HDC; nXStart, nYStart: Integer; crFill: COLORREF): BOOL; stdcall;
function FrameRgn(hdc: HDC; hrgn: HRGN; hbr: HBRUSH; nWidth, nHeight: Integer): BOOL; stdcall;
function GetROP2(hdc: HDC): Integer; stdcall;
function GetAspectRatioFilterEx(hdc: HDC; var lpAspectRatio: TSize): BOOL; stdcall;
function GetBkColor(hdc: HDC): COLORREF; stdcall;

function GetDCBrushColor(hdc: HDC): COLORREF; stdcall;
function GetDCPenColor(hdc: HDC): COLORREF; stdcall;

function GetBkMode(hdc: HDC): Integer; stdcall;
function GetBitmapBits(hbmp: HBITMAP; cbBuffer: LONG; lpvBits: LPVOID): LONG; stdcall;
function GetBitmapDimensionEx(hBitmap: HBITMAP; var lpDimension: TSize): BOOL; stdcall;
function GetBoundsRect(hdc: HDC; var lprcBounds: RECT; flags: UINT): UINT; stdcall;

function GetBrushOrgEx(hdc: HDC; var lppt: POINT): BOOL; stdcall;

function GetCharWidthA(hdc: HDC; iFirstChar, iLastChar: UINT; lpBuffer: LPINT): BOOL; stdcall;
function GetCharWidthW(hdc: HDC; iFirstChar, iLastChar: UINT; lpBuffer: LPINT): BOOL; stdcall;
{$IFDEF UNICODE}
function GetCharWidth(hdc: HDC; iFirstChar, iLastChar: UINT; lpBuffer: LPINT): BOOL; stdcall;
{$ELSE}
function GetCharWidth(hdc: HDC; iFirstChar, iLastChar: UINT; lpBuffer: LPINT): BOOL; stdcall;
{$ENDIF}
function GetCharWidth32A(hdc: HDC; iFirstChar, iLastChar: UINT; lpBuffer: LPINT): BOOL; stdcall;
function GetCharWidth32W(hdc: HDC; iFirstChar, iLastChar: UINT; lpBuffer: LPINT): BOOL; stdcall;
{$IFDEF UNICODE}
function GetCharWidth32(hdc: HDC; iFirstChar, iLastChar: UINT; lpBuffer: LPINT): BOOL; stdcall;
{$ELSE}
function GetCharWidth32(hdc: HDC; iFirstChar, iLastChar: UINT; lpBuffer: LPINT): BOOL; stdcall;
{$ENDIF}
function GetCharWidthFloatA(hdc: HDC; iFirstChar, iLastChar: UINT; pxBuffer: PFLOAT): BOOL; stdcall;
function GetCharWidthFloatW(hdc: HDC; iFirstChar, iLastChar: UINT; pxBuffer: PFLOAT): BOOL; stdcall;
{$IFDEF UNICODE}
function GetCharWidthFloat(hdc: HDC; iFirstChar, iLastChar: UINT; pxBuffer: PFLOAT): BOOL; stdcall;
{$ELSE}
function GetCharWidthFloat(hdc: HDC; iFirstChar, iLastChar: UINT; pxBuffer: PFLOAT): BOOL; stdcall;
{$ENDIF}
function GetCharABCWidthsA(hdc: HDC; uFirstChar, uLastChar: UINT; lpAbc: LPABC): BOOL; stdcall;
function GetCharABCWidthsW(hdc: HDC; uFirstChar, uLastChar: UINT; lpAbc: LPABC): BOOL; stdcall;
{$IFDEF UNICODE}
function GetCharABCWidths(hdc: HDC; uFirstChar, uLastChar: UINT; lpAbc: LPABC): BOOL; stdcall;
{$ELSE}
function GetCharABCWidths(hdc: HDC; uFirstChar, uLastChar: UINT; lpAbc: LPABC): BOOL; stdcall;
{$ENDIF}
function GetCharABCWidthsFloatA(hdc: HDC; iFirstChar, iLastChar: UINT; lpAbcF: LPABCFLOAT): BOOL; stdcall;
function GetCharABCWidthsFloatW(hdc: HDC; iFirstChar, iLastChar: UINT; lpAbcF: LPABCFLOAT): BOOL; stdcall;
{$IFDEF UNICODE}
function GetCharABCWidthsFloat(hdc: HDC; iFirstChar, iLastChar: UINT; lpAbcF: LPABCFLOAT): BOOL; stdcall;
{$ELSE}
function GetCharABCWidthsFloat(hdc: HDC; iFirstChar, iLastChar: UINT; lpAbcF: LPABCFLOAT): BOOL; stdcall;
{$ENDIF}

function GetClipBox(hdc: HDC; var lprc: RECT): Integer; stdcall;
function GetClipRgn(hdc: HDC; hrgn: HRGN): Integer; stdcall;
function GetMetaRgn(hdc: HDC; hrgn: HRGN): Integer; stdcall;
function GetCurrentObject(hdc: HDC; uObjectType: UINT): HGDIOBJ; stdcall;
function GetCurrentPositionEx(hdc: HDC; var lpPoint: POINT): BOOL; stdcall;
function GetDeviceCaps(hdc: HDC; nIndex: Integer): Integer; stdcall;
function GetDIBits(hdc: HDC; hbmp: HBITMAP; uStartScan, cScanLines: UINT; lpvBits: LPVOID; var lpbi: BITMAPINFO; uUsage: UINT): Integer; stdcall;
function GetFontData(hdc: HDC; dwTable, dwOffset: DWORD; lpvBuffer: LPVOID; cbData: DWORD): DWORD; stdcall;
function GetGlyphOutlineA(hdc: HDC; uChar, uFormat: UINT; var lpgm: GLYPHMETRICS; cbBuffer: DWORD; lpvBuffer: LPVOID; const lpMat2: MAT2): DWORD; stdcall;
function GetGlyphOutlineW(hdc: HDC; uChar, uFormat: UINT; var lpgm: GLYPHMETRICS; cbBuffer: DWORD; lpvBuffer: LPVOID; const lpMat2: MAT2): DWORD; stdcall;
{$IFDEF UNICODE}
function GetGlyphOutline(hdc: HDC; uChar, uFormat: UINT; var lpgm: GLYPHMETRICS; cbBuffer: DWORD; lpvBuffer: LPVOID; const lpMat2: MAT2): DWORD; stdcall;
{$ELSE}
function GetGlyphOutline(hdc: HDC; uChar, uFormat: UINT; var lpgm: GLYPHMETRICS; cbBuffer: DWORD; lpvBuffer: LPVOID; const lpMat2: MAT2): DWORD; stdcall;
{$ENDIF}
function GetGraphicsMode(hdc: HDC): Integer; stdcall;
function GetMapMode(hdc: HDC): Integer; stdcall;
function GetMetaFileBitsEx(hmf: HMETAFILE; nSize: UINT; lpvData: LPVOID): UINT; stdcall;
function GetMetaFileA(lpszMetaFile: LPCSTR): HMETAFILE; stdcall;
function GetMetaFileW(lpszMetaFile: LPCWSTR): HMETAFILE; stdcall;
{$IFDEF UNICODE}
function GetMetaFile(lpszMetaFile: LPCWSTR): HMETAFILE; stdcall;
{$ELSE}
function GetMetaFile(lpszMetaFile: LPCSTR): HMETAFILE; stdcall;
{$ENDIF}
function GetNearestColor(hdc: HDC; crColor: COLORREF): COLORREF; stdcall;
function GetNearestPaletteIndex(hPal: HPALETTE; crColor: COLORREF): UINT; stdcall;
function GetObjectType(h: HGDIOBJ): DWORD; stdcall;

function GetOutlineTextMetricsA(hdc: HDC; cbData: UINT; lpOTM: LPOUTLINETEXTMETRICA): UINT; stdcall;
function GetOutlineTextMetricsW(hdc: HDC; cbData: UINT; lpOTM: LPOUTLINETEXTMETRICW): UINT; stdcall;
{$IFDEF UNICODE}
function GetOutlineTextMetrics(hdc: HDC; cbData: UINT; lpOTM: LPOUTLINETEXTMETRICW): UINT; stdcall;
{$ELSE}
function GetOutlineTextMetrics(hdc: HDC; cbData: UINT; lpOTM: LPOUTLINETEXTMETRICA): UINT; stdcall;
{$ENDIF}
function GetPaletteEntries(hPal: HPALETTE; iStartIndex, nEntries: UINT; lppe: LPPALETTEENTRY): UINT; stdcall;
function GetPixel(hdc: HDC; nXPos, nYPos: Integer): COLORREF; stdcall;
function GetPixelFormat(hdc: HDC): Integer; stdcall;
function GetPolyFillMode(hdc: HDC): Integer; stdcall;
function GetRasterizerCaps(var lprs: RASTERIZER_STATUS; cb: UINT): BOOL; stdcall;
function GetRandomRgn(hdc: HDC; hrgn: HRGN; iNum: Integer): Integer; stdcall;
function GetRegionData(hrgn: HRGN; dwCount: DWORD; lpRgnData: LPRGNDATA): DWORD; stdcall;
function GetRgnBox(hrgn: HRGN; var lprc: RECT): Integer; stdcall;
function GetStockObject(fnObject: Integer): HGDIOBJ; stdcall;
function GetStretchBltMode(hdc: HDC): Integer; stdcall;
function GetSystemPaletteEntries(hdc: HDC; iStartIndex, nEntries: UINT; lppe: LPPALETTEENTRY): UINT; stdcall;
function GetSystemPaletteUse(hdc: HDC): UINT; stdcall;
function GetTextCharacterExtra(hdc: HDC): Integer; stdcall;
function GetTextAlign(hdc: HDC): UINT; stdcall;
function GetTextColor(hdc: HDC): COLORREF; stdcall;

function GetTextExtentPointA(hdc: HDC; lpString: LPCSTR; cbString: Integer; var Size: TSize): BOOL; stdcall;
function GetTextExtentPointW(hdc: HDC; lpString: LPCWSTR; cbString: Integer; var Size: TSize): BOOL; stdcall;
{$IFDEF UNICODE}
function GetTextExtentPoint(hdc: HDC; lpString: LPCWSTR; cbString: Integer; var Size: TSize): BOOL; stdcall;
{$ELSE}
function GetTextExtentPoint(hdc: HDC; lpString: LPCSTR; cbString: Integer; var Size: TSize): BOOL; stdcall;
{$ENDIF}
function GetTextExtentPoint32A(hdc: HDC; lpString: LPCSTR; cbString: Integer; var Size: TSize): BOOL; stdcall;
function GetTextExtentPoint32W(hdc: HDC; lpString: LPCWSTR; cbString: Integer; var Size: TSize): BOOL; stdcall;
{$IFDEF UNICODE}
function GetTextExtentPoint32(hdc: HDC; lpString: LPCWSTR; cbString: Integer; var Size: TSize): BOOL; stdcall;
{$ELSE}
function GetTextExtentPoint32(hdc: HDC; lpString: LPCSTR; cbString: Integer; var Size: TSize): BOOL; stdcall;
{$ENDIF}
function GetTextExtentExPointA(hdc: HDC; lpszStr: LPCSTR; cchString, nMaxExtend: Integer; lpnFit, alpDx: LPINT; var lpSize: TSize): BOOL; stdcall;
function GetTextExtentExPointW(hdc: HDC; lpszStr: LPCWSTR; cchString, nMaxExtend: Integer; lpnFit, alpDx: LPINT; var lpSize: TSize): BOOL; stdcall;
{$IFDEF UNICODE}
function GetTextExtentExPoint(hdc: HDC; lpszStr: LPCWSTR; cchString, nMaxExtend: Integer; lpnFit, alpDx: LPINT; var lpSize: TSize): BOOL; stdcall;
{$ELSE}
function GetTextExtentExPoint(hdc: HDC; lpszStr: LPCSTR; cchString, nMaxExtend: Integer; lpnFit, alpDx: LPINT; var lpSize: TSize): BOOL; stdcall;
{$ENDIF}
function GetTextCharset(hdc: HDC): Integer; stdcall;
function GetTextCharsetInfo(hdc: HDC; lpSig: LPFONTSIGNATURE; dwFlags: DWORD): Integer; stdcall;
function TranslateCharsetInfo(lpSrc: LPDWORD; lpCs: LPCHARSETINFO; dwFlags: DWORD): BOOL; stdcall;
function GetFontLanguageInfo(hdc: HDC): DWORD; stdcall;
function GetCharacterPlacementA(hdc: HDC; lpString: LPCSTR; nCount, nMaxExtend: Integer; var lpResults: GCP_RESULTSA; dwFlags: DWORD): DWORD; stdcall;
function GetCharacterPlacementW(hdc: HDC; lpString: LPCWSTR; nCount, nMaxExtend: Integer; var lpResults: GCP_RESULTSW; dwFlags: DWORD): DWORD; stdcall;
{$IFDEF UNICODE}
function GetCharacterPlacement(hdc: HDC; lpString: LPCWSTR; nCount, nMaxExtend: Integer; var lpResults: GCP_RESULTSW; dwFlags: DWORD): DWORD; stdcall;
{$ELSE}
function GetCharacterPlacement(hdc: HDC; lpString: LPCSTR; nCount, nMaxExtend: Integer; var lpResults: GCP_RESULTSA; dwFlags: DWORD): DWORD; stdcall;
{$ENDIF}

type
  PWcRange = ^TWcRange;
  tagWCRANGE = record
    wcLow: WCHAR;
    cGlyphs: USHORT;
  end;
  WCRANGE = tagWCRANGE;
  LPWCRANGE = ^WCRANGE;
  TWcRange = WCRANGE;

  PGlyphSet = ^TGlyphSet;
  tagGLYPHSET = record
    cbThis: DWORD;
    flAccel: DWORD;
    cGlyphsSupported: DWORD;
    cRanges: DWORD;
    ranges: array [0..0] of WCRANGE;
  end;
  GLYPHSET = tagGLYPHSET;
  LPGLYPHSET = ^GLYPHSET;
  TGlyphSet = GLYPHSET;

// flAccel flags for the GLYPHSET structure above

const
  GS_8BIT_INDICES = $00000001;

// flags for GetGlyphIndices

  GGI_MARK_NONEXISTING_GLYPHS = $0001;

function GetFontUnicodeRanges(hdc: HDC; lpgs: LPGLYPHSET): DWORD; stdcall;

function GetGlyphIndicesA(hdc: HDC; lpstr: LPCSTR; c: Integer; pgi: LPWORD; fl: DWORD): DWORD; stdcall;
function GetGlyphIndicesW(hdc: HDC; lpstr: LPCWSTR; c: Integer; pgi: LPWORD; fl: DWORD): DWORD; stdcall;

{$IFDEF UNICODE}
function GetGlyphIndices(hdc: HDC; lpstr: LPCWSTR; c: Integer; pgi: LPWORD; fl: DWORD): DWORD; stdcall;
{$ELSE}
function GetGlyphIndices(hdc: HDC; lpstr: LPCSTR; c: Integer; pgi: LPWORD; fl: DWORD): DWORD; stdcall;
{$ENDIF}

function GetTextExtentPointI(hdc: HDC; pgiIn: LPWORD; cgi: Integer; lpSize: LPSIZE): BOOL; stdcall;
function GetTextExtentExPointI(hdc: HDC; pgiIn: LPWORD; cgi, nMaxExtend: Integer;
  lpnFit, alpDx: LPINT; lpSize: LPSIZE): BOOL; stdcall;
function GetCharWidthI(hdc: HDC; giFirst, cgi: UINT; pgi: LPWORD; lpBuffer: LPINT): BOOL; stdcall;
function GetCharABCWidthsI(hdc: HDC; giFirst, cgi: UINT; pgi: LPWORD; lpAbc: LPABC): BOOL; stdcall;

const
  STAMP_DESIGNVECTOR = ($8000000 + Ord('d') + (Ord('v') shl 8));
  STAMP_AXESLIST     = ($8000000 + Ord('a') + (Ord('l') shl 8));
  MM_MAX_NUMAXES     = 16;

type
  PDesignVector = ^TDesignVector;
  tagDESIGNVECTOR = record
    dvReserved: DWORD;
    dvNumAxes: DWORD;
    dvValues: array [0..MM_MAX_NUMAXES - 1] of LONG;
  end;
  DESIGNVECTOR = tagDESIGNVECTOR;
  LPDESIGNVECTOR = ^DESIGNVECTOR;
  TDesignVector = DESIGNVECTOR;

function AddFontResourceExA(lpszFilename: LPCSTR; fl: DWORD; pdv: PVOID): Integer; stdcall;
function AddFontResourceExW(lpszFilename: LPCWSTR; fl: DWORD; pdv: PVOID): Integer; stdcall;

{$IFDEF UNICODE}
function AddFontResourceEx(lpszFilename: LPCWSTR; fl: DWORD; pdv: PVOID): Integer; stdcall;
{$ELSE}
function AddFontResourceEx(lpszFilename: LPCSTR; fl: DWORD; pdv: PVOID): Integer; stdcall;
{$ENDIF}

function RemoveFontResourceExA(lpFilename: LPCSTR; fl: DWORD; pdv: PVOID): BOOL; stdcall;
function RemoveFontResourceExW(lpFilename: LPCWSTR; fl: DWORD; pdv: PVOID): BOOL; stdcall;

{$IFDEF UNICODE}
function RemoveFontResourceEx(lpFilename: LPCWSTR; fl: DWORD; pdv: PVOID): BOOL; stdcall;
{$ELSE}
function RemoveFontResourceEx(lpFilename: LPCSTR; fl: DWORD; pdv: PVOID): BOOL; stdcall;
{$ENDIF}

function AddFontMemResourceEx(pbFont: PVOID; cbFont: DWORD; pdv: PVOID; pcFonts: LPDWORD): HANDLE; stdcall;
function RemoveFontMemResourceEx(fh: HANDLE): BOOL; stdcall;

const
  FR_PRIVATE    = $10;
  FR_NOT_ENUM   = $20;

// The actual size of the DESIGNVECTOR and ENUMLOGFONTEXDV structures
// is determined by dvNumAxes,
// MM_MAX_NUMAXES only detemines the maximal size allowed

const
  MM_MAX_AXES_NAMELEN = 16;

type
  PAxisInfoA = ^TAxisInfoA;
  tagAXISINFOA = record
    axMinValue: LONG;
    axMaxValue: LONG;
    axAxisName: array [0..MM_MAX_AXES_NAMELEN - 1] of BYTE;
  end;
  AXISINFOA = tagAXISINFOA;
  LPAXISINFOA = ^AXISINFOA;
  TAxisInfoA = AXISINFOA;

  PAxisInfoW = ^TAxisInfoW;
  tagAXISINFOW = record
    axMinValue: LONG;
    axMaxValue: LONG;
    axAxisName: array [0..MM_MAX_AXES_NAMELEN - 1] of WCHAR;
  end;
  AXISINFOW = tagAXISINFOW;
  LPAXISINFOW = ^AXISINFOW;
  TAxisInfoW = AXISINFOW;

{$IFDEF UNICODE}
  AXISINFO = AXISINFOW;
  PAXISINFO = PAXISINFOW;
  LPAXISINFO = LPAXISINFOW;
  TAxisInfo = TAxisInfoW;
{$ELSE}
  AXISINFO = AXISINFOA;
  PAXISINFO = PAXISINFOA;
  LPAXISINFO = LPAXISINFOA;
  TAxisInfo = TAxisInfoA;
{$ENDIF}

  PAxesListA = ^TAxesListA;
  tagAXESLISTA = record
    axlReserved: DWORD;
    axlNumAxes: DWORD;
    axlAxisInfo: array [0..MM_MAX_NUMAXES - 1] of AXISINFOA;
  end;
  AXESLISTA = tagAXESLISTA;
  LPAXESLISTA = ^AXESLISTA;
  TAxesListA = tagAXESLISTA;

  PAxesListW = ^TAxesListW;
  tagAXESLISTW = record
    axlReserved: DWORD;
    axlNumAxes: DWORD;
    axlAxisInfo: array [0..MM_MAX_NUMAXES - 1] of AXISINFOW;
  end;
  AXESLISTW = tagAXESLISTW;
  LPAXESLISTW = ^AXESLISTW;
  TAxesListW = tagAXESLISTW;

{$IFDEF UNICODE}
  AXESLIST = AXESLISTW;
  PAXESLIST = PAXESLISTW;
  LPAXESLIST = LPAXESLISTW;
  TAxesList = TAxesListW;
{$ELSE}
  AXESLIST = AXESLISTA;
  PAXESLIST = PAXESLISTA;
  LPAXESLIST = LPAXESLISTA;
  TAxesList = TAxesListA;
{$ENDIF}

// The actual size of the AXESLIST and ENUMTEXTMETRIC structure is
// determined by axlNumAxes,
// MM_MAX_NUMAXES only detemines the maximal size allowed

  PEnumLogFontExDVA = ^TEnumLogFontExDVA;
  tagENUMLOGFONTEXDVA = record
    elfEnumLogfontEx: ENUMLOGFONTEXA;
    elfDesignVector: DESIGNVECTOR;
  end;
  ENUMLOGFONTEXDVA = tagENUMLOGFONTEXDVA;
  LPENUMLOGFONTEXDVA = ^ENUMLOGFONTEXDVA;
  TEnumLogFontExDVA = tagENUMLOGFONTEXDVA;

  PEnumLogFontExDVW = ^TEnumLogFontExDVW;
  tagENUMLOGFONTEXDVW = record
    elfEnumLogfontEx: ENUMLOGFONTEXW;
    elfDesignVector: DESIGNVECTOR;
  end;
  ENUMLOGFONTEXDVW = tagENUMLOGFONTEXDVW;
  LPENUMLOGFONTEXDVW = ^ENUMLOGFONTEXDVW;
  TEnumLogFontExDVW = tagENUMLOGFONTEXDVW;

{$IFDEF UNICODE}
  ENUMLOGFONTEXDV = ENUMLOGFONTEXDVW;
  PENUMLOGFONTEXDV = PENUMLOGFONTEXDVW;
  LPENUMLOGFONTEXDV = LPENUMLOGFONTEXDVW;
  TEnumLogFontExDV = TEnumLogFontExDVW;
{$ELSE}
  ENUMLOGFONTEXDV = ENUMLOGFONTEXDVA;
  PENUMLOGFONTEXDV = PENUMLOGFONTEXDVA;
  LPENUMLOGFONTEXDV = LPENUMLOGFONTEXDVA;
  TEnumLogFontExDV = TEnumLogFontExDVA;
{$ENDIF}

function CreateFontIndirectExA(penumlfex: LPENUMLOGFONTEXDVA): HFONT; stdcall;
function CreateFontIndirectExW(penumlfex: LPENUMLOGFONTEXDVW): HFONT; stdcall;

{$IFDEF UNICODE}
function CreateFontIndirectEx(penumlfex: LPENUMLOGFONTEXDVW): HFONT; stdcall;
{$ELSE}
function CreateFontIndirectEx(penumlfex: LPENUMLOGFONTEXDVA): HFONT; stdcall;
{$ENDIF}

type
  PEnumTextMetricA = ^TEnumTextMetricA;
  tagENUMTEXTMETRICA = record
    etmNewTextMetricEx: NEWTEXTMETRICEXA;
    etmAxesList: AXESLISTA;
  end;
  ENUMTEXTMETRICA = tagENUMTEXTMETRICA;
  LPENUMTEXTMETRICA = ^ENUMTEXTMETRICA;
  TEnumTextMetricA = tagENUMTEXTMETRICA;

  PEnumTextMetricW = ^TEnumTextMetricW;
  tagENUMTEXTMETRICW = record
    etmNewTextMetricEx: NEWTEXTMETRICEXW;
    etmAxesList: AXESLISTW;
  end;
  ENUMTEXTMETRICW = tagENUMTEXTMETRICW;
  LPENUMTEXTMETRICW = ^ENUMTEXTMETRICW;
  TEnumTextMetricW = tagENUMTEXTMETRICW;

{$IFDEF UNICODE}
  ENUMTEXTMETRIC = ENUMTEXTMETRICW;
  PENUMTEXTMETRIC = PENUMTEXTMETRICW;
  LPENUMTEXTMETRIC = LPENUMTEXTMETRICW;
  TEnumTextMetric = TEnumTextMetricW;
{$ELSE}
  ENUMTEXTMETRIC = ENUMTEXTMETRICA;
  PENUMTEXTMETRIC = PENUMTEXTMETRICA;
  LPENUMTEXTMETRIC = LPENUMTEXTMETRICA;
  TEnumTextMetric = TEnumTextMetricA;
{$ENDIF}

function GetViewportExtEx(hdc: HDC; var lpSize: TSize): BOOL; stdcall;
function GetViewportOrgEx(hdc: HDC; var lpPoint: POINT): BOOL; stdcall;
function GetWindowExtEx(hdc: HDC; var lpSize: TSize): BOOL; stdcall;
function GetWindowOrgEx(hdc: HDC; var lpPoint: POINT): BOOL; stdcall;

function IntersectClipRect(hdc: HDC; nLeftRect, nTopRect, nRightRect, nBottomRect: Integer): Integer; stdcall;
function InvertRgn(hdc: HDC; hrgn: HRGN): BOOL; stdcall;
function LineDDA(nXStart, nYStart, nXEnd, nYEnd: Integer; lpLineFunc: LINEDDAPROC; lpData: LPARAM): BOOL; stdcall;
function LineTo(hdc: HDC; nXEnd, nYEnd: Integer): BOOL; stdcall;
function MaskBlt(hdc: HDC; nXDest, nYDest, nWidth, nHeight: Integer; hdcSrc: HDC; nXSrc, nYSrc: Integer; hbmMask: HBITMAP; xMask, yMask: Integer; dwRop: DWORD): BOOL; stdcall;
function PlgBlt(hdc: HDC; lpPoint: LPPOINT; hdcSrc: HDC; nXSrc, nYSrc, nWidth, nHeight: Integer; hbmMask: HBITMAP; xMask, yMask: Integer): BOOL; stdcall;

function OffsetClipRgn(hdc: HDC; nXOffset, nYOffset: Integer): Integer; stdcall;
function OffsetRgn(hrgn: HRGN; nXOffset, nYOffset: Integer): Integer; stdcall;
function PatBlt(hdc: HDC; nXLeft, nYLeft, nWidth, nHeight: Integer; dwRop: DWORD): BOOL; stdcall;
function Pie(hdc: HDC; nLeftRect, nTopRect, nRightRect, nBottomRect, nXRadial1, nYRadial1, nXRadial2, nYRadial2: Integer): BOOL; stdcall;
function PlayMetaFile(hdc: HDC; hmf: HMETAFILE): BOOL; stdcall;
function PaintRgn(hdc: HDC; hrgn: HRGN): BOOL; stdcall;
function PolyPolygon(hdc: HDC; lpPoints: LPPOINT; lpPolyCounts: LPINT; nCount: Integer): BOOL; stdcall;
function PtInRegion(hrgn: HRGN; X, Y: Integer): BOOL; stdcall;
function PtVisible(hdc: HDC; X, Y: Integer): BOOL; stdcall;
function RectInRegion(hrgn: HRGN; const lprc: RECT): BOOL; stdcall;
function RectVisible(hdc: HDC; const lprc: RECT): BOOL; stdcall;
function Rectangle(hdc: HDC; nLeftRect, nTopRect, nRightRect, nBottomRect: Integer): BOOL; stdcall;
function RestoreDC(hdc: HDC; nSavedDc: Integer): BOOL; stdcall;
function ResetDCA(hdc: HDC; const lpInitData: DEVMODEA): HDC; stdcall;
function ResetDCW(hdc: HDC; const lpInitData: DEVMODEW): HDC; stdcall;
{$IFDEF UNICODE}
function ResetDC(hdc: HDC; const lpInitData: DEVMODEW): HDC; stdcall;
{$ELSE}
function ResetDC(hdc: HDC; const lpInitData: DEVMODEA): HDC; stdcall;
{$ENDIF}
function RealizePalette(hdc: HDC): UINT; stdcall;
function RemoveFontResourceA(lpFileName: LPCSTR): BOOL; stdcall;
function RemoveFontResourceW(lpFileName: LPCWSTR): BOOL; stdcall;
{$IFDEF UNICODE}
function RemoveFontResource(lpFileName: LPCWSTR): BOOL; stdcall;
{$ELSE}
function RemoveFontResource(lpFileName: LPCSTR): BOOL; stdcall;
{$ENDIF}
function RoundRect(hdc: HDC; nLeftRect, nTopRect, nRightRect, nBottomRect, nWidth, nHeight: Integer): BOOL; stdcall;
function ResizePalette(hPal: HPALETTE; nEntries: UINT): BOOL; stdcall;
function SaveDC(hdc: HDC): Integer; stdcall;
function SelectClipRgn(hdc: HDC; hrgn: HRGN): Integer; stdcall;
function ExtSelectClipRgn(hdc: HDC; hrgn: HRGN; fnMode: Integer): Integer; stdcall;
function SetMetaRgn(hdc: HDC): Integer; stdcall;
function SelectObject(hdc: HDC; hgdiobj: HGDIOBJ): HGDIOBJ; stdcall;
function SelectPalette(hdc: HDC; hpal: HPALETTE; nForceBackground: BOOL): HPALETTE; stdcall;
function SetBkColor(hdc: HDC; crColor: COLORREF): COLORREF; stdcall;

function SetDCBrushColor(hdc: HDC; crColor: COLORREF): COLORREF; stdcall;
function SetDCPenColor(hdc: HDC; crColor: COLORREF): COLORREF; stdcall;

function SetBkMode(hdc: HDC; iBlMode: Integer): Integer; stdcall;
function SetBitmapBits(hbmp: HBITMAP; cBytes: DWORD; lpBits: LPVOID): LONG; stdcall;

function SetBoundsRect(hdc: HDC; lprcBounds: LPRECT; flags: UINT): UINT; stdcall;
function SetDIBits(hdc: HDC; hbmp: HBITMAP; uStartScan, cScanLines: UINT; lpvBits: LPVOID; const lpbmi: BITMAPINFO; fuColorUse: UINT): Integer; stdcall;
function SetDIBitsToDevice(hdc: HDC; xDest, yDest: Integer; dwWidth, dwHeight: DWORD; XSrc, YSrc: Integer; uStartScan, cScanLines: UINT; lpvBits: LPVOID; const lpbmi: BITMAPINFO; fuColorUse: UINT): Integer; stdcall;
function SetMapperFlags(hdc: HDC; dwFlag: DWORD): DWORD; stdcall;
function SetGraphicsMode(hdc: HDC; iMode: Integer): Integer; stdcall;
function SetMapMode(hdc: HDC; fnMapMode: Integer): Integer; stdcall;

function SetLayout(hdc: HDC; dwLayOut: DWORD): DWORD; stdcall;
function GetLayout(hdc: HDC): DWORD; stdcall;

function SetMetaFileBitsEx(nSize: UINT; lpData: LPBYTE): HMETAFILE; stdcall;
function SetPaletteEntries(hPal: HPALETTE; cStart, nEntries: UINT; lppe: LPPALETTEENTRY): UINT; stdcall;
function SetPixel(hdc: HDC; X, Y: Integer; crColor: COLORREF): COLORREF; stdcall;
function SetPixelV(hdc: HDC; X, Y: Integer; crColor: COLORREF): BOOL; stdcall;
function SetPixelFormat(hdc: HDC; iPixelFormat: Integer; const ppfd: PIXELFORMATDESCRIPTOR): BOOL; stdcall;
function SetPolyFillMode(hdc: HDC; iPolyFillMode: Integer): Integer; stdcall;
function StretchBlt(hdc: HDC; nXOriginDest, nYOriginDest, nWidthDest, nHeightDest: Integer; hdcSrc: HDC; nXOriginSrc, nYOriginSrc, nWidthSrc, nHeightSrc: Integer; dwRop: DWORD): BOOL; stdcall;
function SetRectRgn(hrgn: HRGN; nLeftRect, nTopRect, nRightRect, nBottomRect: Integer): BOOL; stdcall;
function StretchDIBits(hdc: HDC; XDest, YDest, nDestWidth, nYDestHeight, XSrc, YSrc, nSrcWidth, nSrcHeight: Integer; lpBits: LPVOID; const lpBitsInfo: BITMAPINFO; iUsage: UINT; dwRop: DWORD): Integer; stdcall;
function SetROP2(hdc: HDC; fnDrawMode: Integer): Integer; stdcall;
function SetStretchBltMode(hdc: HDC; iStretchMode: Integer): Integer; stdcall;
function SetSystemPaletteUse(hdc: HDC; uUsage: UINT): UINT; stdcall;
function SetTextCharacterExtra(hdc: HDC; nCharExtra: Integer): Integer; stdcall;
function SetTextColor(hdc: HDC; crColor: COLORREF): COLORREF; stdcall;
function SetTextAlign(hdc: HDC; fMode: UINT): UINT; stdcall;
function SetTextJustification(hdc: HDC; nBreakExtra, nBreakCount: Integer): BOOL; stdcall;
function UpdateColors(hdc: HDC): BOOL; stdcall;

//
// image blt
//

type
  COLOR16 = USHORT;

  PTriVertex = ^TTriVertex;
  _TRIVERTEX = record
    x: LONG;
    y: LONG;
    Red: COLOR16;
    Green: COLOR16;
    Blue: COLOR16;
    Alpha: COLOR16;
  end;
  TRIVERTEX = _TRIVERTEX;
  LPTRIVERTEX = ^TRIVERTEX;
  TTriVertex = _TRIVERTEX;

  PGradientTriangle = ^TGradientTriangle;
  _GRADIENT_TRIANGLE = record
    Vertex1: ULONG;
    Vertex2: ULONG;
    Vertex3: ULONG;
  end;
  GRADIENT_TRIANGLE = _GRADIENT_TRIANGLE;
  LPGRADIENT_TRIANGLE = ^GRADIENT_TRIANGLE;
  PGRADIENT_TRIANGLE = ^GRADIENT_TRIANGLE;
  TGradientTriangle = _GRADIENT_TRIANGLE;

  PGradientRect = ^TGradientRect;
  _GRADIENT_RECT = record
    UpperLeft: ULONG;
    LowerRight: ULONG;
  end;
  GRADIENT_RECT = _GRADIENT_RECT;
  LPGRADIENT_RECT = ^GRADIENT_RECT;
  PGRADIENT_RECT = ^GRADIENT_RECT;
  TGradientRect = _GRADIENT_RECT;

  PBlendFunction = ^TBlendFunction;
  _BLENDFUNCTION = record
    BlendOp: BYTE;
    BlendFlags: BYTE;
    SourceConstantAlpha: BYTE;
    AlphaFormat: BYTE;
  end;
  BLENDFUNCTION = _BLENDFUNCTION;
  LPBLENDFUNCTION = ^BLENDFUNCTION;
  TBlendFunction = _BLENDFUNCTION;

//
// currentlly defined blend function
//

const
  AC_SRC_OVER = $00;

//
// alpha format flags
//

  AC_SRC_ALPHA = $01;

function AlphaBlend(hdcDest: HDC; nXOriginDest, nYOriginDest, nWidthDest,
  nHeightDest: Integer; hdcSrc: HDC; nXOriginSrc, nYOriginSrc, nWidthSrc,
  nHeightSrc: Integer; blendFunction: BLENDFUNCTION): BOOL; stdcall;

function TransparentBlt(hdcSrc: HDC; nXOriginSrc, nYOriginSrc, nWidthSrc,
  nHeightSrc: Integer; hdcDest: HDC; nXOriginDest, nYOriginDest, nWidthDest,
  nHeightDest: Integer; blendFunction: BLENDFUNCTION): BOOL; stdcall;

//
// gradient drawing modes
//

const
  GRADIENT_FILL_RECT_H   = $00000000;
  GRADIENT_FILL_RECT_V   = $00000001;
  GRADIENT_FILL_TRIANGLE = $00000002;
  GRADIENT_FILL_OP_FLAG  = $000000ff;

function GradientFill(hdc: HDC; pVertex: PTRIVERTEX; dwNumVertex: ULONG; pMesh: PVOID; dwNumMesh, dwMode: ULONG): BOOL; stdcall;
function PlayMetaFileRecord(hdc: HDC; lpHandleTable: LPHANDLETABLE; lpMetaRecord: LPMETARECORD; nHandles: UINT): BOOL; stdcall;

type
  MFENUMPROC = function (hdc: HDC; lpHTable: LPHANDLETABLE; lpMFR: LPMETARECORD; nObj: Integer; lpClientData: LPARAM): Integer; stdcall;

function EnumMetaFile(hdc: HDC; hemf: HMETAFILE; lpMetaFunc: MFENUMPROC; lParam: LPARAM): BOOL; stdcall;

type
  ENHMFENUMPROC = function (hdc: HDC; lpHTable: LPHANDLETABLE; lpEMFR: LPENHMETARECORD; nObj: Integer; lpData: LPARAM): Integer; stdcall;

// Enhanced Metafile Function Declarations

function CloseEnhMetaFile(hdc: HDC): HENHMETAFILE; stdcall;
function CopyEnhMetaFileA(hemfSrc: HENHMETAFILE; lpszFile: LPCSTR): HENHMETAFILE; stdcall;
function CopyEnhMetaFileW(hemfSrc: HENHMETAFILE; lpszFile: LPCWSTR): HENHMETAFILE; stdcall;
{$IFDEF UNICODE}
function CopyEnhMetaFile(hemfSrc: HENHMETAFILE; lpszFile: LPCWSTR): HENHMETAFILE; stdcall;
{$ELSE}
function CopyEnhMetaFile(hemfSrc: HENHMETAFILE; lpszFile: LPCSTR): HENHMETAFILE; stdcall;
{$ENDIF}
function CreateEnhMetaFileA(hdcRef: HDC; lpFileName: LPCSTR; const lpRect: RECT; lpDescription: LPCSTR): HDC; stdcall;
function CreateEnhMetaFileW(hdcRef: HDC; lpFileName: LPCWSTR; const lpRect: RECT; lpDescription: LPCWSTR): HDC; stdcall;
{$IFDEF UNICODE}
function CreateEnhMetaFile(hdcRef: HDC; lpFileName: LPCWSTR; const lpRect: RECT; lpDescription: LPCWSTR): HDC; stdcall;
{$ELSE}
function CreateEnhMetaFile(hdcRef: HDC; lpFileName: LPCSTR; const lpRect: RECT; lpDescription: LPCSTR): HDC; stdcall;
{$ENDIF}

function DeleteEnhMetaFile(hemf: HENHMETAFILE): BOOL; stdcall;
function EnumEnhMetaFile(hdc: HDC; hemf: HENHMETAFILE; lpEnhMetaFunc: ENHMFENUMPROC; lpData: LPVOID; const lpRect: RECT): BOOL; stdcall;
function GetEnhMetaFileA(lpszMetaFile: LPCSTR): HENHMETAFILE; stdcall;
function GetEnhMetaFileW(lpszMetaFile: LPCWSTR): HENHMETAFILE; stdcall;
{$IFDEF UNICODE}
function GetEnhMetaFile(lpszMetaFile: LPCWSTR): HENHMETAFILE; stdcall;
{$ELSE}
function GetEnhMetaFile(lpszMetaFile: LPCSTR): HENHMETAFILE; stdcall;
{$ENDIF}
function GetEnhMetaFileBits(hemf: HENHMETAFILE; cbBuffer: UINT; lpBuffer: LPBYTE): UINT; stdcall;
function GetEnhMetaFileDescriptionA(hemf: HENHMETAFILE; cchBuffer: UINT; lpszDescription: LPSTR): UINT; stdcall;
function GetEnhMetaFileDescriptionW(hemf: HENHMETAFILE; cchBuffer: UINT; lpszDescription: LPWSTR): UINT; stdcall;
{$IFDEF UNICODE}
function GetEnhMetaFileDescription(hemf: HENHMETAFILE; cchBuffer: UINT; lpszDescription: LPWSTR): UINT; stdcall;
{$ELSE}
function GetEnhMetaFileDescription(hemf: HENHMETAFILE; cchBuffer: UINT; lpszDescription: LPSTR): UINT; stdcall;
{$ENDIF}
function GetEnhMetaFileHeader(hemf: HENHMETAFILE; cbBuffer: UINT; lpemh: LPENHMETAHEADER ): UINT; stdcall;
function GetEnhMetaFilePaletteEntries(hemf: HENHMETAFILE; cEntries: UINT; lppe: LPPALETTEENTRY ): UINT; stdcall;
function GetEnhMetaFilePixelFormat(hemf: HENHMETAFILE; cbBuffer: UINT; var ppfd: PIXELFORMATDESCRIPTOR): UINT; stdcall;
function GetWinMetaFileBits(hemf: HENHMETAFILE; cbBuffer: UINT; lpbBuffer: LPBYTE; fnMapMode: Integer; hdcRef: HDC): UINT; stdcall;
function PlayEnhMetaFile(hdc: HDC; hemf: HENHMETAFILE; const lpRect: RECT): BOOL; stdcall;
function PlayEnhMetaFileRecord(hdc: HDC; lpHandleTable: LPHANDLETABLE; lpEnhMetaRecord: LPENHMETARECORD; nHandles: UINT): BOOL; stdcall;
function SetEnhMetaFileBits(cbBuffer: UINT; lpData: LPBYTE): HENHMETAFILE; stdcall;
function SetWinMetaFileBits(cbBuffer: UINT; lpbBuffer: LPBYTE; hdcRef: HDC; const lpmfp: METAFILEPICT): HENHMETAFILE; stdcall;
function GdiComment(hdc: HDC; cbSize: UINT; lpData: LPBYTE): BOOL; stdcall;

function GetTextMetricsA(hdc: HDC; var lptm: TEXTMETRICA): BOOL; stdcall;
function GetTextMetricsW(hdc: HDC; var lptm: TEXTMETRICW): BOOL; stdcall;
{$IFDEF UNICODE}
function GetTextMetrics(hdc: HDC; var lptm: TEXTMETRICW): BOOL; stdcall;
{$ELSE}
function GetTextMetrics(hdc: HDC; var lptm: TEXTMETRICA): BOOL; stdcall;
{$ENDIF}

// new GDI

type
  PDibSection = ^TDibSection;
  tagDIBSECTION = record
    dsBm: BITMAP;
    dsBmih: BITMAPINFOHEADER;
    dsBitfields: array [0..2] of DWORD;
    dshSection: HANDLE;
    dsOffset: DWORD;
  end;
  DIBSECTION = tagDIBSECTION;
  LPDIBSECTION = ^DIBSECTION;
  TDibSection = DIBSECTION;

function AngleArc(hdc: HDC; X, Y: Integer; dwRadius: DWORD; eStartAngle, eSweepAngle: FLOAT): BOOL; stdcall;
function PolyPolyline(hdc: HDC; lppt: LPPOINT; lpdwPolyPoints: LPDWORD; cCount: DWORD): BOOL; stdcall;
function GetWorldTransform(hdc: HDC; lpXform: LPXFORM): BOOL; stdcall;
function SetWorldTransform(hdc: HDC; lpXform: LPXFORM): BOOL; stdcall;
function ModifyWorldTransform(hdc: HDC; lpXform: LPXFORM; iMode: DWORD): BOOL; stdcall;
function CombineTransform(lpxformResult, lpXform1, lpXform2: LPXFORM): BOOL; stdcall;
function CreateDIBSection(hdc: HDC; pbmi: LPBITMAPINFO; iUsage: UINT;
  ppvBits: PPVOID; hSection: HANDLE; dwOffset: DWORD): HBITMAP; stdcall;
function GetDIBColorTable(hdc: HDC; uStartIndex, cEntries: UINT; pColors: PRGBQUAD): UINT; stdcall;
function SetDIBColorTable(hdc: HDC; uStartIndex, cEntries: UINT; pColors: PRGBQUAD): UINT; stdcall;

// Flags value for COLORADJUSTMENT

const
  CA_NEGATIVE   = $0001;
  CA_LOG_FILTER = $0002;

// IlluminantIndex values

  ILLUMINANT_DEVICE_DEFAULT = 0;
  ILLUMINANT_A              = 1;
  ILLUMINANT_B              = 2;
  ILLUMINANT_C              = 3;
  ILLUMINANT_D50            = 4;
  ILLUMINANT_D55            = 5;
  ILLUMINANT_D65            = 6;
  ILLUMINANT_D75            = 7;
  ILLUMINANT_F2             = 8;
  ILLUMINANT_MAX_INDEX      = ILLUMINANT_F2;

  ILLUMINANT_TUNGSTEN    = ILLUMINANT_A;
  ILLUMINANT_DAYLIGHT    = ILLUMINANT_C;
  ILLUMINANT_FLUORESCENT = ILLUMINANT_F2;
  ILLUMINANT_NTSC        = ILLUMINANT_C;

// Min and max for RedGamma, GreenGamma, BlueGamma

  RGB_GAMMA_MIN = WORD(02500);
  RGB_GAMMA_MAX = WORD(65000);

// Min and max for ReferenceBlack and ReferenceWhite

  REFERENCE_WHITE_MIN = WORD(6000);
  REFERENCE_WHITE_MAX = WORD(10000);
  REFERENCE_BLACK_MIN = WORD(0);
  REFERENCE_BLACK_MAX = WORD(4000);

// Min and max for Contrast, Brightness, Colorfulness, RedGreenTint

  COLOR_ADJ_MIN = SHORT(-100);
  COLOR_ADJ_MAX = SHORT(100);

type
  PColorAdjustment = ^TColorAdjustment;
  tagCOLORADJUSTMENT = record
    caSize: WORD;
    caFlags: WORD;
    caIlluminantIndex: WORD;
    caRedGamma: WORD;
    caGreenGamma: WORD;
    caBlueGamma: WORD;
    caReferenceBlack: WORD;
    caReferenceWhite: WORD;
    caContrast: SHORT;
    caBrightness: SHORT;
    caColorfulness: SHORT;
    caRedGreenTint: SHORT;
  end;
  COLORADJUSTMENT = tagCOLORADJUSTMENT;
  LPCOLORADJUSTMENT = ^COLORADJUSTMENT;
  TColorAdjustment = COLORADJUSTMENT;

function SetColorAdjustment(hdc: HDC; lpca: LPCOLORADJUSTMENT): BOOL; stdcall;
function GetColorAdjustment(hdc: HDC; lpca: LPCOLORADJUSTMENT): BOOL; stdcall;
function CreateHalftonePalette(hdc: HDC): HPALETTE; stdcall;

type
  ABORTPROC = function (hdc: HDC; iError: Integer): BOOL; stdcall;

  PDocInfoA = ^TDocInfoA;
  _DOCINFOA = record
    cbSize: Integer;
    lpszDocName: LPCSTR;
    lpszOutput: LPCSTR;
    {$IFDEF WINVER_0400_GREATER}
    lpszDatatype: LPCSTR;
    fwType: DWORD;
    {$ENDIF}
  end;
  DOCINFOA = _DOCINFOA;
  LPDOCINFOA = ^DOCINFOA;
  TDocInfoA = _DOCINFOA;

  PDocInfoW = ^TDocInfoW;
  _DOCINFOW = record
    cbSize: Integer;
    lpszDocName: LPCWSTR;
    lpszOutput: LPCWSTR;
    {$IFDEF WINVER_0400_GREATER}
    lpszDatatype: LPCWSTR;
    fwType: DWORD;
    {$ENDIF}
  end;
  DOCINFOW = _DOCINFOW;
  LPDOCINFOW = ^DOCINFOW;
  TDocInfoW = _DOCINFOW;

{$IFDEF UNICODE}
  DOCINFO = DOCINFOW;
  LPDOCINFO = LPDOCINFOW;
  TDocInfo = TDocInfoW;
  PDocInfo = PDocInfoW;
{$ELSE}
  DOCINFO = DOCINFOA;
  LPDOCINFO = LPDOCINFOA;
  TDocInfo = TDocInfoA;
  PDocInfo = PDocInfoA;
{$ENDIF}

const
  DI_APPBANDING            = $00000001;
  DI_ROPS_READ_DESTINATION = $00000002;

function StartDocA(hdc: HDC; const lpdi: DOCINFOA): Integer; stdcall;
function StartDocW(hdc: HDC; const lpdi: DOCINFOW): Integer; stdcall;
{$IFDEF UNICODE}
function StartDoc(hdc: HDC; const lpdi: DOCINFOW): Integer; stdcall;
{$ELSE}
function StartDoc(hdc: HDC; const lpdi: DOCINFOA): Integer; stdcall;
{$ENDIF}
function EndDoc(dc: HDC): Integer; stdcall;
function StartPage(dc: HDC): Integer; stdcall;
function EndPage(dc: HDC): Integer; stdcall;
function AbortDoc(dc: HDC): Integer; stdcall;
function SetAbortProc(dc: HDC; lpAbortProc: ABORTPROC): Integer; stdcall;

function AbortPath(hdc: HDC): BOOL; stdcall;
function ArcTo(hdc: HDC; nLeftRect, nTopRect, nRightRect, nBottomRect, nXRadial1, nYRadial1, nXRadial2, nYRadial2: Integer): BOOL; stdcall;
function BeginPath(hdc: HDC): BOOL; stdcall;
function CloseFigure(hdc: HDC): BOOL; stdcall;
function EndPath(hdc: HDC): BOOL; stdcall;
function FillPath(hdc: HDC): BOOL; stdcall;
function FlattenPath(hdc: HDC): BOOL; stdcall;
function GetPath(hdc: HDC; lpPoints: LPPOINT; lpTypes: LPBYTE; nSize: Integer): Integer; stdcall;
function PathToRegion(hdc: HDC): HRGN; stdcall;
function PolyDraw(hdc: HDC; lppt: LPPOINT; lpbTypes: LPBYTE; cCount: Integer): BOOL; stdcall;
function SelectClipPath(hdc: HDC; iMode: Integer): BOOL; stdcall;
function SetArcDirection(hdc: HDC; ArcDirection: Integer): Integer; stdcall;
function SetMiterLimit(hdc: HDC; eNewLimit: FLOAT; peOldLimit: PFLOAT): BOOL; stdcall;
function StrokeAndFillPath(hdc: HDC): BOOL; stdcall;
function StrokePath(hdc: HDC): BOOL; stdcall;
function WidenPath(hdc: HDC): BOOL; stdcall;
function ExtCreatePen(dwPenStyle, dwWidth: DWORD; const lplb: LOGBRUSH; dwStyleCount: DWORD; lpStyle: DWORD): HPEN; stdcall;
function GetMiterLimit(hdc: HDC; var peLimit: FLOAT): BOOL; stdcall;
function GetArcDirection(hdc: HDC): Integer; stdcall;

function GetObjectA(hgdiobj: HGDIOBJ; cbBuffer: Integer; lpvObject: LPVOID): Integer; stdcall;
function GetObjectW(hgdiobj: HGDIOBJ; cbBuffer: Integer; lpvObject: LPVOID): Integer; stdcall;
{$IFDEF UNICODE}
function GetObject(hgdiobj: HGDIOBJ; cbBuffer: Integer; lpvObject: LPVOID): Integer; stdcall;
{$ELSE}
function GetObject(hgdiobj: HGDIOBJ; cbBuffer: Integer; lpvObject: LPVOID): Integer; stdcall;
{$ENDIF}
function MoveToEx(hdc: HDC; X, Y: Integer; lpPoint: LPPOINT): BOOL; stdcall;
function TextOutA(hdc: HDC; nXStart, nYStart: Integer; lpString: LPCSTR; cbString: Integer): BOOL; stdcall;
function TextOutW(hdc: HDC; nXStart, nYStart: Integer; lpString: LPCWSTR; cbString: Integer): BOOL; stdcall;
{$IFDEF UNICODE}
function TextOut(hdc: HDC; nXStart, nYStart: Integer; lpString: LPCWSTR; cbString: Integer): BOOL; stdcall;
{$ELSE}
function TextOut(hdc: HDC; nXStart, nYStart: Integer; lpString: LPCSTR; cbString: Integer): BOOL; stdcall;
{$ENDIF}
function ExtTextOutA(hdc: HDC; X, Y: Integer; fuOptions: UINT; lprc: LPRECT; lpString: LPCSTR; cbCount: UINT; lpDx: LPINT): BOOL; stdcall;
function ExtTextOutW(hdc: HDC; X, Y: Integer; fuOptions: UINT; lprc: LPRECT; lpString: LPCWSTR; cbCount: UINT; lpDx: LPINT): BOOL; stdcall;
{$IFDEF UNICODE}
function ExtTextOut(hdc: HDC; X, Y: Integer; fuOptions: UINT; lprc: LPRECT; lpString: LPCWSTR; cbCount: UINT; lpDx: LPINT): BOOL; stdcall;
{$ELSE}
function ExtTextOut(hdc: HDC; X, Y: Integer; fuOptions: UINT; lprc: LPRECT; lpString: LPCSTR; cbCount: UINT; lpDx: LPINT): BOOL; stdcall;
{$ENDIF}
function PolyTextOutA(hdc: HDC; pptxt: LPPOLYTEXTA; cStrings: Integer): BOOL; stdcall;
function PolyTextOutW(hdc: HDC; pptxt: LPPOLYTEXTW; cStrings: Integer): BOOL; stdcall;
{$IFDEF UNICODE}
function PolyTextOut(hdc: HDC; pptxt: LPPOLYTEXTW; cStrings: Integer): BOOL; stdcall;
{$ELSE}
function PolyTextOut(hdc: HDC; pptxt: LPPOLYTEXTA; cStrings: Integer): BOOL; stdcall;
{$ENDIF}

function CreatePolygonRgn(lppt: LPPOINT; cPoints, fnPolyFillMode: Integer): HRGN; stdcall;
function DPtoLP(hdc: HDC; lpPoints: LPPOINT; nCount: Integer): BOOL; stdcall;
function LPtoDP(hdc: HDC; lpPoints: LPPOINT; nCount: Integer): BOOL; stdcall;
function Polygon(hdc: HDC; lpPoints: LPPOINT; nCount: Integer): BOOL; stdcall;
function Polyline(hdc: HDC; lppt: LPPOINT; nCount: Integer): BOOL; stdcall;

function PolyBezier(hdc: HDC; lppt: LPPOINT; cPoints: DWORD): BOOL; stdcall;
function PolyBezierTo(hdc: HDC; lppt: LPPOINT; cCount: DWORD): BOOL; stdcall;
function PolylineTo(hdc: HDC; lppt: LPPOINT; cCount: DWORD): BOOL; stdcall;

function SetViewportExtEx(hdc: HDC; nXExtend, nYExtend: Integer; lpSize: LPSIZE): BOOL; stdcall;
function SetViewportOrgEx(hdc: HDC; X, Y: Integer; lpPoint: LPPOINT): BOOL; stdcall;
function SetWindowExtEx(hdc: HDC; nXExtend, nYExtend: Integer; lpSize: LPSIZE): BOOL; stdcall;
function SetWindowOrgEx(hdc: HDC; X, Y: Integer; lpPoint: LPPOINT): BOOL; stdcall;

function OffsetViewportOrgEx(hdc: HDC; nXOffset, nYOffset: Integer; lpPoint: LPPOINT): BOOL; stdcall;
function OffsetWindowOrgEx(hdc: HDC; nXOffset, nYOffset: Integer; lpPoint: LPPOINT): BOOL; stdcall;
function ScaleViewportExtEx(hdc: HDC; Xnum, Xdenom, Ynum, Ydenom: Integer; lpSize: LPSIZE): BOOL; stdcall;
function ScaleWindowExtEx(hdc: HDC; Xnum, Xdenom, Ynum, Ydenom: Integer; lpSize: LPSIZE): BOOL; stdcall;
function SetBitmapDimensionEx(hBitmap: HBITMAP; nWidth, nHeight: Integer; lpSize: LPSIZE): BOOL; stdcall;
function SetBrushOrgEx(hdc: HDC; nXOrg, nYOrg: Integer; lppt: LPPOINT): BOOL; stdcall;

function GetTextFaceA(hdc: HDC; nCount: Integer; lpFaceName: LPSTR): Integer; stdcall;
function GetTextFaceW(hdc: HDC; nCount: Integer; lpFaceName: LPWSTR): Integer; stdcall;
{$IFDEF UNICODE}
function GetTextFace(hdc: HDC; nCount: Integer; lpFaceName: LPWSTR): Integer; stdcall;
{$ELSE}
function GetTextFace(hdc: HDC; nCount: Integer; lpFaceName: LPSTR): Integer; stdcall;
{$ENDIF}

const
  FONTMAPPER_MAX = 10;

type
  PKerningPair = ^TKerningPair;
  tagKERNINGPAIR = record
    wFirst: WORD;
    wSecond: WORD;
    iKernAmount: Integer;
  end;
  KERNINGPAIR = tagKERNINGPAIR;
  LPKERNINGPAIR = ^KERNINGPAIR;
  TKerningPair = KERNINGPAIR;

function GetKerningPairsA(hDc: HDC; nNumPairs: DWORD; lpkrnpair: LPKERNINGPAIR): DWORD; stdcall;
function GetKerningPairsW(hDc: HDC; nNumPairs: DWORD; lpkrnpair: LPKERNINGPAIR): DWORD; stdcall;

{$IFDEF UNICODE}
function GetKerningPairs(hDc: HDC; nNumPairs: DWORD; lpkrnpair: LPKERNINGPAIR): DWORD; stdcall;
{$ELSE}
function GetKerningPairs(hDc: HDC; nNumPairs: DWORD; lpkrnpair: LPKERNINGPAIR): DWORD; stdcall;
{$ENDIF}

function GetDCOrgEx(hdc: HDC; lpPoint: LPPOINT): BOOL; stdcall;
function FixBrushOrgEx(hDc: HDC; I1, I2: Integer; lpPoint: LPPOINT): BOOL; stdcall;
function UnrealizeObject(hgdiobj: HGDIOBJ): BOOL; stdcall;

function GdiFlush: BOOL; stdcall;
function GdiSetBatchLimit(dwLimit: DWORD): DWORD; stdcall;
function GdiGetBatchLimit: DWORD; stdcall;

const
  ICM_OFF            = 1;
  ICM_ON             = 2;
  ICM_QUERY          = 3;
  ICM_DONE_OUTSIDEDC = 4;

type
  ICMENUMPROCA = function (lpszFileName: LPSTR; lParam: LPARAM): Integer; stdcall;
  ICMENUMPROCW = function (lpszFileName: LPWSTR; lParam: LPARAM): Integer; stdcall;

{$IFDEF UNICODE}
  ICMENUMPROC = function (lpszFileName: LPWSTR; lParam: LPARAM): Integer; stdcall;
{$ELSE}
  ICMENUMPROC = function (lpszFileName: LPSTR; lParam: LPARAM): Integer; stdcall;
{$ENDIF}

function SetICMMode(hDc: HDC; iEnableICM: Integer): Integer; stdcall;
function CheckColorsInGamut(hDc: HDC; lpRGBTriples, lpBuffer: LPVOID; nCount: DWORD): BOOL; stdcall;
function GetColorSpace(hDc: HDC): HCOLORSPACE; stdcall;

function GetLogColorSpaceA(hColorSpace: HCOLORSPACE; lpBuffer: LPLOGCOLORSPACEA; nSize: DWORD): BOOL; stdcall;
function GetLogColorSpaceW(hColorSpace: HCOLORSPACE; lpBuffer: LPLOGCOLORSPACEW; nSize: DWORD): BOOL; stdcall;

{$IFDEF UNICODE}
function GetLogColorSpace(hColorSpace: HCOLORSPACE; lpBuffer: LPLOGCOLORSPACEW; nSize: DWORD): BOOL; stdcall;
{$ELSE}
function GetLogColorSpace(hColorSpace: HCOLORSPACE; lpBuffer: LPLOGCOLORSPACEA; nSize: DWORD): BOOL; stdcall;
{$ENDIF}

function CreateColorSpaceA(lpLogColorSpace: LPLOGCOLORSPACEA): HCOLORSPACE; stdcall;
function CreateColorSpaceW(lpLogColorSpace: LPLOGCOLORSPACEW): HCOLORSPACE; stdcall;

{$IFDEF UNICODE}
function CreateColorSpace(lpLogColorSpace: LPLOGCOLORSPACEW): HCOLORSPACE; stdcall;
{$ELSE}
function CreateColorSpace(lpLogColorSpace: LPLOGCOLORSPACEA): HCOLORSPACE; stdcall;
{$ENDIF}

function SetColorSpace(hDc: HDC; hColorSpace: HCOLORSPACE): HCOLORSPACE; stdcall;
function DeleteColorSpace(hColorSpace: HCOLORSPACE): BOOL; stdcall;
function GetICMProfileA(hDc: HDC; lpcbName: LPDWORD; lpszFilename: LPSTR): BOOL; stdcall;
function GetICMProfileW(hDc: HDC; lpcbName: LPDWORD; lpszFilename: LPWSTR): BOOL; stdcall;

{$IFDEF UNICODE}
function GetICMProfile(hDc: HDC; lpcbName: LPDWORD; lpszFilename: LPWSTR): BOOL; stdcall;
{$ELSE}
function GetICMProfile(hDc: HDC; lpcbName: LPDWORD; lpszFilename: LPSTR): BOOL; stdcall;
{$ENDIF}

function SetICMProfileA(hDc: HDC; lpFileName: LPSTR): BOOL; stdcall;
function SetICMProfileW(hDc: HDC; lpFileName: LPWSTR): BOOL; stdcall;

{$IFDEF UNICODE}
function SetICMProfile(hDc: HDC; lpFileName: LPWSTR): BOOL; stdcall;
{$ELSE}
function SetICMProfile(hDc: HDC; lpFileName: LPSTR): BOOL; stdcall;
{$ENDIF}

function GetDeviceGammaRamp(hDc: HDC; lpRamp: LPVOID): BOOL; stdcall;
function SetDeviceGammaRamp(hDc: HDC; lpRamp: LPVOID): BOOL; stdcall;
function ColorMatchToTarget(hDc, hdcTarget: HDC; uiAction: DWORD): BOOL; stdcall;

function EnumICMProfilesA(hDc: HDC; lpEnumProc: ICMENUMPROCA; lParam: LPARAM): Integer; stdcall;
function EnumICMProfilesW(hDc: HDC; lpEnumProc: ICMENUMPROCW; lParam: LPARAM): Integer; stdcall;

{$IFDEF UNICODE}
function EnumICMProfiles(hDc: HDC; lpEnumProc: ICMENUMPROCW; lParam: LPARAM): Integer; stdcall;
{$ELSE}
function EnumICMProfiles(hDc: HDC; lpEnumProc: ICMENUMPROCA; lParam: LPARAM): Integer; stdcall;
{$ENDIF}

function UpdateICMRegKeyA(dwReserved: DWORD; lpszCMID, lpszFileName: LPSTR; nCommand: UINT): BOOL; stdcall;
function UpdateICMRegKeyW(dwReserved: DWORD; lpszCMID, lpszFileName: LPWSTR; nCommand: UINT): BOOL; stdcall;

{$IFDEF UNICODE}
function UpdateICMRegKey(dwReserved: DWORD; lpszCMID, lpszFileName: LPWSTR; nCommand: UINT): BOOL; stdcall;
{$ELSE}
function UpdateICMRegKey(dwReserved: DWORD; lpszCMID, lpszFileName: LPSTR; nCommand: UINT): BOOL; stdcall;
{$ENDIF}

function ColorCorrectPalette(hDc: HDC; hColorPalette: HPALETTE; dwFirstEntry, dwNumOfEntries: DWORD): BOOL; stdcall;

// Enhanced metafile constants.

const
  ENHMETA_SIGNATURE = $464D4520;

// Stock object flag used in the object handle index in the enhanced
// metafile records.
// E.g. The object handle index (META_STOCK_OBJECT | BLACK_BRUSH)
// represents the stock object BLACK_BRUSH.

  ENHMETA_STOCK_OBJECT = DWORD($80000000);

// Enhanced metafile record types.

  EMR_HEADER               = 1;
  EMR_POLYBEZIER           = 2;
  EMR_POLYGON              = 3;
  EMR_POLYLINE             = 4;
  EMR_POLYBEZIERTO         = 5;
  EMR_POLYLINETO           = 6;
  EMR_POLYPOLYLINE         = 7;
  EMR_POLYPOLYGON          = 8;
  EMR_SETWINDOWEXTEX       = 9;
  EMR_SETWINDOWORGEX       = 10;
  EMR_SETVIEWPORTEXTEX     = 11;
  EMR_SETVIEWPORTORGEX     = 12;
  EMR_SETBRUSHORGEX        = 13;
  EMR_EOF                  = 14;
  EMR_SETPIXELV            = 15;
  EMR_SETMAPPERFLAGS       = 16;
  EMR_SETMAPMODE           = 17;
  EMR_SETBKMODE            = 18;
  EMR_SETPOLYFILLMODE      = 19;
  EMR_SETROP2              = 20;
  EMR_SETSTRETCHBLTMODE    = 21;
  EMR_SETTEXTALIGN         = 22;
  EMR_SETCOLORADJUSTMENT   = 23;
  EMR_SETTEXTCOLOR         = 24;
  EMR_SETBKCOLOR           = 25;
  EMR_OFFSETCLIPRGN        = 26;
  EMR_MOVETOEX             = 27;
  EMR_SETMETARGN           = 28;
  EMR_EXCLUDECLIPRECT      = 29;
  EMR_INTERSECTCLIPRECT    = 30;
  EMR_SCALEVIEWPORTEXTEX   = 31;
  EMR_SCALEWINDOWEXTEX     = 32;
  EMR_SAVEDC               = 33;
  EMR_RESTOREDC            = 34;
  EMR_SETWORLDTRANSFORM    = 35;
  EMR_MODIFYWORLDTRANSFORM = 36;
  EMR_SELECTOBJECT         = 37;
  EMR_CREATEPEN            = 38;
  EMR_CREATEBRUSHINDIRECT  = 39;
  EMR_DELETEOBJECT         = 40;
  EMR_ANGLEARC             = 41;
  EMR_ELLIPSE              = 42;
  EMR_RECTANGLE            = 43;
  EMR_ROUNDRECT            = 44;
  EMR_ARC                  = 45;
  EMR_CHORD                = 46;
  EMR_PIE                  = 47;
  EMR_SELECTPALETTE        = 48;
  EMR_CREATEPALETTE        = 49;
  EMR_SETPALETTEENTRIES    = 50;
  EMR_RESIZEPALETTE        = 51;
  EMR_REALIZEPALETTE       = 52;
  EMR_EXTFLOODFILL         = 53;
  EMR_LINETO               = 54;
  EMR_ARCTO                = 55;
  EMR_POLYDRAW             = 56;
  EMR_SETARCDIRECTION      = 57;
  EMR_SETMITERLIMIT        = 58;
  EMR_BEGINPATH            = 59;
  EMR_ENDPATH              = 60;
  EMR_CLOSEFIGURE          = 61;
  EMR_FILLPATH             = 62;
  EMR_STROKEANDFILLPATH    = 63;
  EMR_STROKEPATH           = 64;
  EMR_FLATTENPATH          = 65;
  EMR_WIDENPATH            = 66;
  EMR_SELECTCLIPPATH       = 67;
  EMR_ABORTPATH            = 68;

  EMR_GDICOMMENT              = 70;
  EMR_FILLRGN                 = 71;
  EMR_FRAMERGN                = 72;
  EMR_INVERTRGN               = 73;
  EMR_PAINTRGN                = 74;
  EMR_EXTSELECTCLIPRGN        = 75;
  EMR_BITBLT                  = 76;
  EMR_STRETCHBLT              = 77;
  EMR_MASKBLT                 = 78;
  EMR_PLGBLT                  = 79;
  EMR_SETDIBITSTODEVICE       = 80;
  EMR_STRETCHDIBITS           = 81;
  EMR_EXTCREATEFONTINDIRECTW  = 82;
  EMR_EXTTEXTOUTA             = 83;
  EMR_EXTTEXTOUTW             = 84;
  EMR_POLYBEZIER16            = 85;
  EMR_POLYGON16               = 86;
  EMR_POLYLINE16              = 87;
  EMR_POLYBEZIERTO16          = 88;
  EMR_POLYLINETO16            = 89;
  EMR_POLYPOLYLINE16          = 90;
  EMR_POLYPOLYGON16           = 91;
  EMR_POLYDRAW16              = 92;
  EMR_CREATEMONOBRUSH         = 93;
  EMR_CREATEDIBPATTERNBRUSHPT = 94;
  EMR_EXTCREATEPEN            = 95;
  EMR_POLYTEXTOUTA            = 96;
  EMR_POLYTEXTOUTW            = 97;

  EMR_SETICMMODE       = 98;
  EMR_CREATECOLORSPACE = 99;
  EMR_SETCOLORSPACE    = 100;
  EMR_DELETECOLORSPACE = 101;
  EMR_GLSRECORD        = 102;
  EMR_GLSBOUNDEDRECORD = 103;
  EMR_PIXELFORMAT      = 104;

  EMR_RESERVED_105        = 105;
  EMR_RESERVED_106        = 106;
  EMR_RESERVED_107        = 107;
  EMR_RESERVED_108        = 108;
  EMR_RESERVED_109        = 109;
  EMR_RESERVED_110        = 110;
  EMR_COLORCORRECTPALETTE = 111;
  EMR_SETICMPROFILEA      = 112;
  EMR_SETICMPROFILEW      = 113;
  EMR_ALPHABLEND          = 114;
  EMR_SETLAYOUT           = 115;
  EMR_TRANSPARENTBLT      = 116;
  EMR_RESERVED_117        = 117;
  EMR_GRADIENTFILL        = 118;
  EMR_RESERVED_119        = 119;
  EMR_RESERVED_120        = 120;
  EMR_COLORMATCHTOTARGETW = 121;
  EMR_CREATECOLORSPACEW   = 122;

  EMR_MIN = 1;

{$IFDEF WINVER_0500_GREATER}
  EMR_MAX = 122;
{$ELSE}
{$IFDEF WINVER_0400_GREATER}
  EMR_MAX = 104;
{$ELSE}
  EMR_MAX = 97;
{$ENDIF}
{$ENDIF}

// Base record type for the enhanced metafile.

type
  PEmr = ^TEmr;
  tagEMR = record
    iType: DWORD; // Enhanced metafile record type
    nSize: DWORD; // Length of the record in bytes.
                  // This must be a multiple of 4.
  end;
  EMR = tagEMR;
  TEmr = EMR;

// Base text record type for the enhanced metafile.

  PEmrText = ^TEmrText;
  tagEMRTEXT = record
    ptlReference: POINTL;
    nChars: DWORD;
    offString: DWORD; // Offset to the string
    fOptions: DWORD;
    rcl: RECTL;
    offDx: DWORD; // Offset to the inter-character spacing array.
    // This is always given.
  end;
  EMRTEXT = tagEMRTEXT;
  TEmrText = EMRTEXT;

// Record structures for the enhanced metafile.

  PAbortPath = ^TAbortPath;
  tagABORTPATH = record
    emr: EMR;
  end;
  TAbortPath = tagABORTPATH;
  EMRABORTPATH = tagABORTPATH;
  PEMRABORTPATH = ^EMRABORTPATH;
  EMRBEGINPATH = tagABORTPATH;
  PEMRBEGINPATH = ^EMRBEGINPATH;
  EMRENDPATH = tagABORTPATH;
  PEMRENDPATH = ^EMRENDPATH;
  EMRCLOSEFIGURE = tagABORTPATH;
  PEMRCLOSEFIGURE = ^EMRCLOSEFIGURE;
  EMRFLATTENPATH = tagABORTPATH;
  PEMRFLATTENPATH = ^EMRFLATTENPATH;
  EMRWIDENPATH = tagABORTPATH;
  PEMRWIDENPATH = ^EMRWIDENPATH;
  EMRSETMETARGN = tagABORTPATH;
  PEMRSETMETARGN = ^EMRSETMETARGN;
  EMRSAVEDC = tagABORTPATH;
  PEMRSAVEDC = ^EMRSAVEDC;
  EMRREALIZEPALETTE = tagABORTPATH;
  PEMRREALIZEPALETTE = ^EMRREALIZEPALETTE;

  PEmrSelectClipPath = ^TEmrSelectClipPath;
  tagEMRSELECTCLIPPATH = record
    emr: EMR;
    iMode: DWORD;
  end;
  EMRSELECTCLIPPATH = tagEMRSELECTCLIPPATH;
  LPEMRSELECTCLIPPATH = ^EMRSELECTCLIPPATH;
  TEmrSelectClipPath = EMRSELECTCLIPPATH;

  EMRSETBKMODE = tagEMRSELECTCLIPPATH;
  PEMRSETBKMODE = ^EMRSETBKMODE;
  EMRSETMAPMODE = tagEMRSELECTCLIPPATH;
  PEMRSETMAPMODE = ^EMRSETMAPMODE;
  EMRSETLAYOUT = tagEMRSELECTCLIPPATH;
  PEMRSETLAYOUT = ^EMRSETLAYOUT;
  EMRSETPOLYFILLMODE = tagEMRSELECTCLIPPATH;
  PEMRSETPOLYFILLMODE = EMRSETPOLYFILLMODE;
  EMRSETROP2 = tagEMRSELECTCLIPPATH;
  PEMRSETROP2 = ^EMRSETROP2;
  EMRSETSTRETCHBLTMODE = tagEMRSELECTCLIPPATH;
  PEMRSETSTRETCHBLTMODE = ^EMRSETSTRETCHBLTMODE;
  EMRSETICMMODE = tagEMRSELECTCLIPPATH;
  PEMRSETICMMODE = ^EMRSETICMMODE;
  EMRSETTEXTALIGN = tagEMRSELECTCLIPPATH;
  PEMRSETTEXTALIGN = ^EMRSETTEXTALIGN;

  PEmrSetMiterLimit = ^TEmrSetMiterLimit;
  tagEMRSETMITERLIMIT = record
    emr: EMR;
    eMiterLimit: FLOAT;
  end;
  EMRSETMITERLIMIT = tagEMRSETMITERLIMIT;
  TEmrSetMiterLimit = EMRSETMITERLIMIT;

  PEmrRestoreDc = ^TEmrRestoreDc;
  tagEMRRESTOREDC = record
    emr: EMR;
    iRelative: LONG; // Specifies a relative instance
  end;
  EMRRESTOREDC = tagEMRRESTOREDC;
  TEmrRestoreDc = EMRRESTOREDC;

  PEmrSetArcDirection = ^TEmrSetArcDirection;
  tagEMRSETARCDIRECTION = record
    emr: EMR;
    iArcDirection: DWORD; // Specifies the arc direction in the
    // advanced graphics mode.
  end;
  EMRSETARCDIRECTION = tagEMRSETARCDIRECTION;
  TEmrSetArcDirection = EMRSETARCDIRECTION;

  PEmrSetMapperFlags = ^TEmrSetMapperFlags;
  tagEMRSETMAPPERFLAGS = record
    emr: EMR;
    dwFlags: DWORD;
  end;
  EMRSETMAPPERFLAGS = tagEMRSETMAPPERFLAGS;
  TEmrSetMapperFlags = EMRSETMAPPERFLAGS;

  PEmrSetTextColor = ^TEmrSetTextColor;
  tagEMRSETTEXTCOLOR = record
    emr: EMR;
    crColor: COLORREF;
  end;
  EMRSETTEXTCOLOR = tagEMRSETTEXTCOLOR;
  EMRSETBKCOLOR = tagEMRSETTEXTCOLOR;
  PEMRSETBKCOLOR = ^EMRSETTEXTCOLOR;
  TEmrSetTextColor = EMRSETTEXTCOLOR;

  PEmrSelectObject = ^TEmrSelectObject;
  tagEMRSELECTOBJECT = record
    emr: EMR;
    ihObject: DWORD; // Object handle index
  end;
  EMRSELECTOBJECT = tagEMRSELECTOBJECT;
  EMRDELETEOBJECT = tagEMRSELECTOBJECT;
  PEMRDELETEOBJECT = ^EMRDELETEOBJECT;
  TEmrSelectObject = EMRSELECTOBJECT;

  PEmrSelectPalette = ^TEmrSelectPalette;
  tagEMRSELECTPALETTE = record
    emr: EMR;
    ihPal: DWORD; // Palette handle index, background mode only
  end;
  EMRSELECTPALETTE = tagEMRSELECTPALETTE;
  TEmrSelectPalette = EMRSELECTPALETTE;

  PEmrResizePalette = ^TEmrResizePalette;
  tagEMRRESIZEPALETTE = record
    emr: EMR;
    ihPal: DWORD; // Palette handle index
    cEntries: DWORD;
  end;
  EMRRESIZEPALETTE = tagEMRRESIZEPALETTE;
  TEmrResizePalette = EMRRESIZEPALETTE;

  PEmrSetPaletteEntries = ^TEmrSetPaletteEntries;
  tagEMRSETPALETTEENTRIES = record
    emr: EMR;
    ihPal: DWORD; // Palette handle index
    iStart: DWORD;
    cEntries: DWORD;
    aPalEntries: array [0..0] of PALETTEENTRY; // The peFlags fields do not contain any flags
  end;
  EMRSETPALETTEENTRIES = tagEMRSETPALETTEENTRIES;
  TEmrSetPaletteEntries = EMRSETPALETTEENTRIES;

  PEmrSetColorAdjustment = ^TEmrSetColorAdjustment;
  tagEMRSETCOLORADJUSTMENT = record
    emr: EMR;
    ColorAdjustment: COLORADJUSTMENT;
  end;
  EMRSETCOLORADJUSTMENT = tagEMRSETCOLORADJUSTMENT;
  TEmrSetColorAdjustment = EMRSETCOLORADJUSTMENT;

  PEmrGdiComment = ^TEmrGdiComment;
  tagEMRGDICOMMENT = record
    emr: EMR;
    cbData: DWORD; // Size of data in bytes
    Data: array [0..0] of BYTE;
  end;
  EMRGDICOMMENT = tagEMRGDICOMMENT;
  TEmrGdiComment = EMRGDICOMMENT;

  PEmrEof = ^TEmrEof;
  tagEMREOF = record
    emr: EMR;
    nPalEntries: DWORD; // Number of palette entries
    offPalEntries: DWORD; // Offset to the palette entries
    nSizeLast: DWORD; // Same as nSize and must be the last DWORD
    // of the record.  The palette entries,
    // if exist, precede this field.
  end;
  EMREOF = tagEMREOF;
  TEmrEof = EMREOF;

  PEmrLineTo = ^TEmrLineTo;
  tagEMRLINETO = record
    emr: EMR;
    ptl: POINTL;
  end;
  EMRLINETO = tagEMRLINETO;
  EMRMOVETOEX = tagEMRLINETO;
  PEMRMOVETOEX = ^EMRMOVETOEX;
  TEmrLineTo = EMRLINETO;

  PEmrOffsetClipRgn = ^TEmrOffsetClipRgn;
  tagEMROFFSETCLIPRGN = record
    emr: EMR;
    ptlOffset: POINTL;
  end;
  EMROFFSETCLIPRGN = tagEMROFFSETCLIPRGN;
  TEmrOffsetClipRgn = EMROFFSETCLIPRGN;

  PEmrFillPath = ^TEmrFillPath;
  tagEMRFILLPATH = record
    emr: EMR;
    rclBounds: RECTL; // Inclusive-inclusive bounds in device units
  end;
  EMRFILLPATH = tagEMRFILLPATH;
  EMRSTROKEANDFILLPATH = tagEMRFILLPATH;
  PEMRSTROKEANDFILLPATH = ^EMRSTROKEANDFILLPATH;
  EMRSTROKEPATH = tagEMRFILLPATH;
  PEMRSTROKEPATH = ^EMRSTROKEPATH;
  TEmrFillPath = EMRFILLPATH;

  PEmrExcludeClipRect = ^TEmrExcludeClipRect;
  tagEMREXCLUDECLIPRECT = record
    emr: EMR;
    rclClip: RECTL;
  end;
  EMREXCLUDECLIPRECT = tagEMREXCLUDECLIPRECT;
  EMRINTERSECTCLIPRECT = tagEMREXCLUDECLIPRECT;
  PEMRINTERSECTCLIPRECT = ^EMRINTERSECTCLIPRECT;
  TEmrExcludeClipRect = EMREXCLUDECLIPRECT;

  PEmrSetViewPortOrgEx = ^TEmrSetViewPortOrgEx;
  tagEMRSETVIEWPORTORGEX = record
    emr: EMR;
    ptlOrigin: POINTL;
  end;
  EMRSETVIEWPORTORGEX = tagEMRSETVIEWPORTORGEX;
  EMRSETWINDOWORGEX = tagEMRSETVIEWPORTORGEX;
  PEMRSETWINDOWORGEX = ^EMRSETWINDOWORGEX;
  EMRSETBRUSHORGEX = tagEMRSETVIEWPORTORGEX;
  PEMRSETBRUSHORGEX = ^EMRSETBRUSHORGEX;
  TEmrSetViewPortOrgEx = EMRSETVIEWPORTORGEX;

  PEmrSetViewPortExtEx = ^TEmrSetViewPortExtEx;
  tagEMRSETVIEWPORTEXTEX = record
    emr: EMR;
    szlExtent: SIZEL;
  end;
  EMRSETVIEWPORTEXTEX = tagEMRSETVIEWPORTEXTEX;
  EMRSETWINDOWEXTEX = tagEMRSETVIEWPORTEXTEX;
  TEmrSetViewPortExtEx = EMRSETVIEWPORTEXTEX;

  PEmrScaleViewPortExtEx = ^TEmrScaleViewPortExtEx;
  tagEMRSCALEVIEWPORTEXTEX = record
    emr: EMR;
    xNum: LONG;
    xDenom: LONG;
    yNum: LONG;
    yDenom: LONG;
  end;
  EMRSCALEVIEWPORTEXTEX = tagEMRSCALEVIEWPORTEXTEX;
  EMRSCALEWINDOWEXTEX = tagEMRSCALEVIEWPORTEXTEX;
  PEMRSCALEWINDOWEXTEX = ^EMRSCALEWINDOWEXTEX;
  TEmrScaleViewPortExtEx = EMRSCALEVIEWPORTEXTEX;

  PEmrSetWorldTransform = ^TEmrSetWorldTransform;
  tagEMRSETWORLDTRANSFORM = record
    emr: EMR;
    xform: XFORM;
  end;
  EMRSETWORLDTRANSFORM = tagEMRSETWORLDTRANSFORM;
  TEmrSetWorldTransform = EMRSETWORLDTRANSFORM;

  PEmrModifyWorldTransform = ^TEmrModifyWorldTransform;
  tagEMRMODIFYWORLDTRANSFORM = record
    emr: EMR;
    xform: XFORM;
    iMode: DWORD;
  end;
  EMRMODIFYWORLDTRANSFORM = tagEMRMODIFYWORLDTRANSFORM;
  TEmrModifyWorldTransform = EMRMODIFYWORLDTRANSFORM;

  PEmrSetPixelV = ^TEmrSetPixelV;
  tagEMRSETPIXELV = record
    emr: EMR;
    ptlPixel: POINTL;
    crColor: COLORREF;
  end;
  EMRSETPIXELV = tagEMRSETPIXELV;
  TEmrSetPixelV = EMRSETPIXELV;

  PEmrExtFloodFill = ^TEmrExtFloodFill;
  tagEMREXTFLOODFILL = record
    emr: EMR;
    ptlStart: POINTL;
    crColor: COLORREF;
    iMode: DWORD;
  end;
  EMREXTFLOODFILL = tagEMREXTFLOODFILL;
  TEmrExtFloodFill = EMREXTFLOODFILL;

  PEmrEllipse = ^TEmrEllipse;
  tagEMRELLIPSE = record
    emr: EMR;
    rclBox: RECTL; // Inclusive-inclusive bounding rectangle
  end;
  EMRELLIPSE = tagEMRELLIPSE;
  EMRRECTANGLE = tagEMRELLIPSE;
  PEMRRECTANGLE = ^EMRRECTANGLE;
  TEmrEllipse = EMRELLIPSE;

  PEmrRoundRect = ^TEmrRoundRect;
  tagEMRROUNDRECT = record
    emr: EMR;
    rclBox: RECTL; // Inclusive-inclusive bounding rectangle
    szlCorner: SIZEL;
  end;
  EMRROUNDRECT = tagEMRROUNDRECT;
  TEmrRoundRect = EMRROUNDRECT;

  PEmrArc = ^TEmrArc;
  tagEMRARC = record
    emr: EMR;
    rclBox: RECTL; // Inclusive-inclusive bounding rectangle
    ptlStart: POINTL;
    ptlEnd: POINTL;
  end;
  EMRARC = tagEMRARC;
  EMRARCTO = tagEMRARC;
  PEMRARCTO = ^EMRARCTO;
  EMRCHORD = tagEMRARC;
  PEMRCHORD = ^EMRCHORD;
  EMRPIE = tagEMRARC;
  PEMRPIE = ^EMRPIE;
  TEmrArc = EMRARC;

  PEmrAngleArc = ^TEmrAngleArc;
  tagEMRANGLEARC = record
    emr: EMR;
    ptlCenter: POINTL;
    nRadius: DWORD;
    eStartAngle: FLOAT;
    eSweepAngle: FLOAT;
  end;
  EMRANGLEARC = tagEMRANGLEARC;
  TEmrAngleArc = EMRANGLEARC;

  PEmrPolyline = ^TEmrPolyline;
  tagEMRPOLYLINE = record
    emr: EMR;
    rclBounds: RECTL; // Inclusive-inclusive bounds in device units
    cptl: DWORD;
    aptl: array [0..0] of POINTL;
  end;
  EMRPOLYLINE = tagEMRPOLYLINE;
  EMRPOLYBEZIER = tagEMRPOLYLINE;
  PEMRPOLYBEZIER = ^EMRPOLYBEZIER;
  EMRPOLYGON = tagEMRPOLYLINE;
  PEMRPOLYGON = ^EMRPOLYGON;
  EMRPOLYBEZIERTO = tagEMRPOLYLINE;
  PEMRPOLYBEZIERTO = ^EMRPOLYBEZIERTO;
  EMRPOLYLINETO = tagEMRPOLYLINE;
  PEMRPOLYLINETO = ^EMRPOLYLINETO;
  TEmrPolyline = EMRPOLYLINE;

  PEmrPolyline16 = ^TEmrPolyline16;
  tagEMRPOLYLINE16 = record
    emr: EMR;
    rclBounds: RECTL; // Inclusive-inclusive bounds in device units
    cpts: DWORD;
    apts: array [0..0] of POINTS;
  end;
  EMRPOLYLINE16 = tagEMRPOLYLINE16;
  EMRPOLYBEZIER16 = tagEMRPOLYLINE16;
  PEMRPOLYBEZIER16 = ^EMRPOLYBEZIER16;
  EMRPOLYGON16 = tagEMRPOLYLINE16;
  PEMRPOLYGON16 = ^EMRPOLYGON16;
  EMRPOLYBEZIERTO16 = tagEMRPOLYLINE16;
  PEMRPOLYBEZIERTO16 = ^EMRPOLYBEZIERTO16;
  EMRPOLYLINETO16 = tagEMRPOLYLINE16;
  PEMRPOLYLINETO16 = ^EMRPOLYLINETO16;
  TEmrPolyline16 = EMRPOLYLINE16;

  PEmrPolyDraw = ^TEmrPolyDraw;
  tagEMRPOLYDRAW = record
    emr: EMR;
    rclBounds: RECTL; // Inclusive-inclusive bounds in device units
    cptl: DWORD; // Number of points
    aptl: array [0..0] of POINTL; // Array of points
    abTypes: array [0..0] of BYTE; // Array of point types
  end;
  EMRPOLYDRAW = tagEMRPOLYDRAW;
  TEmrPolyDraw = EMRPOLYDRAW;

  PEmrPolyDraw16 = ^TEmrPolyDraw16;
  tagEMRPOLYDRAW16 = record
    emr: EMR;
    rclBounds: RECTL; // Inclusive-inclusive bounds in device units
    cpts: DWORD; // Number of points
    apts: array [0..0] of POINTS; // Array of points
    abTypes: array [0..0] of BYTE; // Array of point types
  end;
  EMRPOLYDRAW16 = tagEMRPOLYDRAW16;
  TEmrPolyDraw16 = EMRPOLYDRAW16;

  PEmrPolyPolyline = ^TEmrPolyPolyline;
  tagEMRPOLYPOLYLINE = record
    emr: EMR;
    rclBounds: RECTL; // Inclusive-inclusive bounds in device units
    nPolys: DWORD; // Number of polys
    cptl: DWORD; // Total number of points in all polys
    aPolyCounts: array [0..0] of DWORD; // Array of point counts for each poly
    aptl: array [0..0] of POINTL; // Array of points
  end;
  EMRPOLYPOLYLINE = tagEMRPOLYPOLYLINE;
  EMRPOLYPOLYGON = tagEMRPOLYPOLYLINE;
  TEmrPolyPolyline = EMRPOLYPOLYLINE;

  PEmrPolyPolyline16 = ^TEmrPolyPolyline16;
  tagEMRPOLYPOLYLINE16 = record
    emr: EMR;
    rclBounds: RECTL; // Inclusive-inclusive bounds in device units
    nPolys: DWORD; // Number of polys
    cpts: DWORD; // Total number of points in all polys
    aPolyCounts: array [0..0] of DWORD; // Array of point counts for each poly
    apts: array [0..0] of POINTS; // Array of points
  end;
  EMRPOLYPOLYLINE16 = tagEMRPOLYPOLYLINE16;
  EMRPOLYPOLYGON16 = tagEMRPOLYPOLYLINE16;
  PEMRPOLYPOLYGON16 = ^EMRPOLYPOLYGON16;
  TEmrPolyPolyline16 = EMRPOLYPOLYLINE16;

  PEmrInvertRgn = ^TEmrInvertRgn;
  tagEMRINVERTRGN = record
    emr: EMR;
    rclBounds: RECTL; // Inclusive-inclusive bounds in device units
    cbRgnData: DWORD; // Size of region data in bytes
    RgnData: array [0..0] of BYTE;
  end;
  EMRINVERTRGN = tagEMRINVERTRGN;
  EMRPAINTRGN = tagEMRINVERTRGN;
  TEmrInvertRgn = EMRINVERTRGN;

  PEmrFillRgn = ^TEmrFillRgn;
  tagEMRFILLRGN = record
    emr: EMR;
    rclBounds: RECTL; // Inclusive-inclusive bounds in device units
    cbRgnData: DWORD; // Size of region data in bytes
    ihBrush: DWORD; // Brush handle index
    RgnData: array [0..0] of BYTE;
  end;
  EMRFILLRGN = tagEMRFILLRGN;
  TEmrFillRgn = EMRFILLRGN;

  PEmrFrameRgn = ^TEmrFrameRgn;
  tagEMRFRAMERGN = record
    emr: EMR;
    rclBounds: RECTL; // Inclusive-inclusive bounds in device units
    cbRgnData: DWORD; // Size of region data in bytes
    ihBrush: DWORD; // Brush handle index
    szlStroke: SIZEL;
    RgnData: array [0..0] of BYTE;
  end;
  EMRFRAMERGN = tagEMRFRAMERGN;
  TEmrFrameRgn = EMRFRAMERGN;

  PEmrExtSelectClipRgn = ^TEmrExtSelectClipRgn;
  tagEMREXTSELECTCLIPRGN = record
    emr: EMR;
    cbRgnData: DWORD; // Size of region data in bytes
    iMode: DWORD;
    RgnData: array [0..0] of BYTE;
  end;
  EMREXTSELECTCLIPRGN = tagEMREXTSELECTCLIPRGN;
  TEmrExtSelectClipRgn = EMREXTSELECTCLIPRGN;

  PEmrExtTextOutA = ^TEmrExtTextOutA;
  tagEMREXTTEXTOUTA = record
    emr: EMR;
    rclBounds: RECTL;     // Inclusive-inclusive bounds in device units
    iGraphicsMode: DWORD; // Current graphics mode
    exScale: FLOAT;       // X and Y scales from Page units to .01mm units
    eyScale: FLOAT;       // if graphics mode is GM_COMPATIBLE.
    emrtext: EMRTEXT;     // This is followed by the string and spacing array
  end;
  EMREXTTEXTOUTA = tagEMREXTTEXTOUTA;
  EMREXTTEXTOUTW = tagEMREXTTEXTOUTA;
  PEMREXTTEXTOUTW = ^EMREXTTEXTOUTW;
  TEmrExtTextOutA = EMREXTTEXTOUTA;

  PEmrPolyTextOutA = ^TEmrPolyTextOutA;
  tagEMRPOLYTEXTOUTA = record
    emr: EMR;
    rclBounds: RECTL; // Inclusive-inclusive bounds in device units
    iGraphicsMode: DWORD; // Current graphics mode
    exScale: FLOAT; // X and Y scales from Page units to .01mm units
    eyScale: FLOAT; // if graphics mode is GM_COMPATIBLE.
    cStrings: LONG;
    aemrtext: array [0..0] of EMRTEXT; // Array of EMRTEXT structures.  This is
    // followed by the strings and spacing arrays.
  end;
  EMRPOLYTEXTOUTA = tagEMRPOLYTEXTOUTA;
  EMRPOLYTEXTOUTW = tagEMRPOLYTEXTOUTA;
  PEMRPOLYTEXTOUTW = ^EMRPOLYTEXTOUTW;
  TEmrPolyTextOutA = EMRPOLYTEXTOUTA;

  PEmrBitBlt = ^TEmrBitBlt;
  tagEMRBITBLT = record
    emr: EMR;
    rclBounds: RECTL; // Inclusive-inclusive bounds in device units
    xDest: LONG;
    yDest: LONG;
    cxDest: LONG;
    cyDest: LONG;
    dwRop: DWORD;
    xSrc: LONG;
    ySrc: LONG;
    xformSrc: XFORM; // Source DC transform
    crBkColorSrc: COLORREF; // Source DC BkColor in RGB
    iUsageSrc: DWORD; // Source bitmap info color table usage
    // (DIB_RGB_COLORS)
    offBmiSrc: DWORD; // Offset to the source BITMAPINFO structure
    cbBmiSrc: DWORD; // Size of the source BITMAPINFO structure
    offBitsSrc: DWORD; // Offset to the source bitmap bits
    cbBitsSrc: DWORD; // Size of the source bitmap bits
  end;
  EMRBITBLT = tagEMRBITBLT;
  TEmrBitBlt = EMRBITBLT;

  PEmrStretchBlt = ^TEmrStretchBlt;
  tagEMRSTRETCHBLT = record
    emr: EMR;
    rclBounds: RECTL; // Inclusive-inclusive bounds in device units
    xDest: LONG;
    yDest: LONG;
    cxDest: LONG;
    cyDest: LONG;
    dwRop: DWORD;
    xSrc: LONG;
    ySrc: LONG;
    xformSrc: XFORM; // Source DC transform
    crBkColorSrc: COLORREF; // Source DC BkColor in RGB
    iUsageSrc: DWORD; // Source bitmap info color table usage
    // (DIB_RGB_COLORS)
    offBmiSrc: DWORD; // Offset to the source BITMAPINFO structure
    cbBmiSrc: DWORD; // Size of the source BITMAPINFO structure
    offBitsSrc: DWORD; // Offset to the source bitmap bits
    cbBitsSrc: DWORD; // Size of the source bitmap bits
    cxSrc: LONG;
    cySrc: LONG;
  end;
  EMRSTRETCHBLT = tagEMRSTRETCHBLT;
  TEmrStretchBlt = EMRSTRETCHBLT;

  PEmrMaskBlt = ^TEmrMaskBlt;
  tagEMRMASKBLT = record
    emr: EMR;
    rclBounds: RECTL; // Inclusive-inclusive bounds in device units
    xDest: LONG;
    yDest: LONG;
    cxDest: LONG;
    cyDest: LONG;
    dwRop: DWORD;
    xSrc: LONG;
    ySrc: LONG;
    xformSrc: XFORM; // Source DC transform
    crBkColorSrc: COLORREF; // Source DC BkColor in RGB
    iUsageSrc: DWORD; // Source bitmap info color table usage
    // (DIB_RGB_COLORS)
    offBmiSrc: DWORD; // Offset to the source BITMAPINFO structure
    cbBmiSrc: DWORD; // Size of the source BITMAPINFO structure
    offBitsSrc: DWORD; // Offset to the source bitmap bits
    cbBitsSrc: DWORD; // Size of the source bitmap bits
    xMask: LONG;
    yMask: LONG;
    iUsageMask: DWORD; // Mask bitmap info color table usage
    offBmiMask: DWORD; // Offset to the mask BITMAPINFO structure if any
    cbBmiMask: DWORD; // Size of the mask BITMAPINFO structure if any
    offBitsMask: DWORD; // Offset to the mask bitmap bits if any
    cbBitsMask: DWORD; // Size of the mask bitmap bits if any
  end;
  EMRMASKBLT = tagEMRMASKBLT;
  TEmrMaskBlt = EMRMASKBLT;

  PEmrPlgBlt = ^TEmrPlgBlt;
  tagEMRPLGBLT = record
    emr: EMR;
    rclBounds: RECTL; // Inclusive-inclusive bounds in device units
    aptlDest: array[0..2] of POINTL;
    xSrc: LONG;
    ySrc: LONG;
    cxSrc: LONG;
    cySrc: LONG;
    xformSrc: XFORM; // Source DC transform
    crBkColorSrc: COLORREF; // Source DC BkColor in RGB
    iUsageSrc: DWORD; // Source bitmap info color table usage
    // (DIB_RGB_COLORS)
    offBmiSrc: DWORD; // Offset to the source BITMAPINFO structure
    cbBmiSrc: DWORD; // Size of the source BITMAPINFO structure
    offBitsSrc: DWORD; // Offset to the source bitmap bits
    cbBitsSrc: DWORD; // Size of the source bitmap bits
    xMask: LONG;
    yMask: LONG;
    iUsageMask: DWORD; // Mask bitmap info color table usage
    offBmiMask: DWORD; // Offset to the mask BITMAPINFO structure if any
    cbBmiMask: DWORD; // Size of the mask BITMAPINFO structure if any
    offBitsMask: DWORD; // Offset to the mask bitmap bits if any
    cbBitsMask: DWORD; // Size of the mask bitmap bits if any
  end;
  EMRPLGBLT = tagEMRPLGBLT;
  TEmrPlgBlt = EMRPLGBLT;

  PEmrSetDiBitsToDevice = ^TEmrSetDiBitsToDevice;
  tagEMRSETDIBITSTODEVICE = record
    emr: EMR;
    rclBounds: RECTL; // Inclusive-inclusive bounds in device units
    xDest: LONG;
    yDest: LONG;
    xSrc: LONG;
    ySrc: LONG;
    cxSrc: LONG;
    cySrc: LONG;
    offBmiSrc: DWORD; // Offset to the source BITMAPINFO structure
    cbBmiSrc: DWORD; // Size of the source BITMAPINFO structure
    offBitsSrc: DWORD; // Offset to the source bitmap bits
    cbBitsSrc: DWORD; // Size of the source bitmap bits
    iUsageSrc: DWORD; // Source bitmap info color table usage
    iStartScan: DWORD;
    cScans: DWORD;
  end;
  EMRSETDIBITSTODEVICE = tagEMRSETDIBITSTODEVICE;
  TEmrSetDiBitsToDevice = EMRSETDIBITSTODEVICE;

  PEmrStretchDiBits = ^TEmrStretchDiBits;
  tagEMRSTRETCHDIBITS = record
    emr: EMR;
    rclBounds: RECTL; // Inclusive-inclusive bounds in device units
    xDest: LONG;
    yDest: LONG;
    xSrc: LONG;
    ySrc: LONG;
    cxSrc: LONG;
    cySrc: LONG;
    offBmiSrc: DWORD; // Offset to the source BITMAPINFO structure
    cbBmiSrc: DWORD; // Size of the source BITMAPINFO structure
    offBitsSrc: DWORD; // Offset to the source bitmap bits
    cbBitsSrc: DWORD; // Size of the source bitmap bits
    iUsageSrc: DWORD; // Source bitmap info color table usage
    dwRop: DWORD;
    cxDest: LONG;
    cyDest: LONG;
  end;
  EMRSTRETCHDIBITS = tagEMRSTRETCHDIBITS;
  TEmrStretchDiBits = EMRSTRETCHDIBITS;

  PEmrExtCreateFontIndirectW = ^TEmrExtCreateFontIndirectW;
  tagEMREXTCREATEFONTINDIRECTW = record
    emr: EMR;
    ihFont: DWORD; // Font handle index
    elfw: EXTLOGFONTW;
  end;
  EMREXTCREATEFONTINDIRECTW = tagEMREXTCREATEFONTINDIRECTW;
  TEmrExtCreateFontIndirectW = EMREXTCREATEFONTINDIRECTW;

  PEmrCreatePalette = ^TEmrCreatePalette;
  tagEMRCREATEPALETTE = record
    emr: EMR;
    ihPal: DWORD; // Palette handle index
    lgpl: LOGPALETTE; // The peFlags fields in the palette entries
    // do not contain any flags
  end;
  EMRCREATEPALETTE = tagEMRCREATEPALETTE;
  TEmrCreatePalette = EMRCREATEPALETTE;

  PEmrCreatePen = ^TEmrCreatePen;
  tagEMRCREATEPEN = record
    emr: EMR;
    ihPen: DWORD; // Pen handle index
    lopn: LOGPEN;
  end;
  EMRCREATEPEN = tagEMRCREATEPEN;
  TEmrCreatePen = EMRCREATEPEN;

  PEmrExtCreatePen = ^TEmrExtCreatePen;
  tagEMREXTCREATEPEN = record
    emr: EMR;
    ihPen: DWORD; // Pen handle index
    offBmi: DWORD; // Offset to the BITMAPINFO structure if any
    cbBmi: DWORD; // Size of the BITMAPINFO structure if any
    // The bitmap info is followed by the bitmap
    // bits to form a packed DIB.
    offBits: DWORD; // Offset to the brush bitmap bits if any
    cbBits: DWORD; // Size of the brush bitmap bits if any
    elp: EXTLOGPEN; // The extended pen with the style array.
  end;
  EMREXTCREATEPEN = tagEMREXTCREATEPEN;
  TEmrExtCreatePen = EMREXTCREATEPEN;

  PEmrCreateBrushIndirect = ^TEmrCreateBrushIndirect;
  tagEMRCREATEBRUSHINDIRECT = record
    emr: EMR;
    ihBrush: DWORD; // Brush handle index
    lb: LOGBRUSH32; // The style must be BS_SOLID, BS_HOLLOW,
    // BS_NULL or BS_HATCHED.
  end;
  EMRCREATEBRUSHINDIRECT = tagEMRCREATEBRUSHINDIRECT;
  TEmrCreateBrushIndirect = EMRCREATEBRUSHINDIRECT;

  PEmrCreateMonoBrush = ^TEmrCreateMonoBrush;
  tagEMRCREATEMONOBRUSH = record
    emr: EMR;
    ihBrush: DWORD; // Brush handle index
    iUsage: DWORD; // Bitmap info color table usage
    offBmi: DWORD; // Offset to the BITMAPINFO structure
    cbBmi: DWORD; // Size of the BITMAPINFO structure
    offBits: DWORD; // Offset to the bitmap bits
    cbBits: DWORD; // Size of the bitmap bits
  end;
  EMRCREATEMONOBRUSH = tagEMRCREATEMONOBRUSH;
  TEmrCreateMonoBrush = EMRCREATEMONOBRUSH;

  PEmrCreateDibPatternBrushPt = ^TEmrCreateDibPatternBrushPt;
  tagEMRCREATEDIBPATTERNBRUSHPT = record
    emr: EMR;
    ihBrush: DWORD; // Brush handle index
    iUsage: DWORD; // Bitmap info color table usage
    offBmi: DWORD; // Offset to the BITMAPINFO structure
    cbBmi: DWORD; // Size of the BITMAPINFO structure
    // The bitmap info is followed by the bitmap
    // bits to form a packed DIB.
    offBits: DWORD; // Offset to the bitmap bits
    cbBits: DWORD; // Size of the bitmap bits
  end;
  EMRCREATEDIBPATTERNBRUSHPT = tagEMRCREATEDIBPATTERNBRUSHPT;
  TEmrCreateDibPatternBrushPt = EMRCREATEDIBPATTERNBRUSHPT;

  PEmrFormat = ^TEmrFormat;
  tagEMRFORMAT = record
    dSignature: DWORD; // Format signature, e.g. ENHMETA_SIGNATURE.
    nVersion: DWORD; // Format version number.
    cbData: DWORD; // Size of data in bytes.
    offData: DWORD; // Offset to data from GDICOMMENT_IDENTIFIER.
    // It must begin at a DWORD offset.
  end;
  EMRFORMAT = tagEMRFORMAT;
  TEmrFormat = EMRFORMAT;

  PEmrGlsRecord = ^TEmrGlsRecord;
  tagEMRGLSRECORD = record
    emr: EMR;
    cbData: DWORD; // Size of data in bytes
    Data: array [0..0] of BYTE;
  end;
  EMRGLSRECORD = tagEMRGLSRECORD;
  TEmrGlsRecord = EMRGLSRECORD;

  PEmrGlsBoundedRecord = ^TEmrGlsBoundedRecord;
  tagEMRGLSBOUNDEDRECORD = record
    emr: EMR;
    rclBounds: RECTL; // Bounds in recording coordinates
    cbData: DWORD; // Size of data in bytes
    Data: array [0..0] of BYTE;
  end;
  EMRGLSBOUNDEDRECORD = tagEMRGLSBOUNDEDRECORD;
  TEmrGlsBoundedRecord = EMRGLSBOUNDEDRECORD;

  PEmrPixelFormat = ^TEmrPixelFormat;
  tagEMRPIXELFORMAT = record
    emr: EMR;
    pfd: PIXELFORMATDESCRIPTOR;
  end;
  EMRPIXELFORMAT = tagEMRPIXELFORMAT;
  TEmrPixelFormat = EMRPIXELFORMAT;

  PEmrCreateColorSpace = ^TEmrCreateColorSpace;
  tagEMRCREATECOLORSPACE = record
    emr: EMR;
    ihCS: DWORD; // ColorSpace handle index
    lcs: LOGCOLORSPACEA; // Ansi version of LOGCOLORSPACE
  end;
  EMRCREATECOLORSPACE = tagEMRCREATECOLORSPACE;
  TEmrCreateColorSpace = EMRCREATECOLORSPACE;

  PEmrSetColorSpace = ^TEmrSetColorSpace;
  tagEMRSETCOLORSPACE = record
    emr: EMR;
    ihCS: DWORD; // ColorSpace handle index
  end;
  EMRSETCOLORSPACE = tagEMRSETCOLORSPACE;
  EMRSELECTCOLORSPACE = tagEMRSETCOLORSPACE;
  PEMRSELECTCOLORSPACE = ^EMRSELECTCOLORSPACE;
  EMRDELETECOLORSPACE = tagEMRSETCOLORSPACE;
  PEMRDELETECOLORSPACE = ^EMRDELETECOLORSPACE;
  TEmrSetColorSpace = EMRSETCOLORSPACE;

  PEmrExtEscape = ^TEmrExtEscape;
  tagEMREXTESCAPE = record
    emr: EMR;
    iEscape: INT; // Escape code
    cbEscData: INT; // Size of escape data
    EscData: array [0..0] of BYTE; // Escape data
  end;
  EMREXTESCAPE = tagEMREXTESCAPE;
  EMRDRAWESCAPE = tagEMREXTESCAPE;
  PEMRDRAWESCAPE = ^EMRDRAWESCAPE;
  TEmrExtEscape = EMREXTESCAPE;

  PEmrNamedEscape = ^TEmrNamedEscape;
  tagEMRNAMEDESCAPE = record
    emr: EMR;
    iEscape: INT; // Escape code
    cbDriver: INT; // Size of driver name
    cbEscData: INT; // Size of escape data
    EscData: array [0..0] of BYTE; // Driver name and Escape data
  end;
  EMRNAMEDESCAPE = tagEMRNAMEDESCAPE;
  TEmrNamedEscape = EMRNAMEDESCAPE;

const
  SETICMPROFILE_EMBEDED = $00000001;

type
  PEmrSetIcmProfile = ^TEmrSetIcmProfile;
  tagEMRSETICMPROFILE = record
    emr: EMR;
    dwFlags: DWORD; // flags
    cbName: DWORD; // Size of desired profile name
    cbData: DWORD; // Size of raw profile data if attached
    Data: array [0..0] of BYTE; // Array size is cbName + cbData
  end;
  EMRSETICMPROFILE = tagEMRSETICMPROFILE;
  EMRSETICMPROFILEA = tagEMRSETICMPROFILE;
  PEMRSETICMPROFILEA = ^EMRSETICMPROFILEA;
  EMRSETICMPROFILEW = tagEMRSETICMPROFILE;
  PEMRSETICMPROFILEW = ^EMRSETICMPROFILEW;
  TEmrSetIcmProfile = EMRSETICMPROFILE;

const
  CREATECOLORSPACE_EMBEDED = $00000001;

type
  PEmrCreateColorSpaceW = ^TEmrCreateColorSpaceW;
  tagEMRCREATECOLORSPACEW = record
    emr: EMR;
    ihCS: DWORD; // ColorSpace handle index
    lcs: LOGCOLORSPACEW; // Unicode version of logical color space structure
    dwFlags: DWORD; // flags
    cbData: DWORD; // size of raw source profile data if attached
    Data: array [0..0] of BYTE; // Array size is cbData
  end;
  EMRCREATECOLORSPACEW = tagEMRCREATECOLORSPACEW;
  TEmrCreateColorSpaceW = EMRCREATECOLORSPACEW;

const
  COLORMATCHTOTARGET_EMBEDED = $00000001;

type
  PColorMatchToTarget = ^TColorMatchToTarget;
  tagCOLORMATCHTOTARGET = record
    emr: EMR;
    dwAction: DWORD;  // CS_ENABLE, CS_DISABLE or CS_DELETE_TRANSFORM
    dwFlags: DWORD;   // flags
    cbName: DWORD;    // Size of desired target profile name
    cbData: DWORD;    // Size of raw target profile data if attached
    Data: array [0..0] of BYTE; // Array size is cbName + cbData
  end;
  //COLORMATCHTOTARGET = tagCOLORMATCHTOTARGET;
  //{$EXTERNALSYM COLORMATCHTOTARGET}
  TColorMatchToTarget = tagCOLORMATCHTOTARGET;

  PColorCorrectPalette = ^TColorCorrectPalette;
  tagCOLORCORRECTPALETTE = record
    emr: EMR;
    ihPalette: DWORD;   // Palette handle index
    nFirstEntry: DWORD; // Index of first entry to correct
    nPalEntries: DWORD; // Number of palette entries to correct
    nReserved: DWORD;   // Reserved
  end;
  //COLORCORRECTPALETTE = tagCOLORCORRECTPALETTE;
  //{$EXTERNALSYM COLORCORRECTPALETTE}
  TColorCorrectPalette = tagCOLORCORRECTPALETTE;

  PEmrAlphaBlend = ^TEmrAlphaBlend;
  tagEMRALPHABLEND = record
    emr: EMR;
    rclBounds: RECTL;       // Inclusive-inclusive bounds in device units
    xDest: LONG;
    yDest: LONG;
    cxDest: LONG;
    cyDest: LONG;
    dwRop: DWORD;
    xSrc: LONG;
    ySrc: LONG;
    xformSrc: XFORM;        // Source DC transform
    crBkColorSrc: COLORREF; // Source DC BkColor in RGB
    iUsageSrc: DWORD;       // Source bitmap info color table usage (DIB_RGB_COLORS)
    offBmiSrc: DWORD;       // Offset to the source BITMAPINFO structure
    cbBmiSrc: DWORD;        // Size of the source BITMAPINFO structure
    offBitsSrc: DWORD;      // Offset to the source bitmap bits
    cbBitsSrc: DWORD;       // Size of the source bitmap bits
    cxSrc: LONG;
    cySrc: LONG;
  end;
  EMRALPHABLEND = tagEMRALPHABLEND;
  TEmrAlphaBlend = EMRALPHABLEND;

  PEmrGradientFill = ^TEmrGradientFill;
  tagEMRGRADIENTFILL = record
    emr: EMR;
    rclBounds: RECTL; // Inclusive-inclusive bounds in device units
    nVer: DWORD;
    nTri: DWORD;
    ulMode: ULONG;
    Ver: array [0..0] of TRIVERTEX;
  end;
  EMRGRADIENTFILL = tagEMRGRADIENTFILL;
  TEmrGradientFill = EMRGRADIENTFILL;

  PEmrTransparentBlt = ^TEmrTransparentBlt;
  tagEMRTRANSPARENTBLT = record
    emr: EMR;
    rclBounds: RECTL;       // Inclusive-inclusive bounds in device units
    xDest: LONG;
    yDest: LONG;
    cxDest: LONG;
    cyDest: LONG;
    dwRop: DWORD;
    xSrc: LONG;
    ySrc: LONG;
    xformSrc: XFORM;        // Source DC transform
    crBkColorSrc: COLORREF; // Source DC BkColor in RGB
    iUsageSrc: DWORD;       // Source bitmap info color table usage
                            // (DIB_RGB_COLORS)
    offBmiSrc: DWORD;       // Offset to the source BITMAPINFO structure
    cbBmiSrc: DWORD;        // Size of the source BITMAPINFO structure
    offBitsSrc: DWORD;      // Offset to the source bitmap bits
    cbBitsSrc: DWORD;       // Size of the source bitmap bits
    cxSrc: LONG;
    cySrc: LONG;
  end;
  EMRTRANSPARENTBLT = tagEMRTRANSPARENTBLT;
  TEmrTransparentBlt = EMRTRANSPARENTBLT;

const
  GDICOMMENT_IDENTIFIER       = $43494447;
  GDICOMMENT_WINDOWS_METAFILE = DWORD($80000001);
  GDICOMMENT_BEGINGROUP       = $00000002;
  GDICOMMENT_ENDGROUP         = $00000003;
  GDICOMMENT_MULTIFORMATS     = $40000004;
  EPS_SIGNATURE               = $46535045;
  GDICOMMENT_UNICODE_STRING   = $00000040;
  GDICOMMENT_UNICODE_END      = $00000080;

// OpenGL wgl prototypes

function wglCopyContext(hglrcSrc, hglrcDest: HGLRC; mask: UINT): BOOL; stdcall;
function wglCreateContext(hdc: HDC): HGLRC; stdcall;
function wglCreateLayerContext(hdc: HDC; iLayerPlane: Integer): HGLRC; stdcall;
function wglDeleteContext(hglrc: HGLRC): BOOL; stdcall;
function wglGetCurrentContext: HGLRC; stdcall;
function wglGetCurrentDC: HDC; stdcall;
function wglGetProcAddress(lpszProc: LPCSTR): PROC; stdcall;
function wglMakeCurrent(hdc: HDC; hglrc: HGLRC): BOOL; stdcall;
function wglShareLists(hglrc1, hglrc2: HGLRC): BOOL; stdcall;

function wglUseFontBitmapsA(hdc: HDC; first, count, listBase: DWORD): BOOL; stdcall;
function wglUseFontBitmapsW(hdc: HDC; first, count, listBase: DWORD): BOOL; stdcall;

{$IFDEF UNICODE}
function wglUseFontBitmaps(hdc: HDC; first, count, listBase: DWORD): BOOL; stdcall;
{$ELSE}
function wglUseFontBitmaps(hdc: HDC; first, count, listBase: DWORD): BOOL; stdcall;
{$ENDIF}

function SwapBuffers(hdc: HDC): BOOL; stdcall;

type
  PPointFloat = ^TPointFloat;
  _POINTFLOAT = record
    x: FLOAT;
    y: FLOAT;
  end;
  POINTFLOAT = _POINTFLOAT;
  TPointFloat = _POINTFLOAT;

  PGlyphMetricsFloat = ^TGlyphMetricsFloat;
  _GLYPHMETRICSFLOAT = record
    gmfBlackBoxX: FLOAT;
    gmfBlackBoxY: FLOAT;
    gmfptGlyphOrigin: POINTFLOAT;
    gmfCellIncX: FLOAT;
    gmfCellIncY: FLOAT;
  end;
  GLYPHMETRICSFLOAT = _GLYPHMETRICSFLOAT;
  LPGLYPHMETRICSFLOAT = ^GLYPHMETRICSFLOAT;
  TGlyphMetricsFloat = _GLYPHMETRICSFLOAT;

const
  WGL_FONT_LINES    = 0;
  WGL_FONT_POLYGONS = 1;

function wglUseFontOutlinesA(hdc: HDC; first, count, listBase: DWORD; deviation,
  extrusion: FLOAT; format: Integer; lpgmf: LPGLYPHMETRICSFLOAT): BOOL; stdcall;
function wglUseFontOutlinesW(hdc: HDC; first, count, listBase: DWORD; deviation,
  extrusion: FLOAT; format: Integer; lpgmf: LPGLYPHMETRICSFLOAT): BOOL; stdcall;

{$IFDEF UNICODE}
function wglUseFontOutlines(hdc: HDC; first, count, listBase: DWORD; deviation,
  extrusion: FLOAT; format: Integer; lpgmf: LPGLYPHMETRICSFLOAT): BOOL; stdcall;
{$ELSE}
function wglUseFontOutlines(hdc: HDC; first, count, listBase: DWORD; deviation,
  extrusion: FLOAT; format: Integer; lpgmf: LPGLYPHMETRICSFLOAT): BOOL; stdcall;
{$ENDIF}

// Layer plane descriptor

type
  PLayerPlaneDescriptor = ^TLayerPlaneDescriptor;
  tagLAYERPLANEDESCRIPTOR = record
    nSize: WORD;
    nVersion: WORD;
    dwFlags: DWORD;
    iPixelType: BYTE;
    cColorBits: BYTE;
    cRedBits: BYTE;
    cRedShift: BYTE;
    cGreenBits: BYTE;
    cGreenShift: BYTE;
    cBlueBits: BYTE;
    cBlueShift: BYTE;
    cAlphaBits: BYTE;
    cAlphaShift: BYTE;
    cAccumBits: BYTE;
    cAccumRedBits: BYTE;
    cAccumGreenBits: BYTE;
    cAccumBlueBits: BYTE;
    cAccumAlphaBits: BYTE;
    cDepthBits: BYTE;
    cStencilBits: BYTE;
    cAuxBuffers: BYTE;
    iLayerPlane: BYTE;
    bReserved: BYTE;
    crTransparent: COLORREF;
  end;
  LAYERPLANEDESCRIPTOR = tagLAYERPLANEDESCRIPTOR;
  LPLAYERPLANEDESCRIPTOR = ^LAYERPLANEDESCRIPTOR;
  TLayerPlaneDescriptor = LAYERPLANEDESCRIPTOR;

// LAYERPLANEDESCRIPTOR flags

const
  LPD_DOUBLEBUFFER   = $00000001;
  LPD_STEREO         = $00000002;
  LPD_SUPPORT_GDI    = $00000010;
  LPD_SUPPORT_OPENGL = $00000020;
  LPD_SHARE_DEPTH    = $00000040;
  LPD_SHARE_STENCIL  = $00000080;
  LPD_SHARE_ACCUM    = $00000100;
  LPD_SWAP_EXCHANGE  = $00000200;
  LPD_SWAP_COPY      = $00000400;
  LPD_TRANSPARENT    = $00001000;

  LPD_TYPE_RGBA       = 0;
  LPD_TYPE_COLORINDEX = 1;

// wglSwapLayerBuffers flags

  WGL_SWAP_MAIN_PLANE = $00000001;
  WGL_SWAP_OVERLAY1   = $00000002;
  WGL_SWAP_OVERLAY2   = $00000004;
  WGL_SWAP_OVERLAY3   = $00000008;
  WGL_SWAP_OVERLAY4   = $00000010;
  WGL_SWAP_OVERLAY5   = $00000020;
  WGL_SWAP_OVERLAY6   = $00000040;
  WGL_SWAP_OVERLAY7   = $00000080;
  WGL_SWAP_OVERLAY8   = $00000100;
  WGL_SWAP_OVERLAY9   = $00000200;
  WGL_SWAP_OVERLAY10  = $00000400;
  WGL_SWAP_OVERLAY11  = $00000800;
  WGL_SWAP_OVERLAY12  = $00001000;
  WGL_SWAP_OVERLAY13  = $00002000;
  WGL_SWAP_OVERLAY14  = $00004000;
  WGL_SWAP_OVERLAY15  = $00008000;
  WGL_SWAP_UNDERLAY1  = $00010000;
  WGL_SWAP_UNDERLAY2  = $00020000;
  WGL_SWAP_UNDERLAY3  = $00040000;
  WGL_SWAP_UNDERLAY4  = $00080000;
  WGL_SWAP_UNDERLAY5  = $00100000;
  WGL_SWAP_UNDERLAY6  = $00200000;
  WGL_SWAP_UNDERLAY7  = $00400000;
  WGL_SWAP_UNDERLAY8  = $00800000;
  WGL_SWAP_UNDERLAY9  = $01000000;
  WGL_SWAP_UNDERLAY10 = $02000000;
  WGL_SWAP_UNDERLAY11 = $04000000;
  WGL_SWAP_UNDERLAY12 = $08000000;
  WGL_SWAP_UNDERLAY13 = $10000000;
  WGL_SWAP_UNDERLAY14 = $20000000;
  WGL_SWAP_UNDERLAY15 = $40000000;

function wglDescribeLayerPlane(hdc: HDC; iPixelFormat, iLayerPlane: Integer;
  nBytes: UINT; plpd: LPLAYERPLANEDESCRIPTOR): BOOL; stdcall;
function wglSetLayerPaletteEntries(hdc: HDC; iLayerPlane, iStart, cEntries: Integer;
  pcr: LPCOLORREF): Integer; stdcall;
function wglGetLayerPaletteEntries(hdc: HDC; iLayerPlane, iStart, cEntries: Integer;
  pcr: LPCOLORREF): Integer; stdcall;
function wglRealizeLayerPalette(hdc: HDC; iLayerPlane: Integer; bRealize: BOOL): BOOL; stdcall;
function wglSwapLayerBuffers(hdc: HDC; fuPlanes: UINT): BOOL; stdcall;

type
  PWglSwap = ^TWglSwap;
  _WGLSWAP = record
    hdc: HDC;
    uiFlags: UINT;
  end;
  WGLSWAP = _WGLSWAP;
  LPWGLSWAP = ^WGLSWAP;
  TWglSwap = _WGLSWAP;

const
  WGL_SWAPMULTIPLE_MAX = 16;

function wglSwapMultipleBuffers(fuCount: UINT; lpBuffers: LPWGLSWAP): DWORD; stdcall;

implementation

function MAKEROP4(Fore, Back: DWORD): DWORD;
begin
  Result := ((Back shl 8) and DWORD($FF000000)) or Fore;
end;

function GetKValue(cmyk: COLORREF): BYTE;
begin
  Result := BYTE(cmyk);
end;

function GetYValue(cmyk: COLORREF): BYTE;
begin
  Result := BYTE(cmyk shr 8);
end;

function GetMValue(cmyk: COLORREF): BYTE;
begin
  Result := BYTE(cmyk shr 16);
end;

function GetCValue(cmyk: COLORREF): BYTE;
begin
  Result := BYTE(cmyk shr 24);
end;

function CMYK(c, m, y, k: BYTE): COLORREF;
begin
  Result := COLORREF(k or (y shl 8) or (m shl 16) or (c shl 24));
end;

function MAKEPOINTS(l: DWORD): POINTS;
begin
  Result.x := LOWORD(l);
  Result.y := HIWORD(l);  
end;

function RGB(r, g, b: BYTE): COLORREF;
begin
  Result := COLORREF(r or (g shl 8) or (b shl 16));
end;

function PALETTERGB(r, g, b: BYTE): COLORREF;
begin
  Result := $02000000 or RGB(r, g, b);
end;

function PALETTEINDEX(i: WORD): COLORREF;
begin
  Result := COLORREF($01000000 or DWORD(i));
end;

function GetRValue(rgb: COLORREF): BYTE;
begin
  Result := BYTE(RGB);
end;

function GetGValue(rgb: COLORREF): BYTE;
begin
  Result := BYTE(rgb shr 8);
end;

function GetBValue(rgb: COLORREF): BYTE;
begin
  Result := BYTE(rgb shr 16);
end;

const
  gdi32 = 'gdi32.dll';
  msimg32 = 'msimg32.dll';
  winspool32 = 'winspool32.drv';
  opengl32 = 'opengl32.dll';
  

{$IFDEF DYNAMIC_LINK}
var
  _AddFontResourceA: Pointer;

function AddFontResourceA;
begin
  GetProcedureAddress(_AddFontResourceA, gdi32, 'AddFontResourceA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AddFontResourceA]
  end;
end;
{$ELSE}
function AddFontResourceA; external gdi32 name 'AddFontResourceA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _AddFontResourceW: Pointer;

function AddFontResourceW;
begin
  GetProcedureAddress(_AddFontResourceW, gdi32, 'AddFontResourceW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AddFontResourceW]
  end;
end;
{$ELSE}
function AddFontResourceW; external gdi32 name 'AddFontResourceW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _AddFontResource: Pointer;

function AddFontResource;
begin
  GetProcedureAddress(_AddFontResource, gdi32, 'AddFontResourceW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AddFontResource]
  end;
end;
{$ELSE}
function AddFontResource; external gdi32 name 'AddFontResourceW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _AddFontResource: Pointer;

function AddFontResource;
begin
  GetProcedureAddress(_AddFontResource, gdi32, 'AddFontResourceA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AddFontResource]
  end;
end;
{$ELSE}
function AddFontResource; external gdi32 name 'AddFontResourceA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _AnimatePalette: Pointer;

function AnimatePalette;
begin
  GetProcedureAddress(_AnimatePalette, gdi32, 'AnimatePalette');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AnimatePalette]
  end;
end;
{$ELSE}
function AnimatePalette; external gdi32 name 'AnimatePalette';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _Arc: Pointer;

function Arc;
begin
  GetProcedureAddress(_Arc, gdi32, 'Arc');
  asm
    mov esp, ebp
    pop ebp
    jmp [_Arc]
  end;
end;
{$ELSE}
function Arc; external gdi32 name 'Arc';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _BitBlt: Pointer;

function BitBlt;
begin
  GetProcedureAddress(_BitBlt, gdi32, 'BitBlt');
  asm
    mov esp, ebp
    pop ebp
    jmp [_BitBlt]
  end;
end;
{$ELSE}
function BitBlt; external gdi32 name 'BitBlt';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CancelDC: Pointer;

function CancelDC;
begin
  GetProcedureAddress(_CancelDC, gdi32, 'CancelDC');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CancelDC]
  end;
end;
{$ELSE}
function CancelDC; external gdi32 name 'CancelDC';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _Chord: Pointer;

function Chord;
begin
  GetProcedureAddress(_Chord, gdi32, 'Chord');
  asm
    mov esp, ebp
    pop ebp
    jmp [_Chord]
  end;
end;
{$ELSE}
function Chord; external gdi32 name 'Chord';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ChoosePixelFormat: Pointer;

function ChoosePixelFormat;
begin
  GetProcedureAddress(_ChoosePixelFormat, gdi32, 'ChoosePixelFormat');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ChoosePixelFormat]
  end;
end;
{$ELSE}
function ChoosePixelFormat; external gdi32 name 'ChoosePixelFormat';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CloseMetaFile: Pointer;

function CloseMetaFile;
begin
  GetProcedureAddress(_CloseMetaFile, gdi32, 'CloseMetaFile');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CloseMetaFile]
  end;
end;
{$ELSE}
function CloseMetaFile; external gdi32 name 'CloseMetaFile';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CombineRgn: Pointer;

function CombineRgn;
begin
  GetProcedureAddress(_CombineRgn, gdi32, 'CombineRgn');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CombineRgn]
  end;
end;
{$ELSE}
function CombineRgn; external gdi32 name 'CombineRgn';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CopyMetaFileA: Pointer;

function CopyMetaFileA;
begin
  GetProcedureAddress(_CopyMetaFileA, gdi32, 'CopyMetaFileA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CopyMetaFileA]
  end;
end;
{$ELSE}
function CopyMetaFileA; external gdi32 name 'CopyMetaFileA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CopyMetaFileW: Pointer;

function CopyMetaFileW;
begin
  GetProcedureAddress(_CopyMetaFileW, gdi32, 'CopyMetaFileW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CopyMetaFileW]
  end;
end;
{$ELSE}
function CopyMetaFileW; external gdi32 name 'CopyMetaFileW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _CopyMetaFile: Pointer;

function CopyMetaFile;
begin
  GetProcedureAddress(_CopyMetaFile, gdi32, 'CopyMetaFileW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CopyMetaFile]
  end;
end;
{$ELSE}
function CopyMetaFile; external gdi32 name 'CopyMetaFileW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _CopyMetaFile: Pointer;

function CopyMetaFile;
begin
  GetProcedureAddress(_CopyMetaFile, gdi32, 'CopyMetaFileA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CopyMetaFile]
  end;
end;
{$ELSE}
function CopyMetaFile; external gdi32 name 'CopyMetaFileA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _CreateBitmap: Pointer;

function CreateBitmap;
begin
  GetProcedureAddress(_CreateBitmap, gdi32, 'CreateBitmap');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateBitmap]
  end;
end;
{$ELSE}
function CreateBitmap; external gdi32 name 'CreateBitmap';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateBitmapIndirect: Pointer;

function CreateBitmapIndirect;
begin
  GetProcedureAddress(_CreateBitmapIndirect, gdi32, 'CreateBitmapIndirect');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateBitmapIndirect]
  end;
end;
{$ELSE}
function CreateBitmapIndirect; external gdi32 name 'CreateBitmapIndirect';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateBrushIndirect: Pointer;

function CreateBrushIndirect;
begin
  GetProcedureAddress(_CreateBrushIndirect, gdi32, 'CreateBrushIndirect');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateBrushIndirect]
  end;
end;
{$ELSE}
function CreateBrushIndirect; external gdi32 name 'CreateBrushIndirect';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateCompatibleBitmap: Pointer;

function CreateCompatibleBitmap;
begin
  GetProcedureAddress(_CreateCompatibleBitmap, gdi32, 'CreateCompatibleBitmap');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateCompatibleBitmap]
  end;
end;
{$ELSE}
function CreateCompatibleBitmap; external gdi32 name 'CreateCompatibleBitmap';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateDiscardableBitmap: Pointer;

function CreateDiscardableBitmap;
begin
  GetProcedureAddress(_CreateDiscardableBitmap, gdi32, 'CreateDiscardableBitmap');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateDiscardableBitmap]
  end;
end;
{$ELSE}
function CreateDiscardableBitmap; external gdi32 name 'CreateDiscardableBitmap';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateCompatibleDC: Pointer;

function CreateCompatibleDC;
begin
  GetProcedureAddress(_CreateCompatibleDC, gdi32, 'CreateCompatibleDC');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateCompatibleDC]
  end;
end;
{$ELSE}
function CreateCompatibleDC; external gdi32 name 'CreateCompatibleDC';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateDCA: Pointer;

function CreateDCA;
begin
  GetProcedureAddress(_CreateDCA, gdi32, 'CreateDCA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateDCA]
  end;
end;
{$ELSE}
function CreateDCA; external gdi32 name 'CreateDCA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateDCW: Pointer;

function CreateDCW;
begin
  GetProcedureAddress(_CreateDCW, gdi32, 'CreateDCW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateDCW]
  end;
end;
{$ELSE}
function CreateDCW; external gdi32 name 'CreateDCW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _CreateDC: Pointer;

function CreateDC;
begin
  GetProcedureAddress(_CreateDC, gdi32, 'CreateDCW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateDC]
  end;
end;
{$ELSE}
function CreateDC; external gdi32 name 'CreateDCW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _CreateDC: Pointer;

function CreateDC;
begin
  GetProcedureAddress(_CreateDC, gdi32, 'CreateDCA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateDC]
  end;
end;
{$ELSE}
function CreateDC; external gdi32 name 'CreateDCA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _CreateDIBitmap: Pointer;

function CreateDIBitmap;
begin
  GetProcedureAddress(_CreateDIBitmap, gdi32, 'CreateDIBitmap');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateDIBitmap]
  end;
end;
{$ELSE}
function CreateDIBitmap; external gdi32 name 'CreateDIBitmap';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateDIBPatternBrush: Pointer;

function CreateDIBPatternBrush;
begin
  GetProcedureAddress(_CreateDIBPatternBrush, gdi32, 'CreateDIBPatternBrush');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateDIBPatternBrush]
  end;
end;
{$ELSE}
function CreateDIBPatternBrush; external gdi32 name 'CreateDIBPatternBrush';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateDIBPatternBrushPt: Pointer;

function CreateDIBPatternBrushPt;
begin
  GetProcedureAddress(_CreateDIBPatternBrushPt, gdi32, 'CreateDIBPatternBrushPt');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateDIBPatternBrushPt]
  end;
end;
{$ELSE}
function CreateDIBPatternBrushPt; external gdi32 name 'CreateDIBPatternBrushPt';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateEllipticRgn: Pointer;

function CreateEllipticRgn;
begin
  GetProcedureAddress(_CreateEllipticRgn, gdi32, 'CreateEllipticRgn');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateEllipticRgn]
  end;
end;
{$ELSE}
function CreateEllipticRgn; external gdi32 name 'CreateEllipticRgn';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateEllipticRgnIndirect: Pointer;

function CreateEllipticRgnIndirect;
begin
  GetProcedureAddress(_CreateEllipticRgnIndirect, gdi32, 'CreateEllipticRgnIndirect');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateEllipticRgnIndirect]
  end;
end;
{$ELSE}
function CreateEllipticRgnIndirect; external gdi32 name 'CreateEllipticRgnIndirect';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateFontIndirectA: Pointer;

function CreateFontIndirectA;
begin
  GetProcedureAddress(_CreateFontIndirectA, gdi32, 'CreateFontIndirectA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateFontIndirectA]
  end;
end;
{$ELSE}
function CreateFontIndirectA; external gdi32 name 'CreateFontIndirectA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateFontIndirectW: Pointer;

function CreateFontIndirectW;
begin
  GetProcedureAddress(_CreateFontIndirectW, gdi32, 'CreateFontIndirectW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateFontIndirectW]
  end;
end;
{$ELSE}
function CreateFontIndirectW; external gdi32 name 'CreateFontIndirectW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _CreateFontIndirect: Pointer;

function CreateFontIndirect;
begin
  GetProcedureAddress(_CreateFontIndirect, gdi32, 'CreateFontIndirectW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateFontIndirect]
  end;
end;
{$ELSE}
function CreateFontIndirect; external gdi32 name 'CreateFontIndirectW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _CreateFontIndirect: Pointer;

function CreateFontIndirect;
begin
  GetProcedureAddress(_CreateFontIndirect, gdi32, 'CreateFontIndirectA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateFontIndirect]
  end;
end;
{$ELSE}
function CreateFontIndirect; external gdi32 name 'CreateFontIndirectA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _CreateFontA: Pointer;

function CreateFontA;
begin
  GetProcedureAddress(_CreateFontA, gdi32, 'CreateFontA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateFontA]
  end;
end;
{$ELSE}
function CreateFontA; external gdi32 name 'CreateFontA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateFontW: Pointer;

function CreateFontW;
begin
  GetProcedureAddress(_CreateFontW, gdi32, 'CreateFontW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateFontW]
  end;
end;
{$ELSE}
function CreateFontW; external gdi32 name 'CreateFontW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _CreateFont: Pointer;

function CreateFont;
begin
  GetProcedureAddress(_CreateFont, gdi32, 'CreateFontW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateFont]
  end;
end;
{$ELSE}
function CreateFont; external gdi32 name 'CreateFontW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _CreateFont: Pointer;

function CreateFont;
begin
  GetProcedureAddress(_CreateFont, gdi32, 'CreateFontA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateFont]
  end;
end;
{$ELSE}
function CreateFont; external gdi32 name 'CreateFontA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _CreateHatchBrush: Pointer;

function CreateHatchBrush;
begin
  GetProcedureAddress(_CreateHatchBrush, gdi32, 'CreateHatchBrush');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateHatchBrush]
  end;
end;
{$ELSE}
function CreateHatchBrush; external gdi32 name 'CreateHatchBrush';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateICA: Pointer;

function CreateICA;
begin
  GetProcedureAddress(_CreateICA, gdi32, 'CreateICA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateICA]
  end;
end;
{$ELSE}
function CreateICA; external gdi32 name 'CreateICA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateICW: Pointer;

function CreateICW;
begin
  GetProcedureAddress(_CreateICW, gdi32, 'CreateICW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateICW]
  end;
end;
{$ELSE}
function CreateICW; external gdi32 name 'CreateICW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _CreateIC: Pointer;

function CreateIC;
begin
  GetProcedureAddress(_CreateIC, gdi32, 'CreateICW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateIC]
  end;
end;
{$ELSE}
function CreateIC; external gdi32 name 'CreateICW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _CreateIC: Pointer;

function CreateIC;
begin
  GetProcedureAddress(_CreateIC, gdi32, 'CreateICA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateIC]
  end;
end;
{$ELSE}
function CreateIC; external gdi32 name 'CreateICA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _CreateMetaFileA: Pointer;

function CreateMetaFileA;
begin
  GetProcedureAddress(_CreateMetaFileA, gdi32, 'CreateMetaFileA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateMetaFileA]
  end;
end;
{$ELSE}
function CreateMetaFileA; external gdi32 name 'CreateMetaFileA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateMetaFileW: Pointer;

function CreateMetaFileW;
begin
  GetProcedureAddress(_CreateMetaFileW, gdi32, 'CreateMetaFileW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateMetaFileW]
  end;
end;
{$ELSE}
function CreateMetaFileW; external gdi32 name 'CreateMetaFileW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _CreateMetaFile: Pointer;

function CreateMetaFile;
begin
  GetProcedureAddress(_CreateMetaFile, gdi32, 'CreateMetaFileW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateMetaFile]
  end;
end;
{$ELSE}
function CreateMetaFile; external gdi32 name 'CreateMetaFileW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _CreateMetaFile: Pointer;

function CreateMetaFile;
begin
  GetProcedureAddress(_CreateMetaFile, gdi32, 'CreateMetaFileA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateMetaFile]
  end;
end;
{$ELSE}
function CreateMetaFile; external gdi32 name 'CreateMetaFileA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _CreatePalette: Pointer;

function CreatePalette;
begin
  GetProcedureAddress(_CreatePalette, gdi32, 'CreatePalette');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreatePalette]
  end;
end;
{$ELSE}
function CreatePalette; external gdi32 name 'CreatePalette';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreatePen: Pointer;

function CreatePen;
begin
  GetProcedureAddress(_CreatePen, gdi32, 'CreatePen');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreatePen]
  end;
end;
{$ELSE}
function CreatePen; external gdi32 name 'CreatePen';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreatePenIndirect: Pointer;

function CreatePenIndirect;
begin
  GetProcedureAddress(_CreatePenIndirect, gdi32, 'CreatePenIndirect');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreatePenIndirect]
  end;
end;
{$ELSE}
function CreatePenIndirect; external gdi32 name 'CreatePenIndirect';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreatePolyPolygonRgn: Pointer;

function CreatePolyPolygonRgn;
begin
  GetProcedureAddress(_CreatePolyPolygonRgn, gdi32, 'CreatePolyPolygonRgn');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreatePolyPolygonRgn]
  end;
end;
{$ELSE}
function CreatePolyPolygonRgn; external gdi32 name 'CreatePolyPolygonRgn';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreatePatternBrush: Pointer;

function CreatePatternBrush;
begin
  GetProcedureAddress(_CreatePatternBrush, gdi32, 'CreatePatternBrush');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreatePatternBrush]
  end;
end;
{$ELSE}
function CreatePatternBrush; external gdi32 name 'CreatePatternBrush';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateRectRgn: Pointer;

function CreateRectRgn;
begin
  GetProcedureAddress(_CreateRectRgn, gdi32, 'CreateRectRgn');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateRectRgn]
  end;
end;
{$ELSE}
function CreateRectRgn; external gdi32 name 'CreateRectRgn';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateRectRgnIndirect: Pointer;

function CreateRectRgnIndirect;
begin
  GetProcedureAddress(_CreateRectRgnIndirect, gdi32, 'CreateRectRgnIndirect');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateRectRgnIndirect]
  end;
end;
{$ELSE}
function CreateRectRgnIndirect; external gdi32 name 'CreateRectRgnIndirect';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateRoundRectRgn: Pointer;

function CreateRoundRectRgn;
begin
  GetProcedureAddress(_CreateRoundRectRgn, gdi32, 'CreateRoundRectRgn');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateRoundRectRgn]
  end;
end;
{$ELSE}
function CreateRoundRectRgn; external gdi32 name 'CreateRoundRectRgn';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateScalableFontResourceA: Pointer;

function CreateScalableFontResourceA;
begin
  GetProcedureAddress(_CreateScalableFontResourceA, gdi32, 'CreateScalableFontResourceA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateScalableFontResourceA]
  end;
end;
{$ELSE}
function CreateScalableFontResourceA; external gdi32 name 'CreateScalableFontResourceA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateScalableFontResourceW: Pointer;

function CreateScalableFontResourceW;
begin
  GetProcedureAddress(_CreateScalableFontResourceW, gdi32, 'CreateScalableFontResourceW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateScalableFontResourceW]
  end;
end;
{$ELSE}
function CreateScalableFontResourceW; external gdi32 name 'CreateScalableFontResourceW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _CreateScalableFontResource: Pointer;

function CreateScalableFontResource;
begin
  GetProcedureAddress(_CreateScalableFontResource, gdi32, 'CreateScalableFontResourceW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateScalableFontResource]
  end;
end;
{$ELSE}
function CreateScalableFontResource; external gdi32 name 'CreateScalableFontResourceW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _CreateScalableFontResource: Pointer;

function CreateScalableFontResource;
begin
  GetProcedureAddress(_CreateScalableFontResource, gdi32, 'CreateScalableFontResourceA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateScalableFontResource]
  end;
end;
{$ELSE}
function CreateScalableFontResource; external gdi32 name 'CreateScalableFontResourceA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _CreateSolidBrush: Pointer;

function CreateSolidBrush;
begin
  GetProcedureAddress(_CreateSolidBrush, gdi32, 'CreateSolidBrush');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateSolidBrush]
  end;
end;
{$ELSE}
function CreateSolidBrush; external gdi32 name 'CreateSolidBrush';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DeleteDC: Pointer;

function DeleteDC;
begin
  GetProcedureAddress(_DeleteDC, gdi32, 'DeleteDC');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DeleteDC]
  end;
end;
{$ELSE}
function DeleteDC; external gdi32 name 'DeleteDC';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DeleteMetaFile: Pointer;

function DeleteMetaFile;
begin
  GetProcedureAddress(_DeleteMetaFile, gdi32, 'DeleteMetaFile');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DeleteMetaFile]
  end;
end;
{$ELSE}
function DeleteMetaFile; external gdi32 name 'DeleteMetaFile';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DeleteObject: Pointer;

function DeleteObject;
begin
  GetProcedureAddress(_DeleteObject, gdi32, 'DeleteObject');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DeleteObject]
  end;
end;
{$ELSE}
function DeleteObject; external gdi32 name 'DeleteObject';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DescribePixelFormat: Pointer;

function DescribePixelFormat;
begin
  GetProcedureAddress(_DescribePixelFormat, gdi32, 'DescribePixelFormat');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DescribePixelFormat]
  end;
end;
{$ELSE}
function DescribePixelFormat; external gdi32 name 'DescribePixelFormat';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DeviceCapabilitiesA: Pointer;

function DeviceCapabilitiesA;
begin
  GetProcedureAddress(_DeviceCapabilitiesA, winspool32, 'DeviceCapabilitiesA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DeviceCapabilitiesA]
  end;
end;
{$ELSE}
function DeviceCapabilitiesA; external winspool32 name 'DeviceCapabilitiesA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DeviceCapabilitiesW: Pointer;

function DeviceCapabilitiesW;
begin
  GetProcedureAddress(_DeviceCapabilitiesW, winspool32, 'DeviceCapabilitiesW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DeviceCapabilitiesW]
  end;
end;
{$ELSE}
function DeviceCapabilitiesW; external winspool32 name 'DeviceCapabilitiesW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _DeviceCapabilities: Pointer;

function DeviceCapabilities;
begin
  GetProcedureAddress(_DeviceCapabilities, winspool32, 'DeviceCapabilitiesW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DeviceCapabilities]
  end;
end;
{$ELSE}
function DeviceCapabilities; external winspool32 name 'DeviceCapabilitiesW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _DeviceCapabilities: Pointer;

function DeviceCapabilities;
begin
  GetProcedureAddress(_DeviceCapabilities, winspool32, 'DeviceCapabilitiesA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DeviceCapabilities]
  end;
end;
{$ELSE}
function DeviceCapabilities; external winspool32 name 'DeviceCapabilitiesA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _DrawEscape: Pointer;

function DrawEscape;
begin
  GetProcedureAddress(_DrawEscape, gdi32, 'DrawEscape');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DrawEscape]
  end;
end;
{$ELSE}
function DrawEscape; external gdi32 name 'DrawEscape';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _Ellipse: Pointer;

function Ellipse;
begin
  GetProcedureAddress(_Ellipse, gdi32, 'Ellipse');
  asm
    mov esp, ebp
    pop ebp
    jmp [_Ellipse]
  end;
end;
{$ELSE}
function Ellipse; external gdi32 name 'Ellipse';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _EnumFontFamiliesExA: Pointer;

function EnumFontFamiliesExA;
begin
  GetProcedureAddress(_EnumFontFamiliesExA, gdi32, 'EnumFontFamiliesExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumFontFamiliesExA]
  end;
end;
{$ELSE}
function EnumFontFamiliesExA; external gdi32 name 'EnumFontFamiliesExA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _EnumFontFamiliesExW: Pointer;

function EnumFontFamiliesExW;
begin
  GetProcedureAddress(_EnumFontFamiliesExW, gdi32, 'EnumFontFamiliesExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumFontFamiliesExW]
  end;
end;
{$ELSE}
function EnumFontFamiliesExW; external gdi32 name 'EnumFontFamiliesExW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _EnumFontFamiliesEx: Pointer;

function EnumFontFamiliesEx;
begin
  GetProcedureAddress(_EnumFontFamiliesEx, gdi32, 'EnumFontFamiliesExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumFontFamiliesEx]
  end;
end;
{$ELSE}
function EnumFontFamiliesEx; external gdi32 name 'EnumFontFamiliesExW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _EnumFontFamiliesEx: Pointer;

function EnumFontFamiliesEx;
begin
  GetProcedureAddress(_EnumFontFamiliesEx, gdi32, 'EnumFontFamiliesExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumFontFamiliesEx]
  end;
end;
{$ELSE}
function EnumFontFamiliesEx; external gdi32 name 'EnumFontFamiliesExA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _EnumFontFamiliesA: Pointer;

function EnumFontFamiliesA;
begin
  GetProcedureAddress(_EnumFontFamiliesA, gdi32, 'EnumFontFamiliesA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumFontFamiliesA]
  end;
end;
{$ELSE}
function EnumFontFamiliesA; external gdi32 name 'EnumFontFamiliesA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _EnumFontFamiliesW: Pointer;

function EnumFontFamiliesW;
begin
  GetProcedureAddress(_EnumFontFamiliesW, gdi32, 'EnumFontFamiliesW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumFontFamiliesW]
  end;
end;
{$ELSE}
function EnumFontFamiliesW; external gdi32 name 'EnumFontFamiliesW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _EnumFontFamilies: Pointer;

function EnumFontFamilies;
begin
  GetProcedureAddress(_EnumFontFamilies, gdi32, 'EnumFontFamiliesW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumFontFamilies]
  end;
end;
{$ELSE}
function EnumFontFamilies; external gdi32 name 'EnumFontFamiliesW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _EnumFontFamilies: Pointer;

function EnumFontFamilies;
begin
  GetProcedureAddress(_EnumFontFamilies, gdi32, 'EnumFontFamiliesA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumFontFamilies]
  end;
end;
{$ELSE}
function EnumFontFamilies; external gdi32 name 'EnumFontFamiliesA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _EnumFontsA: Pointer;

function EnumFontsA;
begin
  GetProcedureAddress(_EnumFontsA, gdi32, 'EnumFontsA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumFontsA]
  end;
end;
{$ELSE}
function EnumFontsA; external gdi32 name 'EnumFontsA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _EnumFontsW: Pointer;

function EnumFontsW;
begin
  GetProcedureAddress(_EnumFontsW, gdi32, 'EnumFontsW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumFontsW]
  end;
end;
{$ELSE}
function EnumFontsW; external gdi32 name 'EnumFontsW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _EnumFonts: Pointer;

function EnumFonts;
begin
  GetProcedureAddress(_EnumFonts, gdi32, 'EnumFontsW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumFonts]
  end;
end;
{$ELSE}
function EnumFonts; external gdi32 name 'EnumFontsW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _EnumFonts: Pointer;

function EnumFonts;
begin
  GetProcedureAddress(_EnumFonts, gdi32, 'EnumFontsA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumFonts]
  end;
end;
{$ELSE}
function EnumFonts; external gdi32 name 'EnumFontsA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _EnumObjects: Pointer;

function EnumObjects;
begin
  GetProcedureAddress(_EnumObjects, gdi32, 'EnumObjects');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumObjects]
  end;
end;
{$ELSE}
function EnumObjects; external gdi32 name 'EnumObjects';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _EqualRgn: Pointer;

function EqualRgn;
begin
  GetProcedureAddress(_EqualRgn, gdi32, 'EqualRgn');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EqualRgn]
  end;
end;
{$ELSE}
function EqualRgn; external gdi32 name 'EqualRgn';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _Escape: Pointer;

function Escape;
begin
  GetProcedureAddress(_Escape, gdi32, 'Escape');
  asm
    mov esp, ebp
    pop ebp
    jmp [_Escape]
  end;
end;
{$ELSE}
function Escape; external gdi32 name 'Escape';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ExtEscape: Pointer;

function ExtEscape;
begin
  GetProcedureAddress(_ExtEscape, gdi32, 'ExtEscape');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ExtEscape]
  end;
end;
{$ELSE}
function ExtEscape; external gdi32 name 'ExtEscape';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ExcludeClipRect: Pointer;

function ExcludeClipRect;
begin
  GetProcedureAddress(_ExcludeClipRect, gdi32, 'ExcludeClipRect');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ExcludeClipRect]
  end;
end;
{$ELSE}
function ExcludeClipRect; external gdi32 name 'ExcludeClipRect';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ExtCreateRegion: Pointer;

function ExtCreateRegion;
begin
  GetProcedureAddress(_ExtCreateRegion, gdi32, 'ExtCreateRegion');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ExtCreateRegion]
  end;
end;
{$ELSE}
function ExtCreateRegion; external gdi32 name 'ExtCreateRegion';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ExtFloodFill: Pointer;

function ExtFloodFill;
begin
  GetProcedureAddress(_ExtFloodFill, gdi32, 'ExtFloodFill');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ExtFloodFill]
  end;
end;
{$ELSE}
function ExtFloodFill; external gdi32 name 'ExtFloodFill';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _FillRgn: Pointer;

function FillRgn;
begin
  GetProcedureAddress(_FillRgn, gdi32, 'FillRgn');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FillRgn]
  end;
end;
{$ELSE}
function FillRgn; external gdi32 name 'FillRgn';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _FloodFill: Pointer;

function FloodFill;
begin
  GetProcedureAddress(_FloodFill, gdi32, 'FloodFill');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FloodFill]
  end;
end;
{$ELSE}
function FloodFill; external gdi32 name 'FloodFill';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _FrameRgn: Pointer;

function FrameRgn;
begin
  GetProcedureAddress(_FrameRgn, gdi32, 'FrameRgn');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FrameRgn]
  end;
end;
{$ELSE}
function FrameRgn; external gdi32 name 'FrameRgn';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetROP2: Pointer;

function GetROP2;
begin
  GetProcedureAddress(_GetROP2, gdi32, 'GetROP2');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetROP2]
  end;
end;
{$ELSE}
function GetROP2; external gdi32 name 'GetROP2';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetAspectRatioFilterEx: Pointer;

function GetAspectRatioFilterEx;
begin
  GetProcedureAddress(_GetAspectRatioFilterEx, gdi32, 'GetAspectRatioFilterEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetAspectRatioFilterEx]
  end;
end;
{$ELSE}
function GetAspectRatioFilterEx; external gdi32 name 'GetAspectRatioFilterEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetBkColor: Pointer;

function GetBkColor;
begin
  GetProcedureAddress(_GetBkColor, gdi32, 'GetBkColor');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetBkColor]
  end;
end;
{$ELSE}
function GetBkColor; external gdi32 name 'GetBkColor';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetDCBrushColor: Pointer;

function GetDCBrushColor;
begin
  GetProcedureAddress(_GetDCBrushColor, gdi32, 'GetDCBrushColor');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetDCBrushColor]
  end;
end;
{$ELSE}
function GetDCBrushColor; external gdi32 name 'GetDCBrushColor';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetDCPenColor: Pointer;

function GetDCPenColor;
begin
  GetProcedureAddress(_GetDCPenColor, gdi32, 'GetDCPenColor');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetDCPenColor]
  end;
end;
{$ELSE}
function GetDCPenColor; external gdi32 name 'GetDCPenColor';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetBkMode: Pointer;

function GetBkMode;
begin
  GetProcedureAddress(_GetBkMode, gdi32, 'GetBkMode');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetBkMode]
  end;
end;
{$ELSE}
function GetBkMode; external gdi32 name 'GetBkMode';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetBitmapBits: Pointer;

function GetBitmapBits;
begin
  GetProcedureAddress(_GetBitmapBits, gdi32, 'GetBitmapBits');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetBitmapBits]
  end;
end;
{$ELSE}
function GetBitmapBits; external gdi32 name 'GetBitmapBits';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetBitmapDimensionEx: Pointer;

function GetBitmapDimensionEx;
begin
  GetProcedureAddress(_GetBitmapDimensionEx, gdi32, 'GetBitmapDimensionEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetBitmapDimensionEx]
  end;
end;
{$ELSE}
function GetBitmapDimensionEx; external gdi32 name 'GetBitmapDimensionEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetBoundsRect: Pointer;

function GetBoundsRect;
begin
  GetProcedureAddress(_GetBoundsRect, gdi32, 'GetBoundsRect');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetBoundsRect]
  end;
end;
{$ELSE}
function GetBoundsRect; external gdi32 name 'GetBoundsRect';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetBrushOrgEx: Pointer;

function GetBrushOrgEx;
begin
  GetProcedureAddress(_GetBrushOrgEx, gdi32, 'GetBrushOrgEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetBrushOrgEx]
  end;
end;
{$ELSE}
function GetBrushOrgEx; external gdi32 name 'GetBrushOrgEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetCharWidthA: Pointer;

function GetCharWidthA;
begin
  GetProcedureAddress(_GetCharWidthA, gdi32, 'GetCharWidthA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCharWidthA]
  end;
end;
{$ELSE}
function GetCharWidthA; external gdi32 name 'GetCharWidthA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetCharWidthW: Pointer;

function GetCharWidthW;
begin
  GetProcedureAddress(_GetCharWidthW, gdi32, 'GetCharWidthW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCharWidthW]
  end;
end;
{$ELSE}
function GetCharWidthW; external gdi32 name 'GetCharWidthW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetCharWidth: Pointer;

function GetCharWidth;
begin
  GetProcedureAddress(_GetCharWidth, gdi32, 'GetCharWidthW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCharWidth]
  end;
end;
{$ELSE}
function GetCharWidth; external gdi32 name 'GetCharWidthW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetCharWidth: Pointer;

function GetCharWidth;
begin
  GetProcedureAddress(_GetCharWidth, gdi32, 'GetCharWidthA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCharWidth]
  end;
end;
{$ELSE}
function GetCharWidth; external gdi32 name 'GetCharWidthA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetCharWidth32A: Pointer;

function GetCharWidth32A;
begin
  GetProcedureAddress(_GetCharWidth32A, gdi32, 'GetCharWidth32A');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCharWidth32A]
  end;
end;
{$ELSE}
function GetCharWidth32A; external gdi32 name 'GetCharWidth32A';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetCharWidth32W: Pointer;

function GetCharWidth32W;
begin
  GetProcedureAddress(_GetCharWidth32W, gdi32, 'GetCharWidth32W');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCharWidth32W]
  end;
end;
{$ELSE}
function GetCharWidth32W; external gdi32 name 'GetCharWidth32W';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetCharWidth32: Pointer;

function GetCharWidth32;
begin
  GetProcedureAddress(_GetCharWidth32, gdi32, 'GetCharWidth32W');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCharWidth32]
  end;
end;
{$ELSE}
function GetCharWidth32; external gdi32 name 'GetCharWidth32W';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetCharWidth32: Pointer;

function GetCharWidth32;
begin
  GetProcedureAddress(_GetCharWidth32, gdi32, 'GetCharWidth32A');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCharWidth32]
  end;
end;
{$ELSE}
function GetCharWidth32; external gdi32 name 'GetCharWidth32A';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetCharWidthFloatA: Pointer;

function GetCharWidthFloatA;
begin
  GetProcedureAddress(_GetCharWidthFloatA, gdi32, 'GetCharWidthFloatA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCharWidthFloatA]
  end;
end;
{$ELSE}
function GetCharWidthFloatA; external gdi32 name 'GetCharWidthFloatA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetCharWidthFloatW: Pointer;

function GetCharWidthFloatW;
begin
  GetProcedureAddress(_GetCharWidthFloatW, gdi32, 'GetCharWidthFloatW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCharWidthFloatW]
  end;
end;
{$ELSE}
function GetCharWidthFloatW; external gdi32 name 'GetCharWidthFloatW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetCharWidthFloat: Pointer;

function GetCharWidthFloat;
begin
  GetProcedureAddress(_GetCharWidthFloat, gdi32, 'GetCharWidthFloatW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCharWidthFloat]
  end;
end;
{$ELSE}
function GetCharWidthFloat; external gdi32 name 'GetCharWidthFloatW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetCharWidthFloat: Pointer;

function GetCharWidthFloat;
begin
  GetProcedureAddress(_GetCharWidthFloat, gdi32, 'GetCharWidthFloatA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCharWidthFloat]
  end;
end;
{$ELSE}
function GetCharWidthFloat; external gdi32 name 'GetCharWidthFloatA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetCharABCWidthsA: Pointer;

function GetCharABCWidthsA;
begin
  GetProcedureAddress(_GetCharABCWidthsA, gdi32, 'GetCharABCWidthsA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCharABCWidthsA]
  end;
end;
{$ELSE}
function GetCharABCWidthsA; external gdi32 name 'GetCharABCWidthsA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetCharABCWidthsW: Pointer;

function GetCharABCWidthsW;
begin
  GetProcedureAddress(_GetCharABCWidthsW, gdi32, 'GetCharABCWidthsW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCharABCWidthsW]
  end;
end;
{$ELSE}
function GetCharABCWidthsW; external gdi32 name 'GetCharABCWidthsW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetCharABCWidths: Pointer;

function GetCharABCWidths;
begin
  GetProcedureAddress(_GetCharABCWidths, gdi32, 'GetCharABCWidthsW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCharABCWidths]
  end;
end;
{$ELSE}
function GetCharABCWidths; external gdi32 name 'GetCharABCWidthsW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetCharABCWidths: Pointer;

function GetCharABCWidths;
begin
  GetProcedureAddress(_GetCharABCWidths, gdi32, 'GetCharABCWidthsA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCharABCWidths]
  end;
end;
{$ELSE}
function GetCharABCWidths; external gdi32 name 'GetCharABCWidthsA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetCharABCWidthsFloatA: Pointer;

function GetCharABCWidthsFloatA;
begin
  GetProcedureAddress(_GetCharABCWidthsFloatA, gdi32, 'GetCharABCWidthsFloatA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCharABCWidthsFloatA]
  end;
end;
{$ELSE}
function GetCharABCWidthsFloatA; external gdi32 name 'GetCharABCWidthsFloatA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetCharABCWidthsFloatW: Pointer;

function GetCharABCWidthsFloatW;
begin
  GetProcedureAddress(_GetCharABCWidthsFloatW, gdi32, 'GetCharABCWidthsFloatW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCharABCWidthsFloatW]
  end;
end;
{$ELSE}
function GetCharABCWidthsFloatW; external gdi32 name 'GetCharABCWidthsFloatW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetCharABCWidthsFloat: Pointer;

function GetCharABCWidthsFloat;
begin
  GetProcedureAddress(_GetCharABCWidthsFloat, gdi32, 'GetCharABCWidthsFloatW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCharABCWidthsFloat]
  end;
end;
{$ELSE}
function GetCharABCWidthsFloat; external gdi32 name 'GetCharABCWidthsFloatW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetCharABCWidthsFloat: Pointer;

function GetCharABCWidthsFloat;
begin
  GetProcedureAddress(_GetCharABCWidthsFloat, gdi32, 'GetCharABCWidthsFloatA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCharABCWidthsFloat]
  end;
end;
{$ELSE}
function GetCharABCWidthsFloat; external gdi32 name 'GetCharABCWidthsFloatA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetClipBox: Pointer;

function GetClipBox;
begin
  GetProcedureAddress(_GetClipBox, gdi32, 'GetClipBox');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetClipBox]
  end;
end;
{$ELSE}
function GetClipBox; external gdi32 name 'GetClipBox';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetClipRgn: Pointer;

function GetClipRgn;
begin
  GetProcedureAddress(_GetClipRgn, gdi32, 'GetClipRgn');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetClipRgn]
  end;
end;
{$ELSE}
function GetClipRgn; external gdi32 name 'GetClipRgn';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetMetaRgn: Pointer;

function GetMetaRgn;
begin
  GetProcedureAddress(_GetMetaRgn, gdi32, 'GetMetaRgn');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetMetaRgn]
  end;
end;
{$ELSE}
function GetMetaRgn; external gdi32 name 'GetMetaRgn';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetCurrentObject: Pointer;

function GetCurrentObject;
begin
  GetProcedureAddress(_GetCurrentObject, gdi32, 'GetCurrentObject');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCurrentObject]
  end;
end;
{$ELSE}
function GetCurrentObject; external gdi32 name 'GetCurrentObject';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetCurrentPositionEx: Pointer;

function GetCurrentPositionEx;
begin
  GetProcedureAddress(_GetCurrentPositionEx, gdi32, 'GetCurrentPositionEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCurrentPositionEx]
  end;
end;
{$ELSE}
function GetCurrentPositionEx; external gdi32 name 'GetCurrentPositionEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetDeviceCaps: Pointer;

function GetDeviceCaps;
begin
  GetProcedureAddress(_GetDeviceCaps, gdi32, 'GetDeviceCaps');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetDeviceCaps]
  end;
end;
{$ELSE}
function GetDeviceCaps; external gdi32 name 'GetDeviceCaps';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetDIBits: Pointer;

function GetDIBits;
begin
  GetProcedureAddress(_GetDIBits, gdi32, 'GetDIBits');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetDIBits]
  end;
end;
{$ELSE}
function GetDIBits; external gdi32 name 'GetDIBits';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetFontData: Pointer;

function GetFontData;
begin
  GetProcedureAddress(_GetFontData, gdi32, 'GetFontData');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetFontData]
  end;
end;
{$ELSE}
function GetFontData; external gdi32 name 'GetFontData';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetGlyphOutlineA: Pointer;

function GetGlyphOutlineA;
begin
  GetProcedureAddress(_GetGlyphOutlineA, gdi32, 'GetGlyphOutlineA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetGlyphOutlineA]
  end;
end;
{$ELSE}
function GetGlyphOutlineA; external gdi32 name 'GetGlyphOutlineA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetGlyphOutlineW: Pointer;

function GetGlyphOutlineW;
begin
  GetProcedureAddress(_GetGlyphOutlineW, gdi32, 'GetGlyphOutlineW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetGlyphOutlineW]
  end;
end;
{$ELSE}
function GetGlyphOutlineW; external gdi32 name 'GetGlyphOutlineW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetGlyphOutline: Pointer;

function GetGlyphOutline;
begin
  GetProcedureAddress(_GetGlyphOutline, gdi32, 'GetGlyphOutlineW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetGlyphOutline]
  end;
end;
{$ELSE}
function GetGlyphOutline; external gdi32 name 'GetGlyphOutlineW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetGlyphOutline: Pointer;

function GetGlyphOutline;
begin
  GetProcedureAddress(_GetGlyphOutline, gdi32, 'GetGlyphOutlineA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetGlyphOutline]
  end;
end;
{$ELSE}
function GetGlyphOutline; external gdi32 name 'GetGlyphOutlineA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetGraphicsMode: Pointer;

function GetGraphicsMode;
begin
  GetProcedureAddress(_GetGraphicsMode, gdi32, 'GetGraphicsMode');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetGraphicsMode]
  end;
end;
{$ELSE}
function GetGraphicsMode; external gdi32 name 'GetGraphicsMode';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetMapMode: Pointer;

function GetMapMode;
begin
  GetProcedureAddress(_GetMapMode, gdi32, 'GetMapMode');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetMapMode]
  end;
end;
{$ELSE}
function GetMapMode; external gdi32 name 'GetMapMode';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetMetaFileBitsEx: Pointer;

function GetMetaFileBitsEx;
begin
  GetProcedureAddress(_GetMetaFileBitsEx, gdi32, 'GetMetaFileBitsEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetMetaFileBitsEx]
  end;
end;
{$ELSE}
function GetMetaFileBitsEx; external gdi32 name 'GetMetaFileBitsEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetMetaFileA: Pointer;

function GetMetaFileA;
begin
  GetProcedureAddress(_GetMetaFileA, gdi32, 'GetMetaFileA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetMetaFileA]
  end;
end;
{$ELSE}
function GetMetaFileA; external gdi32 name 'GetMetaFileA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetMetaFileW: Pointer;

function GetMetaFileW;
begin
  GetProcedureAddress(_GetMetaFileW, gdi32, 'GetMetaFileW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetMetaFileW]
  end;
end;
{$ELSE}
function GetMetaFileW; external gdi32 name 'GetMetaFileW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetMetaFile: Pointer;

function GetMetaFile;
begin
  GetProcedureAddress(_GetMetaFile, gdi32, 'GetMetaFileW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetMetaFile]
  end;
end;
{$ELSE}
function GetMetaFile; external gdi32 name 'GetMetaFileW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetMetaFile: Pointer;

function GetMetaFile;
begin
  GetProcedureAddress(_GetMetaFile, gdi32, 'GetMetaFileA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetMetaFile]
  end;
end;
{$ELSE}
function GetMetaFile; external gdi32 name 'GetMetaFileA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetNearestColor: Pointer;

function GetNearestColor;
begin
  GetProcedureAddress(_GetNearestColor, gdi32, 'GetNearestColor');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetNearestColor]
  end;
end;
{$ELSE}
function GetNearestColor; external gdi32 name 'GetNearestColor';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetNearestPaletteIndex: Pointer;

function GetNearestPaletteIndex;
begin
  GetProcedureAddress(_GetNearestPaletteIndex, gdi32, 'GetNearestPaletteIndex');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetNearestPaletteIndex]
  end;
end;
{$ELSE}
function GetNearestPaletteIndex; external gdi32 name 'GetNearestPaletteIndex';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetObjectType: Pointer;

function GetObjectType;
begin
  GetProcedureAddress(_GetObjectType, gdi32, 'GetObjectType');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetObjectType]
  end;
end;
{$ELSE}
function GetObjectType; external gdi32 name 'GetObjectType';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetOutlineTextMetricsA: Pointer;

function GetOutlineTextMetricsA;
begin
  GetProcedureAddress(_GetOutlineTextMetricsA, gdi32, 'GetOutlineTextMetricsA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetOutlineTextMetricsA]
  end;
end;
{$ELSE}
function GetOutlineTextMetricsA; external gdi32 name 'GetOutlineTextMetricsA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetOutlineTextMetricsW: Pointer;

function GetOutlineTextMetricsW;
begin
  GetProcedureAddress(_GetOutlineTextMetricsW, gdi32, 'GetOutlineTextMetricsW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetOutlineTextMetricsW]
  end;
end;
{$ELSE}
function GetOutlineTextMetricsW; external gdi32 name 'GetOutlineTextMetricsW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetOutlineTextMetrics: Pointer;

function GetOutlineTextMetrics;
begin
  GetProcedureAddress(_GetOutlineTextMetrics, gdi32, 'GetOutlineTextMetricsW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetOutlineTextMetrics]
  end;
end;
{$ELSE}
function GetOutlineTextMetrics; external gdi32 name 'GetOutlineTextMetricsW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetOutlineTextMetrics: Pointer;

function GetOutlineTextMetrics;
begin
  GetProcedureAddress(_GetOutlineTextMetrics, gdi32, 'GetOutlineTextMetricsA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetOutlineTextMetrics]
  end;
end;
{$ELSE}
function GetOutlineTextMetrics; external gdi32 name 'GetOutlineTextMetricsA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetPaletteEntries: Pointer;

function GetPaletteEntries;
begin
  GetProcedureAddress(_GetPaletteEntries, gdi32, 'GetPaletteEntries');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetPaletteEntries]
  end;
end;
{$ELSE}
function GetPaletteEntries; external gdi32 name 'GetPaletteEntries';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetPixel: Pointer;

function GetPixel;
begin
  GetProcedureAddress(_GetPixel, gdi32, 'GetPixel');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetPixel]
  end;
end;
{$ELSE}
function GetPixel; external gdi32 name 'GetPixel';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetPixelFormat: Pointer;

function GetPixelFormat;
begin
  GetProcedureAddress(_GetPixelFormat, gdi32, 'GetPixelFormat');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetPixelFormat]
  end;
end;
{$ELSE}
function GetPixelFormat; external gdi32 name 'GetPixelFormat';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetPolyFillMode: Pointer;

function GetPolyFillMode;
begin
  GetProcedureAddress(_GetPolyFillMode, gdi32, 'GetPolyFillMode');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetPolyFillMode]
  end;
end;
{$ELSE}
function GetPolyFillMode; external gdi32 name 'GetPolyFillMode';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetRasterizerCaps: Pointer;

function GetRasterizerCaps;
begin
  GetProcedureAddress(_GetRasterizerCaps, gdi32, 'GetRasterizerCaps');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetRasterizerCaps]
  end;
end;
{$ELSE}
function GetRasterizerCaps; external gdi32 name 'GetRasterizerCaps';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetRandomRgn: Pointer;

function GetRandomRgn;
begin
  GetProcedureAddress(_GetRandomRgn, gdi32, 'GetRandomRgn');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetRandomRgn]
  end;
end;
{$ELSE}
function GetRandomRgn; external gdi32 name 'GetRandomRgn';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetRegionData: Pointer;

function GetRegionData;
begin
  GetProcedureAddress(_GetRegionData, gdi32, 'GetRegionData');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetRegionData]
  end;
end;
{$ELSE}
function GetRegionData; external gdi32 name 'GetRegionData';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetRgnBox: Pointer;

function GetRgnBox;
begin
  GetProcedureAddress(_GetRgnBox, gdi32, 'GetRgnBox');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetRgnBox]
  end;
end;
{$ELSE}
function GetRgnBox; external gdi32 name 'GetRgnBox';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetStockObject: Pointer;

function GetStockObject;
begin
  GetProcedureAddress(_GetStockObject, gdi32, 'GetStockObject');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetStockObject]
  end;
end;
{$ELSE}
function GetStockObject; external gdi32 name 'GetStockObject';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetStretchBltMode: Pointer;

function GetStretchBltMode;
begin
  GetProcedureAddress(_GetStretchBltMode, gdi32, 'GetStretchBltMode');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetStretchBltMode]
  end;
end;
{$ELSE}
function GetStretchBltMode; external gdi32 name 'GetStretchBltMode';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetSystemPaletteEntries: Pointer;

function GetSystemPaletteEntries;
begin
  GetProcedureAddress(_GetSystemPaletteEntries, gdi32, 'GetSystemPaletteEntries');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetSystemPaletteEntries]
  end;
end;
{$ELSE}
function GetSystemPaletteEntries; external gdi32 name 'GetSystemPaletteEntries';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetSystemPaletteUse: Pointer;

function GetSystemPaletteUse;
begin
  GetProcedureAddress(_GetSystemPaletteUse, gdi32, 'GetSystemPaletteUse');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetSystemPaletteUse]
  end;
end;
{$ELSE}
function GetSystemPaletteUse; external gdi32 name 'GetSystemPaletteUse';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetTextCharacterExtra: Pointer;

function GetTextCharacterExtra;
begin
  GetProcedureAddress(_GetTextCharacterExtra, gdi32, 'GetTextCharacterExtra');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetTextCharacterExtra]
  end;
end;
{$ELSE}
function GetTextCharacterExtra; external gdi32 name 'GetTextCharacterExtra';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetTextAlign: Pointer;

function GetTextAlign;
begin
  GetProcedureAddress(_GetTextAlign, gdi32, 'GetTextAlign');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetTextAlign]
  end;
end;
{$ELSE}
function GetTextAlign; external gdi32 name 'GetTextAlign';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetTextColor: Pointer;

function GetTextColor;
begin
  GetProcedureAddress(_GetTextColor, gdi32, 'GetTextColor');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetTextColor]
  end;
end;
{$ELSE}
function GetTextColor; external gdi32 name 'GetTextColor';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetTextExtentPointA: Pointer;

function GetTextExtentPointA;
begin
  GetProcedureAddress(_GetTextExtentPointA, gdi32, 'GetTextExtentPointA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetTextExtentPointA]
  end;
end;
{$ELSE}
function GetTextExtentPointA; external gdi32 name 'GetTextExtentPointA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetTextExtentPointW: Pointer;

function GetTextExtentPointW;
begin
  GetProcedureAddress(_GetTextExtentPointW, gdi32, 'GetTextExtentPointW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetTextExtentPointW]
  end;
end;
{$ELSE}
function GetTextExtentPointW; external gdi32 name 'GetTextExtentPointW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetTextExtentPoint: Pointer;

function GetTextExtentPoint;
begin
  GetProcedureAddress(_GetTextExtentPoint, gdi32, 'GetTextExtentPointW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetTextExtentPoint]
  end;
end;
{$ELSE}
function GetTextExtentPoint; external gdi32 name 'GetTextExtentPointW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetTextExtentPoint: Pointer;

function GetTextExtentPoint;
begin
  GetProcedureAddress(_GetTextExtentPoint, gdi32, 'GetTextExtentPointA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetTextExtentPoint]
  end;
end;
{$ELSE}
function GetTextExtentPoint; external gdi32 name 'GetTextExtentPointA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetTextExtentPoint32A: Pointer;

function GetTextExtentPoint32A;
begin
  GetProcedureAddress(_GetTextExtentPoint32A, gdi32, 'GetTextExtentPoint32A');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetTextExtentPoint32A]
  end;
end;
{$ELSE}
function GetTextExtentPoint32A; external gdi32 name 'GetTextExtentPoint32A';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetTextExtentPoint32W: Pointer;

function GetTextExtentPoint32W;
begin
  GetProcedureAddress(_GetTextExtentPoint32W, gdi32, 'GetTextExtentPoint32W');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetTextExtentPoint32W]
  end;
end;
{$ELSE}
function GetTextExtentPoint32W; external gdi32 name 'GetTextExtentPoint32W';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetTextExtentPoint32: Pointer;

function GetTextExtentPoint32;
begin
  GetProcedureAddress(_GetTextExtentPoint32, gdi32, 'GetTextExtentPoint32W');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetTextExtentPoint32]
  end;
end;
{$ELSE}
function GetTextExtentPoint32; external gdi32 name 'GetTextExtentPoint32W';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetTextExtentPoint32: Pointer;

function GetTextExtentPoint32;
begin
  GetProcedureAddress(_GetTextExtentPoint32, gdi32, 'GetTextExtentPoint32A');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetTextExtentPoint32]
  end;
end;
{$ELSE}
function GetTextExtentPoint32; external gdi32 name 'GetTextExtentPoint32A';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetTextExtentExPointA: Pointer;

function GetTextExtentExPointA;
begin
  GetProcedureAddress(_GetTextExtentExPointA, gdi32, 'GetTextExtentExPointA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetTextExtentExPointA]
  end;
end;
{$ELSE}
function GetTextExtentExPointA; external gdi32 name 'GetTextExtentExPointA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetTextExtentExPointW: Pointer;

function GetTextExtentExPointW;
begin
  GetProcedureAddress(_GetTextExtentExPointW, gdi32, 'GetTextExtentExPointW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetTextExtentExPointW]
  end;
end;
{$ELSE}
function GetTextExtentExPointW; external gdi32 name 'GetTextExtentExPointW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetTextExtentExPoint: Pointer;

function GetTextExtentExPoint;
begin
  GetProcedureAddress(_GetTextExtentExPoint, gdi32, 'GetTextExtentExPointW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetTextExtentExPoint]
  end;
end;
{$ELSE}
function GetTextExtentExPoint; external gdi32 name 'GetTextExtentExPointW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetTextExtentExPoint: Pointer;

function GetTextExtentExPoint;
begin
  GetProcedureAddress(_GetTextExtentExPoint, gdi32, 'GetTextExtentExPointA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetTextExtentExPoint]
  end;
end;
{$ELSE}
function GetTextExtentExPoint; external gdi32 name 'GetTextExtentExPointA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetTextCharset: Pointer;

function GetTextCharset;
begin
  GetProcedureAddress(_GetTextCharset, gdi32, 'GetTextCharset');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetTextCharset]
  end;
end;
{$ELSE}
function GetTextCharset; external gdi32 name 'GetTextCharset';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetTextCharsetInfo: Pointer;

function GetTextCharsetInfo;
begin
  GetProcedureAddress(_GetTextCharsetInfo, gdi32, 'GetTextCharsetInfo');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetTextCharsetInfo]
  end;
end;
{$ELSE}
function GetTextCharsetInfo; external gdi32 name 'GetTextCharsetInfo';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _TranslateCharsetInfo: Pointer;

function TranslateCharsetInfo;
begin
  GetProcedureAddress(_TranslateCharsetInfo, gdi32, 'TranslateCharsetInfo');
  asm
    mov esp, ebp
    pop ebp
    jmp [_TranslateCharsetInfo]
  end;
end;
{$ELSE}
function TranslateCharsetInfo; external gdi32 name 'TranslateCharsetInfo';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetFontLanguageInfo: Pointer;

function GetFontLanguageInfo;
begin
  GetProcedureAddress(_GetFontLanguageInfo, gdi32, 'GetFontLanguageInfo');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetFontLanguageInfo]
  end;
end;
{$ELSE}
function GetFontLanguageInfo; external gdi32 name 'GetFontLanguageInfo';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetCharacterPlacementA: Pointer;

function GetCharacterPlacementA;
begin
  GetProcedureAddress(_GetCharacterPlacementA, gdi32, 'GetCharacterPlacementA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCharacterPlacementA]
  end;
end;
{$ELSE}
function GetCharacterPlacementA; external gdi32 name 'GetCharacterPlacementA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetCharacterPlacementW: Pointer;

function GetCharacterPlacementW;
begin
  GetProcedureAddress(_GetCharacterPlacementW, gdi32, 'GetCharacterPlacementW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCharacterPlacementW]
  end;
end;
{$ELSE}
function GetCharacterPlacementW; external gdi32 name 'GetCharacterPlacementW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetCharacterPlacement: Pointer;

function GetCharacterPlacement;
begin
  GetProcedureAddress(_GetCharacterPlacement, gdi32, 'GetCharacterPlacementW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCharacterPlacement]
  end;
end;
{$ELSE}
function GetCharacterPlacement; external gdi32 name 'GetCharacterPlacementW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetCharacterPlacement: Pointer;

function GetCharacterPlacement;
begin
  GetProcedureAddress(_GetCharacterPlacement, gdi32, 'GetCharacterPlacementA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCharacterPlacement]
  end;
end;
{$ELSE}
function GetCharacterPlacement; external gdi32 name 'GetCharacterPlacementA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetFontUnicodeRanges: Pointer;

function GetFontUnicodeRanges;
begin
  GetProcedureAddress(_GetFontUnicodeRanges, gdi32, 'GetFontUnicodeRanges');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetFontUnicodeRanges]
  end;
end;
{$ELSE}
function GetFontUnicodeRanges; external gdi32 name 'GetFontUnicodeRanges';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetGlyphIndicesA: Pointer;

function GetGlyphIndicesA;
begin
  GetProcedureAddress(_GetGlyphIndicesA, gdi32, 'GetGlyphIndicesA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetGlyphIndicesA]
  end;
end;
{$ELSE}
function GetGlyphIndicesA; external gdi32 name 'GetGlyphIndicesA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetGlyphIndicesW: Pointer;

function GetGlyphIndicesW;
begin
  GetProcedureAddress(_GetGlyphIndicesW, gdi32, 'GetGlyphIndicesW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetGlyphIndicesW]
  end;
end;
{$ELSE}
function GetGlyphIndicesW; external gdi32 name 'GetGlyphIndicesW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetGlyphIndices: Pointer;

function GetGlyphIndices;
begin
  GetProcedureAddress(_GetGlyphIndices, gdi32, 'GetGlyphIndicesW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetGlyphIndices]
  end;
end;
{$ELSE}
function GetGlyphIndices; external gdi32 name 'GetGlyphIndicesW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetGlyphIndices: Pointer;

function GetGlyphIndices;
begin
  GetProcedureAddress(_GetGlyphIndices, gdi32, 'GetGlyphIndicesA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetGlyphIndices]
  end;
end;
{$ELSE}
function GetGlyphIndices; external gdi32 name 'GetGlyphIndicesA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetTextExtentPointI: Pointer;

function GetTextExtentPointI;
begin
  GetProcedureAddress(_GetTextExtentPointI, gdi32, 'GetTextExtentPointI');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetTextExtentPointI]
  end;
end;
{$ELSE}
function GetTextExtentPointI; external gdi32 name 'GetTextExtentPointI';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetTextExtentExPointI: Pointer;

function GetTextExtentExPointI;
begin
  GetProcedureAddress(_GetTextExtentExPointI, gdi32, 'GetTextExtentExPointI');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetTextExtentExPointI]
  end;
end;
{$ELSE}
function GetTextExtentExPointI; external gdi32 name 'GetTextExtentExPointI';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetCharWidthI: Pointer;

function GetCharWidthI;
begin
  GetProcedureAddress(_GetCharWidthI, gdi32, 'GetCharWidthI');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCharWidthI]
  end;
end;
{$ELSE}
function GetCharWidthI; external gdi32 name 'GetCharWidthI';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetCharABCWidthsI: Pointer;

function GetCharABCWidthsI;
begin
  GetProcedureAddress(_GetCharABCWidthsI, gdi32, 'GetCharABCWidthsI');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCharABCWidthsI]
  end;
end;
{$ELSE}
function GetCharABCWidthsI; external gdi32 name 'GetCharABCWidthsI';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _AddFontResourceExA: Pointer;

function AddFontResourceExA;
begin
  GetProcedureAddress(_AddFontResourceExA, gdi32, 'AddFontResourceExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AddFontResourceExA]
  end;
end;
{$ELSE}
function AddFontResourceExA; external gdi32 name 'AddFontResourceExA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _AddFontResourceExW: Pointer;

function AddFontResourceExW;
begin
  GetProcedureAddress(_AddFontResourceExW, gdi32, 'AddFontResourceExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AddFontResourceExW]
  end;
end;
{$ELSE}
function AddFontResourceExW; external gdi32 name 'AddFontResourceExW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _AddFontResourceEx: Pointer;

function AddFontResourceEx;
begin
  GetProcedureAddress(_AddFontResourceEx, gdi32, 'AddFontResourceExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AddFontResourceEx]
  end;
end;
{$ELSE}
function AddFontResourceEx; external gdi32 name 'AddFontResourceExW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _AddFontResourceEx: Pointer;

function AddFontResourceEx;
begin
  GetProcedureAddress(_AddFontResourceEx, gdi32, 'AddFontResourceExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AddFontResourceEx]
  end;
end;
{$ELSE}
function AddFontResourceEx; external gdi32 name 'AddFontResourceExA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _RemoveFontResourceExA: Pointer;

function RemoveFontResourceExA;
begin
  GetProcedureAddress(_RemoveFontResourceExA, gdi32, 'RemoveFontResourceExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_RemoveFontResourceExA]
  end;
end;
{$ELSE}
function RemoveFontResourceExA; external gdi32 name 'RemoveFontResourceExA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _RemoveFontResourceExW: Pointer;

function RemoveFontResourceExW;
begin
  GetProcedureAddress(_RemoveFontResourceExW, gdi32, 'RemoveFontResourceExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_RemoveFontResourceExW]
  end;
end;
{$ELSE}
function RemoveFontResourceExW; external gdi32 name 'RemoveFontResourceExW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _RemoveFontResourceEx: Pointer;

function RemoveFontResourceEx;
begin
  GetProcedureAddress(_RemoveFontResourceEx, gdi32, 'RemoveFontResourceExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_RemoveFontResourceEx]
  end;
end;
{$ELSE}
function RemoveFontResourceEx; external gdi32 name 'RemoveFontResourceExW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _RemoveFontResourceEx: Pointer;

function RemoveFontResourceEx;
begin
  GetProcedureAddress(_RemoveFontResourceEx, gdi32, 'RemoveFontResourceExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_RemoveFontResourceEx]
  end;
end;
{$ELSE}
function RemoveFontResourceEx; external gdi32 name 'RemoveFontResourceExA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _AddFontMemResourceEx: Pointer;

function AddFontMemResourceEx;
begin
  GetProcedureAddress(_AddFontMemResourceEx, gdi32, 'AddFontMemResourceEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AddFontMemResourceEx]
  end;
end;
{$ELSE}
function AddFontMemResourceEx; external gdi32 name 'AddFontMemResourceEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _RemoveFontMemResourceEx: Pointer;

function RemoveFontMemResourceEx;
begin
  GetProcedureAddress(_RemoveFontMemResourceEx, gdi32, 'RemoveFontMemResourceEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_RemoveFontMemResourceEx]
  end;
end;
{$ELSE}
function RemoveFontMemResourceEx; external gdi32 name 'RemoveFontMemResourceEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateFontIndirectExA: Pointer;

function CreateFontIndirectExA;
begin
  GetProcedureAddress(_CreateFontIndirectExA, gdi32, 'CreateFontIndirectExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateFontIndirectExA]
  end;
end;
{$ELSE}
function CreateFontIndirectExA; external gdi32 name 'CreateFontIndirectExA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateFontIndirectExW: Pointer;

function CreateFontIndirectExW;
begin
  GetProcedureAddress(_CreateFontIndirectExW, gdi32, 'CreateFontIndirectExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateFontIndirectExW]
  end;
end;
{$ELSE}
function CreateFontIndirectExW; external gdi32 name 'CreateFontIndirectExW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _CreateFontIndirectEx: Pointer;

function CreateFontIndirectEx;
begin
  GetProcedureAddress(_CreateFontIndirectEx, gdi32, 'CreateFontIndirectExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateFontIndirectEx]
  end;
end;
{$ELSE}
function CreateFontIndirectEx; external gdi32 name 'CreateFontIndirectExW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _CreateFontIndirectEx: Pointer;

function CreateFontIndirectEx;
begin
  GetProcedureAddress(_CreateFontIndirectEx, gdi32, 'CreateFontIndirectExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateFontIndirectEx]
  end;
end;
{$ELSE}
function CreateFontIndirectEx; external gdi32 name 'CreateFontIndirectExA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetViewportExtEx: Pointer;

function GetViewportExtEx;
begin
  GetProcedureAddress(_GetViewportExtEx, gdi32, 'GetViewportExtEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetViewportExtEx]
  end;
end;
{$ELSE}
function GetViewportExtEx; external gdi32 name 'GetViewportExtEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetViewportOrgEx: Pointer;

function GetViewportOrgEx;
begin
  GetProcedureAddress(_GetViewportOrgEx, gdi32, 'GetViewportOrgEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetViewportOrgEx]
  end;
end;
{$ELSE}
function GetViewportOrgEx; external gdi32 name 'GetViewportOrgEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetWindowExtEx: Pointer;

function GetWindowExtEx;
begin
  GetProcedureAddress(_GetWindowExtEx, gdi32, 'GetWindowExtEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetWindowExtEx]
  end;
end;
{$ELSE}
function GetWindowExtEx; external gdi32 name 'GetWindowExtEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetWindowOrgEx: Pointer;

function GetWindowOrgEx;
begin
  GetProcedureAddress(_GetWindowOrgEx, gdi32, 'GetWindowOrgEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetWindowOrgEx]
  end;
end;
{$ELSE}
function GetWindowOrgEx; external gdi32 name 'GetWindowOrgEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _IntersectClipRect: Pointer;

function IntersectClipRect;
begin
  GetProcedureAddress(_IntersectClipRect, gdi32, 'IntersectClipRect');
  asm
    mov esp, ebp
    pop ebp
    jmp [_IntersectClipRect]
  end;
end;
{$ELSE}
function IntersectClipRect; external gdi32 name 'IntersectClipRect';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _InvertRgn: Pointer;

function InvertRgn;
begin
  GetProcedureAddress(_InvertRgn, gdi32, 'InvertRgn');
  asm
    mov esp, ebp
    pop ebp
    jmp [_InvertRgn]
  end;
end;
{$ELSE}
function InvertRgn; external gdi32 name 'InvertRgn';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _LineDDA: Pointer;

function LineDDA;
begin
  GetProcedureAddress(_LineDDA, gdi32, 'LineDDA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LineDDA]
  end;
end;
{$ELSE}
function LineDDA; external gdi32 name 'LineDDA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _LineTo: Pointer;

function LineTo;
begin
  GetProcedureAddress(_LineTo, gdi32, 'LineTo');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LineTo]
  end;
end;
{$ELSE}
function LineTo; external gdi32 name 'LineTo';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MaskBlt: Pointer;

function MaskBlt;
begin
  GetProcedureAddress(_MaskBlt, gdi32, 'MaskBlt');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MaskBlt]
  end;
end;
{$ELSE}
function MaskBlt; external gdi32 name 'MaskBlt';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _PlgBlt: Pointer;

function PlgBlt;
begin
  GetProcedureAddress(_PlgBlt, gdi32, 'PlgBlt');
  asm
    mov esp, ebp
    pop ebp
    jmp [_PlgBlt]
  end;
end;
{$ELSE}
function PlgBlt; external gdi32 name 'PlgBlt';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _OffsetClipRgn: Pointer;

function OffsetClipRgn;
begin
  GetProcedureAddress(_OffsetClipRgn, gdi32, 'OffsetClipRgn');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OffsetClipRgn]
  end;
end;
{$ELSE}
function OffsetClipRgn; external gdi32 name 'OffsetClipRgn';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _OffsetRgn: Pointer;

function OffsetRgn;
begin
  GetProcedureAddress(_OffsetRgn, gdi32, 'OffsetRgn');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OffsetRgn]
  end;
end;
{$ELSE}
function OffsetRgn; external gdi32 name 'OffsetRgn';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _PatBlt: Pointer;

function PatBlt;
begin
  GetProcedureAddress(_PatBlt, gdi32, 'PatBlt');
  asm
    mov esp, ebp
    pop ebp
    jmp [_PatBlt]
  end;
end;
{$ELSE}
function PatBlt; external gdi32 name 'PatBlt';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _Pie: Pointer;

function Pie;
begin
  GetProcedureAddress(_Pie, gdi32, 'Pie');
  asm
    mov esp, ebp
    pop ebp
    jmp [_Pie]
  end;
end;
{$ELSE}
function Pie; external gdi32 name 'Pie';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _PlayMetaFile: Pointer;

function PlayMetaFile;
begin
  GetProcedureAddress(_PlayMetaFile, gdi32, 'PlayMetaFile');
  asm
    mov esp, ebp
    pop ebp
    jmp [_PlayMetaFile]
  end;
end;
{$ELSE}
function PlayMetaFile; external gdi32 name 'PlayMetaFile';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _PaintRgn: Pointer;

function PaintRgn;
begin
  GetProcedureAddress(_PaintRgn, gdi32, 'PaintRgn');
  asm
    mov esp, ebp
    pop ebp
    jmp [_PaintRgn]
  end;
end;
{$ELSE}
function PaintRgn; external gdi32 name 'PaintRgn';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _PolyPolygon: Pointer;

function PolyPolygon;
begin
  GetProcedureAddress(_PolyPolygon, gdi32, 'PolyPolygon');
  asm
    mov esp, ebp
    pop ebp
    jmp [_PolyPolygon]
  end;
end;
{$ELSE}
function PolyPolygon; external gdi32 name 'PolyPolygon';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _PtInRegion: Pointer;

function PtInRegion;
begin
  GetProcedureAddress(_PtInRegion, gdi32, 'PtInRegion');
  asm
    mov esp, ebp
    pop ebp
    jmp [_PtInRegion]
  end;
end;
{$ELSE}
function PtInRegion; external gdi32 name 'PtInRegion';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _PtVisible: Pointer;

function PtVisible;
begin
  GetProcedureAddress(_PtVisible, gdi32, 'PtVisible');
  asm
    mov esp, ebp
    pop ebp
    jmp [_PtVisible]
  end;
end;
{$ELSE}
function PtVisible; external gdi32 name 'PtVisible';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _RectInRegion: Pointer;

function RectInRegion;
begin
  GetProcedureAddress(_RectInRegion, gdi32, 'RectInRegion');
  asm
    mov esp, ebp
    pop ebp
    jmp [_RectInRegion]
  end;
end;
{$ELSE}
function RectInRegion; external gdi32 name 'RectInRegion';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _RectVisible: Pointer;

function RectVisible;
begin
  GetProcedureAddress(_RectVisible, gdi32, 'RectVisible');
  asm
    mov esp, ebp
    pop ebp
    jmp [_RectVisible]
  end;
end;
{$ELSE}
function RectVisible; external gdi32 name 'RectVisible';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _Rectangle: Pointer;

function Rectangle;
begin
  GetProcedureAddress(_Rectangle, gdi32, 'Rectangle');
  asm
    mov esp, ebp
    pop ebp
    jmp [_Rectangle]
  end;
end;
{$ELSE}
function Rectangle; external gdi32 name 'Rectangle';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _RestoreDC: Pointer;

function RestoreDC;
begin
  GetProcedureAddress(_RestoreDC, gdi32, 'RestoreDC');
  asm
    mov esp, ebp
    pop ebp
    jmp [_RestoreDC]
  end;
end;
{$ELSE}
function RestoreDC; external gdi32 name 'RestoreDC';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ResetDCA: Pointer;

function ResetDCA;
begin
  GetProcedureAddress(_ResetDCA, gdi32, 'ResetDCA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ResetDCA]
  end;
end;
{$ELSE}
function ResetDCA; external gdi32 name 'ResetDCA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ResetDCW: Pointer;

function ResetDCW;
begin
  GetProcedureAddress(_ResetDCW, gdi32, 'ResetDCW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ResetDCW]
  end;
end;
{$ELSE}
function ResetDCW; external gdi32 name 'ResetDCW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _ResetDC: Pointer;

function ResetDC;
begin
  GetProcedureAddress(_ResetDC, gdi32, 'ResetDCW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ResetDC]
  end;
end;
{$ELSE}
function ResetDC; external gdi32 name 'ResetDCW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _ResetDC: Pointer;

function ResetDC;
begin
  GetProcedureAddress(_ResetDC, gdi32, 'ResetDCA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ResetDC]
  end;
end;
{$ELSE}
function ResetDC; external gdi32 name 'ResetDCA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _RealizePalette: Pointer;

function RealizePalette;
begin
  GetProcedureAddress(_RealizePalette, gdi32, 'RealizePalette');
  asm
    mov esp, ebp
    pop ebp
    jmp [_RealizePalette]
  end;
end;
{$ELSE}
function RealizePalette; external gdi32 name 'RealizePalette';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _RemoveFontResourceA: Pointer;

function RemoveFontResourceA;
begin
  GetProcedureAddress(_RemoveFontResourceA, gdi32, 'RemoveFontResourceA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_RemoveFontResourceA]
  end;
end;
{$ELSE}
function RemoveFontResourceA; external gdi32 name 'RemoveFontResourceA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _RemoveFontResourceW: Pointer;

function RemoveFontResourceW;
begin
  GetProcedureAddress(_RemoveFontResourceW, gdi32, 'RemoveFontResourceW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_RemoveFontResourceW]
  end;
end;
{$ELSE}
function RemoveFontResourceW; external gdi32 name 'RemoveFontResourceW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _RemoveFontResource: Pointer;

function RemoveFontResource;
begin
  GetProcedureAddress(_RemoveFontResource, gdi32, 'RemoveFontResourceW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_RemoveFontResource]
  end;
end;
{$ELSE}
function RemoveFontResource; external gdi32 name 'RemoveFontResourceW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _RemoveFontResource: Pointer;

function RemoveFontResource;
begin
  GetProcedureAddress(_RemoveFontResource, gdi32, 'RemoveFontResourceA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_RemoveFontResource]
  end;
end;
{$ELSE}
function RemoveFontResource; external gdi32 name 'RemoveFontResourceA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _RoundRect: Pointer;

function RoundRect;
begin
  GetProcedureAddress(_RoundRect, gdi32, 'RoundRect');
  asm
    mov esp, ebp
    pop ebp
    jmp [_RoundRect]
  end;
end;
{$ELSE}
function RoundRect; external gdi32 name 'RoundRect';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ResizePalette: Pointer;

function ResizePalette;
begin
  GetProcedureAddress(_ResizePalette, gdi32, 'ResizePalette');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ResizePalette]
  end;
end;
{$ELSE}
function ResizePalette; external gdi32 name 'ResizePalette';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SaveDC: Pointer;

function SaveDC;
begin
  GetProcedureAddress(_SaveDC, gdi32, 'SaveDC');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SaveDC]
  end;
end;
{$ELSE}
function SaveDC; external gdi32 name 'SaveDC';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SelectClipRgn: Pointer;

function SelectClipRgn;
begin
  GetProcedureAddress(_SelectClipRgn, gdi32, 'SelectClipRgn');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SelectClipRgn]
  end;
end;
{$ELSE}
function SelectClipRgn; external gdi32 name 'SelectClipRgn';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ExtSelectClipRgn: Pointer;

function ExtSelectClipRgn;
begin
  GetProcedureAddress(_ExtSelectClipRgn, gdi32, 'ExtSelectClipRgn');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ExtSelectClipRgn]
  end;
end;
{$ELSE}
function ExtSelectClipRgn; external gdi32 name 'ExtSelectClipRgn';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetMetaRgn: Pointer;

function SetMetaRgn;
begin
  GetProcedureAddress(_SetMetaRgn, gdi32, 'SetMetaRgn');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetMetaRgn]
  end;
end;
{$ELSE}
function SetMetaRgn; external gdi32 name 'SetMetaRgn';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SelectObject: Pointer;

function SelectObject;
begin
  GetProcedureAddress(_SelectObject, gdi32, 'SelectObject');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SelectObject]
  end;
end;
{$ELSE}
function SelectObject; external gdi32 name 'SelectObject';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SelectPalette: Pointer;

function SelectPalette;
begin
  GetProcedureAddress(_SelectPalette, gdi32, 'SelectPalette');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SelectPalette]
  end;
end;
{$ELSE}
function SelectPalette; external gdi32 name 'SelectPalette';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetBkColor: Pointer;

function SetBkColor;
begin
  GetProcedureAddress(_SetBkColor, gdi32, 'SetBkColor');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetBkColor]
  end;
end;
{$ELSE}
function SetBkColor; external gdi32 name 'SetBkColor';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetDCBrushColor: Pointer;

function SetDCBrushColor;
begin
  GetProcedureAddress(_SetDCBrushColor, gdi32, 'SetDCBrushColor');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetDCBrushColor]
  end;
end;
{$ELSE}
function SetDCBrushColor; external gdi32 name 'SetDCBrushColor';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetDCPenColor: Pointer;

function SetDCPenColor;
begin
  GetProcedureAddress(_SetDCPenColor, gdi32, 'SetDCPenColor');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetDCPenColor]
  end;
end;
{$ELSE}
function SetDCPenColor; external gdi32 name 'SetDCPenColor';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetBkMode: Pointer;

function SetBkMode;
begin
  GetProcedureAddress(_SetBkMode, gdi32, 'SetBkMode');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetBkMode]
  end;
end;
{$ELSE}
function SetBkMode; external gdi32 name 'SetBkMode';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetBitmapBits: Pointer;

function SetBitmapBits;
begin
  GetProcedureAddress(_SetBitmapBits, gdi32, 'SetBitmapBits');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetBitmapBits]
  end;
end;
{$ELSE}
function SetBitmapBits; external gdi32 name 'SetBitmapBits';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetBoundsRect: Pointer;

function SetBoundsRect;
begin
  GetProcedureAddress(_SetBoundsRect, gdi32, 'SetBoundsRect');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetBoundsRect]
  end;
end;
{$ELSE}
function SetBoundsRect; external gdi32 name 'SetBoundsRect';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetDIBits: Pointer;

function SetDIBits;
begin
  GetProcedureAddress(_SetDIBits, gdi32, 'SetDIBits');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetDIBits]
  end;
end;
{$ELSE}
function SetDIBits; external gdi32 name 'SetDIBits';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetDIBitsToDevice: Pointer;

function SetDIBitsToDevice;
begin
  GetProcedureAddress(_SetDIBitsToDevice, gdi32, 'SetDIBitsToDevice');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetDIBitsToDevice]
  end;
end;
{$ELSE}
function SetDIBitsToDevice; external gdi32 name 'SetDIBitsToDevice';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetMapperFlags: Pointer;

function SetMapperFlags;
begin
  GetProcedureAddress(_SetMapperFlags, gdi32, 'SetMapperFlags');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetMapperFlags]
  end;
end;
{$ELSE}
function SetMapperFlags; external gdi32 name 'SetMapperFlags';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetGraphicsMode: Pointer;

function SetGraphicsMode;
begin
  GetProcedureAddress(_SetGraphicsMode, gdi32, 'SetGraphicsMode');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetGraphicsMode]
  end;
end;
{$ELSE}
function SetGraphicsMode; external gdi32 name 'SetGraphicsMode';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetMapMode: Pointer;

function SetMapMode;
begin
  GetProcedureAddress(_SetMapMode, gdi32, 'SetMapMode');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetMapMode]
  end;
end;
{$ELSE}
function SetMapMode; external gdi32 name 'SetMapMode';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetLayout: Pointer;

function SetLayout;
begin
  GetProcedureAddress(_SetLayout, gdi32, 'SetLayout');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetLayout]
  end;
end;
{$ELSE}
function SetLayout; external gdi32 name 'SetLayout';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetLayout: Pointer;

function GetLayout;
begin
  GetProcedureAddress(_GetLayout, gdi32, 'GetLayout');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetLayout]
  end;
end;
{$ELSE}
function GetLayout; external gdi32 name 'GetLayout';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetMetaFileBitsEx: Pointer;

function SetMetaFileBitsEx;
begin
  GetProcedureAddress(_SetMetaFileBitsEx, gdi32, 'SetMetaFileBitsEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetMetaFileBitsEx]
  end;
end;
{$ELSE}
function SetMetaFileBitsEx; external gdi32 name 'SetMetaFileBitsEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetPaletteEntries: Pointer;

function SetPaletteEntries;
begin
  GetProcedureAddress(_SetPaletteEntries, gdi32, 'SetPaletteEntries');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetPaletteEntries]
  end;
end;
{$ELSE}
function SetPaletteEntries; external gdi32 name 'SetPaletteEntries';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetPixel: Pointer;

function SetPixel;
begin
  GetProcedureAddress(_SetPixel, gdi32, 'SetPixel');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetPixel]
  end;
end;
{$ELSE}
function SetPixel; external gdi32 name 'SetPixel';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetPixelV: Pointer;

function SetPixelV;
begin
  GetProcedureAddress(_SetPixelV, gdi32, 'SetPixelV');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetPixelV]
  end;
end;
{$ELSE}
function SetPixelV; external gdi32 name 'SetPixelV';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetPixelFormat: Pointer;

function SetPixelFormat;
begin
  GetProcedureAddress(_SetPixelFormat, gdi32, 'SetPixelFormat');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetPixelFormat]
  end;
end;
{$ELSE}
function SetPixelFormat; external gdi32 name 'SetPixelFormat';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetPolyFillMode: Pointer;

function SetPolyFillMode;
begin
  GetProcedureAddress(_SetPolyFillMode, gdi32, 'SetPolyFillMode');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetPolyFillMode]
  end;
end;
{$ELSE}
function SetPolyFillMode; external gdi32 name 'SetPolyFillMode';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _StretchBlt: Pointer;

function StretchBlt;
begin
  GetProcedureAddress(_StretchBlt, gdi32, 'StretchBlt');
  asm
    mov esp, ebp
    pop ebp
    jmp [_StretchBlt]
  end;
end;
{$ELSE}
function StretchBlt; external gdi32 name 'StretchBlt';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetRectRgn: Pointer;

function SetRectRgn;
begin
  GetProcedureAddress(_SetRectRgn, gdi32, 'SetRectRgn');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetRectRgn]
  end;
end;
{$ELSE}
function SetRectRgn; external gdi32 name 'SetRectRgn';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _StretchDIBits: Pointer;

function StretchDIBits;
begin
  GetProcedureAddress(_StretchDIBits, gdi32, 'StretchDIBits');
  asm
    mov esp, ebp
    pop ebp
    jmp [_StretchDIBits]
  end;
end;
{$ELSE}
function StretchDIBits; external gdi32 name 'StretchDIBits';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetROP2: Pointer;

function SetROP2;
begin
  GetProcedureAddress(_SetROP2, gdi32, 'SetROP2');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetROP2]
  end;
end;
{$ELSE}
function SetROP2; external gdi32 name 'SetROP2';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetStretchBltMode: Pointer;

function SetStretchBltMode;
begin
  GetProcedureAddress(_SetStretchBltMode, gdi32, 'SetStretchBltMode');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetStretchBltMode]
  end;
end;
{$ELSE}
function SetStretchBltMode; external gdi32 name 'SetStretchBltMode';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetSystemPaletteUse: Pointer;

function SetSystemPaletteUse;
begin
  GetProcedureAddress(_SetSystemPaletteUse, gdi32, 'SetSystemPaletteUse');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetSystemPaletteUse]
  end;
end;
{$ELSE}
function SetSystemPaletteUse; external gdi32 name 'SetSystemPaletteUse';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetTextCharacterExtra: Pointer;

function SetTextCharacterExtra;
begin
  GetProcedureAddress(_SetTextCharacterExtra, gdi32, 'SetTextCharacterExtra');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetTextCharacterExtra]
  end;
end;
{$ELSE}
function SetTextCharacterExtra; external gdi32 name 'SetTextCharacterExtra';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetTextColor: Pointer;

function SetTextColor;
begin
  GetProcedureAddress(_SetTextColor, gdi32, 'SetTextColor');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetTextColor]
  end;
end;
{$ELSE}
function SetTextColor; external gdi32 name 'SetTextColor';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetTextAlign: Pointer;

function SetTextAlign;
begin
  GetProcedureAddress(_SetTextAlign, gdi32, 'SetTextAlign');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetTextAlign]
  end;
end;
{$ELSE}
function SetTextAlign; external gdi32 name 'SetTextAlign';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetTextJustification: Pointer;

function SetTextJustification;
begin
  GetProcedureAddress(_SetTextJustification, gdi32, 'SetTextJustification');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetTextJustification]
  end;
end;
{$ELSE}
function SetTextJustification; external gdi32 name 'SetTextJustification';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _UpdateColors: Pointer;

function UpdateColors;
begin
  GetProcedureAddress(_UpdateColors, gdi32, 'UpdateColors');
  asm
    mov esp, ebp
    pop ebp
    jmp [_UpdateColors]
  end;
end;
{$ELSE}
function UpdateColors; external gdi32 name 'UpdateColors';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _AlphaBlend: Pointer;

function AlphaBlend;
begin
  GetProcedureAddress(_AlphaBlend, msimg32, 'AlphaBlend');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AlphaBlend]
  end;
end;
{$ELSE}
function AlphaBlend; external msimg32 name 'AlphaBlend';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _TransparentBlt: Pointer;

function TransparentBlt;
begin
  GetProcedureAddress(_TransparentBlt, msimg32, 'TransparentBlt');
  asm
    mov esp, ebp
    pop ebp
    jmp [_TransparentBlt]
  end;
end;
{$ELSE}
function TransparentBlt; external msimg32 name 'TransparentBlt';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GradientFill: Pointer;

function GradientFill;
begin
  GetProcedureAddress(_GradientFill, msimg32, 'GradientFill');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GradientFill]
  end;
end;
{$ELSE}
function GradientFill; external msimg32 name 'GradientFill';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _PlayMetaFileRecord: Pointer;

function PlayMetaFileRecord;
begin
  GetProcedureAddress(_PlayMetaFileRecord, gdi32, 'PlayMetaFileRecord');
  asm
    mov esp, ebp
    pop ebp
    jmp [_PlayMetaFileRecord]
  end;
end;
{$ELSE}
function PlayMetaFileRecord; external gdi32 name 'PlayMetaFileRecord';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _EnumMetaFile: Pointer;

function EnumMetaFile;
begin
  GetProcedureAddress(_EnumMetaFile, gdi32, 'EnumMetaFile');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumMetaFile]
  end;
end;
{$ELSE}
function EnumMetaFile; external gdi32 name 'EnumMetaFile';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CloseEnhMetaFile: Pointer;

function CloseEnhMetaFile;
begin
  GetProcedureAddress(_CloseEnhMetaFile, gdi32, 'CloseEnhMetaFile');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CloseEnhMetaFile]
  end;
end;
{$ELSE}
function CloseEnhMetaFile; external gdi32 name 'CloseEnhMetaFile';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CopyEnhMetaFileA: Pointer;

function CopyEnhMetaFileA;
begin
  GetProcedureAddress(_CopyEnhMetaFileA, gdi32, 'CopyEnhMetaFileA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CopyEnhMetaFileA]
  end;
end;
{$ELSE}
function CopyEnhMetaFileA; external gdi32 name 'CopyEnhMetaFileA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CopyEnhMetaFileW: Pointer;

function CopyEnhMetaFileW;
begin
  GetProcedureAddress(_CopyEnhMetaFileW, gdi32, 'CopyEnhMetaFileW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CopyEnhMetaFileW]
  end;
end;
{$ELSE}
function CopyEnhMetaFileW; external gdi32 name 'CopyEnhMetaFileW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _CopyEnhMetaFile: Pointer;

function CopyEnhMetaFile;
begin
  GetProcedureAddress(_CopyEnhMetaFile, gdi32, 'CopyEnhMetaFileW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CopyEnhMetaFile]
  end;
end;
{$ELSE}
function CopyEnhMetaFile; external gdi32 name 'CopyEnhMetaFileW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _CopyEnhMetaFile: Pointer;

function CopyEnhMetaFile;
begin
  GetProcedureAddress(_CopyEnhMetaFile, gdi32, 'CopyEnhMetaFileA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CopyEnhMetaFile]
  end;
end;
{$ELSE}
function CopyEnhMetaFile; external gdi32 name 'CopyEnhMetaFileA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _CreateEnhMetaFileA: Pointer;

function CreateEnhMetaFileA;
begin
  GetProcedureAddress(_CreateEnhMetaFileA, gdi32, 'CreateEnhMetaFileA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateEnhMetaFileA]
  end;
end;
{$ELSE}
function CreateEnhMetaFileA; external gdi32 name 'CreateEnhMetaFileA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateEnhMetaFileW: Pointer;

function CreateEnhMetaFileW;
begin
  GetProcedureAddress(_CreateEnhMetaFileW, gdi32, 'CreateEnhMetaFileW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateEnhMetaFileW]
  end;
end;
{$ELSE}
function CreateEnhMetaFileW; external gdi32 name 'CreateEnhMetaFileW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _CreateEnhMetaFile: Pointer;

function CreateEnhMetaFile;
begin
  GetProcedureAddress(_CreateEnhMetaFile, gdi32, 'CreateEnhMetaFileW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateEnhMetaFile]
  end;
end;
{$ELSE}
function CreateEnhMetaFile; external gdi32 name 'CreateEnhMetaFileW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _CreateEnhMetaFile: Pointer;

function CreateEnhMetaFile;
begin
  GetProcedureAddress(_CreateEnhMetaFile, gdi32, 'CreateEnhMetaFileA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateEnhMetaFile]
  end;
end;
{$ELSE}
function CreateEnhMetaFile; external gdi32 name 'CreateEnhMetaFileA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _DeleteEnhMetaFile: Pointer;

function DeleteEnhMetaFile;
begin
  GetProcedureAddress(_DeleteEnhMetaFile, gdi32, 'DeleteEnhMetaFile');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DeleteEnhMetaFile]
  end;
end;
{$ELSE}
function DeleteEnhMetaFile; external gdi32 name 'DeleteEnhMetaFile';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _EnumEnhMetaFile: Pointer;

function EnumEnhMetaFile;
begin
  GetProcedureAddress(_EnumEnhMetaFile, gdi32, 'EnumEnhMetaFile');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumEnhMetaFile]
  end;
end;
{$ELSE}
function EnumEnhMetaFile; external gdi32 name 'EnumEnhMetaFile';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetEnhMetaFileA: Pointer;

function GetEnhMetaFileA;
begin
  GetProcedureAddress(_GetEnhMetaFileA, gdi32, 'GetEnhMetaFileA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetEnhMetaFileA]
  end;
end;
{$ELSE}
function GetEnhMetaFileA; external gdi32 name 'GetEnhMetaFileA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetEnhMetaFileW: Pointer;

function GetEnhMetaFileW;
begin
  GetProcedureAddress(_GetEnhMetaFileW, gdi32, 'GetEnhMetaFileW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetEnhMetaFileW]
  end;
end;
{$ELSE}
function GetEnhMetaFileW; external gdi32 name 'GetEnhMetaFileW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetEnhMetaFile: Pointer;

function GetEnhMetaFile;
begin
  GetProcedureAddress(_GetEnhMetaFile, gdi32, 'GetEnhMetaFileW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetEnhMetaFile]
  end;
end;
{$ELSE}
function GetEnhMetaFile; external gdi32 name 'GetEnhMetaFileW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetEnhMetaFile: Pointer;

function GetEnhMetaFile;
begin
  GetProcedureAddress(_GetEnhMetaFile, gdi32, 'GetEnhMetaFileA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetEnhMetaFile]
  end;
end;
{$ELSE}
function GetEnhMetaFile; external gdi32 name 'GetEnhMetaFileA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetEnhMetaFileBits: Pointer;

function GetEnhMetaFileBits;
begin
  GetProcedureAddress(_GetEnhMetaFileBits, gdi32, 'GetEnhMetaFileBits');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetEnhMetaFileBits]
  end;
end;
{$ELSE}
function GetEnhMetaFileBits; external gdi32 name 'GetEnhMetaFileBits';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetEnhMetaFileDescriptionA: Pointer;

function GetEnhMetaFileDescriptionA;
begin
  GetProcedureAddress(_GetEnhMetaFileDescriptionA, gdi32, 'GetEnhMetaFileDescriptionA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetEnhMetaFileDescriptionA]
  end;
end;
{$ELSE}
function GetEnhMetaFileDescriptionA; external gdi32 name 'GetEnhMetaFileDescriptionA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetEnhMetaFileDescriptionW: Pointer;

function GetEnhMetaFileDescriptionW;
begin
  GetProcedureAddress(_GetEnhMetaFileDescriptionW, gdi32, 'GetEnhMetaFileDescriptionW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetEnhMetaFileDescriptionW]
  end;
end;
{$ELSE}
function GetEnhMetaFileDescriptionW; external gdi32 name 'GetEnhMetaFileDescriptionW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetEnhMetaFileDescription: Pointer;

function GetEnhMetaFileDescription;
begin
  GetProcedureAddress(_GetEnhMetaFileDescription, gdi32, 'GetEnhMetaFileDescriptionW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetEnhMetaFileDescription]
  end;
end;
{$ELSE}
function GetEnhMetaFileDescription; external gdi32 name 'GetEnhMetaFileDescriptionW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetEnhMetaFileDescription: Pointer;

function GetEnhMetaFileDescription;
begin
  GetProcedureAddress(_GetEnhMetaFileDescription, gdi32, 'GetEnhMetaFileDescriptionA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetEnhMetaFileDescription]
  end;
end;
{$ELSE}
function GetEnhMetaFileDescription; external gdi32 name 'GetEnhMetaFileDescriptionA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetEnhMetaFileHeader: Pointer;

function GetEnhMetaFileHeader;
begin
  GetProcedureAddress(_GetEnhMetaFileHeader, gdi32, 'GetEnhMetaFileHeader');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetEnhMetaFileHeader]
  end;
end;
{$ELSE}
function GetEnhMetaFileHeader; external gdi32 name 'GetEnhMetaFileHeader';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetEnhMetaFilePaletteEntries: Pointer;

function GetEnhMetaFilePaletteEntries;
begin
  GetProcedureAddress(_GetEnhMetaFilePaletteEntries, gdi32, 'GetEnhMetaFilePaletteEntries');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetEnhMetaFilePaletteEntries]
  end;
end;
{$ELSE}
function GetEnhMetaFilePaletteEntries; external gdi32 name 'GetEnhMetaFilePaletteEntries';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetEnhMetaFilePixelFormat: Pointer;

function GetEnhMetaFilePixelFormat;
begin
  GetProcedureAddress(_GetEnhMetaFilePixelFormat, gdi32, 'GetEnhMetaFilePixelFormat');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetEnhMetaFilePixelFormat]
  end;
end;
{$ELSE}
function GetEnhMetaFilePixelFormat; external gdi32 name 'GetEnhMetaFilePixelFormat';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetWinMetaFileBits: Pointer;

function GetWinMetaFileBits;
begin
  GetProcedureAddress(_GetWinMetaFileBits, gdi32, 'GetWinMetaFileBits');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetWinMetaFileBits]
  end;
end;
{$ELSE}
function GetWinMetaFileBits; external gdi32 name 'GetWinMetaFileBits';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _PlayEnhMetaFile: Pointer;

function PlayEnhMetaFile;
begin
  GetProcedureAddress(_PlayEnhMetaFile, gdi32, 'PlayEnhMetaFile');
  asm
    mov esp, ebp
    pop ebp
    jmp [_PlayEnhMetaFile]
  end;
end;
{$ELSE}
function PlayEnhMetaFile; external gdi32 name 'PlayEnhMetaFile';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _PlayEnhMetaFileRecord: Pointer;

function PlayEnhMetaFileRecord;
begin
  GetProcedureAddress(_PlayEnhMetaFileRecord, gdi32, 'PlayEnhMetaFileRecord');
  asm
    mov esp, ebp
    pop ebp
    jmp [_PlayEnhMetaFileRecord]
  end;
end;
{$ELSE}
function PlayEnhMetaFileRecord; external gdi32 name 'PlayEnhMetaFileRecord';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetEnhMetaFileBits: Pointer;

function SetEnhMetaFileBits;
begin
  GetProcedureAddress(_SetEnhMetaFileBits, gdi32, 'SetEnhMetaFileBits');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetEnhMetaFileBits]
  end;
end;
{$ELSE}
function SetEnhMetaFileBits; external gdi32 name 'SetEnhMetaFileBits';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetWinMetaFileBits: Pointer;

function SetWinMetaFileBits;
begin
  GetProcedureAddress(_SetWinMetaFileBits, gdi32, 'SetWinMetaFileBits');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetWinMetaFileBits]
  end;
end;
{$ELSE}
function SetWinMetaFileBits; external gdi32 name 'SetWinMetaFileBits';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GdiComment: Pointer;

function GdiComment;
begin
  GetProcedureAddress(_GdiComment, gdi32, 'GdiComment');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GdiComment]
  end;
end;
{$ELSE}
function GdiComment; external gdi32 name 'GdiComment';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetTextMetricsA: Pointer;

function GetTextMetricsA;
begin
  GetProcedureAddress(_GetTextMetricsA, gdi32, 'GetTextMetricsA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetTextMetricsA]
  end;
end;
{$ELSE}
function GetTextMetricsA; external gdi32 name 'GetTextMetricsA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetTextMetricsW: Pointer;

function GetTextMetricsW;
begin
  GetProcedureAddress(_GetTextMetricsW, gdi32, 'GetTextMetricsW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetTextMetricsW]
  end;
end;
{$ELSE}
function GetTextMetricsW; external gdi32 name 'GetTextMetricsW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetTextMetrics: Pointer;

function GetTextMetrics;
begin
  GetProcedureAddress(_GetTextMetrics, gdi32, 'GetTextMetricsW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetTextMetrics]
  end;
end;
{$ELSE}
function GetTextMetrics; external gdi32 name 'GetTextMetricsW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetTextMetrics: Pointer;

function GetTextMetrics;
begin
  GetProcedureAddress(_GetTextMetrics, gdi32, 'GetTextMetricsA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetTextMetrics]
  end;
end;
{$ELSE}
function GetTextMetrics; external gdi32 name 'GetTextMetricsA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _AngleArc: Pointer;

function AngleArc;
begin
  GetProcedureAddress(_AngleArc, gdi32, 'AngleArc');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AngleArc]
  end;
end;
{$ELSE}
function AngleArc; external gdi32 name 'AngleArc';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _PolyPolyline: Pointer;

function PolyPolyline;
begin
  GetProcedureAddress(_PolyPolyline, gdi32, 'PolyPolyline');
  asm
    mov esp, ebp
    pop ebp
    jmp [_PolyPolyline]
  end;
end;
{$ELSE}
function PolyPolyline; external gdi32 name 'PolyPolyline';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetWorldTransform: Pointer;

function GetWorldTransform;
begin
  GetProcedureAddress(_GetWorldTransform, gdi32, 'GetWorldTransform');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetWorldTransform]
  end;
end;
{$ELSE}
function GetWorldTransform; external gdi32 name 'GetWorldTransform';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetWorldTransform: Pointer;

function SetWorldTransform;
begin
  GetProcedureAddress(_SetWorldTransform, gdi32, 'SetWorldTransform');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetWorldTransform]
  end;
end;
{$ELSE}
function SetWorldTransform; external gdi32 name 'SetWorldTransform';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ModifyWorldTransform: Pointer;

function ModifyWorldTransform;
begin
  GetProcedureAddress(_ModifyWorldTransform, gdi32, 'ModifyWorldTransform');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ModifyWorldTransform]
  end;
end;
{$ELSE}
function ModifyWorldTransform; external gdi32 name 'ModifyWorldTransform';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CombineTransform: Pointer;

function CombineTransform;
begin
  GetProcedureAddress(_CombineTransform, gdi32, 'CombineTransform');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CombineTransform]
  end;
end;
{$ELSE}
function CombineTransform; external gdi32 name 'CombineTransform';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateDIBSection: Pointer;

function CreateDIBSection;
begin
  GetProcedureAddress(_CreateDIBSection, gdi32, 'CreateDIBSection');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateDIBSection]
  end;
end;
{$ELSE}
function CreateDIBSection; external gdi32 name 'CreateDIBSection';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetDIBColorTable: Pointer;

function GetDIBColorTable;
begin
  GetProcedureAddress(_GetDIBColorTable, gdi32, 'GetDIBColorTable');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetDIBColorTable]
  end;
end;
{$ELSE}
function GetDIBColorTable; external gdi32 name 'GetDIBColorTable';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetDIBColorTable: Pointer;

function SetDIBColorTable;
begin
  GetProcedureAddress(_SetDIBColorTable, gdi32, 'SetDIBColorTable');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetDIBColorTable]
  end;
end;
{$ELSE}
function SetDIBColorTable; external gdi32 name 'SetDIBColorTable';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetColorAdjustment: Pointer;

function SetColorAdjustment;
begin
  GetProcedureAddress(_SetColorAdjustment, gdi32, 'SetColorAdjustment');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetColorAdjustment]
  end;
end;
{$ELSE}
function SetColorAdjustment; external gdi32 name 'SetColorAdjustment';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetColorAdjustment: Pointer;

function GetColorAdjustment;
begin
  GetProcedureAddress(_GetColorAdjustment, gdi32, 'GetColorAdjustment');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetColorAdjustment]
  end;
end;
{$ELSE}
function GetColorAdjustment; external gdi32 name 'GetColorAdjustment';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateHalftonePalette: Pointer;

function CreateHalftonePalette;
begin
  GetProcedureAddress(_CreateHalftonePalette, gdi32, 'CreateHalftonePalette');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateHalftonePalette]
  end;
end;
{$ELSE}
function CreateHalftonePalette; external gdi32 name 'CreateHalftonePalette';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _StartDocA: Pointer;

function StartDocA;
begin
  GetProcedureAddress(_StartDocA, gdi32, 'StartDocA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_StartDocA]
  end;
end;
{$ELSE}
function StartDocA; external gdi32 name 'StartDocA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _StartDocW: Pointer;

function StartDocW;
begin
  GetProcedureAddress(_StartDocW, gdi32, 'StartDocW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_StartDocW]
  end;
end;
{$ELSE}
function StartDocW; external gdi32 name 'StartDocW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _StartDoc: Pointer;

function StartDoc;
begin
  GetProcedureAddress(_StartDoc, gdi32, 'StartDocW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_StartDoc]
  end;
end;
{$ELSE}
function StartDoc; external gdi32 name 'StartDocW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  __StartDoc: Pointer;

function StartDoc;
begin
  GetProcedureAddress(__StartDoc, gdi32, 'StartDocA');
  asm
    mov esp, ebp
    pop ebp
    jmp [__StartDoc]
  end;
end;
{$ELSE}
function StartDoc; external gdi32 name 'StartDocA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  __EndDoc: Pointer;

function EndDoc;
begin
  GetProcedureAddress(__EndDoc, gdi32, 'EndDoc');
  asm
    mov esp, ebp
    pop ebp
    jmp [__EndDoc]
  end;
end;
{$ELSE}
function EndDoc; external gdi32 name 'EndDoc';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _StartPage: Pointer;

function StartPage;
begin
  GetProcedureAddress(_StartPage, gdi32, 'StartPage');
  asm
    mov esp, ebp
    pop ebp
    jmp [_StartPage]
  end;
end;
{$ELSE}
function StartPage; external gdi32 name 'StartPage';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _EndPage: Pointer;

function EndPage;
begin
  GetProcedureAddress(_EndPage, gdi32, 'EndPage');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EndPage]
  end;
end;
{$ELSE}
function EndPage; external gdi32 name 'EndPage';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  __AbortDoc: Pointer;

function AbortDoc;
begin
  GetProcedureAddress(__AbortDoc, gdi32, 'AbortDoc');
  asm
    mov esp, ebp
    pop ebp
    jmp [__AbortDoc]
  end;
end;
{$ELSE}
function AbortDoc; external gdi32 name 'AbortDoc';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetAbortProc: Pointer;

function SetAbortProc;
begin
  GetProcedureAddress(_SetAbortProc, gdi32, 'SetAbortProc');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetAbortProc]
  end;
end;
{$ELSE}
function SetAbortProc; external gdi32 name 'SetAbortProc';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _AbortPath: Pointer;

function AbortPath;
begin
  GetProcedureAddress(_AbortPath, gdi32, 'AbortPath');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AbortPath]
  end;
end;
{$ELSE}
function AbortPath; external gdi32 name 'AbortPath';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ArcTo: Pointer;

function ArcTo;
begin
  GetProcedureAddress(_ArcTo, gdi32, 'ArcTo');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ArcTo]
  end;
end;
{$ELSE}
function ArcTo; external gdi32 name 'ArcTo';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _BeginPath: Pointer;

function BeginPath;
begin
  GetProcedureAddress(_BeginPath, gdi32, 'BeginPath');
  asm
    mov esp, ebp
    pop ebp
    jmp [_BeginPath]
  end;
end;
{$ELSE}
function BeginPath; external gdi32 name 'BeginPath';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CloseFigure: Pointer;

function CloseFigure;
begin
  GetProcedureAddress(_CloseFigure, gdi32, 'CloseFigure');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CloseFigure]
  end;
end;
{$ELSE}
function CloseFigure; external gdi32 name 'CloseFigure';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _EndPath: Pointer;

function EndPath;
begin
  GetProcedureAddress(_EndPath, gdi32, 'EndPath');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EndPath]
  end;
end;
{$ELSE}
function EndPath; external gdi32 name 'EndPath';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _FillPath: Pointer;

function FillPath;
begin
  GetProcedureAddress(_FillPath, gdi32, 'FillPath');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FillPath]
  end;
end;
{$ELSE}
function FillPath; external gdi32 name 'FillPath';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _FlattenPath: Pointer;

function FlattenPath;
begin
  GetProcedureAddress(_FlattenPath, gdi32, 'FlattenPath');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FlattenPath]
  end;
end;
{$ELSE}
function FlattenPath; external gdi32 name 'FlattenPath';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetPath: Pointer;

function GetPath;
begin
  GetProcedureAddress(_GetPath, gdi32, 'GetPath');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetPath]
  end;
end;
{$ELSE}
function GetPath; external gdi32 name 'GetPath';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _PathToRegion: Pointer;

function PathToRegion;
begin
  GetProcedureAddress(_PathToRegion, gdi32, 'PathToRegion');
  asm
    mov esp, ebp
    pop ebp
    jmp [_PathToRegion]
  end;
end;
{$ELSE}
function PathToRegion; external gdi32 name 'PathToRegion';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _PolyDraw: Pointer;

function PolyDraw;
begin
  GetProcedureAddress(_PolyDraw, gdi32, 'PolyDraw');
  asm
    mov esp, ebp
    pop ebp
    jmp [_PolyDraw]
  end;
end;
{$ELSE}
function PolyDraw; external gdi32 name 'PolyDraw';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SelectClipPath: Pointer;

function SelectClipPath;
begin
  GetProcedureAddress(_SelectClipPath, gdi32, 'SelectClipPath');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SelectClipPath]
  end;
end;
{$ELSE}
function SelectClipPath; external gdi32 name 'SelectClipPath';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetArcDirection: Pointer;

function SetArcDirection;
begin
  GetProcedureAddress(_SetArcDirection, gdi32, 'SetArcDirection');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetArcDirection]
  end;
end;
{$ELSE}
function SetArcDirection; external gdi32 name 'SetArcDirection';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetMiterLimit: Pointer;

function SetMiterLimit;
begin
  GetProcedureAddress(_SetMiterLimit, gdi32, 'SetMiterLimit');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetMiterLimit]
  end;
end;
{$ELSE}
function SetMiterLimit; external gdi32 name 'SetMiterLimit';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _StrokeAndFillPath: Pointer;

function StrokeAndFillPath;
begin
  GetProcedureAddress(_StrokeAndFillPath, gdi32, 'StrokeAndFillPath');
  asm
    mov esp, ebp
    pop ebp
    jmp [_StrokeAndFillPath]
  end;
end;
{$ELSE}
function StrokeAndFillPath; external gdi32 name 'StrokeAndFillPath';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _StrokePath: Pointer;

function StrokePath;
begin
  GetProcedureAddress(_StrokePath, gdi32, 'StrokePath');
  asm
    mov esp, ebp
    pop ebp
    jmp [_StrokePath]
  end;
end;
{$ELSE}
function StrokePath; external gdi32 name 'StrokePath';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WidenPath: Pointer;

function WidenPath;
begin
  GetProcedureAddress(_WidenPath, gdi32, 'WidenPath');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WidenPath]
  end;
end;
{$ELSE}
function WidenPath; external gdi32 name 'WidenPath';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ExtCreatePen: Pointer;

function ExtCreatePen;
begin
  GetProcedureAddress(_ExtCreatePen, gdi32, 'ExtCreatePen');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ExtCreatePen]
  end;
end;
{$ELSE}
function ExtCreatePen; external gdi32 name 'ExtCreatePen';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetMiterLimit: Pointer;

function GetMiterLimit;
begin
  GetProcedureAddress(_GetMiterLimit, gdi32, 'GetMiterLimit');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetMiterLimit]
  end;
end;
{$ELSE}
function GetMiterLimit; external gdi32 name 'GetMiterLimit';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetArcDirection: Pointer;

function GetArcDirection;
begin
  GetProcedureAddress(_GetArcDirection, gdi32, 'GetArcDirection');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetArcDirection]
  end;
end;
{$ELSE}
function GetArcDirection; external gdi32 name 'GetArcDirection';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetObjectA: Pointer;

function GetObjectA;
begin
  GetProcedureAddress(_GetObjectA, gdi32, 'GetObjectA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetObjectA]
  end;
end;
{$ELSE}
function GetObjectA; external gdi32 name 'GetObjectA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetObjectW: Pointer;

function GetObjectW;
begin
  GetProcedureAddress(_GetObjectW, gdi32, 'GetObjectW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetObjectW]
  end;
end;
{$ELSE}
function GetObjectW; external gdi32 name 'GetObjectW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetObject: Pointer;

function GetObject;
begin
  GetProcedureAddress(_GetObject, gdi32, 'GetObjectW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetObject]
  end;
end;
{$ELSE}
function GetObject; external gdi32 name 'GetObjectW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetObject: Pointer;

function GetObject;
begin
  GetProcedureAddress(_GetObject, gdi32, 'GetObjectA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetObject]
  end;
end;
{$ELSE}
function GetObject; external gdi32 name 'GetObjectA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _MoveToEx: Pointer;

function MoveToEx;
begin
  GetProcedureAddress(_MoveToEx, gdi32, 'MoveToEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MoveToEx]
  end;
end;
{$ELSE}
function MoveToEx; external gdi32 name 'MoveToEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _TextOutA: Pointer;

function TextOutA;
begin
  GetProcedureAddress(_TextOutA, gdi32, 'TextOutA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_TextOutA]
  end;
end;
{$ELSE}
function TextOutA; external gdi32 name 'TextOutA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _TextOutW: Pointer;

function TextOutW;
begin
  GetProcedureAddress(_TextOutW, gdi32, 'TextOutW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_TextOutW]
  end;
end;
{$ELSE}
function TextOutW; external gdi32 name 'TextOutW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _TextOut: Pointer;

function TextOut;
begin
  GetProcedureAddress(_TextOut, gdi32, 'TextOutW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_TextOut]
  end;
end;
{$ELSE}
function TextOut; external gdi32 name 'TextOutW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _TextOut: Pointer;

function TextOut;
begin
  GetProcedureAddress(_TextOut, gdi32, 'TextOutA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_TextOut]
  end;
end;
{$ELSE}
function TextOut; external gdi32 name 'TextOutA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _ExtTextOutA: Pointer;

function ExtTextOutA;
begin
  GetProcedureAddress(_ExtTextOutA, gdi32, 'ExtTextOutA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ExtTextOutA]
  end;
end;
{$ELSE}
function ExtTextOutA; external gdi32 name 'ExtTextOutA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ExtTextOutW: Pointer;

function ExtTextOutW;
begin
  GetProcedureAddress(_ExtTextOutW, gdi32, 'ExtTextOutW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ExtTextOutW]
  end;
end;
{$ELSE}
function ExtTextOutW; external gdi32 name 'ExtTextOutW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _ExtTextOut: Pointer;

function ExtTextOut;
begin
  GetProcedureAddress(_ExtTextOut, gdi32, 'ExtTextOutW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ExtTextOut]
  end;
end;
{$ELSE}
function ExtTextOut; external gdi32 name 'ExtTextOutW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _ExtTextOut: Pointer;

function ExtTextOut;
begin
  GetProcedureAddress(_ExtTextOut, gdi32, 'ExtTextOutA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ExtTextOut]
  end;
end;
{$ELSE}
function ExtTextOut; external gdi32 name 'ExtTextOutA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _PolyTextOutA: Pointer;

function PolyTextOutA;
begin
  GetProcedureAddress(_PolyTextOutA, gdi32, 'PolyTextOutA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_PolyTextOutA]
  end;
end;
{$ELSE}
function PolyTextOutA; external gdi32 name 'PolyTextOutA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _PolyTextOutW: Pointer;

function PolyTextOutW;
begin
  GetProcedureAddress(_PolyTextOutW, gdi32, 'PolyTextOutW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_PolyTextOutW]
  end;
end;
{$ELSE}
function PolyTextOutW; external gdi32 name 'PolyTextOutW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _PolyTextOut: Pointer;

function PolyTextOut;
begin
  GetProcedureAddress(_PolyTextOut, gdi32, 'PolyTextOutW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_PolyTextOut]
  end;
end;
{$ELSE}
function PolyTextOut; external gdi32 name 'PolyTextOutW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _PolyTextOut: Pointer;

function PolyTextOut;
begin
  GetProcedureAddress(_PolyTextOut, gdi32, 'PolyTextOutA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_PolyTextOut]
  end;
end;
{$ELSE}
function PolyTextOut; external gdi32 name 'PolyTextOutA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _CreatePolygonRgn: Pointer;

function CreatePolygonRgn;
begin
  GetProcedureAddress(_CreatePolygonRgn, gdi32, 'CreatePolygonRgn');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreatePolygonRgn]
  end;
end;
{$ELSE}
function CreatePolygonRgn; external gdi32 name 'CreatePolygonRgn';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DPtoLP: Pointer;

function DPtoLP;
begin
  GetProcedureAddress(_DPtoLP, gdi32, 'DPtoLP');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DPtoLP]
  end;
end;
{$ELSE}
function DPtoLP; external gdi32 name 'DPtoLP';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _LPtoDP: Pointer;

function LPtoDP;
begin
  GetProcedureAddress(_LPtoDP, gdi32, 'LPtoDP');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LPtoDP]
  end;
end;
{$ELSE}
function LPtoDP; external gdi32 name 'LPtoDP';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _Polygon: Pointer;

function Polygon;
begin
  GetProcedureAddress(_Polygon, gdi32, 'Polygon');
  asm
    mov esp, ebp
    pop ebp
    jmp [_Polygon]
  end;
end;
{$ELSE}
function Polygon; external gdi32 name 'Polygon';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _Polyline: Pointer;

function Polyline;
begin
  GetProcedureAddress(_Polyline, gdi32, 'Polyline');
  asm
    mov esp, ebp
    pop ebp
    jmp [_Polyline]
  end;
end;
{$ELSE}
function Polyline; external gdi32 name 'Polyline';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _PolyBezier: Pointer;

function PolyBezier;
begin
  GetProcedureAddress(_PolyBezier, gdi32, 'PolyBezier');
  asm
    mov esp, ebp
    pop ebp
    jmp [_PolyBezier]
  end;
end;
{$ELSE}
function PolyBezier; external gdi32 name 'PolyBezier';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _PolyBezierTo: Pointer;

function PolyBezierTo;
begin
  GetProcedureAddress(_PolyBezierTo, gdi32, 'PolyBezierTo');
  asm
    mov esp, ebp
    pop ebp
    jmp [_PolyBezierTo]
  end;
end;
{$ELSE}
function PolyBezierTo; external gdi32 name 'PolyBezierTo';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _PolylineTo: Pointer;

function PolylineTo;
begin
  GetProcedureAddress(_PolylineTo, gdi32, 'PolylineTo');
  asm
    mov esp, ebp
    pop ebp
    jmp [_PolylineTo]
  end;
end;
{$ELSE}
function PolylineTo; external gdi32 name 'PolylineTo';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetViewportExtEx: Pointer;

function SetViewportExtEx;
begin
  GetProcedureAddress(_SetViewportExtEx, gdi32, 'SetViewportExtEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetViewportExtEx]
  end;
end;
{$ELSE}
function SetViewportExtEx; external gdi32 name 'SetViewportExtEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetViewportOrgEx: Pointer;

function SetViewportOrgEx;
begin
  GetProcedureAddress(_SetViewportOrgEx, gdi32, 'SetViewportOrgEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetViewportOrgEx]
  end;
end;
{$ELSE}
function SetViewportOrgEx; external gdi32 name 'SetViewportOrgEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetWindowExtEx: Pointer;

function SetWindowExtEx;
begin
  GetProcedureAddress(_SetWindowExtEx, gdi32, 'SetWindowExtEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetWindowExtEx]
  end;
end;
{$ELSE}
function SetWindowExtEx; external gdi32 name 'SetWindowExtEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetWindowOrgEx: Pointer;

function SetWindowOrgEx;
begin
  GetProcedureAddress(_SetWindowOrgEx, gdi32, 'SetWindowOrgEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetWindowOrgEx]
  end;
end;
{$ELSE}
function SetWindowOrgEx; external gdi32 name 'SetWindowOrgEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _OffsetViewportOrgEx: Pointer;

function OffsetViewportOrgEx;
begin
  GetProcedureAddress(_OffsetViewportOrgEx, gdi32, 'OffsetViewportOrgEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OffsetViewportOrgEx]
  end;
end;
{$ELSE}
function OffsetViewportOrgEx; external gdi32 name 'OffsetViewportOrgEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _OffsetWindowOrgEx: Pointer;

function OffsetWindowOrgEx;
begin
  GetProcedureAddress(_OffsetWindowOrgEx, gdi32, 'OffsetWindowOrgEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OffsetWindowOrgEx]
  end;
end;
{$ELSE}
function OffsetWindowOrgEx; external gdi32 name 'OffsetWindowOrgEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ScaleViewportExtEx: Pointer;

function ScaleViewportExtEx;
begin
  GetProcedureAddress(_ScaleViewportExtEx, gdi32, 'ScaleViewportExtEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ScaleViewportExtEx]
  end;
end;
{$ELSE}
function ScaleViewportExtEx; external gdi32 name 'ScaleViewportExtEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ScaleWindowExtEx: Pointer;

function ScaleWindowExtEx;
begin
  GetProcedureAddress(_ScaleWindowExtEx, gdi32, 'ScaleWindowExtEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ScaleWindowExtEx]
  end;
end;
{$ELSE}
function ScaleWindowExtEx; external gdi32 name 'ScaleWindowExtEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetBitmapDimensionEx: Pointer;

function SetBitmapDimensionEx;
begin
  GetProcedureAddress(_SetBitmapDimensionEx, gdi32, 'SetBitmapDimensionEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetBitmapDimensionEx]
  end;
end;
{$ELSE}
function SetBitmapDimensionEx; external gdi32 name 'SetBitmapDimensionEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetBrushOrgEx: Pointer;

function SetBrushOrgEx;
begin
  GetProcedureAddress(_SetBrushOrgEx, gdi32, 'SetBrushOrgEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetBrushOrgEx]
  end;
end;
{$ELSE}
function SetBrushOrgEx; external gdi32 name 'SetBrushOrgEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetTextFaceA: Pointer;

function GetTextFaceA;
begin
  GetProcedureAddress(_GetTextFaceA, gdi32, 'GetTextFaceA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetTextFaceA]
  end;
end;
{$ELSE}
function GetTextFaceA; external gdi32 name 'GetTextFaceA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetTextFaceW: Pointer;

function GetTextFaceW;
begin
  GetProcedureAddress(_GetTextFaceW, gdi32, 'GetTextFaceW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetTextFaceW]
  end;
end;
{$ELSE}
function GetTextFaceW; external gdi32 name 'GetTextFaceW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetTextFace: Pointer;

function GetTextFace;
begin
  GetProcedureAddress(_GetTextFace, gdi32, 'GetTextFaceW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetTextFace]
  end;
end;
{$ELSE}
function GetTextFace; external gdi32 name 'GetTextFaceW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetTextFace: Pointer;

function GetTextFace;
begin
  GetProcedureAddress(_GetTextFace, gdi32, 'GetTextFaceA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetTextFace]
  end;
end;
{$ELSE}
function GetTextFace; external gdi32 name 'GetTextFaceA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetKerningPairsA: Pointer;

function GetKerningPairsA;
begin
  GetProcedureAddress(_GetKerningPairsA, gdi32, 'GetKerningPairsA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetKerningPairsA]
  end;
end;
{$ELSE}
function GetKerningPairsA; external gdi32 name 'GetKerningPairsA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetKerningPairsW: Pointer;

function GetKerningPairsW;
begin
  GetProcedureAddress(_GetKerningPairsW, gdi32, 'GetKerningPairsW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetKerningPairsW]
  end;
end;
{$ELSE}
function GetKerningPairsW; external gdi32 name 'GetKerningPairsW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetKerningPairs: Pointer;

function GetKerningPairs;
begin
  GetProcedureAddress(_GetKerningPairs, gdi32, 'GetKerningPairsW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetKerningPairs]
  end;
end;
{$ELSE}
function GetKerningPairs; external gdi32 name 'GetKerningPairsW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetKerningPairs: Pointer;

function GetKerningPairs;
begin
  GetProcedureAddress(_GetKerningPairs, gdi32, 'GetKerningPairsA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetKerningPairs]
  end;
end;
{$ELSE}
function GetKerningPairs; external gdi32 name 'GetKerningPairsA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetDCOrgEx: Pointer;

function GetDCOrgEx;
begin
  GetProcedureAddress(_GetDCOrgEx, gdi32, 'GetDCOrgEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetDCOrgEx]
  end;
end;
{$ELSE}
function GetDCOrgEx; external gdi32 name 'GetDCOrgEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _FixBrushOrgEx: Pointer;

function FixBrushOrgEx;
begin
  GetProcedureAddress(_FixBrushOrgEx, gdi32, 'FixBrushOrgEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FixBrushOrgEx]
  end;
end;
{$ELSE}
function FixBrushOrgEx; external gdi32 name 'FixBrushOrgEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _UnrealizeObject: Pointer;

function UnrealizeObject;
begin
  GetProcedureAddress(_UnrealizeObject, gdi32, 'UnrealizeObject');
  asm
    mov esp, ebp
    pop ebp
    jmp [_UnrealizeObject]
  end;
end;
{$ELSE}
function UnrealizeObject; external gdi32 name 'UnrealizeObject';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GdiFlush: Pointer;

function GdiFlush;
begin
  GetProcedureAddress(_GdiFlush, gdi32, 'GdiFlush');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GdiFlush]
  end;
end;
{$ELSE}
function GdiFlush; external gdi32 name 'GdiFlush';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GdiSetBatchLimit: Pointer;

function GdiSetBatchLimit;
begin
  GetProcedureAddress(_GdiSetBatchLimit, gdi32, 'GdiSetBatchLimit');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GdiSetBatchLimit]
  end;
end;
{$ELSE}
function GdiSetBatchLimit; external gdi32 name 'GdiSetBatchLimit';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GdiGetBatchLimit: Pointer;

function GdiGetBatchLimit;
begin
  GetProcedureAddress(_GdiGetBatchLimit, gdi32, 'GdiGetBatchLimit');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GdiGetBatchLimit]
  end;
end;
{$ELSE}
function GdiGetBatchLimit; external gdi32 name 'GdiGetBatchLimit';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetICMMode: Pointer;

function SetICMMode;
begin
  GetProcedureAddress(_SetICMMode, gdi32, 'SetICMMode');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetICMMode]
  end;
end;
{$ELSE}
function SetICMMode; external gdi32 name 'SetICMMode';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CheckColorsInGamut: Pointer;

function CheckColorsInGamut;
begin
  GetProcedureAddress(_CheckColorsInGamut, gdi32, 'CheckColorsInGamut');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CheckColorsInGamut]
  end;
end;
{$ELSE}
function CheckColorsInGamut; external gdi32 name 'CheckColorsInGamut';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetColorSpace: Pointer;

function GetColorSpace;
begin
  GetProcedureAddress(_GetColorSpace, gdi32, 'GetColorSpace');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetColorSpace]
  end;
end;
{$ELSE}
function GetColorSpace; external gdi32 name 'GetColorSpace';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetLogColorSpaceA: Pointer;

function GetLogColorSpaceA;
begin
  GetProcedureAddress(_GetLogColorSpaceA, gdi32, 'GetLogColorSpaceA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetLogColorSpaceA]
  end;
end;
{$ELSE}
function GetLogColorSpaceA; external gdi32 name 'GetLogColorSpaceA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetLogColorSpaceW: Pointer;

function GetLogColorSpaceW;
begin
  GetProcedureAddress(_GetLogColorSpaceW, gdi32, 'GetLogColorSpaceW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetLogColorSpaceW]
  end;
end;
{$ELSE}
function GetLogColorSpaceW; external gdi32 name 'GetLogColorSpaceW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetLogColorSpace: Pointer;

function GetLogColorSpace;
begin
  GetProcedureAddress(_GetLogColorSpace, gdi32, 'GetLogColorSpaceW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetLogColorSpace]
  end;
end;
{$ELSE}
function GetLogColorSpace; external gdi32 name 'GetLogColorSpaceW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetLogColorSpace: Pointer;

function GetLogColorSpace;
begin
  GetProcedureAddress(_GetLogColorSpace, gdi32, 'GetLogColorSpaceA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetLogColorSpace]
  end;
end;
{$ELSE}
function GetLogColorSpace; external gdi32 name 'GetLogColorSpaceA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _CreateColorSpaceA: Pointer;

function CreateColorSpaceA;
begin
  GetProcedureAddress(_CreateColorSpaceA, gdi32, 'CreateColorSpaceA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateColorSpaceA]
  end;
end;
{$ELSE}
function CreateColorSpaceA; external gdi32 name 'CreateColorSpaceA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateColorSpaceW: Pointer;

function CreateColorSpaceW;
begin
  GetProcedureAddress(_CreateColorSpaceW, gdi32, 'CreateColorSpaceW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateColorSpaceW]
  end;
end;
{$ELSE}
function CreateColorSpaceW; external gdi32 name 'CreateColorSpaceW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _CreateColorSpace: Pointer;

function CreateColorSpace;
begin
  GetProcedureAddress(_CreateColorSpace, gdi32, 'CreateColorSpaceW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateColorSpace]
  end;
end;
{$ELSE}
function CreateColorSpace; external gdi32 name 'CreateColorSpaceW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _CreateColorSpace: Pointer;

function CreateColorSpace;
begin
  GetProcedureAddress(_CreateColorSpace, gdi32, 'CreateColorSpaceA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateColorSpace]
  end;
end;
{$ELSE}
function CreateColorSpace; external gdi32 name 'CreateColorSpaceA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _SetColorSpace: Pointer;

function SetColorSpace;
begin
  GetProcedureAddress(_SetColorSpace, gdi32, 'SetColorSpace');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetColorSpace]
  end;
end;
{$ELSE}
function SetColorSpace; external gdi32 name 'SetColorSpace';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DeleteColorSpace: Pointer;

function DeleteColorSpace;
begin
  GetProcedureAddress(_DeleteColorSpace, gdi32, 'DeleteColorSpace');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DeleteColorSpace]
  end;
end;
{$ELSE}
function DeleteColorSpace; external gdi32 name 'DeleteColorSpace';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetICMProfileA: Pointer;

function GetICMProfileA;
begin
  GetProcedureAddress(_GetICMProfileA, gdi32, 'GetICMProfileA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetICMProfileA]
  end;
end;
{$ELSE}
function GetICMProfileA; external gdi32 name 'GetICMProfileA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetICMProfileW: Pointer;

function GetICMProfileW;
begin
  GetProcedureAddress(_GetICMProfileW, gdi32, 'GetICMProfileW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetICMProfileW]
  end;
end;
{$ELSE}
function GetICMProfileW; external gdi32 name 'GetICMProfileW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetICMProfile: Pointer;

function GetICMProfile;
begin
  GetProcedureAddress(_GetICMProfile, gdi32, 'GetICMProfileW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetICMProfile]
  end;
end;
{$ELSE}
function GetICMProfile; external gdi32 name 'GetICMProfileW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetICMProfile: Pointer;

function GetICMProfile;
begin
  GetProcedureAddress(_GetICMProfile, gdi32, 'GetICMProfileA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetICMProfile]
  end;
end;
{$ELSE}
function GetICMProfile; external gdi32 name 'GetICMProfileA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _SetICMProfileA: Pointer;

function SetICMProfileA;
begin
  GetProcedureAddress(_SetICMProfileA, gdi32, 'SetICMProfileA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetICMProfileA]
  end;
end;
{$ELSE}
function SetICMProfileA; external gdi32 name 'SetICMProfileA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetICMProfileW: Pointer;

function SetICMProfileW;
begin
  GetProcedureAddress(_SetICMProfileW, gdi32, 'SetICMProfileW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetICMProfileW]
  end;
end;
{$ELSE}
function SetICMProfileW; external gdi32 name 'SetICMProfileW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _SetICMProfile: Pointer;

function SetICMProfile;
begin
  GetProcedureAddress(_SetICMProfile, gdi32, 'SetICMProfileW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetICMProfile]
  end;
end;
{$ELSE}
function SetICMProfile; external gdi32 name 'SetICMProfileW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _SetICMProfile: Pointer;

function SetICMProfile;
begin
  GetProcedureAddress(_SetICMProfile, gdi32, 'SetICMProfileA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetICMProfile]
  end;
end;
{$ELSE}
function SetICMProfile; external gdi32 name 'SetICMProfileA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetDeviceGammaRamp: Pointer;

function GetDeviceGammaRamp;
begin
  GetProcedureAddress(_GetDeviceGammaRamp, gdi32, 'GetDeviceGammaRamp');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetDeviceGammaRamp]
  end;
end;
{$ELSE}
function GetDeviceGammaRamp; external gdi32 name 'GetDeviceGammaRamp';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetDeviceGammaRamp: Pointer;

function SetDeviceGammaRamp;
begin
  GetProcedureAddress(_SetDeviceGammaRamp, gdi32, 'SetDeviceGammaRamp');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetDeviceGammaRamp]
  end;
end;
{$ELSE}
function SetDeviceGammaRamp; external gdi32 name 'SetDeviceGammaRamp';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ColorMatchToTarget: Pointer;

function ColorMatchToTarget;
begin
  GetProcedureAddress(_ColorMatchToTarget, gdi32, 'ColorMatchToTarget');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ColorMatchToTarget]
  end;
end;
{$ELSE}
function ColorMatchToTarget; external gdi32 name 'ColorMatchToTarget';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _EnumICMProfilesA: Pointer;

function EnumICMProfilesA;
begin
  GetProcedureAddress(_EnumICMProfilesA, gdi32, 'EnumICMProfilesA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumICMProfilesA]
  end;
end;
{$ELSE}
function EnumICMProfilesA; external gdi32 name 'EnumICMProfilesA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _EnumICMProfilesW: Pointer;

function EnumICMProfilesW;
begin
  GetProcedureAddress(_EnumICMProfilesW, gdi32, 'EnumICMProfilesW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumICMProfilesW]
  end;
end;
{$ELSE}
function EnumICMProfilesW; external gdi32 name 'EnumICMProfilesW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _EnumICMProfiles: Pointer;

function EnumICMProfiles;
begin
  GetProcedureAddress(_EnumICMProfiles, gdi32, 'EnumICMProfilesW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumICMProfiles]
  end;
end;
{$ELSE}
function EnumICMProfiles; external gdi32 name 'EnumICMProfilesW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _EnumICMProfiles: Pointer;

function EnumICMProfiles;
begin
  GetProcedureAddress(_EnumICMProfiles, gdi32, 'EnumICMProfilesA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumICMProfiles]
  end;
end;
{$ELSE}
function EnumICMProfiles; external gdi32 name 'EnumICMProfilesA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _UpdateICMRegKeyA: Pointer;

function UpdateICMRegKeyA;
begin
  GetProcedureAddress(_UpdateICMRegKeyA, gdi32, 'UpdateICMRegKeyA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_UpdateICMRegKeyA]
  end;
end;
{$ELSE}
function UpdateICMRegKeyA; external gdi32 name 'UpdateICMRegKeyA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _UpdateICMRegKeyW: Pointer;

function UpdateICMRegKeyW;
begin
  GetProcedureAddress(_UpdateICMRegKeyW, gdi32, 'UpdateICMRegKeyW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_UpdateICMRegKeyW]
  end;
end;
{$ELSE}
function UpdateICMRegKeyW; external gdi32 name 'UpdateICMRegKeyW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _UpdateICMRegKey: Pointer;

function UpdateICMRegKey;
begin
  GetProcedureAddress(_UpdateICMRegKey, gdi32, 'UpdateICMRegKeyW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_UpdateICMRegKey]
  end;
end;
{$ELSE}
function UpdateICMRegKey; external gdi32 name 'UpdateICMRegKeyW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _UpdateICMRegKey: Pointer;

function UpdateICMRegKey;
begin
  GetProcedureAddress(_UpdateICMRegKey, gdi32, 'UpdateICMRegKeyA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_UpdateICMRegKey]
  end;
end;
{$ELSE}
function UpdateICMRegKey; external gdi32 name 'UpdateICMRegKeyA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _ColorCorrectPalette: Pointer;

function ColorCorrectPalette;
begin
  GetProcedureAddress(_ColorCorrectPalette, gdi32, 'ColorCorrectPalette');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ColorCorrectPalette]
  end;
end;
{$ELSE}
function ColorCorrectPalette; external gdi32 name 'ColorCorrectPalette';
{$ENDIF DYNAMIC_LINK}


{$IFDEF DYNAMIC_LINK}
var
  _wglCopyContext: Pointer;

function wglCopyContext;
begin
  GetProcedureAddress(_wglCopyContext, opengl32, 'wglCopyContext');
  asm
    mov esp, ebp
    pop ebp
    jmp [_wglCopyContext]
  end;
end;
{$ELSE}
function wglCopyContext; external opengl32 name 'wglCopyContext';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _wglCreateContext: Pointer;

function wglCreateContext;
begin
  GetProcedureAddress(_wglCreateContext, opengl32, 'wglCreateContext');
  asm
    mov esp, ebp
    pop ebp
    jmp [_wglCreateContext]
  end;
end;
{$ELSE}
function wglCreateContext; external opengl32 name 'wglCreateContext';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _wglCreateLayerContext: Pointer;

function wglCreateLayerContext;
begin
  GetProcedureAddress(_wglCreateLayerContext, opengl32, 'wglCreateLayerContext');
  asm
    mov esp, ebp
    pop ebp
    jmp [_wglCreateLayerContext]
  end;
end;
{$ELSE}
function wglCreateLayerContext; external opengl32 name 'wglCreateLayerContext';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _wglDeleteContext: Pointer;

function wglDeleteContext;
begin
  GetProcedureAddress(_wglDeleteContext, opengl32, 'wglDeleteContext');
  asm
    mov esp, ebp
    pop ebp
    jmp [_wglDeleteContext]
  end;
end;
{$ELSE}
function wglDeleteContext; external opengl32 name 'wglDeleteContext';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _wglGetCurrentContext: Pointer;

function wglGetCurrentContext;
begin
  GetProcedureAddress(_wglGetCurrentContext, opengl32, 'wglGetCurrentContext');
  asm
    mov esp, ebp
    pop ebp
    jmp [_wglGetCurrentContext]
  end;
end;
{$ELSE}
function wglGetCurrentContext; external opengl32 name 'wglGetCurrentContext';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _wglGetCurrentDC: Pointer;

function wglGetCurrentDC;
begin
  GetProcedureAddress(_wglGetCurrentDC, opengl32, 'wglGetCurrentDC');
  asm
    mov esp, ebp
    pop ebp
    jmp [_wglGetCurrentDC]
  end;
end;
{$ELSE}
function wglGetCurrentDC; external opengl32 name 'wglGetCurrentDC';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _wglGetProcAddress: Pointer;

function wglGetProcAddress;
begin
  GetProcedureAddress(_wglGetProcAddress, opengl32, 'wglGetProcAddress');
  asm
    mov esp, ebp
    pop ebp
    jmp [_wglGetProcAddress]
  end;
end;
{$ELSE}
function wglGetProcAddress; external opengl32 name 'wglGetProcAddress';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _wglMakeCurrent: Pointer;

function wglMakeCurrent;
begin
  GetProcedureAddress(_wglMakeCurrent, opengl32, 'wglMakeCurrent');
  asm
    mov esp, ebp
    pop ebp
    jmp [_wglMakeCurrent]
  end;
end;
{$ELSE}
function wglMakeCurrent; external opengl32 name 'wglMakeCurrent';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _wglShareLists: Pointer;

function wglShareLists;
begin
  GetProcedureAddress(_wglShareLists, opengl32, 'wglShareLists');
  asm
    mov esp, ebp
    pop ebp
    jmp [_wglShareLists]
  end;
end;
{$ELSE}
function wglShareLists; external opengl32 name 'wglShareLists';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _wglUseFontBitmapsA: Pointer;

function wglUseFontBitmapsA;
begin
  GetProcedureAddress(_wglUseFontBitmapsA, opengl32, 'wglUseFontBitmapsA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_wglUseFontBitmapsA]
  end;
end;
{$ELSE}
function wglUseFontBitmapsA; external opengl32 name 'wglUseFontBitmapsA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _wglUseFontBitmapsW: Pointer;

function wglUseFontBitmapsW;
begin
  GetProcedureAddress(_wglUseFontBitmapsW, opengl32, 'wglUseFontBitmapsW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_wglUseFontBitmapsW]
  end;
end;
{$ELSE}
function wglUseFontBitmapsW; external opengl32 name 'wglUseFontBitmapsW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _wglUseFontBitmaps: Pointer;

function wglUseFontBitmaps;
begin
  GetProcedureAddress(_wglUseFontBitmaps, opengl32, 'wglUseFontBitmapsW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_wglUseFontBitmaps]
  end;
end;
{$ELSE}
function wglUseFontBitmaps; external opengl32 name 'wglUseFontBitmapsW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _wglUseFontBitmaps: Pointer;

function wglUseFontBitmaps;
begin
  GetProcedureAddress(_wglUseFontBitmaps, opengl32, 'wglUseFontBitmapsA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_wglUseFontBitmaps]
  end;
end;
{$ELSE}
function wglUseFontBitmaps; external opengl32 name 'wglUseFontBitmapsA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _SwapBuffers: Pointer;

function SwapBuffers;
begin
  GetProcedureAddress(_SwapBuffers, opengl32, 'SwapBuffers');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SwapBuffers]
  end;
end;
{$ELSE}
function SwapBuffers; external opengl32 name 'SwapBuffers';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _wglUseFontOutlinesA: Pointer;

function wglUseFontOutlinesA;
begin
  GetProcedureAddress(_wglUseFontOutlinesA, opengl32, 'wglUseFontOutlinesA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_wglUseFontOutlinesA]
  end;
end;
{$ELSE}
function wglUseFontOutlinesA; external opengl32 name 'wglUseFontOutlinesA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _wglUseFontOutlinesW: Pointer;

function wglUseFontOutlinesW;
begin
  GetProcedureAddress(_wglUseFontOutlinesW, opengl32, 'wglUseFontOutlinesW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_wglUseFontOutlinesW]
  end;
end;
{$ELSE}
function wglUseFontOutlinesW; external opengl32 name 'wglUseFontOutlinesW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _wglUseFontOutlines: Pointer;

function wglUseFontOutlines;
begin
  GetProcedureAddress(_wglUseFontOutlines, opengl32, 'wglUseFontOutlinesW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_wglUseFontOutlines]
  end;
end;
{$ELSE}
function wglUseFontOutlines; external opengl32 name 'wglUseFontOutlinesW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _wglUseFontOutlines: Pointer;

function wglUseFontOutlines;
begin
  GetProcedureAddress(_wglUseFontOutlines, opengl32, 'wglUseFontOutlinesA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_wglUseFontOutlines]
  end;
end;
{$ELSE}
function wglUseFontOutlines; external opengl32 name 'wglUseFontOutlinesA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _wglDescribeLayerPlane: Pointer;

function wglDescribeLayerPlane;
begin
  GetProcedureAddress(_wglDescribeLayerPlane, opengl32, 'wglDescribeLayerPlane');
  asm
    mov esp, ebp
    pop ebp
    jmp [_wglDescribeLayerPlane]
  end;
end;
{$ELSE}
function wglDescribeLayerPlane; external opengl32 name 'wglDescribeLayerPlane';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _wglSetLayerPaletteEntries: Pointer;

function wglSetLayerPaletteEntries;
begin
  GetProcedureAddress(_wglSetLayerPaletteEntries, opengl32, 'wglSetLayerPaletteEntries');
  asm
    mov esp, ebp
    pop ebp
    jmp [_wglSetLayerPaletteEntries]
  end;
end;
{$ELSE}
function wglSetLayerPaletteEntries; external opengl32 name 'wglSetLayerPaletteEntries';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _wglGetLayerPaletteEntries: Pointer;

function wglGetLayerPaletteEntries;
begin
  GetProcedureAddress(_wglGetLayerPaletteEntries, opengl32, 'wglGetLayerPaletteEntries');
  asm
    mov esp, ebp
    pop ebp
    jmp [_wglGetLayerPaletteEntries]
  end;
end;
{$ELSE}
function wglGetLayerPaletteEntries; external opengl32 name 'wglGetLayerPaletteEntries';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _wglRealizeLayerPalette: Pointer;

function wglRealizeLayerPalette;
begin
  GetProcedureAddress(_wglRealizeLayerPalette, opengl32, 'wglRealizeLayerPalette');
  asm
    mov esp, ebp
    pop ebp
    jmp [_wglRealizeLayerPalette]
  end;
end;
{$ELSE}
function wglRealizeLayerPalette; external opengl32 name 'wglRealizeLayerPalette';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _wglSwapLayerBuffers: Pointer;

function wglSwapLayerBuffers;
begin
  GetProcedureAddress(_wglSwapLayerBuffers, opengl32, 'wglSwapLayerBuffers');
  asm
    mov esp, ebp
    pop ebp
    jmp [_wglSwapLayerBuffers]
  end;
end;
{$ELSE}
function wglSwapLayerBuffers; external opengl32 name 'wglSwapLayerBuffers';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _wglSwapMultipleBuffers: Pointer;

function wglSwapMultipleBuffers;
begin
  GetProcedureAddress(_wglSwapMultipleBuffers, opengl32, 'wglSwapMultipleBuffers');
  asm
    mov esp, ebp
    pop ebp
    jmp [_wglSwapMultipleBuffers]
  end;
end;
{$ELSE}
function wglSwapMultipleBuffers; external opengl32 name 'wglSwapMultipleBuffers';
{$ENDIF DYNAMIC_LINK}

end.
