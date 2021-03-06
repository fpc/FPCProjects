unit sdl_ttf;
{******************************************************************************}
{
  $Id$
  
}
{                                                                              }
{       Borland Delphi SDL - Simple DirectMedia Layer                          }
{       Conversion of the Simple DirectMedia Layer Headers                     }
{                                                                              }
{ Portions created by Sam Lantinga <slouken@devolution.com> are                }
{ Copyright (C) 1997, 1998, 1999, 2000, 2001  Sam Lantinga                     }
{ 5635-34 Springhouse Dr.                                                      }
{ Pleasanton, CA 94588 (USA)                                                   }
{                                                                              }
{ All Rights Reserved.                                                         }
{                                                                              }
{ The original files are : SDL_ttf.h                                           }
{                                                                              }
{ The initial developer of this Pascal code was :                              }
{ Dominqiue Louis <Dominique@SavageSoftware.com.au>                            }
{                                                                              }
{ Portions created by Dominqiue Louis are                                      }
{ Copyright (C) 2000 - 2001 Dominqiue Louis.                                   }
{                                                                              }
{                                                                              }
{ Contributor(s)                                                               }
{ --------------                                                               }
{ Tom Jones <tigertomjones@gmx.de>  His Project inspired this conversion       }
{                                                                              }
{ Obtained through:                                                            }
{ Joint Endeavour of Delphi Innovators ( Project JEDI )                        }
{                                                                              }
{ You may retrieve the latest version of this file at the Project              }
{ JEDI home page, located at http://delphi-jedi.org                            }
{                                                                              }
{ The contents of this file are used with permission, subject to               }
{ the Mozilla Public License Version 1.1 (the "License"); you may              }
{ not use this file except in compliance with the License. You may             }
{ obtain a copy of the License at                                              }
{ http://www.mozilla.org/MPL/MPL-1.1.html                                      }
{                                                                              }
{ Software distributed under the License is distributed on an                  }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or               }
{ implied. See the License for the specific language governing                 }
{ rights and limitations under the License.                                    }
{                                                                              }
{ Description                                                                  }
{ -----------                                                                  }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{ Requires                                                                     }
{ --------                                                                     }
{   The SDL Runtime libraris on Win32  : SDL.dll on Linux : libSDL.so          }
{   They are available from...                                                 }
{   http://www.libsdl.org .                                                    }
{                                                                              }
{ Programming Notes                                                            }
{ -----------------                                                            }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{ Revision History                                                             }
{ ----------------                                                             }
{   December 08 2002 - DL : Fixed definition of TTF_RenderUnicode_Solid        }
{                                                                              }
{   April   03 2003 - DL : Added jedi-sdl.inc include file to support more     }
{                          Pascal compilers. Initial support is now included   }
{                          for GnuPascal, VirtualPascal, TMT and obviously     }
{                          continue support for Delphi Kylix and FreePascal.   }
{                                                                              }
{   April   24 2003 - DL : under instruction from Alexey Barkovoy, I have added}
{                          better TMT Pascal support and under instruction     }
{                          from Prof. Abimbola Olowofoyeku (The African Chief),}
{                          I have added better Gnu Pascal support              }
{                                                                              }
{   April   30 2003 - DL : under instruction from David Mears AKA              }
{                          Jason Siletto, I have added FPC Linux support.      }
{                          This was compiled with fpc 1.1, so remember to set  }
{                          include file path. ie. -Fi/usr/share/fpcsrc/rtl/*   }
{                                                                              }
{
  $Log$
  Revision 1.2  2004/04/04 15:38:28  marco
   * deleted CR char

  Revision 1.1  2004/04/03 20:09:00  marco
   * New units in this version

  Revision 1.2  2004/03/30 20:23:28  savage
  Tidied up use of UNIX compiler directive.

  Revision 1.1  2004/02/16 22:16:40  savage
  v1.0 changes

  
}
{******************************************************************************}

{$I jedi-sdl.inc}

{$ALIGN ON}

interface

uses
{$IFDEF __GPC__}
  gpc,
{$ENDIF}

{$IFDEF WIN32}
  {$IFNDEF __GPC__}
  Windows,
  {$ENDIF}
{$ENDIF}
  sdl;

type
  PTTF_Font = ^TTTF_font;
  TTTF_Font = record
  end;

const
  {$IFDEF WIN32}
  LibName = 'SDL_ttf.dll';
  {$ENDIF}

  {$IFDEF UNIX}
  LibName = 'libSDL_ttf.so';
  {$ENDIF}

  {$IFDEF MACOS}
  LibName =  'libSDL_ttf.dylib';
  {$ENDIF}

{*
   Set and retrieve the font style
   This font style is implemented by modifying the font glyphs, and
   doesn't reflect any inherent properties of the truetype font file.
*}
  TTF_STYLE_NORMAL	= $00;
  TTF_STYLE_BOLD        = $01;
  TTF_STYLE_ITALIC	= $02;
  TTF_STYLE_UNDERLINE	= $04;

//returns 0 on succes, -1 if error occurs
function TTF_Init : integer;
cdecl; external {$IFDEF __GPC__}name 'TTF_Init'{$ELSE} LibName{$ENDIF __GPC__};
{$EXTERNALSYM TTF_Init}

// Open a font file and create a font of the specified point size
function TTF_OpenFont( const filename : Pchar; ptsize : integer ) : PTTF_Font;
cdecl; external {$IFDEF __GPC__}name 'TTF_OpenFont'{$ELSE} LibName{$ENDIF __GPC__};
{$EXTERNALSYM TTF_OpenFont}
function TTF_OpenFontIndex( const filename : Pchar; ptsize : integer; index : Longint ): PTTF_Font;
cdecl; external {$IFDEF __GPC__}name 'TTF_OpenFontIndex'{$ELSE} LibName{$ENDIF __GPC__};
{$EXTERNALSYM TTF_OpenFontIndex}

function TTF_GetFontStyle( font : PTTF_Font) : integer;
cdecl; external {$IFDEF __GPC__}name 'TTF_GetFontStyle'{$ELSE} LibName{$ENDIF __GPC__};
{$EXTERNALSYM TTF_GetFontStyle}
procedure TTF_SetFontStyle( font : PTTF_Font; style : integer );
cdecl; external {$IFDEF __GPC__}name 'TTF_SetFontStyle'{$ELSE} LibName{$ENDIF __GPC__};
{$EXTERNALSYM TTF_SetFontStyle}

{ Get the total height of the font - usually equal to point size }
function TTF_FontHeight( font : PTTF_Font ) : Integer;
cdecl; external {$IFDEF __GPC__}name 'TTF_FontHeight'{$ELSE} LibName{$ENDIF __GPC__};
{$EXTERNALSYM TTF_FontHeight}
{ Get the offset from the baseline to the top of the font
   This is a positive value, relative to the baseline.
}
function TTF_FontAscent( font : PTTF_Font ) : Integer;
cdecl; external {$IFDEF __GPC__}name 'TTF_FontAscent'{$ELSE} LibName{$ENDIF __GPC__};
{$EXTERNALSYM TTF_FontAscent}
{ Get the offset from the baseline to the bottom of the font
   This is a negative value, relative to the baseline.
}
function TTF_FontDescent( font : PTTF_Font ) : Integer;
cdecl; external {$IFDEF __GPC__}name 'TTF_FontDescent'{$ELSE} LibName{$ENDIF __GPC__};
{$EXTERNALSYM TTF_FontDescent}
{ Get the recommended spacing between lines of text for this font }
function TTF_FontLineSkip( font : PTTF_Font ): Integer;
cdecl; external {$IFDEF __GPC__}name 'TTF_FontLineSkip'{$ELSE} LibName{$ENDIF __GPC__};
{$EXTERNALSYM TTF_FontLineSkip}

{ Get the number of faces of the font }
function TTF_FontFaces( font : PTTF_Font ) : Longint;
cdecl; external {$IFDEF __GPC__}name 'TTF_FontFaces'{$ELSE} LibName{$ENDIF __GPC__};
{$EXTERNALSYM TTF_FontFaces}

{ Get the font face attributes, if any }
function TTF_FontFaceIsFixedWidth( font : PTTF_Font ): Integer;
cdecl; external {$IFDEF __GPC__}name 'TTF_FontFaceIsFixedWidth'{$ELSE} LibName{$ENDIF __GPC__};
{$EXTERNALSYM TTF_FontFaceIsFixedWidth}
function TTF_FontFaceFamilyName( font : PTTF_Font ): PChar;
cdecl; external {$IFDEF __GPC__}name 'TTF_FontFaceFamilyName'{$ELSE} LibName{$ENDIF __GPC__};
{$EXTERNALSYM TTF_FontFaceFamilyName}
function TTF_FontFaceStyleName( font : PTTF_Font ): PChar;
cdecl; external {$IFDEF __GPC__}name 'TTF_FontFaceStyleName'{$ELSE} LibName{$ENDIF __GPC__};
{$EXTERNALSYM TTF_FontFaceStyleName}

{ Get the metrics (dimensions) of a glyph }
function TTF_GlyphMetrics( font : PTTF_Font; ch : Uint16;
                            var minx : integer; var maxx : integer;
                            var miny : integer; var maxy : integer;
                            var advance : integer ): Integer;
cdecl; external {$IFDEF __GPC__}name 'TTF_GlyphMetrics'{$ELSE} LibName{$ENDIF __GPC__};
{$EXTERNALSYM TTF_GlyphMetrics}

{ Get the dimensions of a rendered string of text }
function TTF_SizeText( font : PTTF_Font; const text : PChar; var w : integer; var y : integer ): Integer;
cdecl; external {$IFDEF __GPC__}name 'TTF_SizeText'{$ELSE} LibName{$ENDIF __GPC__};
{$EXTERNALSYM TTF_SizeText}
function TTF_SizeUTF8( font : PTTF_Font; const text : PChar; var w : integer; var y : integer): Integer;
cdecl; external {$IFDEF __GPC__}name 'TTF_SizeUTF8'{$ELSE} LibName{$ENDIF __GPC__};
{$EXTERNALSYM TTF_SizeUTF8}
function TTF_SizeUNICODE( font : PTTF_Font; const text : PUint16; var w : integer; var y : integer): Integer;
cdecl; external {$IFDEF __GPC__}name 'TTF_SizeUNICODE'{$ELSE} LibName{$ENDIF __GPC__};
{$EXTERNALSYM TTF_SizeUNICODE}

{ Create an 8-bit palettized surface and render the given text at
   fast quality with the given font and color.  The 0 pixel is the
   colorkey, giving a transparent background, and the 1 pixel is set
   to the text color.
   This function returns the new surface, or NULL if there was an error.
}
function TTF_RenderText_Solid( font : PTTF_Font;
				const text : PChar; fg : TSDL_Color ): PSDL_Surface;
cdecl; external {$IFDEF __GPC__}name 'TTF_RenderText_Solid'{$ELSE} LibName{$ENDIF __GPC__};
{$EXTERNALSYM TTF_RenderText_Solid}
function TTF_RenderUTF8_Solid( font : PTTF_Font;
				const text : PChar; fg : TSDL_Color ): PSDL_Surface;
cdecl; external {$IFDEF __GPC__}name 'TTF_RenderUTF8_Solid'{$ELSE} LibName{$ENDIF __GPC__};
{$EXTERNALSYM TTF_RenderUTF8_Solid}
function TTF_RenderUNICODE_Solid( font : PTTF_Font;
				const text :PUint16; fg : TSDL_Color ): PSDL_Surface;
cdecl; external {$IFDEF __GPC__}name 'TTF_RenderUNICODE_Solid'{$ELSE} LibName{$ENDIF __GPC__};
{$EXTERNALSYM TTF_RenderUNICODE_Solid}
{
Create an 8-bit palettized surface and render the given glyph at
   fast quality with the given font and color.  The 0 pixel is the
   colorkey, giving a transparent background, and the 1 pixel is set
   to the text color.  The glyph is rendered without any padding or
   centering in the X direction, and aligned normally in the Y direction.
   This function returns the new surface, or NULL if there was an error.
}
function  TTF_RenderGlyph_Solid( font : PTTF_Font;
					ch : Uint16; fg : TSDL_Color ): PSDL_Surface;
cdecl; external {$IFDEF __GPC__}name 'TTF_RenderGlyph_Solid'{$ELSE} LibName{$ENDIF __GPC__};
{$EXTERNALSYM TTF_RenderGlyph_Solid}

{ Create an 8-bit palettized surface and render the given text at
   high quality with the given font and colors.  The 0 pixel is background,
   while other pixels have varying degrees of the foreground color.
   This function returns the new surface, or NULL if there was an error.
}
function TTF_RenderText_Shaded( font : PTTF_Font;
				const text : PChar; fg : TSDL_Color; bg : TSDL_Color ): PSDL_Surface;
cdecl; external {$IFDEF __GPC__}name 'TTF_RenderText_Shaded'{$ELSE} LibName{$ENDIF __GPC__};
{$EXTERNALSYM TTF_RenderText_Shaded}
function TTF_RenderUTF8_Shaded( font : PTTF_Font;
				const text : PChar; fg : TSDL_Color; bg : TSDL_Color ): PSDL_Surface;
cdecl; external {$IFDEF __GPC__}name 'TTF_RenderUTF8_Shaded'{$ELSE} LibName{$ENDIF __GPC__};
{$EXTERNALSYM TTF_RenderUTF8_Shaded}
function TTF_RenderUNICODE_Shaded( font : PTTF_Font;
				const text : PUint16; fg : TSDL_Color; bg : TSDL_Color ): PSDL_Surface;
cdecl; external {$IFDEF __GPC__}name 'TTF_RenderUNICODE_Shaded'{$ELSE} LibName{$ENDIF __GPC__};
{$EXTERNALSYM TTF_RenderUNICODE_Shaded}

{ Create an 8-bit palettized surface and render the given glyph at
   high quality with the given font and colors.  The 0 pixel is background,
   while other pixels have varying degrees of the foreground color.
   The glyph is rendered without any padding or centering in the X
   direction, and aligned normally in the Y direction.
   This function returns the new surface, or NULL if there was an error.
}
function  TTF_RenderGlyph_Shaded( font : PTTF_Font; ch : Uint16; fg : TSDL_Color;
                                  bg : TSDL_Color ): PSDL_Surface;
cdecl; external {$IFDEF __GPC__}name 'TTF_RenderGlyph_Shaded'{$ELSE} LibName{$ENDIF __GPC__};
{$EXTERNALSYM TTF_RenderGlyph_Shaded}

{ Create a 32-bit ARGB surface and render the given text at high quality,
   using alpha blending to dither the font with the given color.
   This function returns the new surface, or NULL if there was an error.
}
function TTF_RenderText_Blended( font : PTTF_Font;
				const text : PChar; fg : TSDL_Color ): PSDL_Surface;
cdecl; external {$IFDEF __GPC__}name 'TTF_RenderText_Blended'{$ELSE} LibName{$ENDIF __GPC__};
{$EXTERNALSYM TTF_RenderText_Blended}
function TTF_RenderUTF8_Blended( font : PTTF_Font;
				const text : PChar; fg : TSDL_Color ): PSDL_Surface;
cdecl; external {$IFDEF __GPC__}name 'TTF_RenderUTF8_Blended'{$ELSE} LibName{$ENDIF __GPC__};
{$EXTERNALSYM TTF_RenderUTF8_Blended}
function TTF_RenderUNICODE_Blended( font : PTTF_Font;
				const text: PUint16; fg : TSDL_Color ): PSDL_Surface;
cdecl; external {$IFDEF __GPC__}name 'TTF_RenderUNICODE_Blended'{$ELSE} LibName{$ENDIF __GPC__};
{$EXTERNALSYM TTF_RenderUNICODE_Blended}

{ Create a 32-bit ARGB surface and render the given glyph at high quality,
   using alpha blending to dither the font with the given color.
   The glyph is rendered without any padding or centering in the X
   direction, and aligned normally in the Y direction.
   This function returns the new surface, or NULL if there was an error.
}
function  TTF_RenderGlyph_Blended( font : PTTF_Font; ch : Uint16; fg : TSDL_Color ): PSDL_Surface;
cdecl; external {$IFDEF __GPC__}name 'TTF_RenderGlyph_Blended'{$ELSE} LibName{$ENDIF __GPC__};
{$EXTERNALSYM TTF_RenderGlyph_Blended}

{ For compatibility with previous versions, here are the old functions }
{#define TTF_RenderText(font, text, fg, bg)
	TTF_RenderText_Shaded(font, text, fg, bg)
#define TTF_RenderUTF8(font, text, fg, bg)	
	TTF_RenderUTF8_Shaded(font, text, fg, bg)
#define TTF_RenderUNICODE(font, text, fg, bg)	
	TTF_RenderUNICODE_Shaded(font, text, fg, bg)}

{ Close an opened font file }
procedure TTF_CloseFont( font : PTTF_Font );
cdecl; external {$IFDEF __GPC__}name 'TTF_CloseFont'{$ELSE} LibName{$ENDIF __GPC__};
{$EXTERNALSYM TTF_CloseFont}

//De-initialize TTF engine
procedure TTF_Quit;
cdecl; external {$IFDEF __GPC__}name 'TTF_Quit'{$ELSE} LibName{$ENDIF __GPC__};
{$EXTERNALSYM TTF_Quit}

implementation

{$IFDEF __GPC__}
  {$L 'sdl_ttf'}  { link sdl_ttf.dll.a or libsdl_ttf.so or libsdl_ttf.a }
{$ELSE}
{$ENDIF}

end.
