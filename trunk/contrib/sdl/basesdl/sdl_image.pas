unit sdl_image;
{
  $Id$
  
}
{******************************************************************************}
{                                                                              }
{       Borland Delphi SDL_Image - An example image loading library for use    }
{                                  with SDL                                    }
{       Conversion of the Simple DirectMedia Layer Image Headers               }
{                                                                              }
{ Portions created by Sam Lantinga <slouken@devolution.com> are                }
{ Copyright (C) 1997, 1998, 1999, 2000, 2001  Sam Lantinga                     }
{ 5635-34 Springhouse Dr.                                                      }
{ Pleasanton, CA 94588 (USA)                                                   }
{                                                                              }
{ All Rights Reserved.                                                         }
{                                                                              }
{ The original files are : SDL_image.h                                         }
{                                                                              }
{ The initial developer of this Pascal code was :                              }
{ Matthias Thoma <ma.thoma@gmx.de>                                             }
{                                                                              }
{ Portions created by Matthias Thoma are                                       }
{ Copyright (C) 2000 - 2001 Matthias Thoma.                                    }
{                                                                              }
{                                                                              }
{ Contributor(s)                                                               }
{ --------------                                                               }
{ Dominique Louis <Dominique@SavageSoftware.com.au>                            }
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
{   A simple library to load images of various formats as SDL surfaces         }
{                                                                              }
{ Requires                                                                     }
{ --------                                                                     }
{   SDL.pas in your search path.                                               }
{                                                                              }
{ Programming Notes                                                            }
{ -----------------                                                            }
{   See the Aliens Demo on how to make use of this libaray                     }
{                                                                              }
{ Revision History                                                             }
{ ----------------                                                             }
{   April    02 2001 - MT : Initial Translation                                }
{                                                                              }
{   May      08 2001 - DL : Added ExternalSym derectives and copyright header  }
{                                                                              }
{   April   03 2003 - DL : Added jedi-sdl.inc include file to support more     }
{                          Pascal compilers. Initial support is now included   }
{                          for GnuPascal, VirtualPascal, TMT and obviously     }
{                          continue support for Delphi Kylix and FreePascal.   }
{                                                                              }
{   April   08 2003 - MK : Aka Mr Kroket - Added Better FPC support            }
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
  Revision 1.3  2004/04/03 20:05:02  marco
   * new versions from Dominique. No postediting at all necessary atm

  Revision 1.2  2004/03/30 20:23:28  savage
  Tidied up use of UNIX compiler directive.

  Revision 1.1  2004/02/14 23:35:42  savage
  version 1 of sdl_image, sdl_mixer and smpeg.


}  
{******************************************************************************}

{$I jedi-sdl.inc}

{$ALIGN ON}

interface

uses
{$IFDEF __GPC__}
  gpc,
{$ENDIF}
  sdl;

const
{$IFDEF WIN32}
  SDL_ImageLibName =  'SDL_Image.dll';
{$ENDIF}
{$IFDEF UNIX}
  SDL_ImageLibName =  'libSDL_image.so';
{$ENDIF}
{$IFDEF __MACH__}
  SDL_ImageLibName =  'libSDL_image.dylib';
{$ENDIF}

{ Load an image from an SDL data source.
   The 'type' may be one of: "BMP", "GIF", "PNG", etc.

   If the image format supports a transparent pixel, SDL will set the
   colorkey for the surface.  You can enable RLE acceleration on the
   surface afterwards by calling:
        SDL_SetColorKey(image, SDL_RLEACCEL, image.format.colorkey);
}
function IMG_LoadTyped_RW(src: PSDL_RWops; freesrc: Integer; _type: PChar): PSDL_Surface;
cdecl; external {$IFDEF __GPC__}name 'IMG_LoadTyped_RW'{$ELSE} SDL_ImageLibName{$ENDIF __GPC__};
{$EXTERNALSYM IMG_LoadTyped_RW}
{ Convenience functions }
function IMG_Load(const _file: PChar): PSDL_Surface;
cdecl; external {$IFDEF __GPC__}name 'IMG_Load'{$ELSE} SDL_ImageLibName{$ENDIF __GPC__};
{$EXTERNALSYM IMG_Load}
function IMG_Load_RW(src: PSDL_RWops; freesrc: Integer): PSDL_Surface;
cdecl; external {$IFDEF __GPC__}name 'IMG_Load_RW'{$ELSE} SDL_ImageLibName{$ENDIF __GPC__};
{$EXTERNALSYM IMG_Load_RW}

{ Invert the alpha of a surface for use with OpenGL
  This function is now a no-op, and only provided for backwards compatibility. }
function IMG_InvertAlpha(_on: Integer): Integer;
cdecl; external {$IFDEF __GPC__}name 'IMG_InvertAlpha'{$ELSE} SDL_ImageLibName{$ENDIF __GPC__};
{$EXTERNALSYM IMG_InvertAlpha}

{ Functions to detect a file type, given a seekable source }
function IMG_isBMP(src: PSDL_RWops): Integer;
cdecl; external {$IFDEF __GPC__}name 'IMG_isBMP'{$ELSE} SDL_ImageLibName{$ENDIF __GPC__};
{$EXTERNALSYM IMG_isBMP}
function IMG_isPNM(src: PSDL_RWops): Integer;
cdecl; external {$IFDEF __GPC__}name 'IMG_isPNM'{$ELSE} SDL_ImageLibName{$ENDIF __GPC__};
{$EXTERNALSYM IMG_isPNM}
function IMG_isXPM(src: PSDL_RWops): Integer;
cdecl; external {$IFDEF __GPC__}name 'IMG_isXPM'{$ELSE} SDL_ImageLibName{$ENDIF __GPC__};
{$EXTERNALSYM IMG_isXPM}
function IMG_isXCF(src: PSDL_RWops): Integer;
cdecl; external {$IFDEF __GPC__}name 'IMG_isXCF'{$ELSE} SDL_ImageLibName{$ENDIF __GPC__};
{$EXTERNALSYM IMG_isXCF}
function IMG_isPCX(src: PSDL_RWops): Integer;
cdecl; external {$IFDEF __GPC__}name 'IMG_isPCX'{$ELSE} SDL_ImageLibName{$ENDIF __GPC__};
{$EXTERNALSYM IMG_isPCX}
function IMG_isGIF(src: PSDL_RWops): Integer;
cdecl; external {$IFDEF __GPC__}name 'IMG_isGIF'{$ELSE} SDL_ImageLibName{$ENDIF __GPC__};
{$EXTERNALSYM IMG_isGIF}
function IMG_isJPG(src: PSDL_RWops): Integer;
cdecl; external {$IFDEF __GPC__}name 'IMG_isJPG'{$ELSE} SDL_ImageLibName{$ENDIF __GPC__};
{$EXTERNALSYM IMG_isJPG}
function IMG_isTIF(src: PSDL_RWops): Integer;
cdecl; external {$IFDEF __GPC__}name 'IMG_isTIF'{$ELSE} SDL_ImageLibName{$ENDIF __GPC__};
{$EXTERNALSYM IMG_isTIF}
function IMG_isPNG(src: PSDL_RWops): Integer;
cdecl; external {$IFDEF __GPC__}name 'IMG_isPNG'{$ELSE} SDL_ImageLibName{$ENDIF __GPC__};
{$EXTERNALSYM IMG_isPNG}

{ Individual loading functions }
function IMG_LoadBMP_RW(src: PSDL_RWops): PSDL_Surface;
cdecl; external {$IFDEF __GPC__}name 'IMG_LoadBMP_RW'{$ELSE} SDL_ImageLibName{$ENDIF __GPC__};
{$EXTERNALSYM IMG_LoadBMP_RW}
function IMG_LoadPNM_RW(src: PSDL_RWops): PSDL_Surface;
cdecl; external {$IFDEF __GPC__}name 'IMG_LoadPNM_RW'{$ELSE} SDL_ImageLibName{$ENDIF __GPC__};
{$EXTERNALSYM IMG_LoadPNM_RW}
function IMG_LoadXPM_RW(src: PSDL_RWops): PSDL_Surface;
cdecl; external {$IFDEF __GPC__}name 'IMG_LoadXPM_RW'{$ELSE} SDL_ImageLibName{$ENDIF __GPC__};
{$EXTERNALSYM IMG_LoadXPM_RW}
function IMG_LoadXCF_RW(src: PSDL_RWops): PSDL_Surface;
cdecl; external {$IFDEF __GPC__}name 'IMG_LoadXCF_RW'{$ELSE} SDL_ImageLibName{$ENDIF __GPC__};
{$EXTERNALSYM IMG_LoadXCF_RW}
function IMG_LoadPCX_RW(src: PSDL_RWops): PSDL_Surface;
cdecl; external {$IFDEF __GPC__}name 'IMG_LoadPCX_RW'{$ELSE} SDL_ImageLibName{$ENDIF __GPC__};
{$EXTERNALSYM IMG_LoadPCX_RW}
function IMG_LoadGIF_RW(src: PSDL_RWops): PSDL_Surface;
cdecl; external {$IFDEF __GPC__}name 'IMG_LoadGIF_RW'{$ELSE} SDL_ImageLibName{$ENDIF __GPC__};
{$EXTERNALSYM IMG_LoadGIF_RW}
function IMG_LoadJPG_RW(src: PSDL_RWops): PSDL_Surface;
cdecl; external {$IFDEF __GPC__}name 'IMG_LoadJPG_RW'{$ELSE} SDL_ImageLibName{$ENDIF __GPC__};
{$EXTERNALSYM IMG_LoadJPG_RW}
function IMG_LoadTIF_RW(src: PSDL_RWops): PSDL_Surface;
cdecl; external {$IFDEF __GPC__}name 'IMG_LoadTIF_RW'{$ELSE} SDL_ImageLibName{$ENDIF __GPC__};
{$EXTERNALSYM IMG_LoadTIF_RW}
function IMG_LoadPNG_RW(src: PSDL_RWops): PSDL_Surface;
cdecl; external {$IFDEF __GPC__}name 'IMG_LoadPNG_RW'{$ELSE} SDL_ImageLibName{$ENDIF __GPC__};
{$EXTERNALSYM IMG_LoadPNG_RW}
function IMG_LoadTGA_RW(src: PSDL_RWops): PSDL_Surface;
cdecl; external {$IFDEF __GPC__}name 'IMG_LoadTGA_RW'{$ELSE} SDL_ImageLibName{$ENDIF __GPC__};
{$EXTERNALSYM IMG_LoadTGA_RW}

{ used internally, NOT an exported function }
//function IMG_string_equals( const str1 : PChar; const str2 : PChar ) : integer;
//cdecl; external {$IFDEF __GPC__}name 'IMG_string_equals'{$ELSE} SDL_ImageLibName{$ENDIF __GPC__};
//{ $ EXTERNALSYM IMG_string_equals}

{ Error Macros }
{ We'll use SDL for reporting errors }
procedure IMG_SetError( fmt : PChar );

function IMG_GetError : PChar;

implementation

{$IFDEF __GPC__}
  {$L 'sdl_image'}  { link sdl_image.dll.a or libsdl_image.so or libsdl_image.a }
{$ENDIF}

procedure IMG_SetError( fmt : PChar );
begin
  SDL_SetError( fmt );
end;

function IMG_GetError : PChar;
begin
  result := SDL_GetError;
end;

end.
