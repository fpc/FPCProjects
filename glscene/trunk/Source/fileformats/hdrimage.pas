//
// This unit is part of the GLScene Project, http://glscene.org
//
{: HDRImage<p>
    Good for preview picture in OpenDialog, 
    so you may include both HDRImage (preview) and GLFileHDR (loading)

      <li>21/01/10 - Yar - Creation 
   </ul></font>
}

unit HDRImage;

interface

{$i GLScene.inc}

uses
  Windows, Classes, SysUtils, GLCrossPlatform, VectorGeometry, GLGraphics,
  OpenGL1x, GLPBuffer;

type

  THDRImage = class (TGLBitmap)
  public
   { Public Declarations }
   procedure LoadFromStream(stream : TStream); override;
   procedure SaveToStream(stream : TStream); override;
	end;

implementation

uses
  GLFileHDR, GLTextureFormat;

// ------------------
// ------------------ THDRImage ------------------
// ------------------

// LoadFromStream
//
procedure THDRImage.LoadFromStream(stream : TStream);
var
  FullHDR : TGLHDRImage;
  PBuf : TPixelBuffer;
  y: integer;
  tempBuff, src, dst: PGLubyte;
  tempTex : GLuint;
  DC : HDC;
  RC : HGLRC;
begin
  FullHDR := TGLHDRImage.Create;
  try
    FullHDR.LoadFromStream( stream );
  except
    FullHDR.Free;
    EXIT;
  end;
  // Copy surface as posible to TBitmap
  DC := wglGetCurrentDC;
  RC := wglGetCurrentContext;

  // Create minimal pixel buffer
  if (DC=0) or (RC=0)
  then begin
    PBuf := TPixelBuffer.Create( 1, 1 );
    tempTex := PBuf.TextureID;
  end
  else begin
    Pbuf := nil;
    glPushAttrib(GL_TEXTURE_BIT);
    glGenTextures(1, @tempTex);
  end;
  // Setup texture
  glEnable       ( GL_TEXTURE_2D );
  glBindTexture  ( GL_TEXTURE_2D, tempTex);
  glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  // copy texture to video memory
  glTexImage2D( GL_TEXTURE_2D, 0,
    InternalFormatToOpenGLFormat(FullHDR.InternalFormat), FullHDR.Width,
    FullHDR.Height, 0, FullHDR.ColorFormat, FullHDR.DataType,
    FullHDR.GetLevelData(0));

  CheckOpenGLError;

  GetMem( tempBuff, FullHDR.Width*FullHDR.Height*3 );
  // get texture from video memory in simple format
  glGetTexImage( GL_TEXTURE_2D, 0, GL_BGR, GL_UNSIGNED_BYTE, tempBuff);

  Width       := FullHDR.Width;
  Height      := FullHDR.Height;
  Transparent := false;
  PixelFormat := glpf24bit;

  src := tempBuff;
{$IFNDEF FPC}
  for y := 0 to Height - 1 do
  begin
    dst := ScanLine[Height - 1 - y];
    Move(src^, dst^, Width*3);
    Inc(src, Width*3);
  end;
{$ELSE}

{$ENDIF}
  FullHDR.Free;
  FreeMem( tempBuff );

  CheckOpenGLError;
  if Assigned( pBuf ) then begin
    pBuf.Disable;
    pBuf.Destroy;
  end else
  begin
    glDeleteTextures(1, @tempTex);
    glPopAttrib;
  end;
end;

// SaveToStream
//
procedure THDRImage.SaveToStream(stream : TStream);
begin

end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   TGLPicture.RegisterFileFormat(
     'HDR', 'High Dynamic Range Image', THDRImage);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
finalization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   TGLPicture.UnregisterGraphicClass(THDRImage);

end.
