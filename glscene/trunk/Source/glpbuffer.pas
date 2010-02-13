// =============================================================================
//   Copyright © 2003-2004 by Sascha Willems - http://www.delphigl.de
// =============================================================================
//   --> visit the Delphi OpenGL Community - http://www.delphigl.com <--
// =============================================================================
//   Contents of this file are subject to the GNU Public License (GPL) which can
//   be obtained here : http://opensource.org/licenses/gpl-license.php
//   So only use this file if you fully unterstand that license!!!
// =============================================================================
//   Simple handling of pixelbuffers
// =============================================================================
{  <b>Historique : </b><font size=-1><ul>
      <li>21/09/09 - Yar - Entering to GLScene
  </ul></font>
}
unit GLPBuffer;

interface

uses
  Windows,
  Classes,
  SysUtils,
  OpenGL1x;

type

  TPixelBuffer = class
  private
    DC: HDC;
    RC: HGLRC;
    ParentDC: HDC;
    ParentRC: HGLRC;
    fHandle: GLUInt;
    fWidth: GLUInt;
    fHeight: GLUInt;
    fTextureID: GLUInt;
  public
    constructor Create(pWidth, pHeight: integer);
    destructor Destroy; override;

    function IsLost: boolean;
    procedure Enable;
    procedure Disable;
    procedure Bind;
    procedure Release;

    property Width: GLUInt read fWidth;
    property Height: GLUInt read fHeight;
    property TextureID: GLUInt read fTextureID;
  end;

  EPixelBuffer = class(Exception);

var
  PixelBuffer: TPixelBuffer;

implementation

constructor TPixelBuffer.Create(pWidth, pHeight: integer);
const
  PixelFormatAttribs: array[0..12] of TGLUInt =
   (WGL_SUPPORT_OPENGL_ARB, GL_TRUE,
    WGL_DRAW_TO_PBUFFER_ARB, GL_TRUE,
    WGL_COLOR_BITS_ARB, 24,
    WGL_ALPHA_BITS_ARB, 8,
    WGL_DEPTH_BITS_ARB, 24,
    WGL_DOUBLE_BUFFER_ARB, GL_FALSE, 0);

  PixelBufferAttribs: array[0..4] of TGLUInt =
    (WGL_TEXTURE_FORMAT_ARB, WGL_TEXTURE_RGBA_ARB,
    WGL_TEXTURE_TARGET_ARB,
    WGL_TEXTURE_2D_ARB, 0);
  EmptyF: TGLFLoat = 0;
var
  PFormat: array[0..64] of TGLUInt;
  NumPFormat: TGLUInt;
  TempW, TempH: TGLUInt;
begin
  inherited Create;
  ParentDC := wglGetCurrentDC;
  ParentRC := wglGetCurrentContext;
  fWidth := pWidth;
  fHeight := pHeight;
  if ParentDC = 0 then
  begin
    ParentDC := GetDC(GetDesktopWindow);
    if ParentDC = 0 then
    begin
      raise EPixelBuffer.Create(
        'PixelBuffer->wglGetCurrentDC->Couldn''t obtain valid device context');
      exit;
    end;
  end;

  if not wglChoosePixelFormatARB(ParentDC, @PixelFormatAttribs, @EmptyF,
    Length(PFormat), @PFormat, @NumPFormat) then
  begin
    raise EPixelBuffer.Create(
      'PixelBuffer->wglChoosePixelFormatARB->No suitable pixelformat found');
    exit;
  end;

  fHandle := wglCreatePBufferARB(ParentDC, PFormat[0], fWidth, fHeight,
    @PixelBufferAttribs);
  if fHandle > 0 then
  begin
    wglQueryPbufferARB(fHandle, WGL_PBUFFER_WIDTH_ARB, @TempW);
    wglQueryPbufferARB(fHandle, WGL_PBUFFER_HEIGHT_ARB, @TempH);
  end
  else
  begin
    raise EPixelBuffer.Create(
      'PixelBuffer->wglCreatePBufferARB->Couldn''t obtain valid handle');
    exit;
  end;

  DC := wglGetPBufferDCARB(fHandle);
  if DC = 0 then
  begin
    raise EPixelBuffer.Create(
      'PixelBuffer->wglGetPBufferDCARB->Couldn''t obtain valid DC for PBuffer');
    exit;
  end;

  RC := wglCreateContext(DC);
  if RC = 0 then
  begin
    raise EPixelBuffer.Create(
      'PixelBuffer->wglGetPBufferDCARB->Couldn''t create rendercontext for PBuffer');
    exit;
  end;

  wglMakeCurrent(DC, RC);
  glGenTextures(1, @fTextureID);
end;

destructor TPixelBuffer.Destroy;
begin
  Disable;
  wglDeleteContext(RC);
  wglReleasePbufferDCARB(fHandle, DC);
  wglDestroyPbufferARB(fHandle);
  inherited;
end;

function TPixelBuffer.IsLost: boolean;
var
  Flag: TGLUInt;
begin
  Result := False;
  wglQueryPbufferARB(fHandle, WGL_PBUFFER_LOST_ARB, @Flag);
  if Flag <> 0 then
    Result := True;
end;

procedure TPixelBuffer.Enable;
begin
  ParentDC := wglGetCurrentDC;
  ParentRC := wglGetCurrentContext;
  wglMakeCurrent(DC, RC);
end;

procedure TPixelBuffer.Disable;
begin
  if (ParentDC = 0) or (ParentRC = 0) then
    wglMakeCurrent(0, 0)
  else
    wglMakeCurrent(ParentDC, ParentRC);
end;

procedure TPixelBuffer.Bind;
begin
  wglBindTexImageARB(fHandle, WGL_FRONT_LEFT_ARB);
end;

procedure TPixelBuffer.Release;
begin
  wglReleaseTexImageARB(fHandle, WGL_FRONT_LEFT_ARB);
end;

end.

