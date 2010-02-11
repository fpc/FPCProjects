//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLFBO<p>

   Implements FBO support for GLScene.

   Original author of the unit is Riz.
   Modified by C4 and YarUnderoaker (hope, I didn't miss anybody).

   <b>History : </b><font size=-1><ul>
        <li>09/11/09 - DaStr -  Initial version (contributed to GLScene)
   </ul></font>
}
unit GLFBO;

interface

uses
  GLScene, GLContext, GLTexture, GLColor, GLRenderContextInfo;

type
  TGLRBO = class(TGLContextHandle)
  private
    FWidth: Integer;
    FHeight: Integer;
    FStorageValid: Boolean;
    procedure SetHeight(const Value: Integer);
    procedure SetWidth(const Value: Integer);
  protected
    function DoAllocateHandle: cardinal; override;
    procedure DoDestroyHandle; override;

    function GetInternalFormat: cardinal; virtual; abstract;

    procedure InvalidateStorage;
  public
    constructor Create; override;

    procedure Bind;
    procedure Unbind;

    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;
  end;

  TGLDepthRBO = class(TGLRBO)
  private
    FDepthPrecision: TGLDepthPrecision;
    procedure SetDepthPrecision(const Value: TGLDepthPrecision);
  protected
    function GetInternalFormat: cardinal; override;
  public
    constructor Create; override;

    property DepthPrecision: TGLDepthPrecision read FDepthPrecision write SetDepthPrecision;
  end;

  TGLStencilPrecision = (spDefault, sp1bit, sp4bits, sp8bits, sp16bits);

  TGLStencilRBO = class(TGLRBO)
  private
    FStencilPrecision: TGLStencilPrecision;
    procedure SetStencilPrecision(const Value: TGLStencilPrecision);
  protected
    function GetInternalFormat: cardinal; override;
  public
    constructor Create; override;

    property StencilPrecision: TGLStencilPrecision read FStencilPrecision write SetStencilPrecision;
  end;

  TGLFramebufferStatus = (fsComplete, fsIncompleteAttachment, fsIncompleteMissingAttachment,
    fsIncompleteDuplicateAttachment, fsIncompleteDimensions, fsIncompleteFormats,
    fsIncompleteDrawBuffer, fsIncompleteReadBuffer, fsUnsupported, fsStatusError);

  TGLFBO = class(TGLContextHandle)
  private
    FWidth: Integer;
    FHeight: Integer;
    FTextureMipmap: cardinal;
    FAttachedTexture: array[0..31] of cardinal;

    function GetStatus: TGLFramebufferStatus;
    procedure SetHeight(const Value: Integer);
    procedure SetWidth(const Value: Integer);
  protected
    function DoAllocateHandle: cardinal; override;
    procedure DoDestroyHandle; override;

    procedure AttachTexture(Attachment, TextureTarget, TextureHandle: cardinal); overload;
  public
    constructor Create; override;

    // attaches a depth rbo to the fbo
    // the depth buffer must have the same dimentions as the fbo
    procedure AttachDepthBuffer(DepthBuffer: TGLDepthRBO); overload;
    // detaches depth attachment from the fbo
    procedure DetachDepthBuffer;

    // attaches a stencil rbo to the fbo
    // the stencil buffer must have the same dimentions as the fbo
    procedure AttachStencilBuffer(StencilBuffer: TGLStencilRBO); overload;
    // detaches stencil attachment from the fbo
    procedure DetachStencilBuffer;

    // attaches a depth texture to the fbo
    // the depth texture must have the same dimentions as the fbo
    procedure AttachDepthTexture(Texture: TGLTexture); overload;
    procedure DetachDepthTexture;

    procedure AttachTexture(n: Integer; Texture: TGLTexture); overload;
    procedure DetachTexture(n: Integer);

    procedure Bind;
    procedure Unbind;

    procedure PreRender;
    procedure Render(var rci: TRenderContextInfo; baseObject: TGLBaseSceneObject);
    procedure PostRender;

    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;

    property Status: TGLFramebufferStatus read GetStatus;
  end;

implementation

uses
  OpenGL1x, GLUtils, GLGraphics;

{ TGLRBO }

procedure TGLRBO.Bind;
var
  internalFormat: cardinal;
begin
  glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, Handle);

  CheckOpenGLError;

  if not FStorageValid then
  begin
    internalFormat := GetInternalFormat;
    //    internalFormat:= GL_DEPTH_COMPONENT;
    glRenderbufferStorageEXT(GL_RENDERBUFFER_EXT, internalFormat, FWidth, FHeight);

    CheckOpenGLError;
  end;
end;

constructor TGLRBO.Create;
begin
  inherited;

  FWidth := 256;
  FHeight := 256;
end;

function TGLRBO.DoAllocateHandle: cardinal;
begin
  glGenRenderbuffersEXT(1, @Result);
  CheckOpenGLError;
end;

procedure TGLRBO.DoDestroyHandle;
var
  h: cardinal;
begin
  //  if not vIgnoreContextActivationFailures then
  begin
    // reset error status
    glGetError;
    // delete
    h := Handle;
    glDeleteRenderbuffersEXT(1, @h);
    // check for error
    CheckOpenGLError;
  end;
end;

procedure TGLRBO.InvalidateStorage;
begin
  FStorageValid := False;
end;

procedure TGLRBO.SetHeight(const Value: Integer);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    InvalidateStorage;
  end;
end;

procedure TGLRBO.SetWidth(const Value: Integer);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    InvalidateStorage;
  end;
end;

procedure TGLRBO.Unbind;
begin
  glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, 0);
end;

{ TGLDepthRBO }

constructor TGLDepthRBO.Create;
begin
  inherited Create;

  FDepthPrecision := dpDefault;
end;

function TGLDepthRBO.GetInternalFormat: cardinal;
begin
  case DepthPrecision of
    dp24bits: Result := GL_DEPTH_COMPONENT24_ARB;
    dp16bits: Result := GL_DEPTH_COMPONENT16_ARB;
    dp32bits: Result := GL_DEPTH_COMPONENT32_ARB;
    else
      // dpDefault
      Result := GL_DEPTH_COMPONENT24_ARB;
  end;
end;

procedure TGLDepthRBO.SetDepthPrecision(const Value: TGLDepthPrecision);
begin
  if FDepthPrecision <> Value then
  begin
    FDepthPrecision := Value;
    InvalidateStorage;
  end;
end;

{ TGLStencilRBO }

constructor TGLStencilRBO.Create;
begin
  inherited;

  FStencilPrecision := spDefault;
end;

function TGLStencilRBO.GetInternalFormat: cardinal;
begin
  case StencilPrecision of
    spDefault: Result := GL_STENCIL_INDEX;
    sp1bit: Result := GL_STENCIL_INDEX1_EXT;
    sp4bits: Result := GL_STENCIL_INDEX4_EXT;
    sp8bits: Result := GL_STENCIL_INDEX8_EXT;
    sp16bits: Result := GL_STENCIL_INDEX16_EXT;
    else
      // spDefault
      Result := GL_STENCIL_INDEX;
  end;
end;

procedure TGLStencilRBO.SetStencilPrecision(const Value: TGLStencilPrecision);
begin
  if FStencilPrecision <> Value then
  begin
    FStencilPrecision := Value;
    InvalidateStorage;
  end;
end;

{ TGLFBO }

procedure TGLFBO.AttachTexture(n: Integer; Texture: TGLTexture);
var
  target: Integer;
begin
  target := Texture.Image.NativeTextureTarget;
  Assert((target = GL_TEXTURE_2D) or (target = GL_TEXTURE_RECTANGLE_ARB), 'TGLFBO only supports 2D textures');
  // workaround for TGLBitmap32 SetWidth bug
  glBindTexture(target, Texture.Handle);
  if (Texture.TexWidth <> Width) or (Texture.TexHeight <> Height) then
  begin
    glTexImage2d(target, 0,
      TextureFormatToInternalFormat(Texture.TextureFormat),
      Width, Height, Texture.Border, GL_RGBA, GL_UNSIGNED_BYTE, nil);
  end;

  if not ((Texture.MinFilter in [miNearest, miLinear]) or (target = GL_TEXTURE_RECTANGLE_ARB)) then
  begin
    glGenerateMipmapEXT(target);
    FTextureMipmap := FTextureMipmap or (1 shl n);
  end;

  FAttachedTexture[n] := Texture.Handle;

  glBindTexture(target, 0);

  AttachTexture(GL_COLOR_ATTACHMENT0_EXT + n, target, Texture.Handle);
end;

procedure TGLFBO.AttachDepthBuffer(DepthBuffer: TGLDepthRBO);

  procedure AttachDepthRB;
  begin
    // forces initialization
    DepthBuffer.Bind;
    DepthBuffer.Unbind;
    glFramebufferRenderbufferEXT(GL_FRAMEBUFFER_EXT, GL_DEPTH_ATTACHMENT_EXT,
      GL_RENDERBUFFER_EXT, DepthBuffer.Handle);
    CheckOpenGLError;
  end;

var
  dp: TGLDepthPrecision;
begin
  Bind;

  AttachDepthRB;

  // if default format didn't work, try something else
  // crude, but might work
  if (Status = fsUnsupported) and (DepthBuffer.DepthPrecision = dpDefault) then
  begin
    // try the other formats
    // best quality first
    for dp := high(dp) downto low(dp) do
    begin
      if dp = dpDefault then
        Continue;

      DepthBuffer.DepthPrecision := dp;

      AttachDepthRB;

      if not (Status = fsUnsupported) then
        Break;
    end;
  end;
  Status;
  Unbind;
end;

procedure TGLFBO.AttachDepthTexture(Texture: TGLTexture);
begin
  Assert(Texture.Image.NativeTextureTarget = GL_TEXTURE_2D, 'TGLFBO only supports 2D depth textures');
  // workaround for TGLBitmap32 SetWidth bug
  Texture.Handle;
  if (Texture.TexWidth <> Width) or (Texture.TexHeight <> Height) then
  begin
    glBindTexture(GL_TEXTURE_2D, Texture.Handle);
    glTexImage2d(GL_TEXTURE_2D, 0,
      TextureFormatToInternalFormat(Texture.TextureFormat),
      Width, Height, Texture.Border, GL_DEPTH_COMPONENT, GL_FLOAT, nil);
    glBindTexture(GL_TEXTURE_2D, 0);
    CheckOpenGLError;
  end;

  AttachTexture(GL_DEPTH_ATTACHMENT_EXT, GL_TEXTURE_2D, Texture.Handle);
end;

procedure TGLFBO.AttachStencilBuffer(StencilBuffer: TGLStencilRBO);
begin
  Bind;
  // forces initialization
  StencilBuffer.Bind;
  StencilBuffer.Unbind;
  glFramebufferRenderbufferEXT(GL_FRAMEBUFFER_EXT, GL_STENCIL_ATTACHMENT_EXT,
    GL_RENDERBUFFER_EXT, StencilBuffer.Handle);
  CheckOpenGLError;
  Status;
  Unbind;
end;

procedure TGLFBO.AttachTexture(Attachment, TextureTarget, TextureHandle: cardinal);
begin
  Bind;
  glFramebufferTexture2DEXT(GL_FRAMEBUFFER_EXT,
    Attachment, TextureTarget, TextureHandle, 0);
  CheckOpenGLError;
  Status;
  Unbind;
end;

procedure TGLFBO.Bind;
begin
  glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, Handle);
  CheckOpenGLError;
end;

constructor TGLFBO.Create;
begin
  inherited;

  FWidth := 256;
  FHeight := 256;
end;

procedure TGLFBO.DetachTexture(n: Integer);
begin
  Bind;
  // textarget ignored when binding 0
  AttachTexture(GL_COLOR_ATTACHMENT0_EXT + n, GL_TEXTURE_2D, 0);

  FTextureMipmap := FTextureMipmap and (not (1 shl n));
  FAttachedTexture[n] := 0;

  CheckOpenGLError;
  Unbind;
end;

procedure TGLFBO.DetachDepthBuffer;
begin
  Bind;
  glFramebufferRenderbufferEXT(GL_FRAMEBUFFER_EXT, GL_DEPTH_ATTACHMENT_EXT,
    GL_RENDERBUFFER_EXT, 0);
  CheckOpenGLError;
  Unbind;
end;

procedure TGLFBO.DetachDepthTexture;
begin
  AttachTexture(GL_DEPTH_ATTACHMENT_EXT, GL_TEXTURE_2D, 0);
end;

procedure TGLFBO.DetachStencilBuffer;
begin
  AttachTexture(GL_STENCIL_ATTACHMENT_EXT, GL_TEXTURE_2D, 0);
end;

function TGLFBO.DoAllocateHandle: cardinal;
begin
  Result := 0;

  if not GL_EXT_framebuffer_object then
    Exit;

  glGenFramebuffersEXT(1, @Result);
  CheckOpenGLError;
end;

procedure TGLFBO.DoDestroyHandle;
var
  h: cardinal;
begin
  //  if not vIgnoreContextActivationFailures then
  begin
    // reset error status
    glGetError;
    // delete
    h := Handle;
    glDeleteFramebuffersEXT(1, @h);
    // check for error
    CheckOpenGLError;
  end;
end;

function TGLFBO.GetStatus: TGLFramebufferStatus;
var
  status: cardinal;
begin
  status := glCheckFramebufferStatusEXT(GL_FRAMEBUFFER_EXT);

  case status of
    GL_FRAMEBUFFER_COMPLETE_EXT: Result := fsComplete;
    GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT_EXT: Result := fsIncompleteAttachment;
    GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT_EXT: Result := fsIncompleteMissingAttachment;
    GL_FRAMEBUFFER_INCOMPLETE_DUPLICATE_ATTACHMENT_EXT: Result := fsIncompleteDuplicateAttachment;
    GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS_EXT: Result := fsIncompleteDimensions;
    GL_FRAMEBUFFER_INCOMPLETE_FORMATS_EXT: Result := fsIncompleteFormats;
    GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER_EXT: Result := fsIncompleteDrawBuffer;
    GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER_EXT: Result := fsIncompleteReadBuffer;
    GL_FRAMEBUFFER_UNSUPPORTED_EXT: Result := fsUnsupported;
    else
      Result := fsStatusError;
  end;
end;

procedure TGLFBO.PostRender;
var
  I:  Integer;
  ot: Integer;
begin
  glGetIntegerv(GL_TEXTURE_BINDING_2D, @ot);
  for I := 0 to 15 do
  begin
    if FTextureMipmap and (1 shl I) = 0 then
      Continue;

    glBindTexture(GL_TEXTURE_2D, FAttachedTexture[I]);
    glGenerateMipmapEXT(GL_TEXTURE_2D);
  end;
  glBindTexture(GL_TEXTURE_2D, ot);
end;

procedure TGLFBO.PreRender;
begin

end;

procedure TGLFBO.Render(var rci: TRenderContextInfo; baseObject: TGLBaseSceneObject);
var
  backColor: TColorVector;
  buffer:    TGLSceneBuffer;
begin
  Bind;
  Assert(Status = fsComplete, 'Framebuffer not complete');

  buffer := TGLSceneBuffer(rci.buffer);

  backColor := ConvertWinColor(buffer.BackgroundColor);
  glClearColor(backColor[0], backColor[1], backColor[2], buffer.BackgroundAlpha);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  baseObject.Render(rci);

  CheckOpenGLError;

  Unbind;
end;

procedure TGLFBO.SetHeight(const Value: Integer);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
  end;
end;

procedure TGLFBO.SetWidth(const Value: Integer);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
  end;
end;

procedure TGLFBO.Unbind;
begin
  glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, 0);
  CheckOpenGLError;
end;

end.
