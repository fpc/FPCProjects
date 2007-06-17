unit GLDynamicTexture;

interface

uses
  Classes, OpenGL1x, GLContext, GLTexture, GLGraphics;

type
  // A dynamic texture image
  // Allows for fast updating of the texture at runtime
  TGLDynamicTextureImage = class(TGLBlankImage)
  private
    FUpdating: integer;
    FTexSize: integer;
    FBuffer: pointer;
    FPBO: TGLBufferObjectHandle;
    FData: pointer;
    TTarget: TGLuint;
    FUseBGR: boolean;
    FUsePBO: boolean;
    procedure SetUsePBO(const Value: boolean);
  protected
    function GetTexSize: integer;
    function GetBitsPerPixel: integer;
    function GetDataFormat: integer;
    function GetTextureFormat: integer;

    property BitsPerPixel: integer read GetBitsPerPixel;
    property DataFormat: integer read GetDataFormat;
    property TextureFormat: integer read GetTextureFormat;
  public
    constructor Create(AOwner: TPersistent); override;

    procedure NotifyChange(Sender: TObject); override;

    // Caches the target to indicate which target to update
    // Do not call this method with a different target than
    // intended for the Texture owner
    function GetBitmap32(target : TGLUInt) : TGLBitmap32; override;

    // Must be called before using the Data pointer
    // Rendering context must be active!
    procedure BeginUpdate;
    
    // Must be called after data is changed
    // This will upload the new data
    procedure EndUpdate;

    // pointer to buffer data
    // will be nil outside a BeginUpdate / EndUpdate block
    property Data: pointer read FData;

    // Indicates that the data is stored as BGR(A) instead of
    // RGB(A). The default is to use BGR(A).
    property UseBGR: boolean read FUseBGR write FUseBGR;

    // Enables or disables use of a PBO. Default is true.
    property UsePBO: boolean read FUsePBO write SetUsePBO;
  end;

implementation

{ TGLDynamicTextureImage }

procedure TGLDynamicTextureImage.BeginUpdate;
begin
  Assert(FUpdating >= 0, 'Unbalanced begin/end update');

  FUpdating:= FUpdating + 1;

  if FUpdating > 1 then
    exit;

  // initialization
  if not (assigned(FPBO) or assigned(FBuffer)) then
  begin
    // cache so we know if it's changed
    FTexSize:= GetTexSize;
    
    if FUsePBO and (GL_ARB_pixel_buffer_object or GL_EXT_pixel_buffer_object) then
    begin
      FPBO:= TGLUnpackPBOHandle.CreateAndAllocate;
      FPBO.BindBufferData(nil, FTexSize, GL_STREAM_DRAW_ARB);
    end
    else
    begin
      // fall back to regular memory buffer if PBO's aren't supported
      FBuffer:= AllocMem(FTexSize);
    end;

    glBindTexture(TTarget, OwnerTexture.Handle);
    glTexImage2D(TTarget, 0, OwnerTexture.OpenGLTextureFormat, Width, Height, 0, TextureFormat, GL_UNSIGNED_BYTE, nil);
  end;

  CheckOpenGLError;
  
  if assigned(FPBO) then
  begin
    FPBO.Bind;

    FData:= FPBO.MapBuffer(GL_WRITE_ONLY_ARB);
  end
  else
  begin
    FData:= FBuffer;
  end;

  CheckOpenGLError;
end;

constructor TGLDynamicTextureImage.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);

  FUseBGR:= true;
  FUsePBO:= true;
end;

procedure TGLDynamicTextureImage.EndUpdate;
var
  d: pointer;
begin
  Assert(FUpdating > 0, 'Unbalanced begin/end update');

  FUpdating:= FUpdating - 1;

  if FUpdating > 0 then
    exit;

  if assigned(FPBO) then
  begin
    FPBO.UnmapBuffer;
    d:= nil;
  end
  else
  begin
    d:= FBuffer;
  end;

  if TTarget <> 0 then
  begin
    // only change data if it's already been uploaded
    glBindTexture(TTarget, OwnerTexture.Handle);
    glTexSubImage2D(TTarget, 0, 0, 0, Width, Height, TextureFormat, DataFormat, d);

    if assigned(FPBO) then
    begin
      FPBO.UnBind;
    end
    else
    begin
    end;

    glBindTexture(TTarget, 0);
  end;

  FData:= nil;

  CheckOpenGLError;
end;

function TGLDynamicTextureImage.GetBitmap32(target: TGLUInt): TGLBitmap32;
begin
  result:= inherited GetBitmap32(target);
  TTarget:= target;
end;

function TGLDynamicTextureImage.GetBitsPerPixel: integer;
var
  tf: TGLTextureFormat;
begin
  tf:= OwnerTexture.TextureFormat;
  if tf = tfDefault then
    tf:= vDefaultTextureFormat;

  result:= 0;
  case tf of
    tfDefault: Assert(false, 'Invalid texture format');
    tfRGB: result:= 3;
    tfRGBA: result:= 4;
    tfRGB16: result:= 6;
    tfRGBA16: result:= 8;
    tfAlpha: result:= 1;
    tfLuminance: result:= 1;
    tfLuminanceAlpha: result:= 2;
    tfIntensity: result:= 1;
    tfNormalMap: result:= 3;
    tfRGBAFloat16: result:= 8;
    tfRGBAFloat32: result:= 16;
  else
    Assert(false, 'Invalid texture format');
  end;
end;

function TGLDynamicTextureImage.GetDataFormat: integer;
var
  tf: TGLTextureFormat;
begin
  tf:= OwnerTexture.TextureFormat;
  if tf = tfDefault then
    tf:= vDefaultTextureFormat;

  result:= 0;
  case tf of
    tfDefault: Assert(false, 'Invalid texture format');
    tfRGB16, tfRGBA16: result:= GL_UNSIGNED_SHORT;
    tfRGBAFloat16, tfRGBAFloat32: result:= GL_FLOAT;
  else
    // safe since any invalid texture formats will get
    // caught by GetBitsPerPixel before this
    result:= GL_UNSIGNED_BYTE;
  end;
end;

function TGLDynamicTextureImage.GetTexSize: integer;
begin
  result:= Width * Height * BitsPerPixel;
end;

function TGLDynamicTextureImage.GetTextureFormat: integer;
const
  RGBFormat: array[boolean] of integer = (GL_RGB, GL_BGR);
  RGBAFormat: array[boolean] of integer = (GL_RGBA, GL_BGRA);
var
  tf: TGLTextureFormat;
begin
  tf:= OwnerTexture.TextureFormat;
  if tf = tfDefault then
    tf:= vDefaultTextureFormat;

  result:= 0;
  case tf of
    tfDefault: Assert(false, 'Invalid texture format');
    tfRGB, tfRGB16, tfNormalMap: result:= RGBFormat[FUseBGR];
    tfRGBA, tfRGBA16, tfRGBAFloat16, tfRGBAFloat32: result:= RGBAFormat[FUseBGR];
    tfAlpha: result:= GL_ALPHA;
    tfLuminance: result:= GL_LUMINANCE;
    tfLuminanceAlpha: result:= GL_LUMINANCE_ALPHA;
    tfIntensity: result:= GL_INTENSITY;
  else
    Assert(false, 'Invalid texture format');
  end;
end;

procedure TGLDynamicTextureImage.NotifyChange(Sender: TObject);
begin
  if FTexSize <> GetTexSize then
  begin
    FPBO.Free;
    if assigned(FBuffer) then    
      FreeMem(FBuffer);
  end;

  inherited;
end;

procedure TGLDynamicTextureImage.SetUsePBO(const Value: boolean);
begin
  Assert(FUpdating = 0, 'Cannot change PBO settings while updating');
  if FUsePBO <> Value then
  begin
    FUsePBO := Value;
    if not FUsePBO then
    begin
      FPBO.Free;
      FPBO:= nil;
    end;
  end;
end;

initialization
  RegisterGLTextureImageClass(TGLDynamicTextureImage);

end.
