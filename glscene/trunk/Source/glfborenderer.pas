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
unit GLFBORenderer;

interface

uses
  Classes, VectorGeometry, GLScene, GLTexture, GLContext, GLFBO, GLColor,
  GLMaterial, GLRenderContextInfo;

type
  TGLEnabledRenderBuffer = (erbDepth, erbStencil);
  TGLEnabledRenderBuffers = set of TGLEnabledRenderBuffer;

  TGLFBOTargetVisibility = (tvDefault, tvFBOOnly);

  TGLFBORenderer = class(TGLBaseSceneObject, IGLMaterialLibrarySupported)
  private
    FFbo: TGLFBO;
    FDepthRBO: TGLDepthRBO;
    FStencilRBO: TGLStencilRBO;
    FColorAttachment: Integer;
    FRendering: Boolean;

    FHasColor: Boolean;
    FHasDepth: Boolean;
    FHasStencil: Boolean;

    FChanged: Boolean;

    FMaterialLibrary: TGLMaterialLibrary;
    FColorTextureName: TGLLibMaterialName;
    FDepthTextureName: TGLLibMaterialName;
    FWidth: Integer;
    FHeight: Integer;
    FForceTextureDimensions: Boolean;
    FStencilPrecision: TGLStencilPrecision;
    FRootObject: TGLBaseSceneObject;
    FRootVisible: Boolean;
    FCamera: TGLCamera;
    FEnabledRenderBuffers: TGLEnabledRenderBuffers;
    FTargetVisibility: TGLFBOTargetVisibility;
    FBeforeRender: TNotifyEvent;
    FPostInitialize: TNotifyEvent;
    FAfterRender: TNotifyEvent;
    FPreInitialize: TNotifyEvent;
    FBackgroundColor: TGLColor;
    FUseBufferBackground: Boolean;
    FAspect: Single;
    FSceneScaleFactor: Single;
    FUseLibraryAsMultiTarget: Boolean;

    //implementing IGLMaterialLibrarySupported
    function GetMaterialLibrary: TGLMaterialLibrary;
    procedure SetMaterialLibrary(const Value: TGLMaterialLibrary);
    procedure SetDepthTextureName(const Value: TGLLibMaterialName);
    procedure SetColorTextureName(const Value: TGLLibMaterialName);
    procedure SetForceTextureDimentions(const Value: Boolean);
    procedure SetHeight(Value: Integer);
    procedure SetWidth(Value: Integer);
    procedure SetStencilPrecision(const Value: TGLStencilPrecision);
    procedure SetRootObject(const Value: TGLBaseSceneObject);
    function GetViewport: TRectangle;
    procedure SetCamera(const Value: TGLCamera);
    procedure SetEnabledRenderBuffers(const Value: TGLEnabledRenderBuffers);
    procedure SetTargetVisibility(const Value: TGLFBOTargetVisibility);
    procedure SetBackgroundColor(const Value: TGLColor);
    procedure SetUseBufferBackground(const Value: Boolean);
    function StoreSceneScaleFactor: Boolean;
    function StoreAspect: Boolean;
    procedure SetUseLibraryAsMultiTarget(Value: Boolean);
  protected
    procedure InitializeFBO;

    procedure ForceDimensions(Texture: TGLTexture);

    procedure RenderToFBO(var ARci: TRenderContextInfo);

    procedure ApplyCamera;
    procedure UnApplyCamera;
    procedure SetupLights;

    procedure DoBeforeRender;
    procedure DoAfterRender;
    procedure DoPreInitialize;
    procedure DoPostInitialize;

    property HasColor: Boolean read FHasColor;
    property HasDepth: Boolean read FHasDepth;
    property HasStencil: Boolean read FHasStencil;

    property Viewport: TRectangle read GetViewport;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure StructureChanged; override;

    procedure DoRender(var ARci: TRenderContextInfo; ARenderSelf: Boolean; ARenderChildren: Boolean); override;

  published
    // force texture dimensions when initializing
    // only works with TGLBlankImage and TGLFloatDataImage, otherwise does nothing
    property ForceTextureDimensions: Boolean read FForceTextureDimensions write SetForceTextureDimentions default False;

    property Width: Integer read FWidth write SetWidth default 256;
    property Height: Integer read FHeight write SetHeight default 256;

    property Aspect: Single read FAspect write FAspect stored StoreAspect;

    property ColorTextureName: TGLLibMaterialName read FColorTextureName write SetColorTextureName;

    property DepthTextureName: TGLLibMaterialName read FDepthTextureName write SetDepthTextureName;
    property MaterialLibrary: TGLMaterialLibrary read GetMaterialLibrary write SetMaterialLibrary;

    property BackgroundColor: TGLColor read FBackgroundColor write SetBackgroundColor;
    property UseBufferBackground: Boolean read FUseBufferBackground write SetUseBufferBackground;

    // camera used for rendering to the FBO
    // if not assigned, use the active view's camera
    property Camera: TGLCamera read FCamera write SetCamera;

    // adjust the scene scale of the camera so that the rendering
    // becomes independent of the width of the fbo renderer
    // 0 = disabled
    property SceneScaleFactor: Single read FSceneScaleFactor write FSceneScaleFactor stored StoreSceneScaleFactor;

    // root object used when rendering to the FBO
    // if not assigned, uses itself as root and renders the child objects to the FBO
    property RootObject: TGLBaseSceneObject read FRootObject write SetRootObject;

    // determines if target is rendered to FBO only or rendered normally
    // in FBO only mode, if RootObject is assigned, the RootObject's Visible flag is modified
    // in default mode, if RootObject is not assigned, children are rendered normally after being
    // rendered to the FBO
    property TargetVisibility: TGLFBOTargetVisibility read FTargetVisibility write SetTargetVisibility default tvDefault;

    // Enables the use of a render buffer if a texture is not assigned
    property EnabledRenderBuffers: TGLEnabledRenderBuffers read FEnabledRenderBuffers write SetEnabledRenderBuffers;

    // use stencil buffer
    property StencilPrecision: TGLStencilPrecision read FStencilPrecision write SetStencilPrecision default spDefault;

    // called before rendering to the FBO
    property BeforeRender: TNotifyEvent read FBeforeRender write FBeforeRender;
    // called after the rendering to the FBO
    property AfterRender: TNotifyEvent read FAfterRender write FAfterRender;
    // called before the FBO is initialized
    // the FBO is bound before calling this event
    property PreInitialize: TNotifyEvent read FPreInitialize write FPreInitialize;
    // called after the FBO is initialized, but before any rendering
    // the FBO is bound before calling this event
    property PostInitialize: TNotifyEvent read FPostInitialize write FPostInitialize;

    property UseLibraryAsMultiTarget: Boolean read FUseLibraryAsMultiTarget write SetUseLibraryAsMultiTarget default False;
  end;

implementation

uses
  OpenGL1x;

{ TGLFBORenderer }

procedure TGLFBORenderer.ApplyCamera;
var
  sc: Single;
begin
  if assigned(Camera) then
  begin
    glMatrixMode(GL_PROJECTION);
    glPushMatrix;
    glLoadIdentity;
    sc := FCamera.SceneScale;
    if FSceneScaleFactor > 0 then
      FCamera.SceneScale := Width / FSceneScaleFactor;
    FCamera.ApplyPerspective(Viewport, Width, Height, 96); // 96 is default dpi
    FCamera.SceneScale := sc;

    glMatrixMode(GL_MODELVIEW);
    glPushMatrix;
    glLoadIdentity;
    glScalef(1.0 / FAspect, 1.0, 1.0);
    FCamera.Apply;
  end
  else
  begin
    glMatrixMode(GL_MODELVIEW);
    glPushMatrix;
    glScalef(1.0 / FAspect, 1.0, 1.0);
  end;
end;

constructor TGLFBORenderer.Create(AOwner: TComponent);
begin
  inherited;

  ObjectStyle := [osDirectDraw, osNoVisibilityCulling];

  FBackgroundColor := TGLColor.Create(Self);
  FUseBufferBackground := True;
  FUseLibraryAsMultiTarget := False;
  FWidth := 256;
  FHeight := 256;
  FEnabledRenderBuffers := [erbDepth];

  FAspect := 1.0;
  FSceneScaleFactor := 1.0;
end;

destructor TGLFBORenderer.Destroy;
begin
  FFbo.Free;
  FDepthRBO.Free;
  FStencilRBO.Free;

  inherited;
end;

procedure TGLFBORenderer.DoAfterRender;
begin
  if assigned(FAfterRender) then
    FAfterRender(Self);
end;

procedure TGLFBORenderer.DoBeforeRender;
begin
  if assigned(FBeforeRender) then
    FBeforeRender(Self);
end;

procedure TGLFBORenderer.DoPostInitialize;
begin
  if assigned(FPreInitialize) then
    FPreInitialize(Self);
end;

procedure TGLFBORenderer.DoPreInitialize;
begin
  if assigned(FPostInitialize) then
    FPostInitialize(Self);
end;

procedure TGLFBORenderer.DoRender(var ARci: TRenderContextInfo; ARenderSelf, ARenderChildren: Boolean);
begin
  if (csDesigning in ComponentState) then
    Exit;

  RenderToFBO(ARci);

  if (not assigned(FRootObject)) and (TargetVisibility = tvDefault) and ARenderChildren then
  begin
    RenderChildren(0, Count - 1, ARci);
  end;
end;

procedure TGLFBORenderer.ForceDimensions(Texture: TGLTexture);
var
  bi: TGLBlankImage;
  fi: TGLFloatDataImage;
begin
  if Texture.Image is TGLBlankImage then
  begin
    bi := TGLBlankImage(Texture.Image);
    bi.Width := Width;
    bi.Height := Height;
  end
  else if Texture.Image is TGLFloatDataImage then
  begin
    fi := TGLFloatDataImage(Texture.Image);
    fi.Width := Width;
    fi.Height := Height;
  end;
end;

function TGLFBORenderer.GetViewport: TRectangle;
begin
  Result.Left := 0;
  Result.Top := 0;
  Result.Width := Width;
  Result.Height := Height;
end;

procedure TGLFBORenderer.InitializeFBO;
var
  colorTex: TGLTexture;
  depthTex: TGLTexture;
  I: Integer;
  maxAttachment: Integer;
begin
  glGetIntegerv(GL_MAX_COLOR_ATTACHMENTS, @maxAttachment);
  if not assigned(FFbo) then
  begin
    FFbo := TGLFBO.CreateAndAllocate;
  end
  else
    for I := 0 to maxAttachment - 1 do
      FFbo.DetachTexture(I);

  FFbo.Width := Width;
  FFbo.Height := Height;

  FFbo.Bind;
  DoPreInitialize;
  FFbo.Unbind;

  colorTex := FMaterialLibrary.TextureByName(ColorTextureName);
  depthTex := FMaterialLibrary.TextureByName(DepthTextureName);


  FHasColor := False;
  FHasDepth := False;
  FHasStencil := False;
  FColorAttachment := 0;

  if FUseLibraryAsMultiTarget then
  begin
    Assert(FMaterialLibrary.Materials.Count <= maxAttachment,
      'Too many color attachments.');
    // Multicolor attachments
    for I := 0 to FMaterialLibrary.Materials.Count - 1 do
    begin
      colorTex := FMaterialLibrary.Materials[I].Material.Texture;
      // Skip depth texture
      if colorTex = depthTex then
        Continue;
      if ForceTextureDimensions then
        ForceDimensions(colorTex);
      FFbo.AttachTexture(I, colorTex);
      Inc(FColorAttachment);
    end;
    FHasColor := FColorAttachment > 0;
  end
  else
  begin
    // One color attachment
    if Assigned(colorTex) then
    begin
      if ForceTextureDimensions then
        ForceDimensions(colorTex);
      FFbo.AttachTexture(0, colorTex);
      Inc(FColorAttachment);
      FHasColor := True;
    end;
  end;

  if Assigned(depthTex) then
  begin
    if ForceTextureDimensions then
      ForceDimensions(depthTex);
    FFbo.AttachDepthTexture(depthTex);
    FDepthRBO.Free;
    FDepthRBO := nil;
    FHasDepth := True;
  end
  else
  if erbDepth in EnabledRenderBuffers then
  begin
    if not Assigned(FDepthRBO) then
      FDepthRBO := TGLDepthRBO.CreateAndAllocate;

    FDepthRBO.Width := Width;
    FDepthRBO.Height := Height;

    FFbo.AttachDepthBuffer(FDepthRBO);
    FHasDepth := True;
  end
  else
  begin
    FFbo.DetachDepthBuffer;
    FDepthRBO.Free;
    FDepthRBO := nil;
  end;

  if erbStencil in EnabledRenderBuffers then
  begin
    if not assigned(FStencilRBO) then
    begin
      FStencilRBO := TGLStencilRBO.CreateAndAllocate;
    end;

    FStencilRBO.StencilPrecision := FStencilPrecision;
    FStencilRBO.Width := Width;
    FStencilRBO.Height := Height;

    FFbo.AttachStencilBuffer(FStencilRBO);
    FHasStencil := True;
  end
  else
  begin
    FFbo.DetachStencilBuffer;
    FStencilRBO.Free;
    FStencilRBO := nil;
  end;

  CheckOpenGLError;

  FFbo.Bind;

  if FColorAttachment = 0 then
  begin
    glDrawBuffer(GL_NONE);
    glReadBuffer(GL_NONE);

    CheckOpenGLError;
  end;

  DoPostInitialize;
  FFbo.Unbind;

  FChanged := False;
end;

procedure TGLFBORenderer.RenderToFBO(var ARci: TRenderContextInfo);
const
  cDrawBuffers: array[0..15] of GLenum =
    (
    GL_COLOR_ATTACHMENT0_EXT,
    GL_COLOR_ATTACHMENT1_EXT,
    GL_COLOR_ATTACHMENT2_EXT,
    GL_COLOR_ATTACHMENT3_EXT,
    GL_COLOR_ATTACHMENT4_EXT,
    GL_COLOR_ATTACHMENT5_EXT,
    GL_COLOR_ATTACHMENT6_EXT,
    GL_COLOR_ATTACHMENT7_EXT,
    GL_COLOR_ATTACHMENT8_EXT,
    GL_COLOR_ATTACHMENT9_EXT,
    GL_COLOR_ATTACHMENT10_EXT,
    GL_COLOR_ATTACHMENT11_EXT,
    GL_COLOR_ATTACHMENT12_EXT,
    GL_COLOR_ATTACHMENT13_EXT,
    GL_COLOR_ATTACHMENT14_EXT,
    GL_COLOR_ATTACHMENT15_EXT
    );

  function GetClearBits: cardinal;
  begin
    Result := 0;
    if HasColor then
      Result := Result or GL_COLOR_BUFFER_BIT;
    if HasDepth then
      Result := Result or GL_DEPTH_BUFFER_BIT;
    if HasStencil then
      Result := Result or GL_STENCIL_BUFFER_BIT;
  end;

type
  TGLStates = record
    ColorClearValue: TColorVector;
    ColorWriteMasks: array[0..3] of TGLboolean;
    DepthTest: TGLboolean;
    StencilTest: TGLboolean;
  end;

  function SaveStates: TGLStates;
  begin
    glGetFloatv(GL_COLOR_CLEAR_VALUE, @Result.ColorClearValue);
    glGetBooleanv(GL_COLOR_WRITEMASK, @Result.ColorWriteMasks);
    Result.DepthTest := glIsEnabled(GL_DEPTH_TEST);
    Result.StencilTest := glIsEnabled(GL_STENCIL_TEST);
  end;

  procedure RestoreStates(const states: TGLStates);

    procedure SetEnabled(cap: cardinal; Enabled: TGLboolean);
    begin
      if Enabled then
        glEnable(cap)
      else
        glDisable(cap);
    end;

  begin
    glClearColor(states.ColorClearValue[0], states.ColorClearValue[1],
      states.ColorClearValue[2], states.ColorClearValue[3]);
    glColorMask(states.ColorWriteMasks[0], states.ColorWriteMasks[1],
      states.ColorWriteMasks[2], states.ColorWriteMasks[3]);

    SetEnabled(GL_DEPTH_TEST, states.DepthTest);
    SetEnabled(GL_STENCIL_TEST, states.StencilTest);
  end;

var
  saveLighting: cardinal;
  backColor:   TColorVector;
  buffer:      TGLSceneBuffer;
  savedStates: TGLStates;
begin
  // prevent recursion
  if FRendering then
    Exit;

  FRendering := True;

  if (not assigned(FFbo)) or FChanged then
    InitializeFBO;

  try
    ApplyCamera;

    DoBeforeRender;

    FFbo.Bind;
    glDrawBuffers(FColorAttachment, @cDrawBuffers);
    Assert(FFbo.Status = fsComplete, 'Framebuffer not complete');

    saveLighting := 0;
    if assigned(Camera) then
      saveLighting := GL_LIGHTING_BIT;

    // due to some bug, pushing either GL_ENABLE_BIT or GL_COLOR_BUFFER_BIT
    // breaks post processing using shaders 
    glPushAttrib(saveLighting or GL_VIEWPORT_BIT);

    // so we save the states we modify manually
    savedStates := SaveStates;

    SetupLights;

    glViewport(0, 0, Width, Height);

    buffer := ARci.buffer as TGLSceneBuffer;

    if HasColor then
      glColorMask(True, True, True, True)
    else
      glColorMask(False, False, False, False);

    if HasDepth then
      glEnable(GL_DEPTH_TEST)
    else
      glDisable(GL_DEPTH_TEST);

    if HasStencil then
      glEnable(GL_STENCIL_TEST)
    else
      glDisable(GL_STENCIL_TEST);

    if UseBufferBackground then
    begin
      backColor := ConvertWinColor(buffer.BackgroundColor);
      glClearColor(backColor[0], backColor[1], backColor[2], buffer.BackgroundAlpha);
    end
    else
    begin
      glClearColor(FBackgroundColor.Red, FBackgroundColor.Green,
        FBackgroundColor.Blue, FBackgroundColor.Alpha);
    end;

    glClear(GetClearBits);

    FFbo.PreRender;

    // render to fbo
    if Assigned(RootObject) then
    begin
      // if object should only be rendered to the fbo
      // ensure it's visible before rendering to fbo
      if TargetVisibility = tvFBOOnly then
        RootObject.Visible := True;

      RootObject.Render(ARci);

      // then make it invisible afterwards
      if TargetVisibility = tvFBOOnly then
        RootObject.Visible := False;
    end
    else if (Count > 0) then
      RenderChildren(0, Count - 1, ARci);

    CheckOpenGLError;

    FFbo.PostRender;

    RestoreStates(savedStates);

    glPopAttrib;

    UnApplyCamera;
  finally
    FFbo.Unbind;
    FRendering := False;

    DoAfterRender;
  end;
  CheckOpenGLError;
end;

procedure TGLFBORenderer.SetBackgroundColor(const Value: TGLColor);
begin
  FBackgroundColor.Assign(Value);
end;

procedure TGLFBORenderer.SetCamera(const Value: TGLCamera);
begin
  if FCamera <> Value then
  begin
    FCamera := Value;
    StructureChanged;
  end;
end;

procedure TGLFBORenderer.SetColorTextureName(const Value: TGLLibMaterialName);
begin
  if FColorTextureName <> Value then
  begin
    FColorTextureName := Value;
    StructureChanged;
  end;
end;

procedure TGLFBORenderer.SetDepthTextureName(const Value: TGLLibMaterialName);
begin
  if FDepthTextureName <> Value then
  begin
    FDepthTextureName := Value;
    StructureChanged;
  end;
end;

procedure TGLFBORenderer.SetEnabledRenderBuffers(const Value: TGLEnabledRenderBuffers);
begin
  if FEnabledRenderBuffers <> Value then
  begin
    FEnabledRenderBuffers := Value;
    StructureChanged;
  end;
end;

procedure TGLFBORenderer.SetForceTextureDimentions(const Value: Boolean);
begin
  if FForceTextureDimensions <> Value then
  begin
    FForceTextureDimensions := Value;
    StructureChanged;
  end;
end;

// GetMaterialLibrary
//
function TGLFBORenderer.GetMaterialLibrary: TGLMaterialLibrary;
begin
  Result := FMaterialLibrary;
end;

procedure TGLFBORenderer.SetMaterialLibrary(const Value: TGLMaterialLibrary);
begin
  if FMaterialLibrary <> Value then
  begin
    FMaterialLibrary := Value;
    StructureChanged;
  end;
end;

// SetUseLibraryAsMultiTarget
//
procedure TGLFBORenderer.SetUseLibraryAsMultiTarget(Value: Boolean);
begin
  //  Value := Value and (GL_ARB_draw_buffers or GL_ATI_draw_buffers);
  if FUseLibraryAsMultiTarget <> Value then
  begin
    FUseLibraryAsMultiTarget := Value;
    StructureChanged;
  end;
end;

procedure TGLFBORenderer.SetRootObject(const Value: TGLBaseSceneObject);
begin
  if FRootObject <> Value then
  begin
    FRootObject := Value;
    StructureChanged;
  end;
end;

procedure TGLFBORenderer.SetStencilPrecision(const Value: TGLStencilPrecision);
begin
  if FStencilPrecision <> Value then
  begin
    FStencilPrecision := Value;
    StructureChanged;
  end;
end;

procedure TGLFBORenderer.SetTargetVisibility(const Value: TGLFBOTargetVisibility);
begin
  if FTargetVisibility <> Value then
  begin
    if assigned(RootObject) then
    begin
      if (TargetVisibility = tvFBOOnly) then
      begin
        // we went from fbo only, restore root's old visibility
        RootObject.Visible := FRootVisible;
      end
      else
      begin
        // we're going to fbo only, save root visibility for later
        FRootVisible := RootObject.Visible;
      end;
    end;

    FTargetVisibility := Value;
    StructureChanged;
  end;
end;

procedure TGLFBORenderer.SetupLights;
var
  maxLights: Integer;
begin
  if not assigned(Camera) then
    Exit;
  glGetIntegerv(GL_MAX_LIGHTS, @maxLights);
  Camera.Scene.SetupLights(maxLights);
end;

procedure TGLFBORenderer.SetUseBufferBackground(const Value: Boolean);
begin
  if FUseBufferBackground <> Value then
  begin
    FUseBufferBackground := Value;
    StructureChanged;
  end;
end;

// StoreSceneScaleFactor
//
function TGLFBORenderer.StoreSceneScaleFactor: Boolean;
begin
  Result := (FSceneScaleFactor <> 1);
end;

// StoreAspect
//
function TGLFBORenderer.StoreAspect: Boolean;
begin
  Result := (FAspect <> 1.0);
end;

procedure TGLFBORenderer.SetWidth(Value: Integer);
var
  MaxSize: GLsizei;
begin
  if FWidth <> Value then
  begin
    glGetIntegerv(GL_MAX_RENDERBUFFER_SIZE_EXT, @MaxSize);
    if Value > MaxSize then
      Value := MaxSize;
    FWidth := Value;
    StructureChanged;
  end;
end;


procedure TGLFBORenderer.SetHeight(Value: Integer);
var
  MaxSize: GLsizei;
begin
  if FHeight <> Value then
  begin
    glGetIntegerv(GL_MAX_RENDERBUFFER_SIZE_EXT, @MaxSize);
    if Value > MaxSize then
      Value := MaxSize;
    FHeight := Value;
    StructureChanged;
  end;
end;

procedure TGLFBORenderer.StructureChanged;
begin
  FChanged := True;

  inherited;
end;

procedure TGLFBORenderer.UnApplyCamera;
begin
  if assigned(Camera) then
  begin
    glMatrixMode(GL_PROJECTION);
    glPopMatrix;

    glMatrixMode(GL_MODELVIEW);
    glPopMatrix;
  end
  else
  begin
    glMatrixMode(GL_MODELVIEW);
    glPopMatrix;
  end;
end;

initialization

  RegisterClasses([TGLFBORenderer]);

end.
