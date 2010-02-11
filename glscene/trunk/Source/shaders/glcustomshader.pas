// This unit is part of the GLScene Project, http://glscene.org
//
{: GLCustomShader<p>

    A collection of pure abstract classes - descendants of TGLShader, which are
    used for purpose of not having to write the same stuff all over and over
    again in your own shader classes.
    It also contains a procedures and function that can be used in all shaders.<p>

	<b>History : </b><font size=-1><ul>
      <li>25/10/09 - DaStr - Updated TGLGeometryProgram (thanks YarUnderoaker)
      <li>24/08/09 - DaStr - Separated TGLShaderProgram into TGLVertexProgram,
                              TGLFragmentProgram and TGLGeometryProgram
                             Added TGLCustomShaderParameter.AsUniformBuffer
                              (thanks YarUnderoaker)
      <li>28/07/09 - DaStr - Added GeometryShader support (thanks YarUnderoaker)
                             Fixed TGLCustomShader.[...]Program serialization
      <li>24/07/09 - DaStr - Added TGLCustomShader.DebugMode
                             Fixed spelling mistake in TGLShaderUnAplyEvent
                             Added TGLShaderFogSupport, IsFogEnabled()
      <li>03/04/07 - DaStr - Added TGLCustomShaderParameter.AsFloat and AsInteger
      <li>25/03/07 - DaStr - Added TGLCustomShaderParameter.SetToTextureOf
      <li>20/03/07 - DaStr - Added DrawTexturedScreenQuad[4/5/6]
                             "TextureType" parameter renamed to "TextureTarget"
                             Finished working on TGLCustomShaderParameter
      <li>04/03/07 - DaStr - Added IGLPostShader
      <li>03/03/07 - DaStr - Added TGLCustomShaderParameter (beta state)
      <li>22/02/07 - DaStr - Initial version (contributed to GLScene)


    What different shader prefixes might mean:

      ML - Multi Light      -    Shader supports up to 8 lights.
                                 Attributes such as [Ambient/Diffuse/Specular]
                                 Colors are taken from the current OpenGL
                                 state (that means from TGLLightSource too)
                                 In all other cases shader supports only
                                 one light, position of which is taken
                                 from the first registered OpenGL light.



      What different shader suffixes might mean:

       MP - Manual Parameters    - [Ambient/Diffuse/Specular] Colors have
                                   to be set manualy as shader's properties.
                                   In all other cases they are taken from
                                   the current OpenGL  state
                                   (that means from TGLLightSource too)

       MT - Manual Main Texture  - Main Texture is taken not from the
                                   current texture, that is applied to the
                                   rendered object, but has to be set manualy
                                   as shader's property
                                   In all other cases it is taken from
                                   the current texture, that is applied to
                                   the rendered object

       AM - All Manual           - MP + MMT

       AST - Auto Secondary Textures - All other textures are taken from the
                                       textures, that are applied to the
                                       rendered object after the main one
                                       (like the one in TGLLIbMaterial.Texture2Name,
                                       or any other textures that are applied to the
                                       object manualy using TGLMaterial.Apply
                                       or Direct OpenGL API)
                                       In all other cases they are taken from
                                       the shader's properties


    Previous version history:
      v1.0    11 March     '2006  Creation, separated from GLSLShader
      v1.1    06 August    '2006  TGLCustomShader.HandleShaderNotSupportedException added
                                  TGLCustomShader.ShaderNotSupportedExceptionMessage added
      v1.2    14 August    '2006  IGLShaderSupported separated
                                  TGLShaderTextureSource added
      v1.2.2  19 August    '2006  IMultiShaderCompatible added
      v1.2.4  24 August    '2006  TGLCustomShader.ParameterTexture[1-3]D added
      v1.2.6  04 September '2006  Minor fixes
      v1.3    04 November  '2006  TGLShaderUnUplyEvent added
                                  OnApply, OnUnApply, OnInitialize moved to
                                   the protected section
                                  (Un)ApplyBlendingMode added
                                  (Get/Set)ParameterTexture[1/2/3]DHandle added
                                  InitTexture(), DrawTexturedScreenQuad() added
                                  (Get/Set)ParameterCustomTextureHandle support added
      v1.3.2  16 December  '2006  Added shader Naming convention in the comments
                                  STR_SHADER_NEEDS_AT_LEAST_ONE_LIGHT_SOURCE
                                   moved here from StrangeGLSLBumpShader
                                  vStrangeShaderClassList and all shader
                                   registration utility functions added
      v1.3.4  18 February  '2007  StrangeTextureUtilities dependancy removed
                                  Updated to the latest CVS version of GLScene


}
unit GLCustomShader;

interface

{$I GLScene.inc}

uses
  // VCL
  Classes, SysUtils,

  // GLScene
  VectorGeometry, VectorTypes, GLTexture, GLCadencer, OpenGL1x, GLScene,
  GLStrings, GLCrossPlatform, GLContext, GLRenderContextInfo, GLMaterial,
  VectorLists;

const
  glsShaderMaxLightSources = 8;

type
  TGLShaderFogSupport = (sfsEnabled, sfsDisabled, sfsAuto);
  TGLTransformFeedBackMode = (tfbmInterleaved, tfbmSeparate);
  TGLgsInTypes = (gsInPoints, gsInLines, gsInAdjLines, gsInTriangles, gsInAdjTriangles);
  TGLgsOutTypes = (gsOutPoints, gsOutLineStrip, gsOutTriangleStrip);

  EGLCustomShaderException = class(EGLShaderException);

  TGLCustomShader = class;
  TGLVertexProgram = class;
  TGLFragmentProgram = class;
  TGLGeometryProgram = class;

  TGLShaderEvent = procedure(Shader: TGLCustomShader) of object;
  TGLShaderUnAplyEvent = procedure(Shader: TGLCustomShader; var ThereAreMorePasses: Boolean) of object;

  TGLLightSourceEnum = 1..glsShaderMaxLightSources;
  TGLLightSourceSet = set of TGLLightSourceEnum;

  {: This interface describes user shaders, in order to be able to access them
    via a unified interface. If user shader does not support some option, don't
    raise an axception, just ignore it.
  }
  IGLShaderDescription = interface
  ['{04089C64-60C2-43F5-AC9C-38ED46264812}']
    procedure SetShaderTextures(const Textures: array of TGLTexture);
    procedure GetShaderTextures(var Textures: array of TGLTexture);

    procedure SetShaderColorParams(const AAmbientColor, ADiffuseColor, ASpecularcolor: TVector4f);
    procedure GetShaderColorParams(var AAmbientColor, ADiffuseColor, ASpecularcolor: TVector4f);

    procedure SetShaderMiscParameters(const ACadencer: TGLCadencer; const AMatLib: TGLMaterialLibrary; const ALightSources: TGLLightSourceSet);
    procedure GetShaderMiscParameters(var ACadencer: TGLCadencer; var AMatLib: TGLMaterialLibrary; var ALightSources: TGLLightSourceSet);

    function GetShaderAlpha: Single;
    procedure SetShaderAlpha(const Value: Single);

    function GetShaderDescription: string;
  end;

  {: Used in the TGLPostShaderHolder component. }
  IGLPostShader = interface
  ['{68A62362-AF0A-4CE8-A9E1-714FE02AFA4A}']
    {: Called on every pass. }
    procedure DoUseTempTexture(const TempTexture: TGLTextureHandle; const TextureTarget: Cardinal);
    {: Called to determine if it is compatible. }
    function GetTextureTarget: TGLTextureTarget;
  end;

  {: A pure abstract class, must be overriden. }
  TGLCustomShader = class(TGLShader)
  private
    FFragmentProgram: TGLFragmentProgram;
    FVertexProgram: TGLVertexProgram;
    FGeometryProgram: TGLGeometryProgram;

    FTagObject: TObject;
    procedure SetFragmentProgram(const Value: TGLFragmentProgram);
    procedure SetGeometryProgram(const Value: TGLGeometryProgram);
    procedure SetVertexProgram(const Value: TGLVertexProgram);
    function StoreFragmentProgram: Boolean;
    function StoreGeometryProgram: Boolean;
    function StoreVertexProgram: Boolean;
  protected
    FDebugMode: Boolean;
    procedure SetDebugMode(const Value: Boolean); virtual;

    property FragmentProgram: TGLFragmentProgram read FFragmentProgram write SetFragmentProgram stored StoreFragmentProgram;
    property VertexProgram: TGLVertexProgram read FVertexProgram write SetVertexProgram stored StoreVertexProgram;
    property GeometryProgram: TGLGeometryProgram read FGeometryProgram write SetGeometryProgram stored StoreGeometryProgram;

    {: Treats warnings as errors and displays this error,
       instead of a general shader-not-supported message. }
    property DebugMode: Boolean read FDebugMode write SetDebugMode default False;
    property TagObject: TObject read FTagObject write FTagObject default nil;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure LoadShaderPrograms(const VPFilename, FPFilename: string; GPFilename: string = '');
  end;

  {: A custom shader program. }
  TGLShaderProgram = class(TPersistent)
  private
    FParent: TGLCustomShader;
    FEnabled: Boolean;
    FCode: TStrings;
    procedure SetCode(const Value: TStrings);
    procedure SetEnabled(const Value: Boolean);
    procedure OnChangeCode(Sender: TObject);
  protected
    function GetOwner: TPersistent; override;
  public
    procedure LoadFromFile(const AFileName: string);
    procedure Apply; virtual;
    constructor Create(const AParent: TGLCustomShader); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Code: TStrings read FCode write SetCode;
    property Enabled: Boolean read FEnabled write SetEnabled default False;
  end;

  TGLVertexProgram = class(TGLShaderProgram)
  published
    property Code;
    property Enabled;
  end;

  TGLFragmentProgram = class(TGLShaderProgram)
  published
    property Code;
    property Enabled;
  end;

  TGLGeometryProgram = class(TGLShaderProgram)
  private
    FInputPrimitiveType: TGLgsInTypes;
    FOutputPrimitiveType: TGLgsOutTypes;
    FVerticesOut: TGLint;
    procedure SetInputPrimitiveType(const Value: TGLgsInTypes);
    procedure SetOutputPrimitiveType(const Value: TGLgsOutTypes);
    procedure SetVerticesOut(const Value: TGLint);
  public
    constructor Create(const AParent: TGLCustomShader); override;
  published
    property Code;
    property Enabled;

    property InputPrimitiveType: TGLgsInTypes read FInputPrimitiveType write SetInputPrimitiveType default gsInPoints;
    property OutputPrimitiveType: TGLgsOutTypes read FOutputPrimitiveType write SetOutputPrimitiveType default gsOutPoints;
    property VerticesOut: TGLint read FVerticesOut write SetVerticesOut default 0;
  end;

  {: Wrapper around a parameter of the main program. }
  TGLCustomShaderParameter = class(TObject)
  private
    { Private Declarations }
  protected
    { Protected Declarations }
    function GetAsVector1f: Single; virtual; abstract;
    function GetAsVector1i: Integer; virtual; abstract;
    function GetAsVector2f: TVector2f; virtual; abstract;
    function GetAsVector2i: TVector2i; virtual; abstract;
    function GetAsVector3f: TVector3f; virtual; abstract;
    function GetAsVector3i: TVector3i; virtual; abstract;
    function GetAsVector4f: TVector; virtual; abstract;
    function GetAsVector4i: TVector4i; virtual; abstract;

    procedure SetAsVector1f(const Value: Single); virtual; abstract;
    procedure SetAsVector1i(const Value: Integer); virtual; abstract;
    procedure SetAsVector2i(const Value: TVector2i); virtual; abstract;
    procedure SetAsVector3i(const Value: TVector3i); virtual; abstract;
    procedure SetAsVector4i(const Value: TVector4i); virtual; abstract;
    procedure SetAsVector2f(const Value: TVector2f); virtual; abstract;
    procedure SetAsVector3f(const Value: TVector3f); virtual; abstract;
    procedure SetAsVector4f(const Value: TVector4f); virtual; abstract;

    function GetAsMatrix2f: TMatrix2f; virtual; abstract;
    function GetAsMatrix3f: TMatrix3f; virtual; abstract;
    function GetAsMatrix4f: TMatrix4f; virtual; abstract;
    procedure SetAsMatrix2f(const Value: TMatrix2f); virtual; abstract;
    procedure SetAsMatrix3f(const Value: TMatrix3f); virtual; abstract;
    procedure SetAsMatrix4f(const Value: TMatrix4f); virtual; abstract;

    procedure SetAsTexture1D(const TextureIndex: Integer;
      const Value: TGLTexture);
    procedure SetAsTexture2D(const TextureIndex: Integer;
      const Value: TGLTexture);
    procedure SetAsTexture3D(const TextureIndex: Integer;
      const Value: TGLTexture);
    procedure SetAsTextureCube(const TextureIndex: Integer;
      const Value: TGLTexture);
    procedure SetAsTextureRect(const TextureIndex: Integer;
      const Value: TGLTexture);

    function GetAsCustomTexture(const TextureIndex: Integer;
      const TextureTarget: Word): Cardinal; virtual; abstract;
    procedure SetAsCustomTexture(const TextureIndex: Integer;
      const TextureTarget: Word; const Value: Cardinal); virtual; abstract;

    function GetAsUniformBuffer: GLenum; virtual; abstract;
    procedure SetAsUniformBuffer(UBO: GLenum); virtual; abstract;
  public
    { Public Declarations }

    {: This overloaded SetAsVector accepts open array as input. e.g.
       SetAsVectorF([0.1, 0.2]). Array length must between 1-4. }
    procedure SetAsVectorF(const Values: array of Single); overload;
    procedure SetAsVectorI(const Values: array of Integer); overload;

    {: SetToTextureOf determines texture type on-the-fly.}
    procedure SetToTextureOf(const LibMaterial: TGLLibMaterial; const TextureIndex: Integer); overload;
    procedure SetToTextureOf(const Texture: TGLTexture; const TextureIndex: Integer); overload;

    //: GLScene-friendly properties.
    property AsVector: TVector read GetAsVector4f write SetAsVector4f;
    property AsAffineVector: TAffineVector read GetAsVector3f write SetAsVector3f;

    //: Standard types.
    property AsFloat: Single read GetAsVector1f write SetAsVector1f;
    property AsInteger: Integer read GetAsVector1i write SetAsVector1i;

    //: Float vector types.
    property AsVector1f: Single    read GetAsVector1f write SetAsVector1f;
    property AsVector2f: TVector2f read GetAsVector2f write SetAsVector2f;
    property AsVector3f: TVector3f read GetAsVector3f write SetAsVector3f;
    property AsVector4f: TVector4f read GetAsVector4f write SetAsVector4f;

    //: Integer vector  types.
    property AsVector1i: Integer   read GetAsVector1i write SetAsVector1i;
    property AsVector2i: TVector2i read GetAsVector2i write SetAsVector2i;
    property AsVector3i: TVector3i read GetAsVector3i write SetAsVector3i;
    property AsVector4i: TVector4i read GetAsVector4i write SetAsVector4i;

    //: Matrix Types.
    property AsMatrix2f: TMatrix2f read GetAsMatrix2f write SetAsMatrix2f;
    property AsMatrix3f: TMatrix3f read GetAsMatrix3f write SetAsMatrix3f;
    property AsMatrix4f: TMatrix4f read GetAsMatrix4f write SetAsMatrix4f;

    //: Texture Types.
    property AsTexture1D  [const TextureIndex: Integer]: TGLTexture write SetAsTexture1D;
    property AsTexture2D  [const TextureIndex: Integer]: TGLTexture write SetAsTexture2D;
    property AsTexture3D  [const TextureIndex: Integer]: TGLTexture write SetAsTexture3D;
    property AsTextureRect[const TextureIndex: Integer]: TGLTexture write SetAsTextureRect;
    property AsTextureCube[const TextureIndex: Integer]: TGLTexture write SetAsTextureCube;

    property AsCustomTexture[const TextureIndex: Integer; const TextureTarget: Word]: Cardinal read GetAsCustomTexture write SetAsCustomTexture;

    property AsUniformBuffer: GLenum read GetAsUniformBuffer write SetAsUniformBuffer;
  end;


  {: Adds two more blending modes to standard ones.
    Not sure how to name them or if they should be included in TBlending mode,
    so I created a new type here. }
  TGLBlendingModeEx = (bmxOpaque, bmxTransparency, bmxAdditive,
    bmxAlphaTest50, bmxAlphaTest100, bmxModulate,
    bmxDestColorOne, bmxDestAlphaOne);

// Exported procedures.
procedure ApplyBlendingModeEx(const BlendingMode: TGLBlendingModeEx);
procedure UnApplyBlendingModeEx;
procedure InitTexture(const TextureHandle: Cardinal; const TextureSize: TGLSize; const TextureTarget: Word = GL_TEXTURE_2D);

// Probably need to give them proper names, instead of numbers... 
procedure DrawTexturedScreenQuad;
procedure DrawTexturedScreenQuad2(const ViewPortSize: TGLSize);
procedure DrawTexturedScreenQuad3;
procedure DrawTexturedScreenQuad4(const ViewPortSize: TGLSize);
procedure DrawTexturedScreenQuad5(const ViewPortSize: TGLSize);
procedure DrawTexturedScreenQuad6(const ViewPortSize: TGLSize);

procedure CopyScreentoTexture(const ViewPortSize: TGLSize; const TextureTarget: Word = GL_TEXTURE_2D);
procedure CopyScreentoTexture2(const ViewPortSize: TGLSize; const TextureTarget: Word = GL_TEXTURE_2D);

function IsFogEnabled(const AFogSupportMode: TGLShaderFogSupport; var rci: TRenderContextInfo): Boolean;
procedure GetActiveLightsList(const ALightIDs: TIntegerList);

implementation

procedure GetActiveLightsList(const ALightIDs: TIntegerList);
var
  MaxLights: Integer;
  I: Integer;
  LightEnabled: GLBoolean;
begin
  ALightIDs.Clear;
  glGetIntegerv(GL_MAX_LIGHTS, @maxLights);
  for I := 0 to maxLights - 1 do
  begin
    glGetBooleanv(GL_LIGHT0 + I, @lightEnabled);
    if lightEnabled then
      ALightIDs.Add(GL_LIGHT0 + I);
  end;
end;

function IsFogEnabled(const AFogSupportMode: TGLShaderFogSupport; var rci: TRenderContextInfo): Boolean;
begin
  case AFogSupportMode of
    sfsEnabled:  Result := True;
    sfsDisabled: Result := False;
    sfsAuto:     Result := TGLSceneBuffer(rci.buffer).FogEnable;
  else
    Result := False;
    Assert(False, glsUnknownType);
  end;
end;

procedure CopyScreentoTexture(const ViewPortSize: TGLSize; const TextureTarget: Word = GL_TEXTURE_2D);
begin
  glCopyTexSubImage2D(TextureTarget, 0, 0, 0, 0, 0, ViewPortSize.cx, ViewPortSize.cy);
end;

procedure CopyScreentoTexture2(const ViewPortSize: TGLSize; const TextureTarget: Word = GL_TEXTURE_2D);
begin
  glCopyTexImage2D(TextureTarget, 0, GL_RGB, 0, 0, ViewPortSize.cx, ViewPortSize.cy, 0);
end;

procedure ApplyBlendingModeEx(const BlendingMode: TGLBlendingModeEx);
begin
  glPushAttrib(GL_COLOR_BUFFER_BIT);
  glEnable(GL_BLEND);

  case BlendingMode of
    bmxOpaque: glBlendFunc(GL_SRC_ALPHA, GL_ONE);
    bmxTransparency: glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    bmxAdditive: glBlendFunc(GL_SRC_ALPHA, GL_ONE);
    bmxAlphaTest50: glAlphaFunc(GL_GEQUAL, 0.5);
    bmxAlphaTest100: glAlphaFunc(GL_GEQUAL, 1.0);
    bmxModulate: glBlendFunc(GL_DST_COLOR, GL_ZERO);
    bmxDestColorOne: glBlendFunc(GL_DST_COLOR, GL_ONE);
    bmxDestAlphaOne: glBlendFunc(GL_DST_ALPHA, GL_ONE);
    else
      Assert(False, glsErrorEx + glsUnknownType);
  end;
end;

procedure UnApplyBlendingModeEx;
begin
  glPopAttrib;
end;

procedure DrawTexturedScreenQuad;
begin
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix;
  glLoadIdentity;
  glMatrixMode(GL_PROJECTION);
    glPushMatrix;
    glLoadIdentity;

    // drawing rectangle over screen
    glDisable(GL_DEPTH_TEST);
    DrawTexturedScreenQuad3;
    glEnable(GL_DEPTH_TEST);

  glPopMatrix;
  glMatrixMode(GL_MODELVIEW);
  glPopMatrix;
end;

procedure DrawTexturedScreenQuad2(const ViewPortSize: TGLSize);
//var
//  ProjectionMatrix: TMatrix4f;
begin
  glPushMatrix;
  glMatrixMode(GL_PROJECTION);
    glPushMatrix;
    glLoadIdentity;
    glOrtho(0, ViewPortSize.cx, ViewPortSize.cy, 0, 0, 1);
    {
          glMatrixMode(GL_MODELVIEW);
          glGetFloatv(GL_PROJECTION_MATRIX, @ProjectionMatrix);
          glLoadMatrixf(@ProjectionMatrix);
          glLoadIdentity;
    }
    glDisable(GL_DEPTH_TEST);
    glDepthMask(False);
    glBegin(GL_QUADS);
      glTexCoord2f(0.0, ViewPortSize.cy);             glVertex2f(0, 0);
      glTexCoord2f(0.0, 0.0);                         glVertex2f(0, ViewPortSize.cy);
      glTexCoord2f(ViewPortSize.cx, 0.0);             glVertex2f(ViewPortSize.cx, ViewPortSize.cy);
      glTexCoord2f(ViewPortSize.cx, ViewPortSize.cy); glVertex2f(ViewPortSize.cx, 0);
    glEnd;
    glDepthMask(True);
    glEnable(GL_DEPTH_TEST);
    glMatrixMode(GL_PROJECTION);
    glPopMatrix;
  glMatrixMode(GL_MODELVIEW);
  glPopMatrix;
end;

procedure DrawTexturedScreenQuad4(const ViewPortSize: TGLSize);
begin
  glBegin(GL_QUADS);
    glTexCoord2f(0, 0);                             glVertex2f(-1, -1);
    glTexCoord2f(ViewPortSize.cx, 0);               glVertex2f( 1, -1);
    glTexCoord2f(ViewPortSize.cx, ViewPortSize.cy); glVertex2f( 1,  1);
    glTexCoord2f(0, ViewPortSize.cy);               glVertex2f(-1,  1);
  glEnd;
end;

procedure DrawTexturedScreenQuad5(const ViewPortSize: TGLSize);
begin
  glMatrixMode( GL_PROJECTION );
  glPushMatrix;
    glLoadIdentity;
    glOrtho( 0, ViewPortSize.cx, ViewPortSize.cy, 0, 0, 1 );
    glMatrixMode(GL_MODELVIEW);
    glPushMatrix;
      glLoadIdentity;
      glDisable(GL_DEPTH_TEST);
      glDepthMask( FALSE );
      DrawTexturedScreenQuad3;
      glDepthMask( TRUE );
      glEnable(GL_DEPTH_TEST);
    glPopMatrix;
    glMatrixMode( GL_PROJECTION );
  glPopMatrix;
  glMatrixMode( GL_MODELVIEW );
end;

procedure DrawTexturedScreenQuad6(const ViewPortSize: TGLSize);
begin
  glMatrixMode( GL_PROJECTION );
  glPushMatrix;
    glLoadIdentity;
    glOrtho( 0, ViewPortSize.cx, ViewPortSize.cy, 0, 0, 1 );
    glMatrixMode(GL_MODELVIEW);
    glPushMatrix;
      glLoadIdentity;
      glDisable(GL_DEPTH_TEST);
      glDepthMask( FALSE );
      DrawTexturedScreenQuad4(ViewPortSize);;
      glDepthMask( TRUE );
      glEnable(GL_DEPTH_TEST);
    glPopMatrix;
    glMatrixMode( GL_PROJECTION );
  glPopMatrix;
  glMatrixMode( GL_MODELVIEW );
end;

procedure DrawTexturedScreenQuad3;
begin
  glBegin(GL_QUADS);
    glTexCoord2f(0, 0); glVertex2f(-1, -1);
    glTexCoord2f(1, 0); glVertex2f(1, -1);
    glTexCoord2f(1, 1); glVertex2f(1, 1);
    glTexCoord2f(0, 1); glVertex2f(-1, 1);
  glEnd;
end;

procedure InitTexture(const TextureHandle: Cardinal; const TextureSize: TGLSize; const TextureTarget: Word = GL_TEXTURE_2D);
begin
  glBindTexture(TextureTarget, TextureHandle);
  glTexParameteri(TextureTarget, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(TextureTarget, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  glTexParameteri(TextureTarget, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(TextureTarget, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glCopyTexImage2d(TextureTarget, 0, GL_RGBA8, 0, 0, TextureSize.cx, TextureSize.cy, 0);
end;

{ TGLShaderProgram }

procedure TGLShaderProgram.Apply;
begin
  FParent.FinalizeShader;
end;


procedure TGLShaderProgram.Assign(Source: TPersistent);
begin
  if Source = nil then
    Exit;

  if (Source is TGLShaderProgram) then
  begin
    FEnabled := TGLShaderProgram(Source).FEnabled;
    FCode.Assign(TGLShaderProgram(Source).FCode);
  end
  else
    inherited; //die, die, die!!!
end;


constructor TGLShaderProgram.Create(const AParent: TGLCustomShader);
begin
  FParent := AParent;
  FCode := TStringList.Create;
  TStringList(FCode).OnChange := OnChangeCode;
  FEnabled := False;
end;


destructor TGLShaderProgram.Destroy;
begin
  FCode.Destroy;
end;


function TGLShaderProgram.GetOwner: TPersistent;
begin
  Result := FParent;
end;

procedure TGLShaderProgram.LoadFromFile(const AFileName: string);
begin
  FCode.LoadFromFile(AFileName);
  FEnabled := True;
end;


procedure TGLShaderProgram.OnChangeCode(Sender: TObject);
begin
  FEnabled := True;
  FParent.NotifyChange(self);
end;


procedure TGLShaderProgram.SetCode(const Value: TStrings);
begin
  FCode.Assign(Value);
  FParent.NotifyChange(self);
end;


procedure TGLShaderProgram.SetEnabled(const Value: Boolean);
begin
  if Value = FEnabled then
    Exit;
  FEnabled := Value;
  if FEnabled then
    FParent.FinalizeShader;
end;


{ TGLCustomShader }

procedure TGLCustomShader.Assign(Source: TPersistent);
begin
  if Source is TGLCustomShader then
  begin
    FFragmentProgram.Assign(TGLCustomShader(Source).FFragmentProgram);
    FVertexProgram.Assign(TGLCustomShader(Source).FVertexProgram);
    FGeometryProgram.Assign(TGLCustomShader(Source).FGeometryProgram);
    FTagObject := TGLCustomShader(Source).FTagObject;
  end;
  inherited;
end;


constructor TGLCustomShader.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FDebugMode := False;
  FFragmentProgram := TGLFragmentProgram.Create(Self);
  FVertexProgram := TGLVertexProgram.Create(Self);
  FGeometryProgram := TGLGeometryProgram.Create(Self);
end;


destructor TGLCustomShader.Destroy;
begin
  FFragmentProgram.Destroy;
  FVertexProgram.Destroy;
  FGeometryProgram.Destroy;

  inherited;
end;

procedure TGLCustomShader.LoadShaderPrograms(const VPFilename, FPFilename: string; GPFilename: string = '');
begin
  If VPFilename <> '' then VertexProgram.LoadFromFile(VPFilename);
  If FPFilename <> '' then FragmentProgram.LoadFromFile(FPFilename);
  If GPFilename <> '' then GeometryProgram.LoadFromFile(GPFilename);
end;

procedure TGLCustomShader.SetDebugMode(const Value: Boolean);
begin
  if FDebugMode <> Value then
  begin
    FDebugMode := Value;

    if FDebugMode then
      FailedInitAction := fiaReRaiseException
    else
      FailedInitAction := fiaRaiseStandardException;
  end;
end;

procedure TGLCustomShader.SetFragmentProgram(const Value: TGLFragmentProgram);
begin
  FFragmentProgram.Assign(Value);
end;

procedure TGLCustomShader.SetGeometryProgram(const Value: TGLGeometryProgram);
begin
  FGeometryProgram.Assign(Value);
end;

procedure TGLCustomShader.SetVertexProgram(const Value: TGLVertexProgram);
begin
  FVertexProgram.Assign(Value);
end;

function TGLCustomShader.StoreFragmentProgram: Boolean;
begin
  Result := FFragmentProgram.Enabled or (FFragmentProgram.Code.Text <> '')
end;

function TGLCustomShader.StoreGeometryProgram: Boolean;
begin
  Result := FGeometryProgram.Enabled or (FGeometryProgram.Code.Text <> '')
end;

function TGLCustomShader.StoreVertexProgram: Boolean;
begin
  Result := FVertexProgram.Enabled or (FVertexProgram.Code.Text <> '')
end;

{ TGLCustomShaderParameter }

procedure TGLCustomShaderParameter.SetAsTexture1D(
  const TextureIndex: Integer; const Value: TGLTexture);
begin
  SetAsCustomTexture(TextureIndex, GL_TEXTURE_1D, Value.Handle);
end;

procedure TGLCustomShaderParameter.SetAsTexture2D(
  const TextureIndex: Integer; const Value: TGLTexture);
begin
  SetAsCustomTexture(TextureIndex, GL_TEXTURE_2D, Value.Handle);
end;

procedure TGLCustomShaderParameter.SetAsTexture3D(
  const TextureIndex: Integer; const Value: TGLTexture);
begin
  SetAsCustomTexture(TextureIndex, GL_TEXTURE_3D, Value.Handle);
end;

procedure TGLCustomShaderParameter.SetAsTextureCube(
  const TextureIndex: Integer; const Value: TGLTexture);
begin
  SetAsCustomTexture(TextureIndex, GL_TEXTURE_CUBE_MAP_ARB, Value.Handle);
end;

procedure TGLCustomShaderParameter.SetAsTextureRect(
  const TextureIndex: Integer; const Value: TGLTexture);
begin
  SetAsCustomTexture(TextureIndex, GL_TEXTURE_RECTANGLE_ARB, Value.Handle);
end;

procedure TGLCustomShaderParameter.SetAsVectorF(const Values: array of Single);
begin
  case Length(Values) of
    1: SetAsVector1f(Values[0]);
    2: SetAsVector2f(Vector2fMake(Values[0], Values[1]));
    3: SetAsVector3f(Vector3fMake(Values[0], Values[1], Values[2]));
    4: SetAsVector4f(Vector4fMake(Values[0], Values[1], Values[2], Values[3]));
  else
    Assert(False, 'Vector length must be between 1 to 4');
  end;
end;

procedure TGLCustomShaderParameter.SetAsVectorI(const Values: array of Integer);
begin
  case Length(Values) of
    1: SetAsVector1i(Values[0]);
    2: SetAsVector2i(Vector2iMake(Values[0], Values[1]));
    3: SetAsVector3i(Vector3iMake(Values[0], Values[1], Values[2]));
    4: SetAsVector4i(Vector4iMake(Values[0], Values[1], Values[2], Values[3]));
  else
    Assert(False, 'Vector length must be between 1 to 4');
  end;
end;

procedure TGLCustomShaderParameter.SetToTextureOf(
  const LibMaterial: TGLLibMaterial; const TextureIndex: Integer);
begin
  SetToTextureOf(LibMaterial.Material.Texture, TextureIndex);
end;

procedure TGLCustomShaderParameter.SetToTextureOf(
  const Texture: TGLTexture; const TextureIndex: Integer);
begin
  case Texture.Image.NativeTextureTarget of
    GL_TEXTURE_2D : SetAsCustomTexture(TextureIndex, GL_TEXTURE_2D, Texture.Handle);
    GL_TEXTURE_1D : SetAsCustomTexture(TextureIndex, GL_TEXTURE_1D, Texture.Handle);
    GL_TEXTURE_3D : SetAsCustomTexture(TextureIndex, GL_TEXTURE_3D, Texture.Handle);
    GL_TEXTURE_CUBE_MAP_ARB : SetAsCustomTexture(TextureIndex, GL_TEXTURE_CUBE_MAP_ARB, Texture.Handle);
    GL_TEXTURE_RECTANGLE_ARB : SetAsCustomTexture(TextureIndex, GL_TEXTURE_RECTANGLE_ARB, Texture.Handle);
  else
    Assert(False, glsErrorEx + glsUnknownType);
  end;
end;

constructor TGLGeometryProgram.Create(const AParent: TGLCustomShader);
begin
  inherited Create(AParent);
  FInputPrimitiveType := gsInPoints;
  FOutputPrimitiveType := gsOutPoints;
  FVerticesOut := 0;
end;

procedure TGLGeometryProgram.SetInputPrimitiveType(const Value: TGLgsInTypes);
begin
  if Value <> FInputPrimitiveType then
  begin
    FInputPrimitiveType := Value;
    FParent.NotifyChange(Self);
  end;
end;

procedure TGLGeometryProgram.SetOutputPrimitiveType(const Value: TGLgsOutTypes);
begin
  if Value<>FOutputPrimitiveType then
  begin
    FOutputPrimitiveType := Value;
    FParent.NotifyChange(Self);
  end;
end;

procedure TGLGeometryProgram.SetVerticesOut(const Value: TGLint);
begin
  if Value<>FVerticesOut then
  begin
    FVerticesOut := Value;
    FParent.NotifyChange(Self);
  end;
end;

initialization
  RegisterClasses([TGLCustomShader, TGLShaderProgram,
                   TGLVertexProgram, TGLFragmentProgram, TGLGeometryProgram]);

end.