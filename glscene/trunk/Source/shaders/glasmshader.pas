//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLAsmShader<p>

    TGLAsmShader is a wrapper for all ARB shaders<p>


	<b>History : </b><font size=-1><ul>
      <li>22/02/07 - DaStr - Initial version (contributed to GLScene)



    Previous version history:
      v1.0    12 March     '2005  Creation
      v1.1    31 October   '2006  TGLCustomAsmShader.DoUnApply() Fix
                                  TGLAsmShader has more stuff in the published section
}
unit GLAsmShader;

interface

uses
  // VCL
  Classes, SysUtils,

  // GLScene
  VectorGeometry, VectorTypes, GLTexture, OpenGL1x, VectorLists, ARBProgram,
  GLCustomShader;

  {$Include GLScene.inc}

type
  TGLCustomAsmShader = class(TGLCustomShader)
  private
    { Private Declarations }
    FVPHandle: cardinal;
    FFPHandle: cardinal;
  protected
    FLightIDs: TIntegerList;
    procedure FillLights; virtual;
    procedure UnApplyLights; virtual;

    procedure DoLightPass(lightID: cardinal); virtual;
    procedure DoAmbientPass; virtual;
    procedure DestroyARBPrograms; virtual;

    procedure DoInitialize; override;
    procedure DoApply(var rci: TRenderContextInfo; Sender: TObject); override;
    function DoUnApply(var rci: TRenderContextInfo): Boolean; override;
    procedure DoFinalize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    function ShaderSupported: Boolean; override;
  end;

  TGLAsmShader = class(TGLCustomAsmShader)
  published
    property FragmentProgram;
    property VertexProgram;

    property OnApply;
    property OnUnApply;
    property OnInitialize;

    property ShaderStyle;
    property FailedInitAction;
  end;

implementation

{ TGLCustomAsmShader }

procedure TGLCustomAsmShader.DoFinalize;
begin
  inherited;
  DestroyARBPrograms;
end;


procedure TGLCustomAsmShader.Assign(Source: TPersistent);
begin
  inherited Assign(Source);

  if Source is TGLCustomAsmShader then
  begin
    FLightIDs.Assign(TGLCustomAsmShader(Source).FLightIDs);
  end;
end;


constructor TGLCustomAsmShader.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLightIDs := TIntegerList.Create;
end;


destructor TGLCustomAsmShader.Destroy;
begin
  DestroyARBPrograms;
  FLightIDs.Free;
  inherited Destroy;
end;

{$Warnings Off}
procedure TGLCustomAsmShader.DestroyARBPrograms;
begin
  if FVPHandle <> 0 then
  begin
    glDeleteProgramsARB(1, @FVPHandle);
    FVPHandle := 0;
  end;
  if FFPHandle <> 0 then
  begin
    glDeleteProgramsARB(1, @FFPHandle);
    FFPHandle := 0;
  end;  
end;


procedure TGLCustomAsmShader.DoAmbientPass;
var
  ambient, materialAmbient: TVector;
begin
  glDisable(GL_LIGHTING);
  glActiveTextureARB(GL_TEXTURE0_ARB);
  glDisable(GL_TEXTURE_2D);
  glActiveTextureARB(GL_TEXTURE1_ARB);
  glDisable(GL_TEXTURE_2D);
  glActiveTextureARB(GL_TEXTURE0_ARB);

  glGetFloatv(GL_LIGHT_MODEL_AMBIENT, @ambient);
  glGetMaterialfv(GL_FRONT, GL_AMBIENT, @materialAmbient);
  ambient[0] := ambient[0] * materialAmbient[0];
  ambient[1] := ambient[1] * materialAmbient[1];
  ambient[2] := ambient[2] * materialAmbient[2];
  glColor3fv(@ambient);
end;

{$Warnings On}

procedure TGLCustomAsmShader.DoApply(var rci: TRenderContextInfo; Sender: TObject);
begin
  if Assigned(OnApply) then
    OnApply(Self);
end;


procedure TGLCustomAsmShader.DoInitialize;
var
  FailedText: string;
begin
  if not ShaderSupported then
  begin
    Enabled := False;
    HandleFailedInitialization;
  end
  else
  begin
    if VertexProgram.Enabled then
      try
        LoadARBProgram(GL_VERTEX_PROGRAM_ARB, VertexProgram.Code.Text, FVPHandle);
      except
        on E: Exception do
        begin
          FailedText := 'VertexProgram error: ' + #13  + E.Message;
          VertexProgram.Enabled := False;
        end;
      end;

    if FragmentProgram.Enabled then
      try
        LoadARBProgram(GL_FRAGMENT_PROGRAM_ARB, FragmentProgram.Code.Text, FFPHandle);
      except
        on E: Exception do
        begin
          FailedText := 'FragmentProgram error: ' + #13  + E.Message;
          FragmentProgram.Enabled := False;
        end;
      end;

    Enabled := (FragmentProgram.Enabled or VertexProgram.Enabled) and (FailedText = '');
    if Enabled then
    begin
      if Assigned(OnInitialize) then
        OnInitialize(Self)
    end
    else
      HandleFailedInitialization(FailedText);
  end;
end;


procedure TGLCustomAsmShader.DoLightPass(lightID: cardinal);
var
  light: TVector;
begin
  glEnable(GL_VERTEX_PROGRAM_ARB);
  glBindProgramARB(GL_VERTEX_PROGRAM_ARB, FVPHandle);
  glGetLightfv(lightID, GL_POSITION, @light[0]);
  glProgramLocalParameter4fvARB(GL_VERTEX_PROGRAM_ARB, 0, @light[0]);

  glEnable(GL_FRAGMENT_PROGRAM_ARB);
  glBindProgramARB(GL_FRAGMENT_PROGRAM_ARB, FFPHandle);
  glGetLightfv(lightID, GL_DIFFUSE, @light[0]);
  glProgramLocalParameter4fvARB(GL_FRAGMENT_PROGRAM_ARB, 0, @light[0]);
  glGetLightfv(lightID, GL_SPECULAR, @light[0]);
  glProgramLocalParameter4fvARB(GL_FRAGMENT_PROGRAM_ARB, 1, @light[0]);
end;


function TGLCustomAsmShader.DoUnApply(var rci: TRenderContextInfo): Boolean;
begin
  if Assigned(OnUnApply) then
    OnUnApply(Self, Result)
  else
    Result := False;
end;


procedure TGLCustomAsmShader.FillLights;
var
  MaxLights: Integer;
  I: Integer;
  LightEnabled: GLBoolean;
begin
  FLightIDs.Clear;
  glGetIntegerv(GL_MAX_LIGHTS, @maxLights);
  for I := 0 to maxLights - 1 do
  begin
    glGetBooleanv(GL_LIGHT0 + I, @lightEnabled);
    if lightEnabled then
      FLightIDs.Add(GL_LIGHT0 + I);
  end;
end;


function TGLCustomAsmShader.ShaderSupported: Boolean;
var
  maxTextures: Integer;
begin
  Result := (GL_ARB_multitexture and GL_ARB_vertex_program and
             GL_ARB_fragment_program);

  glGetIntegerv(GL_MAX_TEXTURE_UNITS_ARB, @maxTextures);
  if maxTextures < 3 then
    Result := False;
end;


procedure TGLCustomAsmShader.UnApplyLights;
begin
  glDepthFunc(GL_LEQUAL);
  glEnable(GL_BLEND);
  glBlendFunc(GL_ONE, GL_ONE);
  DoLightPass(FLightIDs[0]);
  FLightIDs.Delete(0);
end;

initialization
  RegisterClasses([TGLCustomAsmShader, TGLAsmShader]);

end.

