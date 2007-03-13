//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLPhongShader<p>

   An ARBvp1.0 + ARBfp1.0 shader that implements phong shading.<p>

   <b>History : </b><font size=-1><ul>
      <li>25/02/07 - DaStr - Completely replaced with a descendant of TGLCustomAsmShader.
      <li>11/10/04 - SG - Creation.
   </ul></font>
}
unit GLPhongShader;

interface

{$I GLScene.inc }

uses
  // VCL
  Classes, SysUtils,

  // GLScene
  GLTexture, ARBProgram, VectorGeometry, VectorLists, OpenGL1x, GLAsmShader;

type
  TGLPhongShader = class(TGLCustomAsmShader)
  private
    FDesignTimeEnabled: Boolean;
    FAmbientPass: Boolean;
    procedure SetDesignTimeEnabled(const Value: Boolean);
  protected
    { Protected Declarations }
    procedure DoApply(var rci: TRenderContextInfo; Sender: TObject); override;
    function DoUnApply(var rci: TRenderContextInfo): Boolean; override;
    procedure DoInitialize; override;
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
  published
    { Published Declarations }
    property DesignTimeEnabled: Boolean read FDesignTimeEnabled write SetDesignTimeEnabled default False;
  end;

implementation

// DoApply
//
procedure TGLPhongShader.DoApply(var rci: TRenderContextInfo; Sender: TObject);
begin
  if (csDesigning in ComponentState) and not DesignTimeEnabled then Exit;

  FAmbientPass := False;
  glPushAttrib(GL_ENABLE_BIT or
               GL_TEXTURE_BIT or
               GL_DEPTH_BUFFER_BIT or
               GL_COLOR_BUFFER_BIT);

  FillLights;

  if FLightIDs.Count > 0 then
  begin
    glDepthFunc(GL_LEQUAL);
    glDisable(GL_BLEND);
    DoLightPass(FLightIDs[0]);
    FLightIDs.Delete(0);
  end
  else
  begin
    DoAmbientPass;
    FAmbientPass := True;
  end;
end;

// DoUnApply
//
function TGLPhongShader.DoUnApply(var rci: TRenderContextInfo): Boolean;
begin
  Result := False;
  if (csDesigning in ComponentState) and not DesignTimeEnabled then Exit;

  if FLightIDs.Count > 0 then
  begin
    UnApplyLights;
    Result := True;
    Exit;
  end
  else
  if not FAmbientPass then
  begin
    glDisable(GL_VERTEX_PROGRAM_ARB);
    glDisable(GL_FRAGMENT_PROGRAM_ARB);

    glEnable(GL_BLEND);
    glBlendFunc(GL_ONE, GL_ONE);
    DoAmbientPass;
    FAmbientPass := True;

    Result := True;
    Exit;
  end;

  glPopAttrib;
end;

// DoInitialize
//
procedure TGLPhongShader.DoInitialize;
begin
  if (csDesigning in ComponentState) and not DesignTimeEnabled then Exit;
  inherited DoInitialize;
end;

// SetDesignTimeEnabled
//
procedure TGLPhongShader.SetDesignTimeEnabled(const Value: Boolean);
begin
  if Value <> FDesignTimeEnabled then
  begin
    FDesignTimeEnabled := Value;
    NotifyChange(Self);
  end;
end;

// Create
//
constructor TGLPhongShader.Create(AOwner: TComponent);
begin
  inherited;
  with VertexProgram.Code do
  begin
    Add('!!ARBvp1.0');
    Add('OPTION ARB_position_invariant;');

    Add('PARAM mvinv[4] = { state.matrix.modelview.inverse };');
    Add('PARAM mvit[4] = { state.matrix.modelview.invtrans };');
    Add('PARAM lightPos = program.local[0];');
    Add('TEMP light, normal, eye;');

    Add('   ADD eye, mvit[3], -vertex.position;');
    Add('   MOV eye.w, 0.0;');

    Add('   DP4 light.x, mvinv[0], lightPos;');
    Add('   DP4 light.y, mvinv[1], lightPos;');
    Add('   DP4 light.z, mvinv[2], lightPos;');
    Add('   ADD light, light, -vertex.position;');
    Add('   MOV light.w, 0.0;');

    Add('   MOV result.texcoord[0], vertex.normal;');
    Add('   MOV result.texcoord[1], light;');
    Add('   MOV result.texcoord[2], eye;');

    Add('END');
  end;

  with FragmentProgram.Code do
  begin
    Add('!!ARBfp1.0');

    Add('PARAM lightDiff = program.local[0];');
    Add('PARAM lightSpec = program.local[1];');
    Add('PARAM materialDiff = state.material.diffuse;');
    Add('PARAM materialSpec = state.material.specular;');
    Add('PARAM shininess = state.material.shininess;');
    Add('TEMP temp, light, normal, eye, R, diff, spec;');

    Add('   DP3 temp, fragment.texcoord[0], fragment.texcoord[0];');
    Add('   RSQ temp, temp.x;');
    Add('   MUL normal, temp.x, fragment.texcoord[0];');
    Add('   DP3 temp, fragment.texcoord[1], fragment.texcoord[1];');
    Add('   RSQ temp, temp.x;');
    Add('   MUL light, temp.x, fragment.texcoord[1];');
    Add('   DP3 temp, fragment.texcoord[2], fragment.texcoord[2];');
    Add('   RSQ temp, temp.x;');
    Add('   MUL eye, temp.x, fragment.texcoord[2];');

    Add('   DP3_SAT diff, normal, light;');
    Add('   MUL diff, diff, lightDiff;');
    Add('   MUL diff, diff, materialDiff;');

    Add('   DP3 R, normal, light;');
    Add('   MUL R, R.x, normal;');
    Add('   MUL R, 2.0, R;');
    Add('   ADD R, R, -light;');

    Add('   DP3_SAT spec, R, eye;');
    Add('   POW spec, spec.x, shininess.x;');
    Add('   MUL spec, spec, lightDiff;');
    Add('   MUL spec, spec, materialDiff;');

    Add('   ADD_SAT result.color, diff, spec;');
    Add('   MOV result.color.w, 1.0;');

    Add('END');
  end;
end;

initialization
  RegisterClasses([TGLPhongShader]);

end.
