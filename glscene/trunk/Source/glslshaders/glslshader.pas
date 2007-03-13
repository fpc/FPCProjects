//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLSLShader<p>

    TGLSLShader is a wrapper for GLS shaders.<p>

	<b>History : </b><font size=-1><ul>
      <li>21/02/07 - DaStr - Initial version (contributed to GLScene)



    Previous version history:
      v1.0    11 March     '2006  Creation
      v1.1    06 August    '2006  TGLCustomGLSLShader.DoInitialize bugfixed
      v1.1.2  24 August    '2006  TGLCustomShader.SetParameterTexture[1-3]D added
      v1.1.4  09 September '2006  Fixed a memory leak which occured when
                                   enabling / disabling the shader several times
      v1.1.6  22 September '2006  DoUnApply fixed (suggested by Nelsol Chu)
      v1.2    04 November  '2006  function GetGLSLProg added (just in case)
                                  TGLSLShader has more published properties
                                  Bugfix in DoInitialize (when no shader is active)
                                  (Get/Set)ParameterTexture[1/2/3]DHandle added
                                  (Get/Set)ParameterCustomTextureHandle support added
      v1.2.4  22 November  '2006  TGLProgramHandle.Name is now used
                                  Assign() bugfixed
                                  Fixed a possible bug in DoInitialize
                                    (Handle was freed, but not nil'ed)

}
unit GLSLShader;

interface

{$I GLScene.inc}

uses
  //VCL
  Classes, SysUtils,
  //GLScene
  VectorGeometry, VectorTypes, GLTexture, GLContext, OpenGL1x, GLCustomShader;

type
  EGLSLShaderException = class(EGLCustomShaderException);

  TGLCustomGLSLShader = class(TGLCustomShader)
  private
    FGLSLProg: TGLProgramHandle;
  protected
    function GetParameter1i(const Index: string): integer; override;
    procedure SetParameter1i(const Index: string; Value: integer); override;
    function GetParameter1f(const Index: string): single; override;
    procedure SetParameter1f(const Index: string; Value: single); override;
    function GetParameter3f(const Index: string): TAffineVector; override;
    procedure SetParameter3f(const Index: string; const Value: TAffineVector); override;
    function GetParameter4f(const Index: string): TVector; override;
    procedure SetParameter4f(const Index: string; const Value: TVector); override;

    function GetParameterMatrix3fv(const Index: string): TAffineMatrix; override;
    procedure SetParameterMatrix3fv(const Index: string; const Value: TAffineMatrix); override;
    function GetParameterMatrix4fv(const Index: string): TMatrix; override;
    procedure SetParameterMatrix4fv(const Index: string; const Value: TMatrix); override;
    function GetParameter2f(const Index: string): TVector2f; override;
    function GetParameter2i(const Index: string): TVector2i; override;
    function GetParameter3i(const Index: string): TVector3i; override;
    function GetParameter4i(const Index: string): TVector4i; override;
    procedure SetParameter2f(const Index: string; const Value: TVector2f); override;
    procedure SetParameter2i(const Index: string; const Value: TVector2i); override;
    procedure SetParameter3i(const Index: string; const Value: TVector3i); override;
    procedure SetParameter4i(const Index: string; const Value: TVector4i); override;

    procedure SetParameterTexture1D(const ParameterName: string; const TextureIndex: Integer; const Value: TGLTexture); override;
    procedure SetParameterTexture2D(const ParameterName: string; const TextureIndex: Integer; const Value: TGLTexture); override;
    procedure SetParameterTexture3D(const ParameterName: string; const TextureIndex: Integer; const Value: TGLTexture); override;

    function GetParameterTexture1DHandle(const ParameterName: string; const TextureIndex: Integer): Cardinal; override;
    function GetParameterTexture2DHandle(const ParameterName: string; const TextureIndex: Integer): Cardinal; override;
    function GetParameterTexture3DHandle(const ParameterName: string; const TextureIndex: Integer): Cardinal; override;

    procedure SetParameterTexture1DHandle(const ParameterName: string; const TextureIndex: Integer; const Value: Cardinal); override;
    procedure SetParameterTexture2DHandle(const ParameterName: string; const TextureIndex: Integer; const Value: Cardinal); override;
    procedure SetParameterTexture3DHandle(const ParameterName: string; const TextureIndex: Integer; const Value: Cardinal); override;

    function GetParameterCustomTextureHandle(const ParameterName: string; const TextureIndex: Integer; const TextureType: Word): Cardinal; override;
    procedure SetParameterCustomTextureHandle(const ParameterName: string; const TextureIndex: Integer; const TextureType: Word; const Value: Cardinal); override;

    function GetGLSLProg: TGLProgramHandle; virtual;
    procedure DoInitialize; override;
    procedure DoFinalize; override;
    procedure DoApply(var rci: TRenderContextInfo; Sender: TObject); override;
    function DoUnApply(var rci: TRenderContextInfo): Boolean; override;
  public
    procedure Assign(Source: TPersistent); override;
    function ShaderSupported: Boolean; override;
  end;


  TGLSLShader = class(TGLCustomGLSLShader)
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

{ TGLCustomGLSLShader }

procedure TGLCustomGLSLShader.DoApply(var rci: TRenderContextInfo; Sender: TObject);
begin
  FGLSLProg.UseProgramObject;
  if Assigned(OnApply) then
    OnApply(Self);
end;


procedure TGLCustomGLSLShader.DoInitialize;
begin
  try
    if not ShaderSupported then
      HandleFailedInitialization
    else
    try
      if VertexProgram.Enabled or FragmentProgram.Enabled then
      begin
        FGLSLProg := TGLProgramHandle.CreateAndAllocate;
        if Name <> '' then
          FGLSLProg.Name := Name
        else
          FGLSLProg.Name := ClassName;
      end;

      if VertexProgram.Enabled then
        FGLSLProg.AddShader(TGLVertexShaderHandle, VertexProgram.Code.Text);
      if FragmentProgram.Enabled then
        FGLSLProg.AddShader(TGLFragmentShaderHandle, FragmentProgram.Code.Text);

      if VertexProgram.Enabled or FragmentProgram.Enabled then
      begin
        if (not FGLSLProg.LinkProgram) then
          raise EGLSLShaderException.Create(FGLSLProg.InfoLog);
        if (not FGLSLProg.ValidateProgram) then
          raise EGLSLShaderException.Create(FGLSLProg.InfoLog);
      end
      else
        FreeAndNil(FGLSLProg);

    except
      on E: Exception do
      begin
        HandleFailedInitialization(E.Message);
        FreeAndNil(FGLSLProg);
      end;
    end;

  finally
    Enabled := (FGLSLProg <> nil);

    if Assigned(OnInitialize) and Enabled then
    begin
      FGLSLProg.UseProgramObject;
      OnInitialize(Self);
      FGLSLProg.EndUseProgramObject;
    end;
  end;
end;


function TGLCustomGLSLShader.DoUnApply(var rci: TRenderContextInfo): Boolean;
begin
  if Assigned(OnUnApply) then
    OnUnApply(Self, Result)
  else
    Result := False;
  if not Result then
  begin
    glActiveTextureARB(GL_TEXTURE0_ARB); //suggested by Nelsol Chu
    FGLSLProg.EndUseProgramObject;
  end;  
end;


function TGLCustomGLSLShader.ShaderSupported: Boolean;
begin
  Result := (GL_ARB_shader_objects and GL_ARB_vertex_program and
             GL_ARB_vertex_shader and GL_ARB_fragment_shader);
end;

// GetParameter1i
//
function TGLCustomGLSLShader.GetParameter1i(const Index: string): integer;
begin
  glGetUniformivARB(FGLSLProg.Handle, FGLSLProg.GetUniformLocation(Index), @Result);
end;

// SetParameter1f
//
procedure TGLCustomGLSLShader.SetParameter1f(const Index: string; Value: single);
begin
  glUniform1fARB(FGLSLProg.GetUniformLocation(Index), Value);
end;

// GetParameter1f
//
function TGLCustomGLSLShader.GetParameter1f(const Index: string): single;
begin
  glGetUniformfvARB(FGLSLProg.Handle, FGLSLProg.GetUniformLocation(Index), @Result);
end;

// SetParameter1i
//
procedure TGLCustomGLSLShader.SetParameter1i(const Index: string; Value: integer);
begin
  glUniform1iARB(FGLSLProg.GetUniformLocation(Index), Value);
end;

// GetParameter3f
//
function TGLCustomGLSLShader.GetParameter3f(const Index: string): TAffineVector;
begin
  glGetUniformfvARB(FGLSLProg.Handle, FGLSLProg.GetUniformLocation(Index), @Result);
end;

// SetParameter3f
//
procedure TGLCustomGLSLShader.SetParameter3f(const Index: string; const Value: TAffineVector);
begin
  glUniform3fARB(FGLSLProg.GetUniformLocation(Index), Value[0], Value[1], Value[2]);
end;

// GetParameter4f
//
function TGLCustomGLSLShader.GetParameter4f(const Index: string): TVector;
begin
  glGetUniformfvARB(FGLSLProg.Handle, FGLSLProg.GetUniformLocation(Index), @Result);
end;

// SetParameter4f
//
procedure TGLCustomGLSLShader.SetParameter4f(const Index: string; const Value: TVector);
begin
  glUniform4fARB(FGLSLProg.GetUniformLocation(Index), Value[0], Value[1], Value[2], Value[3]);
end;

// GetParameterMatrix4fv
//
function TGLCustomGLSLShader.GetParameterMatrix4fv(const Index: string): TMatrix;
begin
  glGetUniformfvARB(FGLSLProg.Handle, FGLSLProg.GetUniformLocation(Index), @Result);
end;

// SetParameterMatrix4fv
//
procedure TGLCustomGLSLShader.SetParameterMatrix4fv(const Index: string; const Value: TMatrix);
begin
  glUniformMatrix4fvARB(FGLSLProg.GetUniformLocation(Index), 1, False, @Value);
end;

// GetParameterMatrix3fv
//
function TGLCustomGLSLShader.GetParameterMatrix3fv(const Index: string): TAffineMatrix;
begin
  glGetUniformfvARB(FGLSLProg.Handle, FGLSLProg.GetUniformLocation(Index), @Result);
end;

// SetParameterMatrix3fv
//
procedure TGLCustomGLSLShader.SetParameterMatrix3fv(const Index: string; const Value: TAffineMatrix);
begin
  glUniformMatrix3fvARB(FGLSLProg.GetUniformLocation(Index), 1, False, @Value);
end;


function TGLCustomGLSLShader.GetParameter2f(const Index: string): TVector2f;
begin
  glGetUniformfvARB(FGLSLProg.Handle, FGLSLProg.GetUniformLocation(Index), @Result);
end;


function TGLCustomGLSLShader.GetParameter2i(const Index: string): TVector2i;
begin
  glGetUniformfvARB(FGLSLProg.Handle, FGLSLProg.GetUniformLocation(Index), @Result);
end;


function TGLCustomGLSLShader.GetParameter3i(const Index: string): TVector3i;
begin
  glGetUniformfvARB(FGLSLProg.Handle, FGLSLProg.GetUniformLocation(Index), @Result);
end;


function TGLCustomGLSLShader.GetParameter4i(const Index: string): TVector4i;
begin
  glGetUniformfvARB(FGLSLProg.Handle, FGLSLProg.GetUniformLocation(Index), @Result);
end;


procedure TGLCustomGLSLShader.SetParameter2f(const Index: string; const Value: TVector2f);
begin
  glUniform2fARB(FGLSLProg.GetUniformLocation(Index), Value[0], Value[1]);
end;


procedure TGLCustomGLSLShader.SetParameter2i(const Index: string; const Value: TVector2i);
begin
  glUniform2iARB(FGLSLProg.GetUniformLocation(Index), Value[0], Value[1]);
end;


procedure TGLCustomGLSLShader.SetParameter3i(const Index: string; const Value: TVector3i);
begin
  glUniform3iARB(FGLSLProg.GetUniformLocation(Index), Value[0], Value[1], Value[2]);
end;


procedure TGLCustomGLSLShader.SetParameter4i(const Index: string; const Value: TVector4i);
begin
  glUniform4iARB(FGLSLProg.GetUniformLocation(Index), Value[0], Value[1], Value[2], Value[3]);
end;


procedure TGLCustomGLSLShader.Assign(Source: TPersistent);
begin
  inherited Assign(Source);

  if Source is TGLCustomGLSLShader then
  begin
    FreeAndNil(FGLSLProg); //just free the handle for it to be recreated on next initialization
  end;
end;

procedure TGLCustomGLSLShader.SetParameterTexture1D(
  const ParameterName: string; const TextureIndex: Integer;
  const Value: TGLTexture);
begin
  glActiveTextureARB(GL_TEXTURE0_ARB + TextureIndex);
  glBindTexture(GL_TEXTURE_1D, Value.Handle);
  Parameter1i[ParameterName] := TextureIndex;
end;

procedure TGLCustomGLSLShader.SetParameterTexture2D(
  const ParameterName: string; const TextureIndex: Integer;
  const Value: TGLTexture);
begin
  glActiveTextureARB(GL_TEXTURE0_ARB + TextureIndex);
  glBindTexture(GL_TEXTURE_2D, Value.Handle);
  Parameter1i[ParameterName] := TextureIndex;
end;

procedure TGLCustomGLSLShader.SetParameterTexture3D(
  const ParameterName: string; const TextureIndex: Integer;
  const Value: TGLTexture);
begin
  glActiveTextureARB(GL_TEXTURE0_ARB + TextureIndex);
  glBindTexture(GL_TEXTURE_3D, Value.Handle);
  Parameter1i[ParameterName] := TextureIndex;
end;

procedure TGLCustomGLSLShader.DoFinalize;
begin
  inherited;
  FreeAndNil(FGLSLProg);
end;

function TGLCustomGLSLShader.GetGLSLProg: TGLProgramHandle;
begin
  Result := FGLSLProg;
end;

procedure TGLCustomGLSLShader.SetParameterTexture1DHandle(
  const ParameterName: string; const TextureIndex: Integer;
  const Value: Cardinal);
begin
  inherited;
  glActiveTextureARB(GL_TEXTURE0_ARB + TextureIndex);
  glBindTexture(GL_TEXTURE_1D, Value);
  Parameter1i[ParameterName] := TextureIndex;
end;

procedure TGLCustomGLSLShader.SetParameterTexture2DHandle(
  const ParameterName: string; const TextureIndex: Integer;
  const Value: Cardinal);
begin
  inherited;
  glActiveTextureARB(GL_TEXTURE0_ARB + TextureIndex);
  glBindTexture(GL_TEXTURE_2D, Value);
  Parameter1i[ParameterName] := TextureIndex;
end;

procedure TGLCustomGLSLShader.SetParameterTexture3DHandle(
  const ParameterName: string; const TextureIndex: Integer;
  const Value: Cardinal);
begin
  glActiveTextureARB(GL_TEXTURE0_ARB + TextureIndex);
  glBindTexture(GL_TEXTURE_3D, Value);
  Parameter1i[ParameterName] := TextureIndex;
end;

function TGLCustomGLSLShader.GetParameterTexture1DHandle(
  const ParameterName: string; const TextureIndex: Integer): Cardinal;
begin
  Result := Parameter1i[ParameterName];
end;

function TGLCustomGLSLShader.GetParameterTexture2DHandle(
  const ParameterName: string; const TextureIndex: Integer): Cardinal;
begin
  Result := Parameter1i[ParameterName];
end;

function TGLCustomGLSLShader.GetParameterTexture3DHandle(
  const ParameterName: string; const TextureIndex: Integer): Cardinal;
begin
  Result := Parameter1i[ParameterName];
end;

function TGLCustomGLSLShader.GetParameterCustomTextureHandle(
  const ParameterName: string; const TextureIndex: Integer;
  const TextureType: Word): Cardinal;
begin
  Result := Parameter1i[ParameterName];
end;

procedure TGLCustomGLSLShader.SetParameterCustomTextureHandle(
  const ParameterName: string; const TextureIndex: Integer;
  const TextureType: Word; const Value: Cardinal);
begin
  glActiveTextureARB(GL_TEXTURE0_ARB + TextureIndex);
  glBindTexture(TextureType, Value);
  Parameter1i[ParameterName] := TextureIndex;
end;

initialization
  RegisterClasses([TGLCustomGLSLShader, TGLSLShader]);

end.

