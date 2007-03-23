{: GLSLProjectedTextures<p>

   Implements projected textures through a GLScene object via GLSL.

   <b>History : </b><font size=-1><ul>
        <li>23/03/07 - fig -    Fixed reverse projection bug and added Quick Decimal Separator fix.
                                Finished Design time support.
        <li>22/03/07 - fig -    Initial version.
   </ul></font>
}

{; Known bugs/limitations

1. Only 1 texture can be used for all emitters
2. Only up to 6 Emitters can be used (more on better cards)
   A way round this is to make the emiitters a children of the 6 nearest objects to the camera.
3. Changing emiiter properties causes a slight delay while recreating the shader.
   So, no dynamic flickering lights etc. yet.
4. All children of the ProjectedTextures must have use a texture.
   The shader can't be changed between rendering each seperate object..
}

unit GLSLProjectedTextures;

interface

uses
    Classes,
    GLScene,
    GLTexture,
    OpenGL1x,
    VectorGeometry,
    GLContext,
    sysutils;

type
    TGLSLProjectedTexturesStyle = (ptsLight, ptsShadow);

    TGLSLProjectedTextures = class;

    // TGLSLTextureEmmiter
    //
    {: A projected texture emmiter.<p>
       Can be places anywhere in the scene.
       Used to generate a modelview and texture matrix for the shader}
    TGLSLTextureEmitter = class(TGLBaseSceneObject)
    private
        FFOV: single;
        FAspect, FBrightness, FAttenuation: single;
        FStyle: TGLSLProjectedTexturesStyle;
        FColor: TGLColor;
        FUseAttenuation, FAllowReverseProjection: boolean;
        procedure ColorNotifyChange(sender: TObject);
    protected
        ProjectedTexturesObject: TGLSLProjectedTextures;
        TexMatrix, invmatrix: TMatrix;
        TexMatrixChanged: boolean;
        procedure SetupTexMatrix;
        procedure SetAttenuation(val: single);
        procedure SetBrightness(val: single);
        procedure SetColor(val: TGLColor);
        procedure SetStyle(val: TGLSLProjectedTexturesStyle);
        procedure SetUseAttenuation(val: boolean);
        procedure SetAspect(val: single);
        procedure SetFOV(val: single);
        procedure SetAllowReverseProjection(val: boolean);
    public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        procedure DoRender(var rci: TRenderContextInfo; renderSelf, renderChildren: boolean); override;
    published
        {: Indicates the field-of-view of the projection frustum.}
        property FOV: single read FFOV write SetFOV;
        {: x/y ratio. For no distortion, this should be set to
           texture.width/texture.height.}
        property Aspect: single read FAspect write SetAspect;
        {: Indicates the style of the projected textures.}
        property Style: TGLSLProjectedTexturesStyle read FStyle write SetStyle;
        {:Fall off/ attenuation of the projected texture}
        property Attenuation: single read FAttenuation write SetAttenuation;
        property Brightness: single read FBrightness write SetBrightness;
        property Color: TGLColor read FColor write SetColor;
        property UseAttenuation: boolean read FUseAttenuation write SetUseAttenuation;
        property AllowReverseProjection: boolean read FAllowReverseProjection write SetAllowReverseProjection;

        property ObjectsSorting;
         property VisibilityCulling;
         property Direction;
         property PitchAngle;
         property Position;
         property RollAngle;
         property Scale;
         property ShowAxes;
         property TurnAngle;
         property Up;
         property Visible;
         property OnProgress;
         property Behaviours;
         property Effects;
    end;

    // TGLSLTextureEmitterItem
    //
    {: Specifies an item on the TGLSLTextureEmitters collection. }
    TGLSLTextureEmitterItem = class(TCollectionItem)
    private
        FEmitter: TGLSLTextureEmitter;
    protected
        procedure SetEmitter(const val: TGLSLTextureEmitter);
        procedure RemoveNotification(aComponent: TComponent);
        function GetDisplayName: string; override;
    public
        constructor Create(Collection: TCollection); override;
        procedure Assign(Source: TPersistent); override;
    published
        property Emitter: TGLSLTextureEmitter read FEmitter write SetEmitter;
    end;

    // TGLSLTextureEmitters
    //
    {: Collection of TGLSLTextureEmitter. }
    TGLSLTextureEmitters = class(TCollection)
    private
        FOwner: TGLSLProjectedTextures;
    protected
        function GetOwner: TPersistent; override;
        function GetItems(index: Integer): TGLSLTextureEmitterItem;
        procedure RemoveNotification(aComponent: TComponent);
    public
        procedure AddEmitter(texEmitter: TGLSLTextureEmitter);
        property Items[index: Integer]: TGLSLTextureEmitterItem read GetItems; default;
    end;

    // TGLSLProjectedTextures
    //
    {: Projected Texture Manager.<p>
       Specifies active Emitters and receivers (children of this object).
       At the moment, only 1 texture can be used.}
    TGLSLProjectedTextures = class(TGLSceneObject)
    private
        FEmitters: TGLSLTextureEmitters;
        FUseLightmaps: boolean;
        Shader: TGLProgramHandle;
        FAmbient: TGLColor;
        procedure SetupShader;
    protected
        ShaderChanged: boolean;
        procedure SetUseLightmaps(val: boolean);
    public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        procedure DoRender(var rci: TRenderContextInfo;
            renderSelf, renderChildren: Boolean); override;
        procedure StructureChanged; override;
    published
        {: List of emitters. }
        property Emitters: TGLSLTextureEmitters read FEmitters write FEmitters;

        //Ambient is use if no lightmap..
        property Ambient: TGLColor read fAmbient write fAmbient;
        property UseLightmaps: boolean read FUseLightmaps write SetUseLightmaps;
    end;



implementation

// ------------------
// ------------------ TGLSLTextureEmitter ------------------
// ------------------

// Create
//

constructor TGLSLTextureEmitter.Create(aOwner: TComponent);
begin
    inherited Create(aOwner);
    FFOV := 90;
    FAspect := 1;
    FStyle := ptsLight;
    FAllowReverseProjection := false;
    FUseAttenuation := false;
    FAttenuation := 100;
    FBrightness := 1;
    FColor := TGLColor.create(self);
    FColor.SetColor(1, 1, 1);
    FColor.OnNotifyChange := ColorNotifyChange;
end;

destructor TGLSLTextureEmitter.Destroy;
begin
    FColor.free;
    inherited;
end;
// SetupTexMatrix
//

procedure TGLSLTextureEmitter.DoRender(var rci: TRenderContextInfo;
    renderSelf, renderChildren: boolean);
begin
    glGetFloatv(GL_MODELVIEW_MATRIX, @invmatrix[0][0]);
    InvertMatrix(invmatrix);
    inherited;
end;

procedure TGLSLTextureEmitter.SetupTexMatrix;
begin
    glMatrixMode(GL_TEXTURE);
    glpushmatrix;
    glLoadIdentity;

    // First scale and bias into [0..1] range.
    glTranslatef(0.5, 0.5, 0);
    glScalef(0.5, 0.5, 1);

    // Then set the projector's "perspective" (i.e. the "spotlight cone"):.
    gluPerspective(FFOV, FAspect, 0.1, 1);

    //  glMultMatrixf(PGLFloat(invabsolutematrixasaddress));
    glGetFloatv(GL_TEXTURE_MATRIX, @TexMatrix[0][0]);
    glpopmatrix;
    glMatrixMode(GL_MODELVIEW);
end;

procedure TGLSLTextureEmitter.ColorNotifyChange(sender: TObject);
begin
    if assigned(ProjectedTexturesObject) then
        ProjectedTexturesObject.ShaderChanged := true;
end;

procedure TGLSLTextureEmitter.SetAllowReverseProjection(val: boolean);
begin
    FAllowReverseProjection := val;
    if assigned(ProjectedTexturesObject) then
        ProjectedTexturesObject.ShaderChanged := true;
end;

procedure TGLSLTextureEmitter.SetUseAttenuation(val: boolean);
begin
    FUseAttenuation := val;
    if assigned(ProjectedTexturesObject) then
        ProjectedTexturesObject.ShaderChanged := true;
end;

procedure TGLSLTextureEmitter.SetAttenuation(val: single);
begin
    FAttenuation := val;
    if assigned(ProjectedTexturesObject) then
        ProjectedTexturesObject.ShaderChanged := true;
end;

procedure TGLSLTextureEmitter.SetBrightness(val: single);
begin
    FBrightness := val;
    if assigned(ProjectedTexturesObject) then
        ProjectedTexturesObject.ShaderChanged := true;
end;

procedure TGLSLTextureEmitter.SetColor(val: TGLColor);
begin
    FColor := val;
    if assigned(ProjectedTexturesObject) then
        ProjectedTexturesObject.ShaderChanged := true;
end;

procedure TGLSLTextureEmitter.SetStyle(val: TGLSLProjectedTexturesStyle);
begin
    FStyle := val;
    if assigned(ProjectedTexturesObject) then
        ProjectedTexturesObject.ShaderChanged := true;
end;

procedure TGLSLTextureEmitter.SetAspect(val: single);
begin
    FAspect := val;
    TexMatrixChanged := true;
end;

procedure TGLSLTextureEmitter.SetFOV(val: single);
begin
    FFOV := Val;
    TexMatrixChanged := true;
end;
// ------------------
// ------------------ TGLSLTextureEmitterItem ------------------
// ------------------

// Create
//

constructor TGLSLTextureEmitterItem.Create(Collection: TCollection);
begin
    inherited Create(Collection);
end;

// Assign
//

procedure TGLSLTextureEmitterItem.Assign(Source: TPersistent);
begin
    if Source is TGLSLTextureEmitterItem then
    begin
        FEmitter := TGLSLTextureEmitterItem(Source).FEmitter;
        TGLSLProjectedTextures(TGLSLTextureEmitters(Collection).GetOwner).StructureChanged;
    end;
    inherited;
end;

// SetCaster
//

procedure TGLSLTextureEmitterItem.SetEmitter(const val: TGLSLTextureEmitter);
begin
    if FEmitter <> val then
    begin
        FEmitter := val;
        TGLSLProjectedTextures(TGLSLTextureEmitters(Collection).GetOwner).StructureChanged;
    end;
end;

// RemoveNotification
//

procedure TGLSLTextureEmitterItem.RemoveNotification(aComponent: TComponent);
begin
    if aComponent = FEmitter then
        FEmitter := nil;
end;

// GetDisplayName
//

function TGLSLTextureEmitterItem.GetDisplayName: string;
begin
    if Assigned(FEmitter) then
    begin
        Result := '[Emitter] ' + FEmitter.Name;
    end
    else
        Result := 'nil';
end;

// ------------------
// ------------------ TGLSLTextureEmitters ------------------
// ------------------

// GetOwner
//

function TGLSLTextureEmitters.GetOwner: TPersistent;
begin
    Result := FOwner;
end;

// GetItems
//

function TGLSLTextureEmitters.GetItems(index: Integer): TGLSLTextureEmitterItem;
begin
    Result := TGLSLTextureEmitterItem(inherited Items[index]);
end;

// RemoveNotification
//

procedure TGLSLTextureEmitters.RemoveNotification(aComponent: TComponent);
var
    i: Integer;
begin
    for i := 0 to Count - 1 do
    begin
        Items[i].RemoveNotification(aComponent);
        TGLSLProjectedTextures(GetOwner).shaderChanged := true;
    end;
end;

// AddEmitter
//

procedure TGLSLTextureEmitters.AddEmitter(texEmitter: TGLSLTextureEmitter);
var
    item: TGLSLTextureEmitterItem;
begin
    item := TGLSLTextureEmitterItem(self.Add);
    item.Emitter := texEmitter;
    item.Emitter.ProjectedTexturesObject := TGLSLProjectedTextures(GetOwner);
    TGLSLProjectedTextures(GetOwner).shaderChanged := true;
end;

// ------------------
// ------------------ TGLSLProjectedTextures ------------------
// ------------------

// Create
//

constructor TGLSLProjectedTextures.Create(AOwner: TComponent);
begin
    inherited Create(aOWner);
    FEmitters := TGLSLTextureEmitters.Create(TGLSLTextureEmitterItem);
    FEmitters.FOwner := self;
    Shader := TGLProgramHandle.Create;
    FUseLightmaps := false;
    ShaderChanged := true;
    Ambient := TGLColor.Create(self);
    ambient.SetColor(0.5, 0.5, 0.5, 0.5);
end;

// Destroy
//

destructor TGLSLProjectedTextures.Destroy;
begin
    Shader.free;
    FEmitters.Free;
    Ambient.Free;

    inherited destroy;
end;

procedure TGLSLProjectedTextures.SetUseLightmaps(val: boolean);
begin
    FUseLightmaps := val;
    ShaderChanged := true;
end;

procedure TGLSLProjectedTextures.SetupShader;
var
    vp, fp: TStringlist;
    i: integer;
    emitter: TGLSLTextureEmitter;
    OldSeparator: char;
begin
    OldSeparator := sysutils.DecimalSeparator;
    DecimalSeparator := '.';

    if shader.Handle <> 0 then
        shader.DestroyHandle;
    shader.AllocateHandle;

    vp := TStringlist.create;
    fp := TStringlist.create;

    //define the vertex program
    if emitters.count > 0 then
    begin
        for i := 0 to emitters.count - 1 do
        begin
            emitter := Emitters[i].Emitter;
            if not assigned(emitter) then continue;
            if not emitter.Visible then continue;
            vp.add(format('uniform mat4 InvModelViewMatrix%d;', [i]));
            vp.add(format('uniform mat4 TextureMatrix%d;', [i]));
            vp.add(format('varying vec4 ProjTexCoords%d;', [i]));
        end;
    end;

    vp.add('void main(){');
    vp.add('vec4 P = gl_Vertex;');
    vp.add('gl_Position = gl_ModelViewProjectionMatrix * P;');
    vp.add('vec4 Pe = gl_ModelViewMatrix * P;');

    vp.add('gl_TexCoord[0] = gl_TextureMatrix[0] * gl_MultiTexCoord0;');

    if UseLightmaps then
        vp.add('gl_TexCoord[1] = gl_TextureMatrix[1] * gl_MultiTexCoord1;');
    if emitters.count > 0 then
    begin
        vp.add('vec4 Pinv;');
        for i := 0 to emitters.count - 1 do
        begin
            emitter := Emitters[i].Emitter;
            if not assigned(emitter) then continue;
            if not emitter.Visible then continue;
            vp.add(format('Pinv = InvModelViewMatrix%d * Pe;', [i]));
            vp.add(format('ProjTexCoords%d = TextureMatrix%d * Pinv;', [i, i]));
        end;
    end;
    vp.add('}');

    //define the fragment program
    fp.add('uniform sampler2D TextureMap;');
    if UseLightmaps then
        fp.add('uniform sampler2D LightMap;');
    if emitters.count > 0 then
    begin
        fp.add('uniform sampler2D ProjMap;');

        for i := 0 to emitters.count - 1 do
        begin
            emitter := Emitters[i].Emitter;
            if not assigned(emitter) then continue;
            if not emitter.Visible then continue;
            fp.add(format('varying vec4 ProjTexCoords%d;', [i]));
            if Emitter.UseAttenuation then
                fp.add(format('uniform float Attenuation%d;', [i]));
        end;
    end;

    fp.add('void main(){');
    fp.add('vec3 color = texture2D(TextureMap, gl_TexCoord[0].st).rgb;');
    if UseLightmaps then
        fp.add('vec3 light = texture2D(LightMap, gl_TexCoord[1].st).rgb;')
    else
        fp.add(format('vec3 light = vec3(%.4f, %.4f, %.4f);', [Ambient.Red, ambient.Green, ambient.Blue]));

    if emitters.count > 0 then
    begin
        fp.add('vec3 projlight = vec3(0, 0, 0);');
        fp.add('vec3 projshadow = vec3(0, 0, 0);');
        fp.add('vec3 temp;');
        fp.add('float dist;');
        fp.add('vec3 col;');
        for i := 0 to emitters.count - 1 do
        begin
            emitter := Emitters[i].Emitter;
            if not assigned(emitter) then continue;
            if not emitter.Visible then continue;
            if not emitter.AllowReverseProjection then
                fp.add(format('if (ProjTexCoords%d.q<0.0){', [i]));
            with emitter.color do
                fp.add(format('col = vec3(%.4f,%.4f,%.4f);', [red, green, blue]));
            case emitter.Style of
                ptslight:
                    fp.add(format('projlight+= (texture2DProj(ProjMap, ProjTexCoords%d).rgb*col*%.4f);', [i, emitter.brightness]));
                ptsShadow:
                    fp.add(format('projshadow+= (texture2DProj(ProjMap, ProjTexCoords%d).rgb*col*%.4f);', [i, emitter.brightness]));
            end;

            if emitter.UseAttenuation then
            begin
                fp.add(format('dist = 1.0 - clamp(ProjTexCoords%d.q / %.4f, 0.0, 1.0);', [i, emitter.Attenuation]));
                case emitter.Style of
                    ptslight:
                        fp.add('projlight *= dist;');
                    ptsShadow:
                        fp.add('projshadow *= dist;');
                end;
            end;

            if not emitter.AllowReverseProjection then
                fp.add('}');
        end;

        fp.add('projlight = clamp(projlight,0.0,1.2);');
        fp.add('projshadow = clamp(projshadow,0.0,0.8);');

        fp.add('vec3 totlight = 1.0-((( 1.0-projlight)*( 1.0-light)) +(projshadow*light)) ;');
    end
    else
        fp.add('vec3 totlight = light;');

    fp.add('gl_FragColor = vec4(1.5*totlight *color , 1);}');

    // vp.SaveToFile('c:\vp.txt');
    // fp.SaveToFile('c:\fp.txt');
    Shader.AddShader(TGLVertexShaderHandle, vp.Text, True);
    Shader.AddShader(TGLFragmentShaderHandle, fp.Text, True);

    vp.free;
    fp.free;
    DecimalSeparator := OldSeparator;
    if not Shader.LinkProgram then
        raise Exception.Create(Shader.InfoLog);
    if not Shader.ValidateProgram then
        raise Exception.Create(Shader.InfoLog);
end;

// DoRender
//

procedure TGLSLProjectedTextures.DoRender(var rci: TRenderContextInfo;
    renderSelf, renderChildren: boolean);
var
    i: integer;
    emitter: TGLSLTextureEmitter;
    m: TMatrix;
begin
    if not (renderSelf or renderChildren) then Exit;
    if (csDesigning in ComponentState) then
    begin
        inherited;
        Exit;
    end;

    if ShaderChanged then
    begin
        SetupShader;
        ShaderChanged := false;
    end;

    Shader.UseProgramObject;

    for i := 0 to Emitters.Count - 1 do
    begin
        emitter := Emitters[i].Emitter;
        if not assigned(emitter) then continue;
        if not emitter.Visible then continue;
        if emitter.TexMatrixChanged then
        begin
            emitter.setupTexMatrix;
            emitter.TexMatrixChanged := false;
        end;

        m := emitter.InvAbsoluteMatrix;

        Shader.Uniformmatrix4fv['InvModelViewMatrix' + inttostr(i)] := {emitter.invabsolutematrix;} emitter.InvMatrix;
        Shader.Uniformmatrix4fv['TextureMatrix' + inttostr(i)] := emitter.texMatrix;
    end;

    Shader.Uniform1i['TextureMap'] := 0;

    if UseLightmaps then
        Shader.Uniform1i['LightMap'] := 1;

    if emitters.count > 0 then
        Shader.Uniform1i['ProjMap'] := 2;

    glActiveTextureARB(GL_TEXTURE2_ARB);
    glBindTexture(GL_TEXTURE_2D, Material.Texture.Handle);
    glActiveTextureARB(GL_TEXTURE0_ARB);

    self.RenderChildren(0, Count - 1, rci);

    Shader.EndUseProgramObject;
end;

procedure TGLSLProjectedTextures.StructureChanged;
begin
    inherited;
    shaderchanged := true;
end;

initialization

    RegisterClasses([TGLSLTextureEmitter, TGLSLProjectedTextures]);

end.

