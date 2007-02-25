{: glmultimaterialshader<p>

   A shader that applies a render pass for each material in
   its assigned MaterialLibrary.<p>

      $Log: glmultimaterialshader.pas,v $
      Revision 1.1  2006/01/10 20:50:46  z0m3ie
      recheckin to make shure that all is lowercase

      Revision 1.1  2006/01/09 21:02:34  z0m3ie
      *** empty log message ***

      Revision 1.3  2005/12/04 16:53:04  z0m3ie
      renamed everything to lowercase to get better codetools support and avoid unit finding bugs

      Revision 1.2  2005/08/03 00:41:39  z0m3ie
      - added automatical generated History from CVS

   <b>History : </b><font size=-1><ul>
      <li>24/05/04 - Mrqzzz - Re-added design-time rendering option
                          (seems stable now)
      <li>29/07/03 - SG - Removed design-time rendering option
                          (shader unstable at design-time)
      <li>29/07/03 - SG - Creation
   </ul></font>
}
unit glmultimaterialshader;

interface

uses
   classes, gltexture, opengl1x;

type
   TGLMultiMaterialShader = class(TGLShader)
      private
         FPass : Integer;
         FMaterialLibrary : TGLMaterialLibrary;
         FVisibleAtDesignTime: boolean;
         FShaderActiveAtDesignTime : boolean;
    procedure SetVisibleAtDesignTime(const Value: boolean);
      protected
         procedure SetMaterialLibrary(const val : TGLMaterialLibrary);
         procedure DoApply(var rci : TRenderContextInfo; Sender : TObject); override;
         function DoUnApply(var rci : TRenderContextInfo) : Boolean; override;
      public
         constructor Create(aOwner : TComponent); override;
      published
         property MaterialLibrary : TGLMaterialLibrary read FMaterialLibrary write SetMaterialLibrary;
         property VisibleAtDesignTime : boolean read FVisibleAtDesignTime write SetVisibleAtDesignTime;
   end;

procedure Register;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

procedure Register;
begin
   RegisterComponents('GLScene Shaders', [TGLMultiMaterialShader]);
end;

// ------------------
// ------------------ TGLMultiMaterialShader ------------------
// ------------------

// Create
//
constructor TGLMultiMaterialShader.Create(aOwner : TComponent);
begin
   inherited;
   ShaderStyle:=ssReplace;
   FVisibleAtDesignTime := False;
end;

// DoApply
//
procedure TGLMultiMaterialShader.DoApply(var rci: TRenderContextInfo; Sender : TObject);
begin
   if not Assigned(FMaterialLibrary) then exit;

   FShaderActiveAtDesignTime := FVisibleAtDesignTime;

   FPass:=1;
   if (not (csDesigning in ComponentState)) or FShaderActiveAtDesignTime then begin
      glPushAttrib(GL_ALL_ATTRIB_BITS);
      glEnable(GL_DEPTH_TEST);
      glDepthFunc(GL_LEQUAL);
      if FMaterialLibrary.Materials.Count>0 then
         FMaterialLibrary.Materials[0].Apply(rci);
  end;
end;

// DoUnApply
//
function TGLMultiMaterialShader.DoUnApply(
   var rci: TRenderContextInfo): Boolean;
begin
   Result:=False;
   if not Assigned(FMaterialLibrary) then exit;
   if (not (csDesigning in ComponentState)) or FShaderActiveAtDesignTime then begin
      if FMaterialLibrary.Materials.Count>0 then
         FMaterialLibrary.Materials[FPass-1].UnApply(rci);
      if (FPass >= FMaterialLibrary.Materials.Count) then begin
         glDepthFunc(GL_LESS);
         glPopAttrib;
         exit;
      end;
      FMaterialLibrary.Materials[FPass].Apply(rci);
      Result:=True;
      Inc(FPass);
   end;
end;

// SetMaterialLibrary
//
procedure TGLMultiMaterialShader.SetMaterialLibrary(
   const val: TGLMaterialLibrary);
begin
   if val<>FMaterialLibrary then begin
      FMaterialLibrary:=val;
      NotifyChange(Self);
   end;
end;

procedure TGLMultiMaterialShader.SetVisibleAtDesignTime(
  const Value: boolean);
begin
  FVisibleAtDesignTime := Value;
  if csDesigning in ComponentState then
     NotifyChange(Self);
end;

end.
