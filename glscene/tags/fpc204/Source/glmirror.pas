// glmirror
{: implements a basic, stencil-based mirror (as in mark kilgard's demo).<p>

   It is strongly recommended to read and understand the explanations in the
   materials/mirror demo before using this component.<p>

      $Log: glmirror.pas,v $
      Revision 1.1  2006/01/10 20:50:45  z0m3ie
      recheckin to make shure that all is lowercase

      Revision 1.3  2006/01/09 20:45:49  z0m3ie
      *** empty log message ***

      Revision 1.2  2005/12/04 16:53:05  z0m3ie
      renamed everything to lowercase to get better codetools support and avoid unit finding bugs

      Revision 1.1  2005/12/01 21:24:11  z0m3ie
      *** empty log message ***

      Revision 1.3  2005/08/03 00:41:39  z0m3ie
      - added automatical generated History from CVS

	<b>History : </b><font size=-1><ul>
      <li>18/07/04 - Orlando - added custom shapes
      <li>13/02/03 - DanB - added TGLMirror.AxisAlignedDimensionsUnscaled
      <li>13/11/02 - EG - Fixed TGLMirror.DoRender transform
      <li>06/11/02 - EG - Fixed Stencil setup
      <li>30/10/02 - EG - Added OnBegin/EndRenderingMirrors
      <li>25/10/02 - EG - Fixed Stencil cleanup
      <li>22/02/01 - EG - Fixed change notification,
                          Fixed special effects support (PFX, etc.) 
      <li>07/12/01 - EG - Creation
   </ul></font>
}
unit glmirror;

interface

uses classes, glscene, vectorgeometry, opengl1x, glmisc, gltexture;

type

   // TMirrorOptions
   //
   tmirroroption = (mousestencil, moopaque, momirrorplaneclip, moclearzbuffer);
   tmirroroptions = set of tmirroroption;

const
   cdefaultmirroroptions = [mousestencil];

type

   // TMirrorShapes  ORL
   TMirrorShapes = (msRect, msDisk);

   // TGLMirror
   //
   {: A simple plane mirror.<p>
      This mirror requires a stencil buffer for optimal rendering!<p>
      The object is a mix between a plane and a proxy object, in that the plane
      defines the mirror's surface, while the proxy part is used to reference
      the objects that should be mirrored (it is legal to self-mirror, but no
      self-mirror visuals will be rendered).<p>
      It is strongly recommended to read and understand the explanations in the
      materials/mirror demo before using this component. }
	TGLMirror = class (TGLSceneObject)
	   private
			{ Private Declarations }
         FRendering : Boolean;
         FMirrorObject : TGLBaseSceneObject;
			FWidth, FHeight : TGLFloat;
         FMirrorOptions : TMirrorOptions;
         FOnBeginRenderingMirrors, FOnEndRenderingMirrors : TNotifyEvent;

      FShape : TMirrorShapes; //ORL
      FRadius : TGLFloat; //ORL
      FSlices : TGLInt; //ORL

		protected
			{ Protected Declarations }
         procedure Notification(AComponent: TComponent; Operation: TOperation); override;
         procedure SetMirrorObject(const val : TGLBaseSceneObject);
         procedure SetMirrorOptions(const val : TMirrorOptions);
         procedure ClearZBufferArea;

		   procedure SetHeight(AValue: TGLFloat);
		   procedure SetWidth(AValue: TGLFloat);

       procedure SetRadius(const aValue : Single);  //ORL
       procedure SetSlices(const aValue : TGLInt);  //ORL
   		 procedure SetShape(aValue : TMirrorShapes);  //ORL
       function GetRadius : single;                 //ORL
       function GetSlices : TGLInt;                 //ORL

		public
			{ Public Declarations }
			constructor Create(AOwner: TComponent); override;

         procedure DoRender(var rci : TRenderContextInfo;
                            renderSelf, renderChildre : Boolean); override;
		   procedure BuildList(var rci : TRenderContextInfo); override;

		   procedure Assign(Source: TPersistent); override;
//         function AxisAlignedDimensions : TVector; override;
         function AxisAlignedDimensionsUnscaled : TVector; override;

		published
			{ Public Declarations }
         {: Selects the object to mirror.<p>
            If nil, the whole scene is mirrored. }
         property MirrorObject : TGLBaseSceneObject read FMirrorObject write SetMirrorObject;
         {: Controls rendering options.<p>
            <ul>
            <li>mousestencil: mirror area is stenciled, prevents reflected
               objects to be visible on the sides of the mirror (stencil buffer
               must be active in the viewer)
            <li>moopaque: mirror is opaque (ie. painted with background color)
            <li>momirrorplaneclip: a clipplane is defined to prevent reflections
               from popping out of the mirror (for objects behind or halfway through)
            <li>moclearzbuffer: mirror area's zbuffer is cleared so that background
               objects don't interfere with reflected objects (reflected objects
               must be rendered after the mirror in the hierarchy). works only
               along with stenciling.
            </ul>
         }
         property mirroroptions : tmirroroptions read fmirroroptions write setmirroroptions default cdefaultmirroroptions;

			property Height: TGLFloat read FHeight write SetHeight;
         property Width: TGLFloat read FWidth write SetWidth;

         {: Fired before the object's mirror images are rendered. }
         property OnBeginRenderingMirrors : TNotifyEvent read FOnBeginRenderingMirrors write FOnBeginRenderingMirrors;
         {: Fired after the object's mirror images are rendered. }
         property OnEndRenderingMirrors : TNotifyEvent read FOnEndRenderingMirrors write FOnEndRenderingMirrors;

      property Radius : TGLFloat read FRadius write SetRadius; //ORL
      property Slices : TGLInt read FSlices write SetSlices default 16; //ORL
			property Shape : TMirrorShapes read FShape write SetShape default msRect; //ORL
   end;

//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------
implementation
//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------

uses glstate;

// ------------------
// ------------------ TGLMirror ------------------
// ------------------

// Create
//
constructor TGLMirror.Create(AOwner:Tcomponent);
begin
   inherited Create(AOwner);
   FWidth:=1;
   FHeight:=1;
   FMirrorOptions:=cDefaultMirrorOptions;
   ObjectStyle:=ObjectStyle+[osDirectDraw];
   Material.FrontProperties.Diffuse.Initialize(VectorMake(1, 1, 1, 0.1));
   Material.BlendingMode:=bmTransparency;

   FRadius:=1; //ORL
   FSlices:=16; //ORL
   Shape:=msRect; //ORL
end;

// DoRender
//
procedure TGLMirror.DoRender(var rci : TRenderContextInfo;
                          renderSelf, renderChildre : Boolean);
var
   oldProxySubObject : Boolean;
   refMat, curMat : TMatrix;
   clipPlane : TDoubleHmgPlane;
   bgColor : TColorVector;
   cameraPosBackup, cameraDirectionBackup : TVector;
begin
   if FRendering then Exit;
   FRendering:=True;
   try
      oldProxySubObject:=rci.proxySubObject;
      rci.proxySubObject:=True;

      if VectorDotProduct(VectorSubtract(rci.cameraPosition, AbsolutePosition), AbsoluteDirection)>0 then begin

         glPushAttrib(GL_ENABLE_BIT);

         // "Render" stencil mask
         if MirrorOptions<>[] then begin
            if (mousestencil in mirroroptions) then begin
               glclearstencil(0);
               glclear(gl_stencil_buffer_bit);
               glenable(gl_stencil_test);
               glstencilfunc(gl_always, 1, 1);
               glstencilop(gl_replace, gl_zero, gl_replace);
            end;
            if (moopaque in mirroroptions) then begin
               bgcolor:=convertwincolor(scene.currentbuffer.backgroundcolor);
               rci.glstates.setglmaterialcolors(gl_front, bgcolor, clrblack, clrblack, clrblack, 0);
               rci.glstates.unsetglstate(sttexture2d);
            end else begin
               glcolormask(false, false, false, false);
            end;
            gldepthmask(false);

            BuildList(rci);

            glDepthMask(True);
            if (mousestencil in mirroroptions) then begin
               glstencilfunc(gl_equal, 1, 1);
               glstencilop(gl_keep, gl_keep, gl_keep);
            end;

            if (moClearZBuffer in MirrorOptions) then
               ClearZBufferArea;

            if not (moOpaque in MirrorOptions) then
               glColorMask(True, True, True, True);
         end;

         // Mirror lights
         glPushMatrix;
         glLoadMatrixf(@Scene.CurrentBuffer.ModelViewMatrix);
         refMat:=MakeReflectionMatrix(AffineVectorMake(AbsolutePosition),
                                      AffineVectorMake(AbsoluteDirection));
         glMultMatrixf(@refMat);
         Scene.SetupLights(Scene.CurrentBuffer.LimitOf[limLights]);

         // mirror geometry and render master
         glGetFloatv(GL_MODELVIEW_MATRIX, @curMat);
         glLoadMatrixf(@Scene.CurrentBuffer.ModelViewMatrix);
         Scene.CurrentBuffer.PushModelViewMatrix(curMat);

         glDisable(GL_CULL_FACE);
         glEnable(GL_NORMALIZE);

         if moMirrorPlaneClip in MirrorOptions then begin
            glEnable(GL_CLIP_PLANE0);
            SetPlane(clipPlane, PlaneMake(AffineVectorMake(AbsolutePosition),
                                          VectorNegate(AffineVectorMake(AbsoluteDirection))));
            glClipPlane(GL_CLIP_PLANE0, @clipPlane);
         end;

         cameraPosBackup:=rci.cameraPosition;
         cameraDirectionBackup:=rci.cameraDirection;
         rci.cameraPosition:=VectorTransform(rci.cameraPosition, refMat);
         rci.cameraDirection:=VectorTransform(rci.cameraDirection, refMat);

         glMultMatrixf(@refMat);

         if Assigned(FOnBeginRenderingMirrors) then
            FOnBeginRenderingMirrors(Self);
         if Assigned(FMirrorObject) then begin
            if FMirrorObject.Parent<>nil then
               glMultMatrixf(PGLFloat(FMirrorObject.Parent.AbsoluteMatrixAsAddress));
            glMultMatrixf(PGLFloat(FMirrorObject.LocalMatrix));
            FMirrorObject.DoRender(rci, renderSelf, FMirrorObject.Count>0);
         end else begin
            Scene.Objects.DoRender(rci, renderSelf, True);
         end;
         if Assigned(FOnBeginRenderingMirrors) then
            FOnBeginRenderingMirrors(Self);

         rci.cameraPosition:=cameraPosBackup;
         rci.cameraDirection:=cameraDirectionBackup;

         // Restore to "normal"
         Scene.CurrentBuffer.PopModelViewMatrix;
         glLoadMatrixf(@Scene.CurrentBuffer.ModelViewMatrix);
         Scene.SetupLights(Scene.CurrentBuffer.LimitOf[limLights]);

         glPopMatrix;
         glPopAttrib;
         rci.GLStates.ResetGLMaterialColors;
         rci.GLStates.ResetGLCurrentTexture;

         rci.proxySubObject:=oldProxySubObject;

         // start rendering self
         if renderSelf then begin
            Material.Apply(rci);
            repeat
               BuildList(rci);
            until not Material.UnApply(rci);
         end;
         
      end;
      
      if renderChildre then
         Self.RenderChildren(0, Count-1, rci);

      if Assigned(FMirrorObject) then
         FMirrorObject.Effects.RenderPostEffects(Scene.CurrentBuffer, rci);
   finally
      FRendering:=False;
   end;
end;

// BuildList
//
procedure TGLMirror.BuildList(var rci : TRenderContextInfo);
var
   hw, hh : TGLFloat;
   quadric : PGLUquadricObj;
begin
  if msRect = FShape then
  begin
    hw:=FWidth*0.5;
    hh:=FHeight*0.5;
    glNormal3fv(@ZVector);
    glBegin(GL_QUADS);
      glVertex3f( hw,  hh, 0);
      glVertex3f(-hw,  hh, 0);
      glVertex3f(-hw, -hh, 0);
      glVertex3f( hw, -hh, 0);
    glEnd;
  end
  else
  begin
	  quadric:=gluNewQuadric;
  	gluDisk(Quadric, 0, FRadius, FSlices, 1);  //radius. slices, loops
  end;
end;

// BuildList
//
procedure TGLMirror.ClearZBufferArea;
var
   worldMat : TMatrix;
   p : TAffineVector;
begin
   with Scene.CurrentBuffer do begin
      glPushMatrix;
      worldMat:=Self.AbsoluteMatrix;
      glMatrixMode(GL_PROJECTION);
      glPushMatrix;
      glLoadIdentity;
      glOrtho(0, Width, 0, Height, 1, -1);
      glMatrixMode(GL_MODELVIEW);
      glLoadIdentity;

      glDepthFunc(GL_ALWAYS);
      glColorMask(False, False, False, False);

      glBegin(GL_QUADS);
         p:=WorldToScreen(VectorTransform(AffineVectorMake(Self.Width*0.5, Self.Height*0.5, 0), worldMat));
         glVertex3f(p[0], p[1], 0.999);
         p:=WorldToScreen(VectorTransform(AffineVectorMake(-Self.Width*0.5, Self.Height*0.5, 0), worldMat));
         glVertex3f(p[0], p[1], 0.999);
         p:=WorldToScreen(VectorTransform(AffineVectorMake(-Self.Width*0.5, -Self.Height*0.5, 0), worldMat));
         glVertex3f(p[0], p[1], 0.999);
         p:=WorldToScreen(VectorTransform(AffineVectorMake(Self.Width*0.5, -Self.Height*0.5, 0), worldMat));
         glVertex3f(p[0], p[1], 0.999);
      glEnd;

      glColorMask(True, True, True, True);
      glDepthFunc(GL_LESS);
      
      glMatrixMode(GL_PROJECTION);
      glPopMatrix;
      glMatrixMode(GL_MODELVIEW);
      glPopMatrix;
   end;
end;

// Notification
//
procedure TGLMirror.Notification(AComponent: TComponent; Operation: TOperation);
begin
   if (Operation = opRemove) and (AComponent = FMirrorObject) then
      MirrorObject:=nil;
   inherited;
end;

// SetMirrorObject
//
procedure TGLMirror.SetMirrorObject(const val : TGLBaseSceneObject);
begin
   if FMirrorObject<>val then begin
      if Assigned(FMirrorObject) then
         FMirrorObject.RemoveFreeNotification(Self);
      FMirrorObject:=val;
      if Assigned(FMirrorObject) then
         FMirrorObject.FreeNotification(Self);
      NotifyChange(Self);
   end;
end;

// SetWidth
//
procedure TGLMirror.SetWidth(AValue : TGLFloat);
begin
   if AValue<>FWidth then begin
      FWidth:=AValue;
      NotifyChange(Self);
   end;
end;

// SetHeight
//
procedure TGLMirror.SetHeight(AValue : TGLFloat);
begin
   if AValue<>FHeight then begin
      FHeight:=AValue;
      NotifyChange(Self);
   end;
end;

// Assign
//
procedure TGLMirror.Assign(Source: TPersistent);
begin
   if Assigned(Source) and (Source is TGLMirror) then begin
      FWidth:=TGLMirror(Source).FWidth;
      FHeight:=TGLMirror(Source).FHeight;
      FMirrorOptions:=TGLMirror(Source).FMirrorOptions;
      MirrorObject:=TGLMirror(Source).MirrorObject;
   end;
   inherited Assign(Source);
end;
{
// AxisAlignedDimensions
//
function TGLMirror.AxisAlignedDimensions: TVector;
begin
   Result:=VectorMake(0.5*Abs(FWidth)*Scale.DirectX,
                      0.5*Abs(FHeight)*Scale.DirectY, 0);
end;
}
// AxisAlignedDimensions
//
function TGLMirror.AxisAlignedDimensionsUnscaled: TVector;
begin
   Result:=VectorMake(0.5*Abs(FWidth),
                      0.5*Abs(FHeight), 0);
end;


// SetMirrorOptions
//
procedure TGLMirror.SetMirrorOptions(const val : TMirrorOptions);
begin
   if FMirrorOptions<>val then begin
      FMirrorOptions:=val;
      NotifyChange(Self);
   end;
end;

//ORL add-ons

// SetRadius
//
procedure TGLMirror.SetRadius(const aValue : Single);
begin
   if aValue<>FRadius then begin
      FRadius:=aValue;
      StructureChanged;
   end;
end;

// GetRadius
//
function TGLMirror.GetRadius: single;
begin
  result := FRadius;
end;

// SetSlices
//
procedure TGLMirror.SetSlices(const aValue : TGLInt);
begin
  if aValue<>FSlices then
  begin
    if aValue>2 then
      FSlices:=aValue;
      StructureChanged;
   end
   else
   begin
   end;
end;

// GetSlices
//
function TGLMirror.GetSlices: TGLInt;
begin
  result := FSlices;
end;

// SetShape
//
procedure TGLMirror.SetShape(aValue: TMirrorShapes);
begin
   if aValue<>FShape then begin
      FShape:=aValue;
      StructureChanged;
   end;
end;

//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------
initialization
//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------

   RegisterClasses([TGLMirror]);

end.
