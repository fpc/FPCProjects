// glproxyobjects
{: implements specific proxying classes.<p>

      $Log: glproxyobjects.pas,v $
      Revision 1.1  2006/01/10 20:50:45  z0m3ie
      recheckin to make shure that all is lowercase

      Revision 1.3  2006/01/09 20:45:50  z0m3ie
      *** empty log message ***

      Revision 1.2  2005/12/04 16:53:05  z0m3ie
      renamed everything to lowercase to get better codetools support and avoid unit finding bugs

      Revision 1.1  2005/12/01 21:24:11  z0m3ie
      *** empty log message ***

      Revision 1.3  2005/08/03 00:41:39  z0m3ie
      - added automatical generated History from CVS

	<b>History : </b><font size=-1><ul>
      <li>18/12/03 - Dave - Dropped "Object" from "ProxyObject" class names
      <li>17/12/03 - Dave - Changed class check in Octree code to Assert
      <li>17/12/03 - Dave+Dan - Added OctreeSphereSweep
      <li>06/12/03 - EG - Creation from GLScene.pas split
   </ul></font>
}
unit glproxyobjects;

interface

uses classes, glscene, vectorgeometry, glmisc, gltexture, glsilhouette,
   glvectorfileobjects;

type

   // TGLColorProxy
   //
   {: A proxy object with its own color.<p>
      This proxy object can have a unique color. Note that multi-material
      objects (Freeforms linked to a material library f.i.) won't honour
      the color. }
   TGLColorProxy = class (TGLProxyObject)
      private
         { Private Declarations }
         FFrontColor: TGLFaceProperties;

      public
         { Public Declarations }
         constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;

         procedure DoRender(var rci : TRenderContextInfo;
                            renderSelf, renderChildre : Boolean); override;
      published
         { Published Declarations }
         property FrontColor: TGLFaceProperties read FFrontColor;
   end;

   // TGLFreeFormProxy
   //
   {: A proxy object specialized for FreeForms.<p> }
   TGLFreeFormProxy = class (TGLProxyObject)
      protected
         { Protected Declarations }
         procedure SetMasterObject(const val : TGLBaseSceneObject); override;

      public
         { Public Declarations }

         {: If the MasterObject is a FreeForm, you can raycast against the Octree,
            which is alot faster.  You must build the octree before using. }
         function OctreeRayCastIntersect(const rayStart, rayVector : TVector;
                                         intersectPoint : PVector = nil;
                                         intersectNormal : PVector = nil) : Boolean;
         {: WARNING: This function is not yet 100% reliable with scale+rotation. }
        function OctreeSphereSweepIntersect(const rayStart, rayVector : TVector;
                                        const velocity, radius, modelscale: Single;
                                        intersectPoint : PVector = nil;
                                        intersectNormal : PVector = nil) : Boolean;
   end;

//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------
implementation
//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------

uses sysutils,opengl1x;

// ------------------
// ------------------ TGLColorProxy ------------------
// ------------------

// Create
//
constructor TGLColorProxy.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFrontColor:=TGLFaceProperties.Create(Self);
end;

// Destroy
//
destructor TGLColorProxy.Destroy;
begin
   FFrontColor.Free;

   inherited Destroy;
end;

// Render
//
procedure TGLColorProxy.DoRender(var rci : TRenderContextInfo;
                                  renderSelf, renderChildre : Boolean);
var
   gotMaster, masterGotEffects, oldProxySubObject : Boolean;
begin
   if FRendering then Exit;
   FRendering:=True;
   try
      gotMaster:=Assigned(MasterObject);
      masterGotEffects:=gotMaster and (pooEffects in ProxyOptions)
                        and (MasterObject.Effects.Count>0);
      if gotMaster then begin
         if pooObjects in ProxyOptions then begin
            oldProxySubObject:=rci.proxySubObject;
            rci.proxySubObject:=True;
            if pooTransformation in ProxyOptions then
               glMultMatrixf(PGLFloat(MasterObject.MatrixAsAddress));
            TGLCustomSceneObject(MasterObject).Material.FrontProperties.Assign(FFrontColor);
            MasterObject.DoRender(rci, renderSelf, RenderChildre);
            rci.proxySubObject:=oldProxySubObject;
         end;
      end;
      // now render self stuff (our children, our effects, etc.)
      if renderChildre and (Count>0) then
         Self.RenderChildren(0, Count-1, rci);
      if masterGotEffects then
         MasterObject.Effects.RenderPostEffects(Scene.CurrentBuffer, rci);
   finally
      FRendering:=False;
   end;
   ClearStructureChanged;
end;

// ------------------
// ------------------ TGLFreeFormProxy ------------------
// ------------------

// SetMasterObject
//
procedure TGLFreeFormProxy.SetMasterObject(const val : TGLBaseSceneObject);
begin
   if Assigned(val) and not (val is TGLFreeForm) then
      raise Exception.Create(ClassName+' accepts only FreeForms as master!');
   inherited;
end;

// OctreeRayCastIntersect
//
function TGLFreeFormProxy.OctreeRayCastIntersect(const rayStart, rayVector : TVector;
                                 intersectPoint : PVector = nil;
                                 intersectNormal : PVector = nil) : Boolean;
var
   localRayStart, localRayVector : TVector;
begin
   if Assigned(MasterObject) then begin
      Assert(MasterObject is TGLFreeForm);
      SetVector(localRayStart, AbsoluteToLocal(rayStart));
      SetVector(localRayStart, MasterObject.LocalToAbsolute(localRayStart));
      SetVector(localRayVector, AbsoluteToLocal(rayVector));
      SetVector(localRayVector, MasterObject.LocalToAbsolute(localRayVector));
      NormalizeVector(localRayVector);

      Result:=TGLFreeForm(MasterObject).OctreeRayCastIntersect(localRayStart, localRayVector,
                                            intersectPoint, intersectNormal);
      if Result then begin
         if Assigned(intersectPoint) then begin
            SetVector(intersectPoint^, MasterObject.AbsoluteToLocal(intersectPoint^));
            SetVector(intersectPoint^, LocalToAbsolute(intersectPoint^));
         end;
         if Assigned(intersectNormal) then begin
            SetVector(intersectNormal^, MasterObject.AbsoluteToLocal(intersectNormal^));
            SetVector(intersectNormal^, LocalToAbsolute(intersectNormal^));
         end;
      end;
   end else Result:=False;
end;

// OctreeSphereSweepIntersect
//
function TGLFreeFormProxy.OctreeSphereSweepIntersect(const rayStart, rayVector : TVector;
                                        const velocity, radius, modelscale: Single;
                                        intersectPoint : PVector = nil;
                                        intersectNormal : PVector = nil) : Boolean;
var
   localRayStart, localRayVector : TVector;
   localVelocity, localRadius: single;
begin
  Result:=False;
   if Assigned(MasterObject) then begin
      Assert(MasterObject is TGLFreeForm);
      localVelocity := velocity * modelscale;
      localRadius := radius * modelscale;

      SetVector(localRayStart, AbsoluteToLocal(rayStart));
      SetVector(localRayStart, MasterObject.LocalToAbsolute(localRayStart));
      SetVector(localRayVector, AbsoluteToLocal(rayVector));
      SetVector(localRayVector, MasterObject.LocalToAbsolute(localRayVector));
      NormalizeVector(localRayVector);

      Result:=TGLFreeForm(MasterObject).OctreeSphereSweepIntersect(localRayStart, localRayVector,
                                            localVelocity, localRadius,
                                            intersectPoint, intersectNormal);
      if Result then begin
         if Assigned(intersectPoint) then begin
            SetVector(intersectPoint^, MasterObject.AbsoluteToLocal(intersectPoint^));
            SetVector(intersectPoint^, LocalToAbsolute(intersectPoint^));
         end;
         if Assigned(intersectNormal) then begin
            SetVector(intersectNormal^, MasterObject.AbsoluteToLocal(intersectNormal^));
            SetVector(intersectNormal^, LocalToAbsolute(intersectNormal^));
         end;
      end;

   end;
end;

//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------
initialization
//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------

   RegisterClasses([TGLColorProxy, TGLFreeFormProxy]);

end.
