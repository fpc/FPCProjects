{: glspatialpartitioning<p>

  <p>spatial partitioning related code that also uses glscene objects

      $Log: glspatialpartitioning.pas,v $
      Revision 1.1  2006/01/10 20:50:46  z0m3ie
      recheckin to make shure that all is lowercase

      Revision 1.4  2006/01/10 20:38:38  z0m3ie
      bugfixes from main tree

      Revision 1.3  2006/01/09 20:45:50  z0m3ie
      *** empty log message ***

      Revision 1.2  2005/12/04 16:53:06  z0m3ie
      renamed everything to lowercase to get better codetools support and avoid unit finding bugs

      Revision 1.1  2005/12/01 21:24:11  z0m3ie
      *** empty log message ***

      Revision 1.5  2005/11/14 21:38:07  z0m3ie
      making this stuff again Linux compatible please dont break multi platform support again

      Revision 1.4  2005/09/17 22:04:09  k00m
      *** empty log message ***

      Revision 1.3  2005/08/03 00:41:39  z0m3ie
      - added automatical generated History from CVS

	<b>History : </b><font size=-1><ul>
      <li>03/12/04 - MF - Created
  </ul></font>
}

unit glspatialpartitioning;

interface

uses
  {$ifdef mswindows}
  glwin32viewer,
  {$else}
  gllclviewer,
  {$endif}
  spatialpartitioning, glscene, vectorgeometry, opengl1x,
  geometrybb;

type
  {: Object for holding glscene objects in a spatial partitioning }
  TSceneObj = class(TSpacePartitionLeaf)
  public
    Obj : TGLBaseSceneObject;
    procedure UpdateCachedAABBAndBSphere; override;
    constructor CreateObj(Owner: TSectoredSpacePartition; aObj : TGLBaseSceneObject);
    destructor Destroy; override;
  end;

  {: Render a spacial partitioning descending from TSectoredSpacePartition
  (octree and quadtree) as a grid - great for debugging and visualisation }
  procedure RenderSpatialPartitioning(const Space : TSectoredSpacePartition);

  {: Create an extended frustum from a GLSceneViewer - this makes the unit
  specific to the windows platform!}
  function ExtendedFrustumMakeFromSceneViewer(const AFrustum : TFrustum;
        const vWidth, vHeight : integer; AGLCamera : TGLCamera) : TExtendedFrustum; //Changes here!

  {: Renders an AABB as a line }
  procedure RenderAABB(AABB : TAABB; w, r,g,b : single); overload;
  procedure RenderAABB(AABB : TAABB); overload;

implementation

procedure RenderAABB(AABB : TAABB);
begin
  RenderAABB(AABB, 1, 0.8, 0.8, 0.8);
end;

procedure RenderAABB(AABB : TAABB; w, r,g,b : single);
begin
  glColor3f(r,g,b);
  glLineWidth(w);

  glBegin(GL_LINE_STRIP);
    glVertex3f(AABB.min[0],AABB.min[1], AABB.min[2]);
    glVertex3f(AABB.min[0],AABB.max[1], AABB.min[2]);
    glVertex3f(AABB.max[0],AABB.max[1], AABB.min[2]);
    glVertex3f(AABB.max[0],AABB.min[1], AABB.min[2]);
    glVertex3f(AABB.min[0],AABB.min[1], AABB.min[2]);

    glVertex3f(AABB.min[0],AABB.min[1], AABB.max[2]);
    glVertex3f(AABB.min[0],AABB.max[1], AABB.max[2]);
    glVertex3f(AABB.max[0],AABB.max[1], AABB.max[2]);
    glVertex3f(AABB.max[0],AABB.min[1], AABB.max[2]);
    glVertex3f(AABB.min[0],AABB.min[1], AABB.max[2]);
  glEnd;

  glBegin(GL_LINES);
    glVertex3f(AABB.min[0],AABB.max[1], AABB.min[2]);
    glVertex3f(AABB.min[0],AABB.max[1], AABB.max[2]);

    glVertex3f(AABB.max[0],AABB.max[1], AABB.min[2]);
    glVertex3f(AABB.max[0],AABB.max[1], AABB.max[2]);

    glVertex3f(AABB.max[0],AABB.min[1], AABB.min[2]);
    glVertex3f(AABB.max[0],AABB.min[1], AABB.max[2]);
  glEnd;
end;

// RenderSpatialPartitioning
//
procedure RenderSpatialPartitioning(const Space : TSectoredSpacePartition);


  procedure RenderSectorNode(Node : TSectorNode);
  var
    i : integer;
    AABB : TAABB;
  begin
    if Node.NoChildren then begin
      AABB := Node.AABB;

      if Node.RecursiveLeafCount > 0 then
        RenderAABB(AABB, 1, 0, 0, 0)
      else
        RenderAABB(AABB, 1, 0.8, 0.8, 0.8)//}

    end else begin
      for i := 0 to Node.ChildCount-1 do
        RenderSectorNode(Node.Children[i]);
    end;
  end;
begin
  glPushAttrib(GL_ENABLE_BIT or GL_CURRENT_BIT or GL_LINE_BIT or GL_COLOR_BUFFER_BIT);
  glDisable(GL_LIGHTING);
  RenderSectorNode(Space.RootNode);
  glPopAttrib;
end;

{function ExtendedFrustumMakeFromSceneViewer(const AFrustum : TFrustum;
  const AGLSceneViewer : TGLSceneViewer) : TExtendedFrustum; //old version
begin
  Assert(Assigned(AGLSceneViewer.Camera),'GLSceneViewer must have camera specified!');
  result := ExtendedFrustumMake(AFrustum,
    AGLSceneViewer.Camera.NearPlane,
    AGLSceneViewer.Camera.DepthOfView,
    AGLSceneViewer.FieldOfView,
    AGLSceneViewer.Camera.Position.AsAffineVector,
    AGLSceneViewer.Camera.Direction.AsAffineVector);
end;}

function ExtendedFrustumMakeFromSceneViewer(const AFrustum : TFrustum;
  const vWidth, vHeight : integer; AGLCamera : TGLCamera) : TExtendedFrustum; //changed version
var buffov:single;
begin
if vWidth<vHeight then buffov := AGLCamera.GetFieldOfView(vWidth) else buffov := AGLCamera.GetFieldOfView(vHeight);
result := ExtendedFrustumMake(AFrustum,
                              AGLCamera.NearPlane,
                              AGLCamera.DepthOfView,
                              buffov,
                              AGLCamera.Position.AsAffineVector,
                              AGLCamera.Direction.AsAffineVector);
end;

{ TSceneObj }

constructor TSceneObj.CreateObj(Owner: TSectoredSpacePartition; aObj : TGLBaseSceneObject);
begin
  Obj := aObj;
  inherited CreateOwned(Owner);
end;

destructor TSceneObj.Destroy;
begin
  inherited;
end;

procedure TSceneObj.UpdateCachedAABBAndBSphere;
begin
  FCachedAABB := Obj.AxisAlignedBoundingBox;
  FCachedAABB.min := Obj.LocalToAbsolute(FCachedAABB.min);
  FCachedAABB.max := Obj.LocalToAbsolute(FCachedAABB.max);
  FCachedBSphere.Radius := Obj.BoundingSphereRadius;
  FCachedBSphere.Center := AffineVectorMake(Obj.AbsolutePosition);
end;
end.
