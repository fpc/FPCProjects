//
// this unit is part of the glscene project, http://glscene.org
//
{: glverletclasses<p>

   Classes and functions that make integration between verlets and glscene
   objects easy.

      $Log: glverletclasses.pas,v $
      Revision 1.1  2006/01/10 20:50:46  z0m3ie
      recheckin to make shure that all is lowercase

      Revision 1.3  2006/01/09 20:45:51  z0m3ie
      *** empty log message ***

      Revision 1.2  2005/12/04 16:53:06  z0m3ie
      renamed everything to lowercase to get better codetools support and avoid unit finding bugs

      Revision 1.1  2005/12/01 21:24:11  z0m3ie
      *** empty log message ***

      Revision 1.2  2005/08/03 00:41:39  z0m3ie
      - added automatical generated History from CVS

	<b>History : </b><font size=-1><ul>
      <li>13/04/04 - mf - verlet call now uses tverletprogresstimes
      <li>06/03/04 - mf - creation
   </ul>
}
unit glverletclasses;

interface

uses
  verletclasses, vectorgeometry, glscene, globjects;

type
  // TGLVerletNode
  //
  {: Specialized verlet node that can be anchored to a GLScene object. If it's
     anchored and has the property "NailedDown" set, it will remain in the same
     relative position to the GLScene object.}
  TGLVerletNode = class(TVerletNode)
  private
    FRelativePosition: TAffineVector;
    FGLBaseSceneObject: TGLBaseSceneObject;
    procedure SetGLBaseSceneObject(const Value: TGLBaseSceneObject);
  protected
    procedure SetLocation(const Value: TAffineVector);override;
  public
    procedure Verlet(const vpt : TVerletProgressTimes); override;

    property GLBaseSceneObject : TGLBaseSceneObject read FGLBaseSceneObject write SetGLBaseSceneObject;
    property RelativePosition : TAffineVector read FRelativePosition write FRelativePosition;
  end;

  function CreateVCPlaneFromGLPlane(Plane : TGLPlane; VerletWorld : TVerletWorld; Offset : single) : TVCFloor;

implementation

function CreateVCPlaneFromGLPlane(Plane : TGLPlane; VerletWorld : TVerletWorld; Offset : single) : TVCFloor;
begin
  result := TVCFloor.Create(VerletWorld);
  with result do
  begin
    Normal := VectorNormalize(Plane.Direction.AsAffineVector);

    Location := VectorAdd(Plane.Position.AsAffineVector, VectorScale(Normal, Offset));
  end;
end;

{ TGLVerletNode }

procedure TGLVerletNode.SetGLBaseSceneObject(
  const Value: TGLBaseSceneObject);
begin
  FGLBaseSceneObject := Value;

  if Assigned(GLBaseSceneObject) and NailedDown then
    FRelativePosition := AffineVectorMake(GLBaseSceneObject.AbsoluteToLocal(VectorMake(FLocation, 1)));
end;

procedure TGLVerletNode.SetLocation(const Value: TAffineVector);
begin
  inherited;
  if Assigned(GLBaseSceneObject) and NailedDown then
    FRelativePosition := GLBaseSceneObject.AbsoluteToLocal(Value);
end;

procedure TGLVerletNode.Verlet(const vpt : TVerletProgressTimes);
begin
  if Assigned(GLBaseSceneObject) and NailedDown then
  begin
    FLocation := GLBaseSceneObject.LocalToAbsolute(FRelativePosition);
  end else
    inherited;
end;
end.
