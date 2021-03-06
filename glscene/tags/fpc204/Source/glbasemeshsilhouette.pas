// glbasemeshsilhouette
{: silhouette classes for glbasemesh and facegroups.<p>

      $Log: glbasemeshsilhouette.pas,v $
      Revision 1.1  2006/01/10 20:50:45  z0m3ie
      recheckin to make shure that all is lowercase

      Revision 1.3  2006/01/09 20:45:49  z0m3ie
      *** empty log message ***

      Revision 1.2  2005/12/04 16:53:05  z0m3ie
      renamed everything to lowercase to get better codetools support and avoid unit finding bugs

      Revision 1.1  2005/12/01 21:24:10  z0m3ie
      *** empty log message ***

      Revision 1.3  2005/08/03 00:41:38  z0m3ie
      - added automatical generated History from CVS

	<b>History : </b><font size=-1><ul>
      <li>09/02/04 - MF - Fixed bug where vertices weren't freed when owned
      <li>24/06/03 - MF - Created file from parts of GLShilouette
   </ul></font>
}

unit glbasemeshsilhouette;

interface

uses classes, glmisc, vectorgeometry, vectorlists, glvectorfileobjects, glsilhouette;

type
   // TFaceGroupConnectivity
   //
   TFaceGroupConnectivity = class(TConnectivity)
       private
          FMeshObject : TMeshObject;
          FOwnsVertices : boolean;
          procedure SetMeshObject(const Value: TMeshObject);

       public
          procedure Clear; override;

          {: Builds the connectivity information. }
          procedure RebuildEdgeList;


          property MeshObject : TMeshObject read FMeshObject write SetMeshObject;

          constructor Create(PrecomputeFaceNorml : boolean); override;
          constructor CreateFromMesh(aMeshObject : TMeshObject; precomputeFaceNorml : Boolean);
          destructor Destroy; override;
   end;

   // TGLBaseMeshConnectivity
   //
   TGLBaseMeshConnectivity = class(TBaseConnectivity)
       private
          FGLBaseMesh : TGLBaseMesh;
          FFaceGroupConnectivityList : TList;
          function GetFaceGroupConnectivity(i: integer): TFaceGroupConnectivity;
          function GetConnectivityCount: integer;
          procedure SetGLBaseMesh(const Value: TGLBaseMesh);

       protected
          function GetEdgeCount: integer; override;
          function GetFaceCount: integer; override;

       public
          property ConnectivityCount : integer read GetConnectivityCount;
          property FaceGroupConnectivity[i : integer] : TFaceGroupConnectivity read GetFaceGroupConnectivity;
          property GLBaseMesh : TGLBaseMesh read FGLBaseMesh write SetGLBaseMesh;

          procedure Clear(SaveFaceGroupConnectivity : boolean);

          {: Builds the connectivity information. }
          procedure RebuildEdgeList;

          procedure CreateSilhouette(const silhouetteParameters : TGLSilhouetteParameters; var aSilhouette : TGLSilhouette; AddToSilhouette : boolean); override;

          constructor Create(PrecomputeFaceNorml : boolean); override;
          constructor CreateFromMesh(aGLBaseMesh : TGLBaseMesh);
          destructor Destroy; override;
   end;

implementation

{ TFaceGroupConnectivity }

// ------------------
// ------------------ TFaceGroupConnectivity ------------------
// ------------------

procedure TFaceGroupConnectivity.Clear;
begin
  if Assigned(FVertices) then
  begin
    if FOwnsVertices then
      FVertices.Clear
    else
      FVertices := nil;

    inherited;

    if not FOwnsVertices and Assigned(FMeshObject) then
      FVertices := FMeshObject.Vertices;
  end else
    inherited;
end;

constructor TFaceGroupConnectivity.Create(PrecomputeFaceNorml: boolean);
begin
  inherited;

  FOwnsVertices := true;
end;

procedure TFaceGroupConnectivity.SetMeshObject(const Value: TMeshObject);
begin
  Clear;

  FMeshObject := Value;

  if FOwnsVertices then
    FVertices.Free;

  FVertices := FMeshObject.Vertices;

  FOwnsVertices := false;

  RebuildEdgeList;
end;

constructor TFaceGroupConnectivity.CreateFromMesh(aMeshObject: TMeshObject;
  PrecomputeFaceNorml: boolean);
begin
  Create(PrecomputeFaceNormal);

  MeshObject := aMeshObject;
end;

destructor TFaceGroupConnectivity.Destroy;
begin
  if FOwnsVertices then
    FVertices.Free;

  FVertices := nil;
  inherited;
end;

procedure TFaceGroupConnectivity.RebuildEdgeList;
var
  iFaceGroup, iFace, iVertex  : integer;
  FaceGroup : TFGVertexIndexList;
  List : PIntegerArray;
begin
  // Make sure that the connectivity information is empty
  Clear;

  // Create a list of edges for the meshobject
  for iFaceGroup := 0 to FMeshObject.FaceGroups.Count-1 do
  begin
    Assert(FMeshObject.FaceGroups[iFaceGroup] is TFGVertexIndexList,
           'Method only works for descendants of TFGVertexIndexList.');
    FaceGroup := TFGVertexIndexList(FMeshObject.FaceGroups[iFaceGroup]);

    case FaceGroup.Mode of
      fgmmTriangles, fgmmFlatTriangles :
      begin
        for iFace := 0 to FaceGroup.TriangleCount - 1 do
        begin
          List := @FaceGroup.VertexIndices.List[iFace * 3 + 0];
          AddIndexedFace(List^[0], List^[1], List^[2]);
        end;
      end;
      fgmmTriangleStrip :
      begin
        for iFace:=0 to FaceGroup.VertexIndices.Count-3 do
        begin
          List := @FaceGroup.VertexIndices.List[iFace];
          if (iFace and 1)=0 then
             AddIndexedFace(List^[0], List^[1], List^[2])
          else
             AddIndexedFace(List^[2], List^[1], List^[0]);
        end;
      end;
      fgmmTriangleFan :
      begin
        List := FaceGroup.VertexIndices.List;

        for iVertex:=2 to FaceGroup.VertexIndices.Count-1 do
          AddIndexedFace(List^[0], List^[iVertex-1], List^[iVertex])
      end;
      else
        Assert(false,'Not supported');
    end;
  end;
end;

// ------------------
// ------------------ TGLBaseMeshConnectivity ------------------
// ------------------

procedure TGLBaseMeshConnectivity.RebuildEdgeList;
var
  i : integer;
begin
  for i := 0 to ConnectivityCount - 1 do
    FaceGroupConnectivity[i].RebuildEdgeList;
end;

procedure TGLBaseMeshConnectivity.Clear(SaveFaceGroupConnectivity : boolean);
var
  i : integer;
begin
  if SaveFaceGroupConnectivity then
  begin
    for i := 0 to ConnectivityCount - 1 do
      FaceGroupConnectivity[i].Clear;
  end else
  begin
    for i := 0 to ConnectivityCount - 1 do
      FaceGroupConnectivity[i].Free;

    FFaceGroupConnectivityList.Clear;
  end;
end;

constructor TGLBaseMeshConnectivity.Create(PrecomputeFaceNorml: boolean);
begin
  FFaceGroupConnectivityList := TList.Create;

  inherited;
end;

constructor TGLBaseMeshConnectivity.CreateFromMesh(aGLBaseMesh: TGLBaseMesh);
begin
  Create(not (aGLBaseMesh is TGLActor));
  GLBaseMesh := aGLBaseMesh;
end;

procedure TGLBaseMeshConnectivity.SetGLBaseMesh(const Value: TGLBaseMesh);
var
  i : integer;
  MO : TMeshObject;
  Connectivity : TFaceGroupConnectivity;
begin
  Clear(False);

  FGLBaseMesh := Value;

  // Only precompute normals if the basemesh isn't an actor (because they change)
  FPrecomputeFaceNormal := not (Value is TGLActor);
  FGLBaseMesh := Value;

  for i := 0 to Value.MeshObjects.Count-1 do
  begin
    MO := Value.MeshObjects[i];

    Connectivity := TFaceGroupConnectivity.CreateFromMesh(MO, FPrecomputeFaceNormal);

    FFaceGroupConnectivityList.Add(Connectivity);
  end;
end;

procedure TGLBaseMeshConnectivity.CreateSilhouette(
  const silhouetteParameters : TGLSilhouetteParameters;
  var aSilhouette : TGLSilhouette; AddToSilhouette : boolean);

var
  i : integer;
begin
  if aSilhouette=nil then
    aSilhouette:=TGLSilhouette.Create
  else
    aSilhouette.Flush;

  for i := 0 to ConnectivityCount-1 do
    FaceGroupConnectivity[i].CreateSilhouette(silhouetteParameters, aSilhouette, true);
end;

destructor TGLBaseMeshConnectivity.Destroy;
begin
  Clear(false);
  FFaceGroupConnectivityList.Free;

  inherited;
end;

function TGLBaseMeshConnectivity.GetConnectivityCount: integer;
begin
  result := FFaceGroupConnectivityList.Count;
end;

function TGLBaseMeshConnectivity.GetEdgeCount: integer;
var
  i : integer;
begin
  result := 0;
  for i := 0 to ConnectivityCount - 1 do
    result := result + FaceGroupConnectivity[i].EdgeCount;
end;

function TGLBaseMeshConnectivity.GetFaceCount: integer;
var
  i : integer;
begin
  result := 0;
  for i := 0 to ConnectivityCount - 1 do
    result := result + FaceGroupConnectivity[i].FaceCount;
end;

function TGLBaseMeshConnectivity.GetFaceGroupConnectivity(
  i: integer): TFaceGroupConnectivity;
begin
  result := TFaceGroupConnectivity(FFaceGroupConnectivityList[i]);
end;
end.
