{: glportal<p>

	Portal Rendering support for GLScene.<p>

   The portal structures are subclasses of the Mesh structures, with a "sector"
   being assimilated to a "MeshObject" and sector polygons to facegroups.<p>

      $Log: glportal.pas,v $
      Revision 1.1  2006/01/10 20:50:45  z0m3ie
      recheckin to make shure that all is lowercase

      Revision 1.3  2006/01/09 20:45:50  z0m3ie
      *** empty log message ***

      Revision 1.2  2005/12/04 16:53:05  z0m3ie
      renamed everything to lowercase to get better codetools support and avoid unit finding bugs

      Revision 1.1  2005/12/01 21:24:11  z0m3ie
      *** empty log message ***

      Revision 1.2  2005/08/03 00:41:39  z0m3ie
      - added automatical generated History from CVS

	<b>Historique : </b><font size=-1><ul>
      <li>30/01/03 - Egg - Completed class registration
	   <li>13/08/00 - Egg - Creation
	</ul></font>
}
unit glportal;

interface

uses classes, glvectorfileobjects, glscene, gltexture, glmisc, vectorgeometry;

type

   // TPortalMeshObjectList
   //
   {: A mesh object list that handles portal rendering.<p>
      The items are treated as being sectors. } 
   TPortalMeshObjectList = class (TMeshObjectList)
      private
         { Private Declarations }

      protected
         { Protected Declarations }

      public
         { Public Declarations }
         constructor CreateOwned(AOwner : TGLBaseMesh);
         destructor Destroy; override;

         procedure BuildList(var mrci : TRenderContextInfo); override;
   end;


   // TSectorMeshObject
   //
   {: A portal renderer sector.<p> }
   TSectorMeshObject = class (TMorphableMeshObject)
      private
         { Private Declarations }
         FRenderDone : Boolean;

      protected
         { Protected Declarations }

      public
         { Public Declarations }
         constructor CreateOwned(AOwner : TMeshObjectList);
         destructor Destroy; override;

         procedure BuildList(var mrci : TRenderContextInfo); override;
         procedure Prepare; override;

         property RenderDone : Boolean read FRenderDone write FRenderDone;
   end;

	// TFGPolygon
	//
   {: A portal polygon.<p>
      This is the base class for portal polygons, the TFGPortalPolygon class
      implements the portal. }
	TFGPolygon = class (TFGVertexNormalTexIndexList)
	   private
	      { Private Declarations }

	   protected
	      { Protected Declarations }

	   public
	      { Public Declarations }
	      constructor CreateOwned(AOwner : TFaceGroups); override;
         destructor Destroy; override;

         procedure Prepare; override;
	end;

	// TFGPolygon
	//
   {: A portal polygon.<p>
      This is the base class for portal polygons, the TFGPortalPolygon class
      implements the portal. }
	TFGPortalPolygon = class (TFGPolygon)
	   private
	      { Private Declarations }
         FDestinationSectorIndex : Integer;
         FCenter, FNormal : TAffineVector;
         FRadius : Single;

	   protected
	      { Protected Declarations }

	   public
	      { Public Declarations }
	      constructor CreateOwned(AOwner : TFaceGroups); override;
         destructor Destroy; override;

         procedure BuildList(var mrci : TRenderContextInfo); override;

         procedure Prepare; override;

         property DestinationSectorIndex : Integer read FDestinationSectorIndex write FDestinationSectorIndex;
	end;

   // TGLPortal
   //
   {: Portal Renderer class. }
   TGLPortal = class(TGLBaseMesh)
      private
         { Private Declarations }

      protected
         { Protected Declarations }

      public
         { Public Declarations }
         constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;

      published
         { Published Declarations }
         property MaterialLibrary;
    end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses sysutils;

// ------------------
// ------------------ TPortalMeshObjectList ------------------
// ------------------

// CreateOwned
//
constructor TPortalMeshObjectList.CreateOwned(AOwner : TGLBaseMesh);
begin
   inherited CreateOwned(AOwner);
end;

// Destroy
//
destructor TPortalMeshObjectList.Destroy;
begin
   inherited;
end;

// BuildList
//
procedure TPortalMeshObjectList.BuildList(var mrci : TRenderContextInfo);
var
   i : Integer;
   startSector : TMeshObject;
begin
   for i:=0 to Count-1 do with TSectorMeshObject(Items[i]) do
      if InheritsFrom(TSectorMeshObject) then RenderDone:=False;
   startSector:=nil;
   for i:=0 to Count-1 do begin
      if Items[i].PointInObject(PAffineVector(@mrci.cameraPosition)^) then begin
         startSector:=Items[i];
         Break;
      end;
   end;
   if startSector<>nil then
      startSector.BuildList(mrci)
   else for i:=0 to Count-1 do Items[i].BuildList(mrci);
end;

// ------------------
// ------------------ TSectorMeshObject ------------------
// ------------------

// CreateOwned
//
constructor TSectorMeshObject.CreateOwned(AOwner : TMeshObjectList);
begin
	inherited;
   Mode:=momFaceGroups;
end;

// Destroy
//
destructor TSectorMeshObject.Destroy;
begin
	inherited;
end;

// BuildList
//
procedure TSectorMeshObject.BuildList(var mrci : TRenderContextInfo);
var
   i : Integer;
   libMat : TGLLibMaterial;
begin
   if not RenderDone then begin
      RenderDone:=True;
      // single pass : portals/polygons were sorted earlier
      if Assigned(mrci.materialLibrary) then begin
         for i:=0 to FaceGroups.Count-1 do with FaceGroups[i] do begin
            if Length(MaterialName)>0 then begin
               libMat:=mrci.materialLibrary.Materials.GetLibMaterialByName(MaterialName);
               if Assigned(libMat) then begin
                  libMat.Apply(mrci);
                  repeat
                     BuildList(mrci);
                  until not libMat.UnApply(mrci);
               end else BuildList(mrci);
            end else BuildList(mrci);
         end;
      end else for i:=0 to FaceGroups.Count-1 do
         FaceGroups[i].BuildList(mrci);
   end;
end;

// Prepare
//
procedure TSectorMeshObject.Prepare;
var
   i : Integer;
begin
   for i:=0 to FaceGroups.Count-1 do
      TFGPolygon(FaceGroups[i]).Prepare;
   FaceGroups.SortByMaterial; // this brings portals first
end;

// ------------------
// ------------------ TFGPolygon ------------------
// ------------------

// CreateOwned
//
constructor TFGPolygon.CreateOwned(AOwner : TFaceGroups);
begin
	inherited;
   Mode:=fgmmTriangleFan;
end;

// Destroy
//
destructor TFGPolygon.Destroy;
begin
	inherited;
end;

// Prepare
//
procedure TFGPolygon.Prepare;
begin
   // nothing, ain't no portal !
end;

// ------------------
// ------------------ TFGPortalPolygon ------------------
// ------------------

// CreateOwned
//
constructor TFGPortalPolygon.CreateOwned(AOwner : TFaceGroups);
begin
	inherited;
end;

// Destroy
//
destructor TFGPortalPolygon.Destroy;
begin
	inherited;
end;

// BuildList
//
procedure TFGPortalPolygon.BuildList(var mrci : TRenderContextInfo);
var
   dir : TAffineVector;
begin
   if FDestinationSectorIndex>=0 then begin
      VectorSubtract(FCenter, PAffineVector(@mrci.rcci.origin)^, dir);
      if (VectorDotProduct(FNormal, dir)<=0) and
            (not IsVolumeClipped(FCenter, FRadius, mrci.rcci)) then begin
         Owner.Owner.Owner.Items[FDestinationSectorIndex].BuildList(mrci);
      end
   end;
end;

// Prepare
//
procedure TFGPortalPolygon.Prepare;
var
   min, max : TAffineVector;
begin
   GetExtents(min, max);
   FNormal:=GetNormal;
   VectorAdd(min, max, FCenter);
   ScaleVector(FCenter, 0.5);
   FRadius:=VectorDistance(min, max)*0.5;
end;

// ------------------
// ------------------ TGLPortal ------------------
// ------------------

// Create
//
constructor TGLPortal.Create(AOwner: TComponent);
begin
   FMeshObjects:=TPortalMeshObjectList.CreateOwned(Self);
   inherited;
   ObjectStyle:=ObjectStyle+[osDirectDraw];
   UseMeshMaterials:=True;
end;

// Destroy
//
destructor TGLPortal.Destroy;
begin
   inherited;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

	// class registrations
   RegisterClasses([TGLPortal, TSectorMeshObject, TFGPolygon, TFGPortalPolygon]);

end.

