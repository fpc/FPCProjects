//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLFileMD2<p>

	Quake2 MD2 vector file format implementation.<p>

	<b>History :</b><font size=-1><ul>
      <li>31/03/07 - DaStr - Added $I GLScene.inc
      <li>05/06/03 - SG - Separated from GLVectorFileObjects.pas
	</ul></font>
}
unit GLFileMD2;

interface

{$I GLScene.inc}

uses
  Classes, SysUtils, GLVectorFileObjects, ApplicationFileIO, FileMD2, TypesMD2;

type
   // TGLMD2VectorFile
   //
   {: The MD2 vector file (Quake2 actor file).<p>
      Stores a set of "frames" describing the different postures of the actor,
      it may be animated by TGLActor. The "Skin" must be loaded indepentendly
      (the whole mesh uses a single texture bitmap).<p>
      Based on code by Roger Cao. }
   TGLMD2VectorFile = class(TVectorFile)
      public
         { Public Declarations }
         class function Capabilities : TDataFileCapabilities; override;
         procedure LoadFromStream(aStream : TStream); override;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TGLMD2VectorFile ------------------
// ------------------

// Capabilities
//
class function TGLMD2VectorFile.Capabilities : TDataFileCapabilities;
begin
   Result:=[dfcRead];
end;

// LoadFromStream
//
procedure TGLMD2VectorFile.LoadFromStream(aStream : TStream);
var
   i, j : Integer;
   MD2File : TFileMD2;
   mesh : TMorphableMeshObject;
   faceGroup : TFGIndexTexCoordList;
   morphTarget : TMeshMorphTarget;
begin
   MD2File:=TFileMD2.Create;
   MD2File.LoadFromStream(aStream);
   try
      // retrieve mesh data
      mesh:=TMorphableMeshObject.CreateOwned(Owner.MeshObjects);
      with mesh, MD2File do begin
         Mode:=momFaceGroups;
         faceGroup:=TFGIndexTexCoordList.CreateOwned(FaceGroups);
         with faceGroup do begin
            MaterialName:='';
            VertexIndices.Capacity:=iTriangles*3;
            TexCoords.Capacity:=iTriangles*3;
            // copy the face list
            for i:=0 to iTriangles-1 do with IndexList(m_index_list)[i] do begin
               Add(a, a_s, -a_t);
               Add(b, b_s, -b_t);
               Add(c, c_s, -c_t);
            end;
         end;
         // retrieve frames data (morph targets)
         for i:=0 to iFrames-1 do begin
            morphTarget:=TMeshMorphTarget.CreateOwned(MorphTargets);
            with morphTarget do begin
               Name:='Frame'+IntToStr(i);
               Vertices.Capacity:=iVertices;
               for j:=0 to iVertices-1 do
                  Vertices.Add(VertexList(frameList(m_frame_list)[i])[j]);
               BuildNormals(faceGroup.VertexIndices, momTriangles);
            end;
         end;
      end;
      if GetOwner is TGLActor then with TGLActor(GetOwner).Animations do begin
         Clear;
         with MD2File do for i:=0 to frameNames.Count-1 do with Add do begin
            Name:=frameNames[i];
            Reference:=aarMorph;
            StartFrame:=Integer(frameNames.Objects[i]);
            if i<frameNames.Count-1 then
               EndFrame:=Integer(frameNames.Objects[i+1])-1
            else EndFrame:=iFrames-1;
         end;
      end;
      if mesh.MorphTargets.Count>0 then
         mesh.MorphTo(0);
   finally
      MD2File.Free;
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterVectorFileFormat('md2', 'Quake II model files', TGLMD2VectorFile);

end.