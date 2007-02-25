//
// this unit is part of the glscene project, http://glscene.org
//
{
  glfilenmf - normalmapper loading into glscene freeforms/actors
  
  notes:
    normalmapper can be found at http://www.ati.com/developer/tools.html

      $Log: glfilenmf.pas,v $
      Revision 1.1  2006/01/10 20:50:44  z0m3ie
      recheckin to make shure that all is lowercase

      Revision 1.1  2006/01/09 21:02:31  z0m3ie
      *** empty log message ***

      Revision 1.3  2005/12/04 16:53:03  z0m3ie
      renamed everything to lowercase to get better codetools support and avoid unit finding bugs

      Revision 1.2  2005/08/03 00:41:38  z0m3ie
      - added automatical generated History from CVS

  History:
    20/05/2003 - SG - Fixed SaveToStream to use ExtractTriangles
    16/05/2003 - SG - Creation
}
unit glfilenmf;

interface

uses
  classes, glvectorfileobjects, glmisc, vectorgeometry, vectorlists, applicationfileio,
  filenmf;

type
  TGLNMFVectorFile = class (TVectorFile)
    public
      class function Capabilities : TDataFileCapabilities; override;
      procedure LoadFromStream(aStream : TStream); override;
      procedure SaveToStream(aStream : TStream); override;
  end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TGLNMFVectorFile ------------------
// ------------------

// Capabilities
//
class function TGLNMFVectorFile.Capabilities : TDataFileCapabilities;
begin
  Result:=[dfcRead, dfcWrite];
end;

// LoadFromStream
//
procedure TGLNMFVectorFile.LoadFromStream(aStream : TStream);
var
  i,j  : Integer;
  mesh : TMeshObject;
  nmf  : TFileNMF;
begin
  nmf:=TFileNMF.Create;
  try
    nmf.LoadFromStream(aStream);
    mesh:=TMeshObject.CreateOwned(Owner.MeshObjects);
    mesh.Mode:=momTriangles;
    for i:=0 to nmf.NumTris-1 do begin
      for j:=0 to 2 do begin
        mesh.Vertices.Add(nmf.RawTriangles[i].vert[j]);
        mesh.Normals.Add(nmf.RawTriangles[i].norm[j]);
        mesh.TexCoords.Add(nmf.RawTriangles[i].texCoord[j]);
      end;
    end;
  finally
    nmf.Free;
  end;
end;

// SaveToStream
//
procedure TGLNMFVectorFile.SaveToStream(aStream : TStream);
var
  i,j  : Integer;
  nmf  : TFileNMF;
  Vertices,
  TempVertices,
  Normals,
  TexCoords : TAffineVectorList;
begin
  nmf:=TFileNMF.Create;
  Vertices:=TAffineVectorList.Create;
  Normals:=TAffineVectorList.Create;
  TexCoords:=TAffineVectorList.Create;
  try
    for i:=0 to Owner.MeshObjects.Count-1 do begin
      TempVertices:=Owner.MeshObjects[i].ExtractTriangles(TexCoords,Normals);
      Vertices.Add(TempVertices);
      TempVertices.Free;
    end;

    nmf.NumTris:=(Vertices.count div 3);
    SetLength(nmf.RawTriangles,nmf.NumTris);
    for i:=0 to nmf.NumTris-1 do begin
      for j:=0 to 2 do begin
        nmf.RawTriangles[i].vert[j]:=Vertices[3*i+j];
        nmf.RawTriangles[i].norm[j]:=Normals[3*i+j];
        nmf.RawTriangles[i].texCoord[j].S:=TexCoords[3*i+j][0];
        nmf.RawTriangles[i].texCoord[j].T:=TexCoords[3*i+j][1];
      end;
    end;
    nmf.SaveToStream(aStream);
  finally
    Vertices.Free;
    Normals.Free;
    TexCoords.Free;
    nmf.Free;
  end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

  RegisterVectorFileFormat('nmf', 'NormalMapper files', TGLNMFVectorFile);
  
end.
