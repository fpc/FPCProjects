//
// this unit is part of the glscene project, http://glscene.org
//
unit typesstl;

interface

uses vectorgeometry;

type

  TSTLHeader = packed record
     dummy : array[0..79] of byte;
     nbFaces : Longint;
  end;

  TSTLVertex = TAffineVector;
{ Original specs : = packed record
	   x : single;
	   y : single;
	   z : single;
  end; }

  TSTLFace = packed record
	   normal : TSTLVertex;	// facet surface normal
	   v1 : TSTLVertex;	// vertex 1
	   v2 : TSTLVertex;	// vertex 2
	   v3 : TSTLVertex;	// vertex 3
      padding : array[0..1] of byte;
   end;

implementation

end.
