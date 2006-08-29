// glpolyhedron
{: standard polyhedrons.<p>

      $Log: glpolyhedron.pas,v $
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

	<b>History : </b><font size=-1><ul>
      <li>20/01/04 - SG - Added TGLIcosahedron
      <li>21/07/03 - EG - Creation from GLObjects split
   </ul></font>
}
unit glpolyhedron;

interface

uses classes, glscene, gltexture, vectorgeometry, opengl1x, glmisc;

type

   // TGLDodecahedron
   //
   {: A Dodecahedron.<p>
      The dodecahedron has no texture coordinates defined, ie. without using
      a texture generation mode, no texture will be mapped. }
   TGLDodecahedron = class(TGLSceneObject)
      public
			{ Public Declarations }
         procedure BuildList(var rci : TRenderContextInfo); override;
   end;

   // TGLIcosahedron
   //
   {: A Icosahedron.<p>
      The icosahedron has no texture coordinates defined, ie. without using
      a texture generation mode, no texture will be mapped. }
   TGLIcosahedron = class(TGLSceneObject)
      public
			{ Public Declarations }
         procedure BuildList(var rci : TRenderContextInfo); override;
   end;

//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------
implementation
//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------

uses globjects;

// ------------------
// ------------------ TGLDodecahedron ------------------
// ------------------

// BuildList
//
procedure TGLDodecahedron.BuildList(var rci : TRenderContextInfo);
begin
   DodecahedronBuildList;
end;

// ------------------
// ------------------ TGLIcosahedron ------------------
// ------------------

// BuildList
//
procedure TGLIcosahedron.BuildList(var rci : TRenderContextInfo);
begin
   IcosahedronBuildList;
end;

//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------
initialization
//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------

   RegisterClasses([TGLDodecahedron, TGLIcosahedron]);

end.

