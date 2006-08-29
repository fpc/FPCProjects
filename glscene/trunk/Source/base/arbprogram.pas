//
// this unit is part of the glscene project, http://glscene.org
//
// arbprogram
{: some useful methods for setting up arb vertex and fragment programs.<p>

   <b>History : </b><font size=-1><ul>
      <li>11/10/04 - SG - Creation
   </ul></font>
}
unit arbprogram;

interface

uses
   sysutils, opengl1x, glcontext;

procedure LoadARBProgram(target : GLenum; programText : String; var handle : cardinal);

implementation

procedure LoadARBProgram(target : GLenum; programText : String; var handle : cardinal);
var
   errPos : Integer;
   errString : String;
begin
   if (target = GL_VERTEX_PROGRAM_ARB) and not GL_ARB_vertex_program then
      raise Exception.Create('GL_ARB_vertex_program required!');
   if (target = GL_FRAGMENT_PROGRAM_ARB) and not GL_ARB_fragment_program then
      raise Exception.Create('GL_ARB_fragment_program required!');
   glGenProgramsARB(1, @handle);
   glBindProgramARB(target, handle);
   glProgramStringARB(target, GL_PROGRAM_FORMAT_ASCII_ARB,
      Length(programText), PChar(programText));
   glGetIntegerv(GL_PROGRAM_ERROR_POSITION_ARB, @errPos);
   if errPos>-1 then begin
      errString:=glGetString(GL_PROGRAM_ERROR_STRING_ARB);
      raise Exception.CreateFmt('ARB Program Error - [Handle: %d][Pos: %d][Error %s]', [handle, errPos, errString]);
   end;
   CheckOpenGLError;
end;

end.
