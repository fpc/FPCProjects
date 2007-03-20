//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLViewer<p>

   Platform independant viewer.<p>

    History:
      <li>17/03/07 - DaStr - Dropped Kylix support in favor of FPC (BugTracekrID=1681585)
      <li>24/01/02 -  EG   - Initial version
}

unit GLViewer;

interface

{$I GLScene.inc}

uses
{$IFDEF FPC}
  GLLCLViewer;
{$ELSE}
  {$IFDEF UNIX}GLLinuxViewer; {$ENDIF UNIX}
  {$IFDEF MSWINDOWS}GLWin32Viewer; {$ENDIF MSWINDOWS}
{$ENDIF FPC}

type
{$IFDEF FPC}
  TGLSceneViewer = class(GLLCLViewer.TGLSceneViewer);
{$ELSE}
{$IFDEF UNIX}
  TGLSceneViewer = class(TGLLinuxSceneViewer);
{$ENDIF LINUX}
{$IFDEF MSWINDOWS}
  TGLSceneViewer = class(TGLWin32SceneViewer);
{$ENDIF MSWINDOWS}
{$ENDIF FPC}
implementation

end.
 
