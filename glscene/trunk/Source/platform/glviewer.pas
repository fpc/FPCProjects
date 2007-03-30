//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLViewer<p>

   Platform independant viewer.<p>

    History:
      <li>30/03/07 - DaStr - Another update after the previous fix (removed class())
                             Added TVSyncMode type and constants.
      <li>24/03/07 - DaStr - Update for Windows after the previous fix
      <li>21/03/07 - DaStr - Improved Cross-Platform compatibility
                             (thanks Burkhard Carstens) (Bugtracker ID = 1684432)
      <li>17/03/07 - DaStr - Dropped Kylix support in favor of FPC (BugTrackerID=1681585)
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
{$IFDEF FPC}  //For FPC, always use LCLViewer
  TGLSceneViewer = GLLCLViewer.TGLSceneViewer;
  TVSyncMode = GLLCLViewer.TVSyncMode;
{$ELSE}  // if not FPC then
  {$IFDEF UNIX}  // kylix
    TGLSceneViewer = GLLinuxViewer.TGLLinuxSceneViewer;
    TVSyncMode = GLLinuxViewer.TVSyncMode;
  {$ENDIF UNIX}
  {$IFDEF MSWINDOWS} // windows
    TGLSceneViewer = GLWin32Viewer.TGLSceneViewer;
    TVSyncMode = GLWin32Viewer.TVSyncMode;
  {$ENDIF MSWINDOWS}
{$ENDIF FPC}

const
{$IFDEF FPC}  // FPC.
  // TVSyncMode.
  vsmSync = GLLCLViewer.vsmSync;
  vsmNoSync = GLLCLViewer.vsmNoSync;
{$ELSE}
  {$IFDEF UNIX}  // Kylix.
    vsmSync = GLLinuxViewer.vsmSync;
    vsmNoSync = GLLinuxViewer.vsmNoSync;
  {$ENDIF UNIX}
  {$IFDEF MSWINDOWS} // Windows.
    vsmSync = GLWin32Viewer.vsmSync;
    vsmNoSync = GLWin32Viewer.vsmNoSync;
  {$ENDIF MSWINDOWS}
{$ENDIF FPC}

implementation

end.
 
