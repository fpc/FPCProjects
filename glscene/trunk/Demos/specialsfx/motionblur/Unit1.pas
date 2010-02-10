{: Simple motion blur demo.<p>

   This demo illustrates a simple technique to obtain a motion blur: using
   a plane that covers all the viewport that is used to transparently blend
   the previous frame. By adjusting the transparency, you control how many
   frames are taken into account in the blur.<br>
   Since it is a frame-to-frame mechanism, the result is highly dependant
   on framerate, which is illustrated here by turning VSync ON or OFF in the
   demo (hit V or S key). You can control the number of frames with the up
   and down arrow key.<p>
   In a more complex application, you will have to implement a framerate
   control mechanism (relying on VSync isn't such a control mechanism,
   VSync frequency is a user setting that depends on the machine and monitor).<p>

   Original demo by Piotr Szturmaj.
}
unit Unit1;

{$MODE Delphi}

interface

uses LCLtype, Forms, Classes, Controls, GLLCLViewer, GLCadencer, GLScene,
  GLObjects, GLTexture, GLHUDObjects, SysUtils, ExtCtrls, GLPolyhedron,
  GLGeomObjects, GLUtils, LResources, GLViewer;

type

  { TForm1 }

  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer: TGLSceneViewer;
    Camera: TGLCamera;
    GLCadencer1: TGLCadencer;
    Light: TGLLightSource;
    Cube: TGLCube;
    HUD: TGLHUDSprite;
    Torus: TGLTorus;
    Timer1: TTimer;
    Dodecahedron: TGLDodecahedron;
    DummyCube: TGLDummyCube;
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure FormCreate(Sender: TObject);
    procedure GLSceneViewerPostRender(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure GLSceneViewerMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewerMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
    Frames : Integer;
    mx, my : Integer;
  end;

var
  Form1: TForm1;

implementation


procedure TForm1.FormCreate(Sender: TObject);
begin
   Frames:=5;
   HUD.Material.FrontProperties.Diffuse.Alpha:=1-1/Frames;
end;

procedure TForm1.GLSceneViewerPostRender(Sender: TObject);
begin
   // render is done, we transfer it to our hud plane so it can be used
   // in the next frame
   GLSceneViewer.Buffer.CopyToTexture(HUD.Material.Texture);
end;

procedure TForm1.FormResize(Sender: TObject);
var
   w, h : Integer;
begin
   // Here we resize our texture and plane to follow window dimension changes
   // Note that we have to stick to power of two texture dimensions if we don't
   // want performance to drop dramatically, this implies we can waste 3/4
   // of our texture memory... (f.i. a 513x513 window will require and use
   // a 1024x1024 texture)
   w:=RoundUpToPowerOf2(GLSceneViewer.Width);
   h:=RoundUpToPowerOf2(GLSceneViewer.Height);
   HUD.Material.Texture.DestroyHandles;
   with ((HUD.Material.Texture.Image) as TGLBlankImage) do begin
      Width:=w;
      Height:=h;
   end;
   HUD.Position.X:=w*0.5;
   HUD.Position.Y:=GLSceneViewer.Height-h*0.5;
   HUD.Width:=w;
   HUD.Height:=h;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
   // make things move
   Cube.TurnAngle:=newTime*90;
   DummyCube.PitchAngle:=newTime*60;
   Dodecahedron.RollAngle:=newTime*15;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
const
   cVSync : array [vsmSync..vsmNoSync] of String = ('VSync ON', 'VSync OFF');
begin
   Caption:=Format('Motion Blur on %d frames | %s | %f FPS',
                   [frames, cVSync[GLSceneViewer.VSync], GLSceneViewer.FramesPerSecond]);
   GLSceneViewer.ResetPerformanceMonitor;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
   // turn on/off VSync, this has an obvious impact on framerate,
   // which in turns impacts the motion blur look
   if (Key=Ord('S')) or (Key=Ord('V')) then
      if GLSceneViewer.VSync=vsmNoSync then
         GLSceneViewer.VSync:=vsmSync
      else GLSceneViewer.VSync:=vsmNoSync;

   // change the number of motion blur frames, and adjust
   // the transparency of the plane accordingly
   if Key=VK_UP then Inc(Frames);
   if (Key=VK_DOWN) and (Frames>0) then Dec(Frames);
   if Frames=0 then
      HUD.Visible:=False
   else begin
      HUD.Visible:=True;
      HUD.Material.FrontProperties.Diffuse.Alpha:=1-1/(1+Frames);
   end;
end;

// standard issue camera movement

procedure TForm1.GLSceneViewerMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   mx:=x; my:=y;
end;

procedure TForm1.GLSceneViewerMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
   if Shift=[ssLeft] then
      Camera.MoveAroundTarget(my-y, mx-x);
   mx:=x; my:=y;
end;

initialization
  {$i Unit1.lrs}

end.
