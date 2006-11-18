{: This sample illustrates basic actor use.<p>

	I'm using the GLScene built-in Actor Load from File, Load Texture,
   add one weapon, and animate both. The sensation is how both are one.
   Both weapon and actor are just TGLActor objects, the role is different.
   In the sample, it is possible change the animation of the actor and
   enabled/disable frame interpolation (smooth movement).<p>

   An actor is basicly a set of frames (or poses), which are arranged in
   animations, for instance to animate a salute, we have a first frame with
   the hand down, in the second it moved up, etc. Each animation consists
   series of frame defined by a name, a start frame and an end frame.<br>
   TGLActor take cares of animation (through progression events generated by
   a cadencer for instance) and generates intermediate frames if interpolation
   is enabled. In this sample only "Loop" mode is used, but note that there
   are other modes ("bounce", "playonce"...).<p>

   Performance note : with a small window and a fast 3D board, the FPS goes so
   high that it becomes limited by the... animation frame counter ;)<p>

   The stuff used for this sample is free, and information about this is in
   the waste.txt text file (see "media" directory).<p>

   Special Thanks to Eric Grange an Roger Cao for your help
}

// Modified by Dave Gravel to work with Lazarus first working version.

unit Unit1;

interface

uses
  LCLIntf, GLCadencer, GLVectorFileObjects, GLScene, GLObjects, GLMisc,
  StdCtrls, Buttons, Controls, ExtCtrls, ComCtrls, Classes, Forms, GLGraphics,
  GLTexture, GLLCLViewer, LResources, GLFileMD2, GLGeomObjects;

type

  { TForm1 }

  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    DummyCube1: TGLDummyCube;
    Disk1: TGLDisk;
    GLSceneViewer1: TGLSceneViewer;
    Actor1: TGLActor;
    Actor2: TGLActor;
    GLCadencer1: TGLCadencer;
    StatusBar1: TStatusBar;
    Panel1: TPanel;
    SBPlay: TSpeedButton;
    SBStop: TSpeedButton;
    CBAnimations: TComboBox;
    BBLoadWeapon: TBitBtn;
    SBFrameToFrame: TSpeedButton;
    Label1: TLabel;
    CBSmooth: TCheckBox;
    Timer1: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
	 procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure Panel1Click(Sender: TObject);
    procedure SBPlayClick(Sender: TObject);
    procedure SBStopClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BBLoadWeaponClick(Sender: TObject);
    procedure CBAnimationsChange(Sender: TObject);
    procedure SBFrameToFrameClick(Sender: TObject);
    procedure Actor1FrameChanged(Sender: TObject);
    procedure CBSmoothClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
  private
    { Déclarations privées }
    mdx, mdy : Integer;
  public
	 { Déclarations publiques }
  end;

var
  Form1: TForm1;
  i: integer;

implementation

uses VectorGeometry, SysUtils, JPEG;

procedure TForm1.FormCreate(Sender: TObject);
begin
   // Load Actor into GLScene
   Actor1.LoadFromFile('../../media/waste.md2');
   
   Actor1.Material.Texture.Image.LoadFromFile('../../media/waste.jpg');

   // Load Quake2 animations defaults, for "waste.md2", this is not required
   // since the author did not renamed the frames, and thus, GLScene can
   // recover them from the .MD2, but other authors just made a mess...
   // Loading the default animations takes care of that
   Actor1.Animations.LoadFromFile('../../media/Quake2Animations.aaf');

   // Scale Actor for put in the Scene
   Actor1.Scale.SetVector(0.04, 0.04, 0.04, 0);

   // Send animation names to the combo, to allow user selection
   Actor1.Animations.SetToStrings(CBAnimations.Items);
   // Force state to stand (first in list)
   CBAnimations.ItemIndex:=0;
   CBAnimationsChange(Self);

   // Load Texture for ground disk
   Disk1.Material.Texture.Image.LoadFromFile('../../media/clover.jpg');
end;

procedure TForm1.SBPlayClick(Sender: TObject);
begin
   // start playing
   Actor1.AnimationMode:=aamLoop;
   Actor2.AnimationMode:=aamLoop;

   // update buttons
   SBPlay.Enabled:=False;
   SBStop.Enabled:=True;
   SBFrameToFrame.Enabled:=False;
end;

procedure TForm1.SBStopClick(Sender: TObject);
begin
   // stop playing
   Actor1.AnimationMode:=aamNone;
   Actor2.AnimationMode:=aamNone;

   // update buttons
   SBPlay.Enabled:=True;
   SBStop.Enabled:=False;
   SBFrameToFrame.Enabled:=True;
end;

procedure TForm1.BBLoadWeaponClick(Sender: TObject);
begin
   // Load weapon model and texture
   Actor2.LoadFromFile('../../media/WeaponWaste.md2');
   Actor2.Material.Texture.Image.LoadFromFile('../../media/WeaponWaste.jpg');

   // Get animations frames from the main actor
   Actor2.Animations.Assign(Actor1.Animations);

   // Synch both actors
   Actor2.Synchronize(Actor1);
end;

procedure TForm1.CBAnimationsChange(Sender: TObject);
begin
   // Change animation
   Actor1.SwitchToAnimation(CBAnimations.Text, True);

   // Normally actors for Quake II Model have one number of frames
   // for all states 198 for actors and 172 for weapon,
   // frames 173 to 198 are for death
   // I use this for Hide and show weapon.
   Actor2.Visible:=(Actor1.NextFrameIndex<173);
   if Actor2.Visible then
      Actor2.Synchronize(Actor1);
end;

procedure TForm1.SBFrameToFrameClick(Sender: TObject);
begin
   // Animate Frame to Frame
   Actor1.NextFrame;
   Actor2.NextFrame;
end;

procedure TForm1.CBSmoothClick(Sender: TObject);
begin
   // Smooth movement is achieved by using linear frame interpolation
   if CBSmooth.Checked then begin
      Actor1.FrameInterpolation:=afpLinear;
      Actor2.FrameInterpolation:=afpLinear;
   end else begin
      Actor1.FrameInterpolation:=afpNone;
      Actor2.FrameInterpolation:=afpNone;
   end;
end;

procedure TForm1.Actor1FrameChanged(Sender: TObject);
begin
   StatusBar1.SimpleText:='CurrentFrame = '+IntToStr(Actor1.CurrentFrame);
end;

//
// events that follow handle camera movements and FPS rate
//

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
	// store mouse coordinates when a button went down
	mdx:=x; mdy:=y;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin

end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
	// (we're moving around the parent and target dummycube)
   if Shift<>[] then
      GLCamera1.MoveAroundTarget(mdy-y, mdx-x);
	mdx:=x; mdy:=y;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
	// Note that 1 wheel-step induces a WheelDelta of 120,
	// this code adjusts the distance to target with a 10% per wheel-step ratio
	GLCamera1.AdjustDistanceToTarget(Power(1.1, WheelDelta/120));
end;

procedure TForm1.Panel1Click(Sender: TObject);
begin

end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
   Caption:=Format('%.2f FPS', [GLSceneViewer1.FramesPerSecond]);
   GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
   GLSceneViewer1.Invalidate;
end;

initialization
  {$i Unit1.lrs}


end.
