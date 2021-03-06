{: This is a basic use for the Dynamic Collision Engine (DCE) by Lucas Goraieb.<p>

     The engine pretty much works by creating a TGLDCEManager, and several
     TGLDCEDynamic and TGLDCEStatic behaviours on the objects that should
     interact. Each object can be either an ellipsoid, cube, freeForm or terrain,
     have different sizes and friction, respond differently to collisions, etc.

     This means your next FPS project is pretty much done: All you have to do
     is keep loading object files into freeForms and letting DCE do the trick
     for you. The only "real" code in this demo is inside the onProgress event
     of the cadencer, that takes care of input.
}

unit Unit1;

{$MODE Delphi}

interface

uses
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, GLScene, GLObjects, GLMisc, GLCadencer, GLWin32Viewer,
  GLTexture, GLHeightData, GLTerrainRenderer, GLVectorFileObjects,
  ExtCtrls, GLBitmapFont, GLWindowsFont, GLHUDObjects, LResources,
  GLDCE, Windows;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    GLCamera1: TGLCamera;
    Player: TGLDummyCube;
    GLDCEManager1: TGLDCEManager;
    Terrain: TGLTerrainRenderer;
    GLBitmapHDS1: TGLBitmapHDS;
    GLMatlLib: TGLMaterialLibrary;
    GLLightSource1: TGLLightSource;
    GLActor1: TGLActor;
    GLSphere1: TGLSphere;
    GLLightSource2: TGLLightSource;
    Balls: TGLDummyCube;
    GLWindowsBitmapFont1: TGLWindowsBitmapFont;
    Timer1: TTimer;
    GLHUDText1: TGLHUDText;
    Mushrooms: TGLDummyCube;
    moMushroom: TGLFreeForm;
    GLDirectOpenGL1: TGLDirectOpenGL;
    GLCube1: TGLCube;
    Help: TGLHUDText;
    HelpShadow: TGLHUDText;
    Ground: TGLPlane;
    procedure FormShow(Sender: TObject);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure PlayerBehaviours0Collision(Sender: TObject;
      ObjectCollided: TGLBaseSceneObject; CollisionInfo: TDCECollision);
    procedure Timer1Timer(Sender: TObject);
    procedure GLDirectOpenGL1Render(Sender: TObject;
      var rci: TRenderContextInfo);
  private
    { Private declarations }
  public
    { Public declarations }
    mx, my : Integer;
    Jumped: boolean;
    procedure Load;
    procedure HandleKeys;
    procedure HandleAnimation;
    procedure AddBall;
    procedure AddMushrooms;
  end;

var
  Form1: TForm1;

const
  cForce: Single = 250;
  cSpread = 200;
  cNbMushrooms = 20;

implementation

uses Jpeg, GLFileMD2, GLFile3DS, VectorGeometry, GLKeyBoard, GLProxyObjects,
     OpenGL1x, GLEllipseCollision;


{ TForm1 }

procedure TForm1.Load;
begin
  //Load Materials
  with GLMatlLib do
  begin
    AddTextureMaterial('Terrain','..\..\media\snow512.jpg');
    AddTextureMaterial('Actor','..\..\media\waste.jpg');
  end;

  //Load Terrain
  GLBitmapHDS1.MaxPoolSize:=8*1024*1024;
  GLBitmapHDS1.Picture.LoadFromFile('..\..\media\terrain.bmp');
  Terrain.Direction.SetVector(0,1,0);
  Terrain.Material.LibMaterialName := 'Terrain';
  Terrain.TilesPerTexture:=256/Terrain.TileSize;
  Terrain.Scale.SetVector(1,1,0.02);

  Ground.Material.LibMaterialName := 'Terrain';

  // Load mushroom mesh
  //Always use AutoScaling property or you may get some problems
  moMushRoom.AutoScaling.SetPoint(0.1,0.1,0.1);
  moMushRoom.LoadFromFile('..\..\media\Mushroom.3ds');
  moMushRoom.Direction.SetVector(0,1,0);
  moMushRoom.BuildOctree;

  //Load player
  Player.Position.SetPoint(0,3,0);
  //Actor
  GLActor1.LoadFromFile('..\..\media\Waste.md2');
  GLActor1.Direction.SetVector(0,1,0);
  GLActor1.Up.SetVector(1,0,0);
  GLActor1.Scale.SetVector(0.05,0.05,0.05);
  GLActor1.Material.LibMaterialName := 'Actor';
  GLActor1.Animations.LoadFromFile('..\..\media\Quake2Animations.aaf');
  // Define animation properties
  GLActor1.AnimationMode:=aamLoop;
  GLActor1.SwitchToAnimation('stand');
  GLActor1.FrameInterpolation:=afpLinear;

  //DCE Behaviour
  GLSphere1.Scale.Assign(GetOrCreateDCEDynamic(Player).Size);
  GetOrCreateDCEDynamic(Player).OnCollision := PlayerBehaviours0Collision;
end;

procedure TForm1.HandleKeys;
var
  Force: TAffineVector;
begin

  Force := NullVector;
  if IsKeyDown('w') or IsKeyDown('z') then Force[2] := cForce;
  if IsKeyDown('s') then Force[2] := -cForce;
  if IsKeyDown('a') or IsKeyDown('q') then Force[0] := cForce;
  if IsKeyDown('d') then Force[0] := -cForce;

  GetOrCreateDCEDynamic(Player).ApplyAccel(Force);
end;

procedure TForm1.HandleAnimation;
var anim: string;
begin
  if VectorNorm(GetOrCreateDCEDynamic(Player).Speed) > 0.1 then
    anim := 'run'
  else
    anim := 'stand';

  if Jumped then
  begin
    if (not GetOrCreateDCEDynamic(Player).InGround) then
      anim := 'jump'
    else
     Jumped := False;
  end;

  if anim = 'jump' then GLActor1.Interval := 500
  else GLActor1.Interval := 100;

   if GLActor1.CurrentAnimation<>anim then
      GLActor1.SwitchToAnimation(anim);
end;

procedure TForm1.AddBall;
var Ball: TGLSphere;
    S: Single;
begin
  Ball := TGLSphere(Balls.AddNewChild(TGLSphere));
  with Ball do
  begin
    Tag := 1; //set the identifier of a ball
    Radius := 1;
    S := (100+Random(900))/500;
    Scale.SetVector(s,s,s);
    Position.SetPoint(Random(40)-random(40),4+Random(10),Random(40)-random(40));
    Material.FrontProperties.Diffuse.SetColor((100+Random(900))/1000,(100+Random(900))/1000,(100+Random(900))/1000);
  end;
  with GetOrCreateDCEDynamic(Ball) do
  begin
    Manager := GLDCEManager1;
    BounceFactor := 0.75;
    Friction := 0.1;
    SlideOrBounce := csbBounce;
    Size.Assign(Ball.Scale);
  end;
end;

procedure TForm1.AddMushrooms;
var
   i : Integer;
   proxy : TGLFreeFormProxy;
   s : TVector;
   f : Single;
begin
   // spawn some more mushrooms using proxy objects
   for i:=0 to cNbMushrooms-1 do begin
      // create a new proxy and set its MasterObject property
      proxy:=TGLFreeFormProxy(MushRooms.AddNewChild(TGLFreeFormProxy));
      with proxy do begin
         ProxyOptions:=[pooObjects];
         MasterObject:=moMushroom;
         // retrieve reference attitude
         Direction:=moMushroom.Direction;
         Up:=moMushroom.Up;
         // randomize scale
         s:=moMushroom.Scale.AsVector;
         f:=(2*Random+1);
         ScaleVector(s, f);
         Scale.AsVector:=s;
         // randomize position
         Position.SetPoint(Random(cSpread)-(cSpread/2),
                           moMushroom.Position.z+1.5*f,
                           Random(cSpread)-(cSpread/2));
         // randomize orientation
         RollAngle:=Random(360);
         TransformationChanged;
      end;
      with GetOrCreateDCEStatic(Proxy) do
      begin
        Manager := GLDCEManager1;
        BounceFactor := 0.75;
        Friction := 10;
        Shape := csFreeform;
      end;

   end;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  Load;
  GLCadencer1.Enabled := true;
  Help.Text :=
   'Mouse Drag - Look'+#13+
   'A,W,S,D - movement'+#13+
   'SPACE - Jump'+#13+
   'F1 - Add one ball'+#13+
   'F2 - Add 10 balls'+#13+
   'F3 - Add 20 mushrooms'+#13+
   'F4 - Change ground to box'+#13+
   'F5 - Toggle step mode'+#13+
   'RETURN - Reset';
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  //Mouse look
  if ssLeft in Shift then
  begin
    GLCamera1.MoveAroundTarget((my-y),0);
    Player.Turn(- (mx-x));
  end;
  mx:=x;
  my:=y;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  HandleKeys;
  HandleAnimation;
  //This shows the manual progress, don't need this if you use the automatic mode
  if GLDCEManager1.ManualStep then GLDCEManager1.Step(deltaTime);

  Help.ModulateColor.Alpha := Help.ModulateColor.Alpha - (deltaTime * 0.05);
  if Help.ModulateColor.Alpha < 0.25 then Help.ModulateColor.Alpha := 0.25;
  HelpShadow.ModulateColor.Alpha := Help.ModulateColor.Alpha;
  HelpShadow.Text := Help.Text;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var i: integer;
begin
  if Key = VK_F1 then AddBall;
  if Key = VK_F2 then for i := 1 to 10 do AddBall;
  if Key = VK_F3 then AddMushrooms;
  if (Key = VK_Space) then
  begin
    GetOrCreateDCEDynamic(Player).Jump(1,20);
    Jumped := true;
  end;
  if key = VK_F4 then
  begin
    Terrain.Visible := False;
    Ground.Visible := true;
    GetOrCreateDCEStatic(Terrain).Active := False;
    GetOrCreateDCEStatic(Ground).Active := True;
  end;
  if key = VK_F5 then GLDCEManager1.ManualStep := not GLDCEManager1.ManualStep;

  if (Key = VK_RETURN) then
  begin
    Player.Position.SetPoint(0,3,0);
    Balls.DeleteChildren;
    MushRooms.DeleteChildren;
    Help.ModulateColor.Alpha := 1;
    Terrain.Visible := True;
    Ground.Visible := False;
    GetOrCreateDCEStatic(Terrain).Active := True;
    GetOrCreateDCEStatic(Ground).Active := False;
  end;
end;

procedure TForm1.PlayerBehaviours0Collision(Sender: TObject;
  ObjectCollided: TGLBaseSceneObject; CollisionInfo: TDCECollision);
var v: TAffineVector;
begin
  //Use some kind of identifier to know what object you are colliding
  //You can use the Tag, TagFloat, Name, Class
  if ObjectCollided.Tag = 1 then
  begin
    v := AffineVectorMake(VectorSubtract(ObjectCollided.AbsolutePosition,Player.AbsolutePosition));
    NormalizeVector(v);
    ScaleVector(v,400);
    GetOrCreateDCEDynamic(ObjectCollided).StopAbsAccel;
    GetOrCreateDCEDynamic(ObjectCollided).ApplyAbsAccel(v);
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var s: string;
begin
  if GLDCEManager1.ManualStep then s := 'Manual'
  else s := 'Automatic';
  GLHUDText1.Text := Format('FPS: %.1f - Dynamics: %d - Statics: %d - Step mode: %s',
    [GLSceneViewer1.FramesPerSecond, GLDCEManager1.DynamicCount,GLDCEManager1.StaticCount,s]);
  GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TForm1.GLDirectOpenGL1Render(Sender: TObject;
  var rci: TRenderContextInfo);
var i: integer;
    p,n: TAffineVector;
begin
  //To use this you will need to enable the debug define in the
  //GLEllipseCollision.pas, if you do, don't forget to clear the
  //triangle list! -> SetLength(debug_tri,0);

  glPointSize(5.0);
  glColor3f(0,1,0);
  glPushAttrib(GL_LIGHTING_BIT);
  glDisable(GL_LIGHTING);

  for i := 0 to High(debug_tri) do
  with debug_tri[i] do begin

    {if d_TriList[i].Collided then
    begin

      glColor3f(1,0,0);
      glBegin(GL_POLYGON);
        glVertex3f(p1[0],p1[1],p1[2]);
        glVertex3f(p2[0],p2[1],p2[2]);
        glVertex3f(p3[0],p3[1],p3[2]);
      glEnd;

    end;}

    glColor3f(0,0,0);
    glBegin(GL_LINE_STRIP);
      glVertex3f(p1[0],p1[1],p1[2]);
      glVertex3f(p2[0],p2[1],p2[2]);
      glVertex3f(p3[0],p3[1],p3[2]);
    glEnd;
    CalcPlaneNormal(p1,p2,p3,n);
    ScaleVector(n,0.25);
    p[0] := (p1[0] + p2[0] + p3[0]) / 3;
    p[1] := (p1[1] + p2[1] + p3[1]) / 3;
    p[2] := (p1[2] + p2[2] + p3[2]) / 3;
    glColor3f(0,0,1);
    glBegin(GL_LINE_STRIP);
      glVertex3f(p[0],p[1],p[2]);
      glVertex3f(p[0]+n[0],p[1]+n[1],p[2]+n[2]);
    glEnd;
    glBegin(GL_POINTS);
      glVertex3f(p[0]+n[0],p[1]+n[1],p[2]+n[2]);
    glEnd;

  end; //}

  //glEnable(GL_DEPTH_TEST);
  glPopAttrib;
  SetLength(debug_tri,0);
end;


initialization
  {$i Unit1.lrs}

end.
