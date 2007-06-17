unit Unit1;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  {$IFDEF LCL} lcltype, {$ELSE} Windows,{$ENDIF}
  Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, GLScene, GLObjects, GLMisc, GLTexture, GLViewer, GLCadencer,
  ExtCtrls
  {$IFDEF LCL} , LResources {$ENDIF};

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    GLDummyCube1: TGLDummyCube;
    GLCube1: TGLCube;
    GLDirectOpenGL1: TGLDirectOpenGL;
    GLCadencer1: TGLCadencer;
    Timer1: TTimer;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure GLDirectOpenGL1Render(Sender: TObject;
      var rci: TRenderContextInfo);
    procedure Timer1Timer(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
  private
    { Private declarations }
    frame: integer;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$IFNDEF FPC}
{$R *.dfm}
{$ENDIF}


uses
  OpenGL1x, GLUtils, GLContext, GLDynamicTexture;


procedure TForm1.FormCreate(Sender: TObject);
begin
  GLSceneViewer1.Align:= alClient;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  tex: TGLTexture;
  img: TGLDynamicTextureImage;
begin
  tex:= GLMaterialLibrary1.TextureByName('Anim');
  if not (tex.Image is TGLDynamicTextureImage) then
    exit;

  img:= TGLDynamicTextureImage(tex.Image);

  case Key of
    VK_F2: begin
      img.UsePBO:= false;
      GLSceneViewer1.ResetPerformanceMonitor;
    end;
    VK_F3: begin
      img.UsePBO:= true;
      GLSceneViewer1.ResetPerformanceMonitor;
    end;
  end;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  GLCamera1.SceneScale:= GLSceneViewer1.ClientWidth / 400;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  GLSceneViewer1.Invalidate;
end;

procedure TForm1.GLDirectOpenGL1Render(Sender: TObject;
  var rci: TRenderContextInfo);
var
  tex: TGLTexture;
  img: TGLDynamicTextureImage;
  p: PRGBQuad;
  x, y: integer;
begin
  tex:= GLMaterialLibrary1.TextureByName('Anim');
  if tex.Disabled then
  begin
    tex.ImageClassName:= TGLDynamicTextureImage.ClassName;
    img:= TGLDynamicTextureImage(tex.Image);
    img.Width:= 256;
    img.Height:= 256;

    tex.TextureFormat:= tfRGBA;

    tex.TextureMode:= tmReplace;
    tex.Disabled:= false;
  end;

  img:= TGLDynamicTextureImage(tex.Image);

  img.BeginUpdate;

  // draw some silly stuff
  p:= img.Data;
  frame:= frame + 1;
  for y := 0 to img.Height - 1 do
  begin
    for x := 0 to img.Width - 1 do
    begin
      p^.rgbRed:= ((x xor y) + frame) and 255;
      p^.rgbGreen:= ((x + frame) xor y) and 255;
      p^.rgbBlue:= ((x - frame) xor (y + frame)) and 255;
      inc(p);
    end;
  end;

  img.EndUpdate;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
const
  PBOText: array[boolean] of string = ('PBO disabled', 'PBO enabled');
var
  tex: TGLTexture;
  img: TGLDynamicTextureImage;
  s: string;
begin
  tex:= GLMaterialLibrary1.TextureByName('Anim');
  if (tex.Image is TGLDynamicTextureImage) then
  begin
    img:= TGLDynamicTextureImage(tex.Image);
    s:= PBOText[img.UsePBO];
  end;

  Caption:= Format('%s - %s', [GLSceneViewer1.FramesPerSecondText, s]);
  GLSceneViewer1.ResetPerformanceMonitor;
end;

initialization
  {$IFDEF LCL}
  {$i unit1.lrs}
  {$ENDIF}
end.
