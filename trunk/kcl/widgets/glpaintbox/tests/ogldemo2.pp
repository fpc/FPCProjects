// $Id$

{$MODE objfpc}
{$H+}

program ogldemo2;
uses SysUtils, Classes, KCL, GLPaintBox, GL;

const

  KCLImg: array[0..4, 0..12] of Byte =
    ((1, 0, 0, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0),
     (1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0),
     (1, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0),
     (1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0),
     (1, 0, 0, 1, 0, 0, 1, 1, 0, 1, 1, 1, 1));

type

  TMainForm = class(TForm)
  protected
    glbox: TGLPaintBox;
    timer: TTimer;
    counter: Integer;
    tex: array[0..63, 0..63, 0..2] of Byte;

    procedure OnInitGLBox(Sender: TObject);
    procedure OnPaintGLBox(Sender: TObject; ACanvas: TCanvas;
      const ARect: TRect);
    procedure OnTimer(Sender: TObject);
  public
    constructor Create;
  end;


const
  colors: array[0..7, 0..2] of Single =
    ((0, 0, 0), (0, 0, 1), (0, 1, 0), (0, 1, 1),
     (1, 0, 0), (1, 0, 1), (1, 1, 0), (1, 1, 1));
  corners: array[0..7, 0..2] of Single =
    ((-1, -1, -1), (+1, -1, -1), (+1, +1, -1), (-1, +1, -1),
     (-1, -1, +1), (+1, -1, +1), (+1, +1, +1), (-1, +1, +1));


procedure DrawCube;
  procedure DrawSide(i1, i2, i3, i4: Integer);
  begin
    glColor4f (colors [i1, 0], colors [i1, 1], colors [i1, 2], 0.5);
    glVertex3f(corners[i1, 0], corners[i1, 1], corners[i1, 2]);
    glColor4f (colors [i2, 0], colors [i2, 1], colors [i2, 2], 0.5);
    glVertex3f(corners[i2, 0], corners[i2, 1], corners[i2, 2]);
    glColor4f (colors [i3, 0], colors [i3, 1], colors [i3, 2], 0.5);
    glVertex3f(corners[i3, 0], corners[i3, 1], corners[i3, 2]);
    
    glVertex3f(corners[i4, 0], corners[i4, 1], corners[i4, 2]);
  end;
begin
  glBegin(GL_QUADS);
  DrawSide(4, 5, 6, 7);		// Front
  DrawSide(3, 2, 1, 0);		// Back
  DrawSide(2, 3, 7, 6);		// Top
  DrawSide(0, 1, 5, 4);		// Bottom
  DrawSide(4, 7, 3, 0);		// Left
  DrawSide(1, 2, 6, 5);		// Right
  glEnd;
end;

procedure DrawInvCube;
  procedure DrawSide(i1, i2, i3, i4: Integer);
  begin
    glTexCoord2f(0, 0);
    glVertex3f(corners[i1, 0], corners[i1, 1], corners[i1, 2]);

    glTexCoord2f(4, 0);
    glVertex3f(corners[i2, 0], corners[i2, 1], corners[i2, 2]);

    glTexCoord2f(4, 4);
    glVertex3f(corners[i3, 0], corners[i3, 1], corners[i3, 2]);

    glTexCoord2f(0, 4);
    glVertex3f(corners[i4, 0], corners[i4, 1], corners[i4, 2]);
  end;
begin
  glBegin(GL_QUADS);
  glColor3f(1, 1, 1);
  DrawSide(7, 6, 5, 4);		// Front
  DrawSide(0, 1, 2, 3);		// Back
  DrawSide(6, 7, 3, 2);		// Top
  DrawSide(4, 5, 1, 0);		// Bottom
  DrawSide(0, 3, 7, 4);		// Left
  DrawSide(5, 6, 2, 1);		// Right
  glEnd;
end;


constructor TMainForm.Create;
var
  x, y: Integer;
begin
  inherited Create(nil);
  Text := Application.Title;
  SetDefaultSize(600, 400);
  glbox := TGlPaintBox.Create(Self);
  glbox.OnInitGL := @OnInitGLBox;
  glbox.OnPaint := @OnPaintGLBox;
  Content := glbox;

  timer := TTimer.Create(Self);
  timer.Enabled := False;
  timer.Interval := 40;
  timer.OnTimer := @OnTimer;

  // Create wall texture
  for y := 0 to 63 do
    for x := 0 to 63 do begin
      tex[y, x, 0] := Round(Sin(x * 0.049867) * 127 + 128);
      tex[y, x, 2] := Round(Sin(y * 0.049867) * 127 + 128);
    end;
end;

procedure TMainForm.OnInitGLBox(Sender: TObject);
begin
  WriteLn('GL info:');
  WriteLn('  Vendor: ', glGetString(GL_VENDOR));
  WriteLn('  Renderer: ', glGetString(GL_RENDERER));
  WriteLn('  Version: ', glGetString(GL_VERSION));
  WriteLn('  Extensions: ', glGetString(GL_EXTENSIONS));

  // Enable backface culling
  glEnable(GL_CULL_FACE);

  // Set up depth buffer
  glEnable(GL_DEPTH_TEST);
  glDepthFunc(GL_LESS);

  // Set up projection matrix
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  gluPerspective(90, 1.3, 0.1, 100);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
  glTranslatef(0, 0, -5.5);
end;


procedure TMainForm.OnPaintGLBox(Sender: TObject; ACanvas: TCanvas;
  const ARect: TRect);
begin
  ACanvas.Line(0, 0, Width, Height);

  if not timer.Enabled then timer.Enabled := True;
  Dec(counter);
  OnTimer(Sender);
end;


procedure TMainForm.OnTimer(Sender: TObject);
var
  x, y: Integer;

begin
  Inc(counter);
  if not glbox.BeginGL then exit;

  glClearColor(0, 0.2, 0, 1);
  glClear([GL_DEPTH_BUFFER_BIT]);

  glPushMatrix;
  glScalef(20, 20, 20);
  glRotatef(counter, 0, 1, 0);

  glEnable(GL_TEXTURE_2D);
  glTexImage2D(GL_TEXTURE_2D, 0 {mip level}, GL_RGB, 64, 64, 0 {border}, GL_RGB, GL_BYTE, @tex);
  DrawInvCube;
  glDisable(GL_TEXTURE_2D);
  glPopMatrix;

  glPushMatrix;
  glTranslatef(0, 0, Sin(Single(counter) / 20.0) * 5.0 - 5.0);
  glRotatef(Sin(Single(counter) / 200.0) * 720.0, 0, 1, 0);
  glRotatef(counter, 0, 0, 1);

  for y := 0 to 4 do begin
    for x := 0 to 12 do
      if KCLImg[y, x] > 0 then begin
        glPushMatrix;
	glRotatef(x * Sin(Single(counter) / 5.0), 0, 1, 0);
	glRotatef(y * Sin(Single(counter) / 12.0) * 4.0, 0, 0, 1);
        glTranslatef((x - 6) * 1, (2 - y) * 1, 0);
        glScalef(0.4, 0.4, 0.4);
        glRotatef(counter, 0.5, 1, 0);
        DrawCube;
        glPopMatrix;
      end;
  end;

  glPopMatrix;

  glbox.EndGL;
  glbox.SwapBuffers;
  Inc(counter);
end;


begin
  Application.Initialize;
  Application.Title := 'KCL GL Demo #2';

  if not InitGl then begin
    WriteLn('OpenGL is not supported on this system');
    Halt(2);
  end;

  if not InitGLU then begin
    WriteLn('Couldn''t load GLU module');
    Halt(3);
  end;

  Application.AddForm(TMainForm.Create);
  Application.Run;
end.


{
  $Log$
  Revision 1.1  1999/12/31 19:21:42  sg
  * Initial version for the new KCL

}
