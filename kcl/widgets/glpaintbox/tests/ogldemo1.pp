// $Id$

{$MODE objfpc}
{$H+}

program ogldemo1;
uses SysUtils, Classes, GL, KCL, GLPaintBox;

type

  TMainForm = class(TForm)
  protected
    glbox: TGLPaintBox;

    procedure OnInitGLBox(Sender: TObject);
    procedure OnPaintGLBox(Sender: TObject; ACanvas: TCanvas;
      const ARect: TRect);
  public
    constructor Create;
  end;


constructor TMainForm.Create;
begin
  inherited Create(nil);
  Text := Application.Title;
  SetDefaultSize(600, 400);
  glbox := TGlPaintBox.Create(Self);
  glbox.OnInitGL := @OnInitGLBox;
  glbox.OnPaint := @OnPaintGLBox;
  Content := glbox;
end;

procedure TMainForm.OnInitGLBox(Sender: TObject);
begin
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  gluOrtho2D(0, 100, 100, 0);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
end;

procedure TMainForm.OnPaintGLBox(Sender: TObject; ACanvas: TCanvas;
  const ARect: TRect);
begin
  ACanvas.Line(0, 0, Width, Height);

  if not glbox.BeginGL then exit;

  glClearColor(0, 0.2, 0, 1);
  glClear([GL_COLOR_BUFFER_BIT]);

  glBegin(GL_TRIANGLES);
  glColor3f(1, 0, 0);
  glVertex2f(10, 10);
  glColor3f(0, 1, 0);
  glVertex2f(10, 90);
  glColor3f(0, 0, 1);
  glVertex2f(90, 90);
  glEnd;

  glbox.EndGL;
  glbox.SwapBuffers;
end;


begin
  Application.Initialize;
  Application.Title := 'KCL GL Demo #1';

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
