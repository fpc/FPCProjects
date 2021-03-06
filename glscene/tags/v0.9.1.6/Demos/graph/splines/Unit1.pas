{: This is a quick demo for the TGLLines object and spline functionality.<p>

   TGLLines can handle normal lines and cubic splines, each node can have a
   different color, and the line can be color-interpolated.<p>

   Note that the camera in this sample is in <i>orthogonal</i> mode, this makes
   for a quick and easy way to work in 2D with OpenGL (try switching the camera
   to perpective mode if you don't see the point).
}
unit Unit1;

{$MODE Delphi}

interface

uses
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  GLObjects, GLMisc, GLLCLViewer, LResources, GLScene;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    Lines1: TGLLines;
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    procedure MoveCenterNodeTo(x, y : Integer);
  end;

var
  Form1: TForm1;

implementation


uses VectorGeometry;

procedure TForm1.MoveCenterNodeTo(x, y : Integer);
begin
   Lines1.Nodes[1].AsAffineVector:=GLSceneViewer1.Buffer.ScreenToWorld(x, y);
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   MoveCenterNodeTo(x, y);
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
   if Shift<>[] then
      MoveCenterNodeTo(x, y);
end;

initialization
  {$i Unit1.lrs}

end.
