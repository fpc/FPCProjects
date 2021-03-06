{: Loading NURBS into a GLScene FreeForm/Actor object<p>

   A very simple parametric model of a duck, comprised of 3 NURBS
   surfaces. The Nurbs format is essentially the NurbsSurface geometry 
   type used in VRML. One limitation at the moment is the Control points
   must each be on a separate line. Inverted surfaces are handled with 
   the ccw FALSE statement in the .nurbs file (duck3.nurbs uses this 
   setting).<p>
   
   Use the resolution slider to increase or decrease the models triangle
   count dynamically.<p>
}
unit Unit1;

{$MODE Delphi}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, GLScene, GLVectorFileObjects, GLObjects,
  ExtCtrls, ComCtrls, StdCtrls, GLTexture, LResources, GLCadencer,
  GLViewer, GLMaterial;

type

  { TForm1 }

  TForm1 = class(TForm)
    GLCadencer1: TGLCadencer;
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    GLDummyCube1: TGLDummyCube;
    GLLightSource1: TGLLightSource;
    Panel1: TPanel;
    GLSceneViewer1: TGLSceneViewer;
    TrackBar1: TTrackBar;
    Label1: TLabel;
    CheckBox1: TCheckBox;
    GLActor1: TGLActor;
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    mx,my : Integer;
  end;

var
  Form1: TForm1;

implementation


uses GLFileNurbs, GLParametricSurfaces, VectorGeometry, VectorLists;

procedure TForm1.FormCreate(Sender: TObject);
var
  cp : TAffineVectorList;
begin
  // Load the nurbs data
  GLActor1.LoadFromFile('..' + PathDelim + '..' + PathDelim + 'media' + PathDelim + 'duck1.nurbs');
  GLActor1.AddDataFromFile('..' + PathDelim + '..' + PathDelim + 'media' + PathDelim + 'duck2.nurbs');
  GLActor1.AddDataFromFile('..' + PathDelim + '..' + PathDelim + 'media' + PathDelim + 'duck3.nurbs');

  { Translate Actor based on the first mesh object's average
    control point. Quick and dirty ... or maybe just dirty :P }
  cp:=TMOParametricSurface(GLActor1.MeshObjects[0]).ControlPoints;
  GLActor1.Position.Translate(VectorNegate(VectorScale(cp.Sum,1/cp.Count)));
end;

procedure TForm1.Panel1Click(Sender: TObject);
begin

end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  mx:=x;
  my:=y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then
    GLCamera1.MoveAroundTarget(my-y,mx-x);
  mx:=x;
  my:=y;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
var
  i : integer;
begin
  for i:=0 to 2 do
    TMOParametricSurface(GLActor1.MeshObjects[i]).Resolution:=TrackBar1.Position;
  GLActor1.StructureChanged;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  with GLActor1.Material do begin
    if Checkbox1.Checked then begin
      FrontProperties.PolygonMode:=pmLines;
      FaceCulling:=fcNoCull;
    end else begin
      FrontProperties.PolygonMode:=pmFill;
      FaceCulling:=fcBufferDefault;
    end;
  end;
end;

initialization
  {$i Unit1.lrs}

end.
