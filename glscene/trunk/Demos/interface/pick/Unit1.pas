{: Basic interactive object picking<p>

	This is a bare bones sample on the use of the GetPickedObject function.
	Two events are handled : OnMouseMove triggers a color change (grey/red) when
	the mouse is moved over an object, and a message popups when an object is
	clicked in OnMouseDown.<p>

	In a real world proggie, both events should make use of the oldPick variable
	(since you can't click what is not under the mouse, the GetPickedObject in
	OnMouseDown returns what we already have in oldPick, set during the last
	OnMouseMove).
}
unit Unit1;

{$MODE Delphi}

interface

uses
  Forms, GLObjects, GLTexture, Classes, Controls, Dialogs,
  GLGeomObjects, LResources, GLViewer, GLScene, GLColor;

type

  { TForm1 }

  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    DummyCube1: TGLDummyCube;
    GLLightSource1: TGLLightSource;
    Sphere: TGLSphere;
    Cylinder: TGLCylinder;
    Torus: TGLTorus;
    Cone: TGLCone;
procedure FormCreate(Sender: TObject);
procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
		X, Y: Integer);
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
	 { D�clarations priv�es }
	 oldPick : TGLCustomSceneObject;
  public
	 { D�clarations publiques }
  end;

var
  Form1: TForm1;

implementation


procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
	pick : TGLCustomSceneObject;
begin
	// find what's under the mouse
	pick:=(GLSceneViewer1.Buffer.GetPickedObject(x, y) as TGLCustomSceneObject);
	// if it has changed since last MouseMove...
	if (pick<>oldPick) then begin
		// ...turn to black previous "hot" object...
		if Assigned(oldPick) then
			oldPick.Material.FrontProperties.Emission.Color:=clrBlack;
		// ...and heat up the new selection...
		if Assigned(pick) then
			pick.Material.FrontProperties.Emission.Color:=clrRed;
		// ...and don't forget it !
		oldPick:=pick;
	end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin

end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
	pick : TGLCustomSceneObject;
begin
	// if an object is picked...
	pick:=(GLSceneViewer1.Buffer.GetPickedObject(x, y) as TGLCustomSceneObject);
	if Assigned(pick) then begin
		// ...turn it to yellow and show its name
		pick.Material.FrontProperties.Emission.Color:=clrYellow;
		ShowMessage('You clicked the '+pick.Name);
	end;
end;

initialization
  {$i Unit1.lrs}

end.
