unit glgizmo;

interface

uses classes, vectorgeometry, glscene, gltexture, glmisc, opengl1x, sysutils,
   vectorlists, glcrossplatform, glcontext, glsilhouette, globjects,
   geometrybb;


type
  TGizmoArrowType = (gtAxisX, gtAxisY, gtAxisZ);
  TGizmoCornerType = (gtPlaneXY, gtPlaneXZ, gtPlaneYZ);

  TGLGizmoArrow = class (TGLQuadricObject)
  private
    FTransformPosition: TVector;
    FGizmoType: TGizmoArrowType;
    FBottomRadius: TGLFloat;
    FHeight: TGLFloat;
    FArrowHeadHeight: TGLFloat;
    FSelected: Boolean;
    FSelectedColor: TGLColor;
    procedure SetGizmoType(const Value: TGizmoArrowType);
    procedure SetBottomRadius(const Value: TGLFloat);
    procedure SetHeight(const Value: TGLFloat);
    procedure SetArrowHeadHeight(const Value: TGLFloat);
    procedure SetSelected(const Value: Boolean);
    procedure SetSelectedColor(const Value: TGLColor);
    { Private Declarations }
  protected
    { Protected Declarations }
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override; 
    procedure Assign(Source: TPersistent); override;

    procedure BuildList(var rci : TRenderContextInfo); override;

    procedure SetTransformPosition(Positio: TVector);
  published
    { Public Declarations }
    property GizmoType: TGizmoArrowType read FGizmoType write SetGizmoType;
    property BottomRadius: TGLFloat read FBottomRadius write SetBottomRadius;
    property Height: TGLFloat read FHeight write SetHeight;
    property ArrowHeadHeight: TGLFloat read FArrowHeadHeight write SetArrowHeadHeight;
    property Selected: Boolean read FSelected write SetSelected;
    property SelectedColor: TGLColor read FSelectedColor write SetSelectedColor;
  end;


	TGLGizmoCorner = class (TGLQuadricObject)
  private
    FTransformPosition: TVector;
    FGizmoType: TGizmoCornerType;
    FHeight: TGLFloat;
    FDistance: TGLFloat;
    FSelected: Boolean;
    FSelectedColor: TGLColor;
    procedure SetGizmoType(const Value: TGizmoCornerType);
    procedure SetHeight(const Value: TGLFloat);
    procedure SetDistance(const Value: TGLFloat);
    procedure SetSelected(const Value: Boolean);
    procedure SetSelectedColor(const Value: TGLColor);
    { Private Declarations }
  protected
    { Protected Declarations }
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure BuildList(var rci : TRenderContextInfo); override;

    procedure SetTransformPosition(Positio: TVector);
  published
    { Public Declarations }
    property GizmoType: TGizmoCornerType read FGizmoType write SetGizmoType;
    property Height: TGLFloat read FHeight write SetHeight;
    property Distance: TGLFloat read FDistance write SetDistance;
    property Selected: Boolean read FSelected write SetSelected;
    property SelectedColor: TGLColor read FSelectedColor write SetSelectedColor;
  end;


	TGLBoundingBox = class(TGLCustomSceneObject)
  private
    FFactor: Single;
    BB: THmgBoundingBox;
    FAssignedObject: TGLCustomSceneObject;
    procedure SetFactor(const Value: Single);
    { Private Declarations }
  protected
    { Protected Declarations }
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure UpdatePosition;

    procedure BuildList(var rci : TRenderContextInfo); override;
    procedure AssignObj(AObject: TGLCustomSceneObject);
    property AssignedObject: TGLCustomSceneObject read FAssignedObject;
  published
    { Public Declarations }
    property Factor: Single read FFactor write SetFactor;
  end;

const
  ACorners: array [0..7, 0..2] of byte = ((1, 3, 4),
                                          (0, 2, 5),
                                          (1, 6, 3),
                                          (0, 2, 7),
                                          (0, 5, 7),
                                          (1, 4, 6),
                                          (2, 5, 7),
                                          (3, 4, 6));

implementation

{ TGLGizmoArrow }

procedure TGLGizmoArrow.Assign(Source: TPersistent);
begin
  inherited;
end;

procedure TGLGizmoArrow.BuildList(var rci: TRenderContextInfo);
var
  color: TColorVector;
	quadric : PGLUquadricObj;
  MultiplierX,
  MultiplierY,
  MultiplierZ: Byte;
begin

  case FGizmoType of
    gtAxisX : begin
                color := clrRed;
                MultiplierX := 1;
                MultiplierY := 0;
                MultiplierZ := 0;
              end;
    gtAxisY : begin
                color := clrGreen;
                MultiplierX := 0;
                MultiplierY := 1;
                MultiplierZ := 0;
              end;
    gtAxisZ : begin
                color := clrBlue;
                MultiplierX := 0;
                MultiplierY := 0;
                MultiplierZ := 1;
              end;
  end;

  GLdisable( GL_DEPTH_TEST );

//  glPushAttrib(GL_ENABLE_BIT or GL_CURRENT_BIT or GL_LIGHTING_BIT or GL_LINE_BIT or GL_COLOR_BUFFER_BIT);
  glPushAttrib(GL_ALL_ATTRIB_BITS);
  glDisable(GL_LIGHTING);
  glEnable(GL_LINE_SMOOTH);

  glTranslatef(FTransformPosition[0], FTransformPosition[1], FTransformPosition[2]);

  glLineWidth(1);
//   ResetGLMaterialColors;
  glColorMaterial(GL_FRONT, GL_EMISSION);
  glEnable(GL_COLOR_MATERIAL);
  if Selected then
  begin
    glColor4fv(SelectedColor.AsAddress);
  end
  else
  begin
    glColor4fv(@color);
  end;

  glBegin(GL_LINES);
    glVertex3f(0, 0, 0);
    glVertex3f((FHeight - FArrowHeadHeight) * MultiplierX,
               (FHeight - FArrowHeadHeight) * MultiplierY,
               (FHeight - FArrowHeadHeight) * MultiplierZ);
  glEnd;


  glPushMatrix;
  glColor4fv(@color);
	quadric:=gluNewQuadric();
	SetupQuadricParams(Quadric);

  case FGizmoType of
    gtAxisX : begin
                glRotated(90, 0, 1, 0);
                glTranslatef(0, 0, FHeight - FArrowHeadHeight); //{FHeight}0.2*0.5
              end;
    gtAxisY : begin
                glRotated(-90, 1, 0, 0);
                glTranslatef(0, 0, FHeight - FArrowHeadHeight); //{FHeight}0.2*0.5
              end;
    gtAxisZ : begin
                glRotated(-90, 0, 0, 1);
                glTranslatef(0, 0, FHeight - FArrowHeadHeight); //{FHeight}0.2*0.5
              end;
  end;


  gluCylinder(quadric, BottomRadius, 0, FArrowHeadHeight, {Slices} 3, {Stacks}1);

  SetInvertedQuadricOrientation(quadric);
  glColor4fv(@color);
	gluDisk(quadric, 0, BottomRadius, {Slices}8, {FLoops}1);

	gluDeleteQuadric(Quadric);

  glPopMatrix;

  glPopAttrib;

  { Switch back to the normal rendering mode }
  GLenable( GL_DEPTH_TEST );
end;

constructor TGLGizmoArrow.Create(AOwner: TComponent);
begin
  inherited;
  FBottomRadius := 0.05;
  FHeight := 1;
  FArrowHeadHeight := 0.2;
  ObjectStyle := ObjectStyle + [osDirectDraw];
  FSelected := False;
  FSelectedColor := TGLColor.Create(Self);
  FSelectedColor.Color := clrYellow;
end;

destructor TGLGizmoArrow.Destroy;
begin
  FSelectedColor.Free;
  inherited;
end;

procedure TGLGizmoArrow.SetArrowHeadHeight(const Value: TGLFloat);
begin
  if FArrowHeadHeight <> Value then
  begin
    FArrowHeadHeight := Value;
    StructureChanged;
  end;
end;

procedure TGLGizmoArrow.SetBottomRadius(const Value: TGLFloat);
begin
  if FBottomRadius <> Value then
  begin
    FBottomRadius := Value;
    StructureChanged;
  end;
end;

procedure TGLGizmoArrow.SetGizmoType(const Value: TGizmoArrowType);
begin
  if FGizmoType <> Value then
  begin
    FGizmoType := Value;
    StructureChanged;
  end;
end;

procedure TGLGizmoArrow.SetHeight(const Value: TGLFloat);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    StructureChanged;
  end;
end;

procedure TGLGizmoArrow.SetSelected(const Value: Boolean);
begin
  if FSelected <> Value then
  begin
    FSelected := Value;
    NotifyChange(Self);
  end;
end;

procedure TGLGizmoArrow.SetSelectedColor(const Value: TGLColor);
begin
  FSelectedColor.Color := Value.Color;
  NotifyChange(Self);
end;

procedure TGLGizmoArrow.SetTransformPosition(Positio: TVector);
begin
  FTransformPosition := Positio;
end;

{ TGLGizmoCorner }

procedure TGLGizmoCorner.Assign(Source: TPersistent);
begin
  inherited;

end;

procedure TGLGizmoCorner.BuildList(var rci: TRenderContextInfo);
var
  color: TColorVector;
  MultiplierX,
  MultiplierY,
  MultiplierZ: Byte;
begin
  GLdisable( GL_DEPTH_TEST );

  case FGizmoType of
    gtPlaneXY : begin
                  MultiplierX := 1;
                  MultiplierY := 1;
                  MultiplierZ := 0;
                end;
    gtPlaneXZ : begin
                  MultiplierX := 1;
                  MultiplierY := 0;
                  MultiplierZ := 1;
                end;
    gtPlaneYZ : begin
                  MultiplierX := 0;
                  MultiplierY := 1;
                  MultiplierZ := 1;
                end;
  end;

//  glPushAttrib(GL_ENABLE_BIT or GL_CURRENT_BIT or GL_LIGHTING_BIT or GL_LINE_BIT or GL_COLOR_BUFFER_BIT);
  glPushAttrib(GL_ALL_ATTRIB_BITS);
  glDisable(GL_LIGHTING);
  glEnable(GL_LINE_SMOOTH);

  glLineWidth(1);

  glColorMaterial(GL_FRONT, GL_EMISSION);
  glEnable(GL_COLOR_MATERIAL);

  glTranslatef(FTransformPosition[0], FTransformPosition[1], FTransformPosition[2]);

  if Selected then
  begin
    glColor4fv(SelectedColor.AsAddress);
  end
  else
  begin
    if MultiplierY = 1 then
    begin
      color := clrGreen;
      glColor4fv(@color);
    end
    else
    begin
      color := clrBlue;
      glColor4fv(@color);
    end;
  end;

  glBegin(GL_LINES);
    // x part of the corner
    glVertex3f(FDistance * MultiplierX, FDistance * MultiplierY, FDistance * MultiplierZ);
    glVertex3f((FDistance - FHeight) * MultiplierX, FDistance * MultiplierY, FDistance * MultiplierZ);
  glEnd;

  if Selected then
  begin
    glColor4fv(SelectedColor.AsAddress);
  end
  else
  begin
    if MultiplierX = 1 then
    begin
      color := clrRed;
      glColor4fv(@color);
    end
    else
    begin
      color := clrBlue;
      glColor4fv(@color);
    end;
  end;

  glBegin(GL_LINES);
    //y part of the corner
    glVertex3f(FDistance * MultiplierX, FDistance * MultiplierY, FDistance * MultiplierZ);
    glVertex3f(FDistance * MultiplierX, (FDistance - FHeight) * MultiplierY, FDistance * MultiplierZ);
  glEnd;

  if Selected then
  begin
    glColor4fv(SelectedColor.AsAddress);
  end
  else
  begin
    if MultiplierX = 1 then
    begin
      color := clrRed;
      glColor4fv(@color);
    end
    else
    begin
      color := clrGreen;
      glColor4fv(@color);
    end;
  end;

  glBegin(GL_LINES);
    //z part of the corner
    glVertex3f(FDistance * MultiplierX, FDistance * MultiplierY, FDistance * MultiplierZ);
    glVertex3f(FDistance * MultiplierX, FDistance * MultiplierY, (FDistance - FHeight) * MultiplierZ);
  glEnd;

  glPopAttrib;

  { Switch back to the normal rendering mode }
  GLenable( GL_DEPTH_TEST );
end;

constructor TGLGizmoCorner.Create(AOwner: TComponent);
begin
  inherited;
  FHeight := 0.2;
  FDistance := 1;
  ObjectStyle := ObjectStyle + [osDirectDraw];

  FSelectedColor := TGLColor.Create(Self);
  FSelectedColor.Color := clrYellow;
end;

destructor TGLGizmoCorner.Destroy;
begin
  FSelectedColor.Free;
  inherited;
end;

procedure TGLGizmoCorner.SetDistance(const Value: TGLFloat);
begin
  if FDistance <> Value then
  begin
    FDistance := Value;
    StructureChanged;
  end;
end;

procedure TGLGizmoCorner.SetGizmoType(const Value: TGizmoCornerType);
begin
  if FGizmoType <> Value then
  begin
    FGizmoType := Value;
    StructureChanged;
  end;
end;

procedure TGLGizmoCorner.SetHeight(const Value: TGLFloat);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    StructureChanged;
  end;
end;

procedure TGLGizmoCorner.SetSelected(const Value: Boolean);
begin
  if FSelected <> Value then
  begin
    FSelected := Value;
    NotifyChange(Self);
  end;
end;

procedure TGLGizmoCorner.SetSelectedColor(const Value: TGLColor);
begin
  FSelectedColor.Color := Value.Color;
  NotifyChange(Self);
end;

procedure TGLGizmoCorner.SetTransformPosition(Positio: TVector);
begin
  FTransformPosition := Positio;
end;

{ TGLBoundingBox }

procedure TGLBoundingBox.Assign(Source: TPersistent);
begin
  inherited;

end;

procedure TGLBoundingBox.AssignObj(AObject: TGLCustomSceneObject);
begin
  FAssignedObject := AObject;
  if AObject = nil then exit;

  Position.AsVector := AObject.AbsolutePosition;
  Up.AsVector := AObject.AbsoluteUp;
  Direction.AsVector := AObject.AbsoluteDirection;
  BB := AObject.BoundingBox;
  NotifyChange(Self);
end;

procedure TGLBoundingBox.BuildList(var rci: TRenderContextInfo);
var
  AVector: TVector;
  color: TColorVector;
  i, j: Byte;
begin
//  glPushAttrib(GL_ENABLE_BIT or GL_CURRENT_BIT or GL_LIGHTING_BIT or GL_LINE_BIT or GL_COLOR_BUFFER_BIT);
  glPushAttrib(GL_ALL_ATTRIB_BITS);
  glDisable(GL_LIGHTING);
  glEnable(GL_LINE_SMOOTH);

  glLineWidth(1);

  glColorMaterial(GL_FRONT, GL_EMISSION);
  glEnable(GL_COLOR_MATERIAL);

  color := clrWhite;
  glColor4fv(@color);

  for i := 0 to 7 do
  begin
    for j := 0 to 2 do
    begin
      AVector := VectorSubtract(BB[ACorners[i][j]], BB[i]);
      AVector := VectorScale(AVector, 0.25);
      AVector := VectorAdd(AVector, BB[i]);

      glBegin(GL_LINES);
        glVertex3f(BB[i][0], BB[i][1], BB[i][2]);
        glVertex3f(AVector[0], AVector[1], AVector[2]);
      glEnd;
    end;
  end;

  glPopAttrib;
end;

constructor TGLBoundingBox.Create(AOwner: TComponent);
begin
  inherited;
  FFactor := 0.25;
  ObjectStyle := ObjectStyle + [osDirectDraw];
end;

destructor TGLBoundingBox.Destroy;
begin

  inherited;
end;

procedure TGLBoundingBox.SetFactor(const Value: Single);
begin
  if FFactor <> Value then
  begin
    FFactor := Value;
    NotifyChange(Self);
  end;
end;

procedure TGLBoundingBox.UpdatePosition;
begin
  Position.AsVector := FAssignedObject.AbsolutePosition;
  Up.AsVector := FAssignedObject.AbsoluteUp;
  Direction.AsVector := FAssignedObject.AbsoluteDirection;
  BB := FAssignedObject.BoundingBox;
  NotifyChange(Self);
end;

initialization
  RegisterClasses([TGLGizmoCorner, TGLGizmoArrow, TGLBoundingBox]);

finalization

end.
