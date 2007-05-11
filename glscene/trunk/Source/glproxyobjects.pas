//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLProxyObjects<p>

   Implements specific proxying classes.<p>

	<b>History : </b><font size=-1><ul>
      <li>10/05/07 - DaStr - Bugfixed TGLColorProxy.DoRender
                              (thanks Paul Robello) (Bugtracker ID = 1716692)
      <li>28/03/07 - DaStr - Renamed parameters in some methods
                             (thanks Burkhard Carstens) (Bugtracker ID = 1678658)
      <li>25/02/07 - Made TGLActorProxy.SetAnimation a bit safer
      <li>20/02/07 - DaStr - Redeclared MasterObject of TGLColorProxy and TGLFreeFormProxy
                             Added TGLActorProxy (based on a demo published
                             on the newsgroup by don't know who...)
      <li>18/12/03 - Dave - Dropped "Object" from "ProxyObject" class names
      <li>17/12/03 - Dave - Changed class check in Octree code to Assert
      <li>17/12/03 - Dave+Dan - Added OctreeSphereSweep
      <li>06/12/03 - EG - Creation from GLScene.pas split
   </ul></font>
}
unit GLProxyObjects;

interface

uses
  // VCL
  Classes, SysUtils,

  // GLScene
  GLScene, VectorGeometry, GLMisc, GLTexture, GLSilhouette, GLVectorFileObjects,
  GLStrings;

type
  EGLProxyException = class(Exception);

   // TGLColorProxy
   //
   {: A proxy object with its own color.<p>
      This proxy object can have a unique color. Note that multi-material
      objects (Freeforms linked to a material library f.i.) won't honour
      the color. }
   TGLColorProxy = class (TGLProxyObject)
      private
         { Private Declarations }
         FFrontColor: TGLFaceProperties;
         function GetMasterMaterialObject: TGLCustomSceneObject;
         procedure SetMasterMaterialObject(const Value: TGLCustomSceneObject);

      public
         { Public Declarations }
         constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;

         procedure DoRender(var ARci : TRenderContextInfo;
                            ARenderSelf, ARenderChildren : Boolean); override;
      published
         { Published Declarations }
         property FrontColor: TGLFaceProperties read FFrontColor;
        // Redeclare as TGLCustomSceneObject.
        property MasterObject: TGLCustomSceneObject read GetMasterMaterialObject write SetMasterMaterialObject;
   end;

   // TGLFreeFormProxy
   //
   {: A proxy object specialized for FreeForms.<p> }
   TGLFreeFormProxy = class (TGLProxyObject)
      private
         function GetMasterFreeFormObject: TGLFreeForm;
         procedure SetMasterFreeFormObject(const Value: TGLFreeForm);
      protected
         { Protected Declarations }

      public
         { Public Declarations }

         {: If the MasterObject is a FreeForm, you can raycast against the Octree,
            which is alot faster.  You must build the octree before using. }
         function OctreeRayCastIntersect(const rayStart, rayVector : TVector;
                                         intersectPoint : PVector = nil;
                                         intersectNormal : PVector = nil) : Boolean;
         {: WARNING: This function is not yet 100% reliable with scale+rotation. }
        function OctreeSphereSweepIntersect(const rayStart, rayVector : TVector;
                                        const velocity, radius, modelscale: Single;
                                        intersectPoint : PVector = nil;
                                        intersectNormal : PVector = nil) : Boolean;
      published
         { Published Declarations }
        // Redeclare as TGLFreeForm.
        property MasterObject: TGLFreeForm read GetMasterFreeFormObject write SetMasterFreeFormObject;
   end;

  // TGLActorProxy
  //
  {: A proxy object specialized for Actors.<p> }
  TGLActorProxy = class(TGLProxyObject)
  private
    { Private Declarations }
    FCurrentFrame: Integer;
    FStartFrame: Integer;
    FEndFrame: Integer;
    FCurrentFrameDelta: Single;
    FCurrentTime: TProgressTimes;
    FInterval: Integer;
    FAnimation: TActorAnimationName;
    procedure SetAnimation(const Value: TActorAnimationName);
    procedure SetMasterActorObject(const Value: TGLActor);
    function GetMasterActorObject: TGLActor;
  protected
    { Protected Declarations }
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    procedure DoRender(var ARci : TRenderContextInfo;
                        ARenderSelf, ARenderChildren : Boolean); override;
    procedure DoProgress(const progressTime : TProgressTimes); override;
  published
    { Published Declarations }
    property Interval: Integer read FInterval write FInterval default 0;
    property Animation: TActorAnimationName read FAnimation write SetAnimation;
    // Redeclare as TGLActor.
    property MasterObject: TGLActor read GetMasterActorObject write SetMasterActorObject;
    // Redeclare without pooTransformation
    // (Don't know why it causes the object to be oriented incorrecly.)
    property ProxyOptions default [pooEffects, pooObjects];
  end;

//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------
implementation
//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------

uses OpenGL1x;

// ------------------
// ------------------ TGLColorProxy ------------------
// ------------------

// Create
//
constructor TGLColorProxy.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFrontColor:=TGLFaceProperties.Create(Self);
end;

// Destroy
//
destructor TGLColorProxy.Destroy;
begin
   FFrontColor.Free;

   inherited Destroy;
end;

// Render
//
procedure TGLColorProxy.DoRender(var ARci : TRenderContextInfo;
                                  ARenderSelf, ARenderChildren : Boolean);
var
   gotMaster, masterGotEffects, oldProxySubObject : Boolean;
begin
   if FRendering then Exit;
   FRendering:=True;
   try
      gotMaster:=Assigned(MasterObject);
      masterGotEffects:=gotMaster and (pooEffects in ProxyOptions)
                        and (MasterObject.Effects.Count>0);
      if gotMaster then begin
         if pooObjects in ProxyOptions then begin
            oldProxySubObject:=ARci.proxySubObject;
            ARci.proxySubObject:=True;
            if pooTransformation in ProxyOptions then
               glMultMatrixf(PGLFloat(MasterObject.MatrixAsAddress));
            GetMasterMaterialObject.Material.FrontProperties.Assign(FFrontColor);
            MasterObject.DoRender(ARci, ARenderSelf, MasterObject.Count > 0);
            ARci.proxySubObject:=oldProxySubObject;
         end;
      end;
      // now render self stuff (our children, our effects, etc.)
      if ARenderChildren and (Count>0) then
         Self.RenderChildren(0, Count-1, ARci);
      if masterGotEffects then
         MasterObject.Effects.RenderPostEffects(Scene.CurrentBuffer, ARci);
   finally
      FRendering:=False;
   end;
   ClearStructureChanged;
end;

// GetMasterMaterialObject
//
function TGLColorProxy.GetMasterMaterialObject: TGLCustomSceneObject;
begin
  Result := TGLCustomSceneObject(inherited MasterObject);
end;

// SetMasterMaterialObject
//
procedure TGLColorProxy.SetMasterMaterialObject(
  const Value: TGLCustomSceneObject);
begin
  Assert(Value is TGLCustomSceneObject, ClassName + ' accepts only TGLCustomSceneObject as master!');
  SetMasterObject(Value);
end;

// ------------------
// ------------------ TGLFreeFormProxy ------------------
// ------------------

// OctreeRayCastIntersect
//
function TGLFreeFormProxy.OctreeRayCastIntersect(const rayStart, rayVector : TVector;
                                 intersectPoint : PVector = nil;
                                 intersectNormal : PVector = nil) : Boolean;
var
   localRayStart, localRayVector : TVector;
begin
   if Assigned(MasterObject) then begin
      Assert(MasterObject is TGLFreeForm);
      SetVector(localRayStart, AbsoluteToLocal(rayStart));
      SetVector(localRayStart, MasterObject.LocalToAbsolute(localRayStart));
      SetVector(localRayVector, AbsoluteToLocal(rayVector));
      SetVector(localRayVector, MasterObject.LocalToAbsolute(localRayVector));
      NormalizeVector(localRayVector);

      Result:=TGLFreeForm(MasterObject).OctreeRayCastIntersect(localRayStart, localRayVector,
                                            intersectPoint, intersectNormal);
      if Result then begin
         if Assigned(intersectPoint) then begin
            SetVector(intersectPoint^, MasterObject.AbsoluteToLocal(intersectPoint^));
            SetVector(intersectPoint^, LocalToAbsolute(intersectPoint^));
         end;
         if Assigned(intersectNormal) then begin
            SetVector(intersectNormal^, MasterObject.AbsoluteToLocal(intersectNormal^));
            SetVector(intersectNormal^, LocalToAbsolute(intersectNormal^));
         end;
      end;
   end else Result:=False;
end;

// OctreeSphereSweepIntersect
//
function TGLFreeFormProxy.OctreeSphereSweepIntersect(const rayStart, rayVector : TVector;
                                        const velocity, radius, modelscale: Single;
                                        intersectPoint : PVector = nil;
                                        intersectNormal : PVector = nil) : Boolean;
var
   localRayStart, localRayVector : TVector;
   localVelocity, localRadius: single;
begin
  Result:=False;
   if Assigned(MasterObject) then begin
      Assert(MasterObject is TGLFreeForm);
      localVelocity := velocity * modelscale;
      localRadius := radius * modelscale;

      SetVector(localRayStart, AbsoluteToLocal(rayStart));
      SetVector(localRayStart, MasterObject.LocalToAbsolute(localRayStart));
      SetVector(localRayVector, AbsoluteToLocal(rayVector));
      SetVector(localRayVector, MasterObject.LocalToAbsolute(localRayVector));
      NormalizeVector(localRayVector);

      Result:=TGLFreeForm(MasterObject).OctreeSphereSweepIntersect(localRayStart, localRayVector,
                                            localVelocity, localRadius,
                                            intersectPoint, intersectNormal);
      if Result then begin
         if Assigned(intersectPoint) then begin
            SetVector(intersectPoint^, MasterObject.AbsoluteToLocal(intersectPoint^));
            SetVector(intersectPoint^, LocalToAbsolute(intersectPoint^));
         end;
         if Assigned(intersectNormal) then begin
            SetVector(intersectNormal^, MasterObject.AbsoluteToLocal(intersectNormal^));
            SetVector(intersectNormal^, LocalToAbsolute(intersectNormal^));
         end;
      end;

   end;
end;

// GetMasterFreeFormObject
//
function TGLFreeFormProxy.GetMasterFreeFormObject: TGLFreeForm;
begin
  Result := TGLFreeForm(inherited MasterObject);
end;

// SetMasterFreeFormObject
//
procedure TGLFreeFormProxy.SetMasterFreeFormObject(
  const Value: TGLFreeForm);
begin
  Assert(Value is TGLFreeForm, ClassName + ' accepts only TGLFreeForm as master!');
  SetMasterObject(Value);
end;

// ------------------
// ------------------ TGLActorProxy ------------------
// ------------------

// Create
//
constructor TGLActorProxy.Create(AOwner: TComponent);
begin
  inherited;
  ProxyOptions := ProxyOptions - [pooTransformation];
end;

// DoProgress
//
procedure TGLActorProxy.DoProgress(const progressTime: TProgressTimes);
begin
  inherited;
  FCurrentTime := progressTime;
end;

// DoRender
//
procedure TGLActorProxy.DoRender(var ARci: TRenderContextInfo; ARenderSelf,
  ARenderChildren: Boolean);
var
  // TGLActorProxy specific
  cf, sf, ef: Integer;
  cfd: Single;
  // General proxy stuff.
  gotMaster, masterGotEffects, oldProxySubObject: Boolean;
  MasterActor: TGLActor;
begin
  try
    MasterActor := GetMasterActorObject;
    gotMaster := MasterActor <> nil;
    masterGotEffects := gotMaster and (pooEffects in ProxyOptions) and (MasterObject.Effects.Count > 0);
    if gotMaster then
    begin
      if pooObjects in ProxyOptions then
      begin
        oldProxySubObject := ARci.proxySubObject;
        ARci.proxySubObject := True;
        if pooTransformation in ProxyOptions then
          glMultMatrixf(PGLFloat(MasterActor.MatrixAsAddress));

        // At last TGLActorProxy specific stuff!
        with MasterActor do
        begin
          cfd := CurrentFrameDelta;
          cf := CurrentFrame;
          sf := startframe;
          ef := endframe;
          CurrentFrameDelta := FCurrentFrameDelta;
          SetCurrentFrameDirect(FCurrentFrame);
          StartFrame := FStartFrame;
          EndFrame := FEndFrame;
          DoProgress(FCurrentTime);
          DoRender(ARci,ARenderSelf,Count>0);
          FCurrentFrameDelta := CurrentFrameDelta;
          FCurrentFrame := CurrentFrame;
          CurrentFrameDelta := cfd;
          SetCurrentFrameDirect(cf);
          startframe := sf;
          endframe := ef;
        end;

        ARci.proxySubObject := oldProxySubObject;
      end;
    end;
    // now render self stuff (our children, our effects, etc.)
    if ARenderChildren and (Count > 0) then
      Self.RenderChildren(0, Count - 1, ARci);
    if masterGotEffects then
      MasterActor.Effects.RenderPostEffects(Scene.CurrentBuffer, ARci);
  finally
    ClearStructureChanged;
  end;
end;

// GetMasterObject
//
function TGLActorProxy.GetMasterActorObject: TGLActor;
begin
  Result := TGLActor(inherited MasterObject);
end;

// SetAnimation
//
procedure TGLActorProxy.SetAnimation(const Value: TActorAnimationName);
var
  anAnimation : TActorAnimation;
begin
  // We first assign the value (for persistency support), then check it.
  FAnimation := Value;

  if not Assigned(MasterObject) then
    raise EGLProxyException.Create(glsErrorEx + 'No MasterObject defined!');

  anAnimation := GetMasterActorObject.Animations.FindName(Value);
  if Assigned(anAnimation) then
  begin
    FStartFrame := anAnimation.StartFrame;
    FEndFrame := anAnimation.EndFrame;
    FCurrentFrame := FStartFrame;
  end;
end;

// SetMasterObject
//
procedure TGLActorProxy.SetMasterActorObject(const Value: TGLActor);
begin
  Assert(Value is TGLActor, ClassName + ' accepts only TGLActor as master!');
  SetMasterObject(Value);
end;

//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------
initialization
//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------

   RegisterClasses([TGLColorProxy, TGLFreeFormProxy, TGLActorProxy]);

end.
