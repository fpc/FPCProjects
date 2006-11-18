unit gllazarusregister;


{
      $Log: gllazarusregister.pas,v $
      Revision 1.3  2006/03/01 10:08:42  skinhat
      glplot cf glplott

      Revision 1.2  2006/01/12 19:44:02  z0m3ie
      *** empty log message ***

      Revision 1.1  2006/01/10 20:50:44  z0m3ie
      recheckin to make shure that all is lowercase

      Revision 1.1  2006/01/09 21:02:30  z0m3ie
      *** empty log message ***

      Revision 1.15  2005/11/14 21:38:07  z0m3ie
      making this stuff again Linux compatible please dont break multi platform support again

      Revision 1.14  2005/10/11 20:31:42  z0m3ie
      - removed ode and sdl stuff becouse dll´s arend loaded dynamically
        maybe we should make seperate packages or load dlls dynamic

      Revision 1.13  2005/09/17 22:40:44  k00m
      *** empty log message ***

      Revision 1.12  2005/09/16 18:38:05  k00m
      Register my ode component and more about viewer.

      Revision 1.11  2005/09/09 23:16:18  z0m3ie
      linux corrections

      Revision 1.10  2005/08/23 03:36:36  k00m
      adding basic color support.

      Revision 1.9  2005/08/23 03:00:48  k00m
      correction from me with the glsceneedit.

      Revision 1.8  2005/08/23 02:44:28  k00m
      register HDS and remove some bad thing I have adding before.

      Revision 1.7  2005/08/22 23:22:25  k00m
      updated register again not ready.

      Revision 1.6  2005/08/22 22:12:06  k00m
      try to register more to see what working.

      Revision 1.5  2005/08/22 20:43:49  k00m
      adding and register sound type.

      Revision 1.4  2005/08/22 20:27:38  k00m
      adding soundfile unit.

      Revision 1.3  2005/08/22 20:07:47  k00m
      Adding Sounds.

      Revision 1.2  2005/08/03 00:41:38  z0m3ie
      - added automatical generated History from CVS

}

interface

uses
   {$ifndef linux}windows,{$endif} classes, controls, stdctrls, dialogs, glscene, lresources;


type

   PSceneObjectEntry = ^TGLSceneObjectEntry;
   // holds a relation between an scene object class, its global identification,
   // its location in the object stock and its icon reference
   TGLSceneObjectEntry = record
      ObjectClass : TGLSceneObjectClass;
      Name : String[32];     // type name of the object
      Category : String[32]; // category of object
      Index,                 // index into "FObjectStock"
      ImageIndex : Integer;  // index into "FObjectIcons"
   end;

   // TObjectManager
   //
   TObjectManager = class (TObject)
      private
         { Private Declarations }
         FSceneObjectList : TList;
         {$IFNDEF FPC}
         FObjectIcons : TImageList;       // a list of icons for scene objects
         {$ENDIF}
         {$ifdef WIN32}
         FOverlayIndex,                   // indices into the object icon list
         {$endif}
         FSceneRootIndex,
         FCameraRootIndex,
         FLightsourceRootIndex,
         FObjectRootIndex,
         FStockObjectRootIndex : Integer;

      protected
			{ Protected Declarations }
         procedure CreateDefaultObjectIcons;
         procedure DestroySceneObjectList;
         function FindSceneObjectClass(AObjectClass: TGLSceneObjectClass;
            const ASceneObject: String = '') : PSceneObjectEntry;

      public
         { Public Declarations }
         constructor Create;
         destructor Destroy; override;

         function GetClassFromIndex(Index: Integer): TGLSceneObjectClass;
         function GetImageIndex(ASceneObject: TGLSceneObjectClass) : Integer;
         function GetCategory(ASceneObject: TGLSceneObjectClass) : String;
         procedure GetRegisteredSceneObjects(ObjectList: TStringList);
         //: Registers a stock object and adds it to the stock object list
         procedure RegisterSceneObject(ASceneObject: TGLSceneObjectClass; const aName, aCategory : String);
         //: Unregisters a stock object and removes it from the stock object list
         procedure UnRegisterSceneObject(ASceneObject: TGLSceneObjectClass);
//         procedure Notify(Sender: TPlugInManager; Operation: TOperation; PlugIn: Integer); override;
         {$IFNDEF FPC}
         property ObjectIcons: TImageList read FObjectIcons;
         {$ENDIF}
         property SceneRootIndex: Integer read FSceneRootIndex;
         property LightsourceRootIndex: Integer read FLightsourceRootIndex;
         property CameraRootIndex: Integer read FCameraRootIndex;
         property ObjectRootIndex: Integer read FObjectRootIndex;

   end;

procedure Register;

//: Auto-create for object manager
function ObjectManager : TObjectManager;

implementation

uses
   {$ifdef linux} gllclviewer,  {$endif}
   {$ifdef mswindows} glwin32viewer,glwin32fullscreenviewer,{$endif}
   typinfo,sysutils, graphics, componenteditors, propedits, glsceneeditnew,
   glvectorfileobjects,glscreen,glmesh, glmisc, glcrossplatform,
   fvectoreditor, vectorgeometry,glstrings,glcadencer,gltexture,glgui,
   glbitmapfont,glwindowsfont,glparticlefx,glperlinpfx,gllinepfx,glfirefx,
   glthorfx,asynctimer,globjects, glgeomobjects, glteapot, glimposter,
   glcollision,glpolyhedron,glanimatedsprite,glmultipolygon,gltileplane,
   glportal,glgraph,glparticles,glskydome,glskybox,glhudobjects,glwindows,
   gllensflare,gltexlensflare,glmirror,glshadowplane,glshadowvolume,
   glzbuffer,glprojectedtextures,glblur,gltrail,gltree,glmultiproxy,
   glfeedback,glextrusion,glbumpmaphds,glheighttilefilehds,
   glterrainrenderer,glgamemenu, gltextureimageeditors,
   glstate, glutils, glwaterplane,
   glheightdata,glperlin
   {glsdlcontext,gloxode,glscriptbase,}
   {$ifdef mswindows}
   ,glsound,glsoundfileobjects,glspacetext
   {$endif}
   ;

var
   vObjectManager : TObjectManager;

function ObjectManager : TObjectManager;
begin
   if not Assigned(vObjectManager) then
      vObjectManager:=TObjectManager.Create;
   Result:=vObjectManager;
end;

type

   // TGLSceneViewerEditor
   //
   TGLSceneViewerEditor = class(TComponentEditor)
      public
         { Public Declarations }
         procedure ExecuteVerb(Index: Integer); override;
         function GetVerb(Index: Integer): String; override;
         function GetVerbCount: Integer; override;
   end;

   // TGLSceneEditor
   //
   TGLSceneEditor = class (TComponentEditor)
      public
         { Public Declarations }
         procedure Edit; override;

         procedure ExecuteVerb(Index: Integer); override;
         function GetVerb(Index: Integer): String; override;
         function GetVerbCount: Integer; override;
   end;

   TGLColorProperty = class (TClassProperty)
      private
        { Private Declarations }
      protected
        { Protected Declarations }
	function GetAttributes: TPropertyAttributes; override;
	procedure GetValues(Proc: TGetStrProc); override;
	procedure Edit; override;
        function ColorToBorderColor(aColor: TColorVector; selected : Boolean) : TColor;
      public
        function GetValue: String; override;
	procedure SetValue(const Value: string); override;
   end;
//{$endif}
         
{$ifdef WIN32}
   // TSoundFileProperty
   //
   TSoundFileProperty = class (TClassProperty)
      protected
         { Protected Declarations }
         function GetAttributes : TPropertyAttributes; override;
         function GetValue: String; override;
         procedure Edit; override;
   end;

   // TSoundNameProperty
   //
   TSoundNameProperty = class (TStringProperty)
      protected
         { Protected Declarations }
         function GetAttributes : TPropertyAttributes; override;
      	procedure GetValues(Proc: TGetStrProc); override;
   end;
{$endif}


   // TGLCoordinatesProperty
   //
   TGLCoordinatesProperty = class(TClassProperty)
      protected
         { Protected Declarations }
         function GetAttributes: TPropertyAttributes; override;
         procedure Edit; override;
   end;


//----------------- TObjectManager ---------------------------------------------

// Create
//
constructor TObjectManager.Create;
begin
  inherited;
  FSceneObjectList:=TList.Create;
  CreateDefaultObjectIcons;
end;

// Destroy
//
destructor TObjectManager.Destroy;
begin
   DestroySceneObjectList;
   {$IFNDEF FPC}
   FObjectIcons.Free;
   {$ENDIF}
   inherited Destroy;
end;

// Notify
//
//procedure TObjectManager.Notify(Sender: TPlugInManager; Operation: TOperation; PlugIn: Integer);
//begin
//end;

// FindSceneObjectClass
//
function TObjectManager.FindSceneObjectClass(AObjectClass: TGLSceneObjectClass;
                           const aSceneObject: String = '') : PSceneObjectEntry;
var
   I     : Integer;
   Found : Boolean;
begin
   Result:=nil;
   Found:=False;
   with FSceneObjectList do begin
      for I:=0 to Count-1 do
         with TGLSceneObjectEntry(Items[I]^) do
         if (ObjectClass = AObjectClass) and (Length(ASceneObject) = 0)
               or (CompareText(Name, ASceneObject) = 0) then begin
            Found:=True;
            Break;
         end;
      if Found then Result:=Items[I];
   end;
end;

// GetClassFromIndex
//
function TObjectManager.GetClassFromIndex(Index: Integer): TGLSceneObjectClass;
begin
   if Index<0 then
      Index:=0;
   if Index>FSceneObjectList.Count-1 then
      Index:=FSceneObjectList.Count-1;
  Result:=TGLSceneObjectEntry(FSceneObjectList.Items[Index+1]^).ObjectClass;
end;

// GetImageIndex
//
function TObjectManager.GetImageIndex(ASceneObject: TGLSceneObjectClass) : Integer;
var
   classEntry : PSceneObjectEntry;
begin
   classEntry:=FindSceneObjectClass(ASceneObject);
   if Assigned(classEntry) then
      Result:=classEntry^.ImageIndex
   else Result:=0;
end;

// GetCategory
//
function TObjectManager.GetCategory(ASceneObject: TGLSceneObjectClass) : String;
var
   classEntry : PSceneObjectEntry;
begin
   classEntry:=FindSceneObjectClass(ASceneObject);
   if Assigned(classEntry) then
      Result:=classEntry^.Category
   else Result:='';
end;

// GetRegisteredSceneObjects
//
procedure TObjectManager.GetRegisteredSceneObjects(objectList: TStringList);
var
   i : Integer;
begin
   if Assigned(objectList) then with objectList do begin
      Clear;
      for i:=1 to FSceneObjectList.Count-1 do
         with TGLSceneObjectEntry(FSceneObjectList.Items[I]^) do
            AddObject(Name, Pointer(ObjectClass));
   end;
end;

// RegisterSceneObject
//
procedure TObjectManager.RegisterSceneObject(ASceneObject: TGLSceneObjectClass;
                                             const aName, aCategory : String);
var
   newEntry  : PSceneObjectEntry;
   pic       : TPicture;
   {$IFNDEF FPC}
   resBitmapName : String;
   bmp : TBitmap;
   {$ENDIF}
begin
//>>Lazarus will crash at this function
   RegisterNoIcon([aSceneObject]);
   with FSceneObjectList do begin
      // make sure no class is registered twice
      if Assigned(FindSceneObjectClass(ASceneObject, AName)) then Exit;
      New(NewEntry);
      pic:=TPicture.Create;
      try
         with NewEntry^ do begin
            // object stock stuff
            // registered objects list stuff
            ObjectClass:=ASceneObject;
            NewEntry^.Name:=aName;
            NewEntry^.Category:=aCategory;
            Index:=FSceneObjectList.Count;
            {$IFNDEF FPC}
            resBitmapName:=ASceneObject.ClassName;
            GLLoadBitmapFromInstance(Pic.Bitmap,resBitmapName);
            bmp:=TBitmap.Create;
            bmp.PixelFormat:=glpf24bit;
            bmp.Width:=24; bmp.Height:=24;
            bmp.Canvas.Draw(0, 0, Pic.Bitmap);
            Pic.Bitmap:=bmp;
            bmp.Free;
            if Cardinal(Pic.Bitmap.Handle)<>0 then begin
               FObjectIcons.AddMasked(Pic.Bitmap, Pic.Bitmap.Canvas.Pixels[0, 0]);
               ImageIndex:=FObjectIcons.Count-1;
            end else ImageIndex:=0;
            {$ENDIF}
		   end;
       Add(NewEntry);
      finally
         pic.Free;
      end;
   end;
end;

// UnRegisterSceneObject
//
procedure TObjectManager.UnRegisterSceneObject(ASceneObject: TGLSceneObjectClass);
var
   oldEntry : PSceneObjectEntry;
begin
   // find the class in the scene object list
   OldEntry:=FindSceneObjectClass(ASceneObject);
   // found?
   if assigned(OldEntry) then begin
      // remove its entry from the list of registered objects
      FSceneObjectList.Remove(OldEntry);
      // finally free the memory for the entry
      Dispose(OldEntry);
   end;
end;

// CreateDefaultObjectIcons
//
procedure TObjectManager.CreateDefaultObjectIcons;
var
   pic : TPicture;
begin
   pic:=TPicture.Create;
   // load first pic to get size
   GLLoadBitmapFromInstance(Pic.Bitmap,'gls_cross');
   //FObjectIcons:=TImageList.CreateSize(Pic.Width, Pic.height);
   {$IFNDEF FPC}
   FObjectIcons:=TImageList.CreateSize(16, 16);

   with FObjectIcons, pic.Bitmap.Canvas do begin
      try
         // There's a more direct way for loading images into the image list, but
         // the image quality suffers too much
         AddMasked(Pic.Bitmap, Pixels[0, 0]);
         {$ifdef WIN32}
         FOverlayIndex:=Count-1;
         //Overlay(FOverlayIndex, 0); // used as indicator for disabled objects
         {$endif}
         GLLoadBitmapFromInstance(Pic.Bitmap,'gls_root');
         AddMasked(Pic.Bitmap, Pixels[0, 0]); FSceneRootIndex:=Count-1;
         GLLoadBitmapFromInstance(Pic.Bitmap,'gls_camera');
         AddMasked(Pic.Bitmap, Pixels[0, 0]); FCameraRootIndex:=Count-1;
         GLLoadBitmapFromInstance(Pic.Bitmap,'gls_lights');
         AddMasked(Pic.Bitmap, Pixels[0, 0]); FLightsourceRootIndex:=Count-1;
         GLLoadBitmapFromInstance(Pic.Bitmap,'gls_objects');
         AddMasked(Pic.Bitmap, Pixels[0, 0]); FObjectRootIndex:=Count-1;
         AddMasked(Pic.Bitmap, Pixels[0, 0]); FStockObjectRootIndex:=Count-1;
      finally
         Pic.Free;
      end;
   end;
   {$ENDIF}
end;

// DestroySceneObjectList
//
procedure TObjectManager.DestroySceneObjectList;
var
   i : Integer;
begin
   with FSceneObjectList do begin
      for i:=0 to Count-1 do
         Dispose(PSceneObjectEntry(Items[I]));
      Free;
   end;
end;


//----------------- TGLSceneViewerEditor ---------------------------------------

// ExecuteVerb
//
procedure TGLSceneViewerEditor.ExecuteVerb(Index : Integer);
var
  source : TGLSceneViewer;
begin
  source:=Component as TGLSceneViewer;
  case Index of
    0 : source.Buffer.ShowInfo;
  end;
end;

// GetVerb
//
function TGLSceneViewerEditor.GetVerb(Index : Integer) : String;
begin
  case Index of
    0 : Result:='Show context info';
  end;
end;

// GetVerbCount
//
function TGLSceneViewerEditor.GetVerbCount: Integer;
begin
  Result:=1;
end;


//----------------- TGLSceneEditor ---------------------------------------------

// Edit
//
procedure TGLSceneEditor.Edit;
begin
   with GLSceneEditorForm do begin
      SetScene(Self.Component as TGLScene, TComponentEditorDesigner(Self.Designer));
      Show;
   end;
end;

// ExecuteVerb
//
procedure TGLSceneEditor.ExecuteVerb(Index : Integer);
begin
   case Index of
      0 : Edit;
   end;
end;

// GetVerb
//
function TGLSceneEditor.GetVerb(Index : Integer) : String;
begin
   case Index of
      0 : Result:='Show Scene Editor';
   end;
end;

// GetVerbCount
//
function TGLSceneEditor.GetVerbCount: Integer;
begin
   Result:=1;
end;

procedure TGLColorProperty.Edit;
var
   colorDialog : TColorDialog;
   glColor : TGLColor;
begin
   colorDialog:=TColorDialog.Create(nil);
   try
      glColor:=TGLColor(GetOrdValue);
      colorDialog.Color:=ConvertColorVector(glColor.Color);
      if colorDialog.Execute then begin
         glColor.Color:=ConvertWinColor(colorDialog.Color);
         Modified;
      end;
   finally
      colorDialog.Free;
   end;
end;

function TGLColorProperty.GetAttributes: TPropertyAttributes;
begin
  Result:=[paSubProperties, paValueList, paDialog];
end;

procedure TGLColorProperty.GetValues(Proc: TGetStrProc);
begin
  ColorManager.EnumColors(Proc);
end;

function TGLColorProperty.GetValue: String;
begin
  Result:=ColorManager.GetColorName(TGLColor(GetOrdValue).Color);
end;

procedure TGLColorProperty.SetValue(const Value: string);
begin
  TGLColor(GetOrdValue).Color:=ColorManager.GetColor(Value);
  Modified;
end;

// ColorToBorderColor
//
function TGLColorProperty.ColorToBorderColor(aColor: TColorVector; selected : Boolean) : TColor;
begin
   if (aColor[0]>0.75) or (aColor[1]>0.75) or (aColor[2]>0.75) then
      Result:=clBlack
   else if selected then
      Result:=clWhite
   else Result:=ConvertColorVector(AColor);
end;

//----------------- TSoundFileProperty -----------------------------------------

{$ifdef WIN32}
// GetAttributes
//
function TSoundFileProperty.GetAttributes: TPropertyAttributes;
begin
   Result:=[paDialog];
end;

// GetValue
//
function TSoundFileProperty.GetValue: String;
var
   sample : TGLSoundSample;
begin
   sample:=GetComponent(0) as TGLSoundSample;
   if sample.Data<>nil then
      Result:='('+sample.Data.ClassName+')'
   else Result:='(empty)';
end;

// Edit
//
procedure TSoundFileProperty.Edit;
var
   ODialog   : TOpenDialog;
   sample : TGLSoundSample;
   Desc, F    : String;
begin
   sample:=GetComponent(0) as TGLSoundSample;
   ODialog:=TOpenDialog.Create(nil);
   try
      GetGLSoundFileFormats.BuildFilterStrings(TGLSoundFile, Desc, F);
      ODialog.Filter:=Desc;
      if ODialog.Execute then begin
         sample.LoadFromFile(ODialog.FileName);
         Modified;
      end;
   finally
      ODialog.Free;
   end;
end;

//----------------- TSoundNameProperty -----------------------------------------

// GetAttributes
//
function TSoundNameProperty.GetAttributes: TPropertyAttributes;
begin
   Result:=[paValueList];
end;

// GetValues
//
procedure TSoundNameProperty.GetValues(Proc: TGetStrProc);
var
   i : Integer;
   source : TGLBaseSoundSource;
begin
   source:=(GetComponent(0) as TGLBaseSoundSource);
   if Assigned(source.SoundLibrary) then with source.SoundLibrary do
      for i:=0 to Samples.Count-1 do Proc(Samples[i].Name);
end;
{$endif WIN32}

//----------------- TGLCoordinatesProperty -------------------------------------

// GetAttributes
//
function TGLCoordinatesProperty.GetAttributes: TPropertyAttributes;
begin
   Result:=[paDialog, paSubProperties];
end;

// Edit;
//
procedure TGLCoordinatesProperty.Edit;
var
   glc : TGLCoordinates;
   x, y, z : Single;
begin
   glc:=TGLCoordinates(GetOrdValue);
   x:=glc.x;
   y:=glc.y;
   z:=glc.z;
   if VectorEditorForm.Execute(x, y, z) then begin
      glc.AsVector:=VectorMake(x, y, z);
      Modified;
   end;
end;

procedure Register;
begin
   RegisterComponents('GLScene',
                      [TGLScene,
                       TGLSceneViewer, TGLMemoryViewer,
                       TGLMaterialLibrary,
                       TGLCadencer,
                       TGLGuiLayout,
                       TGLBitmapFont, TGLWindowsBitmapFont, TGLStoredBitmapFont
                       {$ifdef WIN32}
                       ,
                       TGLFullScreenViewer
                       {$endif}
                      ]);

   RegisterComponents('GLScene PFX',
                      [
                       TGLCustomPFXManager,
                       TGLPolygonPFXManager, TGLPointLightPFXManager,
                       TGLCustomSpritePFXManager,
                       TGLPerlinPFXManager, TGLLinePFXManager,
                       TGLFireFXManager, TGLThorFXManager
                      ]);

   RegisterComponents('GLScene Utils',
                      [TAsyncTimer,
                       TGLStaticImposterBuilder,
                       TGLBitmapHDS,TGLCustomHDS, TGLHeightTileFileHDS,
                       TGLBumpmapHDS, TGLPerlinHDS,
                       TCollisionManager, TGLAnimationControler
                      ]);

   RegisterComponentEditor(TGLSceneViewer, TGLSceneViewerEditor);
   RegisterComponentEditor(TGLScene, TGLSceneEditor);
   
   RegisterPropertyEditor(TypeInfo(TGLColor), nil, '', TGLColorProperty);
   
   {$ifdef WIN32}
   RegisterPropertyEditor(TypeInfo(TGLSoundFile), TGLSoundSample, '', TSoundFileProperty);
   RegisterPropertyEditor(TypeInfo(String), TGLBaseSoundSource, 'SoundName', TSoundNameProperty);
   {$endif}

   RegisterClasses([TGLCoordinates]);

   RegisterPropertyEditor(TypeInfo(TGLCoordinates), nil, '', TGLCoordinatesProperty);
end;

initialization


   GLMisc.vUseDefaultSets:=True;
   //ReadVideoModes;

   with ObjectManager do begin
      RegisterSceneObject(TGLCamera, 'Camera', '');
      RegisterSceneObject(TGLLightSource, 'LightSource', '');
      RegisterSceneObject(TGLDummyCube, 'DummyCube', '');

      RegisterSceneObject(TGLSprite, 'Sprite', glsOCBasicGeometry);
      RegisterSceneObject(TGLPoints, 'Points', glsOCBasicGeometry);
      RegisterSceneObject(TGLLines, 'Lines', glsOCBasicGeometry);
      RegisterSceneObject(TGLPlane, 'Plane', glsOCBasicGeometry);
      RegisterSceneObject(TGLPolygon, 'Polygon', glsOCBasicGeometry);
      RegisterSceneObject(TGLCube, 'Cube', glsOCBasicGeometry);
      RegisterSceneObject(TGLFrustrum, 'Frustrum', glsOCBasicGeometry);
      RegisterSceneObject(TGLSphere, 'Sphere', glsOCBasicGeometry);
      RegisterSceneObject(TGLDisk, 'Disk', glsOCBasicGeometry);
      RegisterSceneObject(TGLCone, 'Cone', glsOCBasicGeometry);
      RegisterSceneObject(TGLCylinder, 'Cylinder', glsOCBasicGeometry);
      RegisterSceneObject(TGLDodecahedron, 'Dodecahedron', glsOCBasicGeometry);
      RegisterSceneObject(TGLIcosahedron, 'Icosahedron', glsOCBasicGeometry);

      RegisterSceneObject(TGLAnimatedSprite, 'Animated Sprite', glsOCAdvancedGeometry);
      RegisterSceneObject(TGLArrowLine, 'ArrowLine', glsOCAdvancedGeometry);
      RegisterSceneObject(TGLAnnulus, 'Annulus', glsOCAdvancedGeometry);
      RegisterSceneObject(TGLExtrusionSolid, 'ExtrusionSolid', glsOCAdvancedGeometry);
      RegisterSceneObject(TGLMultiPolygon, 'MultiPolygon', glsOCAdvancedGeometry);
      RegisterSceneObject(TGLPipe, 'Pipe', glsOCAdvancedGeometry);
      RegisterSceneObject(TGLRevolutionSolid, 'RevolutionSolid', glsOCAdvancedGeometry);
      RegisterSceneObject(TGLTorus, 'Torus', glsOCAdvancedGeometry);

      RegisterSceneObject(TGLActor, 'Actor', glsOCMeshObjects);
      RegisterSceneObject(TGLFreeForm, 'FreeForm', glsOCMeshObjects);
      RegisterSceneObject(TGLMesh, 'Mesh', glsOCMeshObjects);
      RegisterSceneObject(TGLTilePlane, 'TilePlane', glsOCMeshObjects);
      RegisterSceneObject(TGLPortal, 'Portal', glsOCMeshObjects);
      RegisterSceneObject(TGLTerrainRenderer, 'TerrainRenderer', glsOCMeshObjects);

      RegisterSceneObject(TGLFlatText, 'FlatText', glsOCGraphPlottingObjects);
      RegisterSceneObject(TGLHeightField, 'HeightField', glsOCGraphPlottingObjects);
      RegisterSceneObject(TGLXYZGrid, 'XYZGrid', glsOCGraphPlottingObjects);

      RegisterSceneObject(TGLParticles, 'Particles', glsOCParticleSystems);
      RegisterSceneObject(TGLParticleFXRenderer, 'PFX Renderer', glsOCParticleSystems);

      RegisterSceneObject(TGLEarthSkyDome, 'EarthSkyDome', glsOCEnvironmentObjects);
      RegisterSceneObject(TGLSkyDome, 'SkyDome', glsOCEnvironmentObjects);
      RegisterSceneObject(TGLSkyBox, 'SkyBox', glsOCEnvironmentObjects);

      RegisterSceneObject(TGLHUDSprite, 'HUDSprite', glsOCHUDObjects);
      RegisterSceneObject(TGLHUDText, 'HUDText', glsOCHUDObjects);
      RegisterSceneObject(TGLGameMenu, 'GameMenu', glsOCHUDObjects);

      RegisterSceneObject(TGLBaseControl, 'Root Control', glsOCGuiObjects);
      RegisterSceneObject(TGLPopupMenu, 'GLPopupMenu', glsOCGuiObjects);
      RegisterSceneObject(TGLForm, 'GLForm', glsOCGuiObjects);
      RegisterSceneObject(TGLPanel, 'GLPanel', glsOCGuiObjects);
      RegisterSceneObject(TGLButton, 'GLButton', glsOCGuiObjects);
      RegisterSceneObject(TGLCheckBox, 'GLCheckBox', glsOCGuiObjects);
      RegisterSceneObject(TGLEdit, 'GLEdit', glsOCGuiObjects);
      RegisterSceneObject(TGLLabel, 'GLLabel', glsOCGuiObjects);
      RegisterSceneObject(TGLAdvancedLabel, 'GLAdvancedLabel', glsOCGuiObjects);
      RegisterSceneObject(TGLScrollbar, 'GLScrollbar', glsOCGuiObjects);
      RegisterSceneObject(TGLStringGrid, 'GLStringGrid', glsOCGuiObjects);
      RegisterSceneObject(TGLCustomControl, 'GLBitmapControl', glsOCGuiObjects);

      RegisterSceneObject(TGLLensFlare, 'LensFlare', glsOCSpecialObjects);
      RegisterSceneObject(TGLTextureLensFlare, 'TextureLensFlare', glsOCSpecialObjects);
      RegisterSceneObject(TGLMirror, 'Mirror', glsOCSpecialObjects);
      RegisterSceneObject(TGLShadowPlane, 'ShadowPlane', glsOCSpecialObjects);
      RegisterSceneObject(TGLShadowVolume, 'ShadowVolume', glsOCSpecialObjects);
      RegisterSceneObject(TGLZShadows, 'ZShadows', glsOCSpecialObjects);

      RegisterSceneObject(TGLTextureEmitter, 'Texture Emitter', glsOCSpecialObjects);
      RegisterSceneObject(TGLProjectedTextures, 'Projected Textures', glsOCSpecialObjects);
      RegisterSceneObject(TGLBlur, 'GLBlur', glsOCSpecialObjects);
      RegisterSceneObject(TGLTrail, 'GLTrail', glsOCSpecialObjects);
      //////////////////////////////////////////////////////////////////////////
      // GLOxOde
{      RegisterSceneObject(TGLSizableDummyCube, 'GLSizableDummyCube', glsOCSpecialObjects);
      RegisterSceneObject(TGLOXOdeEngine, 'GLOXOdeEngine', glsOCSpecialObjects);
      RegisterSceneObject(TGLOXStaBall, 'GLOXStaBall', glsOCSpecialObjects);
      RegisterSceneObject(TGLOXStaCylinder, 'GLOXStaCylinder', glsOCSpecialObjects);
      RegisterSceneObject(TGLOXStaCCylinder, 'GLOXStaCCylinder', glsOCSpecialObjects);
      RegisterSceneObject(TGLOXStaBox, 'GLOXStaBox', glsOCSpecialObjects);
      RegisterSceneObject(TGLOXStaMesh, 'GLOXStaMesh', glsOCSpecialObjects);
      RegisterSceneObject(TGLOXZStaTerrain, 'GLOXZStaTerrain', glsOCSpecialObjects);
      RegisterSceneObject(TGLOXStaCone, 'GLOXStaCone', glsOCSpecialObjects);
      RegisterSceneObject(TGLOXDynMesh , 'GLOXDynMesh ', glsOCSpecialObjects);
      RegisterSceneObject(TGLOXDynBall, 'GLOXDynBall', glsOCSpecialObjects);
      RegisterSceneObject(TGLOXDynBox, 'GLOXDynBox', glsOCSpecialObjects);
      RegisterSceneObject(TGLOXDynCylinder, 'GLOXDynCylinder', glsOCSpecialObjects);
      RegisterSceneObject(TGLOXDynCCylinder, 'GLOXDynCCylinder', glsOCSpecialObjects);
      RegisterSceneObject(TGLOXDynCCylinder, 'GLOXDynCone', glsOCSpecialObjects);
      RegisterSceneObject(TGLOXDynCar, 'GLOXDynCar', glsOCSpecialObjects);
      RegisterSceneObject(TGLOXRagdoll, 'GLOXRagdoll', glsOCSpecialObjects);
      RegisterSceneObject(TGLOXAMotor, 'GLOXAMotor', glsOCSpecialObjects);}
      //////////////////////////////////////////////////////////////////////////
      RegisterSceneObject(TGLTeapot, 'Teapot', glsOCDoodad);
      RegisterSceneObject(TGLTree, 'Tree', glsOCDoodad);
      RegisterSceneObject(TGLWaterPlane, 'WaterPlane', glsOCDoodad);

      RegisterSceneObject(TGLDirectOpenGL, 'Direct OpenGL', '');
      RegisterSceneObject(TGLProxyObject, 'ProxyObject', '');
      RegisterSceneObject(TGLMultiProxy, 'MultiProxy', '');
      RegisterSceneObject(TGLRenderPoint, 'Render Point', '');
      RegisterSceneObject(TGLImposter, 'Imposter Sprite', '');
      RegisterSceneObject(TGLFeedback, 'OpenGL Feedback', '');

      {$IFNDEF LINUX}
      RegisterSceneObject(TGLSpaceText, 'SpaceText', glsOCDoodad);
      {$ENDIF}
   end;

  {$i GLLazarusRegister.lrs}


finalization
   ObjectManager.Free;

end.
