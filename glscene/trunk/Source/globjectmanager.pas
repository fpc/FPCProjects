//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLObjectManager<p>

   The object manager is used for registering classes together with a category,
   description + icon, so that they can be displayed visually.  This can then
   be used by run-time or design-time scene editors for choosing which
   scene objects to place into a scene.<p>

   TODO: add some notification code, so that when a scene object is registered/
   unregistered, any editor that is using the object manager can be notified.

	<b>History : </b><font size=-1><ul>
      <li>14/03/09 - DanB - Created by moving TObjectManager in from GLSceneRegister.pas,
                            made some slight adjustments to allow resources being loaded
                            from separate packages.
	</ul></font>
}


unit GLObjectManager;

interface

{$i GLScene.inc}

uses Classes, Controls, GLCrossPlatform, GLScene,
     Graphics, lresources;

type

   PSceneObjectEntry = ^TGLSceneObjectEntry;
   // holds a relation between an scene object class, its global identification,
   // its location in the object stock and its icon reference
   TGLSceneObjectEntry = record
      ObjectClass : TGLSceneObjectClass;
    Name : String;         // type name of the object
    Category : String;     // category of object
    Index,                 // index into "FSceneObjectList"
    ImageIndex : Integer;  // index into "FObjectIcons"
  end;

   // TObjectManager
   //
   TObjectManager = class (TObject)
      private
         { Private Declarations }
         FSceneObjectList : TList;
         FObjectIcons : TImageList;       // a list of icons for scene objects
         FOverlayIndex,                   // indices into the object icon list
         FSceneRootIndex,
         FCameraRootIndex,
         FLightsourceRootIndex,
         FObjectRootIndex : Integer;

    protected
      { Protected Declarations }
      procedure DestroySceneObjectList;
      function FindSceneObjectClass(AObjectClass: TGLSceneObjectClass;
                                   const ASceneObject: String = '') : PSceneObjectEntry;

    public
      { Public Declarations }
      constructor Create;
      destructor Destroy; override;
      procedure RegisterSceneObjectsToIDE;

      procedure CreateDefaultObjectIcons(ResourceModule: Cardinal);
      function GetClassFromIndex(Index: Integer): TGLSceneObjectClass;
      function GetImageIndex(ASceneObject: TGLSceneObjectClass) : Integer;
      function GetCategory(ASceneObject: TGLSceneObjectClass) : String;
      procedure GetRegisteredSceneObjects(ObjectList: TStringList);
      //: Registers a stock object and adds it to the stock object list
      procedure RegisterSceneObject(ASceneObject: TGLSceneObjectClass; const aName, aCategory : String);overload;
      procedure RegisterSceneObject(ASceneObject: TGLSceneObjectClass; const aName, aCategory : String; aBitmap: TCustomBitmap);overload;
      procedure RegisterSceneObject(ASceneObject: TGLSceneObjectClass; const aName, aCategory : String; ResourceModule: Cardinal; ResourceName:String='');overload;
      //: Unregisters a stock object and removes it from the stock object list
      procedure UnRegisterSceneObject(ASceneObject: TGLSceneObjectClass);

      property ObjectIcons: TImageList read FObjectIcons;
      property SceneRootIndex: Integer read FSceneRootIndex;
      property LightsourceRootIndex: Integer read FLightsourceRootIndex;
      property CameraRootIndex: Integer read FCameraRootIndex;
      property ObjectRootIndex: Integer read FObjectRootIndex;
  end;

implementation

uses SysUtils;

//----------------- TObjectManager ---------------------------------------------

// Create
//
constructor TObjectManager.Create;
begin
  inherited;
  FSceneObjectList:=TList.Create;
  // FObjectIcons Width + Height are set when you add the first bitmap
  FObjectIcons:=TImageList.CreateSize(16, 16);
end;

// Destroy
//
destructor TObjectManager.Destroy;
begin
  DestroySceneObjectList;
  FObjectIcons.Free;
  inherited Destroy;
end;

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
      for i:=0 to FSceneObjectList.Count-1 do
         with TGLSceneObjectEntry(FSceneObjectList.Items[I]^) do
            AddObject(Name, Pointer(ObjectClass));
   end;
end;


procedure TObjectManager.RegisterSceneObjectsToIDE;
var i : integer;
begin
  if not(Assigned(RegisterNoIconProc)) then exit;
  for i := 0 to FSceneObjectList.count-1 do begin
     RegisterNoIcon([PSceneObjectEntry(FSceneObjectList[i])^.ObjectClass]);
  end;
end;


// RegisterSceneObject
//
procedure TObjectManager.RegisterSceneObject(ASceneObject: TGLSceneObjectClass;
                                             const aName, aCategory : String);
var
   resBitmapName : String;
   bmp : TCustomBitmap;
begin
  // Since no resource name was provided, assume it's the same as class name
  resBitmapName:=ASceneObject.ClassName;
  bmp := TPixmap.Create;
  try
    // Try loading bitmap from module that class is in
    GLLoadBitmapFromInstance((*FindClassHInstance(ASceneObject),*) bmp, resBitmapName);
    if bmp.Width=0 then
      GLLoadBitmapFromInstance((*HInstance,*) bmp, resBitmapName);
    // If resource was found, register scene object with bitmap
    if bmp.Width<>0 then
    begin
      RegisterSceneObject(ASceneObject, aName, aCategory, bmp);
    end
    else
      // Resource not found, so register without bitmap
      RegisterSceneObject(ASceneObject, aName, aCategory, nil);
  finally
    bmp.Free;
  end;
end;

// RegisterSceneObject
//
procedure TObjectManager.RegisterSceneObject(ASceneObject: TGLSceneObjectClass; const aName, aCategory : String; aBitmap: TCustomBitmap);
var
   newEntry  : PSceneObjectEntry;
   bmp: TBitmap;
begin
   if Assigned(RegisterNoIconProc) then
      RegisterNoIcon([aSceneObject]);
   with FSceneObjectList do begin
      // make sure no class is registered twice
      if Assigned(FindSceneObjectClass(ASceneObject, AName)) then Exit;
      New(NewEntry);
      try
         with NewEntry^ do begin
            // object stock stuff
            // registered objects list stuff
            ObjectClass:=ASceneObject;
            NewEntry^.Name:=aName;
            NewEntry^.Category:=aCategory;
            Index:=FSceneObjectList.Count;
            if Assigned(aBitmap) then
            begin
              bmp:=TBitmap.Create;
              try
                // If we just add the bitmap, and it has different dimensions, then
                // all icons will be cleared, so ensure this doesn't happen
                bmp.PixelFormat:=glpf24bit;
                bmp.Width:=FObjectIcons.Width;
                bmp.Height:=FObjectIcons.Height;
                bmp.Canvas.Draw(0,0,aBitmap);
                FObjectIcons.AddMasked(bmp, bmp.Canvas.Pixels[0,0]);
                ImageIndex:=FObjectIcons.Count-1;
              finally
                bmp.free;
              end;
            end
            else
              ImageIndex:=0;
		   end;
         Add(NewEntry);
      finally
        //
      end;
   end;
end;

// RegisterSceneObject
//
procedure TObjectManager.RegisterSceneObject(ASceneObject: TGLSceneObjectClass; const aName, aCategory : String; ResourceModule: Cardinal; ResourceName:String='');
var
   bmp : TBitmap;
   resBitmapName : String;
begin
  // lazarus doesn't support runtime modules, so we don't need this.
  // simply call the "normal" version:
  RegisterSceneObject(ASceneObject,aName,aCategory);
  exit;
  if ResourceName='' then
    resBitmapName:=ASceneObject.ClassName
  else
    resBitmapName:=ResourceName;
  bmp:=TBitmap.Create;
  try
    // Load resource
    if (ResourceModule<>0) then
      GLLoadBitmapFromInstance((*ResourceModule,*) bmp, resBitmapName);
    // If the resource was found, then register scene object using the bitmap
    if bmp.Width>0 then
      RegisterSceneObject(ASceneObject,aName,aCategory,bmp)
    else
      // Register the scene object with no icon
      RegisterSceneObject(ASceneObject,aName, aCategory, nil);
  finally
    bmp.Free;
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
procedure TObjectManager.CreateDefaultObjectIcons(ResourceModule: Cardinal);
var
   bmp : TCustomBitmap;

begin
   bmp:=TPixmap.create;
   with FObjectIcons, bmp.Canvas do begin
      try
         // There's a more direct way for loading images into the image list, but
         // the image quality suffers too much
         {.$ifdef WIN32}
         GLLoadBitmapFromInstance((*ResourceModule,*) bmp,'gls_cross');
         FOverlayIndex:=count; InsertMasked(count,bmp, Pixels[0, 0]);
         //Overlay(FOverlayIndex, 0); // used as indicator for disabled objects
         {.$endif}
         GLLoadBitmapFromInstance((*ResourceModule,*) bmp,'gls_root');
         FSceneRootIndex:=count; InsertMasked(count,bmp, Pixels[0, 0]);
         GLLoadBitmapFromInstance((*ResourceModule,*) bmp,'gls_camera');
         FCameraRootIndex:=count; InsertMasked(count,bmp, Pixels[0, 0]);
         GLLoadBitmapFromInstance((*ResourceModule,*) bmp,'gls_lights');
         FLightsourceRootIndex:=count; InsertMasked(count,bmp, Pixels[0, 0]);
         GLLoadBitmapFromInstance((*ResourceModule,*) bmp,'gls_objects');
         FObjectRootIndex:=count; InsertMasked(count,bmp, Pixels[0, 0]);
      finally
         bmp.Free;
      end;
   end;
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

end.
