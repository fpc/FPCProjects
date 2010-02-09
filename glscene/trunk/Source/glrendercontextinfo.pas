//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLRenderContextInfo<p>

   Stores contextual info useful during rendering methods.<p>

	<b>History : </b><font size=-1><ul>
      <li>09/10/08 - DanB - Added TRenderContextClippingInfo + IsVolumeClipped
                            functions from VectorGeometry.pas, added nearClippingDistance
      <li>05/10/08 - DanB - Created from GLTexture.pas split
   </ul></font>
}
unit GLRenderContextInfo;

interface

uses PersistentClasses, VectorGeometry, GLState, GLColor;

type

   TDrawState = (dsRendering, dsPicking, dsPrinting);

   TGLSize = record
      cx : Longint;
      cy : Longint;
   end;

   // TGLObjectsSorting
   //
   {: Determines if objects are sorted, and how.<p>
      Sorting is done level by level (and not for all entities), values are :<ul>
      <li>osInherited : use inherited sorting mode, defaults to osRenderFarthestFirst
      <li>osNone : do not sort objects.
		<li>osRenderFarthestFirst : render objects whose Position is the farthest from
			the camera first.
      <li>osRenderBlendedLast : opaque objects are not sorted and rendered
         first, blended ones are rendered afterwards and depth sorted.
		<li>osRenderNearestFirst : render objects whose Position is the nearest to
			the camera first.
       </ul> }
   TGLObjectsSorting = (osInherited, osNone,
                        osRenderFarthestFirst, osRenderBlendedLast,
                        osRenderNearestFirst);

   // TGLVisibilityCulling
   //
   {: Determines the visibility culling mode.
      Culling is done level by level, allowed values are:<ul>
      <li>vcInherited : use inherited culling value, if selected for the root
         level, defaults to vcNone
      <li>vcNone : no visibility culling is performed
      <li>vcObjectBased : culling is done on a per-object basis, each object may
         or may not be culled base on its own AxisAlignedDimensions,
         culling has no impact on the visibility of its children
      <li>vcHierarchical : culling is performed hierarchically, using hierarchical
         bounding boxes, if a parent is culled, all of its children, whatever their
         culling options are invisible.
      <li><br>Depending on the structure of your scene the most efficient culling
      method will be either vcObjectBased or vcHierarchical. Also note that if
      you use many objects with "static" geometry and have a T&amp;L graphics
      board, it may be faster not to cull at all (ie. leave this to the hardware). }
   TGLVisibilityCulling = (vcInherited, vcNone, vcObjectBased, vcHierarchical);

   // TRenderContextClippingInfo
   //
   TRenderContextClippingInfo = record
      origin : TVector;
      clippingDirection : TVector;
      viewPortRadius : Single; // viewport bounding radius per distance unit
      nearClippingDistance : Single;
      farClippingDistance : Single;
      frustum : TFrustum;
   end;

   // TRenderContextInfo
   //
   {: Stores contextual info useful during rendering methods. }
   TRenderContextInfo = record
      scene : TObject; //usually TGLScene
      buffer : TObject; //usually TGLSceneBuffer
      cameraPosition : TVector;
      cameraDirection, cameraUp : TVector;
      modelViewMatrix : PMatrix;
      viewPortSize : TGLSize;
      renderDPI : Integer;
      materialLibrary : TObject; //usually TGLMaterialLibrary;
      lightmapLibrary : TObject; //usually TGLMaterialLibrary;
      fogDisabledCounter : Integer;
      lightingDisabledCounter : Integer;
      drawState : TDrawState;
      objectsSorting : TGLObjectsSorting;
      visibilityCulling : TGLVisibilityCulling;
      GLStates : TGLStateCache;
      rcci : TRenderContextClippingInfo;
      sceneAmbientColor : TColorVector;
      bufferFaceCull : Boolean;
      proxySubObject : Boolean;
      ignoreMaterials : Boolean;
      ignoreBlendingRequests : Boolean;
      amalgamating : Boolean;
      lights: TPersistentObjectList;
   end;
   PRenderContextInfo = ^TRenderContextInfo;

function IsVolumeClipped(const objPos : TVector; const objRadius : Single;
                         const rcci : TRenderContextClippingInfo) : Boolean; overload;
function IsVolumeClipped(const objPos : TAffineVector; const objRadius : Single;
                         const rcci : TRenderContextClippingInfo) : Boolean; overload;
function IsVolumeClipped(const min, max : TAffineVector;
                         const rcci : TRenderContextClippingInfo) : Boolean; overload;


implementation

// IsVolumeClipped
//
function IsVolumeClipped(const objPos : TVector; const objRadius : Single;
                         const rcci : TRenderContextClippingInfo) : Boolean;
begin
   Result:=IsVolumeClipped(PAffineVector(@objPos)^, objRadius, rcci);
end;

// IsVolumeClipped
//
function IsVolumeClipped(const objPos : TAffineVector; const objRadius : Single;
                         const rcci : TRenderContextClippingInfo) : Boolean;
var
   negRadius : Single;
begin
   negRadius:=-objRadius;
   Result:=   (PlaneEvaluatePoint(rcci.frustum.pLeft, objPos)<negRadius)
           or (PlaneEvaluatePoint(rcci.frustum.pTop, objPos)<negRadius)
           or (PlaneEvaluatePoint(rcci.frustum.pRight, objPos)<negRadius)
           or (PlaneEvaluatePoint(rcci.frustum.pBottom, objPos)<negRadius)
           or (PlaneEvaluatePoint(rcci.frustum.pNear, objPos)<negRadius)
           or (PlaneEvaluatePoint(rcci.frustum.pFar, objPos)<negRadius);
end;

// IsVolumeClipped
//
function IsVolumeClipped(const min, max : TAffineVector;
                         const rcci : TRenderContextClippingInfo) : Boolean;
begin
   // change box to sphere
   Result:=IsVolumeClipped(VectorScale(VectorAdd(min, max), 0.5),
                           VectorDistance(min, max)*0.5, rcci);
end;


end.
