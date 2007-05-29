//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLFileASE<p>

	 ASE (ASCI Scene Export) file format support for GLScene<p>

	<b>History :</b><font size=-1><ul>
      <li>19/05/07 - DaStr - Initial version (contributed by MaX)


  Parser supported features list:
    blablabla :)
}
unit GLFileASE;

interface

{$I GLScene.inc}

uses
  Classes, GLVectorFileObjects, ApplicationFileIO, VectorGeometry, VectorLists;

const
  MAX_TEXURE_CHANNELS = 5; // maximum texture channels
  MAX_SUBMATERIALS =    5; // maximum material submaterials
  MAX_SMOOTH_GROUPS =   5; // maximum smoothing groups

type
  // face texture channel indices
  TGLASEFaceTexure = record
    Idx0, Idx1, Idx2: Integer;
  end;

  // face texture channels
  TGLASEFaceTexureChannels = record
    Count: Integer;
    ChanelTexture: array [0..MAX_TEXURE_CHANNELS - 1] of TGLASEFaceTexure;
  end;

  TGLASESmoothingGroups = record
    Count: Integer;
    Groups: array [0..MAX_SMOOTH_GROUPS] of Integer;
  end;

  // ASE mesh face
  TGLASEFace = class(TObject)
  private
    FV: array [0..2] of Integer;
    FNormal: TAffineVector;
    FN: array [0..2] of TAffineVector;
    FSmoothing: TGLASESmoothingGroups;
    FSubMaterialID: Integer;                    // submaterial ID
    FTextChannels: TGLASEFaceTexureChannels;
  public
    constructor Create;
    property VertIdx1: Integer read FV[0];        // vertex 0 index
    property VertIdx2: Integer read FV[1];        // vertex 1 index
    property VertIdx3: Integer read FV[2];        // vertex 2 index
    property Normal: TAffineVector read FNormal;  // face normal
    property Normal1: TAffineVector read FN[0];   // vertex 0 normal
    property Normal2: TAffineVector read FN[1];   // vertex 1 normal
    property Normal3: TAffineVector read FN[2];   // vertex 2 normal
    property TextChannels: TGLASEFaceTexureChannels read FTextChannels; // channels texture coordinates
    property Smoothing: TGLASESmoothingGroups read FSmoothing;   // smoothing group list
    property SubMaterialID: Integer read FSubMaterialID; // submaterial ID
  end;

  // ASE mesh faces list
  TGLASEFaceList = class(TObject)
  private
    FItems: TList;
    function GetFace(Index: Integer): TGLASEFace;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function Add: TGLASEFace;
    procedure Delete(aIndex: Integer);
    procedure Clear;
    property Face[Index: Integer]: TGLASEFace read GetFace; default;
    property Count: Integer read GetCount;
  end;

  // ASE geom object, represents single mesh object;
  // contains: vertices, faces, verice indices, faces and vertices normals,
  // channels of texture coordinates and indices, scaling and location info;
  // this object used only to store ASE data temporary to copy supported piece of it into GLScene TMeshObject
  TGLASEMeshObject = class(TObject)
  private
    FFaces: TGLASEFaceList;
    FVertices: TAffineVectorList;
    FMatrix: TMatrix;
    FInheritedPosition: TAffineVector;
    FInheritedScale: TAffineVector;
    FInheritedRotation: TAffineVector;
    FRotationAngle: Single;
    FRotationAxis: TAffineVector;
    FPosition: TVector;
    FScale: TAffineVector;
    FScaleAxisAngle: Single;
    FScaleAxis: TAffineVector;
    FTexChannels: array [0..MAX_TEXURE_CHANNELS - 1] of TAffineVectorList;
    FTexChannelsCount: Integer;
    FHasNormals: Boolean;
    FMaterialID: Integer;
    function AddTexChannel: TAffineVectorList;
    function GetTextChannel(Channel: Integer): TAffineVectorList;
  public
    constructor Create;
    destructor Destroy; override;
    property Faces: TGLASEFaceList read FFaces;
    property Vertices: TAffineVectorList read FVertices;
    property TextChannel[Channel: Integer]: TAffineVectorList read GetTextChannel;
    property TextChannelsCount: Integer read FTexChannelsCount;
    property Matrix: TMatrix read FMatrix;
    property InheritedPosition: TAffineVector read FInheritedPosition;
    property InheritedRotation: TAffineVector read FInheritedRotation;
    property InheritedScale: TAffineVector read FInheritedScale;
    property Position: TVector read FPosition;
    property RotationAxis: TAffineVector read FRotationAxis;
    property RotationAngle: Single read FRotationAngle;
    property Scale: TAffineVector read FScale;
    property ScaleAxis: TAffineVector read FScaleAxis;
    property ScaleAxisAngle: Single read FScaleAxisAngle;
    property HasNormals: Boolean read FHasNormals;
    property MaterialID: Integer read FMaterialID;
  end;

  TGLASEMaterialDiffuseMap = record
    Name: string;
    _Class: string;
    No: Integer;
    Amount: Single;
    Bitmap: string;
    UOffset: Single;
    VOffset: Single;
    UTiling: Single;
    VTiling: Single;
    Angle: Single;
    Blur: Single;
    BlurOffset: Single;
    NouseAmount: Single;
    NoiseSize: Single;
    NoiseLevel: Integer;
    NoisePhase: Single;
  end;

  TGLASESubMaterial = record
    Name: string;
    Ambient: TAffineVector;
    Diffuse: TAffineVector;
    Specular: TAffineVector;
    Shiness: Single;
    ShineStrength: Single;
    Transparency: Single;
    WireSize: Single;
    SelfIllumination: Single;
    DiffuseMap: TGLASEMaterialDiffuseMap;
  end;

  TGLASESubMaterialList = record
    SubMaterial: array [0..MAX_SUBMATERIALS - 1] of TGLASESubMaterial;
    Count: Integer;
  end;

  TGLASEMaterial = class(TObject)
  private
    FWireSize: Single;
    FShineStrength: Single;
    FShiness: Single;
    FTransparency: Single;
    FName: string;
    FDiffuse: TAffineVector;
    FAmbient: TAffineVector;
    FSpecular: TAffineVector;
    FSubMaterials: TGLASESubMaterialList;
  public
    constructor Create;
    property Name: string read FName;
    property Ambient: TAffineVector read FAmbient;
    property Diffuse: TAffineVector read FDiffuse;
    property Specular: TAffineVector read FSpecular;
    property Shiness: Single read FShiness;
    property ShineStrength: Single read FShineStrength;
    property Transparency: Single read FTransparency;
    property WireSize: Single read FWireSize;
    property SubMaterials: TGLASESubMaterialList read FSubMaterials;
  end;

  TGLASEMaterialList = class(TObject)
  private
    FItems: TList;
    function GetCount: Integer;
    function GetMaterial(Index: Integer): TGLASEMaterial;
  public
    constructor Create;
    destructor Destroy; override;
    function Add: TGLASEMaterial;
    procedure Delete(aIndex: Integer);
    procedure Clear;
    property Material[Index: Integer]: TGLASEMaterial read GetMaterial; default;
    property Count: Integer read GetCount;
  end;


  // ASE vector file parser
  TGLASEVectorFile = class(TVectorFile)
  private
    FStringData: TStringList;
    FHeader: string;
    FComment: string;
    FRECVShadow: Boolean;
    FCastShadow: Boolean;
    FMotionBlur: Boolean;
    FMaterialList: TGLASEMaterialList;
    function ContainString(const aData, aString: string): Boolean;
    function GetTagOnData(const aData: string): Integer;
    function IsEndOfSection(const aData: string): Boolean;
    function GetValue3D(const aData: string): TAffineVector;
    function GetValue4D(const aData: string; var Value0: Integer): TAffineVector;
    function GetStringValue(const aData: string): string;
    function GetDoubleValue(const aData: string): Double;
    function GetFirstValue(aData: string): string;
    function GetEndOfFirstValue(const aData: string): Integer;
    procedure ParseFaceString(const aData: string; var Index, A, B, C, AB, BC, CA, MatID: Integer; var Smooth: TGLASESmoothingGroups);
  private
    procedure ParseScene(var aLineIndex: Integer);
    procedure ParseGeomObject(var aLineIndex: Integer);
    procedure ParseMeshOptions(var aLineIndex: Integer; aMesh: TGLASEMeshObject);
    procedure ParseMeshGeom(var aLineIndex: Integer; aMesh: TGLASEMeshObject);
    procedure ParseMeshVertices(var aLineIndex: Integer; aMesh: TGLASEMeshObject; VerticesCount: Integer);
    procedure ParseMeshFaces(var aLineIndex: Integer; aMesh: TGLASEMeshObject; FacesCount: Integer);
    procedure ParseMeshNormals(var aLineIndex: Integer; aMesh: TGLASEMeshObject; FacesCount: Integer);
    procedure ParseMeshTextureVertices(var aLineIndex: Integer; aMesh: TGLASEMeshObject; TextureVerticesCount: Integer);
    procedure ParseMeshTextureFaces(var aLineIndex: Integer; aMesh: TGLASEMeshObject; TextureFacesCount: Integer);
    procedure ParseMaterialList(var aLineIndex: Integer);
    procedure ParseMaterial(var aLineIndex: Integer; aMaterial: TGLASEMaterial);
    procedure ParseSubMaterial(var aLineIndex: Integer; aMaterial: TGLASEMaterial);
    procedure ParseDiffuseMap(var aLineIndex: Integer; aMaterial: TGLASEMaterial; aSubMaterialIndex: Integer);
    function GetPropMBlur(const aData: string): Boolean;
    function GetPropCastShadow(const aData: string): Boolean;
    function GetPropRECVShadow(const aData: string): Boolean;
    procedure Parse;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure LoadFromStream(aStream: TStream); override;
    class function Capabilities: TDataFileCapabilities; override;

    property Header: string read FHeader;
    property Comment: string read FComment;
    property MotionBlur: Boolean read FMotionBlur;
    property CastShadow: Boolean read FCastShadow;
    property RECVShadow: Boolean read FRECVShadow;
  end;

implementation

uses
  SysUtils, GLTexture;

// ASE file tags
const
  ASCII_COMMENT_S =              'COMMENT';
  ASCII_SCENE_S =                'SCENE';
  ASCII_GEOMOBJECT_S =           'GEOMOBJECT';
  ASCII_NODE_NAME_S =            'NODE_NAME';
  ASCII_NODE_TM_S =              'NODE_TM';
  ASCII_INHERIT_POS_S =          'INHERIT_POS';
  ASCII_INHERIT_ROT_S =          'INHERIT_ROT';
  ASCII_INHERIT_SCL_S =          'INHERIT_SCL';
  ASCII_ROW_S =                  'TM_ROW0';
  ASCII_POS_S =                  'TM_POS';
  ASCII_ROTAXIS_S =              'TM_ROTAXIS';
  ASCII_ROTANGLE_S =             'TM_ROTANGLE';
  ASCII_SCALE_S =                'TM_SCALE';
  ASCII_SCALEAXIS_S =            'TM_SCALEAXIS';
  ASCII_SCALEAXISANG_S =         'TM_SCALEAXISANG';
  ASCII_MESH_S =                 'MESH';
  ASCII_TIMEVALUE_S =            'TIMEVALUE';
  ASCII_MESH_NUM_VERTEX_S =      'MESH_NUMVERTEX';
  ASCII_NUM_FACES_S =            'MESH_NUMFACES';
  ASCII_MESH_VERTEX_S =          'MESH_VERTEX';
  ASCII_VERTEX_LIST_S =          'MESH_VERTEX_LIST';
  ASCII_MESH_FACE_S =            'MESH_FACE';
  ASCII_MESH_FACE_LIST_S =       'MESH_FACE_LIST';
  ASCII_MESH_NORMALS_S =         'MESH_NORMALS';
  ASCII_MESH_FACE_NORMAL_S =     'MESH_FACENORMAL';
  ASCII_MESH_VERTEX_NORMAL_S =   'MESH_VERTEXNORMAL';
  ASCII_PROP_MOTION_BLUR_S =     'PROP_MOTIONBLUR';
  ASCII_PROP_CAST_SHADOW_S =     'PROP_CASTSHADOW';
  ASCII_PROP_RECV_SHADOW_S =     'PROP_RECVSHADOW';
  ASCII_ROW1_S =                 'TM_ROW1';
  ASCII_ROW2_S =                 'TM_ROW2';
  ASCII_ROW3_S =                 'TM_ROW3';
  ASCII_MESH_NUMTVERTEX_S =      'MESH_NUMTVERTEX';
  ASCII_MESH_TVERTLIST_S =       'MESH_TVERTLIST';
  ASCII_MESH_NUMTVFACES_S =      'MESH_NUMTVFACES';
  ASCII_MESH_TFACELIST_S =       'MESH_TFACELIST';
  ASCII_MESH_MAPPINGCHANNEL_S =  'MESH_MAPPINGCHANNEL';
  ASCII_MATERIAL_S =             'MATERIAL';
  ASCII_MATERIAL_LIST_S =        'MATERIAL_LIST';
  ASCII_MATERIAL_COUNT_S =       'MATERIAL_COUNT';
  ASCII_MATERIAL_NAME_S =        'MATERIAL_NAME';
  ASCII_MATERIAL_CLASS_S =       'MATERIAL_CLASS';
  ASCII_MATERIAL_AMBIENT_S =     'MATERIAL_AMBIENT';
  ASCII_MATERIAL_DIFFUSE_S =     'MATERIAL_DIFFUSE';
  ASCII_MATERIAL_SPECULAR_S =    'MATERIAL_SPECULAR';
  ASCII_MATERIAL_SHINE_S =       'MATERIAL_SHINE';
  ASCII_MATERIAL_SHINESTRENGTH_S = 'MATERIAL_SHINESTRENGTH';
  ASCII_MATERIAL_TRANSPARENCY_S = 'MATERIAL_TRANSPARENCY';
  ASCII_MATERIAL_WIRESIZE_S =    'MATERIAL_WIRESIZE';
  ASCII_NUMSUBMTLS_S =           'NUMSUBMTLS';
  ASCII_SUBMATERIAL_S =          'SUBMATERIAL';
  ASCII_MATERIAL_SHADING_S =     'MATERIAL_SHADING';
  ASCII_MATERIAL_XP_FALLOFF_S =  'MATERIAL_XP_FALLOFF';
  ASCII_MATERIAL_SELFILLUM_S =   'MATERIAL_SELFILLUM';
  ASCII_MATERIAL_FALLOFF_S =     'MATERIAL_FALLOFF';
  ASCII_MATERIAL_XP_TYPE_S =     'MATERIAL_XP_TYPE';
  ASCII_MAP_DIFFUSE_S =          'MAP_DIFFUSE';
  ASCII_MAP_NAME_S =             'MAP_NAME';
  ASCII_MAP_CLASS_S =            'MAP_CLASS';
  ASCII_MAP_SUBNO_S =            'MAP_SUBNO';
  ASCII_MAP_AMOUNT_S =           'MAP_AMOUNT';
  ASCII_BITMAP_S =               'BITMAP';
  ASCII_MAP_TYPE_S =             'MAP_TYPE';
  ASCII_UVW_U_OFFSET_S =         'UVW_U_OFFSET';
  ASCII_UVW_V_OFFSET_S =         'UVW_V_OFFSET';
  ASCII_UVW_U_TILING_S =         'UVW_U_TILING';
  ASCII_UVW_V_TILING_S =         'UVW_V_TILING';
  ASCII_UVW_ANGLE_S =            'UVW_ANGLE';
  ASCII_UVW_BLUR_S =             'UVW_BLUR';
  ASCII_UVW_BLUR_OFFSET_S =      'UVW_BLUR_OFFSET';
  ASCII_UVW_NOUSE_AMT_S =        'UVW_NOUSE_AMT';
  ASCII_UVW_NOISE_SIZE_S =       'UVW_NOISE_SIZE';
  ASCII_UVW_NOISE_LEVEL_S =      'UVW_NOISE_LEVEL';
  ASCII_UVW_NOISE_PHASE_S =      'UVW_NOISE_PHASE';
  ASCII_BITMAP_FILTER_S =        'BITMAP_FILTER';
  ASCII_MESH_SMOOTHING_S =       'MESH_SMOOTHING';
  ASCII_MESH_MTLID_S =           'MESH_MTLID';
  ASCII_MATERIAL_REF_S =         'MATERIAL_REF';


const
  ASCII_COMMENT_I =              0;
  ASCII_SCENE_I =                1;
  ASCII_GEOMOBJECT_I =           2;
  ASCII_NODE_NAME_I =            3;
  ASCII_NODE_TM_I =              4;
  ASCII_INHERIT_POS_I =          5;
  ASCII_INHERIT_ROT_I =          6;
  ASCII_INHERIT_SCL_I =          7;
  ASCII_ROW_I =                  8;
  ASCII_POS_I =                  9;
  ASCII_ROTAXIS_I =              10;
  ASCII_ROTANGLE_I =             11;
  ASCII_SCALE_I =                12;
  ASCII_SCALEAXIS_I =            13;
  ASCII_SCALEAXISANG_I =         14;
  ASCII_MESH_I =                 15;
  ASCII_TIMEVALUE_I =            16;
  ASCII_MESH_NUM_VERTEX_I =      17;
  ASCII_NUM_FACES_I =            18;
  ASCII_MESH_VERTEX_I =          19;
  ASCII_VERTEX_LIST_I =          20;
  ASCII_MESH_FACE_I =            21;
  ASCII_MESH_FACE_LIST_I =       22;
  ASCII_MESH_NORMALS_I =         23;
  ASCII_MESH_FACE_NORMAL_I =     24;
  ASCII_MESH_VERTEX_NORMAL_I =   25;
  ASCII_PROP_MOTION_BLUR_I =     26;
  ASCII_PROP_CAST_SHADOW_I =     27;
  ASCII_PROP_RECV_SHADOW_I =     28;
  ASCII_ROW1_I =                 29;
  ASCII_ROW2_I =                 30;
  ASCII_ROW3_I =                 31;
  ASCII_MESH_NUMTVERTEX_I =      32;
  ASCII_MESH_TVERTLIST_I =       33;
  ASCII_MESH_NUMTVFACES_I =      34;
  ASCII_MESH_TFACELIST_I =       35;
  ASCII_MESH_MAPPINGCHANNEL_I =  36;
  ASCII_MATERIAL_I =             37;
  ASCII_MATERIAL_LIST_I =        38;
  ASCII_MATERIAL_COUNT_I =       39;
  ASCII_MATERIAL_NAME_I =        40;
  ASCII_MATERIAL_CLASS_I =       41;
  ASCII_MATERIAL_AMBIENT_I =     42;
  ASCII_MATERIAL_DIFFUSE_I =     43;
  ASCII_MATERIAL_SPECULAR_I =    44;
  ASCII_MATERIAL_SHINE_I =       45;
  ASCII_MATERIAL_SHINESTRENGTH_I = 46;
  ASCII_MATERIAL_TRANSPARENCY_I = 47;
  ASCII_MATERIAL_WIRESIZE_I =    48;
  ASCII_NUMSUBMTLS_I =           49;
  ASCII_SUBMATERIAL_I =          50;
  ASCII_MATERIAL_SHADING_I =     51;
  ASCII_MATERIAL_XP_FALLOFF_I =  52;
  ASCII_MATERIAL_SELFILLUM_I =   53;
  ASCII_MATERIAL_FALLOFF_I =     54;
  ASCII_MATERIAL_XP_TYPE_I =     55;
  ASCII_MAP_DIFFUSE_I =          56;
  ASCII_MAP_NAME_I =             57;
  ASCII_MAP_CLASS_I =            58;
  ASCII_MAP_SUBNO_I =            59;
  ASCII_MAP_AMOUNT_I =           60;
  ASCII_BITMAP_I =               61;
  ASCII_MAP_TYPE_I =             62;
  ASCII_UVW_U_OFFSET_I =         63;
  ASCII_UVW_V_OFFSET_I =         64;
  ASCII_UVW_U_TILING_I =         65;
  ASCII_UVW_V_TILING_I =         66;
  ASCII_UVW_ANGLE_I =            67;
  ASCII_UVW_BLUR_I =             68;
  ASCII_UVW_BLUR_OFFSET_I =      69;
  ASCII_UVW_NOUSE_AMT_I =        70;
  ASCII_UVW_NOISE_SIZE_I =       71;
  ASCII_UVW_NOISE_LEVEL_I =      72;
  ASCII_UVW_NOISE_PHASE_I =      73;
  ASCII_BITMAP_FILTER_I =        74;
  ASCII_MESH_SMOOTHING_I =       75;
  ASCII_MESH_MTLID_I =           76;
  ASCII_MATERIAL_REF_I =         77;

const
  ASCII_MESH_OT = 0;
  ASCII_LIGHT_OT = 1;

type
  TTagIdx = record
    Idx: Integer;
    Name: string;
  end;

//  WARNING: if vTagIdx elements Names are substring one to each other, it must be arragned by Name length,
//  for correct fast lookup. i.e.:
//  MESH_FACE must be placed before MESH_FACE_LIST
const
  vTagIdx: array [0..77] of TTagIdx = (
    (Idx: ASCII_COMMENT_I;              Name: ASCII_COMMENT_S),
    (Idx: ASCII_SCENE_I;                Name: ASCII_SCENE_S),
    (Idx: ASCII_GEOMOBJECT_I;           Name: ASCII_GEOMOBJECT_S),
    (Idx: ASCII_NODE_NAME_I;            Name: ASCII_NODE_NAME_S),
    (Idx: ASCII_NODE_TM_I;              Name: ASCII_NODE_TM_S),
    (Idx: ASCII_INHERIT_POS_I;          Name: ASCII_INHERIT_POS_S),
    (Idx: ASCII_INHERIT_ROT_I;          Name: ASCII_INHERIT_ROT_S),
    (Idx: ASCII_INHERIT_SCL_I;          Name: ASCII_INHERIT_SCL_S),
    (Idx: ASCII_ROW_I;                  Name: ASCII_ROW_S),
    (Idx: ASCII_POS_I;                  Name: ASCII_POS_S),
    (Idx: ASCII_ROTAXIS_I;              Name: ASCII_ROTAXIS_S),
    (Idx: ASCII_ROTANGLE_I;             Name: ASCII_ROTANGLE_S),
    (Idx: ASCII_SCALE_I;                Name: ASCII_SCALE_S),
    (Idx: ASCII_SCALEAXIS_I;            Name: ASCII_SCALEAXIS_S),
    (Idx: ASCII_SCALEAXISANG_I;         Name: ASCII_SCALEAXISANG_S),
    (Idx: ASCII_MESH_I;                 Name: ASCII_MESH_S),
    (Idx: ASCII_TIMEVALUE_I;            Name: ASCII_TIMEVALUE_S),
    (Idx: ASCII_MESH_NUM_VERTEX_I;      Name: ASCII_MESH_NUM_VERTEX_S),
    (Idx: ASCII_NUM_FACES_I;            Name: ASCII_NUM_FACES_S),
    (Idx: ASCII_MESH_VERTEX_I;          Name: ASCII_MESH_VERTEX_S),
    (Idx: ASCII_VERTEX_LIST_I;          Name: ASCII_VERTEX_LIST_S),
    (Idx: ASCII_MESH_FACE_I;            Name: ASCII_MESH_FACE_S),
    (Idx: ASCII_MESH_FACE_LIST_I;       Name: ASCII_MESH_FACE_LIST_S),
    (Idx: ASCII_MESH_NORMALS_I;         Name: ASCII_MESH_NORMALS_S),
    (Idx: ASCII_MESH_FACE_NORMAL_I;     Name: ASCII_MESH_FACE_NORMAL_S),
    (Idx: ASCII_MESH_VERTEX_NORMAL_I;   Name: ASCII_MESH_VERTEX_NORMAL_S),
    (Idx: ASCII_PROP_MOTION_BLUR_I;     Name: ASCII_PROP_MOTION_BLUR_S),
    (Idx: ASCII_PROP_CAST_SHADOW_I;     Name: ASCII_PROP_CAST_SHADOW_S),
    (Idx: ASCII_PROP_RECV_SHADOW_I;     Name: ASCII_PROP_RECV_SHADOW_S),
    (Idx: ASCII_ROW1_I;                 Name: ASCII_ROW1_S),
    (Idx: ASCII_ROW2_I;                 Name: ASCII_ROW2_S),
    (Idx: ASCII_ROW3_I;                 Name: ASCII_ROW3_S),
    (Idx: ASCII_MESH_NUMTVERTEX_I;      Name: ASCII_MESH_NUMTVERTEX_S),
    (Idx: ASCII_MESH_TVERTLIST_I;       Name: ASCII_MESH_TVERTLIST_S),
    (Idx: ASCII_MESH_NUMTVFACES_I;      Name: ASCII_MESH_NUMTVFACES_S),
    (Idx: ASCII_MESH_TFACELIST_I;       Name: ASCII_MESH_TFACELIST_S),
    (Idx: ASCII_MESH_MAPPINGCHANNEL_I;  Name: ASCII_MESH_MAPPINGCHANNEL_S),
    (Idx: ASCII_MATERIAL_I;             Name: ASCII_MATERIAL_S),
    (Idx: ASCII_MATERIAL_LIST_I;        Name: ASCII_MATERIAL_LIST_S),
    (Idx: ASCII_MATERIAL_COUNT_I;       Name: ASCII_MATERIAL_COUNT_S),
    (Idx: ASCII_MATERIAL_NAME_I;        Name: ASCII_MATERIAL_NAME_S),
    (Idx: ASCII_MATERIAL_CLASS_I;       Name: ASCII_MATERIAL_CLASS_S),
    (Idx: ASCII_MATERIAL_AMBIENT_I;     Name: ASCII_MATERIAL_AMBIENT_S),
    (Idx: ASCII_MATERIAL_DIFFUSE_I;     Name: ASCII_MATERIAL_DIFFUSE_S),
    (Idx: ASCII_MATERIAL_SPECULAR_I;    Name: ASCII_MATERIAL_SPECULAR_S),
    (Idx: ASCII_MATERIAL_SHINE_I;       Name: ASCII_MATERIAL_SHINE_S),
    (Idx: ASCII_MATERIAL_SHINESTRENGTH_I; Name: ASCII_MATERIAL_SHINESTRENGTH_S),
    (Idx: ASCII_MATERIAL_TRANSPARENCY_I; Name: ASCII_MATERIAL_TRANSPARENCY_S),
    (Idx: ASCII_MATERIAL_WIRESIZE_I;    Name: ASCII_MATERIAL_WIRESIZE_S),
    (Idx: ASCII_NUMSUBMTLS_I;           Name: ASCII_NUMSUBMTLS_S),
    (Idx: ASCII_SUBMATERIAL_I;          Name: ASCII_SUBMATERIAL_S),
    (Idx: ASCII_MATERIAL_SHADING_I;     Name: ASCII_MATERIAL_SHADING_S),
    (Idx: ASCII_MATERIAL_XP_FALLOFF_I;  Name: ASCII_MATERIAL_XP_FALLOFF_S),
    (Idx: ASCII_MATERIAL_SELFILLUM_I;   Name: ASCII_MATERIAL_SELFILLUM_S),
    (Idx: ASCII_MATERIAL_FALLOFF_I;     Name: ASCII_MATERIAL_FALLOFF_S),
    (Idx: ASCII_MATERIAL_XP_TYPE_I;     Name: ASCII_MATERIAL_XP_TYPE_S),
    (Idx: ASCII_MAP_DIFFUSE_I;          Name: ASCII_MAP_DIFFUSE_S),
    (Idx: ASCII_MAP_NAME_I;             Name: ASCII_MAP_NAME_S),
    (Idx: ASCII_MAP_CLASS_I;            Name: ASCII_MAP_CLASS_S),
    (Idx: ASCII_MAP_SUBNO_I;            Name: ASCII_MAP_SUBNO_S),
    (Idx: ASCII_MAP_AMOUNT_I;           Name: ASCII_MAP_AMOUNT_S),
    (Idx: ASCII_BITMAP_I;               Name: ASCII_BITMAP_S),
    (Idx: ASCII_MAP_TYPE_I;             Name: ASCII_MAP_TYPE_S),
    (Idx: ASCII_UVW_U_OFFSET_I;         Name: ASCII_UVW_U_OFFSET_S),
    (Idx: ASCII_UVW_V_OFFSET_I;         Name: ASCII_UVW_V_OFFSET_S),
    (Idx: ASCII_UVW_U_TILING_I;         Name: ASCII_UVW_U_TILING_S),
    (Idx: ASCII_UVW_V_TILING_I;         Name: ASCII_UVW_V_TILING_S),
    (Idx: ASCII_UVW_ANGLE_I;            Name: ASCII_UVW_ANGLE_S),
    (Idx: ASCII_UVW_BLUR_I;             Name: ASCII_UVW_BLUR_S),
    (Idx: ASCII_UVW_BLUR_OFFSET_I;      Name: ASCII_UVW_BLUR_OFFSET_S),
    (Idx: ASCII_UVW_NOUSE_AMT_I;        Name: ASCII_UVW_NOUSE_AMT_S),
    (Idx: ASCII_UVW_NOISE_SIZE_I;       Name: ASCII_UVW_NOISE_SIZE_S),
    (Idx: ASCII_UVW_NOISE_LEVEL_I;      Name: ASCII_UVW_NOISE_LEVEL_S),
    (Idx: ASCII_UVW_NOISE_PHASE_I;      Name: ASCII_UVW_NOISE_PHASE_S),
    (Idx: ASCII_BITMAP_FILTER_I;        Name: ASCII_BITMAP_FILTER_S),
    (Idx: ASCII_MESH_SMOOTHING_I;       Name: ASCII_MESH_SMOOTHING_S),
    (Idx: ASCII_MESH_MTLID_I;           Name: ASCII_MESH_MTLID_S),
    (Idx: ASCII_MATERIAL_REF_I;         Name: ASCII_MATERIAL_REF_S)
  );

procedure ChangeDotToComma(var aValue: string);
var
  Index: Integer;
begin
  for Index := 1 to Length(aValue) do
    if aValue[Index] = '.' then
      aValue[Index] := ',';
end;

procedure ChangeCommaToDot(var aValue: string);
var
  Index: Integer;
begin
  for Index := 1 to Length(aValue) do
    if aValue[Index] = ',' then
      aValue[Index] := '.';
end;

procedure ChangeTabToSpace(var aValue: string);
var
  Index: Integer;
begin
  for Index := 1 to Length(aValue) do
    if aValue[Index] = #9 then
      aValue[Index] := #32;
end;

function StringToFloatRegular(aValue: string): Double;
begin
  Result := StrToFloatDef(aValue, 0);
  if Result = 0 then begin
    ChangeDotToComma(aValue);
    Result := StrToFloatDef(aValue, 0);
    if Result = 0 then begin
      ChangeCommaToDot(aValue);
      Result := StrToFloatDef(aValue, 0);
    end;
  end;
end;


// here ASE geom object is converted to GLScene mesh
procedure CopyASEToMesh(aASEMesh: TGLASEMeshObject; aMesh: TMeshObject; aASEMaterials: TGLASEMaterialList);

   function GetOrAllocateMaterial(const aIndex, aSubID: Integer) : string;
   var
      material : TGLASEMaterial;
      specColor : TVector;
      matLib : TGLMaterialLibrary;
      libMat : TGLLibMaterial;
   begin
      material := aASEMaterials[aIndex];
      Assert(Assigned(material));
      if Assigned(aMesh.Owner) and Assigned(aMesh.Owner.Owner) then begin
         matLib := aMesh.Owner.Owner.MaterialLibrary;
         if Assigned(matLib) then begin
            Result := material.Name + IntToStr(aIndex) + IntToStr(aSubID);
            libMat := matLib.Materials.GetLibMaterialByName(Result);
            if not Assigned(libMat) then begin
               libMat:=matLib.Materials.Add;
               libMat.Name := Result;
               with libMat.Material.FrontProperties do begin
                  Ambient.Color:= VectorMake(material.Ambient, 1);
                  Diffuse.Color:= VectorMake(material.Diffuse, 1);
                  specColor := VectorMake(material.Specular, 1);
                  ScaleVector(specColor, 1 - material.Shiness);
                  Specular.Color:=specColor;
                  Shininess:=MaxInteger(0, Integer(Round((1 - material.ShineStrength) * 128)));
               end;

               if (material.SubMaterials.Count > 0{aSubID}) and
                  (Trim(material.SubMaterials.SubMaterial[0{aSubID}].DiffuseMap.Bitmap)<>'') then begin
                  try
                     with libMat.Material.Texture do begin
                        Image.LoadFromFile(material.SubMaterials.SubMaterial[0{aSubID}].DiffuseMap.Bitmap);
                        Disabled:=False;
                        TextureMode:=tmModulate;
                     end;
                  except
                     on E: ETexture do begin
                        if not aMesh.Owner.Owner.IgnoreMissingTextures then
                           raise;
                     end;
                  end;
               end;
            end;
         end else Result:='';
      end else Result:='';
   end;

   function GetOrAllocateLightMap(const aIndex, aSubID: Integer) : Integer;
   var
      material : TGLASEMaterial;
      matLib : TGLMaterialLibrary;
      libMat : TGLLibMaterial;
      name: string;
   begin
      Result:=-1;
      material := aASEMaterials.Material[aIndex];
      name := material.Name + IntToStr(aIndex) + IntToStr(aSubID);
      Assert(Assigned(material));
      if Assigned(aMesh.Owner) and Assigned(aMesh.Owner.Owner) then begin
         matLib := aMesh.Owner.Owner.LightmapLibrary;
         if Assigned(matLib) then begin
             if (material.SubMaterials.Count > 1) and
                (Trim(material.SubMaterials.SubMaterial[1].DiffuseMap.Bitmap)<>'') then begin
               libMat:=matLib.Materials.GetLibMaterialByName(name);
               if not Assigned(libMat) then begin
                  libMat:=matLib.Materials.Add;
                  libMat.Name:=name;
                  try
                     with libMat.Material.Texture do begin
                        Image.LoadFromFile(material.SubMaterials.SubMaterial[1].DiffuseMap.Bitmap);
                        Disabled:=False;
                        TextureMode:=tmModulate;
                     end;
                  except
                     on E: ETexture do begin
                        if not aMesh.Owner.Owner.IgnoreMissingTextures then
                           raise;
                     end;
                  end;
               end;
               Result:=libmat.Index;
            end;
         end;
      end;
   end;

   var
    vLastFG: TFGVertexIndexList;

   function GetFaceGroup(aMaterialID, aSubMaterialID: Integer): TFGVertexIndexList;
   var
     i: Integer;
     name: string;
   begin
    Result := nil;
    name := aASEMaterials[aMaterialID].Name + IntToStr(aMaterialID) + IntToStr(aSubMaterialID);
    if Assigned(vLastFG) and (vLastFG.MaterialName = name) then
      Result := vLastFG
    else begin
      vLastFG := nil;
      for i := 0 to aMesh.FaceGroups.Count - 1 do
        if aMesh.FaceGroups.Items[i].MaterialName = name then begin
          Result := TFGVertexIndexList(aMesh.FaceGroups.Items[i]);
          vLastFG := Result;
          Break;
        end;
      if not Assigned(Result) then begin
        Result := TFGVertexIndexList.CreateOwned(aMesh.FaceGroups);
        if aMaterialID > -1 then begin
          Result.MaterialName := GetOrAllocateMaterial(aMaterialID, aSubMaterialID);
          Result.LightMapIndex := GetOrAllocateLightMap(aMaterialID, aSubMaterialID);
        end;
        vLastFG := Result;
      end;
    end;
   end;

var
  fg: TFGVertexIndexList;
  aseFace: TGLASEFace;
  i: Integer;
  norm, tex, light: Boolean;
  lmt: array [0..2] of TAffineVector;
  subID: Integer;
  vi: TIntegerList;
begin
  norm := aASEMesh.HasNormals;
  tex := aASEMesh.TextChannelsCount > 0;
  light := tex and (aASEMesh.TextChannelsCount > 1);
  subID := -1;

  vi := TIntegerList.Create;
  if tex or norm then begin
    // here used NOT optimized storage

    aMesh.Mode := momFaceGroups;

    aMesh.Vertices.Capacity := aASEMesh.Faces.Count*3;
    if norm then
      aMesh.Normals.Capacity := aASEMesh.Faces.Count*3;
    if tex then
      aMesh.Normals.Capacity := aASEMesh.Faces.Count*3;


    vLastFG := nil;
    for i := 0 to aASEMesh.Faces.Count - 1 do begin
      aseFace := aASEMesh.Faces[i];
      fg := GetFaceGroup(aASEMesh.MaterialID, aseFace.SubMaterialID);

      if (aseFace.SubMaterialID > -1) and (subID = -1) then
        subID := aseFace.SubMaterialID;

      aMesh.Vertices.Add(aASEMesh.Vertices[aseFace.VertIdx1],
                         aASEMesh.Vertices[aseFace.VertIdx2],
                         aASEMesh.Vertices[aseFace.VertIdx3]);
      if norm then
        aMesh.Normals.Add(aseFace.Normal1, aseFace.Normal2, aseFace.Normal3);

      if tex then begin
        Assert(aseFace.TextChannels.Count = aASEMesh.TextChannelsCount);
        aMesh.TexCoords.Add(aASEMesh.TextChannel[0].Items[aseFace.TextChannels.ChanelTexture[0].Idx0],
                            aASEMesh.TextChannel[0].Items[aseFace.TextChannels.ChanelTexture[0].Idx1],
                            aASEMesh.TextChannel[0].Items[aseFace.TextChannels.ChanelTexture[0].Idx2]);
        if light then begin
          lmt[0] := aASEMesh.TextChannel[1].Items[aseFace.TextChannels.ChanelTexture[1].Idx0];
          lmt[1] := aASEMesh.TextChannel[1].Items[aseFace.TextChannels.ChanelTexture[1].Idx1];
          lmt[2] := aASEMesh.TextChannel[1].Items[aseFace.TextChannels.ChanelTexture[1].Idx2];
          aMesh.LightMapTexCoords.Add(lmt[0][0], lmt[0][1]);
          aMesh.LightMapTexCoords.Add(lmt[1][0], lmt[1][1]);
          aMesh.LightMapTexCoords.Add(lmt[2][0], lmt[2][1]);
        end;
      end;

      fg.VertexIndices.Add(i*3 + 0, i*3 + 1, i*3 + 2);
      vi.Add(i*3 + 0, i*3 + 1, i*3 + 2);
    end;
  end else begin
    fg := TFGVertexIndexList.CreateOwned(aMesh.FaceGroups);
    aMesh.Vertices.Assign(aASEMesh.Vertices);
    aMesh.Mode := momTriangles;
    fg.VertexIndices.Capacity := aASEMesh.Faces.Count*3;
    for i := 0 to aASEMesh.Faces.Count - 1 do begin
      aseFace := aASEMesh.Faces[i];
      fg.VertexIndices.Add(aseFace.VertIdx1, aseFace.VertIdx2, aseFace.VertIdx3);
    end;
  end;

  // if ASE does not contain normals data we should build it automatically
  if aMesh.Normals.Count = 0 then
    aMesh.BuildNormals(vi, momTriangles);
  vi.Free;
end;

{ TGLASEFace }

constructor TGLASEFace.Create;
begin
  FTextChannels.Count := 0;
  FSmoothing.Count := 0;
end;

{ TGLASEFaceList }

constructor TGLASEFaceList.Create;
begin
  FItems := TList.Create;
end;

destructor TGLASEFaceList.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited;
end;

function TGLASEFaceList.Add: TGLASEFace;
begin
  Result := TGLASEFace.Create;
  FItems.Add(Result);
end;

procedure TGLASEFaceList.Delete(aIndex: Integer);
begin
  Face[aIndex].Free;
  FItems.Delete(aIndex);
end;

procedure TGLASEFaceList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Face[i].Free;
  FItems.Clear;
end;

function TGLASEFaceList.GetFace(Index: Integer): TGLASEFace;
begin
  Result := FItems[Index];
end;

function TGLASEFaceList.GetCount: Integer;
begin
  Result := FItems.Count;
end;

{ TGLASEMaterial }

constructor TGLASEMaterial.Create;
begin
  FSubMaterials.Count := 0;
end;

{ TGLASEMeshObject }

constructor TGLASEMeshObject.Create;
begin
  FFaces := TGLASEFaceList.Create;
  FVertices := TAffineVectorList.Create;
  FTexChannelsCount := 0;
  FHasNormals := False;
  FMaterialID := -1;
end;

destructor TGLASEMeshObject.Destroy;
var
  i: Integer;
begin
  FreeAndNil(FFaces);
  FreeAndNil(FVertices);
  for i := 0 to FTexChannelsCount - 1 do
    FTexChannels[i].Free;
  inherited;
end;

function TGLASEMeshObject.AddTexChannel: TAffineVectorList;
begin
  Assert(FTexChannelsCount < MAX_TEXURE_CHANNELS, 'texture channels count maximum reached');
  Result := TAffineVectorList.Create;
  FTexChannels[FTexChannelsCount] := Result;
  Inc(FTexChannelsCount);
end;

function TGLASEMeshObject.GetTextChannel(Channel: Integer): TAffineVectorList;
begin
  Result := FTexChannels[Channel];
end;

{ TGLASEMaterialList }

constructor TGLASEMaterialList.Create;
begin
  FItems := TList.Create;
end;

destructor TGLASEMaterialList.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited;
end;

function TGLASEMaterialList.Add: TGLASEMaterial;
begin
  Result := TGLASEMaterial.Create;
  FItems.Add(Result);
end;

procedure TGLASEMaterialList.Delete(aIndex: Integer);
begin
  Material[aIndex].Free;
  FItems.Delete(aIndex);
end;

procedure TGLASEMaterialList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Material[i].Free;
  FItems.Clear;
end;

function TGLASEMaterialList.GetMaterial(Index: Integer): TGLASEMaterial;
begin
  Result := FItems[Index];
end;

function TGLASEMaterialList.GetCount: Integer;
begin
  Result := FItems.Count;
end;

{ TGLASEVectorFile }

constructor TGLASEVectorFile.Create;
begin
  inherited;
  FStringData := TStringList.Create;
  FMaterialList := TGLASEMaterialList.Create;
end;

destructor TGLASEVectorFile.Destroy;
begin
  FMaterialList.Free;
  FStringData.Free;
  inherited;
end;

class function TGLASEVectorFile.Capabilities: TDataFileCapabilities;
begin
  Result := [dfcRead];
end;

procedure TGLASEVectorFile.LoadFromStream(aStream: TStream);
begin
  FStringData.LoadFromStream(aStream);
  Parse;
end;

function TGLASEVectorFile.ContainString(const aData, aString: string): Boolean;
begin
  Result := (Length(aData) >= Length(aString)) and (Pos(AString, aData) > 0);
end;

function TGLASEVectorFile.GetDoubleValue(const aData: string): Double;
var
  Start: Integer;
  Data: string;
  Value: string;
begin
  Start := GetEndOfFirstValue(aData);
  Data := Copy(aData, Start, Length(aData) - Start + 1);
  Value := GetFirstValue(Data);
  Result := StringToFloatRegular(Value);
end;

function TGLASEVectorFile.GetEndOfFirstValue(const aData: string): Integer;
var
  Value: string;
begin
  Value := GetFirstValue(aData);
  Result := Pos(Value, aData) + Length(Value);
end;

function TGLASEVectorFile.GetFirstValue(aData: string): string;
const
  sep1 = #32;
var
  Index: Integer;
  start, ending: Integer;
begin
  ChangeTabToSpace(aData);
  start := 1;
  ending := Length(aData);

  // getting start
  for Index := 1 to Length(aData) do
    if aData[Index] <> sep1 then begin
      start := Index;
      Break;
    end;

  // getting ending
  Result :='';
  for Index := start to Length(aData) do
    if aData[Index] = sep1 then begin
      ending := Index;
      Break;
    end;

  if ending = Length(aData) then
    Inc(ending);

  Result := Copy(aData, start, ending - start);
end;

function TGLASEVectorFile.GetPropCastShadow(const aData: string): Boolean;
begin
  Result := Pos('1', aData) > 0;
end;

function TGLASEVectorFile.GetPropMBlur(const aData: string): Boolean;
begin
  Result := Pos('1', aData) > 0;
end;

function TGLASEVectorFile.GetPropRECVShadow(const aData: string): Boolean;
begin
  Result := Pos('1', aData) > 0;
end;

function TGLASEVectorFile.GetStringValue(const aData: string): string;
const
  Breaker = '"';
var
  Start: Integer;
begin
  Start := Pos(Breaker, aData) + 1;
  Result := Copy(aData, Start, Length(aData) - Start);
end;

function TGLASEVectorFile.GetTagOnData(const aData: string): Integer;
var
  Index: Integer;
begin
  Result := -1;
  for Index := High(vTagIdx) downto Low(vTagIdx) do
    if ContainString(aData, vTagIdx[Index].Name) then begin
      Result := vTagIdx[Index].Idx;
      Break;
    end;
end;

function TGLASEVectorFile.GetValue3D(const aData: string): TAffineVector;
var
  Start: Integer;
  Data: string;
  Value: String;
begin
  Data := aData;

  Start := GetEndOfFirstValue(Data);
  Data := Copy(Data, Start, Length(Data) - Start + 1);
  Value := GetFirstValue(Data);
  Result[0] := StringToFloatRegular(Value);

  Start := GetEndOfFirstValue(Data) + 1;
  Data := Copy(Data, Start, Length(Data) - Start + 1);
  Value := GetFirstValue(Data);
  Result[1] := StringToFloatRegular(Value);

  Start := GetEndOfFirstValue(Data) + 1;
  Data := Copy(Data, Start, Length(Data) - Start + 1);
  Value := GetFirstValue(Data);
  Result[2] := StringToFloatRegular(Value);
end;

function TGLASEVectorFile.GetValue4D(const aData: string; var Value0: Integer): TAffineVector;
var
  Start: Integer;
  Data: string;
  Value: String;
begin
  Data := aData;

  Value0 := Round(GetDoubleValue(aData));
  
  Start := GetEndOfFirstValue(Data) + 1;
  Data := Copy(Data, Start, Length(Data) - Start + 1);

  Start := GetEndOfFirstValue(Data) + 1;
  Data := Copy(Data, Start, Length(Data) - Start + 1);
  Value := GetFirstValue(Data);
  Result[0] := StringToFloatRegular(Value);
  
  Start := GetEndOfFirstValue(Data) + 1;
  Data := Copy(Data, Start, Length(Data) - Start + 1);
  Value := GetFirstValue(Data);
  Result[1] := StringToFloatRegular(Value);

  Start := GetEndOfFirstValue(Data) + 1;
  Data := Copy(Data, Start, Length(Data) - Start + 1);
  Value := GetFirstValue(Data);
  Result[2] := StringToFloatRegular(Value);
end;

function TGLASEVectorFile.IsEndOfSection(const aData: string): Boolean;
begin
  if AData[1] = '}' then begin
    Result := True;
  end else begin
    Result := Pos('}', aData) > 0;
  end;
end;

procedure TGLASEVectorFile.Parse;
var
  LineIndex: Integer;
  Data: string;
begin
  LineIndex := 0;
  while LineIndex < FStringData.Count do begin
    Data := FStringData[LineIndex];
    case GetTagOnData(Data) of
      ASCII_COMMENT_I: begin
        FComment := GetStringValue(Data);
        Inc(LineIndex);
      end;
      ASCII_SCENE_I: begin
        ParseScene(LineIndex);
      end;
      ASCII_GEOMOBJECT_I: begin
        ParseGeomObject(LineIndex);
      end;
      ASCII_PROP_MOTION_BLUR_I: begin
        FMotionBlur := GetPropMBlur(Data);
        Inc(LineIndex);
      end;
      ASCII_PROP_CAST_SHADOW_I: begin
        FCastShadow := GetPropCastShadow(Data);
        Inc(LineIndex);
      end;
      ASCII_PROP_RECV_SHADOW_I: begin
        FRECVShadow := GetPropRECVShadow(Data);
        Inc(LineIndex);
      end;
      ASCII_MATERIAL_LIST_I: begin
        ParseMaterialList(LineIndex);
      end;
      else begin
        if LineIndex = 0 then begin
          FHeader := Data;
        end;
        Inc(LineIndex);
      end;
    end;
  end;
end;

procedure TGLASEVectorFile.ParseFaceString(const aData: string; var Index, A, B, C, AB, BC, CA, MatID: Integer;
  var Smooth: TGLASESmoothingGroups);
var
  Data: string;
  Start: Integer;
  Value: string;

  function GetNextValueStr: string;
  begin
    Start := GetEndOfFirstValue(Data);
    Data := Copy(Data, Start, Length(Data) - Start + 1);
    Start := GetEndOfFirstValue(Data);
    Data := Copy(Data, Start, Length(Data) - Start + 1);
    Result := GetFirstValue(Data);
  end;

  function GetNextValue: Integer;
  begin
    Result := StrToInt(GetNextValueStr);
  end;

  function GetLastValue: Integer;
  begin
    Start := GetEndOfFirstValue(Data);
    Data := Copy(Data, Start, Length(Data) - Start + 1);
    Result := StrToInt(GetFirstValue(Data));
  end;

  procedure GetNextValues(var aValues: TGLASESmoothingGroups);

    procedure iAdd(aIdx: Integer);
    begin
      aValues.Groups[aValues.Count] := aIdx;
      Inc(aValues.Count);
    end;

  const
    sep = ',';
  var
    v, sub: string;
    p: Integer;
  begin
    v := GetNextValueStr;
    if Trim(v) = '' then
      Exit;
    p := Pos(sep, v);
    if (p = 0) and (StrToIntDef(v, -1) = -1) then
      Exit;
    while p > 0 do begin
      aValues.Count := 0;
      sub := Copy(v, 1, p - 1);
      iAdd(StrToInt(sub));
      v := Copy(v, p + 1, Length(v));
      p := Pos(sep, v);
    end;
    iAdd(StrToInt(v));
  end;

begin
  Data := aData;

  Start := GetEndOfFirstValue(Data);
  Data := Copy(Data, Start, Length(Data) - Start + 1);
  // Index
  Value := GetFirstValue(Data);
  Value := Copy(Value, 1, Length(Value) - 1);
  Index := StrToInt(Value);

  A := GetNextValue;
  B := GetNextValue;
  C := GetNextValue;
  AB := GetNextValue;
  BC := GetNextValue;
  CA := GetNextValue;
  if Pos(ASCII_MESH_SMOOTHING_S, Data) > 0 then
    GetNextValues(Smooth);
  if Pos(ASCII_MESH_MTLID_S, Data) > 0 then begin
    if Smooth.Count > 0 then
      MatID := GetNextValue
    else
      MatID := GetLastValue;
  end;
end;

procedure TGLASEVectorFile.ParseGeomObject(var aLineIndex: Integer);
var
  aseMesh: TGLASEMeshObject;
  obj: TMeshObject;
  Data: string;
begin
  aseMesh := TGLASEMeshObject.Create;
  try
    obj := TMeshObject.CreateOwned(Owner.MeshObjects);

    Inc(aLineIndex);
    Data := FStringData[aLineIndex];
    while not IsEndOfSection(Data) do begin
      case GetTagOnData(Data) of
        ASCII_NODE_NAME_I:   obj.Name := GetStringValue(Data);
        ASCII_NODE_TM_I:     ParseMeshOptions(aLineIndex, aseMesh);
        ASCII_MESH_I:        ParseMeshGeom(aLineIndex, aseMesh);
        ASCII_MATERIAL_REF_I: aseMesh.FMaterialID := Round(GetDoubleValue(Data));
      end;
      Inc(aLineIndex);
      Data := FStringData[aLineIndex];
    end;

    CopyASEToMesh(aseMesh, obj, FMaterialList);
  finally
    aseMesh.Free;
  end;
end;

procedure TGLASEVectorFile.ParseMeshFaces(var aLineIndex: Integer; aMesh: TGLASEMeshObject; FacesCount: Integer);
var
  Data: string;
  Index, A, B, C, AB, BC, CA, ID: Integer;
  face: TGLASEFace;
  smooth: TGLASESmoothingGroups;
begin
  Inc(aLineIndex);
  Data := FStringData[aLineIndex];
  while not IsEndOfSection(Data) do begin
    smooth.Count := 0;
    ID := -1;
    ParseFaceString(Data, Index, A, B, C, AB, BC, CA, ID, smooth);
    Assert(Index = aMesh.Faces.Count, 'unexpexted unsorted faces');
    face := aMesh.Faces.Add;
    face.FV[0] := A;
    face.FV[1] := B;
    face.FV[2] := C;
    face.FSmoothing := smooth;
//    face.AB := AB;
//    face.BC := BC;
//    face.CA := CA;
//    face.FSmoothing := Smooth;
    face.FSubMaterialID := ID;

    Inc(aLineIndex);
    Data := FStringData[aLineIndex];
  end;
end;

procedure TGLASEVectorFile.ParseMeshNormals(var aLineIndex: Integer; aMesh: TGLASEMeshObject; FacesCount: Integer);
var
  Data: string;
  Index: Integer;
  Counter: Integer;
  face: TGLASEFace;
  Normal: TAffineVector;
begin
  aMesh.FHasNormals := True;
  Inc(aLineIndex);
  Data := FStringData[aLineIndex];
  Counter := 0;
  face := nil;
  while not IsEndOfSection(Data) do begin
    if Counter = 0 then begin
      Normal := GetValue4D(Data, Index);
      face := aMesh.Faces[Index];
      face.FNormal := Normal;
      Inc(Counter);
    end else begin
      Assert(Assigned(face));
      Normal := GetValue4D(Data, Index);
      Assert(face.FV[Counter - 1] = Index);
      face.FN[Counter - 1] := Normal;
      if Counter = 3 then begin
        Counter := 0;
        face := nil;
      end else
        Inc(Counter);
    end;
    Inc(aLineIndex);
    Data := FStringData[ALineIndex];
  end;
end;

procedure TGLASEVectorFile.ParseMeshTextureFaces(var aLineIndex: Integer; aMesh: TGLASEMeshObject;
  TextureFacesCount: Integer);
var
  Data: string;
  faceIndex: Integer;
  face: TGLASEFace;
  v3D: TAffineVector;
  chanelIdx: Integer;
begin
  Inc(aLineIndex);
  Data := FStringData[aLineIndex];
  while not IsEndOfSection(Data) do begin
    v3D := GetValue4D(Data, faceIndex);
    face := aMesh.Faces[faceIndex];

    chanelIdx := face.FTextChannels.Count;
    face.FTextChannels.ChanelTexture[chanelIdx].Idx0 := Round(v3D[0]);
    face.FTextChannels.ChanelTexture[chanelIdx].Idx1 := Round(v3D[1]);
    face.FTextChannels.ChanelTexture[chanelIdx].Idx2 := Round(v3D[2]);
    Inc(face.FTextChannels.Count);

    Inc(aLineIndex);
    Data := FStringData[aLineIndex];
  end;
end;

procedure TGLASEVectorFile.ParseMeshTextureVertices(var aLineIndex: Integer; aMesh: TGLASEMeshObject;
  TextureVerticesCount: Integer);
var
  Data: string;
  Index: Integer;
  channel: TAffineVectorList;
begin
  Inc(aLineIndex);
  Data := FStringData[aLineIndex];
  channel := aMesh.AddTexChannel;
  while not IsEndOfSection(Data) do begin
    channel.Add(GetValue4D(Data, Index));
    Assert(Index = (channel.Count - 1), 'unexpected unsorted texture coordinates');
    Inc(aLineIndex);
    Data := FStringData[aLineIndex];
  end;
end;

procedure TGLASEVectorFile.ParseMeshGeom(var aLineIndex: Integer; aMesh: TGLASEMeshObject);
var
  Data: string;
  VerticesCount: Integer;
  FacesCount: Integer;
  TextureVerticesCount: Integer;
  TextureFacesCount: Integer;
begin
  VerticesCount := 0;
  FacesCount := 0;
  TextureVerticesCount := 0;
  TextureFacesCount := 0;
  Inc(aLineIndex);
  Data := FStringData[aLineIndex];
  while not IsEndOfSection(Data) do begin
    case GetTagOnData(Data) of
      ASCII_TIMEVALUE_I:          ;//aMesh.TimeValue := Round(GetDoubleValue(Data));
      ASCII_MESH_NUM_VERTEX_I:      VerticesCount := Round(GetDoubleValue(Data));
      ASCII_NUM_FACES_I:            FacesCount := Round(GetDoubleValue(Data));
      ASCII_VERTEX_LIST_I:          ParseMeshVertices(aLineIndex, aMesh, VerticesCount);
      ASCII_MESH_FACE_LIST_I:       ParseMeshFaces(aLineIndex, aMesh, FacesCount);
      ASCII_MESH_NORMALS_I:         ParseMeshNormals(aLineIndex, aMesh, FacesCount);
      ASCII_MESH_NUMTVERTEX_I:      TextureVerticesCount := Round(GetDoubleValue(Data));
      ASCII_MESH_TVERTLIST_I:       ParseMeshTextureVertices(aLineIndex, aMesh, TextureVerticesCount);
      ASCII_MESH_NUMTVFACES_I:      TextureFacesCount := Round(GetDoubleValue(Data));
      ASCII_MESH_TFACELIST_I:       ParseMeshTextureFaces(aLineIndex, aMesh, TextureFacesCount);
      ASCII_MESH_MAPPINGCHANNEL_I:  ; // texure channels will be added automatically using tag MESH_FACE_LIST
    end;
    Inc(aLineIndex);
    Data := FStringData[aLineIndex];
  end;
  Inc(aLineIndex);
  Data := FStringData[aLineIndex];
end;

procedure TGLASEVectorFile.ParseMeshVertices(var aLineIndex: Integer; aMesh: TGLASEMeshObject;
  VerticesCount: Integer);
var
  Data: string;
  VertIndex: Integer;
begin
  Inc(aLineIndex);
  Data := FStringData[aLineIndex];
  while not IsEndOfSection(Data) do begin
    aMesh.Vertices.Add(GetValue4D(Data, VertIndex));
    Assert(VertIndex = (aMesh.Vertices.Count - 1), 'unexpeted unsorted vertices');
    Inc(aLineIndex);
    Data := FStringData[aLineIndex];
  end;
end;

procedure TGLASEVectorFile.ParseMeshOptions(var aLineIndex: Integer; aMesh: TGLASEMeshObject);
var
  Data: string;
begin
  Inc(aLineIndex);
  Data := FStringData[aLineIndex];
  while not IsEndOfSection(Data) do begin
    case GetTagOnData(Data) of
      ASCII_INHERIT_POS_I:  aMesh.FInheritedPosition := GetValue3D(Data);
      ASCII_INHERIT_ROT_I:  aMesh.FInheritedRotation := GetValue3D(Data);
      ASCII_INHERIT_SCL_I:  aMesh.FInheritedScale := GetValue3D(Data);
      ASCII_ROW_I:          aMesh.FMatrix[0] := VectorMake(GetValue3D(Data));
      ASCII_ROW1_I:         aMesh.FMatrix[1] := VectorMake(GetValue3D(Data));
      ASCII_ROW2_I:         aMesh.FMatrix[2] := VectorMake(GetValue3D(Data));
      ASCII_ROW3_I:         aMesh.FMatrix[3] := PointMake(GetValue3D(Data));
      ASCII_POS_I:          aMesh.FPosition := PointMake(GetValue3D(Data));
      ASCII_ROTAXIS_I:      aMesh.FRotationAxis := GetValue3D(Data);
      ASCII_ROTANGLE_I:     aMesh.FRotationAngle := GetDoubleValue(Data);
      ASCII_SCALE_I:        aMesh.FScale := GetValue3D(Data);
      ASCII_SCALEAXIS_I:    aMesh.FScaleAxis := GetValue3D(Data);
      ASCII_SCALEAXISANG_I: aMesh.FScaleAxisAngle := GetDoubleValue(Data);
    end;
    Inc(aLineIndex);
    Data := FStringData[aLineIndex];
  end;
end;

procedure TGLASEVectorFile.ParseScene(var aLineIndex: Integer);
var
  Data: string;
begin
  Inc(aLineIndex);
  Data := FStringData[aLineIndex];
  while not IsEndOfSection(Data) do begin
    Inc(aLineIndex);
    Data := FStringData[aLineIndex];
  end;
end;

procedure TGLASEVectorFile.ParseMaterialList(var aLineIndex: Integer);
var
  Data: string;
  material: TGLASEMaterial;
begin
  Inc(aLineIndex);
  Data := FStringData[aLineIndex];
  while not IsEndOfSection(Data) do begin
    case GetTagOnData(Data) of
      ASCII_MATERIAL_I: begin
        material := FMaterialList.Add;
        ParseMaterial(aLineIndex, material);
      end;
    end;
    Inc(aLineIndex);
    Data := FStringData[aLineIndex];
  end;
end;

procedure TGLASEVectorFile.ParseMaterial(var aLineIndex: Integer; aMaterial: TGLASEMaterial);
var
  Data: string;
begin
  Inc(aLineIndex);
  Data := FStringData[aLineIndex];
  while not IsEndOfSection(Data) do begin
    case GetTagOnData(Data) of
      ASCII_MATERIAL_NAME_I:          aMaterial.FName := GetStringValue(Data);
      ASCII_MATERIAL_AMBIENT_I:       aMaterial.FAmbient := GetValue3D(Data);
      ASCII_MATERIAL_DIFFUSE_I:       aMaterial.FDiffuse := GetValue3D(Data);
      ASCII_MATERIAL_SPECULAR_I:      aMaterial.FSpecular := GetValue3D(Data);
      ASCII_MATERIAL_SHINE_I:         aMaterial.FShiness := GetDoubleValue(Data);
      ASCII_MATERIAL_SHINESTRENGTH_I: aMaterial.FShineStrength := GetDoubleValue(Data);
      ASCII_MATERIAL_TRANSPARENCY_I:  aMaterial.FTransparency := GetDoubleValue(Data);
      ASCII_MATERIAL_WIRESIZE_I:      aMaterial.FWireSize := GetDoubleValue(Data);
      ASCII_SUBMATERIAL_I:  ParseSubMaterial(aLineIndex, aMaterial);
    end;
    Inc(aLineIndex);
    Data := FStringData[aLineIndex];
  end;
end;

procedure TGLASEVectorFile.ParseSubMaterial(var aLineIndex: Integer; aMaterial: TGLASEMaterial);
var
  Data: string;
  Index: Integer;
begin
  Index := aMaterial.FSubMaterials.Count;
  Inc(aMaterial.FSubMaterials.Count);

  Inc(aLineIndex);
  Data := FStringData[aLineIndex];
  while not IsEndOfSection(Data) do begin
    case GetTagOnData(Data) of
      ASCII_MATERIAL_NAME_I:          aMaterial.FSubMaterials.SubMaterial[Index].Name := GetStringValue(Data);
      ASCII_MATERIAL_AMBIENT_I:       aMaterial.FSubMaterials.SubMaterial[Index].Ambient := GetValue3D(Data);
      ASCII_MATERIAL_DIFFUSE_I:       aMaterial.FSubMaterials.SubMaterial[Index].Diffuse := GetValue3D(Data);
      ASCII_MATERIAL_SPECULAR_I:      aMaterial.FSubMaterials.SubMaterial[Index].Specular := GetValue3D(Data);
      ASCII_MATERIAL_SHINE_I:         aMaterial.FSubMaterials.SubMaterial[Index].Shiness := GetDoubleValue(Data);
      ASCII_MATERIAL_SHINESTRENGTH_I: aMaterial.FSubMaterials.SubMaterial[Index].ShineStrength := GetDoubleValue(Data);
      ASCII_MATERIAL_TRANSPARENCY_I:  aMaterial.FSubMaterials.SubMaterial[Index].Transparency := GetDoubleValue(Data);
      ASCII_MATERIAL_WIRESIZE_I:      aMaterial.FSubMaterials.SubMaterial[Index].WireSize := GetDoubleValue(Data);
      ASCII_MATERIAL_SELFILLUM_I:     aMaterial.FSubMaterials.SubMaterial[Index].SelfIllumination := GetDoubleValue(Data);
      ASCII_MAP_DIFFUSE_I: ParseDiffuseMap(aLineIndex, aMaterial, Index);
    end;
    Inc(aLineIndex);
    Data := FStringData[aLineIndex];
  end;
end;

procedure TGLASEVectorFile.ParseDiffuseMap(var aLineIndex: Integer; aMaterial: TGLASEMaterial; aSubMaterialIndex: Integer);
var
  Data: string;
begin
  Inc(aLineIndex);
  Data := FStringData[aLineIndex];
  while not IsEndOfSection(Data) do begin
    case GetTagOnData(Data) of
      ASCII_MAP_NAME_I:       aMaterial.FSubMaterials.SubMaterial[aSubMaterialIndex].DiffuseMap.Name := GetStringValue(Data);
      ASCII_MAP_CLASS_I:      aMaterial.FSubMaterials.SubMaterial[aSubMaterialIndex].DiffuseMap._Class := GetStringValue(Data);
      ASCII_MAP_SUBNO_I:      aMaterial.FSubMaterials.SubMaterial[aSubMaterialIndex].DiffuseMap.No := Round(GetDoubleValue(Data));
      ASCII_MAP_AMOUNT_I:     aMaterial.FSubMaterials.SubMaterial[aSubMaterialIndex].DiffuseMap.Amount := GetDoubleValue(Data);
      ASCII_BITMAP_I:         aMaterial.FSubMaterials.SubMaterial[aSubMaterialIndex].DiffuseMap.Bitmap := GetStringValue(Data);
      ASCII_UVW_U_OFFSET_I:   aMaterial.FSubMaterials.SubMaterial[aSubMaterialIndex].DiffuseMap.UOffset := GetDoubleValue(Data);
      ASCII_UVW_V_OFFSET_I:   aMaterial.FSubMaterials.SubMaterial[aSubMaterialIndex].DiffuseMap.VOffset := GetDoubleValue(Data);
      ASCII_UVW_U_TILING_I:   aMaterial.FSubMaterials.SubMaterial[aSubMaterialIndex].DiffuseMap.UTiling := GetDoubleValue(Data);
      ASCII_UVW_V_TILING_I:   aMaterial.FSubMaterials.SubMaterial[aSubMaterialIndex].DiffuseMap.VTiling := GetDoubleValue(Data);
      ASCII_UVW_ANGLE_I:      aMaterial.FSubMaterials.SubMaterial[aSubMaterialIndex].DiffuseMap.Angle := GetDoubleValue(Data);
      ASCII_UVW_BLUR_I:       aMaterial.FSubMaterials.SubMaterial[aSubMaterialIndex].DiffuseMap.Blur := GetDoubleValue(Data);
      ASCII_UVW_BLUR_OFFSET_I:aMaterial.FSubMaterials.SubMaterial[aSubMaterialIndex].DiffuseMap.BlurOffset := GetDoubleValue(Data);
      ASCII_UVW_NOUSE_AMT_I:  aMaterial.FSubMaterials.SubMaterial[aSubMaterialIndex].DiffuseMap.NouseAmount := GetDoubleValue(Data);
      ASCII_UVW_NOISE_SIZE_I: aMaterial.FSubMaterials.SubMaterial[aSubMaterialIndex].DiffuseMap.NoiseSize := GetDoubleValue(Data);
      ASCII_UVW_NOISE_LEVEL_I:aMaterial.FSubMaterials.SubMaterial[aSubMaterialIndex].DiffuseMap.NoiseLevel := Round(GetDoubleValue(Data));
      ASCII_UVW_NOISE_PHASE_I:aMaterial.FSubMaterials.SubMaterial[aSubMaterialIndex].DiffuseMap.NoisePhase := GetDoubleValue(Data);
    end;
    Inc(aLineIndex);
    Data := FStringData[aLineIndex];
  end;
end;

initialization
  RegisterVectorFileFormat('ase', 'ASCII files', TGLASEVectorFile);
end.
