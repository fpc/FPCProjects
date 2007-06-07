object Form1: TForm1
  Left = 192
  Top = 114
  Width = 431
  Height = 430
  Caption = 'Cg Cell Shading Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 423
    Height = 396
    Camera = GLCamera1
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
  end
  object GLScene1: TGLScene
    Left = 8
    Top = 8
    object GLDummyCube1: TGLDummyCube
      CubeSize = 1.000000000000000000
      object GLCamera1: TGLCamera
        DepthOfView = 100.000000000000000000
        FocalLength = 50.000000000000000000
        TargetObject = GLDummyCube1
        Position.Coordinates = {00000000000000400000A0400000803F}
        Direction.Coordinates = {00000000000000000000803F00000000}
        Up.Coordinates = {000000000000803F0000008000000000}
        object GLLightSource1: TGLLightSource
          ConstAttenuation = 1.000000000000000000
          SpotCutOff = 180.000000000000000000
        end
      end
    end
    object GLActor1: TGLActor
      Direction.Coordinates = {000000000000803F2EBD3BB300000000}
      PitchAngle = 90.000000000000000000
      Up.Coordinates = {000000002EBD3BB3000080BF00000000}
      Material.MaterialLibrary = GLMaterialLibrary1
      Material.LibMaterialName = 'CgShaderMat'
      Interval = 100
    end
  end
  object CgCellShader: TCgShader
    VertexProgram.OnApply = CgCellShaderApplyVP
    FragmentProgram.OnApply = CgCellShaderApplyFP
    FragmentProgram.OnUnApply = CgCellShaderUnApplyFP
    OnApplyVP = CgCellShaderApplyVP
    OnApplyFP = CgCellShaderApplyFP
    OnUnApplyFP = CgCellShaderUnApplyFP
    OnInitialize = CgCellShaderInitialize
    Left = 40
    Top = 40
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <
      item
        Name = 'CgShaderMat'
        Material.Texture.Disabled = False
        Tag = 0
        Shader = CgCellShader
      end>
    Left = 40
    Top = 8
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    Left = 8
    Top = 40
  end
  object AsyncTimer1: TAsyncTimer
    Enabled = True
    OnTimer = AsyncTimer1Timer
    Left = 8
    Top = 72
  end
end
