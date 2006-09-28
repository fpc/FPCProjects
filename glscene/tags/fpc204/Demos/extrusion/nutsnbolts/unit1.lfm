object Form1: TForm1
  Left = 162
  Top = 77
  Width = 463
  Height = 332
  AutoSize = True
  BorderWidth = 3
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 449
    Height = 297
    Camera = GLCamera1
    Buffer.BackgroundColor = clNavy
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
  end
  object GLScene1: TGLScene
    Left = 8
    Top = 8
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1
      Position.Coordinates = {0000484200004842000048420000803F}
      SpotCutOff = 180
    end
    object DummyCube1: TGLDummyCube
      CubeSize = 1
      object Bolt: TGLDummyCube
        Position.Coordinates = {000000009A9999BE0000803F0000803F}
        CubeSize = 1
        object RSBoltHead: TGLRevolutionSolid
          Position.Coordinates = {000000001F856B3F000000000000803F}
          Scale.Coordinates = {9A99193E9A99193E9A99193E00000000}
          Nodes = <
            item
              Y = 1.5
            end
            item
              X = 5.90000009536743
              Y = 1.5
            end
            item
              X = 6
              Y = 1.39999997615814
            end
            item
              X = 6
              Y = -1.39999997615814
            end
            item
              X = 5.90000009536743
              Y = -1.5
            end
            item
              Y = -1.5
            end>
          Parts = [rspOutside, rspStartPolygon]
          Slices = 6
          Normals = nsSmooth
        end
        object CYBoltShaft: TGLCylinder
          BottomRadius = 0.5
          Height = 1.54999995231628
          TopRadius = 0.5
        end
        object RSBoltThreads: TGLRevolutionSolid
          Scale.Coordinates = {CDCCCC3DCDCCCC3DCDCCCC3D00000000}
          Nodes = <
            item
              X = 5
              Y = 0.5
            end
            item
              X = 5.5
            end
            item
              X = 5
              Y = -0.5
            end>
          StartAngle = -1800
          StopAngle = 1800
          YOffsetPerTurn = 1.5
          Slices = 24
        end
      end
      object Nut: TGLDummyCube
        Direction.Coordinates = {F304353FF30435BF33BF0D2800000000}
        Position.Coordinates = {0000000000000000000080BF0000803F}
        Up.Coordinates = {F404353FF304353F64C084B300000000}
        CubeSize = 1
        object RSNutThreads: TGLRevolutionSolid
          Scale.Coordinates = {CDCCCC3DCDCCCC3DCDCCCC3D00000000}
          Nodes = <
            item
              X = 5.5
              Y = 0.5
            end
            item
              X = 5
            end
            item
              X = 5.5
              Y = -0.5
            end>
          Parts = [rspInside, rspStartPolygon, rspStopPolygon]
          StartAngle = -475
          StopAngle = 460
          YOffsetPerTurn = 1.45000004768372
          Slices = 24
        end
        object RSNutPans: TGLRevolutionSolid
          Scale.Coordinates = {9A99193E9A99193E9A99193E00000000}
          Nodes = <
            item
              X = 4.5
              Y = 1.5
            end
            item
              X = 5.90000009536743
              Y = 1.5
            end
            item
              X = 6
              Y = 1.39999997615814
            end
            item
              X = 6
              Y = -1.39999997615814
            end
            item
              X = 5.90000009536743
              Y = -1.5
            end
            item
              X = 4.5
              Y = -1.5
            end>
          Slices = 6
        end
        object Annulus1: TGLAnnulus
          BottomRadius = 0.699999988079071
          Height = 0.449999988079071
          BottomInnerRadius = 0.550000011920929
          TopInnerRadius = 0.550000011920929
          TopRadius = 0.699999988079071
          Parts = [anInnerSides, anBottom, anTop]
        end
      end
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100
      FocalLength = 50
      TargetObject = DummyCube1
      Position.Coordinates = {0000804000000000000000000000803F}
      Left = 208
      Top = 136
    end
  end
end
