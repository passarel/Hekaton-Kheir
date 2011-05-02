object FmChVis: TFmChVis
  Left = 175
  Top = 41
  Width = 986
  Height = 729
  Caption = 'FmChVis'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Chart1: TChart
    Left = 0
    Top = 0
    Width = 778
    Height = 675
    BackWall.Brush.Color = clWhite
    BackWall.Brush.Style = bsClear
    Title.Font.Color = clBlack
    Title.Text.Strings = (
      'TChart')
    BottomAxis.Axis.Width = 1
    LeftAxis.Axis.Width = 1
    View3D = False
    Align = alClient
    TabOrder = 0
    object Series1: TLineSeries
      Marks.Arrow.Visible = True
      Marks.Callout.Brush.Color = clBlack
      Marks.Callout.Arrow.Visible = True
      Marks.Visible = False
      Pointer.HorizSize = 2
      Pointer.InflateMargins = True
      Pointer.Style = psCircle
      Pointer.VertSize = 2
      Pointer.Visible = True
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
  end
  object Panel1: TPanel
    Left = 778
    Top = 0
    Width = 200
    Height = 675
    Align = alRight
    Caption = 'Panel1'
    TabOrder = 1
    object ScrollBox1: TScrollBox
      Left = 1
      Top = 185
      Width = 198
      Height = 327
      Align = alClient
      BevelInner = bvNone
      BevelOuter = bvNone
      TabOrder = 0
    end
    object Panel2: TPanel
      Left = 1
      Top = 1
      Width = 198
      Height = 184
      Align = alTop
      BevelOuter = bvNone
      BorderStyle = bsSingle
      TabOrder = 1
      object Label2: TLabel
        Left = 8
        Top = 8
        Width = 48
        Height = 13
        Caption = 'Chart Title'
      end
      object Button1: TButton
        Left = 104
        Top = 56
        Width = 73
        Height = 25
        Caption = 'Switch to B&W'
        TabOrder = 0
        OnClick = Button1Click
      end
      object Edit1: TEdit
        Left = 8
        Top = 24
        Width = 177
        Height = 21
        TabOrder = 1
        OnChange = Edit1Change
      end
      object Panel3: TPanel
        Left = 16
        Top = 56
        Width = 73
        Height = 25
        Caption = 'Chart &BG'
        TabOrder = 2
        OnClick = Panel3Click
      end
      object JvValidateEdit1: TJvValidateEdit
        Left = 32
        Top = 88
        Width = 57
        Height = 21
        CriticalPoints.MaxValueIncluded = False
        CriticalPoints.MinValueIncluded = False
        DisplayFormat = dfFloatGeneral
        DecimalPlaces = 4
        TabOrder = 3
        OnExit = JvValidateEdit1Exit
      end
      object JvValidateEdit2: TJvValidateEdit
        Left = 32
        Top = 120
        Width = 57
        Height = 21
        CriticalPoints.MaxValueIncluded = False
        CriticalPoints.MinValueIncluded = False
        DisplayFormat = dfFloatGeneral
        DecimalPlaces = 4
        TabOrder = 4
        OnExit = JvValidateEdit2Exit
      end
      object JvValidateEdit3: TJvValidateEdit
        Left = 120
        Top = 88
        Width = 57
        Height = 21
        CriticalPoints.MaxValueIncluded = False
        CriticalPoints.MinValueIncluded = False
        DisplayFormat = dfFloatGeneral
        DecimalPlaces = 4
        TabOrder = 5
        OnExit = JvValidateEdit3Exit
      end
      object JvValidateEdit4: TJvValidateEdit
        Left = 120
        Top = 120
        Width = 57
        Height = 21
        CriticalPoints.MaxValueIncluded = False
        CriticalPoints.MinValueIncluded = False
        DisplayFormat = dfFloatGeneral
        DecimalPlaces = 4
        TabOrder = 6
        OnExit = JvValidateEdit4Exit
      end
      object CheckBox2: TCheckBox
        Left = 16
        Top = 152
        Width = 81
        Height = 17
        Caption = 'Logarithmic'
        TabOrder = 7
        OnClick = CheckBox2Click
      end
      object CheckBox3: TCheckBox
        Left = 104
        Top = 152
        Width = 97
        Height = 17
        Caption = 'Logarithmic'
        TabOrder = 8
        OnClick = CheckBox3Click
      end
    end
    object ScrollBox2: TScrollBox
      Left = 1
      Top = 512
      Width = 198
      Height = 162
      Align = alBottom
      BevelInner = bvNone
      BevelOuter = bvNone
      TabOrder = 2
      object StringGrid1: TStringGrid
        Left = 0
        Top = 0
        Width = 193
        Height = 129
        ColCount = 2
        DefaultColWidth = 50
        DefaultRowHeight = 18
        RowCount = 3
        TabOrder = 0
      end
      object CheckBox1: TCheckBox
        Left = 56
        Top = 136
        Width = 97
        Height = 17
        Caption = 'Square Matrix'
        TabOrder = 1
        OnClick = CheckBox1Click
      end
    end
  end
  object MainMenu1: TMainMenu
    Left = 728
    Top = 16
    object Chart2: TMenuItem
      Caption = '&Chart'
      object SaveasJPEG1: TMenuItem
        Caption = 'Save as &JPEG'
        OnClick = SaveasJPEG1Click
      end
      object SaveasWMV1: TMenuItem
        Caption = 'Save as &WMF'
        OnClick = SaveasWMV1Click
      end
      object SaveasEMF1: TMenuItem
        Caption = 'Save as &EMF'
        OnClick = SaveasEMF1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Close1: TMenuItem
        Caption = '&Close'
        OnClick = Close1Click
      end
    end
    object ools1: TMenuItem
      Caption = '&Tools'
      object ShowRightBar1: TMenuItem
        Caption = '&Show Right Bar'
        OnClick = ShowRightBar1Click
      end
    end
  end
  object ColorDialog1: TColorDialog
    Left = 728
    Top = 48
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = '.wmf'
    Filter = 'Windows Metafile|*.wmf'
    Left = 728
    Top = 80
  end
  object SaveDialog2: TSaveDialog
    DefaultExt = '.jpg'
    Filter = 'JPEG Image|*.jpg'
    Left = 728
    Top = 112
  end
  object SaveDialog3: TSaveDialog
    DefaultExt = '.emf'
    Filter = 'Enhanced Metafiles|*.emf'
    Left = 728
    Top = 144
  end
end
