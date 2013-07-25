object FmFunct: TFmFunct
  Left = 52
  Top = 108
  Width = 1094
  Height = 677
  Caption = 'Function Analyser'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 856
    Top = 24
    Width = 44
    Height = 13
    Caption = 'Function:'
  end
  object Chart1: TChart
    Left = 0
    Top = 0
    Width = 833
    Height = 641
    BackWall.Brush.Color = clWhite
    BackWall.Brush.Style = bsClear
    Legend.Visible = False
    Title.Text.Strings = (
      'TChart')
    Chart3DPercent = 5
    LeftAxis.Automatic = False
    LeftAxis.AutomaticMaximum = False
    LeftAxis.AutomaticMinimum = False
    LeftAxis.Maximum = 1.000000000000000000
    LeftAxis.Minimum = -1.000000000000000000
    View3D = False
    TabOrder = 0
    object Series1: TLineSeries
      Marks.Arrow.Visible = True
      Marks.Callout.Brush.Color = clBlack
      Marks.Callout.Arrow.Visible = True
      Marks.Visible = False
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      Pointer.Visible = False
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
    object Series2: TLineSeries
      Marks.Arrow.Visible = True
      Marks.Callout.Brush.Color = clBlack
      Marks.Callout.Arrow.Visible = True
      Marks.Visible = False
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      Pointer.Visible = False
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
  end
  object ComboBox1: TComboBox
    Left = 864
    Top = 40
    Width = 185
    Height = 21
    ItemHeight = 13
    TabOrder = 1
    Text = 'Logsig '
    OnChange = ComboBox1Change
    Items.Strings = (
      'Logsig '
      'Tansig '
      'BiLogsig'
      'Threshold'
      'BiThreshold '
      'Linear'
      'Polynomial A'
      'Polynomial B'
      'Trigonometric A')
  end
  object GroupBox1: TGroupBox
    Left = 840
    Top = 80
    Width = 241
    Height = 65
    Caption = 'Parameter 1'
    TabOrder = 2
    object Label2: TLabel
      Left = 8
      Top = 16
      Width = 17
      Height = 13
      Caption = 'Min'
    end
    object Label3: TLabel
      Left = 192
      Top = 16
      Width = 17
      Height = 13
      Caption = 'Min'
    end
    object SpinEdit1: TSpinEdit
      Left = 8
      Top = 35
      Width = 41
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 0
      Value = 0
    end
    object SpinEdit2: TSpinEdit
      Left = 192
      Top = 35
      Width = 41
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 1
      Value = 0
    end
    object TrackBar1: TTrackBar
      Left = 48
      Top = 28
      Width = 145
      Height = 29
      Max = 100
      Position = 50
      TabOrder = 2
      OnChange = SpinEdit1Change
    end
  end
  object GroupBox2: TGroupBox
    Left = 840
    Top = 152
    Width = 241
    Height = 65
    Caption = 'Parameter 2'
    TabOrder = 3
    object Label4: TLabel
      Left = 8
      Top = 16
      Width = 17
      Height = 13
      Caption = 'Min'
    end
    object Label5: TLabel
      Left = 192
      Top = 16
      Width = 17
      Height = 13
      Caption = 'Min'
    end
    object SpinEdit3: TSpinEdit
      Left = 8
      Top = 35
      Width = 41
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 0
      Value = 0
    end
    object SpinEdit4: TSpinEdit
      Left = 192
      Top = 35
      Width = 41
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 1
      Value = 0
    end
    object TrackBar2: TTrackBar
      Left = 48
      Top = 28
      Width = 145
      Height = 29
      Max = 100
      Position = 50
      TabOrder = 2
      OnChange = SpinEdit1Change
    end
  end
  object GroupBox3: TGroupBox
    Left = 840
    Top = 224
    Width = 241
    Height = 65
    Caption = 'Parameter 3'
    TabOrder = 4
    object Label6: TLabel
      Left = 8
      Top = 16
      Width = 17
      Height = 13
      Caption = 'Min'
    end
    object Label7: TLabel
      Left = 192
      Top = 16
      Width = 17
      Height = 13
      Caption = 'Min'
    end
    object SpinEdit5: TSpinEdit
      Left = 8
      Top = 35
      Width = 41
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 0
      Value = 0
    end
    object SpinEdit6: TSpinEdit
      Left = 192
      Top = 35
      Width = 41
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 1
      Value = 0
    end
    object TrackBar3: TTrackBar
      Left = 48
      Top = 28
      Width = 145
      Height = 29
      Max = 100
      Position = 50
      TabOrder = 2
      OnChange = SpinEdit1Change
    end
  end
  object RadioGroup1: TRadioGroup
    Left = 848
    Top = 304
    Width = 225
    Height = 105
    Caption = 'Series'
    ItemIndex = 0
    Items.Strings = (
      'Only Function'
      'Function and Identity'
      'Merged function')
    TabOrder = 5
    OnClick = RadioGroup1Click
  end
  object CheckBox1: TCheckBox
    Left = 912
    Top = 424
    Width = 97
    Height = 17
    Caption = 'Derivated'
    TabOrder = 6
    OnClick = CheckBox1Click
  end
  object GroupBox4: TGroupBox
    Left = 840
    Top = 480
    Width = 241
    Height = 129
    Caption = 'Chart Control'
    TabOrder = 7
    object Label8: TLabel
      Left = 40
      Top = 56
      Width = 27
      Height = 13
      Caption = 'Zoom'
    end
    object Label9: TLabel
      Left = 160
      Top = 56
      Width = 19
      Height = 13
      Caption = 'Pan'
    end
    object UpDown1: TUpDown
      Left = 8
      Top = 32
      Width = 16
      Height = 64
      Min = -100
      TabOrder = 0
      OnClick = UpDown1Click
    end
    object UpDown2: TUpDown
      Left = 24
      Top = 96
      Width = 64
      Height = 16
      Min = -100
      Orientation = udHorizontal
      TabOrder = 1
    end
    object UpDown3: TUpDown
      Left = 120
      Top = 32
      Width = 16
      Height = 64
      Min = -100
      TabOrder = 2
      OnClick = UpDown1Click
    end
    object UpDown4: TUpDown
      Left = 136
      Top = 96
      Width = 64
      Height = 16
      Min = -100
      Orientation = udHorizontal
      TabOrder = 3
    end
  end
end
