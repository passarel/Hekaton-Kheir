object FmOldPhil: TFmOldPhil
  Left = 166
  Top = 67
  Width = 804
  Height = 481
  Caption = 'Philosophers'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label4: TLabel
    Left = 8
    Top = 8
    Width = 21
    Height = 13
    Caption = 'Files'
  end
  object Label5: TLabel
    Left = 280
    Top = 8
    Width = 3
    Height = 13
  end
  object Label6: TLabel
    Left = 136
    Top = 8
    Width = 3
    Height = 13
  end
  object GroupBox1: TGroupBox
    Left = 0
    Top = 5
    Width = 779
    Height = 412
    Caption = 'GroupBox1'
    TabOrder = 1
    object Gauge1: TGauge
      Left = 168
      Top = 384
      Width = 209
      Height = 25
      Progress = 0
    end
    object GroupBox2: TGroupBox
      Left = 240
      Top = 48
      Width = 185
      Height = 313
      Caption = 'Agents'
      TabOrder = 0
    end
    object SpinEdit1: TSpinEdit
      Left = 288
      Top = 16
      Width = 121
      Height = 22
      MaxValue = 15
      MinValue = 2
      TabOrder = 1
      Value = 3
      OnChange = SpinEdit1Change
    end
    object Button4: TButton
      Left = 440
      Top = 16
      Width = 75
      Height = 25
      Caption = 'Create'
      TabOrder = 2
      OnClick = Button4Click
    end
    object Button5: TButton
      Left = 440
      Top = 48
      Width = 75
      Height = 25
      Caption = 'Free'
      TabOrder = 3
      OnClick = Button5Click
    end
    object Button1: TButton
      Left = 440
      Top = 136
      Width = 75
      Height = 25
      Caption = 'Execute'
      TabOrder = 4
      OnClick = Button1Click
    end
    object Button3: TButton
      Left = 440
      Top = 168
      Width = 75
      Height = 25
      Caption = 'Cancel'
      TabOrder = 5
      OnClick = Button3Click
    end
    object CheckBox5: TCheckBox
      Left = 440
      Top = 96
      Width = 97
      Height = 17
      Caption = 'Online'
      TabOrder = 6
    end
    object GroupBox3: TGroupBox
      Left = 528
      Top = 40
      Width = 225
      Height = 329
      Caption = 'Charts'
      TabOrder = 7
      object CheckBox1: TCheckBox
        Left = 16
        Top = 24
        Width = 97
        Height = 17
        Caption = 'Mean of agents'
        TabOrder = 0
      end
      object CheckBox2: TCheckBox
        Left = 16
        Top = 48
        Width = 137
        Height = 17
        Caption = 'Use neighborhood'
        TabOrder = 1
      end
      object SpinEdit2: TSpinEdit
        Left = 88
        Top = 64
        Width = 81
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 2
        Value = 0
      end
      object CheckBox3: TCheckBox
        Left = 16
        Top = 96
        Width = 137
        Height = 17
        Caption = 'Use disjunct intervals'
        TabOrder = 3
      end
      object SpinEdit3: TSpinEdit
        Left = 88
        Top = 120
        Width = 81
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 4
        Value = 0
      end
      object CheckBox4: TCheckBox
        Left = 16
        Top = 152
        Width = 137
        Height = 17
        Caption = 'Use individual points'
        Checked = True
        State = cbChecked
        TabOrder = 5
      end
      object RadioButton1: TRadioButton
        Left = 16
        Top = 208
        Width = 113
        Height = 17
        Caption = 'Error'
        Checked = True
        TabOrder = 6
        TabStop = True
      end
      object RadioButton2: TRadioButton
        Left = 16
        Top = 232
        Width = 113
        Height = 17
        Caption = 'Delay'
        TabOrder = 7
      end
      object Button6: TButton
        Left = 40
        Top = 296
        Width = 67
        Height = 25
        Caption = 'Plot'
        TabOrder = 8
        OnClick = Button6Click
      end
      object Button8: TButton
        Left = 120
        Top = 296
        Width = 65
        Height = 25
        Caption = 'Show'
        TabOrder = 9
        OnClick = Button8Click
      end
      object RadioButton3: TRadioButton
        Left = 16
        Top = 256
        Width = 113
        Height = 17
        Caption = 'Allocation'
        TabOrder = 10
      end
    end
    object ListBox1: TListBox
      Left = 8
      Top = 24
      Width = 217
      Height = 353
      ItemHeight = 13
      PopupMenu = PopupMenu1
      TabOrder = 8
    end
    object Button2: TButton
      Left = 48
      Top = 384
      Width = 75
      Height = 25
      Caption = 'Add...'
      TabOrder = 9
      OnClick = Button2Click
    end
    object Button11: TButton
      Left = 440
      Top = 248
      Width = 75
      Height = 25
      Caption = 'Button11'
      TabOrder = 10
      OnClick = Button11Click
    end
    object Chart3: TChart
      Left = 24
      Top = 48
      Width = 433
      Height = 289
      BackWall.Brush.Color = clWhite
      BackWall.Brush.Style = bsClear
      Title.Text.Strings = (
        'RMSE x time')
      BottomAxis.ExactDateTime = False
      BottomAxis.Increment = 50.000000000000000000
      Chart3DPercent = 30
      LeftAxis.ExactDateTime = False
      LeftAxis.Increment = 0.100000000000000000
      View3D = False
      View3DOptions.Perspective = 17
      Color = clWhite
      TabOrder = 11
      Visible = False
      object LineSeries2: TLineSeries
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
        object AverageTeeFunction2: TAverageTeeFunction
        end
      end
    end
  end
  object Chart1: TChart
    Left = 280
    Top = 56
    Width = 433
    Height = 289
    BackWall.Brush.Color = clWhite
    BackWall.Brush.Style = bsClear
    Legend.Title.Text.Strings = (
      'Approaches')
    Title.Text.Strings = (
      'Performance of the training approaches')
    BottomAxis.ExactDateTime = False
    BottomAxis.Increment = 50.000000000000000000
    Chart3DPercent = 30
    LeftAxis.ExactDateTime = False
    LeftAxis.Increment = 0.100000000000000000
    LeftAxis.Title.Caption = 'Accuracy (%)'
    View3D = False
    View3DOptions.Perspective = 17
    Color = clWhite
    TabOrder = 0
    Visible = False
    object Series1: TBarSeries
      Marks.Arrow.Visible = True
      Marks.Callout.Brush.Color = clBlack
      Marks.Callout.Arrow.Visible = True
      Marks.Style = smsValue
      Marks.Visible = True
      Gradient.Direction = gdTopBottom
      Shadow.Color = 8487297
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Bar'
      YValues.Order = loNone
      Data = {
        06040000000000000000004D4080808000022331000000000040534055555500
        02233200000000000059403C3C3C000223330000000000005940000000000223
        34}
    end
  end
  object GroupBox4: TGroupBox
    Left = 32
    Top = 424
    Width = 593
    Height = 265
    Caption = 'GroupBox4'
    TabOrder = 2
    Visible = False
    object Chart2: TChart
      Left = 16
      Top = 16
      Width = 433
      Height = 289
      BackWall.Brush.Color = clWhite
      BackWall.Brush.Style = bsClear
      LeftWall.Color = clWhite
      Title.Font.Color = clBlack
      Title.Text.Strings = (
        'RMSE x time')
      BottomAxis.ExactDateTime = False
      BottomAxis.Increment = 50.000000000000000000
      Chart3DPercent = 30
      LeftAxis.Automatic = False
      LeftAxis.AutomaticMaximum = False
      LeftAxis.AutomaticMinimum = False
      LeftAxis.ExactDateTime = False
      LeftAxis.Increment = 0.100000000000000000
      LeftAxis.Maximum = 0.500000000000000000
      View3D = False
      View3DOptions.Perspective = 17
      Color = clWhite
      TabOrder = 0
      object LineSeries1: TLineSeries
        Marks.Arrow.Visible = True
        Marks.Callout.Brush.Color = clBlack
        Marks.Callout.Arrow.Visible = True
        Marks.Visible = False
        SeriesColor = clSilver
        Title = 'NK'
        Pointer.InflateMargins = True
        Pointer.Style = psRectangle
        Pointer.Visible = False
        XValues.Name = 'X'
        XValues.Order = loAscending
        YValues.Name = 'Y'
        YValues.Order = loNone
        object AverageTeeFunction1: TAverageTeeFunction
        end
      end
      object Series3: TLineSeries
        Marks.Arrow.Visible = True
        Marks.Callout.Brush.Color = clBlack
        Marks.Callout.Arrow.Visible = True
        Marks.Visible = False
        SeriesColor = clGray
        Title = 'PK'
        Pointer.InflateMargins = True
        Pointer.Style = psRectangle
        Pointer.Visible = False
        XValues.Name = 'X'
        XValues.Order = loAscending
        YValues.Name = 'Y'
        YValues.Order = loNone
      end
      object Series4: TLineSeries
        Marks.Arrow.Visible = True
        Marks.Callout.Brush.Color = clBlack
        Marks.Callout.Arrow.Visible = True
        Marks.Visible = False
        SeriesColor = clBlack
        Title = 'FK'
        Pointer.InflateMargins = True
        Pointer.Style = psRectangle
        Pointer.Visible = False
        XValues.Name = 'X'
        XValues.Order = loAscending
        YValues.Name = 'Y'
        YValues.Order = loNone
      end
    end
    object Button7: TButton
      Left = 480
      Top = 56
      Width = 113
      Height = 137
      Caption = 'BURRO!!!!'
      TabOrder = 1
      OnClick = Button7Click
    end
    object Button10: TButton
      Left = 518
      Top = 240
      Width = 75
      Height = 25
      Caption = 'Button10'
      TabOrder = 2
      OnClick = Button10Click
    end
    object Button9: TButton
      Left = 494
      Top = 24
      Width = 75
      Height = 25
      Caption = 'Corel'
      TabOrder = 3
      OnClick = Button9Click
    end
  end
  object MainMenu1: TMainMenu
    Left = 168
    Top = 272
    object Configurar1: TMenuItem
      Caption = 'Back'
      OnClick = Configurar1Click
    end
    object Corel1: TMenuItem
      Caption = 'Corel'
      OnClick = Corel1Click
    end
    object Series2: TMenuItem
      Caption = 'Series'
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 176
    Top = 336
  end
  object OpenDialog2: TOpenDialog
    Left = 144
    Top = 344
  end
  object PopupMenu1: TPopupMenu
    Left = 176
    Top = 208
    object Delete1: TMenuItem
      Caption = 'Delete'
      OnClick = Delete1Click
    end
  end
end
