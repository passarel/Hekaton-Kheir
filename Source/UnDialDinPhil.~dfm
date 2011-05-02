object FmPhil: TFmPhil
  Left = 134
  Top = 16
  Width = 1025
  Height = 482
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
  object Chart1: TChart
    Left = 8
    Top = 424
    Width = 985
    Height = 233
    BackWall.Brush.Color = clWhite
    BackWall.Brush.Style = bsClear
    Title.Text.Strings = (
      'RMSE x time')
    Title.Visible = False
    BottomAxis.ExactDateTime = False
    BottomAxis.Increment = 50.000000000000000000
    Chart3DPercent = 30
    LeftAxis.ExactDateTime = False
    LeftAxis.Increment = 0.100000000000000000
    View3D = False
    View3DOptions.Perspective = 17
    Color = clWhite
    TabOrder = 0
    Visible = False
    PrintMargins = (
      15
      38
      15
      38)
  end
  object Chart2: TChart
    Left = 152
    Top = 120
    Width = 433
    Height = 241
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
    TabOrder = 2
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
    Left = 696
    Top = 168
    Width = 113
    Height = 137
    Caption = 'BURRO!!!!'
    TabOrder = 3
    OnClick = Button7Click
  end
  object Button9: TButton
    Left = 752
    Top = 368
    Width = 75
    Height = 25
    Caption = 'Corel'
    TabOrder = 4
    OnClick = Button9Click
  end
  object Button10: TButton
    Left = 712
    Top = 312
    Width = 75
    Height = 25
    Caption = 'Button10'
    TabOrder = 5
    OnClick = Button10Click
  end
  object GroupBox1: TGroupBox
    Left = 0
    Top = 5
    Width = 1001
    Height = 412
    Caption = 'GroupBox1'
    TabOrder = 1
    object Gauge1: TGauge
      Left = 600
      Top = 336
      Width = 161
      Height = 25
      Progress = 0
      Visible = False
    end
    object Label1: TLabel
      Left = 48
      Top = 304
      Width = 134
      Height = 13
      Caption = 'Probability (fork to left agent)'
    end
    object Label2: TLabel
      Left = 80
      Top = 352
      Width = 76
      Height = 13
      Caption = 'Allocation policy'
    end
    object Label7: TLabel
      Left = 352
      Top = 24
      Width = 84
      Height = 13
      Caption = 'Number of agents'
    end
    object Label4: TLabel
      Left = 608
      Top = 136
      Width = 98
      Height = 13
      Caption = 'Learning experiment:'
    end
    object Label10: TLabel
      Left = 608
      Top = 192
      Width = 102
      Height = 13
      Caption = 'Number of time steps:'
    end
    object Label11: TLabel
      Left = 608
      Top = 80
      Width = 96
      Height = 13
      Caption = 'Reference Network:'
    end
    object SpinEdit1: TSpinEdit
      Left = 360
      Top = 40
      Width = 121
      Height = 22
      MaxValue = 15
      MinValue = 0
      TabOrder = 0
      Value = 0
      OnChange = SpinEdit1Change
    end
    object Button4: TButton
      Left = 640
      Top = 16
      Width = 75
      Height = 25
      Caption = 'Create'
      TabOrder = 1
      OnClick = Button4Click
    end
    object Button5: TButton
      Left = 640
      Top = 48
      Width = 75
      Height = 25
      Caption = 'Free'
      Enabled = False
      TabOrder = 2
      OnClick = Button5Click
    end
    object Button1: TButton
      Left = 640
      Top = 264
      Width = 75
      Height = 25
      Caption = 'Run'
      Enabled = False
      TabOrder = 3
      OnClick = Button1Click
    end
    object Button3: TButton
      Left = 640
      Top = 296
      Width = 75
      Height = 25
      Caption = 'Cancel'
      Enabled = False
      TabOrder = 4
      OnClick = Button3Click
    end
    object GroupBox3: TGroupBox
      Left = 768
      Top = 16
      Width = 225
      Height = 345
      Caption = 'Charts'
      TabOrder = 5
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
      Top = 16
      Width = 217
      Height = 249
      ItemHeight = 13
      PopupMenu = PopupMenu1
      TabOrder = 6
    end
    object Button2: TButton
      Left = 72
      Top = 272
      Width = 75
      Height = 25
      Caption = 'Add...'
      TabOrder = 7
      OnClick = Button2Click
    end
    object SpinEdit4: TSpinEdit
      Left = 56
      Top = 320
      Width = 121
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 8
      Value = 0
    end
    object ComboBox1: TComboBox
      Left = 48
      Top = 368
      Width = 145
      Height = 21
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 9
      Text = 'Give fork whenever available'
      Items.Strings = (
        'Give fork whenever available'
        'Avoid deadlocks'
        'Give at most one fork each turn')
    end
    object GroupBox4: TGroupBox
      Left = 240
      Top = 72
      Width = 353
      Height = 329
      Caption = 'Properties'
      TabOrder = 10
      object Label3: TLabel
        Left = 8
        Top = 16
        Width = 31
        Height = 13
        Caption = 'Agent:'
      end
      object Label8: TLabel
        Left = 176
        Top = 16
        Width = 43
        Height = 13
        Caption = 'Network:'
      end
      object Label9: TLabel
        Left = 160
        Top = 272
        Width = 76
        Height = 13
        Caption = 'Current Property'
      end
      object ComboBox2: TComboBox
        Left = 16
        Top = 32
        Width = 145
        Height = 21
        ItemHeight = 13
        TabOrder = 0
        Text = 'ComboBox2'
        OnChange = ComboBox2Change
      end
      object StringGrid1: TStringGrid
        Left = 8
        Top = 72
        Width = 329
        Height = 185
        DefaultColWidth = 36
        DefaultRowHeight = 18
        Enabled = False
        RowCount = 1
        FixedRows = 0
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
        TabOrder = 1
      end
      object ComboBox3: TComboBox
        Left = 184
        Top = 32
        Width = 145
        Height = 21
        ItemHeight = 13
        TabOrder = 2
        Text = 'ComboBox2'
        OnChange = ComboBox3Change
      end
      object ComboBox4: TComboBox
        Left = 168
        Top = 288
        Width = 145
        Height = 21
        Enabled = False
        ItemHeight = 13
        TabOrder = 3
        Text = 'ComboBox2'
        OnChange = ComboBox4Change
        OnEnter = ComboBox4Enter
      end
      object Button11: TButton
        Left = 32
        Top = 288
        Width = 75
        Height = 25
        Caption = 'Add...'
        Enabled = False
        TabOrder = 4
        OnClick = Button11Click
      end
    end
    object ComboBox5: TComboBox
      Left = 608
      Top = 152
      Width = 145
      Height = 21
      ItemHeight = 13
      ItemIndex = 4
      TabOrder = 11
      Text = 'Script'
      Items.Strings = (
        'Black Box (Offline)'
        'Black Box (Online)'
        'Agent'#39's properties'
        'Overall properties'
        'Script')
    end
    object SpinEdit5: TSpinEdit
      Left = 608
      Top = 208
      Width = 145
      Height = 22
      Increment = 100
      MaxValue = 1000000
      MinValue = 0
      TabOrder = 12
      Value = 0
    end
    object ComboBox6: TComboBox
      Left = 608
      Top = 96
      Width = 145
      Height = 21
      ItemHeight = 13
      TabOrder = 13
    end
    object Button12: TButton
      Left = 640
      Top = 376
      Width = 89
      Height = 25
      Caption = 'Show Charts...'
      TabOrder = 14
      OnClick = Button12Click
    end
    object ComboBox7: TComboBox
      Left = 808
      Top = 368
      Width = 145
      Height = 21
      ItemHeight = 13
      TabOrder = 15
      Text = 'Learning type 1: Offline'
      Items.Strings = (
        'Learning type 1: Offline'
        'Learning type 2: Online'
        'Learning type 3: Restricted (1 + 1)'
        'Learning type 3: Restricted (1)'
        'Learning type 5: Restricted (2)'
        'Learning type 6: Free (1 + 1)'
        'Learning type 3: Free (1)'
        'Learning type 5: Free (2)'
        '')
    end
  end
  object Panel1: TPanel
    Left = 8
    Top = 656
    Width = 985
    Height = 33
    Caption = 'Panel1'
    TabOrder = 6
    Visible = False
  end
  object MainMenu1: TMainMenu
    Left = 192
    Top = 96
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
    Left = 192
    Top = 72
  end
  object OpenDialog2: TOpenDialog
    Left = 192
    Top = 24
  end
  object PopupMenu1: TPopupMenu
    Left = 192
    Top = 48
    object Delete1: TMenuItem
      Caption = 'Delete'
      OnClick = Delete1Click
    end
  end
  object SaveDialog1: TSaveDialog
    Left = 192
    Top = 136
  end
end
