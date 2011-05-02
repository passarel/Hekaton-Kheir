object FmChConf: TFmChConf
  Left = 200
  Top = 163
  Width = 719
  Height = 534
  Caption = 'FmChConf'
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
  object GroupBox1: TGroupBox
    Left = 8
    Top = 0
    Width = 145
    Height = 329
    Caption = 'nn Subgroups in the file'
    TabOrder = 0
    object ScrollBox1: TScrollBox
      Left = 2
      Top = 15
      Width = 141
      Height = 312
      Align = alClient
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = bsNone
      TabOrder = 0
      object CheckBox1: TCheckBox
        Left = 8
        Top = 8
        Width = 97
        Height = 17
        Caption = 'CheckBox1'
        TabOrder = 0
      end
      object CheckBox2: TCheckBox
        Left = 8
        Top = 32
        Width = 97
        Height = 17
        Caption = 'CheckBox2'
        TabOrder = 1
      end
    end
  end
  object GroupBox2: TGroupBox
    Left = 160
    Top = 0
    Width = 537
    Height = 329
    Caption = 'mm Columns X ll Groups'
    TabOrder = 1
    object Label4: TLabel
      Left = 216
      Top = 302
      Width = 130
      Height = 13
      Caption = 'Default Grouping Schemes:'
    end
    object Button1: TButton
      Left = 78
      Top = 296
      Width = 75
      Height = 25
      Caption = 'Add Group'
      TabOrder = 0
      OnClick = Button1Click
    end
    object ComboBox3: TComboBox
      Left = 360
      Top = 298
      Width = 145
      Height = 21
      ItemHeight = 13
      TabOrder = 1
      Text = 'Custom'
      OnChange = ComboBox3Change
      Items.Strings = (
        'Custom'
        'Individual columns'
        'Differences by pair'
        'Confusion by pair ( 0, 1)'
        'Confusion by pair (-1, 1)'
        'First confusion ( 0, 1)'
        'First confusion (-1, 1)')
    end
    object ScrollBar1: TScrollBar
      Left = 240
      Top = 272
      Width = 273
      Height = 17
      LargeChange = 10
      PageSize = 0
      SmallChange = 2
      TabOrder = 2
      TabStop = False
      OnChange = ScrollBar1Change
    end
    object ScrollBar2: TScrollBar
      Left = 512
      Top = 32
      Width = 17
      Height = 241
      Kind = sbVertical
      LargeChange = 10
      PageSize = 0
      SmallChange = 2
      TabOrder = 3
      TabStop = False
      OnChange = ScrollBar2Change
    end
    object ScrewBack3: TPanel
      Left = 8
      Top = 32
      Width = 233
      Height = 241
      BevelOuter = bvNone
      Color = clOlive
      TabOrder = 4
      object ScrewBox3: TPanel
        Left = 0
        Top = 0
        Width = 233
        Height = 225
        BevelOuter = bvNone
        TabOrder = 0
      end
    end
    object ScrewBack2: TPanel
      Left = 240
      Top = 32
      Width = 273
      Height = 241
      BevelOuter = bvNone
      Color = clOlive
      TabOrder = 5
      object ScrewBox2: TPanel
        Left = 0
        Top = 0
        Width = 273
        Height = 225
        BevelOuter = bvNone
        TabOrder = 0
      end
    end
    object ScrewBack6: TPanel
      Left = 240
      Top = 8
      Width = 273
      Height = 25
      BevelOuter = bvNone
      Color = clOlive
      TabOrder = 6
      object ScrewBox6: TPanel
        Left = 0
        Top = 0
        Width = 265
        Height = 25
        BevelOuter = bvNone
        TabOrder = 0
      end
    end
  end
  object GroupBox3: TGroupBox
    Left = 8
    Top = 336
    Width = 689
    Height = 137
    Caption = 'Longitudinal grouping'
    TabOrder = 2
    object Label1: TLabel
      Left = 16
      Top = 48
      Width = 124
      Height = 13
      Caption = 'Number of lines per group:'
    end
    object Label2: TLabel
      Left = 16
      Top = 84
      Width = 90
      Height = 13
      Caption = 'Grouping Function:'
    end
    object Label3: TLabel
      Left = 16
      Top = 116
      Width = 76
      Height = 13
      Caption = 'Grouping Mode:'
    end
    object SpinEdit1: TSpinEdit
      Left = 152
      Top = 43
      Width = 81
      Height = 22
      MaxValue = 100000
      MinValue = 1
      TabOrder = 0
      Value = 1
      OnChange = SpinEdit1Change
    end
    object ScrollBox4: TScrollBox
      Left = 240
      Top = 39
      Width = 441
      Height = 66
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = bsNone
      TabOrder = 1
    end
    object ComboBox1: TComboBox
      Left = 112
      Top = 80
      Width = 121
      Height = 21
      ItemHeight = 13
      TabOrder = 2
      Text = 'ComboBox1'
      OnChange = GeneralChange
      Items.Strings = (
        'Simple'
        'Average'
        'Minimum'
        'Maximum'
        'Mean Square'
        'RMS'
        'Std. Deviation'
        'Correct Count'
        'Correct Sum')
    end
    object ComboBox2: TComboBox
      Left = 112
      Top = 112
      Width = 121
      Height = 21
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 3
      Text = 'By group of lines'
      OnChange = GeneralChange
      Items.Strings = (
        'By group of lines'
        'By position in group')
    end
    object CheckBox3: TCheckBox
      Left = 576
      Top = 116
      Width = 97
      Height = 17
      AllowGrayed = True
      Caption = 'Mark All Lines'
      TabOrder = 4
      OnClick = CheckBox3Click
    end
    object CheckBox4: TCheckBox
      Left = 16
      Top = 18
      Width = 217
      Height = 17
      Caption = 'Use sequence separator ()'
      TabOrder = 5
      OnClick = CheckBox4Click
    end
    object CheckBox5: TCheckBox
      Left = 368
      Top = 116
      Width = 153
      Height = 17
      Caption = 'Consider only last value'
      TabOrder = 6
      OnClick = CheckBox5Click
    end
  end
  object MainMenu1: TMainMenu
    Left = 208
    Top = 232
    object OpenReport1: TMenuItem
      Caption = '&File'
      object OpenReport2: TMenuItem
        Caption = '&Open...'
        OnClick = OpenReport2Click
      end
      object RunScript1: TMenuItem
        Caption = '&Run Script...'
        OnClick = RunScript1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object CreateChart1: TMenuItem
        Caption = '&Chart as Output'
        Checked = True
        OnClick = CreateChart1Click
      end
      object SaveNewFile1: TMenuItem
        Caption = 'Fi&le as Output...'
        OnClick = SaveNewFile1Click
      end
      object CreateOutput1: TMenuItem
        Caption = 'Create &Output'
        OnClick = CreateOutput1Click
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'E&xit'
      end
    end
  end
  object SaveDialog1: TSaveDialog
    Left = 528
    Top = 8
  end
  object OpenDialog1: TOpenDialog
    Left = 104
    Top = 8
  end
end
