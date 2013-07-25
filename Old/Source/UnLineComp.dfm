object FmLineComp: TFmLineComp
  Left = 193
  Top = 131
  Width = 870
  Height = 640
  Caption = 'FmLineComp'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object ImpStringgrid1: TImpStringgrid
    Left = 24
    Top = -8
    Width = 609
    Height = 561
    DefaultRowHeight = 16
    TabOrder = 0
  end
  object GroupBox1: TGroupBox
    Left = 664
    Top = 152
    Width = 185
    Height = 217
    Caption = 'Conflict analysis'
    TabOrder = 1
    object Label1: TLabel
      Left = 8
      Top = 24
      Width = 79
      Height = 13
      Caption = 'Selection Criteria'
    end
    object Label2: TLabel
      Left = 8
      Top = 80
      Width = 152
      Height = 13
      Caption = 'Maximum (Euclidean) Difference'
    end
    object Label3: TLabel
      Left = 56
      Top = 160
      Width = 32
      Height = 13
      Caption = 'Label3'
    end
    object Button1: TButton
      Left = 32
      Top = 184
      Width = 113
      Height = 25
      Caption = 'Mark Conflicts'
      TabOrder = 0
      OnClick = Button1Click
    end
    object ComboBox1: TComboBox
      Left = 16
      Top = 40
      Width = 145
      Height = 21
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 1
      Text = 'Max'
      Items.Strings = (
        'Max'
        'Min'
        'All'
        'None')
    end
    object Edit1: TEdit
      Left = 16
      Top = 104
      Width = 121
      Height = 21
      TabOrder = 2
      Text = 'Edit1'
    end
  end
end
