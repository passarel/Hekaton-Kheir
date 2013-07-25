object FmMMTransition: TFmMMTransition
  Left = 829
  Top = 233
  Width = 206
  Height = 306
  Caption = 'Add transition...'
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
    Left = 8
    Top = 8
    Width = 37
    Height = 13
    Caption = 'Source:'
  end
  object Label2: TLabel
    Left = 8
    Top = 56
    Width = 34
    Height = 13
    Caption = 'Target:'
  end
  object Label3: TLabel
    Left = 8
    Top = 152
    Width = 25
    Height = 13
    Caption = 'Goal:'
  end
  object Label4: TLabel
    Left = 8
    Top = 104
    Width = 27
    Height = 13
    Caption = 'Input:'
  end
  object ComboBox1: TComboBox
    Left = 8
    Top = 24
    Width = 177
    Height = 21
    ItemHeight = 13
    TabOrder = 0
  end
  object ComboBox2: TComboBox
    Left = 8
    Top = 72
    Width = 177
    Height = 21
    ItemHeight = 13
    TabOrder = 1
  end
  object CheckBox1: TCheckBox
    Left = 16
    Top = 200
    Width = 161
    Height = 17
    Caption = 'Random Weight and Count'
    TabOrder = 2
  end
  object Button1: TButton
    Left = 104
    Top = 232
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 3
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 8
    Top = 232
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 4
    OnClick = Button2Click
  end
  object ComboBox3: TComboBox
    Left = 5
    Top = 120
    Width = 177
    Height = 21
    ItemHeight = 13
    TabOrder = 5
  end
  object Edit1: TEdit
    Left = 8
    Top = 168
    Width = 161
    Height = 21
    TabOrder = 6
    Text = 'Edit1'
  end
end
