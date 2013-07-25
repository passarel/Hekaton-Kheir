object FmMerge: TFmMerge
  Left = 666
  Top = 170
  BorderStyle = bsDialog
  Caption = 'Merge Style'
  ClientHeight = 103
  ClientWidth = 230
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 12
    Width = 69
    Height = 13
    Caption = 'How to merge:'
  end
  object Label2: TLabel
    Left = 8
    Top = 45
    Width = 88
    Height = 13
    Caption = 'Level of new links:'
  end
  object ComboBox1: TComboBox
    Left = 104
    Top = 8
    Width = 113
    Height = 21
    ItemHeight = 13
    TabOrder = 0
    Text = 'Only add'
    OnChange = ComboBox1Change
    Items.Strings = (
      'Only add'
      'As Input'
      'As Output')
  end
  object SpinEdit1: TSpinEdit
    Left = 104
    Top = 40
    Width = 113
    Height = 22
    MaxValue = 16
    MinValue = -1
    TabOrder = 1
    Value = 0
  end
  object Button1: TButton
    Left = 22
    Top = 72
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 134
    Top = 72
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 3
    OnClick = Button2Click
  end
end
