object FmDoubleFloat: TFmDoubleFloat
  Left = 591
  Top = 304
  Width = 165
  Height = 177
  Caption = 'FmDoubleFloat'
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
    Width = 66
    Height = 13
    Caption = 'Correct value:'
  end
  object Label2: TLabel
    Left = 8
    Top = 56
    Width = 95
    Height = 13
    Caption = 'Absolute Tolerance:'
  end
  object Button1: TButton
    Left = 88
    Top = 104
    Width = 57
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 16
    Top = 104
    Width = 57
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Edit1: TEdit
    Left = 16
    Top = 24
    Width = 121
    Height = 21
    TabOrder = 2
    Text = 'Edit1'
  end
  object Edit2: TEdit
    Left = 16
    Top = 80
    Width = 121
    Height = 21
    TabOrder = 3
    Text = 'Edit1'
  end
end