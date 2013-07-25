object Form2: TForm2
  Left = 197
  Top = 113
  Width = 927
  Height = 675
  Caption = 'Form2'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Modules: TGroupBox
    Left = 16
    Top = 16
    Width = 185
    Height = 305
    Caption = 'Modules'
    TabOrder = 0
    object Memo1: TMemo
      Left = 16
      Top = 24
      Width = 153
      Height = 241
      Lines.Strings = (
        'Each module will be '
        'represented by an individual '
        'neural network, in a SCTL '
        'architecture')
      TabOrder = 0
    end
  end
  object GroupBox1: TGroupBox
    Left = 224
    Top = 24
    Width = 185
    Height = 297
    Caption = 'Communication Manager'
    TabOrder = 1
    object Memo2: TMemo
      Left = 16
      Top = 16
      Width = 153
      Height = 257
      Lines.Strings = (
        'This module should define how '
        'do the modules communicate '
        'with each other, which '
        'constraints are applied to this '
        'communications, and how the '
        'external inputs are presented '
        'to the system. Also, it can '
        'contain some record of the '
        'outputs, and some way to '
        'apply the expected output '
        'values for the learning task')
      TabOrder = 0
    end
  end
end
