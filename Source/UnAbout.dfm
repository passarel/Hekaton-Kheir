object FmAbout: TFmAbout
  Left = 585
  Top = 289
  BorderStyle = bsDialog
  Caption = 'About'
  ClientHeight = 152
  ClientWidth = 315
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 8
    Top = 8
    Width = 297
    Height = 112
    Alignment = taCenter
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    Color = clBtnFace
    Lines.Strings = (
      'Hekaton Khire Project'
      ''
      'By Rafa "Passarel" Borges'
      '(passarel@gmail.com)'
      ''
      'Version 01/2010 '
      ''
      'Last Release: 31/01/2010')
    ReadOnly = True
    TabOrder = 0
  end
  object Button1: TButton
    Left = 120
    Top = 120
    Width = 75
    Height = 25
    Caption = 'Close'
    Default = True
    TabOrder = 1
    OnClick = Button1Click
  end
end
