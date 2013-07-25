object FmGraphViz: TFmGraphViz
  Left = 192
  Top = 114
  BorderStyle = bsDialog
  Caption = 'External Software Required'
  ClientHeight = 190
  ClientWidth = 453
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
    Left = 38
    Top = 116
    Width = 86
    Height = 13
    Caption = '"Dot" Executable:'
  end
  object Memo1: TMemo
    Left = 22
    Top = 8
    Width = 409
    Height = 89
    Alignment = taCenter
    BevelInner = bvNone
    BorderStyle = bsNone
    Color = clBtnFace
    Lines.Strings = (
      
        'To allow this functionality, you must install the "dot" module o' +
        'f GraphViz.'
      'You can download GraphViz at:'
      ''
      'http://www.graphviz.org/'
      ''
      
        'If GraphViz is already installed in your machine, enter the file' +
        ' path below:')
    ReadOnly = True
    TabOrder = 0
  end
  object Edit1: TEdit
    Left = 125
    Top = 112
    Width = 265
    Height = 21
    TabOrder = 1
  end
  object Button1: TButton
    Left = 390
    Top = 110
    Width = 25
    Height = 25
    Caption = '...'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 137
    Top = 160
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 3
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 241
    Top = 160
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 4
    OnClick = Button3Click
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = '.exe'
    Filter = 'Application|*.exe'
    Left = 416
    Top = 152
  end
end
