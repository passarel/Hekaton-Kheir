object Form1: TForm1
  Left = 1
  Top = 11
  Width = 1015
  Height = 621
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Gauge1: TGauge
    Left = 424
    Top = 288
    Width = 113
    Height = 113
    BackColor = clBtnFace
    BorderStyle = bsNone
    ForeColor = clBlue
    Progress = 0
  end
  object Gauge2: TGauge
    Left = 424
    Top = 440
    Width = 113
    Height = 113
    BackColor = clBtnFace
    BorderStyle = bsNone
    ForeColor = clNavy
    Progress = 0
  end
  object Label1: TLabel
    Left = 400
    Top = 8
    Width = 40
    Height = 13
    Caption = 'Network'
  end
  object Label2: TLabel
    Left = 400
    Top = 152
    Width = 39
    Height = 13
    Caption = 'Epochs:'
  end
  object Label3: TLabel
    Left = 400
    Top = 104
    Width = 92
    Height = 13
    Caption = 'Error Threshold (%):'
  end
  object Label4: TLabel
    Left = 424
    Top = 264
    Width = 74
    Height = 13
    Caption = 'Current training:'
  end
  object Label5: TLabel
    Left = 424
    Top = 416
    Width = 64
    Height = 13
    Caption = 'Total training:'
  end
  object Label6: TLabel
    Left = 400
    Top = 56
    Width = 87
    Height = 13
    Caption = 'Number of outputs'
  end
  object StringGrid1: TStringGrid
    Left = 16
    Top = 32
    Width = 345
    Height = 521
    TabOrder = 0
  end
  object Button1: TButton
    Left = 48
    Top = 560
    Width = 75
    Height = 25
    Caption = 'Load...'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 144
    Top = 560
    Width = 75
    Height = 25
    Caption = 'Save...'
    TabOrder = 2
    OnClick = Button2Click
  end
  object StringGrid2: TStringGrid
    Left = 616
    Top = 8
    Width = 377
    Height = 545
    TabOrder = 3
  end
  object Button3: TButton
    Left = 792
    Top = 560
    Width = 75
    Height = 25
    Caption = 'Save'
    TabOrder = 4
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 440
    Top = 200
    Width = 75
    Height = 25
    Caption = 'Execute'
    TabOrder = 5
    OnClick = Button4Click
  end
  object SpinEdit1: TSpinEdit
    Left = 400
    Top = 168
    Width = 161
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 6
    Value = 0
  end
  object Edit1: TEdit
    Left = 400
    Top = 24
    Width = 145
    Height = 21
    TabOrder = 7
    Text = 'Edit1'
  end
  object Button6: TButton
    Left = 240
    Top = 560
    Width = 75
    Height = 25
    Caption = 'Randomize'
    TabOrder = 8
  end
  object SpinEdit2: TSpinEdit
    Left = 400
    Top = 120
    Width = 161
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 9
    Value = 0
  end
  object Button5: TButton
    Left = 552
    Top = 24
    Width = 25
    Height = 25
    Caption = '...'
    TabOrder = 10
  end
  object CheckBox1: TCheckBox
    Left = 32
    Top = 8
    Width = 97
    Height = 17
    Caption = 'First line as label'
    TabOrder = 11
    OnClick = CheckBox1Click
  end
  object SpinEdit3: TSpinEdit
    Left = 400
    Top = 72
    Width = 161
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 12
    Value = 0
  end
  object OpenDialog1: TOpenDialog
    Left = 544
    Top = 224
  end
  object SaveDialog1: TSaveDialog
    Left = 568
    Top = 304
  end
end
