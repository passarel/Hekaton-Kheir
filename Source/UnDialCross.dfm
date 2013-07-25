object FmCross: TFmCross
  Left = 149
  Top = 32
  BorderStyle = bsDialog
  Caption = 'Cross Validation'
  ClientHeight = 546
  ClientWidth = 495
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 8
    Top = 56
    Width = 289
    Height = 449
    Caption = 'Data set'
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 20
      Width = 47
      Height = 13
      Caption = 'File Name'
    end
    object Label2: TLabel
      Left = 8
      Top = 64
      Width = 55
      Height = 13
      Caption = 'Information:'
    end
    object Edit1: TEdit
      Left = 8
      Top = 36
      Width = 249
      Height = 21
      ReadOnly = True
      TabOrder = 0
    end
    object Button1: TButton
      Left = 256
      Top = 36
      Width = 21
      Height = 21
      Caption = '...'
      TabOrder = 1
      OnClick = Button1Click
    end
    object Memo1: TMemo
      Left = 8
      Top = 80
      Width = 265
      Height = 97
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 2
      WordWrap = False
    end
    object PageControl1: TPageControl
      Left = 8
      Top = 184
      Width = 265
      Height = 257
      ActivePage = TabSheet2
      TabOrder = 3
      object TabSheet1: TTabSheet
        Caption = 'Inputs'
        object ScrollBox2: TScrollBox
          Left = 0
          Top = 0
          Width = 257
          Height = 193
          Align = alTop
          TabOrder = 0
        end
        object Button5: TButton
          Left = 80
          Top = 202
          Width = 97
          Height = 25
          Caption = 'All inputs from file'
          TabOrder = 1
          OnClick = Button5Click
        end
      end
      object TabSheet2: TTabSheet
        Caption = 'Outputs'
        ImageIndex = 1
        object ScrollBox1: TScrollBox
          Left = 0
          Top = 0
          Width = 257
          Height = 193
          Align = alTop
          TabOrder = 0
        end
        object Button4: TButton
          Left = 80
          Top = 202
          Width = 97
          Height = 25
          Caption = 'All outputs from file'
          TabOrder = 1
          OnClick = Button4Click
        end
      end
    end
    object CheckBox5: TCheckBox
      Left = 16
      Top = 416
      Width = 249
      Height = 17
      Caption = 'Load "Min" and "Max" values from file'
      TabOrder = 4
    end
  end
  object GroupBox2: TGroupBox
    Left = 304
    Top = 296
    Width = 185
    Height = 209
    Caption = 'Training Process'
    TabOrder = 1
    object Label3: TLabel
      Left = 24
      Top = 72
      Width = 108
      Height = 13
      Caption = 'Groups in Training File:'
    end
    object Label4: TLabel
      Left = 24
      Top = 116
      Width = 91
      Height = 13
      Caption = 'Groups in Test File:'
    end
    object Label7: TLabel
      Left = 24
      Top = 160
      Width = 102
      Height = 13
      Caption = 'Number of validations'
    end
    object SpinEdit1: TSpinEdit
      Left = 32
      Top = 88
      Width = 121
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 0
      Value = 0
      OnChange = SpinEdit1Change
    end
    object SpinEdit2: TSpinEdit
      Left = 32
      Top = 132
      Width = 121
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 1
      Value = 0
      OnChange = SpinEdit2Change
    end
    object SpinEdit5: TSpinEdit
      Left = 32
      Top = 176
      Width = 121
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 2
      Value = 0
    end
    object RadioButton1: TRadioButton
      Left = 8
      Top = 24
      Width = 113
      Height = 17
      Caption = 'Cross Validation'
      TabOrder = 3
    end
    object RadioButton2: TRadioButton
      Left = 8
      Top = 48
      Width = 113
      Height = 17
      Caption = 'Only Training'
      TabOrder = 4
    end
  end
  object GroupBox3: TGroupBox
    Left = 304
    Top = 8
    Width = 185
    Height = 281
    Caption = 'Training Options'
    TabOrder = 2
    object Label5: TLabel
      Left = 16
      Top = 192
      Width = 87
      Height = 13
      Caption = 'Number of epochs'
    end
    object Label6: TLabel
      Left = 14
      Top = 236
      Width = 120
      Height = 13
      Caption = 'Report: Error treshold (%):'
    end
    object CheckBox3: TCheckBox
      Left = 8
      Top = 48
      Width = 113
      Height = 17
      Caption = 'Selective Training'
      TabOrder = 0
    end
    object CheckBox4: TCheckBox
      Left = 8
      Top = 24
      Width = 161
      Height = 17
      Caption = 'Selective Error Calculation'
      TabOrder = 1
    end
    object SpinEdit3: TSpinEdit
      Left = 32
      Top = 208
      Width = 121
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 2
      Value = 0
    end
    object CheckBox2: TCheckBox
      Left = 8
      Top = 72
      Width = 153
      Height = 17
      Caption = 'Keep training best epoch'
      TabOrder = 3
    end
    object CheckBox6: TCheckBox
      Left = 8
      Top = 120
      Width = 169
      Height = 17
      Caption = 'Expand domain when checking'
      TabOrder = 4
    end
    object Edit2: TEdit
      Left = 24
      Top = 162
      Width = 121
      Height = 21
      Enabled = False
      TabOrder = 5
    end
    object Button6: TButton
      Left = 144
      Top = 160
      Width = 27
      Height = 25
      Caption = '...'
      Enabled = False
      TabOrder = 6
      OnClick = Button6Click
    end
    object SpinEdit4: TSpinEdit
      Left = 32
      Top = 251
      Width = 121
      Height = 22
      MaxValue = 100
      MinValue = 0
      TabOrder = 7
      Value = 50
    end
    object CheckBox1: TCheckBox
      Left = 8
      Top = 96
      Width = 153
      Height = 17
      Caption = 'Randomize order in groups'
      TabOrder = 8
    end
    object CheckBox7: TCheckBox
      Left = 8
      Top = 144
      Width = 97
      Height = 17
      Caption = 'Save Networks'
      TabOrder = 9
      OnClick = CheckBox7Click
    end
  end
  object Button2: TButton
    Left = 250
    Top = 513
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Close'
    TabOrder = 3
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 170
    Top = 513
    Width = 75
    Height = 25
    Caption = 'Execute...'
    Default = True
    TabOrder = 4
    OnClick = Button3Click
  end
  object RadioGroup1: TRadioGroup
    Left = 8
    Top = 8
    Width = 289
    Height = 41
    Caption = 'Validation'
    Columns = 2
    ItemIndex = 0
    Items.Strings = (
      'Individual'
      'Directory List')
    TabOrder = 5
    OnClick = RadioGroup1Click
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = '.pid'
    Filter = 'Processed Input|*.pid|All Files|*.*'
    Left = 432
    Top = 480
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = '.rrf'
    Filter = 'Report File|*.rrf|All Files|*.*'
    Left = 400
    Top = 480
  end
  object Timer1: TTimer
    Interval = 10000
    OnTimer = Timer1Timer
    Left = 464
    Top = 480
  end
  object SaveDialog2: TSaveDialog
    Left = 160
    Top = 136
  end
end
