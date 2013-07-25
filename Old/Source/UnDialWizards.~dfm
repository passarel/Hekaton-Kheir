object FmWizards: TFmWizards
  Left = 531
  Top = 104
  BorderStyle = bsDialog
  Caption = 'Architecture Generator'
  ClientHeight = 419
  ClientWidth = 400
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 400
    Height = 419
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'FeedForward'
      object Label1: TLabel
        Left = 8
        Top = 45
        Width = 70
        Height = 13
        Caption = 'Input Neurons:'
      end
      object Label6: TLabel
        Left = 8
        Top = 77
        Width = 78
        Height = 13
        Caption = 'Output Neurons:'
      end
      object Label7: TLabel
        Left = 8
        Top = 109
        Width = 71
        Height = 13
        Caption = 'Hidden Layers:'
      end
      object SpinEdit1: TSpinEdit
        Left = 88
        Top = 40
        Width = 65
        Height = 22
        MaxValue = 255
        MinValue = 1
        TabOrder = 0
        Value = 1
      end
      object SpinEdit2: TSpinEdit
        Left = 88
        Top = 72
        Width = 65
        Height = 22
        MaxValue = 255
        MinValue = 1
        TabOrder = 1
        Value = 1
      end
      object GroupBox1: TGroupBox
        Left = 176
        Top = 8
        Width = 201
        Height = 153
        Caption = 'Neurons in hidden layers:'
        TabOrder = 2
        object ScrollBox1: TScrollBox
          Left = 2
          Top = 15
          Width = 197
          Height = 136
          Align = alClient
          TabOrder = 0
        end
      end
      object SpinEdit3: TSpinEdit
        Left = 88
        Top = 104
        Width = 65
        Height = 22
        MaxValue = 50
        MinValue = 0
        TabOrder = 3
        Value = 0
        OnChange = SpinEdit3Change
      end
      object GroupBox2: TGroupBox
        Left = 17
        Top = 175
        Width = 353
        Height = 170
        Caption = 'Neurons configuration'
        TabOrder = 4
        object Label4: TLabel
          Left = 128
          Top = 22
          Width = 19
          Height = 13
          Caption = 'Eta:'
        end
        object Label8: TLabel
          Left = 16
          Top = 60
          Width = 50
          Height = 13
          Caption = 'Activation:'
        end
        object Label9: TLabel
          Left = 184
          Top = 60
          Width = 51
          Height = 13
          Caption = 'Parameter:'
        end
        object Edit3: TEdit
          Left = 152
          Top = 18
          Width = 89
          Height = 21
          TabOrder = 0
          Text = '0'
          OnEnter = Edit10Enter
          OnExit = Edit10Exit
        end
        object CheckBox2: TCheckBox
          Left = 40
          Top = 120
          Width = 49
          Height = 17
          Caption = 'Bias:'
          TabOrder = 1
        end
        object Edit6: TEdit
          Left = 240
          Top = 56
          Width = 89
          Height = 21
          TabOrder = 2
          Text = '1'
          OnEnter = Edit10Enter
          OnExit = Edit10Exit
        end
        object ComboBox4: TComboBox
          Left = 72
          Top = 56
          Width = 97
          Height = 21
          ItemHeight = 13
          ItemIndex = 2
          TabOrder = 3
          Text = 'LogSig (bipolar)'
          OnChange = ComboBox3Change
          Items.Strings = (
            'Logsig'
            'Tansig'
            'LogSig (bipolar)'
            'Limiar'
            'Limiar (bipolar)'
            'Linear'
            'Crazy')
        end
        object Panel2: TPanel
          Left = 96
          Top = 92
          Width = 201
          Height = 73
          TabOrder = 4
          object Label10: TLabel
            Left = 8
            Top = 44
            Width = 35
            Height = 13
            Caption = 'Range:'
          end
          object Label18: TLabel
            Left = 8
            Top = 12
            Width = 37
            Height = 13
            Caption = 'Weight:'
          end
          object Edit7: TEdit
            Left = 48
            Top = 8
            Width = 81
            Height = 21
            TabOrder = 0
            Text = '0'
            OnEnter = Edit10Enter
            OnExit = Edit10Exit
          end
          object Edit16: TEdit
            Left = 48
            Top = 40
            Width = 81
            Height = 21
            TabOrder = 1
            Text = '0'
            OnEnter = Edit10Enter
            OnExit = Edit10Exit
          end
          object CheckBox3: TCheckBox
            Left = 144
            Top = 28
            Width = 49
            Height = 17
            Caption = 'Fixed'
            TabOrder = 2
          end
        end
      end
      object Button1: TButton
        Left = 59
        Top = 360
        Width = 75
        Height = 25
        Caption = '&Generate'
        Default = True
        TabOrder = 5
        OnClick = Button1Click
      end
      object Button2: TButton
        Left = 259
        Top = 360
        Width = 75
        Height = 25
        Cancel = True
        Caption = 'Cancel'
        TabOrder = 6
        OnClick = Button2Click
      end
      object Button9: TButton
        Left = 160
        Top = 360
        Width = 75
        Height = 25
        Caption = 'Merge...'
        TabOrder = 7
        OnClick = Button9Click
      end
      object CheckBox7: TCheckBox
        Left = 24
        Top = 144
        Width = 121
        Height = 17
        Caption = 'Add '#39'relational'#39' links'
        TabOrder = 8
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Elman Network'
      ImageIndex = 1
      object Label11: TLabel
        Left = 112
        Top = 37
        Width = 70
        Height = 13
        Caption = 'Input Neurons:'
      end
      object Label12: TLabel
        Left = 104
        Top = 117
        Width = 78
        Height = 13
        Caption = 'Output Neurons:'
      end
      object Label13: TLabel
        Left = 104
        Top = 77
        Width = 78
        Height = 13
        Caption = 'Hidden/Context:'
      end
      object SpinEdit4: TSpinEdit
        Left = 192
        Top = 32
        Width = 65
        Height = 22
        MaxValue = 255
        MinValue = 0
        TabOrder = 0
        Value = 0
      end
      object SpinEdit5: TSpinEdit
        Left = 192
        Top = 112
        Width = 65
        Height = 22
        MaxValue = 255
        MinValue = 0
        TabOrder = 1
        Value = 0
      end
      object SpinEdit6: TSpinEdit
        Left = 192
        Top = 72
        Width = 65
        Height = 22
        MaxValue = 255
        MinValue = 0
        TabOrder = 2
        Value = 0
      end
      object GroupBox4: TGroupBox
        Left = 17
        Top = 175
        Width = 353
        Height = 170
        Caption = 'Neurons configuration'
        TabOrder = 3
        object Label16: TLabel
          Left = 128
          Top = 22
          Width = 19
          Height = 13
          Caption = 'Eta:'
        end
        object Label19: TLabel
          Left = 16
          Top = 60
          Width = 50
          Height = 13
          Caption = 'Activation:'
        end
        object Label20: TLabel
          Left = 184
          Top = 60
          Width = 51
          Height = 13
          Caption = 'Parameter:'
        end
        object Edit10: TEdit
          Left = 152
          Top = 18
          Width = 89
          Height = 21
          TabOrder = 0
          Text = '0'
          OnEnter = Edit10Enter
          OnExit = Edit10Exit
        end
        object CheckBox1: TCheckBox
          Left = 40
          Top = 120
          Width = 49
          Height = 17
          Caption = 'Bias:'
          TabOrder = 1
        end
        object Edit12: TEdit
          Left = 240
          Top = 56
          Width = 89
          Height = 21
          TabOrder = 2
          Text = '1'
          OnEnter = Edit10Enter
          OnExit = Edit10Exit
        end
        object ComboBox3: TComboBox
          Left = 72
          Top = 56
          Width = 97
          Height = 21
          ItemHeight = 13
          ItemIndex = 2
          TabOrder = 3
          Text = 'LogSig (bipolar)'
          OnChange = ComboBox3Change
          Items.Strings = (
            'Logsig'
            'Tansig'
            'LogSig (bipolar)'
            'Limiar'
            'Limiar (bipolar)'
            'Linear'
            'Crazy')
        end
        object Panel1: TPanel
          Left = 96
          Top = 92
          Width = 201
          Height = 73
          TabOrder = 4
          object Label21: TLabel
            Left = 8
            Top = 44
            Width = 35
            Height = 13
            Caption = 'Range:'
          end
          object Label22: TLabel
            Left = 8
            Top = 12
            Width = 37
            Height = 13
            Caption = 'Weight:'
          end
          object Edit14: TEdit
            Left = 48
            Top = 8
            Width = 81
            Height = 21
            TabOrder = 0
            Text = '0'
            OnEnter = Edit10Enter
            OnExit = Edit10Exit
          end
          object Edit15: TEdit
            Left = 48
            Top = 40
            Width = 81
            Height = 21
            TabOrder = 1
            Text = '0'
            OnEnter = Edit10Enter
            OnExit = Edit10Exit
          end
          object CheckBox4: TCheckBox
            Left = 144
            Top = 28
            Width = 49
            Height = 17
            Caption = 'Fixed'
            TabOrder = 2
          end
        end
      end
      object Button3: TButton
        Left = 59
        Top = 360
        Width = 75
        Height = 25
        Caption = '&Generate'
        Default = True
        TabOrder = 4
        OnClick = Button3Click
      end
      object Button4: TButton
        Left = 259
        Top = 360
        Width = 75
        Height = 25
        Cancel = True
        Caption = 'Cancel'
        TabOrder = 5
        OnClick = Button2Click
      end
      object Button8: TButton
        Left = 160
        Top = 360
        Width = 75
        Height = 25
        Caption = 'Merge...'
        TabOrder = 6
        OnClick = Button8Click
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'SCTL/CILP - like'
      ImageIndex = 2
      OnShow = TabSheet3Show
      object Label17: TLabel
        Left = 8
        Top = 13
        Width = 70
        Height = 13
        Caption = 'Input Neurons:'
      end
      object Label23: TLabel
        Left = 8
        Top = 69
        Width = 77
        Height = 13
        Caption = 'Hidden Neurons'
      end
      object Label24: TLabel
        Left = 8
        Top = 125
        Width = 78
        Height = 13
        Caption = 'Output Neurons:'
      end
      object Label25: TLabel
        Left = 168
        Top = 29
        Width = 82
        Height = 13
        Caption = 'Context Neurons:'
      end
      object Label26: TLabel
        Left = 168
        Top = 61
        Width = 94
        Height = 13
        Caption = 'Intersection(output):'
      end
      object Label27: TLabel
        Left = 168
        Top = 141
        Width = 94
        Height = 13
        Caption = 'Intersection(output):'
      end
      object Label28: TLabel
        Left = 168
        Top = 109
        Width = 90
        Height = 13
        Caption = 'Temporal Neurons:'
      end
      object GroupBox3: TGroupBox
        Left = 17
        Top = 175
        Width = 353
        Height = 170
        Caption = 'Neurons configuration'
        TabOrder = 0
        object Label2: TLabel
          Left = 128
          Top = 22
          Width = 19
          Height = 13
          Caption = 'Eta:'
        end
        object Label3: TLabel
          Left = 16
          Top = 60
          Width = 50
          Height = 13
          Caption = 'Activation:'
        end
        object Label5: TLabel
          Left = 184
          Top = 60
          Width = 51
          Height = 13
          Caption = 'Parameter:'
        end
        object Edit1: TEdit
          Left = 152
          Top = 18
          Width = 89
          Height = 21
          TabOrder = 0
          Text = '0'
          OnEnter = Edit10Enter
          OnExit = Edit10Exit
        end
        object CheckBox5: TCheckBox
          Left = 40
          Top = 120
          Width = 49
          Height = 17
          Caption = 'Bias:'
          TabOrder = 1
        end
        object Edit2: TEdit
          Left = 240
          Top = 56
          Width = 89
          Height = 21
          TabOrder = 2
          Text = '1'
          OnEnter = Edit10Enter
          OnExit = Edit10Exit
        end
        object ComboBox1: TComboBox
          Left = 72
          Top = 56
          Width = 97
          Height = 21
          ItemHeight = 13
          TabOrder = 3
          Text = 'LogSig (bipolar)'
          OnChange = ComboBox3Change
          Items.Strings = (
            'Logsig'
            'Tansig'
            'LogSig (bipolar)'
            'Limiar'
            'Limiar (bipolar)'
            'Linear'
            'Crazy')
        end
        object Panel3: TPanel
          Left = 96
          Top = 92
          Width = 201
          Height = 73
          TabOrder = 4
          object Label14: TLabel
            Left = 8
            Top = 44
            Width = 35
            Height = 13
            Caption = 'Range:'
          end
          object Label15: TLabel
            Left = 8
            Top = 12
            Width = 37
            Height = 13
            Caption = 'Weight:'
          end
          object Edit4: TEdit
            Left = 48
            Top = 8
            Width = 81
            Height = 21
            TabOrder = 0
            Text = '0'
            OnEnter = Edit10Enter
            OnExit = Edit10Exit
          end
          object Edit5: TEdit
            Left = 48
            Top = 40
            Width = 81
            Height = 21
            TabOrder = 1
            Text = '0'
            OnEnter = Edit10Enter
            OnExit = Edit10Exit
          end
          object CheckBox6: TCheckBox
            Left = 144
            Top = 28
            Width = 49
            Height = 17
            Caption = 'Fixed'
            TabOrder = 2
          end
        end
      end
      object Button5: TButton
        Left = 59
        Top = 360
        Width = 75
        Height = 25
        Caption = '&Generate'
        Default = True
        TabOrder = 1
        OnClick = Button5Click
      end
      object Button6: TButton
        Left = 259
        Top = 360
        Width = 75
        Height = 25
        Cancel = True
        Caption = 'Cancel'
        TabOrder = 2
        OnClick = Button2Click
      end
      object SpinEdit7: TSpinEdit
        Left = 32
        Top = 32
        Width = 89
        Height = 22
        MaxValue = 255
        MinValue = 0
        TabOrder = 3
        Value = 0
      end
      object SpinEdit8: TSpinEdit
        Left = 32
        Top = 88
        Width = 89
        Height = 22
        MaxValue = 255
        MinValue = 0
        TabOrder = 4
        Value = 0
      end
      object SpinEdit9: TSpinEdit
        Left = 32
        Top = 144
        Width = 89
        Height = 22
        MaxValue = 255
        MinValue = 0
        TabOrder = 5
        Value = 0
        OnExit = SpinEdit9Change
      end
      object SpinEdit10: TSpinEdit
        Left = 272
        Top = 24
        Width = 89
        Height = 22
        MaxValue = 255
        MinValue = 0
        TabOrder = 6
        Value = 0
        OnExit = SpinEdit9Change
      end
      object SpinEdit11: TSpinEdit
        Left = 272
        Top = 56
        Width = 89
        Height = 22
        MaxValue = 255
        MinValue = 0
        TabOrder = 7
        Value = 0
        OnExit = SpinEdit12Change
      end
      object SpinEdit12: TSpinEdit
        Left = 272
        Top = 104
        Width = 89
        Height = 22
        MaxValue = 255
        MinValue = 0
        TabOrder = 8
        Value = 0
        OnExit = SpinEdit12Change
      end
      object SpinEdit13: TSpinEdit
        Left = 272
        Top = 136
        Width = 89
        Height = 22
        MaxValue = 255
        MinValue = 0
        TabOrder = 9
        Value = 0
        OnExit = SpinEdit9Change
      end
      object Button7: TButton
        Left = 160
        Top = 360
        Width = 75
        Height = 25
        Caption = 'Merge...'
        TabOrder = 10
        OnClick = Button7Click
      end
    end
  end
end
