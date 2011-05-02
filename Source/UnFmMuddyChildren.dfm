object FmMuddyChildren: TFmMuddyChildren
  Left = 436
  Top = 57
  Width = 653
  Height = 578
  Caption = 'Muddy Children Puzzle'
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
    Width = 645
    Height = 544
    ActivePage = TabSheet2
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Environment Options'
      object GroupBox1: TGroupBox
        Left = 8
        Top = 8
        Width = 289
        Height = 329
        Caption = 'Networks (files) '
        TabOrder = 0
        object ListBox1: TListBox
          Left = 8
          Top = 16
          Width = 273
          Height = 225
          ItemHeight = 13
          ScrollWidth = 500
          TabOrder = 0
          OnClick = ListBox1Click
        end
        object Edit1: TEdit
          Left = 24
          Top = 256
          Width = 225
          Height = 21
          TabOrder = 1
        end
        object Button1: TButton
          Left = 248
          Top = 256
          Width = 21
          Height = 21
          Caption = '...'
          TabOrder = 2
          OnClick = Button1Click
        end
        object Button2: TButton
          Left = 187
          Top = 288
          Width = 75
          Height = 25
          Caption = 'Delete'
          TabOrder = 5
          OnClick = Button2Click
        end
        object Button3: TButton
          Left = 107
          Top = 288
          Width = 75
          Height = 25
          Caption = 'Set'
          TabOrder = 4
          OnClick = Button3Click
        end
        object Button4: TButton
          Left = 27
          Top = 288
          Width = 75
          Height = 25
          Caption = 'Insert'
          TabOrder = 3
          OnClick = Button4Click
        end
      end
      object GroupBox2: TGroupBox
        Left = 320
        Top = 8
        Width = 289
        Height = 329
        Caption = 'Agents'
        TabOrder = 1
        object ListBox2: TListBox
          Left = 8
          Top = 16
          Width = 273
          Height = 137
          ItemHeight = 13
          TabOrder = 0
          OnClick = ListBox2Click
        end
        object Button6: TButton
          Left = 187
          Top = 288
          Width = 75
          Height = 25
          Caption = 'Delete'
          TabOrder = 3
          OnClick = Button6Click
        end
        object Button7: TButton
          Left = 107
          Top = 288
          Width = 75
          Height = 25
          Caption = 'Set'
          TabOrder = 2
          OnClick = Button7Click
        end
        object Button8: TButton
          Left = 27
          Top = 288
          Width = 75
          Height = 25
          Caption = 'Insert'
          TabOrder = 1
          OnClick = Button8Click
        end
        object Panel1: TPanel
          Left = 8
          Top = 160
          Width = 273
          Height = 121
          TabOrder = 4
          object Label1: TLabel
            Left = 8
            Top = 16
            Width = 31
            Height = 13
            Caption = 'Name:'
          end
          object Label2: TLabel
            Left = 8
            Top = 44
            Width = 27
            Height = 13
            Caption = 'Brain:'
          end
          object Label3: TLabel
            Left = 8
            Top = 92
            Width = 103
            Height = 13
            Caption = 'Muddy Probability (%):'
          end
          object ComboBox2: TComboBox
            Left = 88
            Top = 88
            Width = 153
            Height = 21
            ItemHeight = 13
            TabOrder = 3
            Visible = False
          end
          object SpinEdit1: TSpinEdit
            Left = 120
            Top = 88
            Width = 121
            Height = 22
            MaxValue = 0
            MinValue = 0
            TabOrder = 4
            Value = 0
          end
          object Edit2: TEdit
            Left = 48
            Top = 12
            Width = 209
            Height = 21
            TabOrder = 0
          end
          object ComboBox1: TComboBox
            Left = 48
            Top = 40
            Width = 209
            Height = 21
            ItemHeight = 13
            TabOrder = 1
          end
          object CheckBox1: TCheckBox
            Left = 16
            Top = 68
            Width = 97
            Height = 17
            Caption = 'Watcher'
            TabOrder = 2
            OnClick = CheckBox1Click
          end
        end
      end
      object GroupBox4: TGroupBox
        Left = 136
        Top = 344
        Width = 377
        Height = 169
        Caption = 'Other Options'
        TabOrder = 2
        object Label4: TLabel
          Left = 16
          Top = 120
          Width = 148
          Height = 13
          Caption = 'Random Initialization of Agents:'
        end
        object Label5: TLabel
          Left = 16
          Top = 72
          Width = 94
          Height = 13
          Caption = 'Finish game when...'
        end
        object Label7: TLabel
          Left = 304
          Top = 72
          Width = 50
          Height = 13
          Caption = 'Strict Max.'
        end
        object CheckBox2: TCheckBox
          Left = 16
          Top = 24
          Width = 297
          Height = 17
          Caption = 'Reinforce only when agent should know she is muddy'
          TabOrder = 0
        end
        object CheckBox3: TCheckBox
          Left = 16
          Top = 48
          Width = 273
          Height = 17
          Caption = 'Inform q1 as true at every time point'
          TabOrder = 1
        end
        object ComboBox3: TComboBox
          Left = 16
          Top = 88
          Width = 257
          Height = 21
          ItemHeight = 13
          TabOrder = 2
          Text = 'Number of played turns equals number of players'
          Items.Strings = (
            'Number of played turns equals number of players'
            'One agent correctly guess she is muddy'
            'Either one of the conditions are true'
            'Both Conditions are true')
        end
        object ComboBox4: TComboBox
          Left = 16
          Top = 136
          Width = 273
          Height = 21
          ItemHeight = 13
          ItemIndex = 0
          TabOrder = 3
          Text = 'Consider the individual probabilities'
          Items.Strings = (
            'Consider the individual probabilities'
            'Consider the probability of first agent for all others'
            'Randomize the number of muddy agents'
            'Randomize the number, except by first agent')
        end
        object SpinEdit3: TSpinEdit
          Left = 304
          Top = 88
          Width = 65
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 4
          Value = 10
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Puzzle visualisation'
      ImageIndex = 1
      object Gauge1: TGauge
        Left = 520
        Top = 240
        Width = 105
        Height = 105
        BackColor = clBtnFace
        BorderStyle = bsNone
        ForeColor = clTeal
        Kind = gkPie
        Progress = 65
      end
      object Label6: TLabel
        Left = 528
        Top = 120
        Width = 86
        Height = 13
        Caption = 'Number of games:'
      end
      object Button5: TButton
        Left = 528
        Top = 8
        Width = 89
        Height = 25
        Caption = 'Create'
        TabOrder = 0
        OnClick = Button5Click
      end
      object Button9: TButton
        Left = 528
        Top = 40
        Width = 89
        Height = 25
        Caption = 'Single Step'
        TabOrder = 1
        OnClick = Button9Click
      end
      object Button10: TButton
        Left = 528
        Top = 72
        Width = 89
        Height = 25
        Caption = 'Run...'
        TabOrder = 2
        OnClick = Button10Click
      end
      object GroupBox3: TGroupBox
        Left = 8
        Top = 8
        Width = 505
        Height = 505
        Caption = 'GroupBox3'
        TabOrder = 3
      end
      object SpinEdit2: TSpinEdit
        Left = 528
        Top = 136
        Width = 89
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 4
        Value = 0
      end
      object Button11: TButton
        Left = 536
        Top = 432
        Width = 75
        Height = 25
        Caption = 'Tmp???'
        TabOrder = 5
        OnClick = Button11Click
      end
    end
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = '.xml'
    Filter = 'Networks (XML Files)|*.xml|All files|*.*'
    Left = 544
    Top = 192
  end
  object PopupMenu2: TPopupMenu
    Left = 252
    Top = 336
    object asdasd1: TMenuItem
      Caption = 'asdasd'
    end
  end
  object SaveDialog1: TSaveDialog
    Left = 576
    Top = 192
  end
end
