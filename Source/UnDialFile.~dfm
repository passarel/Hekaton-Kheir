object FmFileConf: TFmFileConf
  Left = 249
  Top = 175
  BorderStyle = bsDialog
  Caption = 'Data File Editor'
  ClientHeight = 568
  ClientWidth = 773
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
  object PageControl1: TPageControl
    Left = 384
    Top = 0
    Width = 385
    Height = 529
    ActivePage = TabSheet1
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Main properties'
      object GroupBox1: TGroupBox
        Left = 0
        Top = 0
        Width = 377
        Height = 73
        Align = alTop
        Caption = 'Labels'
        TabOrder = 0
        object CheckBox5: TCheckBox
          Left = 16
          Top = 48
          Width = 193
          Height = 17
          Caption = 'Consider first line as labels'
          Enabled = False
          TabOrder = 0
          OnClick = CheckBox5Click
        end
        object CheckBox6: TCheckBox
          Left = 16
          Top = 20
          Width = 161
          Height = 17
          Caption = 'Do not use labels'
          Checked = True
          State = cbChecked
          TabOrder = 1
          OnClick = CheckBox6Click
        end
      end
      object GroupBox2: TGroupBox
        Left = 0
        Top = 79
        Width = 367
        Height = 130
        Caption = 'Groups'
        TabOrder = 1
        object CheckBox7: TCheckBox
          Left = 16
          Top = 16
          Width = 97
          Height = 17
          Caption = 'Group lines'
          TabOrder = 0
          OnClick = CheckBox7Click
        end
        object SpinEdit1: TSpinEdit
          Left = 144
          Top = 51
          Width = 121
          Height = 22
          MaxValue = 2
          MinValue = 1
          TabOrder = 1
          Value = 1
          OnExit = SpinEdit1Change
        end
        object RadioButton1: TRadioButton
          Left = 40
          Top = 56
          Width = 97
          Height = 17
          Caption = 'Lines Per Group:'
          Enabled = False
          TabOrder = 2
          OnClick = RadioButton1Click
        end
        object RadioButton2: TRadioButton
          Left = 40
          Top = 32
          Width = 161
          Height = 17
          Caption = 'Use empty lines as marks'
          Checked = True
          Enabled = False
          TabOrder = 3
          TabStop = True
          OnClick = RadioButton2Click
        end
        object CheckBox11: TCheckBox
          Left = 16
          Top = 80
          Width = 177
          Height = 17
          Caption = 'Randomize order when training'
          TabOrder = 4
        end
        object CheckBox12: TCheckBox
          Left = 16
          Top = 104
          Width = 201
          Height = 17
          Caption = 'Expand domain when checking'
          TabOrder = 5
        end
      end
      object Button1: TButton
        Left = 120
        Top = 567
        Width = 75
        Height = 25
        Caption = 'Carregar...'
        TabOrder = 2
      end
      object GroupBox4: TGroupBox
        Left = 0
        Top = 208
        Width = 367
        Height = 249
        Caption = 'Columns'
        TabOrder = 3
        object ScrollBox1: TScrollBox
          Left = 2
          Top = 15
          Width = 363
          Height = 232
          Align = alClient
          TabOrder = 0
        end
      end
      object CheckBox2: TCheckBox
        Left = 134
        Top = 468
        Width = 97
        Height = 17
        AllowGrayed = True
        Caption = 'AllOutput'
        State = cbGrayed
        TabOrder = 4
        OnClick = CheckBox2Click
      end
      object CheckBox1: TCheckBox
        Left = 38
        Top = 468
        Width = 97
        Height = 17
        AllowGrayed = True
        Caption = 'All Input'
        State = cbGrayed
        TabOrder = 5
        OnClick = CheckBox1Click
      end
      object Button5: TButton
        Left = 264
        Top = 464
        Width = 89
        Height = 25
        Caption = 'Auto Selection...'
        TabOrder = 6
        OnClick = Button5Click
      end
      object GroupBox6: TGroupBox
        Left = 64
        Top = 240
        Width = 233
        Height = 185
        Caption = 'AutoSelection'
        TabOrder = 7
        Visible = False
        object Label1: TLabel
          Left = 16
          Top = 24
          Width = 95
          Height = 13
          Caption = 'Null columns on left:'
        end
        object Label2: TLabel
          Left = 16
          Top = 56
          Width = 121
          Height = 13
          Caption = 'Output Columns on Right:'
        end
        object Label4: TLabel
          Left = 16
          Top = 88
          Width = 131
          Height = 13
          Caption = 'Minimum of different values:'
        end
        object CheckBox3: TCheckBox
          Left = 48
          Top = 120
          Width = 145
          Height = 17
          Caption = 'Delete duplicated columns'
          TabOrder = 0
        end
        object SpinEdit3: TSpinEdit
          Left = 152
          Top = 19
          Width = 65
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 1
          Value = 0
        end
        object SpinEdit4: TSpinEdit
          Left = 152
          Top = 51
          Width = 65
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 2
          Value = 0
        end
        object SpinEdit5: TSpinEdit
          Left = 152
          Top = 83
          Width = 65
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 3
          Value = 0
        end
        object Button6: TButton
          Left = 136
          Top = 152
          Width = 75
          Height = 25
          Caption = 'OK'
          TabOrder = 4
          OnClick = Button6Click
        end
        object Button7: TButton
          Left = 32
          Top = 152
          Width = 75
          Height = 25
          Caption = 'Cancel'
          TabOrder = 5
          OnClick = Button7Click
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Subsets Properties'
      ImageIndex = 1
      object Label3: TLabel
        Left = 91
        Top = 16
        Width = 91
        Height = 13
        Caption = 'Number of subsets:'
      end
      object GroupBox3: TGroupBox
        Left = 0
        Top = 64
        Width = 367
        Height = 217
        Caption = 'Similarity Measures'
        TabOrder = 0
        object ScrollBox2: TScrollBox
          Left = 2
          Top = 15
          Width = 363
          Height = 200
          Align = alClient
          TabOrder = 0
        end
      end
      object SpinEdit2: TSpinEdit
        Left = 188
        Top = 11
        Width = 89
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 1
        Value = 0
      end
      object CheckBox8: TCheckBox
        Left = 204
        Top = 40
        Width = 121
        Height = 17
        Caption = 'Keep proportionality'
        TabOrder = 2
        OnClick = CheckBox8Click
      end
      object CheckBox9: TCheckBox
        Left = 41
        Top = 40
        Width = 97
        Height = 17
        Caption = 'Unitary Subsets'
        Checked = True
        State = cbChecked
        TabOrder = 3
        OnClick = CheckBox9Click
      end
      object GroupBox5: TGroupBox
        Left = 0
        Top = 288
        Width = 361
        Height = 145
        Caption = 'Main Outputs '
        TabOrder = 4
        object ScrollBox3: TScrollBox
          Left = 2
          Top = 15
          Width = 357
          Height = 128
          Align = alClient
          TabOrder = 0
        end
      end
      object Button2: TButton
        Left = 280
        Top = 440
        Width = 75
        Height = 25
        Caption = '&Build Subsets'
        TabOrder = 5
        OnClick = Button2Click
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Value editor'
      ImageIndex = 2
      object GroupBox7: TGroupBox
        Left = 8
        Top = 16
        Width = 177
        Height = 153
        Caption = 'List of columns'
        TabOrder = 0
        object ListBox1: TListBox
          Left = 8
          Top = 16
          Width = 161
          Height = 129
          ItemHeight = 13
          MultiSelect = True
          TabOrder = 0
          OnClick = ListBox1Click
        end
      end
      object GroupBox8: TGroupBox
        Left = 200
        Top = 16
        Width = 169
        Height = 153
        Caption = 'Substitution'
        TabOrder = 1
        object Label5: TLabel
          Left = 16
          Top = 104
          Width = 92
          Height = 13
          Caption = 'Number of Columns'
        end
        object RadioButton3: TRadioButton
          Left = 16
          Top = 16
          Width = 113
          Height = 17
          Caption = 'Propositionalisation'
          TabOrder = 0
          OnClick = RadioButton3Click
        end
        object CheckBox4: TCheckBox
          Left = 40
          Top = 32
          Width = 97
          Height = 17
          Caption = 'Bipolar'
          TabOrder = 1
          OnClick = CheckBox4Click
        end
        object RadioButton4: TRadioButton
          Left = 16
          Top = 56
          Width = 113
          Height = 17
          Caption = 'Enumeration'
          TabOrder = 2
          OnClick = RadioButton4Click
        end
        object SpinEdit6: TSpinEdit
          Left = 48
          Top = 120
          Width = 97
          Height = 22
          MaxValue = 100
          MinValue = 1
          TabOrder = 3
          Value = 1
          OnChange = SpinEdit6Change
        end
        object RadioButton5: TRadioButton
          Left = 16
          Top = 80
          Width = 113
          Height = 17
          Caption = 'Custom'
          Checked = True
          TabOrder = 4
          TabStop = True
          OnClick = RadioButton5Click
        end
      end
      object StringGrid1: TStringGrid
        Left = 8
        Top = 176
        Width = 361
        Height = 265
        DefaultColWidth = 50
        DefaultRowHeight = 16
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowSizing, goColSizing, goEditing]
        TabOrder = 2
      end
      object Button8: TButton
        Left = 88
        Top = 446
        Width = 81
        Height = 25
        Caption = 'Apply Changes'
        TabOrder = 3
        OnClick = Button8Click
      end
      object CheckBox10: TCheckBox
        Left = 232
        Top = 452
        Width = 113
        Height = 17
        Caption = 'Keep Old Columns'
        Checked = True
        Enabled = False
        State = cbChecked
        TabOrder = 4
      end
    end
  end
  object ImpStringgrid1: TImpStringgrid
    Left = 8
    Top = 8
    Width = 361
    Height = 553
    ColCount = 2
    DefaultRowHeight = 16
    RowCount = 1
    FixedRows = 0
    PopupMenu = PopupMenu1
    TabOrder = 1
  end
  object Button3: TButton
    Left = 600
    Top = 536
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 488
    Top = 536
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 3
    OnClick = Button4Click
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = '.txt'
    Filter = 'Text File|*.txt|All Files|*.*'
    Left = 208
    Top = 200
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = '.pid'
    Filter = 'Processed Input|*.pid|All Files|*.*'
    Left = 256
    Top = 200
  end
  object PopupMenu1: TPopupMenu
    Left = 232
    Top = 200
    object Randomize1: TMenuItem
      Caption = 'Randomize'
      OnClick = Randomize1Click
    end
    object Rotate1: TMenuItem
      Caption = 'Rotate'
      OnClick = Rotate1Click
    end
    object GroupCentre1: TMenuItem
      Caption = 'Group Centres'
      OnClick = GroupCentre1Click
    end
    object FilterConflicts1: TMenuItem
      Caption = 'Filter Conflicts'
      OnClick = FilterConflicts1Click
    end
  end
end
