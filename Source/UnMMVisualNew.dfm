object FmMMMain: TFmMMMain
  Left = 182
  Top = 0
  Width = 982
  Height = 720
  Caption = 'FmMMMain'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object SimpleGraph: TSimpleGraph
    Left = 0
    Top = 0
    Width = 666
    Height = 662
    Align = alClient
    ClipboardFormats = [cfNative, cfMetafile, cfBitmap]
    Color = clWhite
    FixedScrollBars = True
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    HorzScrollBar.Margin = 8
    HorzScrollBar.Smooth = True
    HorzScrollBar.Tracking = True
    ParentColor = False
    ParentFont = False
    ParentShowHint = False
    PopupMenu = PopupMenu1
    ShowGrid = False
    ShowHint = True
    TabOrder = 0
    VertScrollBar.Margin = 8
    VertScrollBar.Smooth = True
    VertScrollBar.Tracking = True
    OnObjectSelect = SimpleGraphObjectSelect
  end
  object Panel1: TPanel
    Left = 666
    Top = 0
    Width = 300
    Height = 662
    Align = alRight
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    object Panel2: TPanel
      Left = 1
      Top = 33
      Width = 298
      Height = 32
      Align = alTop
      TabOrder = 0
      object Label8: TLabel
        Left = 16
        Top = 8
        Width = 36
        Height = 13
        Caption = 'Inputs'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Bevel1: TBevel
        Left = 0
        Top = 32
        Width = 289
        Height = 2
      end
      object Label3: TLabel
        Left = 8
        Top = 40
        Width = 61
        Height = 13
        Caption = 'Graph Inputs'
      end
      object Label4: TLabel
        Left = 152
        Top = 40
        Width = 67
        Height = 13
        Caption = 'Original Inputs'
      end
      object Button3: TButton
        Left = 272
        Top = 8
        Width = 16
        Height = 16
        Caption = '+'
        TabOrder = 0
        OnClick = Button3Click
      end
      object ListBox3: TListBox
        Left = 8
        Top = 56
        Width = 129
        Height = 137
        ItemHeight = 13
        MultiSelect = True
        TabOrder = 1
        OnClick = ListBox3Click
      end
      object ListBox4: TListBox
        Left = 152
        Top = 56
        Width = 129
        Height = 137
        ItemHeight = 13
        MultiSelect = True
        TabOrder = 2
        OnClick = ListBox4Click
      end
      object Button6: TButton
        Left = 120
        Top = 199
        Width = 75
        Height = 25
        Caption = 'Unify'
        TabOrder = 3
        OnClick = Button6Click
      end
      object Button7: TButton
        Left = 206
        Top = 199
        Width = 75
        Height = 25
        Caption = 'Break'
        TabOrder = 4
        OnClick = Button7Click
      end
    end
    object Panel3: TPanel
      Left = 1
      Top = 65
      Width = 298
      Height = 32
      Align = alTop
      TabOrder = 1
      object Label9: TLabel
        Left = 16
        Top = 8
        Width = 37
        Height = 13
        Caption = 'States'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Bevel2: TBevel
        Left = -2
        Top = 32
        Width = 291
        Height = 2
      end
      object Label1: TLabel
        Left = 16
        Top = 40
        Width = 62
        Height = 13
        Caption = 'Graph States'
      end
      object Label2: TLabel
        Left = 160
        Top = 40
        Width = 68
        Height = 13
        Caption = 'Original States'
      end
      object Label11: TLabel
        Left = 32
        Top = 221
        Width = 10
        Height = 13
        Caption = 'X:'
      end
      object Label12: TLabel
        Left = 32
        Top = 253
        Width = 10
        Height = 13
        Caption = 'Y:'
      end
      object Button4: TButton
        Left = 272
        Top = 8
        Width = 16
        Height = 16
        Caption = '+'
        TabOrder = 0
        OnClick = Button4Click
      end
      object ListBox1: TListBox
        Left = 8
        Top = 55
        Width = 129
        Height = 153
        ItemHeight = 13
        MultiSelect = True
        TabOrder = 1
        OnClick = ListBox1Click
      end
      object ListBox2: TListBox
        Left = 150
        Top = 55
        Width = 129
        Height = 153
        ItemHeight = 13
        MultiSelect = True
        TabOrder = 2
        OnClick = ListBox2Click
      end
      object Button1: TButton
        Left = 184
        Top = 215
        Width = 75
        Height = 25
        Caption = 'Unify'
        TabOrder = 3
        OnClick = Button1Click
      end
      object Button2: TButton
        Left = 184
        Top = 247
        Width = 75
        Height = 25
        Caption = 'Break'
        TabOrder = 4
        OnClick = Button2Click
      end
      object JvValidateEdit1: TJvValidateEdit
        Left = 48
        Top = 217
        Width = 49
        Height = 21
        CriticalPoints.MaxValueIncluded = False
        CriticalPoints.MinValueIncluded = False
        TabOrder = 5
        OnExit = JvValidateEdit1Exit
      end
      object JvValidateEdit2: TJvValidateEdit
        Left = 48
        Top = 249
        Width = 49
        Height = 21
        CriticalPoints.MaxValueIncluded = False
        CriticalPoints.MinValueIncluded = False
        TabOrder = 6
        OnExit = JvValidateEdit2Exit
      end
    end
    object Panel4: TPanel
      Left = 1
      Top = 97
      Width = 298
      Height = 32
      Align = alTop
      TabOrder = 2
      object Label10: TLabel
        Left = 16
        Top = 8
        Width = 63
        Height = 13
        Caption = 'Transitions'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Bevel3: TBevel
        Left = -2
        Top = 32
        Width = 291
        Height = 2
      end
      object Label5: TLabel
        Left = 8
        Top = 40
        Width = 78
        Height = 13
        Caption = 'Graph Transition'
      end
      object Label6: TLabel
        Left = 152
        Top = 40
        Width = 89
        Height = 13
        Caption = 'Original Transitions'
      end
      object Label13: TLabel
        Left = 16
        Top = 229
        Width = 25
        Height = 13
        Caption = 'Goal:'
        Enabled = False
      end
      object Button5: TButton
        Left = 272
        Top = 8
        Width = 16
        Height = 16
        Caption = '+'
        TabOrder = 0
        OnClick = Button5Click
      end
      object ListBox6: TListBox
        Left = 152
        Top = 56
        Width = 129
        Height = 161
        ItemHeight = 13
        MultiSelect = True
        TabOrder = 1
        OnClick = ListBox6Click
      end
      object ListBox5: TListBox
        Left = 8
        Top = 56
        Width = 129
        Height = 161
        ItemHeight = 13
        MultiSelect = True
        TabOrder = 2
        OnClick = ListBox5Click
      end
      object ListBox8: TListBox
        Left = 8
        Top = 255
        Width = 273
        Height = 154
        ItemHeight = 13
        Items.Strings = (
          'Weight'
          'Count'
          'Goal')
        ParentShowHint = False
        ShowHint = True
        TabOrder = 3
        OnClick = ListBox8Click
      end
      object JvValidateEdit3: TJvValidateEdit
        Left = 48
        Top = 225
        Width = 49
        Height = 21
        CriticalPoints.MaxValueIncluded = False
        CriticalPoints.MinValueIncluded = False
        Enabled = False
        TabOrder = 4
        OnChange = JvValidateEdit3Change
      end
      object Button8: TButton
        Left = 168
        Top = 224
        Width = 89
        Height = 25
        Caption = 'Add...'
        Enabled = False
        TabOrder = 5
        OnClick = Button8Click
      end
      object Button27: TButton
        Left = 40
        Top = 416
        Width = 89
        Height = 25
        Caption = 'Logic Program'
        TabOrder = 6
        OnClick = Button27Click
      end
      object Button28: TButton
        Left = 176
        Top = 416
        Width = 81
        Height = 25
        Caption = 'Weighted L.P.'
        TabOrder = 7
        OnClick = Button28Click
      end
    end
    object Panel5: TPanel
      Left = 1
      Top = 225
      Width = 298
      Height = 32
      Align = alTop
      TabOrder = 3
      object Label7: TLabel
        Left = 16
        Top = 8
        Width = 57
        Height = 13
        Caption = 'Execution'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Bevel4: TBevel
        Left = -2
        Top = 32
        Width = 291
        Height = 2
      end
      object Label20: TLabel
        Left = 32
        Top = 169
        Width = 71
        Height = 13
        Caption = 'Current Epoch:'
      end
      object Label21: TLabel
        Left = 8
        Top = 44
        Width = 57
        Height = 13
        Caption = 'Epoch Size:'
      end
      object Label22: TLabel
        Left = 152
        Top = 44
        Width = 56
        Height = 13
        Caption = 'Last epoch:'
      end
      object Label23: TLabel
        Left = 40
        Top = 196
        Width = 34
        Height = 13
        Caption = 'RMSE:'
      end
      object Label24: TLabel
        Left = 8
        Top = 74
        Width = 59
        Height = 13
        Caption = 'Update rate:'
      end
      object Label26: TLabel
        Left = 152
        Top = 72
        Width = 48
        Height = 13
        Caption = 'Sel. Input:'
      end
      object Gauge1: TGauge
        Left = 56
        Top = 480
        Width = 105
        Height = 25
        Progress = 0
      end
      object Gauge2: TGauge
        Left = 160
        Top = 480
        Width = 105
        Height = 25
        Progress = 0
      end
      object Label27: TLabel
        Left = 8
        Top = 486
        Width = 41
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'Label27'
      end
      object Chart1: TChart
        Left = 21
        Top = 208
        Width = 273
        Height = 185
        Legend.Visible = False
        Title.AdjustFrame = False
        Title.Text.Strings = (
          'TChart')
        Title.Visible = False
        BottomAxis.Title.Visible = False
        LeftAxis.Title.Visible = False
        View3D = False
        BevelOuter = bvNone
        PopupMenu = PopupMenu2
        TabOrder = 9
      end
      object Button9: TButton
        Left = 272
        Top = 8
        Width = 16
        Height = 17
        Caption = '+'
        TabOrder = 0
        OnClick = Button9Click
      end
      object Button18: TButton
        Left = 72
        Top = 376
        Width = 73
        Height = 25
        Caption = 'Start'
        TabOrder = 1
        OnClick = Button18Click
      end
      object Button19: TButton
        Left = 152
        Top = 376
        Width = 75
        Height = 25
        Caption = 'Stop'
        Enabled = False
        TabOrder = 2
        OnClick = Button19Click
      end
      object Button20: TButton
        Left = 200
        Top = 163
        Width = 41
        Height = 25
        Caption = 'Reset'
        TabOrder = 3
        OnClick = Button20Click
      end
      object JvValidateEdit6: TJvValidateEdit
        Left = 68
        Top = 40
        Width = 65
        Height = 21
        CriticalPoints.MaxValueIncluded = False
        CriticalPoints.MinValueIncluded = False
        TabOrder = 4
        OnChange = JvValidateEdit6Change
      end
      object JvValidateEdit7: TJvValidateEdit
        Left = 212
        Top = 40
        Width = 65
        Height = 21
        CriticalPoints.MaxValueIncluded = False
        CriticalPoints.MinValueIncluded = False
        TabOrder = 5
      end
      object SpinEdit1: TSpinEdit
        Left = 68
        Top = 69
        Width = 66
        Height = 22
        MaxValue = 1
        MinValue = 0
        TabOrder = 6
        Value = 1
        OnChange = SpinEdit1Change
      end
      object GroupBox3: TGroupBox
        Left = 24
        Top = 410
        Width = 249
        Height = 54
        Caption = 'History '
        TabOrder = 7
        object Button17: TButton
          Left = 40
          Top = 18
          Width = 75
          Height = 25
          Caption = 'Reset'
          TabOrder = 0
          OnClick = Button17Click
        end
        object Button21: TButton
          Left = 136
          Top = 18
          Width = 75
          Height = 25
          Caption = 'Save...'
          TabOrder = 1
          OnClick = Button21Click
        end
      end
      object SpinEdit2: TSpinEdit
        Left = 212
        Top = 68
        Width = 66
        Height = 22
        MaxValue = 100
        MinValue = 0
        TabOrder = 8
        Value = 50
        OnChange = SpinEdit2Change
      end
      object CheckBox3: TCheckBox
        Left = 192
        Top = 196
        Width = 89
        Height = 17
        Caption = 'RMSE Chart'
        Checked = True
        State = cbChecked
        TabOrder = 10
        OnClick = CheckBox3Click
      end
    end
    object Panel6: TPanel
      Left = 1
      Top = 129
      Width = 298
      Height = 32
      Align = alTop
      TabOrder = 4
      object Label15: TLabel
        Left = 16
        Top = 8
        Width = 52
        Height = 13
        Caption = 'Data File'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Bevel5: TBevel
        Left = -2
        Top = 32
        Width = 291
        Height = 2
      end
      object Label16: TLabel
        Left = 8
        Top = 40
        Width = 50
        Height = 13
        Caption = 'File Name:'
      end
      object Button11: TButton
        Left = 272
        Top = 8
        Width = 16
        Height = 16
        Caption = '+'
        TabOrder = 0
        OnClick = Button11Click
      end
      object Edit2: TEdit
        Left = 8
        Top = 56
        Width = 257
        Height = 21
        TabOrder = 1
      end
      object Button12: TButton
        Left = 264
        Top = 56
        Width = 20
        Height = 20
        Caption = '...'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 2
        OnClick = Button12Click
      end
      object RadioGroup2: TRadioGroup
        Left = 48
        Top = 216
        Width = 185
        Height = 81
        Caption = 'Input file in training'
        ItemIndex = 0
        Items.Strings = (
          'Don'#39't use data in file'
          'Use data as input'
          'Use full data')
        TabOrder = 3
        OnClick = RadioGroup2Click
      end
      object Memo2: TMemo
        Left = 8
        Top = 88
        Width = 281
        Height = 113
        Lines.Strings = (
          'Memo1')
        ReadOnly = True
        TabOrder = 4
      end
      object GroupBox2: TGroupBox
        Left = 16
        Top = 320
        Width = 249
        Height = 41
        Caption = 'Data Groups to be used'
        TabOrder = 5
      end
      object CheckBox2: TCheckBox
        Left = 272
        Top = 336
        Width = 17
        Height = 17
        AllowGrayed = True
        TabOrder = 6
        OnClick = CheckBox2Click
      end
    end
    object Panel7: TPanel
      Left = 1
      Top = 1
      Width = 298
      Height = 32
      Align = alTop
      TabOrder = 5
      object Label17: TLabel
        Left = 16
        Top = 8
        Width = 48
        Height = 13
        Caption = 'Network'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Bevel6: TBevel
        Left = -2
        Top = 32
        Width = 291
        Height = 2
      end
      object Label14: TLabel
        Left = 8
        Top = 40
        Width = 47
        Height = 13
        Caption = 'File Name'
      end
      object Button13: TButton
        Left = 272
        Top = 8
        Width = 16
        Height = 16
        Caption = '+'
        TabOrder = 0
        OnClick = Button13Click
      end
      object Edit1: TEdit
        Left = 8
        Top = 56
        Width = 257
        Height = 21
        ReadOnly = True
        TabOrder = 1
      end
      object Button10: TButton
        Left = 264
        Top = 56
        Width = 20
        Height = 20
        Caption = '...'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 2
        OnClick = Button10Click
      end
      object Button14: TButton
        Left = 176
        Top = 80
        Width = 113
        Height = 25
        Caption = 'Save new Network...'
        TabOrder = 3
        OnClick = Button14Click
      end
      object GroupBox1: TGroupBox
        Left = 16
        Top = 112
        Width = 265
        Height = 121
        Caption = 'Learning Parameters '
        TabOrder = 4
        object Label18: TLabel
          Left = 16
          Top = 24
          Width = 67
          Height = 13
          Caption = 'Learning Rate'
        end
        object Label19: TLabel
          Left = 16
          Top = 52
          Width = 52
          Height = 13
          Caption = 'Momentum'
        end
        object JvValidateEdit4: TJvValidateEdit
          Left = 96
          Top = 20
          Width = 121
          Height = 21
          CriticalPoints.MaxValueIncluded = False
          CriticalPoints.MinValueIncluded = False
          TabOrder = 0
        end
        object JvValidateEdit5: TJvValidateEdit
          Left = 96
          Top = 48
          Width = 121
          Height = 21
          CriticalPoints.MaxValueIncluded = False
          CriticalPoints.MinValueIncluded = False
          TabOrder = 1
        end
        object Button15: TButton
          Left = 80
          Top = 80
          Width = 97
          Height = 25
          Caption = 'Apply'
          TabOrder = 2
          OnClick = Button15Click
        end
      end
      object Button16: TButton
        Left = 108
        Top = 240
        Width = 81
        Height = 25
        Caption = 'Reset Training'
        TabOrder = 5
        OnClick = Button16Click
      end
    end
    object Panel8: TPanel
      Left = 1
      Top = 161
      Width = 298
      Height = 32
      Align = alTop
      TabOrder = 6
      object Label25: TLabel
        Left = 16
        Top = 8
        Width = 134
        Height = 13
        Caption = 'Supervision Sequences'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Bevel7: TBevel
        Left = 0
        Top = 32
        Width = 289
        Height = 2
      end
      object Button22: TButton
        Left = 272
        Top = 8
        Width = 16
        Height = 16
        Caption = '+'
        TabOrder = 0
        OnClick = Button22Click
      end
      object JvTreeView1: TJvTreeView
        Left = 8
        Top = 40
        Width = 273
        Height = 337
        Indent = 19
        ReadOnly = True
        TabOrder = 1
        OnClick = JvTreeView1Click
        LineColor = 13160660
      end
      object Button23: TButton
        Left = 24
        Top = 392
        Width = 75
        Height = 25
        Caption = 'Add Sequence'
        TabOrder = 2
        OnClick = Button23Click
      end
      object Button24: TButton
        Left = 112
        Top = 392
        Width = 75
        Height = 25
        Caption = 'Add Input'
        TabOrder = 3
        OnClick = Button24Click
      end
      object Button25: TButton
        Left = 200
        Top = 392
        Width = 75
        Height = 25
        Caption = 'Update'
        TabOrder = 4
        OnClick = Button25Click
      end
      object Button26: TButton
        Left = 112
        Top = 432
        Width = 75
        Height = 25
        Caption = 'Temp Load...'
        TabOrder = 5
        OnClick = Button26Click
      end
    end
    object Panel9: TPanel
      Left = 1
      Top = 193
      Width = 298
      Height = 32
      Align = alTop
      TabOrder = 7
      object Label28: TLabel
        Left = 16
        Top = 8
        Width = 77
        Height = 13
        Caption = 'Extra Options'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Bevel8: TBevel
        Left = -2
        Top = 32
        Width = 291
        Height = 2
      end
      object Button29: TButton
        Left = 272
        Top = 8
        Width = 16
        Height = 16
        Caption = '+'
        TabOrder = 0
        OnClick = Button29Click
      end
      object CheckBox5: TCheckBox
        Left = 28
        Top = 48
        Width = 142
        Height = 17
        Caption = 'Use Network Choice'
        TabOrder = 1
        OnClick = CheckBox5Click
      end
      object CheckBox1: TCheckBox
        Left = 28
        Top = 72
        Width = 104
        Height = 17
        Caption = 'Allow Learning'
        Checked = True
        State = cbChecked
        TabOrder = 2
        OnClick = CheckBox1Click
      end
      object RadioGroup1: TRadioGroup
        Left = 24
        Top = 96
        Width = 185
        Height = 113
        Caption = 'Default Correction'
        ItemIndex = 4
        Items.Strings = (
          'Current'
          'True'
          'False'
          'Reinforce'
          'None')
        TabOrder = 3
        OnClick = RadioGroup1Click
      end
    end
  end
  object MainMenu1: TMainMenu
    Left = 528
    Top = 232
    object File1: TMenuItem
      Caption = '&File'
      object New1: TMenuItem
        Caption = '&New'
      end
      object Load1: TMenuItem
        Caption = '&Load...'
        OnClick = Load1Click
      end
      object Save1: TMenuItem
        Caption = '&Save...'
        OnClick = Save1Click
      end
    end
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = '.xml'
    Filter = 'Network Description|*.xml|All Files|*.*'
    Left = 320
    Top = 544
  end
  object PopupMenu1: TPopupMenu
    Left = 287
    Top = 544
    object Savegraph1: TMenuItem
      Caption = 'Save graph...'
      OnClick = Savegraph1Click
    end
    object SaveWMF1: TMenuItem
      Caption = 'Save &WMF'
      OnClick = SaveWMF1Click
    end
  end
  object OpenDialog2: TOpenDialog
    DefaultExt = '.pid'
    Filter = 'Data file|*.pid|All Files|*.*'
    Left = 351
    Top = 544
  end
  object OpenDialog3: TOpenDialog
    DefaultExt = '.mms'
    Filter = 'Script File|*.mms|All Files|*.*'
    Left = 383
    Top = 544
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = '.xml'
    Filter = 'Network Description|*.xml|All Files|*.*'
    Left = 319
    Top = 584
  end
  object SaveDialog2: TSaveDialog
    DefaultExt = '.sgp'
    Filter = 'Simple Graph file|*.sgp|All Files|*.*'
    Left = 351
    Top = 584
  end
  object SaveDialog3: TSaveDialog
    DefaultExt = '.txt'
    Filter = 'Text Files|*.txt|All Files|*.*'
    Left = 383
    Top = 584
  end
  object OpenDialog4: TOpenDialog
    DefaultExt = '.txt'
    Filter = 'Text Files|*.txt|All Files|*.*'
    Left = 416
    Top = 544
  end
  object SaveDialog4: TSaveDialog
    DefaultExt = '.mms'
    Filter = 'Script File|*.mms|All Files|*.*'
    Left = 416
    Top = 584
  end
  object PopupMenu2: TPopupMenu
    Left = 256
    Top = 328
    object Loadfromfile1: TMenuItem
      Caption = '&Load from file (RMSE hist)'
      OnClick = Loadfromfile1Click
    end
    object Savetofile1: TMenuItem
      Caption = '&Save to file (RMSE hist)'
      OnClick = Savetofile1Click
    end
    object SaveasImage1: TMenuItem
      Caption = 'Save as &Image (WMF)'
      OnClick = SaveasImage1Click
    end
  end
  object SaveDialog5: TSaveDialog
    DefaultExt = '.wmf'
    Filter = 'Windows Metafile|*.wmf'
    Left = 448
    Top = 584
  end
  object OpenDialog5: TOpenDialog
    DefaultExt = '.plg'
    Filter = 'Logic Program|*.plg|All files|*.*'
    Left = 448
    Top = 544
  end
end
