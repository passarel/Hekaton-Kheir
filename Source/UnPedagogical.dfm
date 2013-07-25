object FmPedagogical: TFmPedagogical
  Left = 244
  Top = 127
  Width = 657
  Height = 643
  Caption = 'Pedagogical visualization (prototype)'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 40
    Top = 504
    Width = 32
    Height = 13
    Caption = 'Output'
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 633
    Height = 497
    ActivePage = TabSheet2
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Formulae'
      object ScrollBox1: TScrollBox
        Left = 0
        Top = 0
        Width = 625
        Height = 469
        Align = alClient
        TabOrder = 0
        object ListBox1: TListBox
          Left = 8
          Top = 8
          Width = 609
          Height = 449
          ItemHeight = 13
          TabOrder = 0
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Colors'
      ImageIndex = 1
      object ScrollBox2: TScrollBox
        Left = 0
        Top = 0
        Width = 625
        Height = 469
        Align = alClient
        TabOrder = 0
        object Image1: TImage
          Left = 0
          Top = 24
          Width = 569
          Height = 433
        end
        object Label2: TLabel
          Left = 8
          Top = 8
          Width = 27
          Height = 13
          Caption = 'Input:'
        end
        object TrackBar1: TTrackBar
          Left = 576
          Top = 16
          Width = 33
          Height = 449
          Orientation = trVertical
          TabOrder = 0
          OnChange = TrackBar1Change
        end
      end
    end
  end
  object ComboBox1: TComboBox
    Left = 40
    Top = 520
    Width = 145
    Height = 21
    ItemHeight = 13
    TabOrder = 1
  end
  object GroupBox1: TGroupBox
    Left = 272
    Top = 504
    Width = 169
    Height = 81
    Caption = 'Input Values'
    TabOrder = 2
    object Label3: TLabel
      Left = 8
      Top = 20
      Width = 20
      Height = 13
      Caption = 'Min:'
    end
    object Label4: TLabel
      Left = 8
      Top = 52
      Width = 23
      Height = 13
      Caption = 'Max:'
    end
    object Edit1: TEdit
      Left = 48
      Top = 24
      Width = 121
      Height = 21
      TabOrder = 0
      Text = 'Edit1'
    end
    object Edit2: TEdit
      Left = 48
      Top = 48
      Width = 121
      Height = 21
      TabOrder = 1
      Text = 'Edit1'
    end
  end
  object GroupBox2: TGroupBox
    Left = 456
    Top = 504
    Width = 169
    Height = 81
    Caption = 'Output Values'
    TabOrder = 3
    object Label5: TLabel
      Left = 8
      Top = 20
      Width = 20
      Height = 13
      Caption = 'Min:'
    end
    object Label6: TLabel
      Left = 8
      Top = 52
      Width = 23
      Height = 13
      Caption = 'Max:'
    end
    object Edit4: TEdit
      Left = 40
      Top = 48
      Width = 121
      Height = 21
      TabOrder = 0
      Text = 'Edit1'
    end
    object Edit3: TEdit
      Left = 40
      Top = 16
      Width = 121
      Height = 21
      TabOrder = 1
      Text = 'Edit1'
    end
  end
  object MainMenu1: TMainMenu
    Left = 8
    Top = 136
    object Visualisation1: TMenuItem
      Caption = '&Visualisation'
      object LoadNetwork1: TMenuItem
        Caption = 'Load Network...'
        OnClick = LoadNetwork1Click
      end
      object PonderedView1: TMenuItem
        Caption = 'Pondered View'
        OnClick = PonderedView1Click
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 168
    Top = 8
  end
end
