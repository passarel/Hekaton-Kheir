object FmWeightVis: TFmWeightVis
  Left = 195
  Top = 95
  Width = 652
  Height = 643
  Caption = 'Weights'#39' visualization (prototype)'
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
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 633
    Height = 585
    ActivePage = TabSheet1
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Strings'
      object ScrollBox1: TScrollBox
        Left = 0
        Top = 0
        Width = 625
        Height = 557
        Align = alClient
        TabOrder = 0
        object StringGrid1: TStringGrid
          Left = 0
          Top = 0
          Width = 621
          Height = 553
          Align = alClient
          DefaultColWidth = 80
          DefaultRowHeight = 18
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
        Height = 557
        Align = alClient
        TabOrder = 0
        object Image1: TImage
          Left = 0
          Top = 24
          Width = 577
          Height = 393
        end
        object Image2: TImage
          Left = 0
          Top = 440
          Width = 577
          Height = 113
        end
        object Label1: TLabel
          Left = 8
          Top = 424
          Width = 35
          Height = 13
          Caption = 'Output:'
        end
        object Label2: TLabel
          Left = 8
          Top = 8
          Width = 27
          Height = 13
          Caption = 'Input:'
        end
        object TrackBar1: TTrackBar
          Left = 584
          Top = 24
          Width = 33
          Height = 529
          Max = 100
          Min = 1
          Orientation = trVertical
          Position = 50
          TabOrder = 0
          OnChange = TrackBar1Change
        end
      end
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
      object SaveGrid1: TMenuItem
        Caption = '&Save Grid'
        OnClick = SaveGrid1Click
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 168
    Top = 8
  end
  object SaveDialog1: TSaveDialog
    Left = 420
    Top = 176
  end
end
