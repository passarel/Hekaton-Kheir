object FmTrEvol: TFmTrEvol
  Left = 231
  Top = 57
  BorderStyle = bsDialog
  Caption = 'Treinamento da Rede'
  ClientHeight = 171
  ClientWidth = 678
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
  object Label1: TLabel
    Left = 16
    Top = 8
    Width = 183
    Height = 13
    Caption = 'Executando treinamento: '#201'poca i de N'
  end
  object Gauge1: TGauge
    Left = 48
    Top = 32
    Width = 609
    Height = 25
    ForeColor = clMoneyGreen
    Progress = 57
  end
  object Label2: TLabel
    Left = 48
    Top = 64
    Width = 609
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'Erro quadrado m'#233'dio = '
  end
  object Gauge2: TGauge
    Left = 48
    Top = 112
    Width = 609
    Height = 25
    ForeColor = clMoneyGreen
    Progress = 57
  end
  object Label3: TLabel
    Left = 24
    Top = 88
    Width = 162
    Height = 13
    Caption = 'Cross Validation: Valida'#231#227'o i de N:'
  end
  object Button1: TButton
    Left = 447
    Top = 144
    Width = 98
    Height = 25
    Caption = 'Cancel(Validation)'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Chart1: TChart
    Left = 8
    Top = 176
    Width = 665
    Height = 441
    BackWall.Brush.Color = clWhite
    BackWall.Brush.Style = bsClear
    Border.Visible = True
    Gradient.Direction = gdFromTopLeft
    Gradient.EndColor = 16777173
    Gradient.StartColor = 16777143
    Legend.LegendStyle = lsSeries
    MarginTop = 5
    Title.Text.Strings = (
      'MSE Evolution Through Training')
    BottomAxis.Automatic = False
    BottomAxis.AutomaticMaximum = False
    BottomAxis.AutomaticMinimum = False
    BottomAxis.Axis.Color = clWhite
    BottomAxis.Axis.Style = psDashDotDot
    BottomAxis.Axis.Width = 1
    BottomAxis.Axis.SmallDots = True
    BottomAxis.AxisValuesFormat = '0.###'
    BottomAxis.Grid.Color = 13948116
    BottomAxis.Maximum = 500.000000000000000000
    BottomAxis.Title.Caption = 'Training Epochs'
    BottomAxis.Visible = False
    LeftAxis.Axis.Color = clWhite
    LeftAxis.Axis.Style = psDashDotDot
    LeftAxis.Axis.Width = 1
    LeftAxis.Axis.SmallDots = True
    LeftAxis.AxisValuesFormat = '0.000e-00'
    LeftAxis.Grid.Color = 13948116
    LeftAxis.TicksInner.Visible = False
    LeftAxis.Title.Caption = 'Mean Square Error'
    RightAxis.Axis.Width = 3
    RightAxis.Axis.SmallDots = True
    RightAxis.Visible = False
    TopAxis.Axis.Color = clWhite
    TopAxis.Axis.Width = 1
    TopAxis.Axis.SmallDots = True
    TopAxis.Visible = False
    View3D = False
    View3DWalls = False
    Color = clWhite
    TabOrder = 1
    Visible = False
    object Series1: TFastLineSeries
      Marks.Arrow.Visible = True
      Marks.Callout.Brush.Color = clBlack
      Marks.Callout.Arrow.Visible = True
      Marks.Visible = False
      SeriesColor = clGray
      Title = 'Training'
      LinePen.Color = clGray
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
    object Series2: TFastLineSeries
      Marks.Arrow.Visible = True
      Marks.Callout.Brush.Color = clBlack
      Marks.Callout.Arrow.Visible = True
      Marks.Visible = False
      SeriesColor = clBlack
      Title = 'Test'
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
  end
  object Button2: TButton
    Left = 560
    Top = 144
    Width = 89
    Height = 25
    Caption = 'Cancel All'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 48
    Top = 144
    Width = 97
    Height = 25
    Caption = 'Show Chart...'
    TabOrder = 3
    OnClick = Button3Click
  end
  object UpDown1: TUpDown
    Left = 272
    Top = 620
    Width = 129
    Height = 20
    Orientation = udHorizontal
    TabOrder = 4
    Thousands = False
    OnClick = UpDown1Click
  end
  object Button4: TButton
    Left = 592
    Top = 620
    Width = 75
    Height = 20
    Caption = 'Save...'
    TabOrder = 5
    Visible = False
    OnClick = Button4Click
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = '.wmf'
    Filter = 'Windows Metafile|*.wmf'
    Left = 8
    Top = 40
  end
end
