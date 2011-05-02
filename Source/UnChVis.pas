{
  @abstract(Unit to visualize the classification results through charts and results matrix)
  @author(Rafa "Passarel" Borges <passarel@gmail.com>)
  @created(London, 2008)
  @lastmod(London, January, 2010)
}

unit UnChVis;

interface

uses
  Chart, Classes, Controls, Dialogs, ExtCtrls, Forms, Graphics, Grids,
  JvEdit, JvExStdCtrls, JvValidateEdit, Menus, Messages, Series, StdCtrls,
  SysUtils, TeEngine, TeeProcs, Variants, Windows;

//-------------------------------------------------------------------------------------------------

type
  TFmChVis = class(TForm)
    Button1: TButton;
    Chart1: TChart;
    Chart2: TMenuItem;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    Close1: TMenuItem;
    ColorDialog1: TColorDialog;
    Edit1: TEdit;
    JvValidateEdit1: TJvValidateEdit;
    JvValidateEdit2: TJvValidateEdit;
    JvValidateEdit3: TJvValidateEdit;
    JvValidateEdit4: TJvValidateEdit;
    Label2: TLabel;
    MainMenu1: TMainMenu;
    N1: TMenuItem;
    ools1: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    SaveasEMF1: TMenuItem;
    SaveasJPEG1: TMenuItem;
    SaveasWMV1: TMenuItem;
    SaveDialog1: TSaveDialog;
    SaveDialog2: TSaveDialog;
    SaveDialog3: TSaveDialog;
    ScrollBox1: TScrollBox;
    ScrollBox2: TScrollBox;
    Series1: TLineSeries;
    ShowRightBar1: TMenuItem;
    StringGrid1: TStringGrid;
    procedure Button1Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure Close1Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure JvValidateEdit1Exit(Sender: TObject);
    procedure JvValidateEdit2Exit(Sender: TObject);
    procedure JvValidateEdit3Exit(Sender: TObject);
    procedure JvValidateEdit4Exit(Sender: TObject);
    procedure Panel3Click(Sender: TObject);
    procedure SaveasEMF1Click(Sender: TObject);
    procedure SaveasJPEG1Click(Sender: TObject);
    procedure SaveasWMV1Click(Sender: TObject);
    procedure ShowRightBar1Click(Sender: TObject);
  private
    //Array containing the colours used in the chart
    FColors : array of TColor;

    //Last X Value in the chart
    FLastValue : integer;

    //Procedure to set up the matrix of values when a point in the chart is selected
    //@param(Sender: series which point was clicked - not used)
    //@param(ValueIndex: Index of the clicked position in the chart)
    //@param(X: Position of the clicked point - not used)
    //@param(Y: Position of the clicked point - not used)
    procedure ClickPointer(Sender: TCustomSeries; ValueIndex, X, Y: Integer);

    //Procedure called when the colour of a series is changed
    //@param(Sender: Object that called the method - not used)
    procedure ColorChange(Sender : TObject);

    //Procedure called when the label of a series is changed
    //@param(Sender: Object that called the method - not used)
    procedure LabelChange(Sender : TObject);

    //Procedure called when the line style of a series is changed
    //@param(Sender: Object that called the method - not used)
    procedure LineChange(Sender : TObject);

    //Procedure called when the point style of a series is changed
    //@param(Sender: Object that called the method - not used)
    procedure PointChange(Sender : TObject);

  public
    //Procedure called to show the form, filling the interface properly
    procedure Execute;

  end;

//-------------------------------------------------------------------------------------------------

var
  FmChVis: TFmChVis;

//-------------------------------------------------------------------------------------------------

implementation

uses Math;

{$R *.dfm}

//-------------------------------------------------------------------------------------------------

procedure TFmChVis.Button1Click(Sender: TObject);
  var
    i : integer;
    j : byte;
    d : double;
  begin
  Chart1.Color := clWhite;
  d := 255 / Chart1.SeriesCount;
  for i := 0 to Chart1.SeriesCount - 1 do
    begin
    j := 255 - round((i + 1) * d);
    FColors[i] := RGB(j, j, j);
    if Chart1.Series[i] is TLineSeries then
       (Chart1.Series[i] as TLineSeries).Color := FColors[i];
    end;
  for i := 0 to ScrollBox1.ControlCount - 1 do
    if (ScrollBox1.Controls[i] is TPanel) then
       (ScrollBox1.Controls[i] as TPanel).color := FColors[ScrollBox1.Controls[i].Tag];
  end;

//-------------------------------------------------------------------------------------------------

procedure TFmChVis.CheckBox1Click(Sender: TObject);
  begin
  ClickPointer(nil, FLastValue, 0, 0);
  end;

//-------------------------------------------------------------------------------------------------

procedure TFmChVis.CheckBox2Click(Sender: TObject);
  begin
  Chart1.LeftAxis.Logarithmic := CheckBox2.Checked;
  end;

//-------------------------------------------------------------------------------------------------

procedure TFmChVis.CheckBox3Click(Sender: TObject);
  begin
  Chart1.BottomAxis.Logarithmic := CheckBox3.Checked;
  end;

//-------------------------------------------------------------------------------------------------

procedure TFmChVis.ClickPointer(Sender: TCustomSeries; ValueIndex, X, Y: Integer);
  var
    i, j : integer;
    XRowCount : integer;
  begin
  if CheckBox1.Checked then
     begin
     XRowCount := Ceil(sqrt(Chart1.SeriesCount));
     StringGrid1.ColCount := XRowCount + 1;
     StringGrid1.RowCount := XRowCount + 1;
     StringGrid1.FixedRows := 1;
     StringGrid1.FixedCols := 1;
     StringGrid1.Cells[0, 0] := 'x=' + inttostr(ValueIndex);
     for i := 0 to XRowCount - 1 do
       StringGrid1.Cells[i + 1, 0] := inttostr(i);
     for j := 0 to XRowCount - 1 do
       StringGrid1.Cells[0, j + 1] := inttostr(j);
     for i := 0 to XRowCount - 1 do
       for j := 0 to XRowCount - 1 do
         if  (ValueIndex < Chart1.Series[(j * XRowCount + i)].YValues.Count) then
            StringGrid1.Cells[i + 1, j + 1] :=
              FloatToStrF(Chart1.Series[(j * XRowCount + i)].YValue[ValueIndex], ffGeneral, 4, 4)
         else
            StringGrid1.Cells[i + 1, j + 1] := '';
     end
  else
     begin
     XRowCount := Chart1.SeriesCount;
     StringGrid1.ColCount := 2;
     StringGrid1.RowCount := XRowCount + 1;
     StringGrid1.FixedRows := 1;
     StringGrid1.FixedCols := 1;
     StringGrid1.Cells[0, 0] := 'X value:';
     StringGrid1.Cells[1, 0] := inttostr(ValueIndex);
     for i := 0 to XRowCount - 1 do
       begin
       StringGrid1.Cells[0, i + 1] := Chart1.Series[i].Title;
       if  (ValueIndex < Chart1.Series[i].YValues.Count) then
           StringGrid1.Cells[1, i + 1] :=
             FloatToStrF(Chart1.Series[i].YValue[ValueIndex], ffGeneral, 4, 4)
       else
            StringGrid1.Cells[1, i + 1] := '';
       end;
     end;
  FLastValue := ValueIndex;
  end;

//-------------------------------------------------------------------------------------------------

procedure TFmChVis.Close1Click(Sender: TObject);
  begin
  Close;
  end;

//-------------------------------------------------------------------------------------------------

procedure TFmChVis.ColorChange(Sender : TObject);
  begin
  if Sender is TPanel then
     begin
     ColorDialog1.Color := (Sender as TPanel).Color;
     if ColorDialog1.Execute then
        begin
        FColors[(Sender as TPanel).Tag] := ColorDialog1.Color;
        Chart1.Series[(Sender as TPanel).Tag].Color := ColorDialog1.Color;
        (Sender as TPanel).Color := ColorDialog1.Color;
        end;
     end;
  end;

//-------------------------------------------------------------------------------------------------

procedure TFmChVis.Edit1Change(Sender: TObject);
  begin
  if Chart1.Title.Text.Count = 0 then
     Chart1.Title.Text.Add(Edit1.Text)
  else
  Chart1.Title.Text.Strings[0] := Edit1.Text;
  end;

//-------------------------------------------------------------------------------------------------

procedure TFmChVis.Execute;
  var i : integer;
  begin
  FLastValue := round (Chart1.Series[0].XValues[Chart1.Series[0].Count - 1]);
  ClickPointer(nil, FLastValue, 0, 0);
  SetLength(FColors, Chart1.SeriesCount);
  for i := ScrollBox1.ControlCount - 1 downto 0 do
    ScrollBox1.Controls[i].Free;
  for i := 0 to Chart1.SeriesCount - 1 do
    begin
    //Create TLabel component
    with TLabel.Create(self) do
      begin
      Parent  := ScrollBox1;
      Top     := (i * 50) + 8;
      Left    := 10;
      Height  := 12;
      Tag     := i;
      Caption := 'Series ' + inttostr(i) + ' - label:';
      end;

    //Create TEdit Component
    with TEdit.Create(self) do
      begin
      Parent   := ScrollBox1;
      Top      := (i * 50) + 25;
      Left     := 10;
      Height   := 20;
      Width    := 140;
      Tag      := i;
      Text     := Chart1.Series[i].Title;
      OnChange := LabelChange;
      end;

    //Color Configuration
    with TPanel.Create(self) do
      begin
      Parent  := ScrollBox1;
      Top     := (i * 50) + 9;
      Left    := 100;
      Height  := 15;
      Width   := 15;
      Color   := Chart1.Series[i].color;
      Caption := '';
      Tag     := i;
      OnClick := ColorChange;
      end;

    //Color Configuration
    with TCheckBox.Create(self) do
      begin
      Parent  := ScrollBox1;
      Top     := (i * 50) + 8;
      Left    := 120;
//      Height  := 20;
      Width   := 25;
      Caption := 'P';
      Tag     := i;
      OnClick := PointChange;
      end;

    //Color Configuration
    with TCheckBox.Create(self) do
      begin
      Parent  := ScrollBox1;
      Top     := (i * 50) + 8;
      Left    := 150;
//      Height  := 20;
      Width   := 25;
      Caption := 'L';
      Tag     := i;
      Checked := true;
      OnClick := LineChange;
      end;
    end;
  if Chart1.Title.Text.Count > 0 then
     Edit1.Text := Chart1.Title.Text.Strings[0]
  else
     Edit1.Text := '';
  ShowRightBar1.Checked := true;
  ShowModal;
  end;

//-------------------------------------------------------------------------------------------------

procedure TFmChVis.JvValidateEdit1Exit(Sender: TObject);
  begin
  Chart1.LeftAxis.AutomaticMinimum := false;
  Chart1.LeftAxis.Minimum := JvValidateEdit1.Value;
  end;

//-------------------------------------------------------------------------------------------------

procedure TFmChVis.JvValidateEdit2Exit(Sender: TObject);
  begin
  Chart1.LeftAxis.AutomaticMaximum := false;
  Chart1.LeftAxis.Maximum := JvValidateEdit2.Value;
  end;

//-------------------------------------------------------------------------------------------------

procedure TFmChVis.JvValidateEdit3Exit(Sender: TObject);
  begin
  Chart1.BottomAxis.AutomaticMinimum := false;
  Chart1.BottomAxis.Minimum := JvValidateEdit3.Value;
  end;

//-------------------------------------------------------------------------------------------------

procedure TFmChVis.JvValidateEdit4Exit(Sender: TObject);
  begin
  Chart1.BottomAxis.AutomaticMaximum := false;
  Chart1.BottomAxis.Maximum := JvValidateEdit4.Value;
  end;

//-------------------------------------------------------------------------------------------------

procedure TFmChVis.LabelChange(Sender : TObject);
  begin
  if Sender is TEdit then
     Chart1.Series[(Sender as TEdit).Tag].Title := (Sender as TEdit).Text;
  end;

//-------------------------------------------------------------------------------------------------

procedure TFmChVis.LineChange(Sender : TObject);
  begin
  if Sender is TCheckBox then
     Chart1.Series[(Sender as TCheckBox).Tag].Visible := (Sender as TCheckBox).Checked;
  end;

//-------------------------------------------------------------------------------------------------

procedure TFmChVis.Panel3Click(Sender: TObject);
  begin
  ColorDialog1.Color := Panel3.Color;
  if ColorDialog1.Execute then
     begin
     Panel3.Color := ColorDialog1.Color;
     Chart1.Color := ColorDialog1.Color;
     end;
  end;

//-------------------------------------------------------------------------------------------------

procedure TFmChVis.PointChange(Sender : TObject);
  begin
  if Sender is TCheckBox then
     begin
     if (Sender as TCheckBox).Checked then
        begin
        (Chart1.Series[(Sender as TCheckBox).Tag] as TLineSeries).Pointer.Visible := true;
        (Chart1.Series[(Sender as TCheckBox).Tag] as TLineSeries).Pointer.Size := 2;
        (Chart1.Series[(Sender as TCheckBox).Tag] as TLineSeries).Pointer.Style := psCircle;
        (Chart1.Series[(Sender as TCheckBox).Tag] as TLineSeries).OnClickPointer := ClickPointer;
        end
     else
        begin
        (Chart1.Series[(Sender as TCheckBox).Tag] as TLineSeries).Pointer.Visible := false;
        end
     end;
  end;

//-------------------------------------------------------------------------------------------------

procedure TFmChVis.SaveasEMF1Click(Sender: TObject);
  begin
  if Chart1.Title.Text.Count > 0 then
     SaveDialog3.FileName := Chart1.Title.Text.Strings[0]
  else
     SaveDialog3.FileName := '';

  if SaveDialog3.Execute then
     Chart1.SaveToMetafileEnh(SaveDialog3.FileName);
  end;

//-------------------------------------------------------------------------------------------------

procedure TFmChVis.SaveasJPEG1Click(Sender: TObject);
  begin
  if Chart1.Title.Text.Count > 0 then
     SaveDialog2.FileName := Chart1.Title.Text.Strings[0]
  else
     SaveDialog2.FileName := '';
  if SaveDialog2.Execute then
     Chart1.SaveToBitmapFile(SaveDialog2.FileName);
  end;

//-------------------------------------------------------------------------------------------------

procedure TFmChVis.SaveasWMV1Click(Sender: TObject);
  begin
  if Chart1.Title.Text.Count > 0 then
     SaveDialog1.FileName := Chart1.Title.Text.Strings[0]
  else
     SaveDialog1.FileName := '';
  if SaveDialog1.Execute then
     Chart1.SaveToMetafile(SaveDialog1.FileName);
  end;

//-------------------------------------------------------------------------------------------------

procedure TFmChVis.ShowRightBar1Click(Sender: TObject);
  begin
  ShowRightBar1.Checked := not ShowRightBar1.Checked;
  Panel1.Visible := ShowRightBar1.Checked;
  if ShowRightBar1.Checked then
     Panel1.Width := 200
  else
     Panel1.Width := 0;
  end;

//-------------------------------------------------------------------------------------------------

end.
