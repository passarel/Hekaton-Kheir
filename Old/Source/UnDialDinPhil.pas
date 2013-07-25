{
  @abstract(Unit with the interface to the Dining Philosophers Experiment)
  @author(Rafa "Passarel" Borges <passarel@gmail.com>)
  @created(Porto Alegre, 2006)
  @lastmod(London, January, 2010)
}


unit UnDialDinPhil;

//------------------------------------------------------------------------------------------------

interface

//------------------------------------------------------------------------------------------------

uses
  Chart, Classes, Controls, Dialogs, ExtCtrls, ExtDlgs, Forms, Gauges,
  Graphics, Grids, Menus, Messages, Series, Spin, StdCtrls, SysUtils,
  TeEngine, TeeProcs, UnMCDiningTable, Variants, Windows, TeeFunci;
  
//------------------------------------------------------------------------------------------------


type TFmPhil = class(TForm)
    Button1: TButton;
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Chart1: TChart;
    Chart2: TChart;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    ComboBox4: TComboBox;
    ComboBox5: TComboBox;
    ComboBox6: TComboBox;
    ComboBox7: TComboBox;
    Configurar1: TMenuItem;
    Corel1: TMenuItem;
    Delete1: TMenuItem;
    Gauge1: TGauge;
    GroupBox1: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LineSeries1: TLineSeries;
    ListBox1: TListBox;
    MainMenu1: TMainMenu;
    OpenDialog1: TOpenDialog;
    OpenDialog2: TOpenDialog;
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    SaveDialog1: TSaveDialog;
    Series2: TMenuItem;
    Series3: TLineSeries;
    Series4: TLineSeries;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    SpinEdit3: TSpinEdit;
    SpinEdit4: TSpinEdit;
    SpinEdit5: TSpinEdit;
    StringGrid1: TStringGrid;
    procedure Button1Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure ComboBox3Change(Sender: TObject);
    procedure ComboBox4Change(Sender: TObject);
    procedure ComboBox4Enter(Sender: TObject);
    procedure Configurar1Click(Sender: TObject);
    procedure Corel1Click(Sender: TObject);
    procedure Delete1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure QuandoClicar(Sender : TObject);
    procedure SpinEdit1Change(Sender: TObject);

  private
    FNetsFiles : TStringList;
    FNetsIndex : array of integer;
    FDiningTable : TDiningTable;
    FNoProperties : boolean;

    FCurrentAgent : integer;
    FCancel : boolean;

    FTotalExecutions : integer;
    FCurrentExecution : integer;
    procedure CheckClick(Sender : TObject);
    function convert(x : integer) : double;
    procedure StepEvent(TimePoint, MetricsLength : integer; Metrics : array of double);
    function UpdateAgents : boolean;

  public
    { Public declarations }
  end;

//------------------------------------------------------------------------------------------------

var
  FmPhil: TFmPhil;

//------------------------------------------------------------------------------------------------

implementation

uses Math, UnMCPhilosopher;

{$R *.dfm}

//------------------------------------------------------------------------------------------------

procedure TFmPhil.Button1Click(Sender: TObject);
//  j, k : integer;
//    st : TStringList;

  var
    i, j : integer;
    XNetsIndex : array of Integer;
    XFilename : TFileName;
    XHour, XMin, XSec, XMil : word;

  begin
  UpdateAgents;

  for i := Panel1.ControlCount - 1 downto 0 do
    if Panel1.Controls[i] is TCheckBox then
       Panel1.Controls[i].Free;

  FCancel := false;

  Button5.Enabled := false;
  StringGrid1.Enabled := false;
  Button1.Enabled := false;
  Button11.Enabled := false;
  Label4.Enabled := false;
  Label10.Enabled := false;
  ComboBox5.Enabled := false;
  ComboBox6.Enabled := false;
  SpinEdit5.Enabled := false;

  Button3.Enabled := true;
  Gauge1.visible := true;

  for j := (Chart1.SeriesCount - 1) downto 0 do
    Chart1.Series[j].Free;

  if ComboBox5.ItemIndex = 4 then
     begin
     SetLength(XNetsIndex, SpinEdit1.Value);
     FTotalExecutions := FNetsFiles.Count;
     FCurrentExecution := 0;
     while not FCancel and (FCurrentExecution < FTotalExecutions) do
       begin
       FDiningTable.Free;
       XNetsindex[0] := FCurrentExecution;
       for j := 1 to SpinEdit1.Value - 1 do
         XNetsIndex[j] := FNetsIndex[j];

       DecodeTime(time, XHour, XMin, XSec, XMil);
       FDiningTable := TDiningTable.CreatePar(SpinEdit1.Value, FNetsFiles, XNetsIndex, SpinEdit4.Value/100,
                                       TAllocationPolicy(ComboBox1.ItemIndex));
       FDiningTable.StepEvent := StepEvent;
       XFileName := ChangeFileExt(FNetsFiles.Strings[FCurrentExecution], '_' + Inttostr(XHour) + '-' + InttoStr(XMin) + '.rrf');
       XFileName := ExtractFilePath(XFileName) + '\LT' + inttostr(ComboBox7.ItemIndex + 1) + '_' + ExtractFileName(XFileName);
       FDiningTable.BlackBoxOnline(SpinEdit5.Value, FNetsFiles.Strings[ComboBox6.Itemindex], XFileName, ComboBox7.ItemIndex);
       FCurrentExecution := FCurrentExecution + 1;
       end;
     end
  else if SaveDialog1.Execute then
     begin
     FCurrentExecution := 0;
     FTotalExecutions := 1;
     case ComboBox5.ItemIndex of
       0 : FDiningTable.BlackBoxOnline(SpinEdit5.Value, FNetsFiles.Strings[ComboBox6.ItemIndex], SaveDialog1.FileName, 0);
       1 : FDiningTable.BlackBoxOnline(SpinEdit5.Value, FNetsFiles.Strings[ComboBox6.ItemIndex], SaveDialog1.FileName, 1);
       2 : FDiningTable.IndividualEvolution(SpinEdit5.Value, SaveDialog1.FileName);
       3 : FDiningTable.OverallEvolution(SpinEdit5.Value, SaveDialog1.FileName);
       end;
     FCurrentExecution := 1;
     end;
  Button5.Enabled := true;
  StringGrid1.Enabled := true;
  Button1.Enabled := true;
  Button11.Enabled := true;
  Label4.Enabled := true;
  Label10.Enabled := true;
  ComboBox5.Enabled := true;
  ComboBox6.Enabled := true;
  SpinEdit5.Enabled := true;

  Button3.Enabled := false;
  Gauge1.Visible := false;

  for i := 0 to Chart1.SeriesCount - 1 do
    with TCheckBox.Create(self) do
      begin
      Parent := Panel1;
      top := 8;
      left := (i + 2) * 24;
      Width := 17;
      Height := 17;
      Caption := '';
      Tag := i;
      checked := true;
      OnClick := CheckClick;
      end;
  end;

//------------------------------------------------------------------------------------------------

procedure TFmPhil.Button10Click(Sender: TObject);
//var  i : integer;
 begin
{  Series5.AddXY(0, 50);
  Series5.AddXY(500, 50);
  Series6.AddXY(0, 10);
  Series6.AddXY(500, 10);
  Series7.AddXY(0, 5);
  Series7.AddXY(500, 5);}
  end;

//------------------------------------------------------------------------------------------------

procedure TFmPhil.Button11Click(Sender: TObject);
  begin
  UpdateAgents;
  FDiningTable.GetAgent(FCurrentAgent).AddProperty([]);
  FDiningTable.GetAgent(FCurrentAgent).GetGrid(StringGrid1);
  end;

//------------------------------------------------------------------------------------------------

procedure TFmPhil.Button12Click(Sender: TObject);
  begin
  if Button12.Tag = 0 then
     begin
     Height := 750;
     Chart1.Visible := true;
     Panel1.Visible := true;
     Button12.Tag := 1;
     end
  else
     begin
     Height := 476;
     Chart1.Visible := false;
     Panel1.Visible := false;
     Button12.Tag := 0;
     end;
  end;

//------------------------------------------------------------------------------------------------

procedure TFmPhil.Button2Click(Sender: TObject);
  var i : integer;
  begin
  if OpenDialog1.Execute then
     FNetsFiles.Add(OpenDialog1.FileName);
  ListBox1.Items.Clear;
  ComboBox3.Items.Clear;
  ComboBox6.Items.Clear;

  for i := 0 to FNetsFiles.Count - 1 do
    begin
    ListBox1.Items.Add(ExtractFileName(FNetsFiles.Strings[i]));
    ComboBox3.Items.Add(ExtractFileName(FNetsFiles.Strings[i]));
    ComboBox6.Items.Add(ExtractFileName(FNetsFiles.Strings[i]));
    end;
  end;

//------------------------------------------------------------------------------------------------

procedure TFmPhil.Button3Click(Sender: TObject);
  begin
  FDiningTable.CancelExperiment;
  FCancel := true;

  Button5.Enabled := true;
  StringGrid1.Enabled := true;
  Button1.Enabled := true;
  Button11.Enabled := true;
  Label4.Enabled := true;
  Label10.Enabled := true;
  ComboBox5.Enabled := true;
  ComboBox6.Enabled := true;
  SpinEdit5.Enabled := true;

  Button3.Enabled := false;
  Gauge1.Visible := false;
  end;

//------------------------------------------------------------------------------------------------

procedure TFmPhil.Button4Click(Sender: TObject);
  var i : integer;
  begin
  FDiningTable := TDiningTable.CreatePar(SpinEdit1.Value, FNetsFiles, FNetsIndex, SpinEdit4.Value/100,
                                     TAllocationPolicy(ComboBox1.ItemIndex));
  FDiningTable.StepEvent := StepEvent;
  Button4.Enabled := false;
  ListBox1.Enabled := false;
  Button2.Enabled := false;
  ComboBox3.Enabled := false;
  SpinEdit1.Enabled := false;
  ComboBox1.Enabled := false;
  SpinEdit4.Enabled := false;

  Button5.Enabled := true;
  StringGrid1.Enabled := true;
  Button1.Enabled := true;
  ComboBox4.Enabled := true;
  Button11.Enabled := true;

  FDiningTable.GetAgent(FCurrentAgent).GetGrid(StringGrid1);
  ComboBox4.Items.Clear;
  for i := 0 to FDiningTable.GetAgent(FCurrentAgent).GetPropertyCount - 1 do
    ComboBox4.Items.Add('Property ' + inttostr(i + 1));
  ComboBox4.ItemIndex := FDiningTable.GetAgent(FCurrentAgent).CurrentProperty;
  end;

//------------------------------------------------------------------------------------------------

procedure TFmPhil.Button5Click(Sender: TObject);
  begin
  FDiningTable.Free;
  Button4.Enabled := true;
  ListBox1.Enabled := true;
  Button2.Enabled := true;
  ComboBox3.Enabled := true;
  SpinEdit1.Enabled := true;
  ComboBox1.Enabled := true;
  SpinEdit4.Enabled := true;
  Button5.Enabled := false;
  StringGrid1.Enabled := false;
  Button1.Enabled := false;
  ComboBox4.Enabled := false;
  Button11.Enabled := false;
  end;

//------------------------------------------------------------------------------------------------

procedure TFmPhil.Button6Click(Sender: TObject);
  var
    t : tLineSeries;
    data : array of array of double;
    Z : textFile;
    k, i, j, c, max, weat, deat: integer;
    b : boolean;
    v : double;
  begin
  if opendialog2.Execute then
    begin
    max := 100000;
    for i := Chart1.SeriesCount - 1 downto 0 do
      Chart1.Series[i].Free;

    if RadioButton1.Checked then
       begin
       //configure error chart
       Chart1.LeftAxis.Automatic := false;
       Chart1.LeftAxis.Minimum := 0;
       Chart1.LeftAxis.Maximum := 1;
       Chart1.LeftAxis.Increment := 0.001;
//       Chart2.LeftAxis.Automatic := false;
//       Chart2.LeftAxis.Minimum := 0;
//       Chart2.LeftAxis.Maximum := 1.0;
//       Chart2.LeftAxis.Increment := 0.1;
       end
    else if RadioButton3.Checked then
       begin
       //configure allocation chart
       Chart1.LeftAxis.Automatic := false;
       Chart1.LeftAxis.Minimum := 0;
       Chart1.LeftAxis.Maximum := 0.5;
       Chart1.LeftAxis.Increment := 0.1;
       Chart2.LeftAxis.Automatic := false;
       Chart2.LeftAxis.Minimum := 0;
       Chart2.LeftAxis.Maximum := 0.5;
       Chart2.LeftAxis.Increment := 0.1;
       end
    else
       begin
       Chart1.LeftAxis.Automatic := true;
       end;

//    if checkbox3.Checked then
//       Chart1.BottomAxis.Increment := 0.1;

    SetLength(data, SpinEdit1.Value);
    for i := 0 to SpinEdit1.Value - 1 do
       SetLength(Data[i], max);

    AssignFile(Z, OpenDialog2.FileName);
    reset(z);
    i := 0;
    b := radiobutton1.Checked;
    while (not eof(z)) and (i < max) do
      begin
      j := 0;
      while (not eof(z)) and (j < SpinEdit1.Value) do
        begin
        read(z, data[j, i]);
        if b then j := j + 1;
        b := not b;
        end;
      i := i + 1;
      end;

    c := 0;
    if radiobutton2.Checked then
       begin
       for j := 0 to SpinEdit1.Value - 1 do
         for i := 1 to max - 2 do
           begin
           if (data[j, i - 1] > 0) and (data[j, i - 1] <= data[j, i]) then
              c := c + 1
           else
              c := 0;
           data[j, i - 1] := c;
           end
       end;

    if Radiobutton3.checked then
       begin
       for i := 1 to max - 1 do
         for j := 0 to SpinEdit1.Value - 1 do
           begin
           if (data[j, i - 1] > 0) and (data[j, i - 1] > data[j, i]) then deat := 1 else deat := 0;
           if (data[j, i - 1] > 0) then  weat := 1 else weat := 0;
           data[j, i - 1] := (deat * 10000) + weat;
           end;
         end;

     if checkbox3.Checked then
         begin
         for k := 0 to SpinEdit1.Value - 1 do
           for i := 0 to (max - 1) div SpinEdit3.Value do
             begin
             c := 0;
             v := 0;
             for j := 0 to SpinEdit3.Value - 1 do
               begin
               if (i * SpinEdit3.Value) + j < max then
                 begin
                 if radiobutton3.Checked then
                    begin
                    if convert(round(data[k, (i * SpinEdit3.Value) + j])) >= 0 then
                       begin
                       c := c + 1;
                       v := v + convert(round(data[k, (i * SpinEdit3.Value) + j]));
                       end
                    end
                 else
                    begin
                    c := c + 1;
                    v := v + data[k, (i * SpinEdit3.Value) + j];
                    end;
                 end;
               end;
             if radiobutton3.Checked then
                data[k, i] := (v * 10000) + c
             else
                data[k, i] := v/c;
             end;
         max := ((max - 1) div SpinEdit3.Value) + 1;
         end;

  if checkbox4.Checked then
     begin
     if checkbox1.Checked then
        begin
        t := TLineSeries.Create(Chart1);
        chart1.AddSeries(t);
        for i := 0 to max - 1 do
           begin
           v := 0;
           c := 0;
           for j := 0 to SpinEdit1.Value - 1 do
             begin
             if radiobutton3.Checked then
                begin
                if convert(round(data[j, i])) >= 0 then
                   begin
                   c := c + 1;
                   v := v + convert(round(data[j, i]));
                   end;
                end
             else
                begin
                c := c + 1;
                v := v + data[j, i];
                end;
             end;
           if c > 0 then
              t.AddXY(i, v/c)
           else
              t.AddXY(i, 0);
           end;
        end
     else
        for j := 0 to SpinEdit1.Value - 1 do
          begin
          t := TLineSeries.Create(Chart1);
          chart1.AddSeries(t);
          for i := 0 to max - 1 do
            if radiobutton3.Checked then
               t.AddXY(i, convert(round(data[j, i])))
            else
               t.AddXY(i, data[j, i]);
          end
       end;
     end;
  end;

//------------------------------------------------------------------------------------------------

procedure TFmPhil.Button7Click(Sender: TObject);
//  var i : integer;
  begin
{  Chart2.Series[conta].Clear;
  for i := 0 to Chart1.Series[0].Count - 1 do
    Chart2.Series[conta].AddXY(i, Chart1.Series[0].YValues[i] {* 1000}{);
  Conta := (Conta + 1) mod 3;}
  end;

//------------------------------------------------------------------------------------------------

procedure TFmPhil.Button8Click(Sender: TObject);
  var i : integer;
     tmp : tmenuItem;
  begin
  chart1.Visible := true;
  GroupBox1.Visible := false;
  for i := Series2.Count - 1 downto 0 do
    Series2.Items[i].Free;
  for i := 0 to Chart1.SeriesCount - 1 do
    begin
    tmp := TMenuItem.Create(self);
    tmp.Checked := true;
    tmp.Tag := i;
    tmp.OnClick := QuandoCLicar;
    Series2.Add(tmp);
    end;
  //  for i := Chart1.SeriesCount - 1 downto 1 do
//    Chart1.Series[i].Free;
  end;

//------------------------------------------------------------------------------------------------

procedure TFmPhil.Button9Click(Sender: TObject);
  begin
  Chart2.SaveToMetafile('UmGraficoLegal.wmf');
  end;

//------------------------------------------------------------------------------------------------

procedure TFmPhil.CheckClick(Sender: TObject);
  begin
  if sender is TCheckBox then
     Chart1.Series[(Sender as TCheckBox).Tag].Visible := (Sender as TCheckBox).Checked;
  end;

//------------------------------------------------------------------------------------------------

procedure TFmPhil.ComboBox2Change(Sender: TObject);
  var i : integer;
  begin
  if Button1.Enabled then
     UpdateAgents;

  if (ComboBox2.ItemIndex >= 0) and (ComboBox2.ItemIndex < length(FNetsIndex)) then
     FCurrentAgent := ComboBox2.ItemIndex
  else
     FCurrentAgent := -1;

  if (FNetsIndex[FCurrentAgent] < ComboBox2.Items.Count) then
     ComboBox3.ItemIndex := FNetsIndex[FCurrentAgent]
  else
     ComboBox3.ItemIndex := 0;

  if Button1.Enabled then
     begin
     FDiningTable.GetAgent(FCurrentAgent).GetGrid(StringGrid1);
     ComboBox4.Items.Clear;
     for i := 0 to FDiningTable.GetAgent(FCurrentAgent).GetPropertyCount - 1 do
       ComboBox4.Items.Add('Property ' + inttostr(i + 1));
     ComboBox4.ItemIndex := FDiningTable.GetAgent(FCurrentAgent).CurrentProperty;
     end;
  end;

//------------------------------------------------------------------------------------------------

procedure TFmPhil.ComboBox3Change(Sender: TObject);
  begin
  FNetsIndex[FCurrentAgent] := ComboBox3.ItemIndex
  end;

//------------------------------------------------------------------------------------------------

procedure TFmPhil.ComboBox4Change(Sender: TObject);
  begin
  if (ComboBox4.ItemIndex >= 0) and
     (ComboBox4.ItemIndex < FDiningTable.GetAgent(FCurrentAgent).GetPropertyCount) then
        FDiningTable.GetAgent(FCurrentAgent).CurrentProperty := ComboBox4.ItemIndex;
  end;

//------------------------------------------------------------------------------------------------

procedure TFmPhil.ComboBox4Enter(Sender: TObject);
  begin
  UpdateAgents;
  end;

//------------------------------------------------------------------------------------------------

procedure TFmPhil.Configurar1Click(Sender: TObject);
  begin
  Chart1.Visible := false;
  Groupbox1.Visible := true;
  end;

//------------------------------------------------------------------------------------------------

function TFmPhil.convert(x: integer): double;
  begin
  if x mod 10000 = 0 then
     result := -1
  else
     result := (x div 10000) / (x mod 10000);
  end;

//------------------------------------------------------------------------------------------------

procedure TFmPhil.Corel1Click(Sender: TObject);
   begin
  chart1.SaveToMetafile(OpenDialog2.filename + '.wmf');
  end;

//------------------------------------------------------------------------------------------------

procedure TFmPhil.Delete1Click(Sender: TObject);
  var i : integer;
  begin
  If ListBox1.ItemIndex >= 0 then
     FNetsFiles.Delete(listbox1.ItemIndex);
  ListBox1.Items.Clear;
  ComboBox3.Items.Clear;
  ComboBox6.Items.Clear;
  for i := 0 to FNetsFiles.Count - 1 do
    begin
    ListBox1.Items.Add(ExtractFileName(FNetsFiles.Strings[i]));
    ComboBox3.Items.Add(ExtractFileName(FNetsFiles.Strings[i]));
    ComboBox6.Items.Add(ExtractFileName(FNetsFiles.Strings[i]));
    end;
  end;

//------------------------------------------------------------------------------------------------

procedure TFmPhil.FormCreate(Sender: TObject);
  begin
//  Conta := 0;
  FNetsFiles := TStringList.Create;
  end;

//------------------------------------------------------------------------------------------------

procedure TFmPhil.QuandoClicar(Sender: TObject);
  var i : integer;
  begin
  (Sender as TMenuItem).Checked := not (Sender as TMenuItem).Checked;
  for i := 0 to Series2.Count - 1 do
    Chart1.Series[i].Active := Series2.Items[i].Checked;
  end;

//------------------------------------------------------------------------------------------------

procedure TFmPhil.SpinEdit1Change(Sender: TObject);
  var i : integer;
  begin
  if trim(SpinEdit1.Text) <> '' then
     begin
     ComboBox2.Clear;
     for i := 0 to SpinEdit1.Value - 1 do
       ComboBox2.Items.Add('Agent ' + inttostr(i + 1));
     SetLength(FNetsIndex, SpinEdit1.Value);
     end;
  end;

//------------------------------------------------------------------------------------------------

procedure TFmPhil.StepEvent(TimePoint, MetricsLength: integer;  Metrics: array of double);
  var
    i : integer;
  begin
  if (MetricsLength * (FCurrentExecution + 1)) > Chart1.SeriesCount then
     for i := Chart1.SeriesCount to (MetricsLength * (FCurrentExecution + 1)) - 1 do
       Chart1.AddSeries(TLineSeries.Create(self));
  for i := 0 to MetricsLength - 1 do
    Chart1.Series[(FCurrentExecution * MetricsLength) + i].AddXY(TimePoint, Metrics[i]);
  i := (FCurrentExecution * SpinEdit5.Value) + TimePoint;
  Gauge1.Progress := (i * 100) div (SpinEdit5.Value * FTotalExecutions);
  end;

//------------------------------------------------------------------------------------------------

function TFmPhil.UpdateAgents: boolean;
  var
    i, j : integer;
    XAgent : TPhilosopher;
  begin
  XAgent := FDiningTable.GetAgent(FCurrentAgent);
  i := XAgent.GetPropertyCount;

  FNoProperties := (trim(StringGrid1.cells[0, 0]) = '');

  ComboBox4.Items.Clear;

  if not FNoProperties then
     begin
     while (i < StringGrid1.RowCount) do
       begin
       XAgent.AddProperty([]);
       i := i + 1;
       end;
     for i := 0 to StringGrid1.RowCount - 1 do
       begin
       for j := 0 to XAgent.GetInputCount - 1 do
         XAgent.SpecProperty[i, j] := strtoint(StringGrid1.Cells[j + 1, i]);
       for j := 0 to XAgent.GetOutputCount - 1 do
         XAgent.SpecProperty[i, j + XAgent.GetInputCount] := strtoint(StringGrid1.Cells[j + XAgent.GetInputCount + 2, i]);
       ComboBox4.Items.Add('Property ' + inttostr(i + 1));
       end;
     end;

  result := true;
  end;

//------------------------------------------------------------------------------------------------

end.



