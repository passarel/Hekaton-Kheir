{
  @abstract(Unit that describes the interface to configure the input dataset for network training)
  @author(Rafa "Passarel" Borges <passarel@gmail.com)
  @created(Porto Alegre, 2006)
  @lastmod(London, January, 2010)
}


unit UnDialFile;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, Menus, Spin, ComCtrls, Impstringgrid;

type
  TFmFileConf = class(TForm)
    OpenDialog1: TOpenDialog;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    GroupBox1: TGroupBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    GroupBox2: TGroupBox;
    CheckBox7: TCheckBox;
    SpinEdit1: TSpinEdit;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    Button1: TButton;
    TabSheet2: TTabSheet;
    Label3: TLabel;
    GroupBox3: TGroupBox;
    SpinEdit2: TSpinEdit;
    CheckBox8: TCheckBox;
    ImpStringgrid1: TImpStringgrid;
    ScrollBox2: TScrollBox;
    CheckBox9: TCheckBox;
    GroupBox4: TGroupBox;
    ScrollBox1: TScrollBox;
    SaveDialog1: TSaveDialog;
    GroupBox5: TGroupBox;
    ScrollBox3: TScrollBox;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    PopupMenu1: TPopupMenu;
    Randomize1: TMenuItem;
    Rotate1: TMenuItem;
    CheckBox2: TCheckBox;
    CheckBox1: TCheckBox;
    Button5: TButton;
    GroupBox6: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    CheckBox3: TCheckBox;
    SpinEdit3: TSpinEdit;
    SpinEdit4: TSpinEdit;
    SpinEdit5: TSpinEdit;
    Button6: TButton;
    Button7: TButton;
    TabSheet3: TTabSheet;
    GroupBox7: TGroupBox;
    ListBox1: TListBox;
    GroupBox8: TGroupBox;
    RadioButton3: TRadioButton;
    CheckBox4: TCheckBox;
    RadioButton4: TRadioButton;
    SpinEdit6: TSpinEdit;
    RadioButton5: TRadioButton;
    Label5: TLabel;
    StringGrid1: TStringGrid;
    Button8: TButton;
    CheckBox10: TCheckBox;
    CheckBox11: TCheckBox;
    CheckBox12: TCheckBox;
    GroupCentre1: TMenuItem;
    FilterConflicts1: TMenuItem;
    procedure CheckBox6Click(Sender: TObject);
    procedure CheckBox5Click(Sender: TObject);
    procedure CheckBox7Click(Sender: TObject);
    procedure InputCheck(Sender: TObject);
    procedure OutputCheck(Sender: TObject);
    procedure BtColClick(Sender: TObject);
    procedure RadioButton1Click(Sender: TObject);
    procedure RadioButton2Click(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
    procedure CheckBox9Click(Sender: TObject);
    procedure CheckBox8Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Randomize1Click(Sender: TObject);
    procedure Rotate1Click(Sender: TObject);
    procedure UpdateArrays;
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure SpinEdit6Change(Sender: TObject);
    procedure RadioButton3Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure RadioButton4Click(Sender: TObject);
    procedure RadioButton5Click(Sender: TObject);
    procedure GroupCentre1Click(Sender: TObject);
    procedure FilterConflicts1Click(Sender: TObject);


  private
    FLabels : TStringList;
    FMax    : array of double;
    FMin    : array of double;

    FInput  : array of boolean;
    FOutput : array of boolean;
    FDelay  : array of integer;

    FGroups : array of integer;
    FGroupsCount : integer;
    FMaxGroupSize : integer;

    WillBeOK : boolean;
    FTmpName : string;

    FInsideUpdate : boolean;

    FSimilaritiesX : integer;
    FSimilaritiesY : integer;
    FSimilarities : array of array of TCheckBox;
    FMainOutputs : array of TCheckBox;

    FSelCols : array of integer;

    function IsEmptyRow(rowIndex : integer) : boolean;

    function BeforeShow : boolean;
    procedure AfterShow;

    procedure AutoFillIO(CutEqual : boolean; LeftNull, RightOut, MinDiff : integer);
    procedure UpdateNotFixed;
    procedure UpdateListOfColumns;

    procedure FormGroups;
    procedure VisualUpdate;
    procedure UpdateColumns;
    procedure UpdateSimilarities(x, y : integer);
    procedure UpdateMainOutputs(par : integer);

    procedure GenerateSubsets;
    procedure GroupToArray(var init : integer; group : integer; var ar : array of double);
    procedure RandomizeGroups(n : integer; out m : array of integer);

  public

    function Execute : string;
    { Public declarations }
  end;

var
  FmFileConf: TFmFileConf;

implementation

uses UnDialColumn, math, UnRafaAux2007, UnLineComp;

{$R *.dfm}

function TFmFileConf.Execute : string;
  begin
  WillBeOk := false;
  if BeforeShow then
     begin
     VisualUpdate;
     ShowModal;
     end;
  if WillbeOK then
     begin
     AfterShow;
     result := FTmpName;
     end
  else
     result := '';
  end;

procedure TFmFileConf.CheckBox6Click(Sender: TObject);
  var i, j : integer;
  begin
  if not CheckBox5.Checked then
     begin
     if CheckBox6.Checked then
        begin
         for i := 0 to ImpStringGrid1.ColCount - 1 do
           for j := 0 to ImpStringGrid1.RowCount - 2 do
             ImpStringGrid1.Cells[i, j] := ImpStringGrid1.Cells[i, j + 1];
         ImpStringGrid1.RowCount := ImpStringGrid1.RowCount - 1;
        end
     else
        begin
        ImpStringGrid1.RowCount := ImpStringGrid1.RowCount + 1;
        for i := 0 to ImpStringGrid1.ColCount - 1 do
          begin
          for j := ImpStringGrid1.RowCount - 1 downto 1 do
            ImpStringGrid1.Cells[i, j] := ImpStringGrid1.Cells[i, j - 1];
//          ImpStringGrid1.Cells[i, 0] := '';
          end;

        end;
     end;
  visualUpdate;
  end;

procedure TFmFileConf.VisualUpdate;
  var i : integer;
  begin
  if CheckBox6.Checked then
     begin
     ImpStringGrid1.FixedRows := 0;
     CheckBox5.Enabled := false;
     end
  else
     begin
     ImpStringGrid1.FixedRows := 1;

     for i := 1 to ImpStringGrid1.ColCount - 1 do
       ImpStringGrid1.Columns.Items[i].Title.Caption := FLabels.Strings[i - 1];
//       ImpStringGrid1.Cells[i, 0]  := FLabels.Strings[i - 1];
     CheckBox5.Enabled := true;
     end;

  SpinEdit1.MaxValue := ImpStringGrid1.RowCount - ImpStringGrid1.FixedRows;

  RadioButton1.enabled := checkBox7.Checked;
  RadioButton2.Enabled := checkBox7.Checked;
  SpinEdit1.Enabled    := checkBox7.Checked and RadioButton1.Checked;


  if checkBox7.Checked then
     if RadioButton2.Checked then
        begin
        UpdateSimilarities(FLabels.Count, 2);
        UpdateMainOutputs(0);
        end
     else
        begin
        UpdateSimilarities(FLabels.Count, SpinEdit1.Value);
        UpdateMainOutputs(SpinEdit1.Value);
        end
  else
     begin
     UpdateSimilarities(FLabels.Count, 1);
     UpdateMainOutputs(-1);
     end;


  for i := 1 to ImpStringGrid1.ColCount - 1 do
    if length(FInput) > 0 then
       begin
       if FInput[i - 1] then
         if FOutput[i - 1] then
            ImpStringGrid1.Columns.Items[i].Color := clMoneyGreen
         else
            ImpStringGrid1.Columns.Items[i].Color := clSkyBlue
       else
         if FOutput[i - 1] then
            ImpStringGrid1.Columns.Items[i].Color := clCream
         else
            ImpStringGrid1.Columns.Items[i].Color := clwindow;
       end;

  FormGroups;

   SpinEdit2.MinValue := 1;
   SpinEdit2.MaxValue := FGroupsCount;
   if CheckBox9.Checked then
      SpinEdit2.Value := SpinEdit2.MaxValue;

   SpinEdit2.Enabled := not CheckBox9.Checked;
   CheckBox8.Enabled := not CheckBox9.Checked;

   ScrollBox2.Enabled := (not CheckBox9.Checked) and CheckBox8.Checked;

//  if ImpStringGrid1.FixedRows = 0 then
//     ImpStringGrid1.Columns[0].Title.Caption := inttostr(FGroups[0]);
//  for i := 0 to length(FGroups) - 1 do
//    ImpStringGrid1.Cells[0, i + ImpStringGrid1.FixedRows] := inttostr(FGroups[i]);



  ImpStringGrid1.Repaint;

  UpdateColumns;
  UpdateListOfColumns;
  end;

procedure TFmFileConf.CheckBox5Click(Sender: TObject);
   var i, j : integer;
   begin
   if CheckBox5.Checked then
      begin
      for i := 0 to ImpStringGrid1.ColCount - 1 do
        begin
        for j := 0 to ImpStringGrid1.RowCount - 2 do
          ImpStringGrid1.Cells[i, j] := ImpStringGrid1.Cells[i, j + 1];
        if i > 0 then
           FLabels[i - 1] := ImpStringGrid1.Cells[i, 0];
        end;
      ImpStringGrid1.RowCount := ImpStringGrid1.RowCount - 1;
      CheckBox6.Enabled := false;
      end
   else
      begin
      ImpStringGrid1.RowCount := ImpStringGrid1.RowCount + 1;
      for i := 0 to ImpStringGrid1.ColCount - 1 do
        for j := ImpStringGrid1.RowCount - 1 downto 1 do
          ImpStringGrid1.Cells[i, j] := ImpStringGrid1.Cells[i, j - 1];
      checkbox6.Enabled := true;
      end;
   VisualUpdate;
   end;

procedure TFmFileConf.CheckBox7Click(Sender: TObject);
  begin
  VisualUpdate
  end;

procedure TFmFileConf.UpdateColumns;
  var i, j : integer;
      inp : boolean;
      s : string;
  begin

  if (FLabels.Count * 4) = ScrollBox1.ControlCount then
     begin
     j := 0;
     for i := 0 to ScrollBox1.ControlCount - 1 do
       if ScrollBox1.Controls[i] is TLabel then
          begin
          s := 'Column ' + inttostr(i + 1) + ' - ' + FLabels[j] + ': Values between ';
          s := s + floattostr(FMin[i]) + ' and ' + floattostr(FMax[i]);
          (ScrollBox1.Controls[i] as TLabel).Caption := s;
          j := j + 1;
          end;

     inp := true;
     j := 0;
     for i := 0 to ScrollBox1.ControlCount - 1 do
       if ScrollBox1.Controls[i] is TCheckBox then
          begin
          if inp then
             begin
             (ScrollBox1.Controls[i] as TCheckBox).Caption := 'Input';
             (ScrollBox1.Controls[i] as TCheckBox).Checked := FInput[j];
             inp := false;
             end
          else
             begin
             if FDelay[j] <= 0 then
                s := '(no delay)'
             else
                s := '(delay of ' + inttostr(FDelay[j]) + ' times)';
             (ScrollBox1.Controls[i] as TCheckBox).Caption := 'Output' + s;
             (ScrollBox1.Controls[i] as TCheckBox).Checked := FOutput[j];
             inp := true;
             j := j + 1;
             end;
          end;

     end;
  end;

procedure TFmFileConf.InputCheck(Sender: TObject);
  begin
  with (sender as TCheckBox) do
    FInput[tag] := checked;
  VisualUpdate;
  end;

procedure TFmFileConf.OutputCheck(Sender: TObject);
  begin
  with (sender as TCheckBox) do
    FOutput[tag] := checked;
  VisualUpdate;
  end;

procedure TFmFileConf.BtColClick(Sender: TObject);
  var
    XLabel : string;
    XInput, XOutput : boolean;
    XDelay : integer;
    XMin, XMax : double;
  begin
  XLabel  := FLabels[(Sender as TButton).tag];
  XInput  := FInput [(Sender as TButton).tag];
  XOutput := FOutput[(Sender as TButton).tag];
  XDelay  := FDelay [(Sender as TButton).tag];
  XMin    := FMin   [(Sender as TButton).tag];
  XMax    := FMax   [(Sender as TButton).tag];

  if FmColDial.execute(XLabel, XInput, XOutput, XDelay, XMin, XMax) then
     begin
     FLabels[(Sender as TButton).tag] := XLabel ;
     FInput [(Sender as TButton).tag] := XInput ;
     FOutput[(Sender as TButton).tag] := XOutput;
     FDelay [(Sender as TButton).tag] := XDelay ;
     FMin   [(Sender as TButton).tag] := XMin   ;
     FMax   [(Sender as TButton).tag] := XMax   ;
     end;

  VisualUpdate;
  end;

procedure TFmFileConf.FormGroups;
  var
    i, XRows, j, k : integer;
    b : boolean;
    XGroupSize : integer;
  begin
  XRows := ImpStringGrid1.RowCount - ImpStringGrid1.FixedRows;
  SetLength(FGroups, XRows);
  j := 0;
  k := -1;
  b := true;
  FMaxGroupSize := 0;
  XGroupSize := 0;
  for i := 0 to XRows - 1 do
    begin
    if trim(ImpStringGrid1.Cells[1, i + ImpStringGrid1.FixedRows]) = '' then
       begin
       FGroups[i] := -1;
       b := true;
       if XGroupSize > FMaxGroupSize then
          FMaxGroupSize := XGroupSize;
       XGroupSize := 0;
       end
    else
       begin
       if CheckBox7.Checked then
          if RadioButton1.Checked then
             begin
             FGroups[i] := j div SpinEdit1.Value;
             FGroupsCount := FGroups[i] + 1;
             end
          else
             begin
             if b then k := k + 1;
             FGroups[i] := k;
             FGroupsCount := k + 1;
             XGroupSize := XGroupSize + 1;
             end
       else
          begin
          FGroups[i] := j;
          FGroupsCount := j + 1;
          end;
       b := false;
       j := j + 1;
       end;
    end;
  if RadioButton1.Checked then
     FMaxGroupSize := SpinEdit1.Value;
  end;

procedure TFmFileConf.RadioButton1Click(Sender: TObject);
  begin
  VisualUpdate;
  end;

procedure TFmFileConf.RadioButton2Click(Sender: TObject);
  begin
  VisualUpdate;
  end;

procedure TFmFileConf.SpinEdit1Change(Sender: TObject);
  begin
  if trim(SpinEdit1.Text) <> '' then
     VisualUpdate;
  end;

procedure TFmFileConf.UpdateSimilarities(x, y: integer);
  var i, j : integer;
  begin
  if X > FSimilaritiesX then
     begin
     SetLength(FSimilarities, X, FSimilaritiesY);
     for i := FSimilaritiesX to X - 1 do
       for j := 0 to FSimilaritiesY - 1 do
         begin
         FSimilarities[i, j] := TCheckBox.Create(FmColDial);
         FSimilarities[i, j].Parent  := ScrollBox2;
         FSimilarities[i, j].Caption := '';
         FSimilarities[i, j].height  := 16;
         FSimilarities[i, j].width   := 16;
         FSimilarities[i, j].top     := 16 + (j * 32);
         FSimilarities[i, j].left    := 16 + (i * 32);
         FSimilarities[i, j].checked := false;
         end
     end
  else
     begin
     for i := X to FSimilaritiesX - 1 do
       for j := 0 to FSimilaritiesY - 1 do
         FSimilarities[i, j].Free;
     SetLength(FSimilarities, X, FSimilaritiesY);
     end;
  FSimilaritiesX := X;

  if Y > FSimilaritiesY then
     begin
     SetLength(FSimilarities, FSimilaritiesX, Y);
     for i := 0 to FSimilaritiesX - 1 do
       for j := FSimilaritiesY to Y - 1 do
         begin
         FSimilarities[i, j] := TCheckBox.Create(FmColDial);
         FSimilarities[i, j].Parent  := ScrollBox2;
         FSimilarities[i, j].Caption := '';
         FSimilarities[i, j].height  := 16;
         FSimilarities[i, j].width   := 16;
         FSimilarities[i, j].top     := 16 + (j * 32);
         FSimilarities[i, j].left    := 16 + (i * 32);
         FSimilarities[i, j].checked := false;
         end
     end
  else
     begin
     for i := 0 to FSimilaritiesX - 1 do
       for j := Y to FSimilaritiesY - 1 do
         FSimilarities[i, j].Free;
     SetLength(FSimilarities, FSimilaritiesX, Y);
     end;
  FSimilaritiesY := Y;
  end;



procedure TFmFileConf.GroupToArray(var init: integer; group: integer;
          var ar: array of double);
  var
    i, j, k, l : integer;
  begin
  j := 0;
  k := 0;
  l := 0;
  while (init < ImpStringGrid1.RowCount) and (FGroups[init - ImpStringGrid1.FixedRows] <= group) do
    begin
    if FGroups[init - ImpStringGrid1.FixedRows] = group then
       begin
       for i := 0 to FLabels.Count - 1 do
         if FSimilarities[i, j].Checked then
            begin
            ar[k] := strtofloat(ImpStringGrid1.Cells[i + 1, init]);
            k := k + 1;
            end;
       if (j < 1) or RadioButton1.checked or not CheckBox7.checked then
          j := j + 1
       else
          k := l;
       l := k;
       end;
    init := init + 1;
    end;

  end;



procedure TFmFileConf.GenerateSubsets;
  var arValues : array of array of double;
      arTemp   : array of Double;
      arGroups : array of integer;
      arCCount : array of integer;

//      XArGrSim  : array of Integer;
      XarGrSub  : array of integer;
      XNext     : array of integer;
//      XarSimSub : array of array of integer;
      CCount, SimCount : integer;
      i, j, k, l: integer;
      found : boolean;

  begin

  //Realize counting of checked boxes
  CCount := 0;
  for i := 0 to FSimilaritiesX - 1 do
    for j := 0 to FSimilaritiesY - 1 do
      if FSimilarities[i, j].Checked then
         CCount := CCount + 1;

  //Generate simmilarity classes
  SetLength(ArGroups, FGroupsCount);
  SetLength(ArTemp, CCount);
  SimCount := 0;
  i := ImpStringGrid1.FixedRows;
  k := 0;
  while i < ImpStringGrid1.RowCount do
    begin
    GroupToArray(i, k, arTemp);
    j := 0;
    found := false;
    while (j < SimCount) and not found do
      begin
      found := true;
      for l := 0 to CCount - 1 do
        found := found and (ArTemp[l] = ArValues[j, l]);
      j := j + 1;
      end;
    if not found then
       begin
       SimCount := SimCount + 1;
       SetLength(ArValues, SimCount, CCount);
       SetLength(ArCCount, SimCount);
       for l := 0 to CCount - 1 do
         ArValues[SimCount - 1, l] := arTemp[l];
       ArCCount[SimCount - 1] := 1;
       arGroups[k] := j;
       end
    else
       begin
       ArCCount[j - 1] := ArCCount[j - 1] + 1;
       arGroups[k] := j - 1;
       end;
    k := k + 1;
    end;

  //Fill initial values of XNext
  SetLength(XArGrSub, FGroupsCount);
  SetLength(XNext, SimCount);
  j := 0;
  XNext[0] := 0;
  for i := 0 to FGroupsCount - 1 do
    begin
    while arCCount[j] = 0 do
      begin
      j := j + 1;
      XNext[j] := i mod SpinEdit2.Value;
      end;
    arCCount[j] := arCCount[j] - 1;
    end;

  //Fill the array of subgroups
  for i := 0 to FGroupsCount - 1 do
    begin
    XArGrSub[i] := XNext[ArGroups[i]];
    XNext[ArGroups[i]] := (XNext[ArGroups[i]] + 1) mod SpinEdit2.value;
    end;

  //Passes the value to  the grid
  for i := ImpStringGrid1.FixedRows to ImpStringGrid1.RowCount - 1 do
    begin
    if FGroups[i - ImpStringGrid1.FixedRows] = -1 then
       ImpStringGrid1.Cells[0, i] := ''
    else
       ImpStringGrid1.Cells[0, i] := inttostr(XArGrSub[FGroups[i - ImpStringGrid1.FixedRows]] + 1);
    end;

  if ImpStringGrid1.FixedRows = 1 then
     ImpStringGrid1.Columns[0].Title.caption := ''
  else
     ImpStringGrid1.Columns[0].Title.caption := inttostr(XArGrSub[FGroups[0]] + 1);


  if checkBox7.Checked then
     if radioButton2.Checked then
        begin
        for i := ImpStringGrid1.FixedRows to ImpStringGrid1.RowCount - 1 do
          begin
          if (i = 0) or (ImpStringGrid1.Cells[0, i - 1] = '') then
             begin
             if not FMainOutputs[0].Checked then
                ImpStringGrid1.Cells[0, i] := inttostr(- strtoint(ImpStringGrid1.Cells[0, i]));
             end
          else if (i = ImpStringGrid1.RowCount - 1) or (ImpStringGrid1.Cells[0, i + 1] = '') then
             begin
             if not FMainOutputs[2].Checked then
                ImpStringGrid1.Cells[0, i] := inttostr(- strtoint(ImpStringGrid1.Cells[0, i]));
             end
          else
             begin
             if not FMainOutputs[1].Checked then
                ImpStringGrid1.Cells[0, i] := inttostr(- strtoint(ImpStringGrid1.Cells[0, i]));
             end;
          end;
        if ImpStringGrid1.FixedRows = 0 then
           if not FMainOutputs[0].Checked then
              ImpStringGrid1.columns[0].Title.caption :=
              inttostr(- strtoint(ImpStringGrid1.columns[0].Title.caption));
        end
     else
        begin
        for i := ImpStringGrid1.FixedRows to ImpStringGrid1.RowCount - 1 do
          begin
          if not FMainOutputs[(i - ImpStringGrid1.FixedRows) mod SpinEdit1.Value].Checked then
             ImpStringGrid1.Cells[0, i] :=
                 inttostr(- strtoint(ImpStringGrid1.Cells[0, i]));
          end;
        if ImpStringGrid1.FixedRows = 0 then
           if not FMainOutputs[0].Checked then
              ImpStringGrid1.columns[0].Title.caption :=
                    inttostr(- strtoint(ImpStringGrid1.columns[0].Title.caption));
        end;



{     Define quantos grupos de cada "similarity class" vai haver em cada subset
     Define os subsets de cada grupo}

  end;


procedure TFmFileConf.CheckBox9Click(Sender: TObject);
  begin

  VisualUpdate;
  end;

procedure TFmFileConf.CheckBox8Click(Sender: TObject);
  begin
  VisualUpdate;
  end;

procedure TFmFileConf.UpdateMainOutputs(par: integer);
  var i : integer;
  begin
  case par of
    -1 :
      begin
      for i := ScrollBox3.ControlCount - 1 downto 0 do
        ScrollBox3.Controls[i].free;
      GroupBox5.Enabled := false;
      SetLength(FMainOUtputs, 0);
      end;
    0  :
      begin
      for i := ScrollBox3.ControlCount - 1 downto 0 do
        ScrollBox3.Controls[i].free;
      GroupBox5.Enabled := true;
      SetLength(FMainOutputs, 3);
      FMainOutputs[0] := TCheckBox.Create(FmFileConf);
      FMainOutputs[0].Parent  := ScrollBox3;
      FMainOutputs[0].Top     := 20;
      FMainOutputs[0].Left    := 20;
      FMainOutputs[0].Height  := 17;
      FMainOutputs[0].Width   := 100;
      FMainOutputs[0].Caption := 'First row';
      FMainOutputs[0].Checked := true;

      FMainOutputs[1] := TCheckBox.Create(FmFileConf);
      FMainOutputs[1].Parent  := ScrollBox3;
      FMainOutputs[1].Top     := 54;
      FMainOutputs[1].Left    := 20;
      FMainOutputs[1].Height  := 17;
      FMainOutputs[1].Width   := 100;
      FMainOutputs[1].Caption := 'Middle Rows';
      FMainOutputs[1].Checked := true;

      FMainOutputs[2] := TCheckBox.Create(FmFileConf);
      FMainOutputs[2].Parent  := ScrollBox3;
      FMainOutputs[2].Top     := 88;
      FMainOutputs[2].Left    := 20;
      FMainOutputs[2].Height  := 17;
      FMainOutputs[2].Width   := 100;
      FMainOutputs[2].Caption := 'Last Row';
      FMainOutputs[2].Checked := true;
      end;

    else
      for i := ScrollBox3.ControlCount - 1 downto 0 do
        ScrollBox3.Controls[i].free;
      GroupBox5.Enabled := true;
      SetLength(FMainOutputs, par);
      for i := 0 to par - 1 do
        begin
        FMainOutputs[i] := TCheckBox.Create(FmFileConf);
        FMainOutputs[i].Parent  := ScrollBox3;
        FMainOutputs[i].Top     := 20 + (i * 34);
        FMainOutputs[i].Left    := 20;
        FMainOutputs[i].Height  := 17;
        FMainOutputs[i].Width   := 100;
        FMainOutputs[i].Caption := 'row ' + inttostr(i + 1);
        FMainOutputs[i].Checked := true;
        end;
    end;

  end;

procedure TFmFileConf.Button2Click(Sender: TObject);
  begin
  GenerateSubsets;
  end;

{------------------------------------------------------------------------------}

procedure TFmFileConf.AfterShow;
  var
    i, j : integer;
    x : TStringList;
    c : char;
    s : string;
  begin
  X := TStringList.Create;
  j := 0;
  for i := impStringGrid1.FixedRows to impstringgrid1.RowCount - 1 do
    begin
    c := impstringgrid1.Rows[i].Delimiter;
    Impstringgrid1.Rows[i].Delimiter := ' ';
    if trim(ImpStringGrid1.Rows[i].DelimitedText) <> ''  then
       j := j + 1;
    impstringgrid1.Rows[i].Delimiter := c;
    end;


  s := '*D';
  if CheckBox11.Checked then s := s + 'R';
  if CheckBox11.Checked then s := s + 'C';
  x.Add(#215 + #222 + ' ' + inttostr(FLabels.Count) +  ' ' +
        Inttostr (j) + ' ' + inttostr(SpinEdit2.Value) + ' ' + s + ' ' + inttostr(FMaxGroupSize));

  s :=  '    ';
  for i := 0 to FLabels.Count - 1 do
    S := S + ' ' + FLabels[i];
  x.Add(s);

  s :=  '    ';
  for i := 0 to FLabels.Count - 1 do
    S := S + ' ' + FloatToStr(FMin[i]);
  x.Add(s);

  s :=  '    ';
  for i := 0 to FLabels.Count - 1 do
    S := S + ' ' + FloatToStr(FMax[i]);
  x.Add(s);

  s :=  '    ';
  for i := 0 to FLabels.Count - 1 do
    if FInput[i] then
       if FOutput[i] then
          S := S + ' ' + 'B'
       else
            S := S + ' ' + 'I'
    else
       if FOutput[i] then
          S := S + ' ' + 'O'
       else
          S := S + ' ' + 'N';
  x.Add(s);
  s :=  '    ';
  for i := 0 to FLabels.Count - 1 do
    S := S + ' ' + IntToStr(FDelay[i]);
  x.Add(s);

  for i := ImpStringGrid1.FixedRows to ImpStringGrid1.RowCount - 1 do
    begin
    ImpStringGrid1.Rows[i].Delimiter := ' ';
    if (trim(ImpStringGrid1.Rows[i].Strings[0]) = '') and
                         (trim(ImpStringGrid1.Rows[i].DelimitedText) <> '') then
       X.Add('1 ' + ImpStringGrid1.Rows[i].DelimitedText)
    else
       X.Add(ImpStringGrid1.Rows[i].DelimitedText)
    end;

  X.SaveToFile(FTmpName);
  x.free;
  end;

{------------------------------------------------------------------------------}

function TFmFileConf.BeforeShow : boolean;
  var
    x, y : TStringList;
    i, j : integer;
  begin
  CheckBox5.Checked := false;
  CheckBox6.Checked := true;
  if OpenDialog1.Execute then
     begin
     x := TStringList.Create;
     y := TStringList.Create;
     y.Delimiter := ' ';
     x.LoadFromFile(OpenDialog1.FileName);
     for i := 0 to ImpStringGrid1.RowCount - 1 do
       ImpStringgrid1.Rows[i].Clear;

     ImpStringGrid1.RowCount := x.Count;
     ImpStringGrid1.ColCount := 2;
//     k := 0;
     for i := 0 to x.Count - 1 do
       begin
       y.DelimitedText := x.Strings[i];
       if ImpStringGrid1.ColCount < (y.Count + 1) then
          ImpStringGrid1.ColCount := (y.Count + 1);
       for j := 0 to y.Count - 1 do
          begin
          ImpStringGrid1.Cells[j + 1, i] := y.Strings[j];
          end;
       end;
     UpdateArrays;

     x.Free;
     y.Free;
     result := true;
     end
  else
     result := false;

  FSimilaritiesX := 0;
  FSimilaritiesY := 0;
  end;

procedure TFmFileConf.FormCreate(Sender: TObject);
  begin
  FLabels := TStringList.Create;
  end;

procedure TFmFileConf.Button4Click(Sender: TObject);
  begin
  if SaveDialog1.Execute then
     begin
     FTmpName := SaveDialog1.FileName;
     WillBeOk := true;
     Close;
     end;
  end;

procedure TFmFileConf.Button3Click(Sender: TObject);
  begin
  WillBeOk := false;
  Close;
  end;

procedure TFmFileConf.RandomizeGroups(n: integer; out m: array of integer);
  var
    art : array of integer;
    i, j : integer;
  begin
  SetLength(art, n);
  for i := 0 to n - 1 do
    art[i] := -1;
  for i := 0 to n - 1 do
    begin
    j := random(n);
    while art[j] >= 0 do
      j := (j + 1) mod n;
    m[i] := j;
    art[j] := i
    end;
  end;


procedure TFmFileConf.UpdateArrays;
  var
    i, j : integer;
    Xar : array of boolean;
    XTmp : double;
    XConsider : boolean;
  begin
     FLabels.Clear;
     for i := FLabels.Count to ImpStringGrid1.ColCount - 2 do
       FLabels.Add('');

     SetLength(FMax, FLabels.Count);
     SetLength(FMin, FLabels.Count);
     SetLength(Xar, FLabels.Count);
     for i := 0 to FLabels.Count - 1 do
       XAr[i] := false;
     for i := ImpStringGrid1.FixedRows to ImpStringGrid1.RowCount - 1 do
       for j := 0 to FLabels.Count - 1 do
         if trim(ImpStringGrid1.Cells[j + 1, i]) <> '' then
            begin
            XConsider := true;
            try
              Xtmp := StrToFloat(trim(ImpStringGrid1.Cells[j + 1, i]));
            except
              Xtmp := 0;
              XConsider := false;
              end;
            if not XAr[j] then
               begin
               FMin[j] := XTmp;
               FMax[j] := XTmp;
               XAr[j]  := XConsider;
               end
            else
               if XTmp < FMin[j] then
                  FMin[j] := XTmp;
               if XTmp > FMax[j] then
                  FMax[j] := XTmp;
            end;

     SetLength(FInput, FLabels.Count);
     SetLength(FOutput, FLabels.Count);
     SetLength(FDelay, FLabels.Count);
     for i := 0 to FLabels.Count - 1 do
       begin
       FInput[i]  := false;
       FOutput[i] := false;
       FDelay[i]  := 0;
       end;

     for i := ScrollBox1.ControlCount - 1 downto 0 do
       ScrollBox1.Controls[i].Free;

     for i := 1 to ImpStringGrid1.ColCount - 1 do
       begin
       with TLabel.Create(FmFileConf) do
         begin
         Parent := ScrollBox1;
         Name   := 'LbCol' + inttostr(i);
         top    := ((i - 1) * 48) + 8;
         left   := 16;
         height := 13;
         width  := 250;
         tag    := i - 1 ;
         caption := 'teste ' + FLabels.Strings[i - 1];
         end;

       with TCheckBox.Create(FmFileConf) do
         begin
         Parent  := ScrollBox1;
         Name    := 'CkInput' + inttostr(i);
         top     := ((i - 1) * 48) + 26;
         left    := 48;
         height  := 17;
         width   := 80;
         tag     := i - 1;
         caption := 'Input';
         OnClick := InputCheck;
         end;

       with TCheckBox.Create(FmFileConf) do
         begin
         Parent := ScrollBox1;
         Name   := 'CkOutput' + inttostr(i);
         top    := ((i - 1) * 48) + 26;
         left   := 136;
         height := 17;
         width  := 160;
         tag    := i - 1;
         OnClick := OutputCheck;
         end;

       with TButton.Create(FmFileConf) do
         begin
         Parent  := ScrollBox1;
         Name    := 'BtCol' + inttostr(i);
         top     := ((i - 1) * 48) + 24;
         left    := 304;
         height  := 25;
         width   := 25;
         tag     := i - 1;
         caption := '...';
         OnClick := BtColClick;
         end;
       end;
  end;



procedure TFmFileConf.Randomize1Click(Sender: TObject);
  var
    m, inic, count : array of integer;
    tmp : TStringList;
    XRows, i, j: integer;


  begin
  tmp := TStringList.Create;
  FormGroups;
  XRows := ImpStringGrid1.RowCount - ImpStringGrid1.FixedRows;
  Setlength(m, FGroupsCount);
  Setlength(inic, FGroupsCount);
  Setlength(count, FGroupsCount);
  randomizeGroups(FGroupsCount, m);

  j := -1;

  for i := 0 to XRows - 1 do
    begin
    if (FGroups[i] >= 0) then
       begin
       if (FGroups[i] <> j) then
           begin
           j := FGroups[i];
           inic[j] := i;
           count[j] := 1;
           end
       else
           count[j] := count[j] + 1;
       end;
    end;

  for i := 0 to FGroupsCount - 1 do
    begin
    for j := 0 to count[m[i]] - 1 do
      begin
      //******This needs to be verified: it might cause a big trouble in the future
      ImpStringGrid1.Rows[ImpStringGrid1.FixedRows + inic[m[i]] + j].Delimiter := '&';
      tmp.Add(ImpStringGrid1.Rows[ImpStringGrid1.FixedRows + inic[m[i]] + j].DelimitedText);
      end;
    if CheckBox7.Checked and Radiobutton2.Checked then
       Tmp.Add('');
    end;

  for i := 0 to tmp.Count - 1 do
    begin
    //******This needs to be verified: it might cause a big trouble in the future
    ImpStringGrid1.Rows[ImpStringGrid1.FixedRows + i].Delimiter := '&';
    ImpStringGrid1.Rows[ImpStringGrid1.FixedRows + i].DelimitedText := tmp.Strings[i];
    end;
  FormGroups;
  end;

procedure TFmFileConf.Rotate1Click(Sender: TObject);
  var
    Ar : array of array of string;
    i, j, RCount, CCount : integer;
  begin
  CheckBox5.Checked := false;
  checkBox6.Checked := true;
  RCount := ImpStringgrid1.RowCount;
  CCount := ImpStringGrid1.ColCount;
  setLength(ar, RCount, CCount);
  for i := 0 to rcount - 1 do
    for j := ImpStringgrid1.FixedCols to CCount - 1 do
      ar[i, j - ImpStringGrid1.FixedCols] := ImpStringgrid1.Cells[j, i];
  if RCount > CCount then
    ImpStringgrid1.ColCount := RCount + ImpStringgrid1.FixedCols
  else
    ImpStringgrid1.RowCount := CCount - ImpStringgrid1.FixedCols;
  for i := 0 to Rcount - 1 do
    for j := 0 to CCount - (ImpStringGrid1.FixedCols + 1) do
      ImpStringgrid1.Cells[i + ImpStringGrid1.FixedCols, j] := ar[i, j] ;
  if RCount > CCount then
    ImpStringgrid1.RowCount := CCount - ImpStringgrid1.FixedCols
  else
    ImpStringgrid1.ColCount := RCount + ImpStringgrid1.FixedCols;
  UpdateArrays;
  VisualUpdate;
  end;

procedure TFmFileConf.CheckBox1Click(Sender: TObject);
  var i : integer;
  begin
  if CheckBox1.State <> cbGrayed then
     begin
     for i := 0 to ScrollBox1.ControlCount - 1 do
       begin
       if copy(ScrollBox1.Controls[i].Name, 0, 7) = 'CkInput' then
          (ScrollBox1.Controls[i] as TCheckBox).Checked := CheckBox1.Checked;
       end;
     end;
  end;

procedure TFmFileConf.CheckBox2Click(Sender: TObject);
  var i : integer;
  begin
  if CheckBox1.State <> cbGrayed then
     begin
     for i := 0 to ScrollBox1.ControlCount - 1 do
       begin
       if copy(ScrollBox1.Controls[i].Name, 0, 8) = 'CkOutput' then
          (ScrollBox1.Controls[i] as TCheckBox).Checked := CheckBox2.Checked;
       end;
     end;
  end;


procedure TFmFileConf.AutoFillIO(CutEqual: boolean; LeftNull, RightOut,  MinDiff: integer);
  var
    i, j, k, XRows, XCount, XMaxValCount : integer;
    XDiffValues : array of double;
    XTmpArray1, XTmpArray2 : array of double;
    XValueCount : array of integer;
    found : boolean;

  begin
  //define null columns in the left
  for i := 0 to FLabels.Count - 1 do
    begin
    if i < LeftNull then
       begin
       FInput[i]  := false;
       FOutput[i] := false;
       end
    else if i < FLabels.Count - RightOut then
       begin
       FInput[i]  := true;
       FOutput[i] := false;
       end
    else
       begin
       FOutput[i] := false;
       FOutput[i] := true;
       end
    end;

  //Initialize temporary structures
  XRows := ImpStringGrid1.RowCount - ImpStringGrid1.FixedRows;
  SetLength(XTmpArray1, XRows);
  SetLength(XTmpArray2, XRows);

  //Cut columns with small number of different values
  for i := LeftNull to FLabels.Count - (RightOut + 1) do
    if FInput[i] then
       begin
       for j := 0 to XRows - 1 do
         XTmpArray1[j] := StrToFloat(trim(ImpStringgrid1.Cells[i + ImpStringGrid1.fixedCols, j + ImpStringgrid1.fixedRows]));
       j := 0;
       XMaxValCount := 0;
       XCount := 0;
       while (j < XRows) and ((j - XMaxValCount) <= MinDiff) do
         begin
         k := 0;
         found := false;
         while not found and (k < XCount) do
           begin
           found := (XTmpArray1[j] = XDiffValues[k]);
           k := k + 1;
           end;
         if found then
            begin
            XValueCount[k - 1] := XValueCount[k - 1] + 1;
            if XValueCount[k - 1] > XMaxValCount then XMaxValCount := XValueCount[k - 1];
            end
         else
            begin
            XCount := XCount + 1;
            SetLength(XDiffValues, XCount);
            SetLength(XValueCount, XCount);
            XDiffValues[XCount - 1] := XTmpArray1[j];
            XValueCount[XCount - 1] := 1;
            if XMaxValCount <= 0 then XMaxValCount := 1;
            end;
         j := j + 1;
         end;
       if ((XRows - XMaxValCount) < MinDiff) then
          FInput[i] := false;
       end;

  //Cut equal columns
  for i := LeftNull to FLabels.Count - (RightOut + 1) do
    if FInput[i] then
       begin
       for k := 0 to XRows - 1 do
         XTmpArray1[k] := StrToFloat(trim(ImpStringgrid1.Cells[i + ImpStringGrid1.fixedCols, k + ImpStringgrid1.fixedRows]));
       for j := i + 1  to FLabels.Count - (RightOut + 1) do
         if FInput[j] then
            begin
            for k := 0 to XRows - 1 do
              XTmpArray2[k] := StrToFloat(trim(ImpStringgrid1.Cells[j + ImpStringGrid1.fixedCols, k + ImpStringgrid1.fixedRows]));
            found := false;
            k := 0;
            while not found and (k < XRows) do
              begin
              found := not (XTmpArray1[k] = XTmpArray2[k]);
              k := k + 1;
              end;
            FInput[j] := found;
            end;
       end;



  end;

procedure TFmFileConf.Button5Click(Sender: TObject);
  begin
  GroupBox4.Visible := false;
  GroupBox6.Visible := true;
  end;

procedure TFmFileConf.Button7Click(Sender: TObject);
  begin
  GroupBox4.Visible := true;
  GroupBox6.Visible := false;
  end;

procedure TFmFileConf.Button6Click(Sender: TObject);
  var i, j : integer;
  begin
  AutoFillIO(CheckBox3.Checked, SpinEdit3.Value, SpinEdit4.Value, SpinEdit5.value);

  for i := 0 to ScrollBox1.ControlCount - 1 do
    begin
    if copy(ScrollBox1.Controls[i].Name, 0, 8) = 'CkOutput' then
       with ScrollBox1.Controls[i] as TCheckBox do
         Checked := FOutput[Tag];
    if copy(ScrollBox1.Controls[i].Name, 0, 7) = 'CkInput' then
       with ScrollBox1.Controls[i] as TCheckBox do
         Checked := FInput[Tag];
    end;

  j := 0;
  for i := 0 to FLabels.Count - 1 do
    if FInput[i] then j := j + 1;
  MessageDlg(inttostr(j) + ' columns marked as input.', mtInformation, [mbOK], 0);

  GroupBox4.Visible := true;
  GroupBox6.Visible := false;
  end;

procedure TFmFileConf.ListBox1Click(Sender: TObject);
  var
    i, j, k: integer;
    XEqualOrEmpty : boolean;
  begin
  StringGrid1.ColCount := ListBox1.SelCount + 1;
  StringGrid1.FixedCols := ListBox1.SelCount;
  Setlength(FSelCols, ListBox1.SelCount);
  j := 0;
  for i := 0 to ListBox1.Count - 1 do
    if ListBox1.Selected[i] then
       begin
       FSelCols[j] := i + ImpStringgrid1.FixedCols;
       j := j + 1;
       end;
  StringGrid1.RowCount := StringGrid1.FixedRows + 1;
  for k := 0 to ListBox1.SelCount - 1 do
    StringGrid1.Cells[k, StringGrid1.FixedRows] := trim(ImpStringGrid1.Cells[FSelCols[k], ImpStringGrid1.FixedRows]);
  for i := ImpStringGrid1.FixedRows + 1 to ImpStringgrid1.RowCount - 1 do
    begin
    j := StringGrid1.FixedRows;
    XEqualOrEmpty := true;
    for k := 0 to ListBox1.SelCount - 1 do
      XEqualOrEmpty := XEqualOrEmpty and (trim(ImpStringGrid1.Cells[FSelCols[k], i]) = '');
    while (j < StringGrid1.RowCount) and not XEqualOrEmpty do
      begin
      XEqualOrEmpty := true;
      for k := 0 to ListBox1.SelCount - 1 do
        XEqualOrEmpty := XEqualOrEmpty and
                             (StringGrid1.Cells[k, j] = trim(ImpStringGrid1.Cells[FSelCols[k], i]));
       j := j + 1;
      end;
    if not XEqualOrEmpty then
       begin
       StringGrid1.RowCount := StringGrid1.RowCount + 1;
       for k := 0 to ListBox1.SelCount - 1 do
         StringGrid1.Cells[k, StringGrid1.RowCount - 1] := trim(ImpStringGrid1.Cells[FSelCols[k], i]);
       end;
    end;
  UpdateNotFixed;
  end;

procedure TFmFileConf.UpdateNotFixed;
  var
    i, j, xMax : integer;
    xRoot : double;
    XTmpValues : array of integer;
    stop : boolean;
  begin
  FInsideUpdate := true;
  if RadioButton3.Checked then
     begin
     SpinEdit6.Value := StringGrid1.RowCount - StringGrid1.FixedRows;
     StringGrid1.ColCount := StringGrid1.FixedCols + SpinEdit6.Value;
     for i := 0 to SpinEdit6.Value - 1 do
       for j := 0 to SpinEdit6.Value - 1 do
         begin
         if i = j then
            StringGrid1.Cells[StringGrid1.FixedCols + i, StringGrid1.FixedCols + j] := '1'
         else if CheckBox4.Checked then
            StringGrid1.Cells[StringGrid1.FixedCols + i, StringGrid1.FixedCols + j] := '-1'
         else
            StringGrid1.Cells[StringGrid1.FixedCols + i, StringGrid1.FixedCols + j] := '0'
         end;
     end
  else
     begin
     StringGrid1.ColCount := StringGrid1.FixedCols + SpinEdit6.Value;
     SetLength(XTmpValues, SpinEdit6.Value);
     if RadioButton4.Checked then
        begin
        for j := 0 to SpinEdit6.Value - 1 do
          XTmpValues[j] := 0;
        xRoot := power(StringGrid1.RowCount - StringGrid1.FixedRows, 1/SpinEdit6.Value);
        XMax := round(xRoot);
        if XMax < XRoot then XMax := XMax + 1;
        for i := StringGrid1.FixedRows to StringGrid1.RowCount - 1 do
          begin
          for j := 0 to SpinEdit6.Value - 1 do
            with StringGrid1 do
              Cells[j + FixedCols, i] := intToStr(XTmpValues[j]);
          j := 0;
          stop := false;          
          while (j < SpinEdit6.Value) and not stop do
            begin
            XTmpValues[j] := (XTmpValues[j] + 1) mod xMax;
            Stop := (XTmpValues[j] > 0);
            j := j + 1;
            end;
          end;
        end
     end;
  FInsideUpdate := false;
  end;

procedure TFmFileConf.Button8Click(Sender: TObject);
  var
    XOldCount, XNewCount, i, j, j1: integer;
    XTmpStr : array of string;
    XFound, XEmpty : boolean;
  begin
  XOldCount := ImpStringgrid1.ColCount;
  XNewCount := StringGrid1.ColCount - StringGrid1.FixedCols;
  ImpStringgrid1.ColCount := XOldCount + XNewCount;
  Setlength(XTmpStr, StringGrid1.FixedCols);
  for j := ImpStringGrid1.FixedRows to ImpStringgrid1.RowCount - 1 do
    begin
    j1 := StringGrid1.FixedRows;
    XFound := false;
    XEmpty := true;
    while (j1 < StringGrid1.RowCount) and not XFound do
      begin
      XFound := true;
      XEmpty := true;
      for i := 0 to StringGrid1.FixedCols - 1 do
        begin
        XFound := XFound and (StringGrid1.Cells[i, j1] = trim(ImpStringGrid1.Cells[FSelCols[i], j]));
        XEmpty := XEmpty and (trim(ImpStringGrid1.Cells[FSelCols[i], j]) = '');
        end;
      j1 := j1 + 1;
      end;
    j1 := j1 - 1;
    if XEmpty then
       for i := 0 to (StringGrid1.ColCount - StringGrid1.FixedCols) - 1 do
         ImpStringGrid1.Cells[XOldCount + i, j] := ''
    else if XFound then
       for i := 0 to (StringGrid1.ColCount - StringGrid1.FixedCols) - 1 do
         ImpStringGrid1.Cells[XOldCount + i, j] := StringGrid1.Cells[StringGrid1.FixedCols + i, j1];
    end;

  //???? Check in later
  if not CheckBox10.Checked then
     for i := StringGrid1.FixedCols - 1 downto 0 do
         ImpStringGrid1.Columns.Delete(FSelCols[i]);

  UpdateArrays;
  VisualUpdate;
  end;

procedure TFmFileConf.SpinEdit6Change(Sender: TObject);
  begin
  if (trim(SpinEdit6.Text) <> '') and not FInsideUpdate then
     UpdateNotFixed;
  end;

procedure TFmFileConf.RadioButton3Click(Sender: TObject);
  begin
  UpdateNotFixed
  end;

procedure TFmFileConf.CheckBox4Click(Sender: TObject);
  begin
  UpdateNotFixed
  end;

procedure TFmFileConf.RadioButton4Click(Sender: TObject);
  begin
  UpdateNotFixed
  end;

procedure TFmFileConf.RadioButton5Click(Sender: TObject);
  begin
  UpdateNotFixed
  end;

procedure TFmFileConf.UpdateListOfColumns;
  var i, j : integer;
  begin
  ListBox1.Clear;
  for i := ImpStringgrid1.FixedCols to ImpStringgrid1.ColCount - 1 do
    begin
    j := i - ImpStringGrid1.FixedCols + 1;
    if CheckBox6.Checked then
       ListBox1.AddItem('Column ' + inttostr(j), nil)
    else
       ListBox1.AddItem(inttostr(j) + ': ' + ImpStringgrid1.Columns[i].Title.Caption, nil);
    end;
  end;

procedure TFmFileConf.GroupCentre1Click(Sender: TObject);
  var
    i, j : integer;
    xArray : array of double;
    xCentres : array of TRafaDoubleArray;

  begin
  FormGroups;
  SetLength(xCentres, FGroupsCount);
  for i := 0 to FGroupsCount - 1 do
    xCentres[i] := TRafaDoubleArray.Create(ImpStringGrid1.ColCount - ImpStringGrid1.FixedCols);
  SetLength(XArray, ImpStringGrid1.ColCount - ImpStringGrid1.FixedCols);
  for i := ImpStringGrid1.FixedRows to ImpStringGrid1.RowCount - 1 do
    if (FGroups[i - ImpStringGrid1.FixedRows] >= 0) and (FGroups[i -ImpStringGrid1.FixedRows] < FGroupsCount) then
       begin
       for j := ImpStringGrid1.FixedCols to ImpStringGrid1.ColCount - 1 do
         xArray[j - ImpStringGrid1.FixedCols] := strtofloat(ImpStringGrid1.Cells[j, i]);
       XCentres[FGroups[i- ImpStringGrid1.FixedRows]].InsertValue(XArray, 1);
       end;
  ImpStringGrid1.RowCount:= ImpStringGrid1.FixedRows + FGroupsCount;
  ImpStringGrid1.ColCount := ImpStringGrid1.ColCount + 1;

  for i := 0 to FGroupsCount - 1 do
    begin
    for j := ImpStringGrid1.FixedCols to ImpStringGrid1.ColCount - 2 do
      ImpStringGrid1.Cells[j, ImpStringGrid1.FixedRows + i] := FloatToStr(xCentres[i].Data[j - ImpStringGrid1.FixedCols] / xCentres[i].Aux);
    ImpStringGrid1.Cells[ImpStringGrid1.ColCount - 1, ImpStringGrid1.FixedRows + i] := FloatToStr(XCentres[i].Aux);
    XCentres[i].Free;
    end;

  UpdateArrays;
  VisualUpdate;


  end;

procedure TFmFileConf.FilterConflicts1Click(Sender: TObject);
  var
    i, j, k, XLineIndex, XInputIndex, XOutputIndex : integer;
    XLineCount, XInputCount, XOutputCount : integer;
    XTmp : double;
    XDistances : TRafaLineDistanceArray;
    XInputs : array of array of double;
    XOutputs : array of double;
    XArr1, XArr2 : TRafaDoubleArray;
  begin
  XLineCount := 0;
  for i := ImpStringgrid1.FixedRows to ImpStringgrid1.RowCount - 1 do
    if not IsEmptyRow(i) then XLineCount := XLineCount + 1;
  XOutputCount := 0;
  for j := 0 to FLabels.Count - 1 do
    if FOutput[j] then  XOutputCount := XOutputCount + 1;
  for j := 0 to FLabels.Count - 1 do
    if FInput[j] then  XInputCount := XInputCount + 1;
  XDistances := TRafaLineDistanceArray.Create(XLineCount);
  SetLength(XOutputs, XLineCount * XOutputCount);
  SetLength(XInputs, XLineCount, XInputCount);

  XLineIndex := 0;

  for i := ImpStringgrid1.FixedRows to ImpStringgrid1.RowCount - 1 do
    if not IsEmptyRow(i) then
       begin
       XInputIndex := 0;
       XOutputIndex := 0;
       for j := 0 to FLabels.Count - 1 do
         begin
         if FInput[j] then
            begin
            XTmp := strtofloat(ImpStringgrid1.Cells[ImpStringgrid1.fixedCols + j, i]);
            XInputs[XLineIndex, XInputIndex] := (XTmp - FMin[j]) / (FMax[j] - FMin[j]);
            XInputIndex := XInputIndex + 1;
            end;
         if FOutput[j] then
            begin
            XOutputs[(XLineIndex * XOutputCount) + XOutputIndex] :=
                                 strtofloat(ImpStringgrid1.Cells[ImpStringgrid1.fixedCols + j, i]);
            XOutputIndex := XOutputIndex + 1;
            end;
         end;
       XLineIndex := XLineIndex + 1;
       end;

  for i := 0 to XLineCount - 1 do
    for j := 0 to XLineCount - 1 do
      begin
      XArr1 := TRafaDoubleArray.Create(XInputCount);
      XArr2 := TRafaDoubleArray.Create(XInputCount);
      XArr1.InsertValue(XInputs[i], 1);
      XArr2.InsertValue(XInputs[j], 1);
      XDistances.Data[i, j] := XArr1.EuclideanDistance(Xarr2);
      XArr1.Free;
      XArr2.Free;
      end;

  FmLineComp.Execute(XLineCount, XOutputCount, XOutputs, XDistances);

  XLineIndex := 0;
  j := ImpStringgrid1.FixedRows;
  for i := ImpStringgrid1.FixedRows to ImpStringgrid1.RowCount do
    begin
    if IsEmptyRow(i) or not FmLineComp.IsLineSelected[XLineIndex] then
       begin
       for k := 0 to ImpStringgrid1.ColCount - 1 do
         ImpStringgrid1.Cells[k, j] := ImpStringgrid1.Cells[k, i];
       j := j + 1;
       end;
    if not IsEmptyRow(i) then
       XLineIndex := XLineIndex + 1;
    end;
  ImpStringgrid1.RowCount := j;
  UpdateArrays;
  VisualUpdate;
  XDistances.Free;
  end;

function TFmFileConf.IsEmptyRow(rowIndex : integer) : boolean;
  begin
  result := trim(ImpStringGrid1.Cells[1, rowIndex]) = '';
  end;

end.



