unit UnMMVisualNew;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ComCtrls, SimpleGraph, UnMMStructure, StdCtrls, ExtCtrls, UnNetRep, Spin,
  TeeProcs, TeEngine, Chart, series, Gauges;

type
  TFmMMMain = class(TForm)
    SimpleGraph: TSimpleGraph;
    MainMenu1: TMainMenu;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    Label8: TLabel;
    Bevel1: TBevel;
    Label3: TLabel;
    Label4: TLabel;
    Button3: TButton;
    ListBox3: TListBox;
    ListBox4: TListBox;
    Button6: TButton;
    Button7: TButton;
    Panel3: TPanel;
    Label9: TLabel;
    Bevel2: TBevel;
    Label1: TLabel;
    Label2: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Button4: TButton;
    ListBox1: TListBox;
    ListBox2: TListBox;
    Button1: TButton;
    Button2: TButton;
    JvValidateEdit1: TEdit;
    JvValidateEdit2: TEdit;
    Panel4: TPanel;
    Label10: TLabel;
    Bevel3: TBevel;
    Label5: TLabel;
    Label6: TLabel;
    Label13: TLabel;
    Button5: TButton;
    ListBox6: TListBox;
    ListBox5: TListBox;
    ListBox8: TListBox;
    JvValidateEdit3: TEdit;
    Button8: TButton;
    Panel5: TPanel;
    Button9: TButton;
    Label7: TLabel;
    Bevel4: TBevel;
    Panel6: TPanel;
    Label15: TLabel;
    Bevel5: TBevel;
    Label16: TLabel;
    Button11: TButton;
    Edit2: TEdit;
    Button12: TButton;
    RadioGroup2: TRadioGroup;
    Memo2: TMemo;
    GroupBox2: TGroupBox;
    CheckBox2: TCheckBox;
    Panel7: TPanel;
    Label17: TLabel;
    Bevel6: TBevel;
    Button13: TButton;
    Edit1: TEdit;
    Button10: TButton;
    Label14: TLabel;
    Button14: TButton;
    GroupBox1: TGroupBox;
    Label18: TLabel;
    Label19: TLabel;
    JvValidateEdit4: TEdit;
    JvValidateEdit5: TEdit;
    Button15: TButton;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Button16: TButton;
    Button18: TButton;
    Button19: TButton;
    Button20: TButton;
    JvValidateEdit6: TEdit;
    JvValidateEdit7: TEdit;
    PopupMenu1: TPopupMenu;
    OpenDialog2: TOpenDialog;
    OpenDialog3: TOpenDialog;
    SaveDialog1: TSaveDialog;
    SaveDialog2: TSaveDialog;
    Savegraph1: TMenuItem;
    Label23: TLabel;
    Label24: TLabel;
    SpinEdit1: TSpinEdit;
    GroupBox3: TGroupBox;
    Button17: TButton;
    Button21: TButton;
    SaveDialog3: TSaveDialog;
    Panel8: TPanel;
    Label25: TLabel;
    Bevel7: TBevel;
    Button22: TButton;
    JvTreeView1: TTreeView;
    Button23: TButton;
    Button24: TButton;
    Button25: TButton;
    Button26: TButton;
    OpenDialog4: TOpenDialog;
    Label26: TLabel;
    SpinEdit2: TSpinEdit;
    Chart1: TChart;
    CheckBox3: TCheckBox;
    File1: TMenuItem;
    New1: TMenuItem;
    Load1: TMenuItem;
    Save1: TMenuItem;
    SaveDialog4: TSaveDialog;
    PopupMenu2: TPopupMenu;
    Loadfromfile1: TMenuItem;
    Savetofile1: TMenuItem;
    SaveasImage1: TMenuItem;
    SaveDialog5: TSaveDialog;
    SaveWMF1: TMenuItem;
    Button27: TButton;
    Button28: TButton;
    OpenDialog5: TOpenDialog;
    Gauge1: TGauge;
    Gauge2: TGauge;
    Label27: TLabel;
    Panel9: TPanel;
    Label28: TLabel;
    Bevel8: TBevel;
    Button29: TButton;
    CheckBox5: TCheckBox;
    CheckBox1: TCheckBox;
    RadioGroup1: TRadioGroup;
    N1: TMenuItem;
    HideLabels1: TMenuItem;
    procedure New1Click(Sender: TObject);
    procedure SimpleGraphObjectSelect(Graph: TSimpleGraph; GraphObject: TGraphObject);
    procedure Button1Click(Sender: TObject);
    procedure ListBox2Click(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure JvValidateEdit3Change(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure ListBox3Click(Sender: TObject);
    procedure ListBox4Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure ListBox5Click(Sender: TObject);
    procedure ListBox6Click(Sender: TObject);
    procedure Button13Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure Button18Click(Sender: TObject);
    procedure Button20Click(Sender: TObject);
    procedure Button19Click(Sender: TObject);
    procedure Button17Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure Button14Click(Sender: TObject);
    procedure Button15Click(Sender: TObject);
    procedure Button16Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure Savegraph1Click(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure JvValidateEdit6Change(Sender: TObject);
    procedure Button21Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure JvTreeView1Click(Sender: TObject);
    procedure Button23Click(Sender: TObject);
    procedure Button24Click(Sender: TObject);
    procedure Button22Click(Sender: TObject);
    procedure Button25Click(Sender: TObject);
    procedure Button26Click(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
    procedure SpinEdit2Change(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Load1Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure JvValidateEdit1Exit(Sender: TObject);
    procedure JvValidateEdit2Exit(Sender: TObject);
    procedure CheckBox5Click(Sender: TObject);
    procedure SaveasImage1Click(Sender: TObject);
    procedure Loadfromfile1Click(Sender: TObject);
    procedure Savetofile1Click(Sender: TObject);
    procedure SaveWMF1Click(Sender: TObject);
    procedure ListBox8Click(Sender: TObject);
    procedure Button27Click(Sender: TObject);
    procedure Button28Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure Button29Click(Sender: TObject);
    procedure HideLabels1Click(Sender: TObject);

  private
    FStructure : TRafaMooreMachine;
    FOnStateUpdate : boolean;
    FOnInputUpdate : boolean;
    FOnTransitionUpdate : boolean;
    FArDataGroups : array of integer;

    //File Manipulation fields
    FFGroupsCount   : integer;
    FFPatternsCount : integer;
    FFColumnsCount  : integer;
    FFInputsCount   : integer;
    FFOutputsCount   : integer;
    FFInputs        : array of boolean;
    FFoutputs       : array of boolean;
    FFDelays        : array of integer;
    FFileMax        : array of double;
    FFileMin        : array of double;

    FFileStrl : TStringList;

    FNetwork : TNetworkRep;

    FRmseArray : array of double;
    FCurrentEpoch : integer;
    FOnChartUpdate : boolean;

    procedure ExecutionInfo(CurrentEpoch : integer; rmse : double);

    procedure ShowChart;
    procedure HideChart;

    procedure UpdateGraphStatesList;
    procedure UpdateFullStateList;
    procedure UpdateGraphInputsList;
    procedure UpdateFullInputList;
    procedure UpdateGraphTransitionsList;
    procedure UpdateFullTransitionList;
    procedure UpdateSelectedStates(arr : array of integer);
    procedure UpdateSelectedInputs(arr : array of integer);
    procedure UpdateSelectedTransitions(arr : array of integer);
    function  LoadInputDataFromFile(fileName : TFileName) : boolean;
    function  ProcessDataLine(var s : string) : integer;

    function LoadFromFile(fileName : string) : integer;
    function SaveToFile(FileName : string) : integer;
    function SaveRMSEToFile(FileName : string) : integer;
    function SaveLogicProgram(FileName : string; showExtraInfo : boolean) : integer;

    procedure SimpEvent(fileName : string; size : integer; p1, p2 : double);


  public
    { Public declarations }
  end;

var
  FmMMMain: TFmMMMain;

implementation

uses UnMMDialVar, UnMMDialTransition, Math, UnLogSing;

{$R *.dfm}

procedure TFmMMMain.New1Click(Sender: TObject);
  begin
  if FStructure <> nil then
     FStructure.Free;
  if FNetwork <> nil then FNetwork.Free;
  FStructure := TRafaMooreMachine.Create(SimpleGraph);
  FStructure.UsesLabels := not HideLabels1.Checked;
  FStructure.OnExecutionInfo := ExecutionInfo;
  FmMMDialVar.Execute(FStructure);
  UpdateGraphStatesList;
  UpdateFullStateList;
  UpdateGraphInputsList;
  UpdateFullInputList;
//  FStructure.U

  end;

procedure TFmMMMain.UpdateGraphStatesList;
  var i : integer;
  begin
  ListBox1.Clear;
  for i := 0 to FStructure.TotalStateCount - 1 do
    if (FStructure.StateUnificationInfo[i] = i) or (FStructure.StateUnificationInfo[i] < 0) then
       ListBox1.AddItem(FStructure.StateLabel[i], nil);
  end;

procedure TFmMMMain.UpdateFullStateList;
  var i : integer;
  begin
  ListBox2.Clear;
  for i := 0 to FStructure.TotalStateCount - 1 do
    ListBox2.AddItem(FStructure.StateLabel[i], nil);
  end;

procedure TFmMMMain.UpdateGraphInputsList;
  var i : integer;
  begin
  ListBox3.Clear;
  for i := 0 to FStructure.TotalInputCount - 1 do
    if (FStructure.InputUnificationInfo[i] = i) or (FStructure.InputUnificationInfo[i] < 0) then
       ListBox3.AddItem(FStructure.InputLabel[i], nil);
  end;

procedure TFmMMMain.UpdateFullInputList;
  var i : integer;
  begin
  ListBox4.Clear;
  for i := 0 to FStructure.TotalInputCount - 1 do
    ListBox4.AddItem(FStructure.InputLabel[i], nil);
  end;

procedure TFmMMMain.UpdateGraphTransitionsList;
  var i : integer;
  begin
  ListBox5.Clear;
  for i := 0 to FStructure.GetTransitionCount - 1 do
    if (FStructure.GetTransition(i).UnifiedTo = i) or (FStructure.GetTransition(i).UnifiedTo < 0) then
       ListBox5.AddItem(FStructure.GetTransition(i).ToString, nil);
  end;

procedure TFmMMMain.UpdateFullTransitionList;
  var i : integer;
  begin
  ListBox6.Clear;
  for i := 0 to FStructure.GetTransitionCount - 1 do
    ListBox6.AddItem(FStructure.GetTransition(i).ToString, nil);
  end;

procedure TFmMMMain.UpdateSelectedStates(arr : array of integer);
  var
    i, j, k, l, XDoneSize : integer;
    Xdone, XTmpArr : array of integer;
    XX, XY : integer;
  begin
  FOnStateUpdate := true;
  SimpleGraph.ClearSelection;
  ListBox1.ClearSelection;
  ListBox2.ClearSelection;
  XDoneSize := 0;
  for i := 0 to length(arr) - 1 do
    begin
    j := 0;
    while (j < XDoneSize) and (arr[i] <> XDone[j]) do
      j := j + 1;
    if j >= XDoneSize then
       begin
       for k := 0 to SimpleGraph.Objects.Count - 1 do
         if (SimpleGraph.Objects.Items[k] is TGraphNode) then
            if (SimpleGraph.Objects.Items[k].Tag = arr[i]) then
               SimpleGraph.Objects.Items[k].Selected := true;
       for k := 0 to ListBox2.Count - 1 do
         if (FStructure.StateUnificationInfo[k] = arr[i]) or (k = arr[i]) then
            ListBox2.Selected[k] := true;
       SetLength(XTmpArr, ListBox1.Count);
       l := 0;
       for k := 0 to FStructure.TotalStateCount - 1 do
         if (FStructure.StateUnificationInfo[k] = k) or (FStructure.StateUnificationInfo[k] < 0) then
            begin
            XTmpArr[l] := k;
            l := l + 1;
            end;
       for k := 0 to ListBox1.Count - 1 do
         if XTmpArr[k] = arr[i] then
            ListBox1.Selected[k] := true;
       XDoneSize := XDoneSize + 1;
       SetLength(XDone, XDoneSize);
       XDone[XDoneSize - 1] := arr[i];
       end;
    end;
  XX := -1;
  XY := -1;

  for i := 0 to ListBox2.Count - 1 do
    begin
    if ListBox2.Selected[i] then
       begin
       if XX = -1 then
          XX := FStructure.XPosition[i]
       else if XX <> FStructure.XPosition[i] then
          XX := -2;
       if XY = -1 then
          XY := FStructure.YPosition[i]
       else if XY <> FStructure.YPosition[i] then
          XY := -2;
       end;
    end;
  JvValidateEdit1.text := IntToStr(XX);
  JvValidateEdit2.text := IntToStr(XY);

  FOnStateUpdate := false;
  end;

procedure TFmMMMain.UpdateSelectedInputs(arr : array of integer);
  var
    i, j, k, l, XDoneSize : integer;
    Xdone, XTmpArr : array of integer;
  begin
  FOnInputUpdate := true;
  ListBox3.ClearSelection;
  ListBox4.ClearSelection;
  XDoneSize := 0;
  for i := 0 to length(arr) - 1 do
    begin
    j := 0;
    while (j < XDoneSize) and (arr[i] <> XDone[j]) do
      j := j + 1;
    if j >= XDoneSize then
       begin
       for k := 0 to ListBox4.Count - 1 do
         if (FStructure.InputUnificationInfo[k] = arr[i]) or (k = arr[i]) then
            ListBox4.Selected[k] := true;
       SetLength(XTmpArr, ListBox1.Count);
       l := 0;
       for k := 0 to FStructure.TotalInputCount - 1 do
         if (FStructure.InputUnificationInfo[k] = k) or (FStructure.InputUnificationInfo[k] < 0) then
            begin
            XTmpArr[l] := k;
            l := l + 1;
            end;
       for k := 0 to ListBox3.Count - 1 do
         if XTmpArr[k] = arr[i] then
            ListBox3.Selected[k] := true;
       XDoneSize := XDoneSize + 1;
       SetLength(XDone, XDoneSize);
       XDone[XDoneSize - 1] := arr[i];
       end;
    end;
  FOnInputUpdate := false;
  end;

procedure TFmMMMain.UpdateSelectedTransitions(arr : array of integer);
  var
    i, j, k, l, XDoneSize : integer;
    Xdone, XTmpArr : array of integer;
  begin
  FOnTransitionUpdate := true;
//  SimpleGraph.ClearSelection;
  ListBox5.ClearSelection;
  ListBox6.ClearSelection;
  XDoneSize := 0;
  for i := 0 to length(arr) - 1 do
    begin
    j := 0;
    while (j < XDoneSize) and (arr[i] <> XDone[j]) do
      j := j + 1;
    if j >= XDoneSize then
       begin
       //for k := 0 to SimpleGraph.Objects.Count - 1 do
         //if (SimpleGraph.Objects.Items[k] is TGraphLink) then
           // if (SimpleGraph.Objects.Items[k].Tag = arr[i]) then
             //  SimpleGraph.Objects.Items[k].Selected := true;
       for k := 0 to ListBox6.Count - 1 do
         if (FStructure.GetTransition(k).UnifiedTo = arr[i]) or (k = arr[i]) then
            ListBox6.Selected[k] := true;
       SetLength(XTmpArr, ListBox5.Count);
       l := 0;
       for k := 0 to FStructure.GetTransitionCount - 1 do
         if (FStructure.GetTransition(k).UnifiedTo = k) or (FStructure.GetTransition(k).UnifiedTo < 0) then
            begin
            XTmpArr[l] := k;
            l := l + 1;
            end;
       for k := 0 to ListBox5.Count - 1 do
         if XTmpArr[k] = arr[i] then
            ListBox5.Selected[k] := true;
       XDoneSize := XDoneSize + 1;
       SetLength(XDone, XDoneSize);
       XDone[XDoneSize - 1] := arr[i];
       end;
    end;
  Listbox8.Clear;
  FStructure.TransitionsToText(arr, ListBox8.Items, false);
  FOnTransitionUpdate := false;
  end;



procedure TFmMMMain.SimpleGraphObjectSelect(Graph: TSimpleGraph; GraphObject: TGraphObject);
  var
    i, j : integer;
    XArr : array of integer;
  begin
  if (not FOnStateUpdate) and (SimpleGraph.CommandMode = cmEdit) then
     begin
     SetLength(XArr, SimpleGraph.SelectedObjects.Count);
     j := 0;
     for i := 0 to SimpleGraph.SelectedObjects.Count - 1 do
       if SimpleGraph.SelectedObjects.Items[i] is TGraphNode then
          begin
          XArr[j] := SimpleGraph.SelectedObjects.Items[i].Tag;
          j := j + 1;
          end;
     UpdateSelectedStates(XArr);
     end;
  end;

procedure TFmMMMain.Button1Click(Sender: TObject);
  var
    i, XTot : integer;
    Xarr : array of integer;
  begin
  XTot := 0;
  for i := 0 to ListBox2.Count - 1 do
    if ListBox2.Selected[i] then
       begin
       XTot := XTot + 1;
       SetLength(XArr, XTot);
       XArr[XTot - 1] := i;
       end;
  FStructure.UnifyStates(XArr, '');
  UpdateGraphStatesList;
  UpdateSelectedStates(XArr);
  UpdateGraphTransitionsList;
  ListBox5.ItemIndex := 0;
  end;

procedure TFmMMMain.ListBox2Click(Sender: TObject);
  var
    i, j : integer;
    XArr : array of integer;
  begin
  if not FOnStateUpdate then
     begin
     SetLength(XArr, ListBox2.SelCount);
     j := 0;
     for i := 0 to ListBox2.Count - 1 do
       if ListBox2.Selected[i] then
          begin
          if FStructure.StateUnificationInfo[i] = -1 then
             XArr[j] := i
          else
             XArr[j] := FStructure.StateUnificationInfo[i];
          j := j + 1;
          end;
     UpdateSelectedStates(XArr);
     end;
  end;

procedure TFmMMMain.ListBox1Click(Sender: TObject);
  var
    i, j : integer;
    XArr1, XArr2 : array of integer;
  begin
  if not FOnStateUpdate then
     begin
     SetLength(XArr1, ListBox1.Count);
     j := 0;
     for i := 0 to FStructure.TotalStateCount - 1 do
       if (FStructure.StateUnificationInfo[i] = i) or (FStructure.StateUnificationInfo[i] < 0) then
          begin
          XArr1[j] := i;
          j := j + 1;
          end;
     SetLength(XArr2, ListBox1.SelCount);
     j := 0;
     for i := 0 to ListBox1.Count - 1 do
       if ListBox1.Selected[i] then
          begin
          XArr2[j] := XArr1[i];
          j := j + 1;
          end;
     UpdateSelectedStates(XArr2);
     end;
  end;

procedure TFmMMMain.FormClose(Sender: TObject; var Action: TCloseAction);
  begin
  FOnStateUpdate := true;
  end;

procedure TFmMMMain.FormShow(Sender: TObject);
  begin
  FOnStateUpdate := false;
  end;

procedure TFmMMMain.Button2Click(Sender: TObject);
  var
    i, j : integer;
    Xarr : array of integer;
  begin
  SetLength(XArr, ListBox2.SelCount);
  j := 0;
  for i := 0 to ListBox2.Count - 1 do
    if ListBox2.Selected[i] then
       begin
       XArr[j] := i;
       j := j + 1;
       end;
  for j := 0 to ListBox2.SelCount - 1 do
    FStructure.BreakStateGroup(XArr[j]);
  UpdateGraphStatesList;
  UpdateSelectedStates(XArr);
  UpdateGraphTransitionsList;
  ListBox5.ItemIndex := 0;
  end;

procedure TFmMMMain.Button3Click(Sender: TObject);
  begin
  if Button3.Caption = '+' then
     begin
     Panel2.Height := 235;
     Button3.Caption := '-';
     end
  else
     begin
     Panel2.Height := 32;
     Button3.Caption := '+';
     end;
  end;

procedure TFmMMMain.Button4Click(Sender: TObject);
  begin
  if Button4.Caption = '+' then
     begin
     Panel3.Height := 280;
     Button4.Caption := '-';
     end
  else
     begin
     Panel3.Height := 32;
     Button4.Caption := '+';
     end;
  end;

procedure TFmMMMain.Button5Click(Sender: TObject);
  begin
  if Button5.Caption = '+' then
     begin
     Panel4.Height := 445;
     Button5.Caption := '-';
     end
  else
     begin
     Panel4.Height := 32;
     Button5.Caption := '+';
     end;
  end;

procedure TFmMMMain.JvValidateEdit3Change(Sender: TObject);
  var i : integer;
  begin
  for i := 0 to ListBox6.Count - 1 do
    if ListBox6.Selected[i] then
       FStructure.GetTransition(i).Goal := strToInt(JvValidateEdit1.text);
  end;

procedure TFmMMMain.Button8Click(Sender: TObject);
  var
    i, j : integer;
    XArrSt, XArrIn : array of integer;
    XTr : TRafaTransition;
  begin
  SetLength(XArrSt, ListBox1.Count);
  j := 0;
  for i := 0 to FStructure.TotalStateCount - 1 do
    if (FStructure.StateUnificationInfo[i] = i) or (FStructure.StateUnificationInfo[i] < 0) then
       begin
       XArrSt[j] := i;
       j := j + 1;
       end;
  SetLength(XArrIn, ListBox3.Count);
  j := 0;
  for i := 0 to FStructure.TotalInputCount - 1 do
    if (FStructure.InputUnificationInfo[i] = i) or (FStructure.InputUnificationInfo[i] < 0) then
       begin
       XArrIn[j] := i;
       j := j + 1;
       end;
  XTr := FmMMTransition.Execute(FStructure, ListBox1.Items, ListBox3.Items, XArrSt, XArrIn);
  if XTr <> nil then
     FStructure.AddTransition(XTr);
  UpdateGraphTransitionsList;
  UpdateFullTransitionList;
  end;

procedure TFmMMMain.ListBox3Click(Sender: TObject);
  var
    i, j : integer;
    XArr1, XArr2 : array of integer;
  begin
  if not FOnInputUpdate then
     begin
     SetLength(XArr1, ListBox3.Count);
     j := 0;
     for i := 0 to FStructure.TotalInputCount - 1 do
       if (FStructure.InputUnificationInfo[i] = i) or (FStructure.InputUnificationInfo[i] < 0) then
          begin
          XArr1[j] := i;
          j := j + 1;
          end;
     SetLength(XArr2, ListBox3.SelCount);
     j := 0;
     for i := 0 to ListBox3.Count - 1 do
       if ListBox3.Selected[i] then
          begin
          XArr2[j] := XArr1[i];
          j := j + 1;
          end;
     UpdateSelectedInputs(XArr2);
     end;
  end;


procedure TFmMMMain.ListBox4Click(Sender: TObject);
  var
    i, j : integer;
    XArr : array of integer;
  begin
  if not FOnInputUpdate then
     begin
     SetLength(XArr, ListBox4.SelCount);
     j := 0;
     for i := 0 to ListBox4.Count - 1 do
       if ListBox4.Selected[i] then
          begin
          if FStructure.InputUnificationInfo[i] = -1 then
             XArr[j] := i
          else
             XArr[j] := FStructure.InputUnificationInfo[i];
          j := j + 1;
          end;
     UpdateSelectedInputs(XArr);
     end;
  end;

procedure TFmMMMain.Button6Click(Sender: TObject);
  var
    i, XTot : integer;
    Xarr : array of integer;
  begin
  XTot := 0;
  for i := 0 to ListBox4.Count - 1 do
    if ListBox4.Selected[i] then
       begin
       XTot := XTot + 1;
       SetLength(XArr, XTot);
       XArr[XTot - 1] := i;
       end;
  FStructure.UnifyInputs(XArr, '');
  UpdateGraphInputsList;
  UpdateSelectedInputs(XArr);
  UpdateGraphTransitionsList;
  ListBox5.ItemIndex := 0;
  end;

procedure TFmMMMain.Button7Click(Sender: TObject);
  var
    i, j : integer;
    Xarr : array of integer;
  begin
  SetLength(XArr, ListBox4.SelCount);
  j := 0;
  for i := 0 to ListBox4.Count - 1 do
    if ListBox4.Selected[i] then
       begin
       XArr[j] := i;
       j := j + 1;
       end;
  for j := 0 to ListBox4.SelCount - 1 do
    FStructure.BreakInputGroup(XArr[j]);
  UpdateGraphInputsList;
  UpdateSelectedInputs(XArr);
  UpdateGraphTransitionsList;
  ListBox5.ItemIndex := 0;

  end;

procedure TFmMMMain.ListBox5Click(Sender: TObject);
  var
    i, j : integer;
    XArr1, XArr2 : array of integer;
  begin
  if not FOnTransitionUpdate then
     begin
     SetLength(XArr1, ListBox5.Count);
     j := 0;
     for i := 0 to FStructure.GetTransitionCount - 1 do
       if (FStructure.GetTransition(i).UnifiedTo = i) or (FStructure.GetTransition(i).UnifiedTo < 0) then
          begin
          XArr1[j] := i;
          j := j + 1;
          end;
     SetLength(XArr2, ListBox5.SelCount);
     j := 0;
     for i := 0 to ListBox5.Count - 1 do
       if ListBox5.Selected[i] then
          begin
          XArr2[j] := XArr1[i];
          j := j + 1;
          end;
     UpdateSelectedTransitions(XArr2);
     end;
  end;


procedure TFmMMMain.ListBox6Click(Sender: TObject);
  var
    i, j : integer;
    XArr : array of integer;
  begin
  if not FOnTransitionUpdate then
     begin
     SetLength(XArr, ListBox6.SelCount);
     j := 0;
     for i := 0 to ListBox6.Count - 1 do
       if ListBox6.Selected[i] then
          begin
          if FStructure.GetTransition(i).UnifiedTo = -1 then
             XArr[j] := i
          else
             XArr[j] := FStructure.GetTransition(i).UnifiedTo;
          j := j + 1;
          end;
     UpdateSelectedTransitions(XArr);
     end;
  end;

procedure TFmMMMain.Button13Click(Sender: TObject);
  begin
  if Button13.Caption = '+' then
     begin
     Panel7.Height := 270;
     Button13.Caption := '-';
     end
  else
     begin
     Panel7.Height := 32;
     Button13.Caption := '+';
     end;
  end;

procedure TFmMMMain.Button11Click(Sender: TObject);
  begin
  if Button11.Caption = '+' then
     begin
     Panel6.Height := 370;
     Button11.Caption := '-';
     end
  else
     begin
     Panel6.Height := 32;
     Button11.Caption := '+';
     end;
  end;


procedure TFmMMMain.Button9Click(Sender: TObject);
  begin
  if Button9.Caption = '+' then
     begin
     HideChart;
//     Panel5.Height := 335;
     Button9.Caption := '-';
     end
  else
     begin
     HideChart;
     Panel5.Height := 32;
     Button9.Caption := '+';
     end;
  end;


function TFmMMMain.LoadInputDataFromFile(fileName: TFileName) : boolean;
  var
    s, XtmpStr : string;
    tmpStrl : TStringList;
    i, j, XHeaderSize : integer;
  //  XNewStrList : TStringList;
  begin
  Edit2.Text := FileName;
  if FFileStrl = nil then
     FFileStrl := TStringList.Create;
  if FileExists(FileName) then
     begin
     FFileStrl.Clear;
     FFileStrl.LoadFromFile(FileName);
     tmpStrl := TStringList.Create;
     tmpStrl.Delimiter := ' ';
     tmpStrl.DelimitedText := FFileStrl.Strings[0];
     if tmpStrl[0] = '��' then
        begin
        FFColumnsCount  := strtoint(tmpStrl[1]);
        FFPatternsCount := strtoint(tmpStrl[2]);
        FFGroupsCount   := strtoint(tmpStrl[3]);
        FFInputsCount   := 0;
        FFOutputsCount   := 0;

        SetLength(FFInputs, FFColumnsCount);
        SetLength(FFOutputs, FFColumnsCount);
        SetLength(FFDelays, FFColumnsCount);
        SetLength(FFileMin, FFColumnsCount);
        SetLength(FFileMax, FFColumnsCount);

        tmpStrl.DelimitedText := FFileStrl.Strings[2];
        if tmpStrl.Count = FFColumnsCount then
        for i := 0 to FFColumnsCount - 1 do
          FFileMin[i] := strToFloat(tmpStrl[i]);

        tmpStrl.DelimitedText := FFileStrl.Strings[3];
        if tmpStrl.Count = FFColumnsCount then
        for i := 0 to FFColumnsCount - 1 do
          FFileMax[i] := strToFloat(tmpStrl[i]);

        tmpStrl.DelimitedText := FFileStrl.Strings[4];
        if tmpStrl.Count = FFColumnsCount then
           for i := 0 to FFColumnsCount - 1 do
             begin
             if (tmpStrl[i] = 'B') or (tmpStrl[i] = 'I') then
                begin
                FFInputs[i] := true;
                FFinputsCount := FFInputsCount + 1;
                end
             else
                FFInputs[i] := false;
             if (tmpStrl[i] = 'B') or (tmpStrl[i] = 'O') then
                begin
                FFOutputs[i] := true;
                FFOutputsCount := FFOutputsCount + 1;
                end
             else
                FFOutputs[i] := false;
             end;

        tmpStrl.DelimitedText := FFileStrl.Strings[5];

        if tmpStrl.Count = FFColumnsCount then
           for i := 0 to FFColumnsCount - 1 do
             FFDelays[i] := strToInt(tmpStrl[i]);

        Memo2.Lines.Clear;
        Memo2.Lines.Add('File loaded: ' + FileName);
        memo2.Lines.Add('  Number of Columns: '  + inttostr(FFColumnsCount));
        memo2.Lines.Add('  Number of Inputs: '   + inttostr(FFInputsCount));
        memo2.Lines.Add('  Number of Outputs: '  + inttostr(FFOutputsCount));
        memo2.Lines.Add('');
        memo2.Lines.Add('  Number of Patterns: ' + inttostr(FFPatternsCount));
        memo2.Lines.Add('  Number of Groups: '   + inttostr(FFGroupsCount));

        s := '';
        if FFInputsCount <> FStructure.GetInputVarCount then
           begin
           memo2.Lines.Add('');
           memo2.Lines.Add('Error:');
           s := 'Number of inputs declared on file differs ';
           s := s + 'of the number of input inn the model';
           memo2.Lines.Add(s);
           end;
        if FFOutputsCount > FStructure.GetStateVarCount then
           begin
           memo2.Lines.Add('');
           memo2.Lines.Add('Error:');
           s := 'Number of outputs declared on file is higher then ';
           s := s + 'the number of states in the model';
           memo2.Lines.Add(s);
           end;
        if s = '' then
           begin
           if FFGroupsCount > GroupBox2.ControlCount then
              for i := GroupBox2.ControlCount to FFGroupsCOunt - 1 do
                with TCheckBox.Create(self) do
                  begin
                  Top := 15;
                  Left := (i * 45) + 15;
                  Width := 40;
                  Caption := IntToStr(i + 1);
                  tag := i;
                  Parent := GroupBox2;
                  end
           else if FFGroupsCount < GroupBox2.ControlCount then
              for i := GroupBox2.ControlCount - 1 downto FFGroupsCOunt do
                GroupBox2.Controls[i].Free;

           XHeaderSize := 6;
           while trim(FFileStrl.Strings[XHeaderSize]) = '' do
             XHeaderSize := XHeaderSize + 1;
//           XNewStrList := TStringList.Create;

           SetLength(FArDataGroups, FFPatternsCount);
           i := XHeaderSize;
           j := 0;
           while (i < FFileStrl.Count) and (j < FFPatternsCount) do
             begin
             if trim(FFileStrl.Strings[i]) <> '' then
                begin
                XTmpStr := FFileStrl.Strings[i];
                FArDataGroups[j] := ProcessDataLine(XTmpStr);
                FFileStrl.Strings[i] := XTmpStr;
                j := j + 1;
                end;
             i := i + 1;
             end;

           for i := FFileStrl.Count - 1 downto 6 do
             if trim(FFileStrl.Strings[i]) = '' then
                FFileStrl.Delete(i);
           for i := 5 downto 0 do
             FFileStrl.Delete(i);
           result := true;
           end
        else
           begin
           memo2.Lines.Add('');
           s := 'The file cannot be used for learning because ';
           s := s + 'it is incompatible with the model configuration';
           memo2.Lines.Add(s);
           result := false;
           end;

        //Adjust the remaining of the form
        SpinEdit1.MaxValue := FFPatternsCount;
        end

     else
        begin
        Memo2.Lines.Clear;
        Memo2.Lines.Add('File out of format: ' + FileName);
        result := false;
        end;
     end

  else
     begin
     Memo2.Lines.Clear;
     Memo2.Lines.Add('File not found: ' + FileName);
     result := false;
     end;



  end;

procedure TFmMMMain.CheckBox2Click(Sender: TObject);
  var i : integer;
  begin
  if CheckBox2.State = cbUnchecked then
     begin
     for i := 0 to GroupBox2.ControlCount - 1 do
       if GroupBox2.Controls[i] is TCheckBox then
          (GroupBox2.Controls[i] as TCheckBox).Checked := false;
     end
  else if CheckBox2.State = cbChecked then
     begin
     for i := 0 to GroupBox2.ControlCount - 1 do
       if GroupBox2.Controls[i] is TCheckBox then
          (GroupBox2.Controls[i] as TCheckBox).Checked := true;
     end;


  end;

procedure TFmMMMain.Button18Click(Sender: TObject);
  var
    i : integer;
  begin
  if RadioGroup2.ItemIndex  > 0 then
     begin
     FStructure.GetDataList.Clear;
     for i := 0 to FFPatternsCount - 1 do
       if (GroupBox2.Controls[FArDataGroups[i]] as TCheckBox).Checked then
          FStructure.GetDataList.Add(FFileStrl.Strings[i]);
     FStructure.SetEpochSize(FStructure.GetDataList.Count);
     JvValidateEdit6.text := IntToStr(FStructure.GetDataList.Count);
     end
  else
     FStructure.SetEpochSize(StrToInt(JvValidateEdit6.text));
  FStructure.SetLastEpoch(StrToInt(JvValidateEdit7.text));
  Button17.Enabled := false;
  Button18.Enabled := false;
  Button20.Enabled := false;
  Button19.Enabled := true;
  FStructure.Execute;
  Button17.Enabled := false;
  Button18.Enabled := true;
  Button20.Enabled := true;
  Button19.Enabled := false;
  end;

procedure TFmMMMain.Button20Click(Sender: TObject);
  begin
  FStructure.ResetEpoch;
  Panel5.caption := 'Current Epoch: 0';
  end;

procedure TFmMMMain.Button19Click(Sender: TObject);
  begin
  FStructure.StopExecution;
  end;

procedure TFmMMMain.Button17Click(Sender: TObject);
  begin
  FStructure.ResetHistory;
  end;

procedure TFmMMMain.Button10Click(Sender: TObject);
  begin
  if OpenDialog1.Execute then
     begin
     Edit1.Text := OpenDialog1.FileName;
     if FStructure <> nil then
        FStructure.Free;
     FStructure := TRafaMooreMachine.Create(SimpleGraph);
     FStructure.UsesLabels := not HideLabels1.Checked;     
     FStructure.OnExecutionInfo := ExecutionInfo;
     if FNetwork = nil then
        FNetwork := TNetworkRep.Create(nil,nil);
     FNetwork.LoadFromXML(OpenDialog1.FileName);
     FStructure.LoadNetwork(OpenDialog1.FileName);

     end;
  UpdateGraphStatesList;
  UpdateFullStateList;
  UpdateGraphInputsList;
  UpdateFullInputList;
  UpdateGraphTransitionsList;
  UpdateFullTransitionList;
  end;

procedure TFmMMMain.Button14Click(Sender: TObject);
  begin
  if SaveDialog1.Execute then
     if FStructure.GetNetwork.SaveDescription(FNetwork) then
        FNetwork.SaveToXML(SaveDialog1.FileName);
  end;

procedure TFmMMMain.Button15Click(Sender: TObject);
  begin
  FNetwork.SetNEta(StrToFloat(JvValidateEdit4.text));
  FNetwork.SetNMomentum(StrToFloat(JvValidateEdit5.text));
  end;

procedure TFmMMMain.Button16Click(Sender: TObject);
  begin
  FStructure.GetNetwork.LoadDescription(FNetwork);
  FStructure.ResetEpoch;
  FStructure.ResetHistory;
  end;

procedure TFmMMMain.Button12Click(Sender: TObject);
  begin
  if OpenDialog2.Execute then
     begin
     Edit2.Text := OpenDialog2.FileName;
     LoadInputDataFromFile(OpenDialog2.FileName);
     end;
  end;

procedure TFmMMMain.Savegraph1Click(Sender: TObject);
  begin
  if MessageDlg('This will only save the visual description of the graph, without considering the'
        + ' weights and other learned information. Do you want to continue?',
        MtConfirmation, [MbYes, MbNo], 0) = MrYes then
     if SaveDialog2.Execute then
        SimpleGraph.SaveToFile(SaveDialog2.FileName);
  end;

procedure TFmMMMain.RadioGroup2Click(Sender: TObject);
  begin
  JvValidateEdit6.Enabled := (RadioGroup2.ItemIndex = 0);
  if FStructure <> nil then
     begin
     FStructure.InputFromDataList := RadioGroup2.ItemIndex > 0;
     FStructure.OutputFromDataList := RadioGroup2.ItemIndex > 1;
     end;
  end;

function TFmMMMain.ProcessDataLine(var s : string) : integer;
  var
    i : integer;
    XIndex : integer;
    XTmpStrList1, XTmpStrList2 : TSTringList;
  begin
  XTmpStrList1 := TStringList.Create;
  XTmpStrList2 := TStringList.Create;
  XTmpStrList1.DelimitedText := trim(s);
  try
    XIndex := abs(StrToInt(trim(XTmpStrList1.Strings[0]))) - 1;
  except
    XIndex := -1;
    end;
  for i := 0 to FFColumnsCount - 1 do
    if FFInputs[i] then
       XTmpStrList2.Add(XTmpStrList1.Strings[i + 1]);
  for i := 0 to FFColumnsCount - 1 do
    if FFOutputs[i] then
       XTmpStrList2.Add(XTmpStrList1.Strings[i + 1]);
  s := XTmpStrList2.DelimitedText;
  XTmpStrList1.Free;
  XTmpStrList2.Free;
  result := XIndex;
  end;

procedure TFmMMMain.ExecutionInfo(CurrentEpoch : integer; rmse : double);
  begin
  FCurrentEpoch := CurrentEpoch;
  SetLength(FRmseArray, FCurrentEpoch + 1);
  FRmseArray[FCurrentEpoch] := rmse;
  if (checkBox3.Checked) and (Chart1.SeriesCount >= 1) then
     begin
     if CurrentEpoch = 0 then
        Chart1.Series[0].Clear;
     Chart1.Series[0].AddXY(CurrentEpoch, rmse);
     end;
  Label20.Caption := 'Current Epoch := ' + inttostr(CurrentEpoch);
  Label23.Caption := 'RMSE := ' + FloatToStr(rmse);
  UpdateGraphTransitionsList;
  UpdateFullTransitionList;
  end;

procedure TFmMMMain.JvValidateEdit6Change(Sender: TObject);
  begin
  if StrToInt(JvValidateEdit6.text) > 1 then
     SpinEdit1.MaxValue := StrToInt(JvValidateEdit6.text);
  end;

procedure TFmMMMain.Button21Click(Sender: TObject);
  begin
  if SaveDialog3.Execute then
     FStructure.SaveHistoryToFile(SaveDialog3.FileName);
  end;

procedure TFmMMMain.CheckBox1Click(Sender: TObject);
  begin
  FStructure.AllowLearning := CheckBox1.Checked;
  end;

procedure TFmMMMain.JvTreeView1Click(Sender: TObject);
  var
    XTmpNode : TTreeNode;
    i1, i2, i3 : integer;
  begin
  XTmpNode := JvTreeView1.Selected;
  if XTmpNode.level >= 2 then
     begin
     i3 := XTmpNode.Index;
     i2 := XTmpNode.Parent.Index;
     i1 := XTmpNode.Parent.Parent.Index;
     XTmpNode.Text := FStructure.UpdateSequencesFromTree(i1, i2, i3);     
     end;
  end;




procedure TFmMMMain.Button23Click(Sender: TObject);
  begin
  FStructure.AddEmptySequence;
  FStructure.UpdateSequencesTreeList(JvTreeView1);
  end;

procedure TFmMMMain.Button24Click(Sender: TObject);
  var
    XTmpNode : TTreeNode;
    i : integer;
  begin
  XTmpNode := JvTreeView1.Selected;
  i := -1;
  while XTmpNode <> nil do
     begin
     i := XTmpNode.Index;
     XTmpNode := XTmpNode.Parent;
     end;
  if i >= 0 then
     FStructure.AddEmptyInputToSequence(i);
  FStructure.UpdateSequencesTreeList(JvTreeView1);
  end;

procedure TFmMMMain.Button22Click(Sender: TObject);
  begin
  if Button22.Caption = '+' then
     begin
     Panel8.Height := 465;
     Button22.Caption := '-';
     end
  else
     begin
     Panel8.Height := 32;
     Button22.Caption := '+';
     end;
  end;

procedure TFmMMMain.Button25Click(Sender: TObject);
  begin
  FStructure.UpdateSequencesTreeList(JvTreeView1);
  end;

procedure TFmMMMain.Button26Click(Sender: TObject);
  var XStrList : TStringList;
  begin
  if OpenDialog4.Execute then
     begin
     XStrList := TStringList.Create;
     XStrList.LoadFromFile(OpenDialog4.FileName);
     FStructure.LoadSequencesFromStringList(XStrList, 0);
     XStrList.Free;
     FStructure.UpdateSequencesTreeList(JvTreeView1);
     end;
  end;

procedure TFmMMMain.SpinEdit1Change(Sender: TObject);
  begin
  if trim(SpinEdit1.Text) <> '' then
     try
       FStructure.UpdateRate := SpinEdit1.Value;
     except
       FStructure.UpdateRate := SpinEdit1.MaxValue;
       end;
  end;

procedure TFmMMMain.SpinEdit2Change(Sender: TObject);
  begin
  if trim(SpinEdit2.Text) <> '' then
     try
       FStructure.SetCustomInputSelection(SpinEdit2.Value / 100);
     except
       FStructure.SetCustomInputSelection(0.5);
       end;
  end;

procedure TFmMMMain.CheckBox3Click(Sender: TObject);
  begin
  if not FOnChartUpdate then
     begin
     if CheckBox3.Checked then ShowChart else HideChart;
     end;
  end;

procedure TFmMMMain.ShowChart;
  var
    s : TLineSeries;
    i : integer;
  begin
  FOnChartUpdate := true;
  CheckBox3.Checked := true;
  Chart1.Visible := true;
  if Button11.Caption = '+' then
     Button11.Caption := '-';
  Panel5.Height := 483;
  Button18.Top := 388;
  Button19.Top := 388;
  Groupbox3.Top := 418;
  s := TLineSeries.Create(self);
  Chart1.AddSeries(s);
  for i := 0 to FCurrentEpoch do
    if i < length(FRMSEArray) then
       Chart1.Series[0].AddXY(i, FRmseArray[i]);
  FOnChartUpdate := false;
  end;

procedure TFmMMMain.HideChart;
  var i : integer;
  begin
  FOnChartUpdate := true;
  CheckBox3.Checked := false;
  Chart1.Visible := false;
  Panel5.Height := 315;
  Button18.Top := 220;
  Button19.Top := 220;
  Groupbox3.Top := 250;
  for i := Chart1.SeriesList.Count - 1 downto 0 do
    Chart1.Series[i].Free;
  FOnChartUpdate := false;
  end;

procedure TFmMMMain.FormCreate(Sender: TObject);
  begin
  FOnStateUpdate      := false;
  FOnInputUpdate      := false;
  FOnTransitionUpdate := false;
  FOnChartUpdate      := false;
  FFGroupsCount   := 0;
  FFPatternsCount := 0;
  FFColumnsCount  := 0;
  FFInputsCount   := 0;
  FFOutputsCount  := 0;
  FCurrentEpoch   := -1;
  end;

function TFmMMMain.LoadFromFile(fileName : string) : integer;
  var
    XTmpStr1, XTmpStr2 : TStringList;
    XTmpStrInput, XTmpStrState : TStringList;
    XTmpArr : array of Integer;
    XIsFileList : boolean;
    i, j, k, l : integer;
    s : string;

  begin
  XTmpStr1 := TStringList.Create;
  XTmpStr2 := TStringList.Create;
  XTmpStrInput := nil;
  XTmpStrState := nil;
  XIsFileList := false;
  try
    XTmpStr1.LoadFromFile(fileName);
    i := 0;
    while i < XTmpStr1.Count do
      begin
      XTmpStr2.DelimitedText := trim(XTmpStr1.Strings[i]);
      if XTmpStr2.Count > 0 then
         begin
         if XIsFileList then
            begin
            LoadFromFile(trim(XTmpStr1.Strings[i]));
            FStructure.Free;
            FStructure := nil;
            i := i + 1;
            end
         else if lowerCase(XTmpStr2.Strings[0]) = 'load_filelist' then
            begin
            XIsFileList := true;
            i := i + 1;
            end
         else if lowerCase(XTmpStr2.Strings[0]) = 'load_network' then
            begin
            if XTmpStr2.Count > 1 then
               begin
               s := XTmpStr2.Strings[1];
               Edit1.Text := s;
//               if FStructure <> nil then
//                  FStructure.Free;
               if FStructure = nil then
                  begin
                  FStructure := TRafaMooreMachine.Create(SimpleGraph);
                  FStructure.UsesLabels := not HideLabels1.Checked;                  
                  FStructure.OnExecutionInfo := ExecutionInfo;
                  end;
               if FNetwork = nil then
                  FNetwork := TNetworkRep.Create(nil,nil);
               FNetwork.LoadFromXML(s);
               if XTmpStr2.Count > 2 then
                  begin
                  j := strtoint(trim(XTmpStr2.Strings[2]));
                  if j > 0 then
                     for k := 0 to j - 1 do
                       FNetwork.AddConnectedNeuron(1);
                  FStructure.LoadNetwork(s, j);
                  end
               else
                  FStructure.LoadNetwork(s);
               UpdateGraphStatesList;
               UpdateFullStateList;
               UpdateGraphInputsList;
               UpdateFullInputList;
               UpdateGraphTransitionsList;
               UpdateFullTransitionList;
               end;
            i := i + 1;
            end
         else if lowerCase(XTmpStr2.Strings[0]) = 'add_input_variables' then
            begin
            If XTmpStrInput = nil then
               XTmpStrInput := TStringList.Create
            else
               XTmpStrInput.clear;
            for j := 1 to XTmpStr2.Count - 1 do
              XTmpStrInput.Add(XTmpStr2.Strings[j]);
            if XTmpStrState <> nil then
               begin
               if FStructure = nil then
                  begin
                  FStructure := TRafaMooreMachine.Create(SimpleGraph);
                  FStructure.UsesLabels := not HideLabels1.Checked;                  
                  FStructure.OnExecutionInfo := ExecutionInfo;
                  end;
               FStructure.LoadFromVariables(XTmpStrInput, XTmpStrState, TStringList.Create);
               UpdateGraphStatesList;
               UpdateFullStateList;
               UpdateGraphInputsList;
               UpdateFullInputList;
               UpdateGraphTransitionsList;
               UpdateFullTransitionList;
               end;
            i := i + 1;
            end
         else if lowerCase(XTmpStr2.Strings[0]) = 'add_state_variables' then
            begin
            If XTmpStrState = nil then
               XTmpStrState := TStringList.Create
            else
               XTmpStrState.clear;
            for j := 1 to XTmpStr2.Count - 1 do
              XTmpStrState.Add(XTmpStr2.Strings[j]);
            if XTmpStrInput <> nil then
               begin
               if FStructure = nil then
                  begin
                  FStructure := TRafaMooreMachine.Create(SimpleGraph);
                  FStructure.UsesLabels := not HideLabels1.Checked;
                  FStructure.OnExecutionInfo := ExecutionInfo;
                  end;
               FStructure.LoadFromVariables(XTmpStrInput, XTmpStrState, TStringList.Create);
               UpdateGraphStatesList;
               UpdateFullStateList;
               UpdateGraphInputsList;
               UpdateFullInputList;
               UpdateGraphTransitionsList;
               UpdateFullTransitionList;
               end;
            i := i + 1;
            end
         else if lowerCase(XTmpStr2.Strings[0]) = 'unify_states' then
            begin
            if XTmpStr2.Count > 1 then
               begin
               k := strtoint(XTmpStr2.Strings[1]);
               SetLength(XTmpArr, XTmpStr2.Count - 1);
               XTmpArr[0] := k;
               for j := 2 to XTmpStr2.Count - 1 do
                 begin
                 if k <= strtoint(XTmpStr2.Strings[j]) then
                    XTmpArr[j - 1] := strtoint(XTmpStr2.Strings[j])
                 else
                    begin
                    XTmpArr[0] := strtoint(XTmpStr2.Strings[j]);
                    XTmpArr[j - 1] := k;
                    k := strtoint(XTmpStr2.Strings[j]);
                    end;
                 end;
               FStructure.UnifyStates(XTmpArr, '');
               UpdateGraphStatesList;
               UpdateGraphTransitionsList;
               end;
            i := i + 1;
            end
         else if lowerCase(XTmpStr2.Strings[0]) = 'unify_inputs' then
            begin
            if XTmpStr2.Count > 1 then
               begin
               k := strtoint(XTmpStr2.Strings[1]);
               SetLength(XTmpArr, XTmpStr2.Count - 1);
               XTmpArr[0] := k;
               for j := 2 to XTmpStr2.Count - 1 do
                 begin
                 if k <= strtoint(XTmpStr2.Strings[j]) then
                    XTmpArr[j - 1] := strtoint(XTmpStr2.Strings[j])
                 else
                    begin
                    XTmpArr[0] := strtoint(XTmpStr2.Strings[j]);
                    XTmpArr[j - 1] := k;
                    k := strtoint(XTmpStr2.Strings[j]);
                    end;
                 end;
               FStructure.UnifyInputs(XTmpArr, '');
               UpdateGraphInputsList;
               UpdateGraphTransitionsList;
               end;
            i := i + 1;
            end
         else if lowerCase(XTmpStr2.Strings[0]) = 'set_state_x' then
            begin
            if XTmpStr2.Count > 2 then
               FStructure.XPosition[strToInt(XTmpStr2.Strings[1])] := strToInt(XTmpStr2.Strings[2]);
            i := i + 1;
            end
         else if lowerCase(XTmpStr2.Strings[0]) = 'set_state_y' then
            begin
            if XTmpStr2.Count > 2 then
               FStructure.YPosition[strToInt(XTmpStr2.Strings[1])] := strToInt(XTmpStr2.Strings[2]);
            i := i + 1;
            end
         else if lowerCase(XTmpStr2.Strings[0]) = 'load_datafile' then
            begin
            if XTmpStr2.Count > 1 then
               begin
               Edit2.Text := XTmpStr2.Strings[1];
               LoadInputDataFromFile(XTmpStr2.Strings[1]);
               end;
            i := i + 1;
            end
         else if lowerCase(XTmpStr2.Strings[0]) = 'use_file_none' then
            begin
            RadioGroup2.ItemIndex := 0;
            i := i + 1;
            end
         else if lowerCase(XTmpStr2.Strings[0]) = 'use_file_input' then
            begin
            RadioGroup2.ItemIndex := 1;
            i := i + 1;
            end
         else if lowerCase(XTmpStr2.Strings[0]) = 'use_file_all' then
            begin
            RadioGroup2.ItemIndex := 2;
            i := i + 1;
            end
         else if lowerCase(XTmpStr2.Strings[0]) = 'define_file_groups' then
            begin
            j := 1;
            k := 0;
            while (j < XTmpStr2.Count) and (k < GroupBox2.ControlCount) do
              begin
              if GroupBox2.Controls[k] is TCheckBox then
                begin
                (GroupBox2.Controls[k] as TCheckBox).Checked := (strtoint(XTmpStr2.Strings[j]) > 0);
                j := j + 1;
                end;
              k := k + 1;
              end;
            i := i + 1;
            end
         else if lowerCase(XTmpStr2.Strings[0]) = 'load_sequences' then
            begin
            if XTmpStr2.Count > 1 then
               begin
               i := FStructure.LoadSequencesFromStringList(XTmpStr1, i + 1, strToInt(XTmpStr2.Strings[1]));
               FStructure.UpdateSequencesTreeList(JvTreeView1);
               end;                                            
            end
         else if lowerCase(XTmpStr2.Strings[0]) = 'network_eta' then
            begin
            if XTmpStr2.Count > 1 then
               begin
               FNetwork.SetNEta(StrToFloat(XTmpStr2.Strings[1]));
               JvValidateEdit4.text := XTmpStr2.Strings[1];
               end;
            i := i + 1;
            end
         else if lowerCase(XTmpStr2.Strings[0]) = 'network_momentum' then
            begin
            if XTmpStr2.Count > 1 then
               begin
               FNetwork.SetNMomentum(StrToFloat(XTmpStr2.Strings[1]));
               JvValidateEdit5.text := XTmpStr2.Strings[1];
               end;
            i := i + 1;
            end
         else if lowerCase(XTmpStr2.Strings[0]) = 'reset_training' then
            begin
            FStructure.GetNetwork.LoadDescription(FNetwork);
            FStructure.ResetEpoch;
            FStructure.ResetHistory;
            i := i + 1;
            end
         else if lowerCase(XTmpStr2.Strings[0]) = 'save_network' then
            begin
            if XTmpStr2.Count > 1 then
               if FStructure.GetNetwork.SaveDescription(FNetwork) then
                  FNetwork.SaveToXML(XTmpStr2.Strings[1]);
            i := i + 1;
            end
         else if lowerCase(XTmpStr2.Strings[0]) = 'reset_history' then
            begin
            FStructure.ResetHistory;
            i := i + 1;
            end
         else if lowerCase(XTmpStr2.Strings[0]) = 'save_history' then
            begin
            if XTmpStr2.Count > 1 then
               FStructure.SaveHistoryToFile(XTmpStr2.Strings[1] + '1');
            i := i + 1;
            end
         else if lowerCase(XTmpStr2.Strings[0]) = 'save_rmse_history' then
            begin
            if XTmpStr2.Count > 1 then
               SaveRMSEToFile(XTmpStr2.Strings[1]);
            i := i + 1;
            end
         else if lowerCase(XTmpStr2.Strings[0]) = 'save_diagram_as_wmf' then
            begin
            if XTmpStr2.Count > 1 then
               SimpleGraph.SaveAsMetafile(XTmpStr2.Strings[1]);
            i := i + 1;
            end
         else if lowerCase(XTmpStr2.Strings[0]) = 'save_rmse_as_wmf' then
            begin
            if XTmpStr2.Count > 1 then
               begin
               CheckBox3.Checked := false;
               CheckBox3.Checked := true;
               Chart1.Width := 600;
               Chart1.Height := 400;
               Chart1.SaveToMetafile(XTmpStr2.Strings[1]);
               Chart1.Width := 273;
               Chart1.Height := 185;
               end;
            i := i + 1;
            end
         else if lowerCase(XTmpStr2.Strings[0]) = 'save_logic_program' then
            begin
            if XTmpStr2.Count > 1 then
               SaveLogicProgram(XTmpStr2.Strings[1], false);
            i := i + 1;
            end
         else if lowerCase(XTmpStr2.Strings[0]) = 'save_logic_program_xtra' then
            begin
            if XTmpStr2.Count > 1 then
               SaveLogicProgram(XTmpStr2.Strings[1], true);
            i := i + 1;
            end
         else if lowerCase(XTmpStr2.Strings[0]) = 'allow_learning' then
            begin
            FStructure.AllowLearning := true;
            CheckBox1.Checked := true;
            i := i + 1;
            end
         else if lowerCase(XTmpStr2.Strings[0]) = 'not_allow_learning' then
            begin
            FStructure.AllowLearning := false;
            CheckBox1.Checked := false;
            i := i + 1;
            end
         else if lowerCase(XTmpStr2.Strings[0]) = 'reinforce_current_output' then
            begin
            FStructure.DefaultCorrection := dcReinforce;
            RadioGroup1.ItemIndex := 3;
            i := i + 1;
            end
         else if lowerCase(XTmpStr2.Strings[0]) = 'not_reinforce_current_output' then
            begin
            FStructure.DefaultCorrection := dcNone;
            RadioGroup1.ItemIndex := 4;
            i := i + 1;
            end
         else if lowerCase(XTmpStr2.Strings[0]) = 'default_correction' then
            begin
            if XTmpStr2.Count > 1 then
               begin
               if lowerCase(XTmpStr2.Strings[1]) = 'current' then
                  begin
                  FStructure.DefaultCorrection := dcCurrent;
                  RadioGroup1.ItemIndex := 0;
                  end
               else if lowerCase(XTmpStr2.Strings[1]) = 'true' then
                  begin
                  FStructure.DefaultCorrection := dcTrue;
                  RadioGroup1.ItemIndex := 1;
                  end
               else if lowerCase(XTmpStr2.Strings[1]) = 'false' then
                  begin
                  FStructure.DefaultCorrection := dcFalse;
                  RadioGroup1.ItemIndex := 2;
                  end
               else if lowerCase(XTmpStr2.Strings[1]) = 'reinforce' then
                  begin
                  FStructure.DefaultCorrection := dcReinforce;
                  RadioGroup1.ItemIndex := 3;
                  end
               else if lowerCase(XTmpStr2.Strings[1]) = 'none' then
                  begin
                  FStructure.DefaultCorrection := dcNone;
                  RadioGroup1.ItemIndex := 4;
                  end
               end;
            i := i + 1;
            end
         else if lowerCase(XTmpStr2.Strings[0]) = 'use_network_choice' then
            begin
            FStructure.UseNetworkTransition := true;
            CheckBox5.Checked := true;
            i := i + 1;
            end
         else if lowerCase(XTmpStr2.Strings[0]) = 'not_use_network_choice' then
            begin
            FStructure.UseNetworkTransition := false;
            CheckBox5.Checked := false;
            i := i + 1;
            end
         else if lowerCase(XTmpStr2.Strings[0]) = 'set_scalar_inputs' then
            begin
            k := 0;
            try
              for j := 1 to XTmpStr2.Count - 1 do
                begin
                l := StrToInt(trim(XTmpStr2.Strings[j]));
                if (l >= 0) and (l < FStructure.InputCount) then
                   begin
                   SetLength(XTmpArr, k + 1);
                   XTmpArr[k] := l;
                   k := k + 1;
                   end;
                end;
              FStructure.DefineScalarInput(XTmpArr);
            finally
               i := i + 1;
               end;
            end
         else if lowerCase(XTmpStr2.Strings[0]) = 'reset_scalar_inputs' then
            begin
            FStructure.ResetScalarInputs;
            i := i + 1;
            end
         else if lowerCase(XTmpStr2.Strings[0]) = 'set_scalar_states' then
            begin
            k := 0;
            try
              for j := 1 to XTmpStr2.Count - 1 do
                begin
                l := StrToInt(trim(XTmpStr2.Strings[j]));
                if (l >= 0) and (l < FStructure.StateCount) then
                   begin
                   SetLength(XTmpArr, k + 1);
                   XTmpArr[k] := l;
                   k := k + 1;
                   end;
                end;
              FStructure.DefineScalarState(XTmpArr);
            finally
               i := i + 1;
               end;
            end
         else if lowerCase(XTmpStr2.Strings[0]) = 'reset_scalar_states' then
            begin
            FStructure.ResetScalarStates;
            i := i + 1;
            end
         else if lowerCase(XTmpStr2.Strings[0]) = 'use_scalar_inputs' then
            begin
            FStructure.UseScalarInputs := true;
            i := i + 1;
            end
         else if lowerCase(XTmpStr2.Strings[0]) = 'not_use_scalar_inputs' then
            begin
            FStructure.UseScalarInputs := false;
            i := i + 1;
            end
         else if lowerCase(XTmpStr2.Strings[0]) = 'use_scalar_states' then
            begin
            FStructure.UseScalarStates := true;
            i := i + 1;
            end
         else if lowerCase(XTmpStr2.Strings[0]) = 'not_use_scalar_states' then
            begin
            FStructure.UseScalarStates := false;
            i := i + 1;
            end
         else if lowerCase(XTmpStr2.Strings[0]) = 'allow_empty_scalar_inputs' then
            begin
            FStructure.AllowEmptyScalarInputs := true;
            i := i + 1;
            end
         else if lowerCase(XTmpStr2.Strings[0]) = 'not_allow_empty_scalar_inputs' then
            begin
            FStructure.AllowEmptyScalarInputs := false;
            i := i + 1;
            end
         else if lowerCase(XTmpStr2.Strings[0]) = 'allow_empty_scalar_inputs' then
            begin
            FStructure.AllowEmptyScalarStates := true;
            i := i + 1;
            end
         else if lowerCase(XTmpStr2.Strings[0]) = 'not_allow_empty_scalar_inputs' then
            begin
            FStructure.AllowEmptyScalarStates := false;
            i := i + 1;
            end
         else if lowerCase(XTmpStr2.Strings[0]) = 'randomize_inputs' then
            begin
            FStructure.RandomizeOrder := true;
            i := i + 1;
            end
         else if lowerCase(XTmpStr2.Strings[0]) = 'do_not_randomize_inputs' then
            begin
            FStructure.RandomizeOrder := false;
            i := i + 1;
            end
         else if lowerCase(XTmpStr2.Strings[0]) = 'execute' then
            begin
            k := 1;
            if XTmpStr2.Count > 1 then
               begin
               JvValidateEdit7.text := XTmpStr2.Strings[1];
               end;
            if XTmpStr2.Count > 2 then
               begin
               k := strtoint(XTmpStr2.Strings[2]);
               SpinEdit1.Value := k;
               SpinEdit1Change(SpinEdit1);
               end;
            if XTmpStr2.Count > 3 then
               begin
               SpinEdit2.Value := strtoint(XTmpStr2.Strings[3]);
               end;
            if RadioGroup2.ItemIndex  > 0 then
               begin
               FStructure.GetDataList.Clear;
               for j := 0 to FFPatternsCount - 1 do
                 if (GroupBox2.Controls[FArDataGroups[i]] as TCheckBox).Checked then
                    FStructure.GetDataList.Add(FFileStrl.Strings[j]);
               FStructure.SetEpochSize(FStructure.GetDataList.Count);
               JvValidateEdit6.text := IntToStr(FStructure.GetDataList.Count);
               end
            else
               if XTmpStr2.Count > 4 then
                  begin
                  JvValidateEdit6.text := XTmpStr2.Strings[4];
                  if k > SpinEdit1.Value then
                     SpinEdit1.Value := k;
                  FStructure.SetEpochSize(StrToInt(JvValidateEdit6.text));
                  end;
            FStructure.SetLastEpoch(StrToInt(JvValidateEdit7.text));
            Button17.Enabled := false;
            Button18.Enabled := false;
            Button20.Enabled := false;
            Button19.Enabled := true;
            FStructure.Execute;
            Button17.Enabled := false;
            Button18.Enabled := true;
            Button20.Enabled := true;
            Button19.Enabled := false;
            i := i + 1;
            end
         else
            i := i + 1;
         end
      else
         i := i + 1;
      end;
    XTmpStr2.Free;
    result := XTmpStr1.Count;
    XTmpStr1.Free;
  except
    XTmpStr1.Free;
    XTmpStr2.Free;
    result := -1;
    end;
  end;

function TFmMMMain.SaveToFile(FileName : string) : integer;
  var
    i, j : integer;
    s : string;
    b : boolean;
    XTmpStrList : TStringList;
  begin
  XTmpStrList := TStringList.Create;

  if trim(Edit1.Text) <> '' then
     XTmpStrList.Add('LOAD_DATAFILE ' + Edit2.Text)
  else
     begin
     if FStructure.TotalInputCount > 0 then
        XTmpStrList.Add('ADD_INPUT_VARIABLES ' + FStructure.GetVariablesAsString(false));
     if FStructure.TotalStateCount > 0 then
        XTmpStrList.Add('ADD_STATE_VARIABLES ' + FStructure.GetVariablesAsString(true));
     end;
  XTmpStrList.Add('');

  if FStructure.TotalInputCount <> FStructure.InputCount then
     begin
     for i := 0 to FStructure.TotalInputCount - 1 do
       begin
       b := false;
       s := inttostr(i);
       for j := 0 to FStructure.TotalInputCount - 1 do
         if (i <> j) and (FStructure.InputUnificationInfo[j] = i) then
            begin
            b := true;
            s := s + ' ' + inttostr(j);
            end;
       if b then XTmpStrList.Add(s);
       end;
     end;
  XTmpStrList.Add('');
  if FStructure.TotalStateCount <> FStructure.StateCount then
     begin
     for i := 0 to FStructure.TotalStateCount - 1 do
       begin
       b := false;
       s := inttostr(i);
       for j := 0 to FStructure.TotalStateCount - 1 do
         if (i <> j) and (FStructure.StateUnificationInfo[j] = i) then
            begin
            b := true;
            s := s + ' ' + inttostr(j);
            end;
       if b then XTmpStrList.Add(s);
       end;
     end;
  XTmpStrList.Add('');

  for i := 0 to FStructure.TotalStateCount - 1 do
    begin
    XTmpStrList.Add('SET_STATE_X ' + inttostr(i) + ' ' + inttostr(FStructure.XPosition[i]));
    XTmpStrList.Add('SET_STATE_Y ' + inttostr(i) + ' ' + inttostr(FStructure.YPosition[i]));
    end;

  XTmpStrList.Add('');

  if trim(Edit2.Text) <> '' then
    XTmpStrList.Add('LOAD_DATAFILE ' + Edit2.Text);

  case RadioGroup2.ItemIndex of
    0 : XTmpStrList.Add('USE_FILE_NONE');
    1 : XTmpStrList.Add('USE_FILE_INPUT');
    2 : XTmpStrList.Add('USE_FILE_ALL');
    end;
  XTmpStrList.Add('');
  XTmpStrList.Add('LOAD_SEQUENCES ' + inttostr(FStructure.GetSequenceCount));
  XTmpStrList.SaveToFile(FileName);
  result := XTmpStrList.Count;
  XTmpStrList.Free;
  end;

function TFmMMMain.SaveRMSEToFile(FileName : string) : integer;
  var
    i : integer;
    XTmpStrList : TStringList;
  begin
  XTmpStrList := TStringList.Create;
  for i := 0 to FCurrentEpoch do
    if i < length(FRmseArray) then
       XTmpStrList.Add(FloatToStr(FRmseArray[i]));
  XTmpStrList.SaveToFile(FileName);
  result := XTmpStrList.Count;
  XTmpStrList.Free;
  end;

function TFmMMMain.SaveLogicProgram(FileName : string; showExtraInfo : boolean) : integer;
  var
    XTmpProgram : TSingleLogicProgram;
    Xin, Xout : array of integer;
    i : integer;
  begin
  XTmpProgram := FStructure.TransitionsAsLogicProgram;
  if XTmpProgram <> nil then
     begin
     XTmpProgram.SaveToFile(FileName + '_full', showExtraInfo);

     SetLength(XIn,  FStructure.GetInputVarCount + FStructure.GetStateVarCount);
     SetLength(XOut, FStructure.GetStateVarCount * 2);
     for i := 0 to FStructure.GetInputVarCount + FStructure.GetStateVarCount - 1 do
       XIn[i] := i;
     for i := 0 to FStructure.GetStateVarCount - 1 do
       begin
       XOut[i * 2] := FStructure.GetInputVarCount + FStructure.GetStateVarCount + i;
       XOut[(i * 2) + 1] := FStructure.GetInputVarCount + (2 * FStructure.GetStateVarCount) + i;
       end;

     XTmpProgram.SimplificationEvent := SimpEvent;
     XTmpProgram.KarnaughBased2(XOut, Xin);
     XTmpProgram.SaveToFile(FileName, showExtraInfo);
     result := XTmpProgram.ClausesCount;
     XTmpProgram.freeStructure;
     end
  else
     result := -1;
  end;


procedure TFmMMMain.Load1Click(Sender: TObject);
  begin
  if OpenDialog3.Execute then
     LoadFromFile(OpenDialog3.FileName);
  end;

procedure TFmMMMain.Save1Click(Sender: TObject);
  begin
  if SaveDialog4.Execute then
     SaveToFile(SaveDialog4.FileName);
  end;

procedure TFmMMMain.JvValidateEdit1Exit(Sender: TObject);
  var i : integer;
  begin
  if StrToInt(JvValidateEdit1.text) >= 0 then
     for i := 0 to ListBox2.Count - 1 do
       if ListBox2.Selected[i] then
          FStructure.XPosition[i] := StrToInt(JvValidateEdit1.text);
  end;


procedure TFmMMMain.JvValidateEdit2Exit(Sender: TObject);
  var i : integer;
  begin
  if StrToInt(JvValidateEdit2.text) >= 0 then
     for i := 0 to ListBox2.Count - 1 do
       if ListBox2.Selected[i] then
          FStructure.YPosition[i] := StrToInt(JvValidateEdit2.text);
  end;

procedure TFmMMMain.CheckBox5Click(Sender: TObject);
  begin
  FStructure.UseNetworkTransition := CheckBox5.Checked;
  end;

procedure TFmMMMain.SaveasImage1Click(Sender: TObject);
  begin
  if SaveDialog5.Execute then
     begin
     Chart1.Width := 600;
     Chart1.Height := 400;
     Chart1.SaveToMetafile(SaveDialog5.FileName);
     Chart1.Width := 273;
     Chart1.Height := 185;
     end
  end;

procedure TFmMMMain.Loadfromfile1Click(Sender: TObject);
  var
    i, j: integer;
    XTmpStrList : TStringList;
  begin
  if OpenDialog4.Execute then
     begin
     XTmpStrList := TStringList.Create;
     try
       j := 0;
       XTmpStrList.LoadFromFile(OpenDialog4.FileName);
       for i := 0 to XTmpStrList.Count - 1 do
         begin
         if trim(XTmpStrList.Strings[i]) <> '' then
            begin
            ExecutionInfo(j, StrToFloat(trim(XTmpStrList.Strings[i])));
            j := j + 1;
            end;
         end;
       XTmpStrList.Free;
     except
       FCurrentEpoch := 0;
       XTmpStrList.Free;
       end;
     end;
  end;

procedure TFmMMMain.Savetofile1Click(Sender: TObject);
  begin
  if SaveDialog3.Execute then
     SaveRMSEToFile(SaveDialog3.FileName);
  end;

procedure TFmMMMain.SaveWMF1Click(Sender: TObject);
  begin
  if SaveDialog5.Execute then
     SimpleGraph.SaveAsMetafile(SaveDialog5.FileName);
  end;

procedure TFmMMMain.ListBox8Click(Sender: TObject);
  begin
  ListBox8.hint := ListBox8.Items.Strings[ListBox8.itemIndex];
  end;

procedure TFmMMMain.Button27Click(Sender: TObject);
  begin
  if OpenDialog5.Execute then
     SaveLogicProgram(OpenDialog5.FileName, false);
  end;

procedure TFmMMMain.Button28Click(Sender: TObject);
  begin
  if OpenDialog5.Execute then
     SaveLogicProgram(OpenDialog5.FileName, true);
  end;

procedure TFmMMMain.SimpEvent(fileName : string; size : integer; p1, p2 : double);
  begin
  Label27.Caption := IntToStr(size);
  Gauge1.Progress := round(p1*100);
  Gauge2.Progress := round(p2*100);  
  end;


procedure TFmMMMain.RadioGroup1Click(Sender: TObject);
  begin
  case RadioGroup1.ItemIndex of
    0 : FStructure.DefaultCorrection := dcCurrent;
    1 : FStructure.DefaultCorrection := dcTrue;
    2 : FStructure.DefaultCorrection := dcFalse;
    3 : FStructure.DefaultCorrection := dcReinforce;
    4 : FStructure.DefaultCorrection := dcNone;
    end;
  end;

procedure TFmMMMain.Button29Click(Sender: TObject);
  begin
  if Button29.Caption = '+' then
     begin
     Panel9.Height := 224;
     Button29.Caption := '-';
     end
  else
     begin
     Panel9.Height := 32;
     Button29.Caption := '+';
     end;
  end;

procedure TFmMMMain.HideLabels1Click(Sender: TObject);
  begin
  SimpleGraph.Enabled := not HideLabels1.Checked;
  if FStructure <> nil then FStructure.UsesLabels := not HideLabels1.Checked;
  end;

end.

