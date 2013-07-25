{
  @abstract(Unit describing the interface to configure the exibitions of results in charts)
  @author(Rafa "Passarel" Borges <passarel@gmail.com>)
  @created(London, 2008)
  @lastmod(London, January, 2010)
}

unit UnDialChConf;

interface

uses
 Classes, Controls, Dialogs, ExtCtrls, ExtDlgs, Forms, Graphics, Menus,
 Messages, Spin, StdCtrls, SysUtils, UnResults, Variants, Windows;

//-------------------------------------------------------------------------------------------------

type
  TFmChConf = class(TForm)
    Button1: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    CreateChart1: TMenuItem;
    CreateOutput1: TMenuItem;
    Exit1: TMenuItem;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    MainMenu1: TMainMenu;
    N1: TMenuItem;
    N2: TMenuItem;
    OpenDialog1: TOpenDialog;
    OpenReport1: TMenuItem;
    OpenReport2: TMenuItem;
    RunScript1: TMenuItem;
    SaveDialog1: TSaveDialog;
    SaveNewFile1: TMenuItem;
    ScrewBack2: TPanel;
    ScrewBack3: TPanel;
    ScrewBack6: TPanel;
    ScrewBox2: TPanel;
    ScrewBox3: TPanel;
    ScrewBox6: TPanel;
    ScrollBar1: TScrollBar;
    ScrollBar2: TScrollBar;
    ScrollBox1: TScrollBox;
    ScrollBox4: TScrollBox;
    SpinEdit1: TSpinEdit;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure CBChange(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure ComboBox3Change(Sender: TObject);
    procedure CreateChart1Click(Sender: TObject);
    procedure CreateOutput1Click(Sender: TObject);
    procedure DeleteGroupEvent(Sender: TObject);
    procedure GeneralChange(Sender: TObject);
    procedure OpenReport2Click(Sender: TObject);
    procedure RunScript1Click(Sender: TObject);
    procedure SaveNewFile1Click(Sender: TObject);
    procedure ScrollBar1Change(Sender: TObject);
    procedure ScrollBar2Change(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure CheckBox5Click(Sender: TObject);
  private

    //Array of visual components created dinamically to represent each group of data
    FGroupControls : array of array of TControl;

    //Array of check boxes related to the "longitudinal" groups of data
    FLongitCheckBoxes : array of TCheckBox;

    //Array of check boxes relating the original columns of the data set and the group of data
    FMarkCheckBoxes : array of array of TCheckBox;

    //Boolean variable indicating if the button OK was pressed
    FOK : boolean;

    //Name of the file to be created as output (if the option was checked)
    FOutFileName : string;

    //Structure used to store and manipulate the loaded data
    FStructure   : THekatonResults;

    //Array of check boxes identifying which subsets of the data set will be used
    FSubsetCheckBoxes   : array of TCheckBox;

    //Variable identifying if the interface is being updated
    FUpdateInterfaceMode : boolean;

    //Accessor to the GroupCount property
    function GetGroupCount: integer;

    //Accessor to the LongitudinalFunction property
    function GetLongFunction: TResultTransf;

    //Accessor to the LongitudinalGrouped property
    function GetLongGroup: boolean;

    //Accessor to the LongitudinalMarks property
    function GetLongMarks(index: integer): boolean;

    //Accessor to the LongitudinalSize property
    function GetLongSize: integer;

    //Accessor to the Marks property
    function GetMark(i, j: integer): boolean;

    //Accessor to the SubsetSelection property
    function GetSubsetSelection(index: integer): boolean;

    //Mutator to the LongitudinalFunction property
    procedure SetLongFunction(const Value: TResultTransf);

    //Mutator to the LongitudinalGrouped property
    procedure SetLongGroup(const Value: boolean);

    //Mutator to the LongitudinalMarks property
    procedure SetLongMarks(index: integer; const Value: boolean);

    //Mutator to the LongitudinalSize property
    procedure SetLongSize(const Value: integer);

    //Mutator to the Marks property
    procedure SetMark(i, j: integer; const Value: boolean);

    //Mutator to the SubsetSelection property
    procedure SetSubSetSelection(index: integer; const Value: boolean);

    //procedure to configure the ScrollBoxes according to the number of columns and groups
    //@param(Columns: number of columns in the results dataset)
    //@param(Groups: number of groups - columns in the processed output)
    procedure ScrewConfig(Columns, Groups : integer);

    //Procedure to update the structure @link(FStructure) according to the interface
    procedure UpdateVariables;

  public

    //Count of groups (columns) in the processed output
    property  GroupCount : integer read GetGroupCount;

    //Function used to the longitudinal grouping the data
    property  LongitudinalFunction : TResultTransf read GetLongFunction write SetLongFunction;

    //Property identified if the data will be longitudinally grouped (needs to be better explained)
    property  LongitudinalGrouped : boolean read GetLongGroup write SetLongGroup;

    //Marks identifying which longitudinal groups will be used when generating the output
    property  LongitudinalMarks[index : integer] : boolean read GetLongMarks write SetLongMarks;

    //Size of the longitudinal groups
    property  LongitudinalSize : integer read GetLongSize write SetLongSize;

    //Marks identifying which columns of the input dataset are associated with which group in the
    //processed output
    property  Marks[i, j : integer] : boolean read GetMark write SetMark;

    //Selects which subsets of the original dataset will be considered
    property  SubsetSelection[index : integer] : boolean read GetSubsetSelection write SetSubSetSelection;

    //Add a new output group to the structure
    //@param(func: Function used to group the data)
    //@param(mode: Mode which the data is grouped)
    procedure AddGroup(func : TResultTransf; mode : TResultGrouping);

    //Deletes an output group from the output
    procedure DeleteGroup(index : integer);

    //Shows the interface, loading the fields according to the input dataset
    //@param(FileName: name of the file containing the input dataset)
    function  Execute(FileName : TFileName) : boolean;

    //Returns the information of a specific group inserted on the structure
    function  GetGroup(index : integer) : TResultGroup;

    //Updates the interface according to the definition of the structure
    //@param(ResetColumns: Identifies if the columns need to be reseted)
    //@param(ResetGroups: Identifies if the groups need to be reseted)
    //@param(ResetLong: Identifies if the longitudinal groups need to be reseted)
    procedure UpdateInterface(ResetColumns, ResetGroups, ResetLong : boolean);


  end;

//-------------------------------------------------------------------------------------------------

var
  FmChConf: TFmChConf;

//-------------------------------------------------------------------------------------------------

implementation

uses UnChVis, UnDialDoubleFloat;

{$R *.dfm}

//-------------------------------------------------------------------------------------------------

{ TFmChConf }

//-------------------------------------------------------------------------------------------------

procedure TFmChConf.AddGroup(func: TResultTransf; mode: TResultGrouping);
  var
    tmp : TResultGroup;
    s : string;
  begin
  tmp := TResultGroup.Create(FStructure.ColumnCount);
  tmp.Transf := func;
  tmp.GroupKind := mode;
  s := 'Group ' + inttostr(FStructure.GroupCount);
  FStructure.AddGroup(s, tmp);
  end;

//-------------------------------------------------------------------------------------------------

procedure TFmChConf.Button1Click(Sender: TObject);
  var i : integer;
  begin
  for i := ScrewBox3.ControlCount - 1 downto 0 do
    ScrewBox3.Controls[i].Visible := true;
  AddGroup(RtSimple, RgSingle);
  UpdateInterface(false, true, false);
  end;

//-------------------------------------------------------------------------------------------------

procedure TFmChConf.CBChange(Sender: TObject);
  var
    d1, d2 : double;
//    K : TResultGroup;
  begin
  if (sender is TComboBox) and ((sender as TComboBox).ItemIndex >= 7) then
     with  FStructure.Groups[(sender as TCombobox).Tag] do
       begin
       d1 := SelData;
       d2 := MaxDiff;
       if FmDoubleFloat.Execute(d1, d2) then
          begin
          SelData := d1;
          MaxDiff := d2;
          end;
       end;
  UpdateVariables;
  end;

//-------------------------------------------------------------------------------------------------


procedure TFmChConf.CheckBox3Click(Sender: TObject);
  var i : integer;
  begin
  if CheckBox3.State = cbChecked then
     for i := 0 to FStructure.SequenceSize - 1 do
       FStructure.SequenceMarks[i] := true
  else if CheckBox3.State = cbUnchecked then
     for i := 0 to FStructure.SequenceSize - 1 do
       FStructure.SequenceMarks[i] := false;
  UpdateInterface(false, false, false);
  end;

//-------------------------------------------------------------------------------------------------

procedure TFmChConf.ComboBox3Change(Sender: TObject);
  var i, j : integer;
  begin
  case ComboBox3.ItemIndex of
    1:
      begin
      if FStructure.GroupCount > FStructure.ColumnCount then
         for i := FStructure.GroupCount - 1 downto FStructure.ColumnCount do
           FStructure.DeleteGroup(i)
      else if FStructure.GroupCount < FStructure.ColumnCount then
         for i := FStructure.GroupCount to FStructure.ColumnCount - 1 do
            AddGroup(RtSimple,RgSingle);
      for i := 0 to FStructure.GroupCount - 1 do
        for j := 0 to FStructure.ColumnCount - 1 do
          FStructure.Marks[i, j] := (i = j);
      UpdateInterface(false, true, false);
      end;
    2:
      begin
      if FStructure.GroupCount > (FStructure.ColumnCount div 2)then
         for i := FStructure.GroupCount - 1 downto (FStructure.ColumnCount div 2) do
           FStructure.DeleteGroup(i)
      else if FStructure.GroupCount < (FStructure.ColumnCount div 2) then
         for i := FStructure.GroupCount to (FStructure.ColumnCount div 2) - 1 do
            AddGroup(RtSimple,RgDiff);
      for i := 0 to FStructure.GroupCount - 1 do
        for j := 0 to FStructure.ColumnCount - 1 do
          FStructure.Marks[i, j] := ((j div 2) = i);
      UpdateInterface(false, true, false);
      end;
    3:
      begin
      if FStructure.GroupCount > ((FStructure.ColumnCount div 2) * 4) then
         for i := FStructure.GroupCount - 1 downto ((FStructure.ColumnCount div 2) * 4) do
           FStructure.DeleteGroup(i)
      else if FStructure.GroupCount < ((FStructure.ColumnCount div 2) * 4) then
         for i := FStructure.GroupCount to ((FStructure.ColumnCount div 2) * 4) - 1 do
            AddGroup(RtSimple,RgDiff);
      for i := 0 to (FStructure.GroupCount div 4) - 1 do
        begin
        FStructure.Groups[(i * 4)].GroupKind     := RgSingle;
        FStructure.Groups[(i * 4) + 1].GroupKind := RgDiff;
        FStructure.Groups[(i * 4) + 2].GroupKind := RgDiff;
        FStructure.Groups[(i * 4) + 3].GroupKind := RgSingle;

        FStructure.Groups[(i * 4)].Transf := RtSumCount;
        FStructure.Groups[(i * 4)].SelData := 0;
        FStructure.Groups[(i * 4)].MaxDiff := 0.5;

        FStructure.Groups[(i * 4) + 1].Transf  := RtRightCount;
        FStructure.Groups[(i * 4) + 1].SelData := -1;
        FStructure.Groups[(i * 4) + 1].MaxDiff := 0.5;

        FStructure.Groups[(i * 4) + 2].Transf := RtRightCount;
        FStructure.Groups[(i * 4) + 2].SelData := 1;
        FStructure.Groups[(i * 4) + 2].MaxDiff := 0.5;

        FStructure.Groups[(i * 4) + 3].Transf := RtSumCount;
        FStructure.Groups[(i * 4) + 3].SelData := 2;
        FStructure.Groups[(i * 4) + 3].MaxDiff := 0.5;



        for j := 0 to FStructure.ColumnCount - 1 do
          begin
          FStructure.Marks[(i * 4), j] := ((j div 2) = i);
          FStructure.Marks[(i * 4) + 1, j] := ((j div 2) = i);
          FStructure.Marks[(i * 4) + 2, j] := ((j div 2) = i);
          FStructure.Marks[(i * 4) + 3, j] := ((j div 2) = i);
          end;
        end;
      UpdateInterface(false, true, false);
      end;
    4:
      begin
      if FStructure.GroupCount > ((FStructure.ColumnCount div 2) * 4) then
         for i := FStructure.GroupCount - 1 downto ((FStructure.ColumnCount div 2) * 4) do
           FStructure.DeleteGroup(i)
      else if FStructure.GroupCount < ((FStructure.ColumnCount div 2) * 4) then
         for i := FStructure.GroupCount to ((FStructure.ColumnCount div 2) * 4) - 1 do
            AddGroup(RtSimple,RgDiff);
      for i := 0 to (FStructure.GroupCount div 4) - 1 do
        begin
        FStructure.Groups[(i * 4)].GroupKind     := RgSingle;
        FStructure.Groups[(i * 4) + 1].GroupKind := RgDiff;
        FStructure.Groups[(i * 4) + 2].GroupKind := RgDiff;
        FStructure.Groups[(i * 4) + 3].GroupKind := RgSingle;

        FStructure.Groups[(i * 4)].Transf := RtSumCount;
        FStructure.Groups[(i * 4)].SelData := -2;
        FStructure.Groups[(i * 4)].MaxDiff := 1;

        FStructure.Groups[(i * 4) + 1].Transf  := RtRightCount;
        FStructure.Groups[(i * 4) + 1].SelData := -2;
        FStructure.Groups[(i * 4) + 1].MaxDiff := 1;

        FStructure.Groups[(i * 4) + 2].Transf := RtRightCount;
        FStructure.Groups[(i * 4) + 2].SelData := 2;
        FStructure.Groups[(i * 4) + 2].MaxDiff := 1;

        FStructure.Groups[(i * 4) + 3].Transf := RtSumCount;
        FStructure.Groups[(i * 4) + 3].SelData := 2;
        FStructure.Groups[(i * 4) + 3].MaxDiff := 1;

        for j := 0 to FStructure.ColumnCount - 1 do
          begin
          FStructure.Marks[(i * 4), j] := ((j div 2) = i);
          FStructure.Marks[(i * 4) + 1, j] := ((j div 2) = i);
          FStructure.Marks[(i * 4) + 2, j] := ((j div 2) = i);
          FStructure.Marks[(i * 4) + 3, j] := ((j div 2) = i);
          end;
        end;
      UpdateInterface(false, true, false);
      end;
    5:
      begin
      if FStructure.GroupCount > 4 then
         for i := FStructure.GroupCount - 1 downto 4 do
           FStructure.DeleteGroup(i)
      else if FStructure.GroupCount < 4 then
         for i := FStructure.GroupCount to 4 - 1 do
            AddGroup(RtSimple,RgDiff);

      FStructure.Groups[0].GroupKind     := RgSingle;
      FStructure.Groups[1].GroupKind := RgDiff;
      FStructure.Groups[2].GroupKind := RgDiff;
      FStructure.Groups[3].GroupKind := RgSingle;

      FStructure.Groups[0].Transf := RtSumCount;
      FStructure.Groups[0].SelData := 0;
      FStructure.Groups[0].MaxDiff := 0.5;

      FStructure.Groups[1].Transf  := RtRightCount;
      FStructure.Groups[1].SelData := -1;
      FStructure.Groups[1].MaxDiff := 0.5;

      FStructure.Groups[2].Transf := RtRightCount;
      FStructure.Groups[2].SelData := 1;
      FStructure.Groups[2].MaxDiff := 0.5;

      FStructure.Groups[3].Transf := RtSumCount;
      FStructure.Groups[3].SelData := 2;
      FStructure.Groups[3].MaxDiff := 0.5;

      for j := 0 to FStructure.ColumnCount - 1 do
        begin
        FStructure.Marks[0, j] := ((j div 2) = 0);
        FStructure.Marks[1, j] := ((j div 2) = 0);
        FStructure.Marks[2, j] := ((j div 2) = 0);
        FStructure.Marks[3, j] := ((j div 2) = 0);
        end;
      UpdateInterface(false, true, false);
      end;
    6:
      begin
      if FStructure.GroupCount > 4 then
         for i := FStructure.GroupCount - 1 downto 4 do
           FStructure.DeleteGroup(i)
      else if FStructure.GroupCount < 4 then
         for i := FStructure.GroupCount to 4 - 1 do
            AddGroup(RtSimple,RgDiff);

      FStructure.Groups[0].GroupKind     := RgSingle;
      FStructure.Groups[1].GroupKind := RgDiff;
      FStructure.Groups[2].GroupKind := RgDiff;
      FStructure.Groups[3].GroupKind := RgSingle;

      FStructure.Groups[0].Transf := RtSumCount;
      FStructure.Groups[0].SelData := -2;
      FStructure.Groups[0].MaxDiff := 1;

      FStructure.Groups[1].Transf  := RtRightCount;
      FStructure.Groups[1].SelData := -2;
      FStructure.Groups[1].MaxDiff := 1;

      FStructure.Groups[2].Transf := RtRightCount;
      FStructure.Groups[2].SelData := 2;
      FStructure.Groups[2].MaxDiff := 1;

      FStructure.Groups[3].Transf := RtSumCount;
      FStructure.Groups[3].SelData := 2;
      FStructure.Groups[3].MaxDiff := 1;

      for j := 0 to FStructure.ColumnCount - 1 do
        begin
        FStructure.Marks[0, j] := ((j div 2) = 0);
        FStructure.Marks[1, j] := ((j div 2) = 0);
        FStructure.Marks[2, j] := ((j div 2) = 0);
        FStructure.Marks[3, j] := ((j div 2) = 0);
        end;
      UpdateInterface(false, true, false);
      end;
    end;
  end;

//-------------------------------------------------------------------------------------------------

procedure TFmChConf.CreateChart1Click(Sender: TObject);
  begin
  CreateChart1.Checked := not CreateChart1.Checked;
  end;

//-------------------------------------------------------------------------------------------------

procedure TFmChConf.CreateOutput1Click(Sender: TObject);
  begin
  FStructure.ResetOutputChoices;
  if CreateChart1.Checked then
     FStructure.SetupChartAsOutput(FmChVis.Chart1, FmChVis); ///****Fill in properly later
  if SaveNewFile1.Checked then
     FStructure.SetupFileAsOutput(FOutFileName);
  FStructure.generateoutput;
  if CreateChart1.Checked then
     FmChVis.Execute;
  end;

//-------------------------------------------------------------------------------------------------

procedure TFmChConf.DeleteGroup(index: integer);
  begin
  FStructure.DeleteGroup(index);
  end;

//-------------------------------------------------------------------------------------------------

function TFmChConf.Execute(FileName : TFileName): boolean;
  begin
  if ((trim(FileName) <> '') and FileExists(FileName)) then
     begin
     if FStructure = nil then
        FStructure := THekatonResults.Create;
     FStructure.InputFileName := FileName;
     UpdateInterface(true, false, true);
     ShowModal;
     result := FOK;
     end
  else
     begin
     result := false;
     end;
  end;

//-------------------------------------------------------------------------------------------------


procedure TFmChConf.DeleteGroupEvent(Sender: TObject);
  var i : integer;
  begin
  for i := ScrewBox3.ControlCount - 1 downto 0 do
    ScrewBox3.Controls[i].Visible := true;
  if sender is TControl then
     begin
     DeleteGroup((sender as TControl).Tag);
     (sender as TControl).Visible := false;
     end;
  UpdateInterface(false, true, false);
  end;

//-------------------------------------------------------------------------------------------------

procedure TFmChConf.GeneralChange(Sender: TObject);
  begin
  UpdateVariables;
  end;

//-------------------------------------------------------------------------------------------------

function TFmChConf.GetGroup(index: integer) : TResultGroup;
  begin
  result := FStructure.Groups[index];
  end;

//-------------------------------------------------------------------------------------------------

function TFmChConf.GetGroupCount: integer;
  begin
  result := FStructure.GroupCount;
  end;

//-------------------------------------------------------------------------------------------------

function TFmChConf.GetLongFunction: TResultTransf;
  begin
  result := FStructure.SequenceFunction;
  end;

//-------------------------------------------------------------------------------------------------

function TFmChConf.GetLongGroup: boolean;
  begin
  result := FStructure.GroupSequence;
  end;

//-------------------------------------------------------------------------------------------------

function TFmChConf.GetLongMarks(index: integer): boolean;
  begin
  result := FStructure.SequenceMarks[index];
  end;

//-------------------------------------------------------------------------------------------------

function TFmChConf.GetLongSize: integer;
  begin
  result := FStructure.SequenceSize;
  end;

//-------------------------------------------------------------------------------------------------

function TFmChConf.GetMark(i, j: integer): boolean;
  begin
  result := FStructure.Marks[i, j];
  end;

//-------------------------------------------------------------------------------------------------

function TFmChConf.GetSubsetSelection(index: integer): boolean;
  begin
  result := FStructure.SubSetMarks[index]
  end;

//-------------------------------------------------------------------------------------------------

procedure TFmChConf.OpenReport2Click(Sender: TObject);
  begin
  if OpenDialog1.Execute then
     FStructure.InputFileName := OpenDialog1.FileName;
  UpdateInterface(true, true, true);
  end;

//-------------------------------------------------------------------------------------------------


procedure TFmChConf.RunScript1Click(Sender: TObject);
  var XTmpStructure : THekatonResults;
  begin
  XTmpStructure := THekatonResults.Create;
  if OpenDialog1.Execute then
     XTmpStructure.ProcessScript(OpenDialog1.FileName);
  XTmpStructure.Free;
  end;

//-------------------------------------------------------------------------------------------------

procedure TFmChConf.SaveNewFile1Click(Sender: TObject);
  begin

  SaveNewFile1.Checked := not SaveNewFile1.Checked;
  if SaveNewFile1.Checked then
     if SaveDialog1.Execute then
        FOutFileName := SaveDialog1.FileName
     else
        SaveNewFile1.Checked := false
 end;

//-------------------------------------------------------------------------------------------------

procedure TFmChConf.ScrewConfig(Columns, Groups : integer);
  var
    XWidth, XMinWidth, XHeight, XMinHeight : integer;
  begin
  XWidth     :=  (Columns * 32) + 16;
  XHeight    := (Groups  * 48) + 32;
  XMinWidth  := ScrewBack2.Width;
  XMinHeight := ScrewBack2.Height;

  if XWidth <= XMinWidth then
     begin
     ScrewBox2.Width := XMinWidth;
     ScrewBox6.Width := XMinWidth;
     ScrollBar1.Max := 0;
     end
  else
     begin
     ScrewBox2.Width := XWidth;
     ScrewBox6.Width := XWidth;
     ScrollBar1.Max := XWidth - XMinWidth;
     end;

  if XHeight < XMinHeight then
     begin
     ScrewBox2.Height := XMinHeight;
     ScrewBox3.Height := XMinHeight;
     ScrollBar2.Max := 0;
     end
  else
     begin
     ScrewBox2.Height := XHeight;
     ScrewBox3.Height := XHeight;
     ScrollBar2.Max := XHeight - XMinHeight;
     end;

  ScrollBar1.Min := 0;
  ScrollBar2.Min := 0;
  ScrollBar1.Position := 0;
  ScrollBar2.Position := 0;
  ScrewBox2.Left := 0;
  ScrewBox6.Left := 0;
  ScrewBox2.Top  := 0;
  ScrewBox3.Top  := 0;
  end;

//-------------------------------------------------------------------------------------------------

procedure TFmChConf.ScrollBar1Change(Sender: TObject);
  begin
  ScrewBox2.Left := - ScrollBar1.Position;
  ScrewBox6.Left := - ScrollBar1.Position;
  end;

//-------------------------------------------------------------------------------------------------

procedure TFmChConf.ScrollBar2Change(Sender: TObject);
  begin
  ScrewBox2.Top  := - ScrollBar2.Position;
  ScrewBox3.Top  := - ScrollBar2.Position;
  end;

//-------------------------------------------------------------------------------------------------

procedure TFmChConf.SetLongFunction(const Value: TResultTransf);
  begin
  FStructure.SequenceFunction := value;
  end;

//-------------------------------------------------------------------------------------------------

procedure TFmChConf.SetLongGroup(const Value: boolean);
  begin
  FStructure.GroupSequence := value;
  end;

//-------------------------------------------------------------------------------------------------

procedure TFmChConf.SetLongMarks(index: integer; const Value: boolean);
  begin
  FStructure.SequenceMarks[index] := value;
  end;

//-------------------------------------------------------------------------------------------------

procedure TFmChConf.SetLongSize(const Value: integer);
  begin
  FStructure.SequenceSize := value;
  end;

//-------------------------------------------------------------------------------------------------

procedure TFmChConf.SetMark(i, j: integer; const Value: boolean);
  begin
  FStructure.Marks[i, j] := value;
  end;

//-------------------------------------------------------------------------------------------------

procedure TFmChConf.SetSubSetSelection(index: integer; const Value: boolean);
  begin
  FStructure.SubSetMarks[index] := value;
  end;

//-------------------------------------------------------------------------------------------------

procedure TFmChConf.SpinEdit1Change(Sender: TObject);
  begin
  if trim(SpinEdit1.Text) <> '' then
     begin
     FStructure.SequenceSize := SpinEdit1.Value;
     UpdateInterface(false, false, true);
     UpdateVariables;
     end;
  end;

//-------------------------------------------------------------------------------------------------


{
|---------------| | ------------------------------------------------------------------|
|               | |               |-----------------------------------------------|   |
|               | |               |                      SB6                      |   |
|               | |               |-----------------------------------------------|   |
|               | ||----------------------------------------------------------------| |
|               | ||                               SB5                              | |
|               | || |------------||-----------------------------------------------|| |
|               | || |            ||                                               || |
|      SB1      | || |            ||                                               || |
|               | || |            ||                                               || |
|               | || |    SB3     ||                      SB2                      || |
|               | || |            ||                                               || |
|               | || |            ||                                               || |
|               | || |            ||                                               || |
|               | || |------------||-----------------------------------------------|| |
|               | ||----------------------------------------------------------------| |
|---------------| |-------------------------------------------------------------------|

                  |-------------------------------------------------------------------|
                  |                                                                   |
                  |                              SB4                                  |
                  |                                                                   |
                  |-------------------------------------------------------------------|
}

//-------------------------------------------------------------------------------------------------

procedure TFmChConf.UpdateInterface(ResetColumns, ResetGroups, ResetLong : boolean);
  var i, j : integer;
  begin
  FUpdateInterfaceMode := true;
  if trim(FStructure.SequenceSeparator) <> '' then
     begin
     CheckBox4.Caption := 'Use Sequence Separator (' + FStructure.SequenceSeparator + ')';
     CheckBox4.Enabled := true;
     end
  else
     begin
     CheckBox4.Caption := 'Use Sequence Separator';
     CheckBox4.Enabled := false;
     end;
  CheckBox5.Visible := CheckBox4.Enabled and CheckBox4.Checked;     
  if ResetColumns or ResetGroups then ScrewConfig(FStructure.ColumnCount, FStructure.GroupCount);
  if ResetColumns then
     begin
     for i := ScrollBox1.ControlCount - 1 downto 0 do
       ScrollBox1.Controls[i].Free;
     for i := ScrewBox6.ControlCount - 1 downto 0 do
       ScrewBox6.Controls[i].Free;
     SetLength(FSubsetCheckBoxes, FStructure.SubsetCount);
     for i := 0 to FStructure.SubsetCount - 1 do
       begin
       FSubsetCheckBoxes[i] := TCheckBox.Create(self);
       with FSubsetCheckBoxes[i] do
         begin
         Parent  := ScrollBox1;
         Top     := 8 + (24 * i);
         Left    := 8;
         Height  := 17;
         Width   := 100;
         Caption := 'Sub group ' + inttostr(i);
         Checked := true;
         OnClick := GeneralChange;
         end;
       end;
     for i := 0 to FStructure.ColumnCount - 1 do
       with TLabel.Create(self) do
         begin
         Parent  := ScrewBox6;
         AutoSize := false;
         Top     := 6;
         Left    := 16 + (32 * i);
         Height  := 13;
         Width   := 16;
         Alignment := taCenter;
         caption := inttostr(i);
         end;
     end;

  if ResetColumns or ResetGroups then
     begin
     for i := ScrewBox2.ControlCount - 1 downto 0 do
       ScrewBox2.Controls[i].Free;
     SetLength(FMarkCheckBoxes, FStructure.GroupCount, FStructure.ColumnCount);
     for i := 0 to FStructure.GroupCount - 1 do
       for j := 0 to FStructure.ColumnCount - 1 do
         begin
         FMarkCheckBoxes[i, j] := TCheckBox.Create(self);
         with FMarkCheckBoxes[i, j] do
           begin
           Parent  := ScrewBox2;
           Top     := 16 + (48 * i);
           Left    := 16 + (32 * j);
           Height  := 15;
           Width   := 15;
           Checked := false;
           Caption := '';
           OnClick := GeneralChange;
           end;
         end;

     //Create new array of CheckBoxes (ScrollBox2)
     end;

  if ResetGroups then
     begin
     for i := ScrewBox3.ControlCount - 1 downto 0 do
       if ScrewBox3.Controls[i].Visible then
          ScrewBox3.Controls[i].Free;
     SetLength(FGroupControls, FStructure.GroupCount, 4);
     for i := 0 to FStructure.GroupCount - 1 do
       begin
       FGroupControls[i, 0] := TLabel.Create(self);
       with FGroupControls[i, 0] as TLabel do
         begin
         parent := ScrewBox3;
         top := 48 * i;
         left := 8;
         height := 13;
         width := 41;
         caption := 'Group ' + inttostr(i);
         end;
       FGroupControls[i, 1] := TCombobox.Create(self);
       with FGroupControls[i, 1] as TCombobox do
         begin
         parent := ScrewBox3;
         top := 16 + (48 * i);
         left := 8;
         height := 21;
         width := 80;
         Tag := i;
         OnChange := CBChange;
         for j := 0 to ComboBox1.Items.Count - 1 do
           Items.Add(Combobox1.Items.Strings[j]);
         end;
       FGroupControls[i, 2] := TCombobox.Create(self);
       with FGroupControls[i, 2] as TCombobox do
         begin
         parent := ScrewBox3;
         top := 16 + (48 * i);
         left := 96;
         height := 21;
         width := 81;
         Tag := i;
         OnChange := GeneralChange;
         Items.Add('Individual');
         Items.Add('Difference');
         end;
       FGroupControls[i, 3] := TButton.Create(self);
       with FGroupControls[i, 3] as TButton do
         begin
         parent := ScrewBox3;
         top := 16 + (48 * i);
         left := 184;
         height := 21;
         width := 21;
         caption := '-';
         font.Size := 10;
         Tag := i;
         OnClick := DeleteGroupEvent;
         end;
       end;
     //Create new array of controls to configure groups (ScrollBox3)
     end;

  if ResetLong then
     begin
     for i := ScrollBox4.ControlCount - 1 downto 0 do
       ScrollBox4.Controls[i].Free;
//     if ComboBox2.Itemindex = 0 then
     SetLength(FLongitCheckBoxes, FStructure.SequenceSize);
     for i := 0 to FStructure.SequenceSize - 1 do
       begin
       FLongitCheckBoxes[i] := TCheckBox.Create(self);
       with FLongitCheckBoxes[i] do
         begin
         parent := ScrollBox4;
         top := 16;
         left := 16 + (56 * i);
         width := 48;
         height := 17;
         checked := true;
         caption := inttostr(i);
         OnClick := GeneralChange;
         end;
       end;
     //Create new array of checkboxes for longitudinal grouping (ScrollBox4)
     end;

  for i := 0 to FStructure.SubsetCount - 1 do
    FSubsetCheckBoxes[i].Checked := FStructure.SubsetMarks[i];

  for i := 0 to FStructure.GroupCount - 1 do
    begin
    for j := 0 to FStructure.ColumnCount - 1 do
      if FMarkCheckBoxes[i, j] <> nil then
         FMarkCheckBoxes[i, j].Checked := FStructure.Marks[i, j];
    (FGroupControls[i, 1] as TComboBox).ItemIndex := integer(Fstructure.Groups[i].Transf);
    (FGroupControls[i, 2] as TComboBox).ItemIndex := integer(Fstructure.Groups[i].GroupKind);
    end;

  SpinEdit1.Value := FStructure.SequenceSize;
  ComboBox1.ItemIndex := integer(FStructure.SequenceFunction);
  for i := 0 to FStructure.SequenceSize - 1 do
    FLongitCheckBoxes[i].Checked := FStructure.SequenceMarks[i];
  if FStructure.GroupSequence then
     ComboBox2.ItemIndex := 0
  else
     ComboBox2.ItemIndex := 1;

  FUpdateInterfaceMode := false;
  end;

//-------------------------------------------------------------------------------------------------

procedure TFmChConf.UpdateVariables;
  var i, j : integer;
      Xch, xUn : boolean;
  begin
  if not FUpdateInterfaceMode then
     begin
     for i := 0 to FStructure.SubsetCount - 1 do
       FStructure.SubSetMarks[i] := FSubsetCheckBoxes[i].Checked;

     for i := 0 to FStructure.GroupCount - 1 do
       begin
       for j := 0 to FStructure.ColumnCount - 1 do
         if FMarkCheckBoxes[i, j] <> nil then
            FStructure.Marks[i, j] := FMarkCheckBoxes[i, j].Checked;
       FStructure.Groups[i].Transf    := TResultTransf((FGroupControls[i, 1] as TComboBox).ItemIndex);
       FStructure.Groups[i].GroupKind := TResultGrouping((FGroupControls[i, 2] as TComboBox).ItemIndex);
       end;

     FStructure.GroupSequence    := ComboBox2.ItemIndex = 0;
     FStructure.SequenceFunction := TResultTransf(ComboBox1.ItemIndex);
     FStructure.SequenceGrouping := TResultGrouping(ComboBox2.ItemIndex);
     XCh := false;
     XUn := false;
     for i := 0 to FStructure.SequenceSize - 1 do
       begin
       FStructure.SequenceMarks[i] := FLongitCheckBoxes[i].Checked;
       XCh := XCh or FLongitCheckBoxes[i].Checked;
       XUn := XUn or not FLongitCheckBoxes[i].Checked;
       end;
     if XCh and XUn then CheckBox3.State := CbGrayed
     else if XCh then    CheckBox3.State := CbChecked
     else if XUn then    CheckBox3.State := CbUnchecked;

//     UpdateInterface(false, false, true);

     end;
  end;

//-------------------------------------------------------------------------------------------------

procedure TFmChConf.CheckBox4Click(Sender: TObject);
  begin
  CheckBox5.Visible := CheckBox4.Enabled and CheckBox4.Checked;
  FStructure.UseSequenceBreak := CheckBox4.Enabled and CheckBox4.Checked;
  UpdateInterface(false, false, true);
  end;

procedure TFmChConf.CheckBox5Click(Sender: TObject);
  begin
  FStructure.LongLastMark := CheckBox5.Checked;
  end;

end.
