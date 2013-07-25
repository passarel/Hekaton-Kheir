unit UnLineComp;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, Impstringgrid, StdCtrls;

type TLineSelectionType = (STMin, STMax, StAll, STNone);

type TRafaLineDistanceArray = class
  private
    FSize : integer;
    FData : array of array of double;
    function GetSize : integer;
    function GetData(i, j : integer) : double;
    procedure SetData(i, j : integer; value : double);
  public
    constructor Create(size : integer);
    destructor  Destroy; override;
    property Size : integer read GetSize;
    property Data[i, j: integer] : double read GetData write SetData;
  end;

type
  TFmLineComp = class(TForm)
    ImpStringgrid1: TImpStringgrid;
    GroupBox1: TGroupBox;
    Button1: TButton;
    ComboBox1: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Edit1: TEdit;
    procedure Button1Click(Sender: TObject);
  private
    FNumberOfLines : integer;
    FNumberOfOutputs : integer;
    FDistances: TRafaLineDistanceArray;
    FOutputs  : array of array of double;
    FHasConflict : array of boolean;
    FIsLineSelected : array of boolean;
    function GetLineSelected(index : integer) : boolean;
    procedure InvertSelection(OnlyConflicting : boolean);
    function SelectConflicted(MaxDistance : double; SelectionType : TLineSelectionType; Output : array of integer) : integer;

    procedure ShowOutputs;
    procedure ShowDistances;

  public
    property IsLineSelected[index : integer] : boolean read GetLineSelected;
    procedure Execute(lineCount, outputCount : integer; outputs : array of double; distances : TRafaLineDistanceArray);


  end;

var
  FmLineComp: TFmLineComp;

implementation

{$R *.dfm}

function TFmLineComp.GetLineSelected(index : integer) : boolean;
  begin
  if (index >= 0) and (index < FNumberOfLines) then
     result := FIsLineSelected[index]
  else
     result := false;
  end;

procedure TFmLineComp.InvertSelection(OnlyConflicting : boolean);
  var i : integer;
  begin
  for i := 0 to FNumberOfLines - 1 do
    if (not OnlyConflicting) or (FHasConflict[i]) then
       FIsLineSelected[i] := not FIsLineSelected[i];
  end;

function TFmLineComp.SelectConflicted(MaxDistance : double; SelectionType : TLineSelectionType;
         Output : array of integer) : integer;
  var
    i, j, k : integer;
    XCount : integer;
    stop : boolean;
  begin
  for i := 0 to FNumberOfLines - 1 do
    begin
    FHasConflict[i] := false;
    FIsLineSelected[i] := false;
    end;
  XCount := 0;
  for i := 0 to FNumberOfLines - 1 do
    for j := i + 1 to FNumberOfLines - 1 do
      begin
      if FDistances.Data[i, j] <= MaxDistance then
         begin
         FHasConflict[i] := true;
         FHasConflict[j] := true;
         case SelectionType of
           STMax :
             begin
             k := 0;
             stop := false;
             while (k < length(output)) and not stop do
               begin
               stop := true;
               if FOutputs[i, output[k]] > FOutputs[j, output[k]] then
                  begin
                  if not FIsLineSelected[i] then XCount := XCount + 1;
                  FIsLineSelected[i] := true;
                  end
               else if FOutputs[j, output[k]] > FOutputs[i, output[k]] then
                  begin
                  if not FIsLineSelected[j] then XCount := XCount + 1;
                  FIsLineSelected[j] := true
                  end
               else
                  stop := false;
               k := k + 1;
               end
             end;
           STMin :
             begin
             k := 0;
             stop := false;
             while (k < length(output)) and not stop do
               begin
               stop := true;
               if FOutputs[i, output[k]] < FOutputs[j, output[k]] then
                  begin
                  if not FIsLineSelected[i] then XCount := XCount + 1;
                  FIsLineSelected[i] := true;
                  end
               else if FOutputs[j, output[k]] < FOutputs[i, output[k]] then
                  begin
                  if not FIsLineSelected[j] then XCount := XCount + 1;
                  FIsLineSelected[j] := true
                  end
               else
                  stop := false;
               k := k + 1;
               end
             end;
           STAll :
             begin
             if not FIsLineSelected[i] then XCount := XCount + 1;
             FIsLineSelected[i] := true;
             if not FIsLineSelected[j] then XCount := XCount + 1;
             FIsLineSelected[j] := true;
             end;
           StNone :
             begin
             FIsLineSelected[i] := false;
             FIsLineSelected[j] := false;
             end;
           end;
         end;
      end;
  result := XCount;
  end;

procedure TFmLineComp.Execute(lineCount, outputCount : integer; outputs : array of double;
                distances : TRafaLineDistanceArray);
  var i, j : integer;
  begin
  SetLength(FOutputs, lineCount, outputCount);
  SetLength(FHasConflict, lineCount);
  SetLength(FIsLineSelected, lineCount);
  FDistances := distances;
  FNumberOfLines := lineCount;
  FNumberOfOutputs := outputCount;
  for i := 0 to lineCount - 1 do
    begin
    FHasConflict[i] := false;
    FIsLineSelected[i] := false;
    for j := 0 to outputCount - 1 do
      FOutputs[i, j] := outputs[(i * outputCount) + j]
    end;
  ShowDistances;
  ShowModal;
  end;

procedure TFmLineComp.ShowDistances;
  var
    i, j : integer;
  begin
  ImpStringgrid1.RowCount := FNumberOfLines + 1;
  ImpStringgrid1.ColCount := FNumberOfLines + 1;
  for i := 0 to FNumberOfLines - 1 do
    begin
    ImpStringgrid1.Cells[i + 1, i + 1] := FormatFloat('0.####', FOutputs[i, 0]);
    for j := i + 1 to FNumberOfLines - 1 do
       ImpStringgrid1.Cells[i + 1, j + 1] := FormatFloat('0.####', FDistances.Data[i, j]);
    end;
  end;


procedure TFmLineComp.ShowOutputs;
  var
    i, j : integer;
  begin
  ImpStringgrid1.RowCount := FNumberOfLines + 1;
  ImpStringgrid1.ColCount := FNumberOfOutputs + 1;
  for i := 0 to FNumberOfLines - 1 do
    for j := 0 to FNumberOfOutputs - 1 do
       ImpStringgrid1.Cells[i + 1, j + 1] := FormatFloat('0.####', FOutputs[i, j]);
  end;

//-------------------------------------------------------------------------------------------------

  {TRafaLineDistanceArray}

//-------------------------------------------------------------------------------------------------

constructor TRafaLineDistanceArray.Create(size : integer);
  begin
  inherited Create;
  FSize := size;
  SetLength(FData, size, size);
  end;

//-------------------------------------------------------------------------------------------------

destructor  TRafaLineDistanceArray.Destroy;
  begin
  SetLength(FData, 0, 0);
  inherited Destroy;
  end;

//-------------------------------------------------------------------------------------------------

function TRafaLineDistanceArray.GetData(i, j : integer) : double;
  begin
  if (i >= 0) and (i < FSize) and (j >= 0) and (j < FSize) then
     result := FData[i, j]
  else
     result := 0;
  end;

//-------------------------------------------------------------------------------------------------

function TRafaLineDistanceArray.GetSize : integer;
  begin
  result := FSize;
  end;

//-------------------------------------------------------------------------------------------------

procedure TRafaLineDistanceArray.SetData(i, j : integer; value : double);
  begin
  if (i >= 0) and (i < FSize) and (j >= 0) and (j < FSize) then
     FData[i, j] := value;
  end;

//-------------------------------------------------------------------------------------------------


procedure TFmLineComp.Button1Click(Sender: TObject);
  var
    XArOut : array of integer;
    i : integer;
  begin
  //Improve later
  SetLength(XArOut, 1);
  XArOut[0] := 0;
  case ComboBox1.ItemIndex of
    0 :  Label3.Caption := InttoStr(SelectConflicted(StrToFloat(Edit1.Text), STMax, XArOut));
    1 :  Label3.Caption := InttoStr(SelectConflicted(StrToFloat(Edit1.Text), STMin, XArOut));
    2 :  Label3.Caption := InttoStr(SelectConflicted(StrToFloat(Edit1.Text), STAll, XArOut));
    3 :  Label3.Caption := InttoStr(SelectConflicted(StrToFloat(Edit1.Text), STNone, XArOut));
    end;
  for i := 0 to FNumberOfLines - 1 do
    if FIsLineSelected[i] then
       ImpStringgrid1.Columns[i + 1].Color := clSkyBlue
    else
       ImpStringgrid1.Columns[i + 1].Color := clWhite;
  end;


end.
