unit UnFunct;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, UnFunctions, UnNetRep, StdCtrls, ExtCtrls, ComCtrls, Spin, TeeProcs,
  TeEngine, Chart, Series;

type
  TFmFunct = class(TForm)
    Chart1: TChart;
    ComboBox1: TComboBox;
    Label1: TLabel;
    GroupBox1: TGroupBox;
    SpinEdit1: TSpinEdit;
    Label2: TLabel;
    SpinEdit2: TSpinEdit;
    Label3: TLabel;
    TrackBar1: TTrackBar;
    GroupBox2: TGroupBox;
    Label4: TLabel;
    Label5: TLabel;
    SpinEdit3: TSpinEdit;
    SpinEdit4: TSpinEdit;
    TrackBar2: TTrackBar;
    GroupBox3: TGroupBox;
    Label6: TLabel;
    Label7: TLabel;
    SpinEdit5: TSpinEdit;
    SpinEdit6: TSpinEdit;
    TrackBar3: TTrackBar;
    RadioGroup1: TRadioGroup;
    CheckBox1: TCheckBox;
    Series1: TLineSeries;
    Series2: TLineSeries;
    GroupBox4: TGroupBox;
    UpDown1: TUpDown;
    UpDown2: TUpDown;
    Label8: TLabel;
    Label9: TLabel;
    UpDown3: TUpDown;
    UpDown4: TUpDown;
    procedure SpinEdit1Change(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure UpDown1Click(Sender: TObject; Button: TUDBtnType);
  private
    FPar1 : double;
    FPar2 : double;
    FPar3 : double;
    FFunction : TParamFunction;
    FDerivated : TParamFunction;

    function MergeFunction(x : double; p : array of double) : double;
    function MergeDerivated(x: double; p: array of double): double;
    procedure UpdateFunction;
    procedure UpdateParameters;
    procedure PrintChart;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FmFunct: TFmFunct;

implementation

{$R *.dfm}

{ TFmFunct }

function TFmFunct.MergeFunction(x: double; p: array of double): double;
  begin
  if abs(FFunction(x, p)) > abs(x) then
     result := x
  else
     result := FFunction(x, p);
  end;

function TFmFunct.MergeDerivated(x: double; p: array of double): double;
  begin
  if FDerivated(x, p) > 1 then
     result := 1
  else
     result := FDerivated(x, p);
  end;

procedure TFmFunct.PrintChart;
  var
    i : integer;
    sc : double;
  begin
  if UpDown1.Position >= 0 then
     sc := 1 + Updown1.Position
  else
     sc := 1 / (- UpDown1.position);

  Chart1.LeftAxis.Minimum := (UpDown3.Position * (sc/10)) - sc;
  Chart1.LeftAxis.Maximum := (UpDown3.Position * (sc/10)) + sc;

  if CheckBox1.Checked then  //Print the derivated function
     begin
     case RadioGroup1.ItemIndex of
       0:
         begin
         Series1.Clear;
         Series1.Active := true;
         Series2.Active := false;
         for i := -5000 to 5000 do
           begin
           Series1.AddXY(i/1000, FDerivated(i/1000, [FPar1, FPar2, FPar3]));
           end;
         end;
       1:
         begin
         Series1.Clear;
         Series2.Clear;
         Series1.Active := true;
         Series2.Active := true;
         for i := -5000 to 5000 do
           begin
           Series1.AddXY(i/1000, FDerivated(i/1000, [FPar1, FPar2, FPar3]));
           Series2.AddXy(i/1000, 1);
           end;
         end;
       2:
         begin
         Series1.Clear;
         Series1.Active := true;
         Series2.Active := false;
         for i := -5000 to 5000 do
           begin
           Series1.AddXY(i/1000, MergeDerivated(i/1000, [FPar1, FPar2, FPar3]));
           end;
         end;
       end
     end
  else
     begin  //Print the original function
     case RadioGroup1.ItemIndex of
       0:
         begin
         Series1.Active := true;
         Series2.Active := false;
         Series1.Clear;
         for i := -5000 to 5000 do
           begin
           Series1.AddXY(i/1000, FFunction(i/1000, [FPar1, FPar2, FPar3]));
           end;
         end;
       1:
         begin
         Series1.Clear;
         Series2.Clear;
         Series1.Active := true;
         Series2.Active := true;
         for i := -5000 to 5000 do
           begin
           Series1.AddXY(i/1000, FFunction(i/1000, [FPar1, FPar2, FPar3]));
           Series2.AddXy(i/1000, i/1000);
           end;
         end;
       2:
         begin
         Series1.Clear;
         Series1.Active := true;
         Series2.Active := false;
         for i := -5000 to 5000 do
           begin
           Series1.AddXY(i/1000, MergeFunction(i/1000, [FPar1, FPar2, FPar3]));
           end;
         end;
       end
     end
  end;

procedure TFmFunct.UpdateFunction;
  begin
//  logsig  tansig BiLogsig  Threshold BiThreshold  Linear  PolynomialA  PolynomialB  TrigonometricA
  case Combobox1.ItemIndex of
    0:
      begin
      SpinEdit1.Value := 0;
      SpinEdit2.Value := 0;
      SpinEdit3.Value := 0;
      SpinEdit4.Value := 0;
      SpinEdit5.Value := 0;
      SpinEdit6.Value := 0;
//      Trackbar1.Position := 50;
      FFunction := LogSig;
      FDerivated := LogSig_linha;
      end;
    1:
      begin
      SpinEdit1.Value := 0;
      SpinEdit2.Value := 0;
      SpinEdit3.Value := 0;
      SpinEdit4.Value := 0;
      SpinEdit5.Value := 0;
      SpinEdit6.Value := 0;
//      Trackbar1.Position := 50;
      FFunction := tansig;
      FDerivated := tansig_linha;
      end;
    2:
      begin
      SpinEdit1.Value := 0;
      SpinEdit2.Value := 5;
      SpinEdit3.Value := 0;
      SpinEdit4.Value := 0;
      SpinEdit5.Value := 0;
      SpinEdit6.Value := 0;
//      Trackbar1.Position := 50;
      FFunction := BiLogSig;
      FDerivated := BiLogSig_linha;
      end;
    3:
      begin
      SpinEdit1.Value := 0;
      SpinEdit2.Value := 0;
      SpinEdit3.Value := 0;
      SpinEdit4.Value := 0;
      SpinEdit5.Value := 0;
      SpinEdit6.Value := 0;
//      Trackbar1.Position := 50;
      FFunction := Threshold;
      FDerivated := Threshold_linha;
      end;
    4:
      begin
      SpinEdit1.Value := 0;
      SpinEdit2.Value := 0;
      SpinEdit3.Value := 0;
      SpinEdit4.Value := 0;
      SpinEdit5.Value := 0;
      SpinEdit6.Value := 0;
//      Trackbar1.Position := 50;
      FFunction := BiThreshold;
      FDerivated := BiThreshold_linha;
      end;
    5:
      begin
      SpinEdit1.Value := 0;
      SpinEdit2.Value := 10;
      SpinEdit3.Value := 0;
      SpinEdit4.Value := 0;
      SpinEdit5.Value := 0;
      SpinEdit6.Value := 0;
//      Trackbar1.Position := 50;
      FFunction := Linear;
      FDerivated := Linear_linha;
      end;
    6:
      begin
      SpinEdit1.Value := 0;
      SpinEdit2.Value := 1;
      SpinEdit3.Value := 0;
      SpinEdit4.Value := 2;
      SpinEdit5.Value := 0;
      SpinEdit6.Value := 0;
//      Trackbar1.Position := 50;
      FFunction := PolynomialA;
      FDerivated := PolynomialA_linha;
      end;
    7:
      begin
      SpinEdit1.Value := 0;
      SpinEdit2.Value := 0;
      SpinEdit3.Value := 0;
      SpinEdit4.Value := 0;
      SpinEdit5.Value := 0;
      SpinEdit6.Value := 0;
//      Trackbar1.Position := 50;
      FFunction := PolynomialB;
      FDerivated := PolynomialB_linha;
      end;
    8:
      begin
      SpinEdit1.Value := 0;
      SpinEdit2.Value := 0;
      SpinEdit3.Value := 0;
      SpinEdit4.Value := 0;
      SpinEdit5.Value := 0;
      SpinEdit6.Value := 0;
//      Trackbar1.Position := 50;
      FFunction := TrigonometricA;
      FDerivated := TrigonometricA_linha;
      end;
    end;
  end;

procedure TFmFunct.UpdateParameters;
  begin
  FPar1 := ((TrackBar1.Position/100) * (SpinEdit2.Value - SpinEdit1.Value))
           + SpinEdit1.Value;
  FPar2 := ((TrackBar2.Position/100) * (SpinEdit4.Value - SpinEdit3.Value))
           + SpinEdit3.Value;
  FPar3 := ((TrackBar3.Position/100) * (SpinEdit6.Value - SpinEdit5.Value))
           + SpinEdit5.Value;
  end;

procedure TFmFunct.SpinEdit1Change(Sender: TObject);
  begin
  if trim(SpinEdit1.Text) <> '' then
     begin
     UpdateParameters;
     PrintChart;
     end;
  end;

procedure TFmFunct.CheckBox1Click(Sender: TObject);
  begin
  PrintChart;
  end;

procedure TFmFunct.RadioGroup1Click(Sender: TObject);
  begin
  PrintChart;
  end;

procedure TFmFunct.ComboBox1Change(Sender: TObject);
  begin
  UpdateFunction;
  UpdateParameters;
  PrintChart;
  end;

procedure TFmFunct.UpDown1Click(Sender: TObject; Button: TUDBtnType);
  begin
  PrintChart;
  end;

end.
