{
  @abstract(Unit describing the dialog to set up the features of each column in the input files)
  @author(Rafa "Passarel" Borges <passarel@gmail.com>)
  @created(Porto Alegre, 2006)
  @lastmod(London, January, 2010)
}

unit UnDialColumn;

interface

uses
   Classes, Controls, Dialogs, ExtCtrls, Forms, Graphics, Messages,
   Spin, StdCtrls, SysUtils, Variants, Windows;

//-------------------------------------------------------------------------------------------------

type
  TFmColDial = class(TForm)
    Button1: TButton;
    Button2: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    SpinEdit1: TSpinEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Edit2Exit(Sender: TObject);
    procedure Edit3Exit(Sender: TObject);    
  private
    //Identifies if the OK button was pressed
    FExecuted : boolean;
    //Minimum value of the column
    FMin : double;
    //Maximum value of the column
    FMax : double;
  public
    //Shows the form, updating the fields
    //@param(VLabel : variable parameter identifying the label of the column)
    //@param(VIn : variable parameter identifying if the column is used as input)
    //@param(VOut : variable parameter identifying if the column is used as output)
    //@param(VDelay : variable parameter identifying the delay to be applied on the column's values)
    //@param(VMin : variable parameter identifying the minimum value on the column)
    //@param(VMax : variable parameter identifying the maximum value on  the column)
    function execute(var VLabel : string; var Vin, Vout : boolean;
                     var VDelay : integer; var VMin, VMax : double) : boolean;
  end;

//-------------------------------------------------------------------------------------------------

var
  FmColDial: TFmColDial;

//-------------------------------------------------------------------------------------------------

implementation

{$R *.dfm}

//-------------------------------------------------------------------------------------------------

{ TFmColDial }

//-------------------------------------------------------------------------------------------------

procedure TFmColDial.Button1Click(Sender: TObject);
  begin
  FExecuted := true;
  Close;
  end;

//-------------------------------------------------------------------------------------------------

procedure TFmColDial.Button2Click(Sender: TObject);
  begin
  FExecuted := false;
  Close;
  end;

//-------------------------------------------------------------------------------------------------

procedure TFmColDial.Edit2Exit(Sender: TObject);
  var temp : double;
  begin
  temp := FMax;
  try
    FMax := strtoFloat(Edit2.Text);
  except
    FMax := temp;
    Edit2.Text := floattoStr(FMax);
    end;
  end;

//-------------------------------------------------------------------------------------------------

procedure TFmColDial.Edit3Exit(Sender: TObject);
  var temp : double;
  begin
  temp := FMin;
  try
    FMin := strtoFloat(Edit3.Text);
  except
    FMin := temp;
    Edit3.Text := floattoStr(FMin);
    end;
  end;

//-------------------------------------------------------------------------------------------------

function TFmColDial.execute(var VLabel: string; var Vin, Vout: boolean;
  var VDelay: integer; var VMin, VMax: double): boolean;
  begin
  Edit1.Text := VLabel;
  CheckBox1.Checked := VIn;
  CheckBox2.Checked := VOut;
  SpinEdit1.Value := VDelay;

  FMin := VMin;
  FMax := VMax;

  Edit3.Text := floattoStr(VMin);
  Edit2.Text := floattoStr(VMax);

  ShowModal;

  if FExecuted then
     begin
     VLabel := Edit1.Text;
     VIn    := CheckBox1.Checked;
     VOut   := CheckBox2.Checked;
     VDelay := SpinEdit1.Value;

     VMin := FMin;
     VMax := FMax;

     result := true;
     end
  else
     result := false;
  end;

//-------------------------------------------------------------------------------------------------

end.
