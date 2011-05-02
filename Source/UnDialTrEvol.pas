{
  @abstract(Unit that describes the dialog that shows the evolution on the training)
  @author(Rafa "Passarel" Borges <passarel@gmail.com)
  @created()
  @lastmod()
}
unit UnDialTrEvol;

interface

uses Forms, Controls, StdCtrls, Gauges, Classes, TeEngine, Series,
  ExtCtrls, TeeProcs, Chart, ComCtrls, Dialogs;

{------------------------------------------------------------------------------}

{
@abstract(Class that encapsulates the visual interface to show the evolution of training)
}
type TFmTrEvol = class(TForm)
    Label1: TLabel;
    Gauge1: TGauge;
    Label2: TLabel;
    Button1: TButton;
    Gauge2: TGauge;
    Label3: TLabel;
    Chart1: TChart;
    Series1: TFastLineSeries;
    Series2: TFastLineSeries;
    Button2: TButton;
    Button3: TButton;
    UpDown1: TUpDown;
    Button4: TButton;
    SaveDialog1: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure UpDown1Click(Sender: TObject; Button: TUDBtnType);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    //
    Cancela : boolean;
    //
    CancelAll : boolean;
    //

    procedure UpdatePage(page : integer);
  public
    //
    //@param
    //@param
    //@param
    //@param
    //@param
    function Atualiza(var EpAtual : integer; EpTotal : integer;
                                         TrRMSE, RMSE : double; ValAtual, ValT : integer) : boolean;
    //
    procedure ResetDialog(OnlyTraining : boolean);
    procedure ResetCanc;    

  end;

{------------------------------------------------------------------------------}

var FmTrEvol : TFmTrEvol;

implementation

{$R *.dfm}

uses SysUtils;

{------------------------------------------------------------------------------}

{ TFmTrEvol }

function TFmTrEvol.Atualiza(var EpAtual : integer; EpTotal: integer;
                  TrRMSE, RMSE : double; ValAtual, ValT : integer): boolean;
  var i : integer;
  begin
  Chart1.MaxPointsPerPage := EpTotal;
  UpDatePage(ValAtual + 1);

  Series1.AddXY((ValAtual * EpTotal) + EpAtual, TrRMSE);
  if Series2.Active then
     Series2.AddXY((ValAtual * EpTotal) + EpAtual, RMSE);

  Label1.Caption := 'Executing training: Epoch ' + inttostr(EpAtual) +
                    ' in ' + intToStr(EpTotal);
  Gauge1.Progress := round((EpAtual / Eptotal) * 100 );
  Gauge2.Progress := round(((EpAtual / Eptotal) + ValAtual) * (100 / ValT));
  Label2.Caption := 'Root Mean Square Error = ' + floattostr(RMSE);

  Label3.Caption := 'Validation ' + inttostr(ValAtual + 1) + ' in ' + inttostr(ValT);


  if CancelAll then
     EpAtual := -1;
  Show;

  if cancela then
     for i := (ValAtual * EpTotal) + EpAtual + 1 to (ValAtual + 1) * EpTotal do
       begin
       Series1.AddXY(i, TrRMSE);
       Series2.AddXY(i, RMSE);
       end;

  result := Cancela;
  end;

procedure TFmTrEvol.FormCreate(Sender: TObject);
  begin
  Cancela := false;
  CancelAll := false;

  end;

procedure TFmTrEvol.Button1Click(Sender: TObject);
  begin
  Cancela := true
  end;

procedure TFmTrEvol.ResetDialog(OnlyTraining : boolean);
  begin
  Cancela := false;
  CancelAll := false;
  Chart1.Visible  := false;
  UpDown1.Visible := false;
  Button4.Visible := false;
  Series1.Clear;
  Series2.Clear;
  Button3.Caption := 'Show Chart...';
  Height := 215;
  Series1.Active := not onlytraining;
  end;

procedure TFmTrEvol.Button3Click(Sender: TObject);
  begin
  if Button3.Caption = 'Show Chart...' then
     begin
     Chart1.Visible  := true;
     UpDown1.Visible := true;
     Button4.Visible := true;
     Button3.Caption := 'Hide Chart';
     Height := 680;
     end
  else
     begin
     UpDown1.Visible := false;
     Chart1.Visible  := false;
     Button1.Visible := false;
     Button3.Caption := 'Show Chart...';
     Height := 215;
     end;
  end;

procedure TFmTrEvol.UpdatePage(page: integer);
  begin
  UpDown1.Position := page;
  Chart1.Page := page;
  Chart1.BottomAxis.Minimum := -100;
  Chart1.BottomAxis.Maximum := (page * Chart1.MaxPointsPerPage) - 1;
  Chart1.BottomAxis.Minimum := ((page - 1) * Chart1.MaxPointsPerPage);
  Chart1.Title.Text[0] := 'MSE Evolution Through Training - Validation ' + inttostr(page);
  end;

procedure TFmTrEvol.UpDown1Click(Sender: TObject; Button: TUDBtnType);
  begin
  UpdatePage(UpDown1.Position);
  end;

procedure TFmTrEvol.Button2Click(Sender: TObject);
  begin
  CancelAll := true;
  end;

procedure TFmTrEvol.ResetCanc;
  begin
  Cancela := false;
  end;

procedure TFmTrEvol.Button4Click(Sender: TObject);
  begin
  If SaveDialog1.Execute then
     Chart1.SaveToMetafile(SaveDialog1.FileName);
  end;

end.

