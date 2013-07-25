unit UnDialMerge;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, UnNetRep,
  Dialogs, StdCtrls, Spin;

type
  TFmMerge = class(TForm)
    ComboBox1: TComboBox;
    SpinEdit1: TSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    Button1: TButton;
    Button2: TButton;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
  private
    IsOk : boolean;
  public
    function Execute(out MergeStyle : TMergeStyle; out LinkLevel : integer) : boolean;
  end;

var
  FmMerge: TFmMerge;

implementation

{$R *.dfm}

procedure TFmMerge.Button2Click(Sender: TObject);
  begin
  close;
  end;

function TFmMerge.Execute(out MergeStyle: TMergeStyle; out LinkLevel: integer): boolean;
  begin
  IsOk := False;
  SpinEdit1.Enabled := ComboBox1.ItemIndex > 0;
  ShowModal;
  MergeStyle := TMergeStyle(ComboBox1.ItemIndex);
  LinkLevel := SpinEdit1.Value;
  result := isOk;
  end;

procedure TFmMerge.Button1Click(Sender: TObject);
  begin
  IsOk := true;
  Close;
  end;

procedure TFmMerge.ComboBox1Change(Sender: TObject);
  begin
  SpinEdit1.Enabled := ComboBox1.ItemIndex > 0;
  end;

end.
