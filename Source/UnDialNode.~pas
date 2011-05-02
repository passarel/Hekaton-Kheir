{
  @abstract(Unit that describes the dialog for configuration of
    a new unit on the network)
  @author(Rafa "Passarel" Borges <passarel@gmail.com)
  @created()
  @lastmod()
}
unit UnDialNode; //71 lines

interface

uses Forms, StdCtrls, Controls, ExtCtrls, Classes, UnNetRep;

{------------------------------------------------------------------------------}

{
@abstract(Class encapsulating the visual inteface for configuration of a new unit)
}
type TFmNode = class(TForm)
    RadioGroup1: TRadioGroup;
    Label1: TLabel;
    Edit1: TEdit;
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    FOut : boolean;
    { Private declarations }
  public
    //
    function InsertNode(out s : string; out k : TUnitKind) : boolean;
    //
    function RenameNode(var s : string) : boolean;
  end;



{------------------------------------------------------------------------------}

var FmNode : TFmNode;

implementation

{$R *.dfm}

//uses

{------------------------------------------------------------------------------}

{ TFmNode }

function TFmNode.InsertNode(out s: string; out k: TUnitKind): boolean;
  begin
  Caption := 'Inserção de Unidade';
  Edit1.Text := '';
  RadioGroup1.Enabled := true;
  RadioGroup1.ItemIndex := 0;
  ShowModal;
  s := Edit1.Text;
  k := TUnitKind(RadioGroup1.ItemIndex);
  result := FOut;
  end;

function TFmNode.RenameNode(var s: string): boolean;
  begin
  Caption := 'Renomeação de Unidade';
  Edit1.Text := s;
  RadioGroup1.Enabled := false;
  RadioGroup1.ItemIndex := -1;
  ShowModal;
  if Fout then
     s := Edit1.Text;
  result := FOut;
  end;

procedure TFmNode.Button1Click(Sender: TObject);
  begin
  FOut := true;
  Close;
  end;

procedure TFmNode.Button2Click(Sender: TObject);
  begin
  FOut := False;
  Close;
  end;

end.
