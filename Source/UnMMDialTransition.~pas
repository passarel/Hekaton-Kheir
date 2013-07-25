unit UnMMDialTransition;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, UnMMStructure, StdCtrls;

type
  TFmMMTransition = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    CheckBox1: TCheckBox;
    Button1: TButton;
    Button2: TButton;
    Label3: TLabel;
    ComboBox3: TComboBox;
    Label4: TLabel;
    Edit1: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    FOk : Boolean;
  public
    function Execute(Owner : TRafaMooreMachine; ListSt, ListIn : TStrings;
                    ArrSt, ArrIn : array of integer) : TRafaTransition;
  end;

var
  FmMMTransition: TFmMMTransition;

implementation

{$R *.dfm}

//--------------------------------------------------------------------------------------------------

function TFmMMTransition.Execute(Owner : TRafaMooreMachine; ListSt, ListIn : TStrings;
                    ArrSt, ArrIn : array of integer) : TRafaTransition;
  var
    i : integer;
    XTmpTransition : TRafaTransition;
  begin
  randomize;
  XTmpTransition := nil;
  if ListSt.Count <= 0 then
     MessageDlg('No states declared', MtError, [MbOK], 0)
  else if ListIn.Count <= 0 then
     MessageDlg('No Inputs declared', MtError, [MbOK], 0)
  else
     begin
     ComboBox1.Items.Clear;
     ComboBox2.Items.Clear;
     ComboBox3.Items.Clear;
     for i := 0 to ListSt.Count - 1 do
       begin
       ComboBox1.Items.Add(ListSt.Strings[i]);
       ComboBox2.Items.Add(ListSt.Strings[i]);
       end;
     for i := 0 to ListIn.Count - 1 do
       ComboBox3.Items.Add(ListIn.Strings[i]);
     ComboBox1.ItemIndex := 0;
     ComboBox2.ItemIndex := 0;
     ComboBox3.ItemIndex := 0;
     FOk := false;
     ShowModal;
     if FOK then
        begin
        XTmpTransition := TRafaTransition.Create(Owner);
        XTmpTransition.SourceState := ArrSt[ComboBox1.ItemIndex];
        XTmpTransition.TargetState := ArrSt[ComboBox2.ItemIndex];
        XTmpTransition.Input       := ArrIn[ComboBox3.ItemIndex];
        XTmpTransition.Goal := StrToInt(Edit1.text);
        if CheckBox1.Checked then
           begin
           XTmpTransition.Count  := random(20);
           XTmpTransition.Weight := random(XTmpTransition.Count * 1000) / 1000;
           end
        else if CheckBox1.Checked then
           begin
           XTmpTransition.Count  := 0;
           XTmpTransition.Weight := 0;
           end;
        end;
     end;
  result := XTmpTransition;
  end;

//--------------------------------------------------------------------------------------------------

procedure TFmMMTransition.Button1Click(Sender: TObject);
  begin
  FOk := true;
  Close;
  end;

procedure TFmMMTransition.Button2Click(Sender: TObject);
  begin
  Close;
  end;

end.
