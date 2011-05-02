unit UnDialWizards;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin, ComCtrls, ExtCtrls,

  UnNetRep;

type
  TFmWizards = class(TForm)
    PageControl1 : TPageControl;
    TabSheet1    : TTabSheet;
    TabSheet2    : TTabSheet;
    SpinEdit1    : TSpinEdit;
    SpinEdit2    : TSpinEdit;
    GroupBox1    : TGroupBox;
    SpinEdit3: TSpinEdit;
    Label1: TLabel;
    ScrollBox1: TScrollBox;
    GroupBox2: TGroupBox;
    Label4: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Edit3: TEdit;
    CheckBox2: TCheckBox;
    Edit6: TEdit;
    ComboBox4: TComboBox;
    Panel2: TPanel;
    Label10: TLabel;
    Label18: TLabel;
    Edit7: TEdit;
    Edit16: TEdit;
    CheckBox3: TCheckBox;
    Label6: TLabel;
    Label7: TLabel;
    Label11: TLabel;
    SpinEdit4: TSpinEdit;
    Label12: TLabel;
    SpinEdit5: TSpinEdit;
    SpinEdit6: TSpinEdit;
    Label13: TLabel;
    GroupBox4: TGroupBox;
    Label16: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Edit10: TEdit;
    CheckBox1: TCheckBox;
    Edit12: TEdit;
    ComboBox3: TComboBox;
    Panel1: TPanel;
    Label21: TLabel;
    Label22: TLabel;
    Edit14: TEdit;
    Edit15: TEdit;
    CheckBox4: TCheckBox;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    TabSheet3: TTabSheet;
    GroupBox3: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Edit1: TEdit;
    CheckBox5: TCheckBox;
    Edit2: TEdit;
    ComboBox1: TComboBox;
    Panel3: TPanel;
    Label14: TLabel;
    Label15: TLabel;
    Edit4: TEdit;
    Edit5: TEdit;
    CheckBox6: TCheckBox;
    Button5: TButton;
    Button6: TButton;
    Label17: TLabel;
    SpinEdit7: TSpinEdit;
    Label23: TLabel;
    SpinEdit8: TSpinEdit;
    Label24: TLabel;
    SpinEdit9: TSpinEdit;
    Label25: TLabel;
    SpinEdit10: TSpinEdit;
    Label26: TLabel;
    SpinEdit11: TSpinEdit;
    Label27: TLabel;
    Label28: TLabel;
    SpinEdit12: TSpinEdit;
    SpinEdit13: TSpinEdit;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    CheckBox7: TCheckBox;
    procedure SpinEdit3Change(Sender: TObject);
    procedure Edit10Enter(Sender: TObject);
    procedure Edit10Exit(Sender: TObject);
    procedure ComboBox3Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure SpinEdit12Change(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure SpinEdit9Change(Sender: TObject);
    procedure TabSheet3Show(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
  private
    tmp : double;
    btmp : boolean;
    FNetRep : TNetworkRep;
    { Private declarations }

  public
    function LoadFromWizard(NetRep : TNetworkRep) : boolean;
    { Public declarations }
  end;

var
  FmWizards: TFmWizards;

implementation

uses UnDialMerge;

{$R *.dfm}

procedure TFmWizards.SpinEdit3Change(Sender: TObject);
  var i : integer;
  begin
  if trim(SpinEdit3.Text) <> '' then
     begin
     for i := ScrollBox1.ControlCount - 1 downto 0 do
       ScrollBox1.Controls[i].Free;
     for i := 1 to SpinEdit3.Value do
       begin
       with TLabel.Create(FmWizards) do
         begin
         Top     := ((i - 1) * 32) + 13;
         Left    := 8;
         Height  := 13;
         Width   := 32;
         Caption := 'Hidden ' + inttostr(i) + ':';
         Parent  := ScrollBox1;
         end;
       with TSpinEdit.Create(FmWizards) do
         begin
         Top      := ((i - 1) * 32) + 8;
         Left     := 56;
         Height   := 22;
         Width    := 90;
         MinValue := 1;
         MaxValue := 200;
         Parent   := ScrollBox1;
         end;
       end;
     end;
  end;

procedure TFmWizards.Edit10Enter(Sender: TObject);
  begin
  try
    tmp := strtofloat((sender as TEdit).Text);
  except
    tmp := 0;
    end;
  end;

procedure TFmWizards.Edit10Exit(Sender: TObject);
  begin
  try
    tmp := strtofloat((sender as TEdit).Text);
  except
    (sender as TEdit).Text := FloatToStr(tmp);
    end;

  end;

procedure TFmWizards.ComboBox3Change(Sender: TObject);
  begin
  Edit12.Enabled := (ComboBox3.ItemIndex = 2) or (ComboBox3.ItemIndex = 4);
  Edit6.Enabled  := (ComboBox4.ItemIndex = 2) or (ComboBox4.ItemIndex = 4);
  Edit2.Enabled  := (ComboBox1.ItemIndex = 2) or (ComboBox1.ItemIndex = 4);
  end;

function TFmWizards.LoadFromWizard(NetRep: TNetworkRep): boolean;
  begin
  FNetRep := NetRep;
  btmp := false;
  ShowModal;
  result := btmp;
  end;

procedure TFmWizards.Button1Click(Sender: TObject);
  var
    x : array of integer;
    i, j : integer;
    n : TNeuronRep;
  begin
  SetLength(x, SpinEdit3.Value + 2);
  x[0] := SpinEdit1.Value;
  j := 1;
  for i := 0 to ScrollBox1.ControlCount - 1 do
    if ScrollBox1.Controls[i] is TSpinEdit then
       if (j<= SpinEdit3.Value) then
          begin
          x[j] := (ScrollBox1.Controls[i] as TSpinEdit).Value;
          j := j + 1;
          end;
  x[SpinEdit3.Value + 1] := SpinEdit2.Value;
  n := TNeuronRep.Create('');
  n.Eta := strtofloat(edit3.Text);
  n.AtivFunction := ComboBox4.ItemIndex;
  if (ComboBox4.ItemIndex = 2) or (ComboBox4.ItemIndex = 2) then
     n.AddFunctionParam(StrToFloat(Edit6.Text));
  n.HasBias    := CheckBox2.checked;
  n.FixedBias  := CheckBox3.checked;
  n.RandomBias := StrToFloat(Edit16.text);
  n.BiasWeight := StrToFloat(Edit7.text);

  FNetRep.LoadFeedForward(SpinEdit3.Value + 2, x, n);
  j := 0;
  for i := 1 to SpinEdit3.Value do
    j := j + x[i];
  if CheckBox7.checked then
     if FNetRep.AddLinksToHiddenNeurons(0) <> j then
        MessageDlg('Wrong numbers of links inserted', MtError, [MbOK], 0);
  n.Free;
  btmp := true;
  close;
  end;

procedure TFmWizards.Button2Click(Sender: TObject);
  begin
  btmp := false;
  close;
  end;

procedure TFmWizards.Button3Click(Sender: TObject);
  var n : TNeuronRep;
  begin
  n := TNeuronRep.Create('');
  n.Eta := strtofloat(edit10.Text);
  n.AtivFunction := ComboBox3.ItemIndex;
  if (ComboBox3.ItemIndex = 2) or (ComboBox3.ItemIndex = 2) then
     n.AddFunctionParam(StrToFloat(Edit12.Text));
  n.HasBias    := CheckBox1.checked;
  n.FixedBias  := CheckBox4.checked;
  n.RandomBias := StrToFloat(Edit15.text);
  n.BiasWeight := StrToFloat(Edit14.text);

  FNetRep.LoadElman(SpinEdit4.Value, SpinEdit6.Value, SpinEdit5.Value, n);

  n.Free;
  btmp := true;
  close;
  end;

procedure TFmWizards.SpinEdit12Change(Sender: TObject);
  begin
  if trim(SpinEdit12.Text) <> '' then
     begin
     if (SpinEdit9.Value - SpinEdit11.Value) > SpinEdit12.Value then
        SpinEdit13.MaxValue := SpinEdit12.Value
     else
        SpinEdit13.MaxValue := SpinEdit9.Value - SpinEdit11.Value;
     SpinEdit13.Enabled := (SpinEdit12.Value > 0);
     if SpinEdit13.Value >= SpinEdit13.MaxValue then
        SpinEdit13.Value := SpinEdit13.MaxValue;
     end;
 end;


procedure TFmWizards.Button5Click(Sender: TObject);
  var n : TNeuronRep;
  begin
  n := TNeuronRep.Create('');
  n.Eta := strtofloat(edit1.Text);
  n.AtivFunction := ComboBox1.ItemIndex;
  if (ComboBox3.ItemIndex = 2) or (ComboBox3.ItemIndex = 2) then
     n.AddFunctionParam(StrToFloat(Edit2.Text));
  n.HasBias    := CheckBox5.checked;
  n.FixedBias  := CheckBox6.checked;
  n.RandomBias := StrToFloat(Edit5.text);
  n.BiasWeight := StrToFloat(Edit4.text);

  FNetRep.LoadCILPLike(SpinEdit7.Value, SpinEdit8.Value, SpinEdit9.Value,
     SpinEdit10.Value, SpinEdit11.Value, SpinEdit12.Value, SpinEdit13.Value, n);

  n.Free;
  btmp := true;
  close;
  end;

procedure TFmWizards.SpinEdit9Change(Sender: TObject);
  begin
  if trim(SpinEdit9.Text) <> '' then
     begin
     if (SpinEdit9.Value - SpinEdit13.Value) > SpinEdit10.Value then
        SpinEdit11.MaxValue := SpinEdit10.Value
     else
        SpinEdit11.MaxValue := SpinEdit9.Value - SpinEdit13.Value;
     SpinEdit11.Enabled := (SpinEdit10.Value > 0);
     if SpinEdit11.Value >= SpinEdit11.MaxValue then
        SpinEdit11.Value := SpinEdit11.MaxValue;
     end;
  end;

procedure TFmWizards.TabSheet3Show(Sender: TObject);
  begin
  SpinEdit11.Enabled := (SpinEdit10.Value > 0);
  SpinEdit13.Enabled := (SpinEdit12.Value > 0);
  end;

procedure TFmWizards.Button9Click(Sender: TObject);
  var
    x : array of integer;
    i, j, l : integer;
    MS : TMergeStyle;
    XNetRep : TNetworkRep;
    n : TNeuronRep;

  begin
  if FmMerge.Execute(MS, l) then
     begin
     SetLength(x, SpinEdit3.Value + 2);
     x[0] := SpinEdit1.Value;
     j := 1;
     for i := 0 to ScrollBox1.ControlCount - 1 do
       if ScrollBox1.Controls[i] is TSpinEdit then
          if (j<= SpinEdit3.Value) then
             begin
             x[j] := (ScrollBox1.Controls[i] as TSpinEdit).Value;
             j := j + 1;
             end;
     x[SpinEdit3.Value + 1] := SpinEdit2.Value;

     n := TNeuronRep.Create('');
     n.Eta := strtofloat(edit3.Text);
     n.AtivFunction := ComboBox4.ItemIndex;
     if (ComboBox4.ItemIndex = 2) or (ComboBox4.ItemIndex = 2) then
        n.AddFunctionParam(StrToFloat(Edit6.Text));
     n.HasBias    := CheckBox2.checked;
     n.FixedBias  := CheckBox3.checked;
     n.RandomBias := StrToFloat(Edit16.text);
     n.BiasWeight := StrToFloat(Edit7.text);

     XNetRep := TNetworkRep.Create(nil, nil);
     XNetRep.LoadFeedForward(SpinEdit3.Value + 2, x, n);
     FNetRep.MergeNetwork(XNetRep, MS, l);
     XNetRep.Free;

     n.Free;
     btmp := true;
     close;
     end;
  end;


procedure TFmWizards.Button8Click(Sender: TObject);
  var
    n : TNeuronRep;
    l : integer;
    MS : TMergeStyle;
    XNetRep : TNetworkRep;

  begin
  if FmMerge.Execute(MS, l) then
     begin
     n := TNeuronRep.Create('');
     n.Eta := strtofloat(edit10.Text);
     n.AtivFunction := ComboBox3.ItemIndex;
     if (ComboBox3.ItemIndex = 2) or (ComboBox3.ItemIndex = 2) then
        n.AddFunctionParam(StrToFloat(Edit12.Text));
     n.HasBias    := CheckBox1.checked;
     n.FixedBias  := CheckBox4.checked;
     n.RandomBias := StrToFloat(Edit15.text);
     n.BiasWeight := StrToFloat(Edit14.text);

     XNetRep := TNetworkRep.Create(nil, nil);
     XNetRep.LoadElman(SpinEdit4.Value, SpinEdit6.Value, SpinEdit5.Value, n);
     FNetRep.MergeNetwork(XNetRep, MS, l);
     XNetRep.Free;

     n.Free;
     btmp := true;
     close;
     end;
  end;

procedure TFmWizards.Button7Click(Sender: TObject);
  var
    n : TNeuronRep;
    l : integer;
    MS : TMergeStyle;
    XNetRep : TNetworkRep;

  begin
  if FmMerge.Execute(MS, l) then
     begin
     n := TNeuronRep.Create('');
     n.Eta := strtofloat(edit1.Text);
     n.AtivFunction := ComboBox1.ItemIndex;
     if (ComboBox3.ItemIndex = 2) or (ComboBox3.ItemIndex = 2) then
        n.AddFunctionParam(StrToFloat(Edit2.Text));
     n.HasBias    := CheckBox5.checked;
     n.FixedBias  := CheckBox6.checked;
     n.RandomBias := StrToFloat(Edit5.text);
     n.BiasWeight := StrToFloat(Edit4.text);

     XNetRep := TNetworkRep.Create(nil, nil);
     XNetRep.LoadCILPLike(SpinEdit7.Value, SpinEdit8.Value, SpinEdit9.Value,
        SpinEdit10.Value, SpinEdit11.Value, SpinEdit12.Value, SpinEdit13.Value, n);
     FNetRep.MergeNetwork(XNetRep, MS, l);
     XNetRep.Free;

     n.Free;
     btmp := true;
     close;
     end;
  end;


end.
