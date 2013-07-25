{
  @abstract(Unit that describes the interface for editing the network and its units)
  @author(Rafa "Passarel" Borges <passarel@gmail.com)
  @created(Porto Alegre, 2006)
  @lastmod(London, January, 2010)
}

unit UnDialEdit; //556 lines

interface

uses Forms, Menus, StdCtrls, Controls, ComCtrls, Classes, UnNetRep, ExtCtrls, UnRafaVisual,
  Spin;

//------------------------------------------------------------------------------------------------

{
@abstract(Class encapsulating the visual interface for editing the network's configuration)
}
type TFmEdit = class(TForm)
    Button10: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    CheckBox12: TCheckBox;
    CheckBox14: TCheckBox;
    CheckBox15: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    ComboBox4: TComboBox;
    ComboBox5: TComboBox;
    Edit1: TEdit;
    Edit10: TEdit;
    Edit16: TEdit;
    Edit17: TEdit;
    Edit18: TEdit;
    Edit19: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    Edit8: TEdit;
    Exchange1: TMenuItem;
    Excluir1: TMenuItem;
    Excluir2: TMenuItem;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox6: TGroupBox;
    GroupBox7: TGroupBox;
    Inserir1: TMenuItem;
    Inserir2: TMenuItem;
    Label1: TLabel;
    Label10: TLabel;
    Label12: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    ListBox1: TListBox;
    ListBox2: TListBox;
    ListBox3: TListBox;
    ListBox4: TListBox;
    ListBox5: TListBox;
    MoveFirst1: TMenuItem;
    MoveLast1: TMenuItem;
    N1: TMenuItem;
    Ordenar1: TMenuItem;
    PageControl1: TPageControl;
    Panel2: TPanel;
    PopupMenu1: TPopupMenu;
    PopupMenu2: TPopupMenu;
    RadioGroup1: TRadioGroup;
    Renomear1: TMenuItem;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    Button1: TButton;
    GroupBox5: TGroupBox;
    RadioGroup2: TRadioGroup;
    Button2: TButton;
    Edit9: TEdit;
    Edit11: TEdit;
    procedure Button10Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure ComboBox3Change(Sender: TObject);
    procedure ComboBox4Change(Sender: TObject);
    procedure ComboBox5Change(Sender: TObject);
    procedure Edit10Exit(Sender: TObject);
    procedure Edit16Exit(Sender: TObject);
    procedure Edit17Exit(Sender: TObject);
    procedure Edit18Exit(Sender: TObject);
    procedure Edit19Exit(Sender: TObject);
    procedure Edit1Exit(Sender: TObject);
    procedure Edit2Exit(Sender: TObject);
    procedure Edit3Exit(Sender: TObject);
    procedure Edit4Exit(Sender: TObject);
    procedure Edit5Exit(Sender: TObject);
    procedure Edit6Exit(Sender: TObject);
    procedure Edit7Exit(Sender: TObject);
    procedure Edit8Exit(Sender: TObject);
    procedure Exchange1Click(Sender: TObject);
    procedure Excluir1Click(Sender: TObject);
    procedure Excluir2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Inserir1Click(Sender: TObject);
    procedure Inserir2Click(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure ListBox2Click(Sender: TObject);
    procedure ListBox4Click(Sender: TObject);
    procedure ListBox5Click(Sender: TObject);
    procedure MoveFirst1Click(Sender: TObject);
    procedure MoveLast1Click(Sender: TObject);
    procedure Ordenar1Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure Renomear1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);

  private

    //Array keeping the index of the layer of each node in the network
    FLayersArray : array of integer;

    //Object representing the network
    FNetRep : TNetworkRep;

    //Event called when something related to the network's visualization is changed
    FOnChangeVisualProperties : TRafaVisualEvent;

    //Update the Source and Target combo boxes
    procedure AtualizaCombos;

    //Default procedure loaded to the FOnChangeVisualProperties field - does nothing
    procedure DefaultEvent(index : integer);

    //Exchange the position between two selected nodes
    //@param(kind: 1 if exchanging positions, 2 if moving only the first node and
    //        3 if moving only the latter one )
    procedure TmpExchange(kind : integer);


    //Procedure to update the value shown on the X and Y edit boxes
    //@param(wVal: array indicating which edits should show value)
    //@param(vX: X value to be shown)
    //@param(vY: Y value to be shown)
    procedure UpdateXYEdits(wVal : array of boolean; vX, vY : integer);

    //Procedure to update the value shown in the edit boxes when a delay unit is shown
    //@param(wVal: array indicating which edits should show value)
    //@param(vX: X value to be shown)
    //@param(vY: Y value to be shown)
    //@param(vTimes: Value of the times parameter to be shown)
    //@param(vTimes: Delay function to be shown)
    procedure UpdateDelayEdits(wVal : array of boolean; vX, vY, vTimes, vDelay : integer);

    //Procedure to update the value shown in the edit boxes when an IO unit is shown
    //@param(wVal: array indicating which edits should show value)
    //@param(vX: X value to be shown)
    //@param(vY: Y value to be shown)
    //@param(InMin: value of input minimum to be shown)
    //@param(InMax: value of input maximum  to be shown)
    //@param(OutMin: value of output minimum to be shown)
    //@param(OutMax: value of output maximum to be shown)
    procedure UpdateIOEdits(wVal : array of boolean; vX, vY : integer;
                                 vInMin, vInMax, vOutMin, vOutMax : double);

    //Procedure to update the value shown in the edit boxes when a neuron is shown
    //@param(wVal: array indicating which edits should show value)
    //@param(vX: X value to be shown)
    //@param(vY: Y value to be shown)
    //@param(vEta: value of earning rate to be shown)
    //@param(vMom: value of momentum to be shown)
    //@param(vPar: value of the parameter of the activation function to be shown)
    //@param(vAct: index of the activation function to be shown)
    //@param(vHasB: boolean representing if selected neurons have bias)
    //@param(vFixB: boolean representing if bias of selected neurons are fixed)
    //@param(vWeiB: value of the bias' weight to be shown)
    //@param(vRanB: value of the random range of bias initialization be shown)
    procedure UpdateNeuronEdits(wVal : array of boolean; vX, vY : integer; vEta, vMom, vPar: double;
                                 vAct : integer; vHasB, vFixB : boolean; vWeiB, vRanB : double);

    //Procedure to update the edit boxes with blank values
    procedure UpdateNoEdits;

    //Procedure to update the value shown in the edit boxes when a recurrent link is shown
    //@param(wVal: array indicating which edits should show value)
    //@param(vX: X value to be shown)
    //@param(vY: Y value to be shown)
    //@param(vLev: level of the selected units)
    //@param(vSty: index of the style of the units)
    //@param(vInit: initialization value to be shown)
    procedure UpdateRecLinkEdits(wVal : array of boolean; vX, vY, vLev, vSty : integer;
                                 vInit : double);

  public
    //Event called when something related to the network's visualization is changed
    property OnChangeVisualProperties : TRafaVisualEvent read FOnChangeVisualProperties
             write FOnChangeVisualProperties;

    //Procedure to define the used network
    procedure SetNetwork(net : TNetworkRep);

    //Procedure to update the network's layers listed in the List Box
    procedure UpdateLayers;

    //Procedure to update the controls showing the network's properties
    procedure UpdateNetworkProperties;

    //Procedure to update the controls showing the properties of a specified node
    //@param(selected: identifies the selected node)
    procedure UpdateSelectedNode(selected : integer = -1);

  end;

//------------------------------------------------------------------------------------------------

var FmEdit : TFmEdit;

implementation

{$R *.dfm}

uses UnRafaAux2007, SysUtils, UnDialNode, UnDialWizards;

//------------------------------------------------------------------------------------------------

{ TFmEdit }

//------------------------------------------------------------------------------------------------

procedure TFmEdit.AtualizaCombos;
  begin
  ComboBox2.Items := ListBox1.Items;
  ComboBox3.Items := ListBox1.Items;
  end;

//------------------------------------------------------------------------------------------------

procedure TFmEdit.Button10Click(Sender: TObject);
  var i : integer;
  begin
  i := FNetRep.AddConnectedNeuron(1);
  UpdateLayers;
  OnChangeVisualProperties(i);
  end;

//------------------------------------------------------------------------------------------------

procedure TFmEdit.Button5Click(Sender: TObject);
  var i : integer;
  begin
  for i := 0 to FNetRep.NoArcs - 1 do
    begin
    if CheckBox14.Checked then
       FNetRep.Arc[i].Weight      := strtofloat(Edit4.Text);
    if CheckBox15.Checked then
       FNetRep.Arc[i].RandomRange := strtofloat(Edit17.Text);
    if CheckBox4.State <> CbGrayed then
       FnetRep.Arc[i].isFixed     := (CheckBox4.State = cbChecked);
    end;
  end;

//------------------------------------------------------------------------------------------------

procedure TFmEdit.Button6Click(Sender: TObject);
  var
    i, Xtmp, XSpace, XTotalNeurons : integer;
    XLayers : array of integer;

  begin
  //Define According to the network style
  SetLength(XLayers, ListBox5.Count + 3);
  XTotalNeurons := ListBox5.Count - 4;

  XSpace := (SpinEdit2.Value) div (XTotalNeurons + 2);
  XTmp   := XSpace div 2;

  //input units
  XLayers[XTotalNeurons] := XTmp;
  XTmp := XTmp + (XSPace div 2);

  //input links
  XLayers[XTotalNeurons + 6] := XTmp;
  XTmp := XTmp + (XSPace div 2);

  //Neurons
  for i := 0 to XTotalNeurons - 1 do
    begin
    XLayers[i] := XTmp;
    XTmp := XTmp + (XSPace);
    end;

  //Output Links
  XTmp := XTmp - (XSpace div 2);
  XLayers[XTotalNeurons + 5] := XTmp;
  XTmp := XTmp + (XSpace div 2);

  //Output units
  XLayers[XTotalNeurons + 1] := XTmp;

  //Other Units
  XLayers[XTotalNeurons + 2] := XTmp;
  XLayers[XTotalNeurons + 3] := XTmp;
  XLayers[XTotalNeurons + 4] := XTmp;

  FNetRep.VisualOrg(SpinEdit1.Value, ListBox5.Count + 3, XLayers);
  OnChangeVisualProperties(-1);
  end;

//------------------------------------------------------------------------------------------------

procedure TFmEdit.Button7Click(Sender: TObject);
  begin
  FNetRep.FullyConnect(StrToFloat(Edit9.Text), StrToFloat(Edit10.Text), CheckBox12.checked);
  OnChangeVisualProperties(-1);
  end;

//------------------------------------------------------------------------------------------------

procedure TFmEdit.Button8Click(Sender: TObject);
  var i : integer;
  begin
  i := FNetRep.AddConnectedInputNeuron(true, FNetRep.LinkFlow <> LfNone);
  OnChangeVisualProperties(i);
  end;

//------------------------------------------------------------------------------------------------

procedure TFmEdit.Button9Click(Sender: TObject);
  var i : integer;
  begin
  i := FNetRep.AddConnectedOutputNeuron(true, FNetRep.LinkFlow <> LfNone);
  UpdateLayers;
  OnChangeVisualProperties(i);
  end;

//------------------------------------------------------------------------------------------------

procedure TFmEdit.CheckBox2Click(Sender: TObject);
  var i : integer;
  begin
  for i := 0 to ListBox1.Count - 1 do
    if ListBox1.Selected[i] then
       if FNetRep.Node[i] is TNeuronRep then
          (FNetRep.Node[i] as TNeuronRep).HasBias := Checkbox2.Checked;
  end;

//------------------------------------------------------------------------------------------------

procedure TFmEdit.CheckBox3Click(Sender: TObject);
  var i : integer;
  begin
  for i := 0 to ListBox1.Count - 1 do
    if ListBox1.Selected[i] then
       if FNetRep.Node[i] is TNeuronRep then
          (FNetRep.Node[i] as TNeuronRep).FixedBias := CheckBox3.Checked;
  end;

//------------------------------------------------------------------------------------------------

procedure TFmEdit.CheckBox4Click(Sender: TObject);
  var i : integer;
  begin
  if CheckBox4.State <> CbGrayed then
     for i := 0 to ListBox4.Items.Count - 1 do
       if ListBox4.Selected[i] then
          FNetRep.Arc[i].isFixed := (CheckBox4.State = cbChecked);
  end;

//------------------------------------------------------------------------------------------------

procedure TFmEdit.ComboBox1Change(Sender: TObject);
  var i : integer;
  begin
  for i := 0 to ListBox1.Count - 1 do
    if ListBox1.Selected[i] then
      (FNetRep.Node[i] as TDelayRep).DelayFunction := ComboBox1.ItemIndex;
  end;

//------------------------------------------------------------------------------------------------

procedure TFmEdit.ComboBox2Change(Sender: TObject);
  var i : integer;
  begin
  if ComboBox2.ItemIndex >= 0 then
     for i := 0 to ListBox4.Items.Count - 1 do
       if ListBox4.Selected[i] then
          FNetRep.Arc[i].Source := ComboBox2.ItemIndex;
  FNetRep.UpdateArcs;
  if ListBox1.SelCount = 1 then
     OnChangeVisualProperties(ListBox1.ItemIndex)
  else
     OnChangeVisualProperties(-1);
  end;

//------------------------------------------------------------------------------------------------

procedure TFmEdit.ComboBox3Change(Sender: TObject);
  var i : integer;
  begin
  if ComboBox3.ItemIndex >= 0 then
     for i := 0 to ListBox4.Items.Count - 1 do
       if ListBox4.Selected[i] then
          FNetRep.Arc[i].Target := ComboBox3.ItemIndex;
  FNetRep.UpdateArcs;
  if ListBox1.SelCount = 1 then
     OnChangeVisualProperties(ListBox1.ItemIndex)
  else
     OnChangeVisualProperties(-1);
  end;

//------------------------------------------------------------------------------------------------

procedure TFmEdit.ComboBox4Change(Sender: TObject);
  var i : integer;
  begin
  for i := 0 to ListBox1.Count - 1 do
    if ListBox1.Selected[i] then
       if FNetRep.Node[i] is TNeuronRep then
          begin
          (FNetRep.Node[i] as TNeuronRep).ClearFunctionParams;
          (FNetRep.Node[i] as TNeuronRep).AtivFunction := ComboBox4.ItemIndex;
          if (Combobox4.ItemIndex = 2) or (Combobox4.ItemIndex = 5) then
             begin
             Edit6.Enabled := true;
             try
               (FNetRep.Node[i] as TNeuronRep).AddFunctionParam(StrToFloat(Edit6.Text))
             except
               (FNetRep.Node[i] as TNeuronRep).AddFunctionParam(0);
               end;
             end;
          end;
  end;

//------------------------------------------------------------------------------------------------

procedure TFmEdit.ComboBox5Change(Sender: TObject);
  var i : integer;
  begin
  for i := 0 to ListBox1.Count - 1 do
    if ListBox1.Selected[i] then
       if FNetRep.Node[i] is TRecLinkRep then
          (FNetRep.Node[i] as TRecLinkRep).LinkBackStyle := TLinkBackStyle(Combobox5.ItemIndex);
  end;

//------------------------------------------------------------------------------------------------

procedure TFmEdit.DefaultEvent(index : integer);
  begin
  end;

//------------------------------------------------------------------------------------------------

procedure TFmEdit.Edit10Exit(Sender: TObject);
  begin
  try
    FNetRep.EtaCorrection := StrToFloat(Edit10.text);
  except
    Edit10.text := FloatToStr(FNetRep.EtaCorrection);
    end;
  end;

//------------------------------------------------------------------------------------------------

procedure TFmEdit.Edit16Exit(Sender: TObject);
  var
    i : integer;
    val : double;
  begin
  try
    val := strtofloat(Edit16.Text);
    for i := 0 to ListBox1.Count - 1 do
      if ListBox1.Selected[i] then
         if FNetRep.Node[i] is TNeuronRep then
            (FNetRep.Node[i] as TNeuronRep).RandomBias := val;
  except
    Edit16.Text := '';
    end;
  end;

//------------------------------------------------------------------------------------------------

procedure TFmEdit.Edit17Exit(Sender: TObject);
  var
    i : integer;
    d : double;
  begin
  if trim(Edit17.Text) <> '' then
     begin
     try
     d := strtofloat(Edit17.Text);
     for i := 0 to ListBox4.Items.Count - 1 do
       if ListBox4.Selected[i] then
          FNetRep.Arc[i].RandomRange := d;
     except
       Edit17.Text := FloatToStr(FNetRep.Arc[ListBox4.ItemIndex].RandomRange);
       end;
     end;
  end;

//------------------------------------------------------------------------------------------------

procedure TFmEdit.Edit18Exit(Sender: TObject);
  begin
  try
    FNetRep.Nu := StrToInt(Edit18.text);
  finally
    Edit18.text := IntToStr(FNetRep.Nu);
    end;
  end;

//------------------------------------------------------------------------------------------------

procedure TFmEdit.Edit19Exit(Sender: TObject);
  begin
  try
    FNetRep.Upsilon := StrToInt(Edit19.text);
  finally
    Edit19.text := IntToStr(FNetRep.Upsilon);
    end;
  end;

//------------------------------------------------------------------------------------------------

procedure TFmEdit.Edit1Exit(Sender: TObject);
  var i, val : integer;
  begin
  try
    val := strtoint(Edit1.Text);
    for i := 0 to ListBox1.Count - 1 do
      if ListBox1.Selected[i] then
         FNetRep.Node[i].X := val;
    if ListBox1.SelCount = 1 then
       OnChangeVisualProperties(ListBox1.ItemIndex)
    else
       OnChangeVisualProperties(-1);
  except
    Edit1.Text := '';
    end;
  end;

//------------------------------------------------------------------------------------------------

procedure TFmEdit.Edit2Exit(Sender: TObject);
  var i, val : integer;
  begin
  try
    val := strtoint(Edit2.Text);
    for i := 0 to ListBox1.Count - 1 do
      if ListBox1.Selected[i] then
         FNetRep.Node[i].Y := val;
    if ListBox1.SelCount = 1 then
       OnChangeVisualProperties(ListBox1.ItemIndex)
    else
       OnChangeVisualProperties(-1);
  except
    Edit2.Text := '';
    end;
  end;

//------------------------------------------------------------------------------------------------

procedure TFmEdit.Edit3Exit(Sender: TObject);
  var
    i : integer;
    val : double;
  begin
  try
    val := strtofloat(Edit3.Text);
    for i := 0 to ListBox1.Count - 1 do
      if ListBox1.Selected[i] then
         if FNetRep.Node[i] is TNeuronRep then
            (FNetRep.Node[i] as TNeuronRep).Eta := val
         else if FNetRep.Node[i] is TDelayRep then
            (FNetRep.Node[i] as TDelayRep).Times := round(val)
         else if FNetRep.Node[i] is TIOUnitRep then
            (FNetRep.Node[i] as TIOUnitRep).InMin := val
         else if FNetRep.Node[i] is TRecLinkRep then
            (FNetRep.Node[i] as TRecLinkRep).Level := round(val);
  except
    Edit3.Text := '';
    end;
  end;

//------------------------------------------------------------------------------------------------

procedure TFmEdit.Edit4Exit(Sender: TObject);
  var
    i : integer;
    d : double;
  begin
  if trim(Edit4.Text) <> '' then
     begin
     try
     d := strtofloat(Edit4.Text);
     for i := 0 to ListBox4.Items.Count - 1 do
       if ListBox4.Selected[i] then
          FNetRep.Arc[i].weight := d;
     except
       Edit4.Text := FloatToStr(FNetRep.Arc[ListBox4.ItemIndex].weight);
       end;
     end;
  end;

//------------------------------------------------------------------------------------------------


procedure TFmEdit.Edit5Exit(Sender: TObject);
  var
    i : integer;
    val : double;
  begin
  try
    val := strtofloat(Edit5.Text);
    for i := 0 to ListBox1.Count - 1 do
      if ListBox1.Selected[i] then
         if FNetRep.Node[i] is TIOUnitRep then
            (FNetRep.Node[i] as TIOUnitRep).InMax := val
         else if FNetRep.Node[i] is TNeuronRep then
            (FNetRep.Node[i] as TNeuronRep).Momentum := val;
  except
    Edit5.Text := '';
    end;
  end;

//------------------------------------------------------------------------------------------------

procedure TFmEdit.Edit6Exit(Sender: TObject);
  var
    i : integer;
    val : double;
  begin
  try
    val := strtofloat(Edit6.Text);
    for i := 0 to ListBox1.Count - 1 do
      if ListBox1.Selected[i] then
         if FNetRep.Node[i] is TNeuronRep then
            (FNetRep.Node[i] as TNeuronRep).Param[0] := val
         else if FNetRep.Node[i] is TIOUnitRep then
            (FNetRep.Node[i] as TIOUnitRep).OutMax := val;
  except
    Edit6.Text := '';
    end;
  end;

//------------------------------------------------------------------------------------------------

procedure TFmEdit.Edit7Exit(Sender: TObject);
  var
    i : integer;
    val : double;
  begin
  try
    val := StrToFloat(Edit7.text);
    for i := 0 to ListBox1.Count - 1 do
      if ListBox1.Selected[i] then
         if FNetRep.Node[i] is TNeuronRep then
            (FNetRep.Node[i] as TNeuronRep).BiasWeight := val;
  except
    Edit7.Text := '';
    end;
  end;

//------------------------------------------------------------------------------------------------

procedure TFmEdit.Edit8Exit(Sender: TObject);
  var
    i : integer;
    val : double;
  begin
  try
    val := strtofloat(Edit8.Text);
    for i := 0 to ListBox1.Count - 1 do
      if ListBox1.Selected[i] then
         if FNetRep.Node[i] is TIOUnitRep then
            (FNetRep.Node[i] as TIOUnitRep).OutMin := val
         else if FNetRep.Node[i] is TRecLinkRep then
            (FNetRep.Node[i] as TRecLinkRep).InitValue := val;
  except
    Edit8.Text := '';
    end;
  end;

//------------------------------------------------------------------------------------------------

procedure TFmEdit.Exchange1Click(Sender: TObject);
  begin
  TmpExchange(1);
  end;

//------------------------------------------------------------------------------------------------

procedure TFmEdit.Excluir1Click(Sender: TObject);
  var i : integer;
  begin
  for i := ListBox1.count - 1 downto 0 do
    if ListBox1.Selected[i] then
      FNetRep.DeleteNode(i);
  ListBox1.ItemIndex := 0;
  ListBox1.OnClick(ListBox1);
  AtualizaCombos;
  OnChangeVisualProperties(-1);
  end;

//------------------------------------------------------------------------------------------------

procedure TFmEdit.Excluir2Click(Sender: TObject);
  var x : integer;
  begin
  x := ListBox4.ItemIndex;
  FNetRep.DeleteArc(x);
  if x < ListBox4.Items.Count then
     ListBox4.ItemIndex := x
  else
     ListBox4.ItemIndex := ListBox4.Items.Count - 1;
  ListBox4.OnClick(ListBox4);
  if ListBox1.SelCount = 1 then
     OnChangeVisualProperties(ListBox1.ItemIndex)
  else
     OnChangeVisualProperties(-1);
  end;

//------------------------------------------------------------------------------------------------

procedure TFmEdit.FormCreate(Sender: TObject);
  begin
  FOnChangeVisualProperties := DefaultEvent;
  FNetRep := TNetworkRep.Create(ListBox1.Items, ListBox4.Items);
  ComboBox2.Items := ListBox1.Items;
  ComboBox3.Items := ListBox1.Items;
  end;

//------------------------------------------------------------------------------------------------

procedure TFmEdit.Inserir1Click(Sender: TObject);
  var
    s : string;
    k : TUnitKind;

  begin
  if FmNode.InsertNode(s, k) then
     FNetRep.AddNode(s,K);
  ListBox1.ItemIndex := Listbox1.Items.Count - 1;
  ListBox1.OnClick(ListBox1);
  AtualizaCombos;
  end;

//------------------------------------------------------------------------------------------------

procedure TFmEdit.Inserir2Click(Sender: TObject);
  begin
  if FNetRep.NoNodes > 0 then
     FNetRep.AddArc(0, FNetRep.NoNodes - 1);
  ListBox4.ItemIndex := ListBox4.Items.Count - 1;
  ListBox4.OnClick(ListBox4);
  end;

//------------------------------------------------------------------------------------------------

procedure TFmEdit.ListBox1Click(Sender: TObject);
  var
    i, j : integer;
    XStart, XEnd, XFirst : boolean;
    XX, XY : integer;
    XHasB, XFixB : boolean;
    XValueB, XRangeB, XEta, XMom, XParam : double;
    XFunc, XTimes, XLevel, XSty : integer;
    XInit, XInMax, XInMin, XOutMax, XOutMin : double;
    XBoolArr : array of boolean;
    XSelItems : array of integer;
    XKind : TUnitKind;
  begin
  XStart := true;
  XEnd   := false;

  //Stupid Initialisation
  XX := -1;
  XY := -1;
  XHasB := false;
  XFixB := false;
  XValueB := 0;
  XRangeB := 0;
  XEta := 0;
  XMom := 0;
  XParam := 0;
  XFunc := -1;
  XTimes := 0;
  XLevel := -1;
  XSty := -1;
  XInit := 0;
  XInMax := 0;
  XInMin := 0;
  XOutMax := 0;
  XOutMin := 0;
  XKind := UKNeuron;


  for i := 0 to ListBox1.Count - 1 do
    begin
    if ListBox1.Selected[i] then
       begin
       if XStart then
          begin
          XKind := FNetRep.Node[i].Kind;
          XStart := false;
          end
       else
          if XKind <> FNetRep.Node[i].Kind then
             XEnd := true;
       end;
    end;

  if XStart then
     begin
     UpdateNoEdits;
     end
  else if XEnd then
     begin
     SetLength(XBoolArr, 2);
     XFirst  := true;
     for i := 0 to ListBox1.Count - 1 do
       if ListBox1.Selected[i] then
          if XFirst then
             begin
             for j := 0 to 1 do
               XBoolArr[j] := true;
             XX      := FNetRep.Node[i].X;
             XY      := FNetRep.Node[i].Y;
             XFirst  := false;
             end
          else
             begin
             XBoolArr[0] := XBoolArr[0] and (XX = FNetRep.Node[i].X);
             XBoolArr[1] := XBoolArr[1] and (XY = FNetRep.Node[i].Y);
             end;
     UpdateXYEdits(XBoolArr, XX, XY);
     end
  else
     begin
     case XKind of
       UKInput, UkOutput :
         begin
         SetLength(XBoolArr, 6);
         XFirst  := true;
         for i := 0 to ListBox1.Count - 1 do
           if ListBox1.Selected[i] then
              if XFirst then
                 begin
                 for j := 0 to 5 do
                   XBoolArr[j] := true;
                 XX      := FNetRep.Node[i].X;
                 XY      := FNetRep.Node[i].Y;
                 XInMin  := (FNetRep.Node[i] as TIOUnitRep).InMin;
                 XInMax  := (FNetRep.Node[i] as TIOUnitRep).InMax;
                 XOutMin := (FNetRep.Node[i] as TIOUnitRep).OutMin;
                 XOutMax := (FNetRep.Node[i] as TIOUnitRep).OutMax;
                 XFirst  := false;
                 end
              else
                 begin
                 XBoolArr[0] := XBoolArr[0] and (XX = FNetRep.Node[i].X);
                 XBoolArr[1] := XBoolArr[1] and (XY = FNetRep.Node[i].Y);
                 XBoolArr[2] := XBoolArr[2] and (XInMin = (FNetRep.Node[i] as TIOUnitRep).InMin);
                 XBoolArr[3] := XBoolArr[3] and (XInMax = (FNetRep.Node[i] as TIOUnitRep).InMax);
                 XBoolArr[4] := XBoolArr[4] and (XOutMin = (FNetRep.Node[i] as TIOUnitRep).OutMin);
                 XBoolArr[5] := XBoolArr[5] and (XOutMax = (FNetRep.Node[i] as TIOUnitRep).OutMax);
                 end;
           UpdateIOEdits(XBoolArr, XX, XY, XInMin, XInMax, XOutMin, XOutMax);
           end;
       UKNeuron :
         begin
         SetLength(XBoolArr, 10);
         XFirst  := true;
         for i := 0 to ListBox1.Count - 1 do
           if ListBox1.Selected[i] then
              if XFirst then
                 begin
                 for j := 0 to 9 do
                   XBoolArr[j] := true;
                 XX      := FNetRep.Node[i].X;
                 XY      := FNetRep.Node[i].Y;
                 XFunc   := (FNetRep.Node[i] as TNeuronRep).AtivFunction;
                 XParam  := (FNetRep.Node[i] as TNeuronRep).Param[0];
                 XEta    := (FNetRep.Node[i] as TNeuronRep).Eta;
                 XMom    := (FNetRep.Node[i] as TNeuronRep).Momentum;
                 XHasB   := (FNetRep.Node[i] as TNeuronRep).HasBias;
                 XFixB   := (FNetRep.Node[i] as TNeuronRep).FixedBias;
                 XRangeB := (FNetRep.Node[i] as TNeuronRep).RandomBias;
                 XValueB := (FNetRep.Node[i] as TNeuronRep).BiasWeight;
                 XFirst  := false;
                 end
              else
                 begin
                 XBoolArr[0] := XBoolArr[0] and (XX = FNetRep.Node[i].X);
                 XBoolArr[1] := XBoolArr[1] and (XY = FNetRep.Node[i].Y);
                 XBoolArr[2] := XBoolArr[2] and (XEta = (FNetRep.Node[i] as TNeuronRep).Eta);
                 XBoolArr[3] := XBoolArr[3] and (XMom = (FNetRep.Node[i] as TNeuronRep).Momentum);
                 XBoolArr[4] := XBoolArr[4] and (XParam = (FNetRep.Node[i] as TNeuronRep).Param[0]);
                 XBoolArr[5] := XBoolArr[5] and (XFunc = (FNetRep.Node[i] as TNeuronRep).AtivFunction);
                 XBoolArr[6] := XBoolArr[6] and (XHasB = (FNetRep.Node[i] as TNeuronRep).HasBias);
                 XBoolArr[7] := XBoolArr[7] and (XFixB = (FNetRep.Node[i] as TNeuronRep).FixedBias);
                 XBoolArr[8] := XBoolArr[8] and (XValueB = (FNetRep.Node[i] as TNeuronRep).BiasWeight);
                 XBoolArr[9] := XBoolArr[9] and (XRangeB = (FNetRep.Node[i] as TNeuronRep).RandomBias);
                 end;
         UpdateNeuronEdits(XBoolArr, XX, XY, XEta, XMom, XParam, XFunc, XHasB, XFixB, XValueB, XRangeB);
         end;
       UKDelay :
         begin
         SetLength(XBoolArr, 4);
         XFirst  := true;
         for i := 0 to ListBox1.Count - 1 do
           if ListBox1.Selected[i] then
              if XFirst then
                 begin
                 for j := 0 to 3 do
                   XBoolArr[j] := true;
                 XX      := FNetRep.Node[i].X;
                 XY      := FNetRep.Node[i].Y;
                 XTimes  := (FNetRep.Node[i] as TDelayRep).Times;
                 XFunc   := (FNetRep.Node[i] as TDelayRep).DelayFunction;
                 XFirst  := false;
                 end
              else
                 begin
                 XBoolArr[0] := XBoolArr[0] and (XX = FNetRep.Node[i].X);
                 XBoolArr[1] := XBoolArr[1] and (XY = FNetRep.Node[i].Y);
                 XBoolArr[2] := XBoolArr[2] and (XTimes = (FNetRep.Node[i] as TDelayRep).Times);
                 XBoolArr[3] := XBoolArr[3] and (XFunc = (FNetRep.Node[i] as TDelayRep).DelayFunction);
                 end;
         UpdateDelayEdits(XBoolArr, XX, XY, XTimes, XFunc);
         end;
       UkRec :
         begin
         SetLength(XBoolArr, 5);
         XFirst  := true;
         for i := 0 to ListBox1.Count - 1 do
           if ListBox1.Selected[i] then
              if XFirst then
                 begin
                 for j := 0 to 4 do
                   XBoolArr[j] := true;
                 XX      := FNetRep.Node[i].X;
                 XY      := FNetRep.Node[i].Y;
                 XLevel  := (FNetRep.Node[i] as TRecLinkRep).Level;
                 XSty    := ord((FNetRep.Node[i] as TRecLinkRep).LinkBackStyle);
                 XInit   := (FNetRep.Node[i] as TRecLinkRep).InitValue;
                 XFirst  := false;
                 end
              else
                 begin
                 XBoolArr[0] := XBoolArr[0] and (XX = FNetRep.Node[i].X);
                 XBoolArr[1] := XBoolArr[1] and (XY = FNetRep.Node[i].Y);
                 XBoolArr[2] := XBoolArr[2] and (XLevel = (FNetRep.Node[i] as TRecLinkRep).Level);
                 XBoolArr[3] := XBoolArr[3] and (XSty = ord((FNetRep.Node[i] as TRecLinkRep).LinkBackStyle));
                 XBoolArr[4] := XBoolArr[4] and (XInit = (FNetRep.Node[i] as TRecLinkRep).InitValue);
                 end;
          UpdateRecLinkEdits(XBoolArr, XX, XY, XLevel, XSty, XInit);
         end;
       end;
     end;


   j := 0;
   for i := 0 to ListBox1.Count - 1 do
     if ListBox1.Selected[i] then
        begin
        SetLength(XSelItems, j + 1);
        XSelItems[j] := i;
        j := j + 1;
        end;

   FNetRep.GetSpecArcsList(ListBox3.Items, XSelItems, true);
   FNetRep.GetSpecArcsList(ListBox2.items, XSelItems, false);
   FOnChangeVisualProperties(ListBox1.ItemIndex);
   end;

//------------------------------------------------------------------------------------------------

procedure TFmEdit.ListBox2Click(Sender: TObject);
  var
    i, j, XCount : integer;
    Xarr : array of integer;
    XSel : boolean;
  begin
  XCount := (Sender as TListBox).SelCount;
  SetLength(XArr, XCount);
  j := 0;
  for i := 0 to (Sender as TListBox).Items.count - 1 do
    if (Sender as TListBox).Selected[i] then
       begin
       XArr[j] := ((Sender as TListBox).Items.Objects[i] as TRafaInteger).Data;
       j := j + 1;
       end;
  for i := 0 to ListBox4.Items.Count - 1 do
    begin
    XSel := false;
    j := 0;
    while (not XSel) and (j < XCount) do
      begin
      XSel := XSel or (Xarr[j] = i);
      j := j + 1;
      end;
    ListBox4.Selected[i] := XSel;
    end;
  ListBox4Click(ListBox4);
  end;

//------------------------------------------------------------------------------------------------

procedure TFmEdit.ListBox4Click(Sender: TObject);
  var
    XFirst, XSameSource, XSameTarget, XSameWeight, XSameRange, XSameFixed : boolean;
    i, XSource, XTarget : integer;
    XWeight, XRange : double;
    XFixed : boolean;

  begin
  XFirst := true;
  XSameSource := false;
  XSameTarget := false;
  XSameWeight := false;
  XSameRange := false;
  XSameFixed := false;
  XSource := -1;
  XTarget := -1;
  XWeight := 0;
  XRange  := 0;
  XFixed := false;
  for i := 0 to ListBox4.Items.Count - 1 do
    if ListBox4.Selected[i] then
       begin
       if XFirst then
          begin
          XFirst := false;
          XSameSource := true;
          XSameTarget := true;
          XSameWeight := true;
          XSameRange := true;
          XSameFixed := true;
          XSource := FNetRep.Arc[i].Source;
          XTarget := FNetRep.Arc[i].Target;
          XWeight := FNetRep.Arc[i].Weight;
          XRange  := FNetRep.Arc[i].RandomRange;
          XFixed  := FNetRep.Arc[i].IsFixed;
          end
       else
          begin
          XSameSource := XSameSource and (XSource = FNetRep.Arc[i].Source);
          XSameTarget := XSameTarget and (XTarget = FNetRep.Arc[i].Target);
          XSameWeight := XSameWeight and (XWeight = FNetRep.Arc[i].Weight);
          XSameRange  := XSameRange  and (XRange  = FNetRep.Arc[i].RandomRange);
          XSameFixed  := XSameFixed  and (XFixed  = FNetRep.Arc[i].IsFixed);
          end;
       end;

  if XSameSource then
     ComboBox2.ItemIndex := XSource
  else
     ComboBox2.ItemIndex := -1;

  if XSameTarget then
     ComboBox3.ItemIndex := XTarget
  else
     ComboBox3.ItemIndex := -1;

  if XSameWeight then
     Edit4.Text := floattostr(XWeight)
  else
     Edit4.Text := '';

  if XSameRange then
     Edit17.Text := floattostr(XRange)
  else
     Edit17.Text := '';

  if XSameFixed then
     if XFixed then
        CheckBox4.State := cbChecked
     else
        CheckBox4.State := cbUnchecked
  else
     CheckBox4.State := cbGrayed;

  end;

//------------------------------------------------------------------------------------------------

procedure TFmEdit.ListBox5Click(Sender: TObject);
  var i : integer;
  begin
  for i := 0 to ListBox1.Count - 1 do
    Listbox1.Selected[i] := ListBox5.Selected[FLayersArray[i]];
  ListBox1.OnClick(ListBox1);
  end;

//------------------------------------------------------------------------------------------------

procedure TFmEdit.MoveFirst1Click(Sender: TObject);
  begin
  TmpExchange(2);
  end;

//------------------------------------------------------------------------------------------------

procedure TFmEdit.MoveLast1Click(Sender: TObject);
  begin
  TmpExchange(3);
  end;

//------------------------------------------------------------------------------------------------

procedure TFmEdit.Ordenar1Click(Sender: TObject);
  begin
  FNetRep.SortNodes;
  ListBox1.ItemIndex := 0;
  ListBox1.OnClick(ListBox1);
  AtualizaCombos;
  end;

//------------------------------------------------------------------------------------------------

procedure TFmEdit.RadioGroup1Click(Sender: TObject);
  begin
  case RadioGroup1.ItemIndex of
    0 : FNetRep.LinkFlow := LfNone;
    1 : FNetRep.LinkFlow := LfElman;
    2 : FNetRep.LinkFlow := LfCILP;
    3 : FNetRep.LinkFlow := LfSCTL;
    4 : FNetRep.LinkFlow := LfCML;
    5 : FNetRep.LinkFlow := LfSaving;
    end;
  Edit18.Enabled  := FNetRep.LinkFlow in [LfCILP, LfSCTL, LfCML];
  Label20.Enabled := FNetRep.LinkFlow in [LfCILP, LfSCTL, LfCML];
  Edit19.Enabled  := FNetRep.LinkFlow in [LfCILP, LfSCTL, LfCML];
  Label21.Enabled := FNetRep.LinkFlow in [LfCILP, LfSCTL, LfCML];
  end;

//------------------------------------------------------------------------------------------------

procedure TFmEdit.Renomear1Click(Sender: TObject);
  var
    s : string;

  begin
  s := FNetRep.Node[ListBox1.ItemIndex].Name;
  if FmNode.RenameNode(s) then
     FNetRep.Node[ListBox1.ItemIndex].Name := s;
  AtualizaCombos;
  end;

//------------------------------------------------------------------------------------------------

procedure TFmEdit.SetNetwork(net : TNetworkRep);
  begin
  FNetRep := net;
  end;

//------------------------------------------------------------------------------------------------

procedure TFmEdit.TmpExchange(kind : integer);
  var XPos1, XPos2 : integer;
  begin
  XPos1 := 0;
  while (XPos1 < ListBox1.Count) and (not ListBox1.Selected[XPos1]) do
    XPos1 := XPos1 + 1;
  XPos2 := ListBox1.Count - 1;
  while (XPos2 >= 0) and (not ListBox1.Selected[XPos2]) do
    XPos2 := XPos2 - 1;

  if (XPos1 < ListBox1.Count) and (XPos2 >= 0) and (XPos1 <> XPos2) then
     case kind of
       1 : FNetRep.ExchangeNodes(XPos1, XPos2, false);
       2 : FNetRep.ExchangeNodes(XPos1, XPos2, true);
       3 : FNetRep.ExchangeNodes(XPos2, XPos1, true);
       end;
  end;

//------------------------------------------------------------------------------------------------

procedure TFmEdit.UpdateDelayEdits(wVal: array of boolean; vX, vY, vTimes, vDelay: integer);
  begin
  ComboBox5.Visible := false;
  Edit5.Visible     := false;
  Label8.Visible    := false;
  ComboBox4.Visible := false;
  Edit8.Visible     := false;
  Label9.Visible    := false;
  Edit6.Visible     := false;
  CheckBox2.Visible := false;
  Panel2.Visible    := false;

  Label2.Visible    := true;
  Label3.Visible    := true;
  Edit1.Visible     := true;
  Edit2.Visible     := true;
  Label4.Visible    := true;
  Edit3.Visible     := true;
  Label5.Visible    := true;
  ComboBox1.Visible := true;

  Label4.Caption    := 'Times:';
  Label5.Caption    := 'Delay:';
  Label2.Caption    := 'X Position';
  Label3.Caption    := 'Y Position';


  if wVal[0] then
     Edit1.Text := inttostr(vX)
  else
     Edit1.text := '';
  if wVal[1] then
     Edit2.Text := inttostr(vY)
  else
     Edit2.text := '';
  if wVal[2] then
     Edit3.Text := inttostr(vTimes)
  else
     Edit3.Text := '';
  if wVal[3] then
     ComboBox1.ItemIndex := vDelay
  else
     begin
     ComboBox1.Text := '';
     ComboBox1.ItemIndex := -1;
     end;
  end;

//------------------------------------------------------------------------------------------------

procedure TFmEdit.UpdateIOEdits(wVal: array of boolean; vX, vY: integer; vInMin, vInMax,
          vOutMin, vOutMax: double);
  begin
  ComboBox1.Visible := false;
  ComboBox5.Visible := false;
  ComboBox4.Visible := false;
  CheckBox2.Visible := false;
  Panel2.Visible    := false;

  Label2.Visible    := true;
  Label3.Visible    := true;
  Edit1.Visible     := true;
  Edit2.Visible     := true;
  Label4.Visible    := true;
  Edit3.Visible     := true;
  Label5.Visible    := true;
  Edit5.Visible     := true;
  Label8.Visible    := true;
  Edit8.Visible     := true;
  Label9.Visible    := true;
  Edit6.Visible     := true;

  Edit6.Enabled     := true;

  Label2.Caption    := 'X Position';
  Label3.Caption    := 'Y Position';
  Label4.Caption    := 'Min(in):';
  Label5.Caption    := 'Max(in):';
  Label8.Caption    := 'Min(out):';
  Label9.Caption    := 'Max(out):';

  if wVal[0] then
     Edit1.Text := inttostr(vX)
  else
     Edit1.text := '';
  if wVal[1] then
     Edit2.Text := inttostr(vY)
  else
     Edit2.text := '';
  if wVal[2] then
     Edit3.Text := floattostr(vInMin)
  else
     Edit3.text := '';
  if wVal[3] then
     Edit5.Text := floattostr(vInMax)
  else
     Edit5.text := '';
  if wVal[4] then
     Edit8.Text := floattostr(vOutMin)
  else
     Edit8.text := '';
  if wVal[5] then
     Edit6.Text := floattostr(vOutMax)
  else
     Edit6.text := '';
  end;

//------------------------------------------------------------------------------------------------

procedure TFmEdit.UpdateLayers;
  var
    i, XLayerCount : integer;
  begin
  if FNetRep <> nil then
     begin
     SetLength(FLayersArray, ListBox1.Count);
     XLayerCount := FNetRep.GetLayers(FLayersArray);
     for i := 0 to ListBox1.Count - 1 do
       if FLayersArray[i] < 0 then
          case FNetRep.Node[i].Kind of
            UKInput  : FLayersArray[i] := XLayerCount;
            UKOutput : FLayersArray[i] := XLayerCount + 1;
            UkRec    : FLayersArray[i] := XLayerCount + 2;
            UKDelay  : FLayersArray[i] := XLayerCount + 3;
            end;
     ListBox5.Clear;
     for i := 0 to XLayerCount - 1 do
       ListBox5.AddItem('Layer ' + inttostr(i + 1), nil);
     ListBox5.AddItem('Input Units', nil);
     ListBox5.AddItem('Output Units', nil);
     ListBox5.AddItem('Link Points', nil);
     ListBox5.AddItem('Delay Units', nil);
     end;
  end;

//------------------------------------------------------------------------------------------------

procedure TFmEdit.UpdateNetworkProperties;
  begin
  RadioGroup1.ItemIndex := integer(FNetRep.LinkFlow);
  Edit18.Enabled  := FNetRep.LinkFlow in [LfCILP, LfSCTL, LfCML];
  Label20.Enabled := FNetRep.LinkFlow in [LfCILP, LfSCTL, LfCML];
  Edit19.Enabled  := FNetRep.LinkFlow in [LfCILP, LfSCTL, LfCML];
  Label21.Enabled := FNetRep.LinkFlow in [LfCILP, LfSCTL, LfCML];
  Edit18.Text := inttostr(FNetRep.Nu);
  Edit19.Text := inttostr(FNetRep.Upsilon);
  Edit10.Text := floattostr(FNetRep.EtaCorrection);
  AtualizaCombos;
  UpdateLayers;
  end;

//------------------------------------------------------------------------------------------------

procedure TFmEdit.UpdateNeuronEdits(wVal: array of boolean; vX, vY: integer; vEta, vMom,
          vPar: double; vAct: integer; vHasB, vFixB: boolean; vWeiB, vRanB: double);
  begin
  ComboBox1.Visible := false;
  ComboBox5.Visible := false;
  Edit8.Visible     := false;

  Label2.Visible    := true;
  Label3.Visible    := true;
  Edit1.Visible     := true;
  Edit2.Visible     := true;
  Label4.Visible    := true;
  Edit3.Visible     := true;
  Label5.Visible    := true;
  Edit5.Visible     := true;
  Label8.Visible    := true;
  ComboBox4.Visible := true;
  Label9.Visible    := true;
  Edit6.Visible     := true;
  CheckBox2.Visible := true;
  Panel2.Visible    := true;
  CheckBox3.Visible := true;
  Label10.Visible   := true;
  Label18.Visible   := true;
  Edit7.Visible     := true;
  Edit16.Visible    := true;

  Label2.Caption  := 'X Position';
  Label3.Caption  := 'Y Position';
  Label4.Caption  := 'Eta:';
  Label5.Caption  := 'Momentum:';
  Label8.Caption  := 'Activation:';
  Label9.Caption  := 'Parameter:';
  label10.Caption := 'Range:';
  label18.Caption := 'Weight:';


  if wVal[0] then
     Edit1.Text := inttostr(vX)
  else
     Edit1.text := '';
  if wVal[1] then
     Edit2.Text := inttostr(vY)
  else
     Edit2.text := '';
  if wVal[2] then
     Edit3.Text := floattostr(vEta)
  else
     Edit3.Text := '';
  if wVal[3] then
     Edit5.Text := FloatToStr(vMom)
  else
     Edit5.Text := '';
  if wVal[5] then
     ComboBox4.ItemIndex := vAct
  else
     begin
     ComboBox4.Text := '';
     ComboBox4.ItemIndex := -1;
     end;
  Edit6.Enabled := (ComboBox4.ItemIndex = 2) or (ComboBox4.ItemIndex = 5);
  if wVal[4] then
     Edit6.Text := floattostr(vPar)
  else
     Edit6.Text := '';
  if wVal[6] then
     CheckBox2.Checked := vHasB
  else
     CheckBox2.State := cbGrayed;
  if wVal[7] then
     CheckBox3.Checked := vFixB
  else
     CheckBox3.State := cbGrayed;
  if wVal[8] then
     Edit7.Text := floattostr(vWeiB)
  else
     Edit7.Text := '';
  if wVal[9] then
     Edit16.Text := floattostr(vRanB)
  else
     Edit16.Text := '';
  end;

//------------------------------------------------------------------------------------------------

procedure TFmEdit.UpdateNoEdits;
  begin
  ComboBox5.Visible := false;
  Edit5.Visible     := false;
  Label8.Visible    := false;
  ComboBox4.Visible := false;
  Edit8.Visible     := false;
  Label9.Visible    := false;
  Edit6.Visible     := false;
  CheckBox2.Visible := false;
  Panel2.Visible    := false;
  Label2.Visible    := false;
  Label3.Visible    := false;
  Edit1.Visible     := false;
  Edit2.Visible     := false;
  Label4.Visible    := false;
  Edit3.Visible     := false;
  Label5.Visible    := false;
  ComboBox1.Visible := false;
  end;

//------------------------------------------------------------------------------------------------

procedure TFmEdit.UpdateRecLinkEdits(wVal: array of boolean; vX, vY, vLev, vSty: integer;
          vInit: double);
  begin
  ComboBox1.Visible := false;
  Edit5.Visible     := false;
  ComboBox4.Visible := false;
  Label9.Visible := false;
  Edit6.Visible := false;
  CheckBox2.Visible := false;
  Panel2.Visible := false;

  Label2.Visible    := true;
  Label3.Visible    := true;
  Edit1.Visible     := true;
  Edit2.Visible     := true;
  Label4.Visible    := true;
  Edit3.Visible     := true;
  Label5.Visible    := true;
  ComboBox5.Visible := true;
  Label8.Visible    := true;
  Edit8.Visible     := true;

  Label2.Caption  := 'X Position';
  Label3.Caption  := 'Y Position';
  Label4.Caption := 'Level:';
  Label5.Caption := 'Mode:';
  Label8.caption := 'Initialization';

  if wVal[0] then
     Edit1.Text := inttostr(vX)
  else
     Edit1.text := '';
  if wVal[1] then
     Edit2.Text := inttostr(vY)
  else
     Edit2.text := '';
  if wVal[2] then
     Edit3.Text := inttostr(vLev)
  else
     Edit3.text := '';
  if wVal[3] then
     ComboBox5.ItemIndex := vSty
  else
     begin
     ComboBox5.Text := '';
     ComboBox5.ItemIndex := -1;
     end;
  if wVal[4] then
     Edit8.Text := floattostr(vInit)
  else
     Edit8.text := '';
  end;

//------------------------------------------------------------------------------------------------

procedure TFmEdit.UpdateSelectedNode(selected : integer = -1);
  var i : integer;
  begin
  if (selected >= 0) and (selected < Listbox1.Items.Count) then
     begin
     for i := 0 to ListBox1.Count - 1 do
       ListBox1.Selected[i] := (i = selected);
     ListBox1.ItemIndex := selected;
     PageControl1.ActivePageIndex := 1;
     end
  else
     begin
     if ListBox1.Count > 0 then
        ListBox1.ItemIndex := 0;
     PageControl1.ActivePageIndex := 0;
     end;
  ListBox1.OnClick(ListBox1);
//  UpdateLayers;
  end;

//------------------------------------------------------------------------------------------------

procedure TFmEdit.UpdateXYEdits(wVal: array of boolean; vX, vY: integer);
  begin
  ComboBox5.Visible := false;
  Edit5.Visible     := false;
  Label8.Visible    := false;
  ComboBox4.Visible := false;
  Edit8.Visible     := false;
  Label9.Visible    := false;
  Edit6.Visible     := false;
  CheckBox2.Visible := false;
  Panel2.Visible    := false;
  Label4.Visible    := false;
  Edit3.Visible     := false;
  Label5.Visible    := false;
  ComboBox1.Visible := false;

  Label2.Visible    := true;
  Label3.Visible    := true;
  Edit1.Visible     := true;
  Edit2.Visible     := true;

  Label2.Caption  := 'X Position';
  Label3.Caption  := 'Y Position';

  if wVal[0] then
     Edit1.Text := inttostr(vX)
  else
     Edit1.text := '';
  if wVal[1] then
     Edit2.Text := inttostr(vY)
  else
     Edit2.text := '';
  end;

//------------------------------------------------------------------------------------------------

procedure TFmEdit.Button1Click(Sender: TObject);
  begin
  FNetRep.AlterSCTLtoExtraction;
  OnChangeVisualProperties(-1);
  UpdateLayers;
  end;

procedure TFmEdit.Button2Click(Sender: TObject);
  begin
  case RadioGroup2.ItemIndex of
    0 : FNetRep.AddNode('Input' + InttoStr(FNetRep.NoNodes), UKInput);
    1 : FNetRep.AddNode('Output' + InttoStr(FNetRep.NoNodes), UKOutput);
    2 : FNetRep.AddNode('Link' + InttoStr(FNetRep.NoNodes), UkRec);
    3 : FNetRep.AddNode('Neuron' + InttoStr(FNetRep.NoNodes), UKNeuron);
    end;
  UpdateLayers;
  OnChangeVisualProperties(-1);
  end;

end.

