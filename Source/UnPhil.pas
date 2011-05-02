
unit UnPhil;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, UnNetRep, UnNetBehaviour,
  //xmldom, XMLIntf, msxmldom,  XMLDoc,
  StdCtrls, UnFunctions, Spin, Gauges, ExtDlgs, ExtCtrls, TeeProcs,
  TeEngine, Chart, Series, TeeFunci;



type TPhilosopher = class
  private
    FOldValues : array of double;
    FNetDesc : TNetworkRep;
    FNetwork : TRafaAnn;
  public
    constructor Create(Arq : TFileName);//; XMLDoc : TXMLDocument);
     destructor Destroy; override;

    procedure InputData(input : array of double; out output : array of double);
    procedure Correct(output : array of double);
    procedure SaveNet(fileName : TFileName);
  end;

type TDiningTable = class
   private
     FAgents     : TStringList;

     FPickLeft         : array of boolean;
     FPickRight        : array of boolean;
     FGotLeft          : array of boolean;
     FGotRight         : array of boolean;
     FDropLeft         : array of boolean;
     FEat              : array of Boolean;
     FDropRight        : array of boolean;
     FTriggerEat       : array of boolean;
     FTriggerThink     : array of boolean;
     FShouldPickLeft   : array of boolean;
     FShouldPickRight  : array of boolean;
     FShouldDropLeft   : array of boolean;
     FShouldDropRight  : array of boolean;
     FShouldEat        : array of boolean;

     FErrors           : array of double;

     FForks   : array of Integer;
     FStomach : array of integer;

   public
     Constructor Create(numberOfAgents : integer; Files : TStrings;
                   KnowledgeLevel : array of integer);//; XMLDoc : TXMLDocument);

     destructor Destroy; override;
     procedure SetNewTimePoint;
     procedure SaveNets(BaseFileName : string);
     function Execute(Offline, Correct : boolean) : string;
     function ConfigExecute(Offline, Correct, ShowDesired : boolean; ShowAgents : array of boolean) : string;
     function GetResult(li : TStrings) : boolean;
     function GetAgentsCount : integer;
   end;

type
  TFmOldPhil = class(TForm)
    MainMenu1: TMainMenu;
    Configurar1: TMenuItem;
//    XMLDocument1: TXMLDocument;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    OpenDialog1: TOpenDialog;
    Chart1: TChart;
    OpenDialog2: TOpenDialog;
    PopupMenu1: TPopupMenu;
    Delete1: TMenuItem;
    Corel1: TMenuItem;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    SpinEdit1: TSpinEdit;
    Button4: TButton;
    Button5: TButton;
    Button1: TButton;
    Button3: TButton;
    CheckBox5: TCheckBox;
    GroupBox3: TGroupBox;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    SpinEdit2: TSpinEdit;
    CheckBox3: TCheckBox;
    SpinEdit3: TSpinEdit;
    CheckBox4: TCheckBox;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    Button6: TButton;
    Button8: TButton;
    RadioButton3: TRadioButton;
    ListBox1: TListBox;
    Button2: TButton;
    Series2: TMenuItem;
    Gauge1: TGauge;
    GroupBox4: TGroupBox;
    Chart2: TChart;
    LineSeries1: TLineSeries;
    AverageTeeFunction1: TAverageTeeFunction;
    Series3: TLineSeries;
    Series4: TLineSeries;
    Button7: TButton;
    Button10: TButton;
    Button9: TButton;
    Button11: TButton;
    Chart3: TChart;
    LineSeries2: TLineSeries;
    AverageTeeFunction2: TAverageTeeFunction;
    Series1: TBarSeries;
    procedure Button1Click(Sender: TObject);
    procedure N2agentes1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Configurar1Click(Sender: TObject);
    procedure Delete1Click(Sender: TObject);
    procedure Corel1Click(Sender: TObject);
    procedure QuandoClicar(Sender : TObject);
    procedure Button7Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
  private
    FCancela : boolean;
    DiningTable : TDiningTable;
    Conta : integer;
    function convert(x : integer) : double;

    { Private declarations }
  public
    { Public declarations }
  end;

var
  FmOldPhil: TFmOldPhil;

implementation

{$R *.dfm}


{ TPhilosopher }

procedure TPhilosopher.Correct(output: array of double);
  begin
   FNetwork.ApplyOutputArray(output, 5);
 end;


constructor TPhilosopher.Create(Arq: TFileName);//; XMLDoc : TXMLDocument);
  var i : integer;
  begin
  FNetDesc := TNetworkRep.Create(nil, nil);
  FNetDesc.LoadFromXML(Arq);
  SetLength(FOldValues, 5);
  for i := 0 to 4 do
    FOldValues[i] := -1;
  FNetwork := TRafaAnn.Create;
  FNetwork.AddRealFunction(logsig, logsig_linha);
  FNetwork.AddRealFunction(tansig, tansig_linha);
  FNetwork.AddRealFunction(BiLogsig, BiLogsig_linha);
  FNetwork.AddRealFunction(Threshold, Threshold_Linha);
  FNetwork.AddRealFunction(BiThreshold, BiThreshold_Linha);
  FNetwork.AddRealFunction(Linear, Linear_Linha);
  FNetwork.LoadDescription(FNetDesc);
  FNetwork.SetMode(NmTraining);
  FNetwork.ResetNetwork;
  end;

destructor TPhilosopher.Destroy;
  begin
  FNetwork.Free;
  inherited;
  end;

procedure TPhilosopher.InputData(input: array of double; out output: array of double);
  var
    i : integer;
    arInput : array of double;
  begin
  if FNetDesc.GetInputCount = 4 then
     begin
     FNetwork.ApplyInputArray(input, 4);
     FNetwork.GetOutputArray(output, 5);
     end
  else
     begin
     SetLength(arInput, 9);
     for i := 0 to 3 do
       arInput[i] := input[i];
     for i := 0 to 4 do
       arInput[4 + i] := FOldValues[i];
     FNetwork.ApplyInputArray(arInput, 9);
     FNetwork.GetOutputArray(output, 5);
     for i := 0 to 4 do
       FOldValues[i] := output[i];
     end
  end;

procedure TPhilosopher.SaveNet(fileName : TFileName);
  begin
  FNetwork.SaveDescription(FNetDesc);
  FNetDesc.SaveToXML(fileName);
  end;

{ TDiningTable }

function TDiningTable.ConfigExecute(Offline, Correct, ShowDesired : boolean; ShowAgents : array of boolean) : string;
  var
    i, j : integer;
    arinputs : array[0..3] of double;
    aroutputs1, aroutputs2 : array[0..4] of double;
    sInput, sOutput, sDesOutput, sFinal: string;
  begin
  for i := 0 to FAgents.Count - 1 do
    begin
    if FGotLeft[i]      then arinputs[0] := 1 else arinputs[0] := -1;
    if FGotRight[i]     then arinputs[1] := 1 else arinputs[1] := -1;
    if FTriggerEat[i]   then arinputs[2] := 1 else arinputs[2] := -1;
    if FTriggerThink[i] then arinputs[3] := 1 else arinputs[3] := -1;

    (FAgents.Objects[i] as TPhilosopher).InputData(arinputs, aroutputs1);

    if OffLine then
      begin
      FPickLeft [i] := FShouldPickLeft [i];
      FPickRight[i] := FShouldPickRight[i];
      FDropLeft [i] := FShouldDropLeft [i];
      FDropRight[i] := FShouldDropRight[i];
      FEat[i]       := FShouldEat[i];
      end
    else
      begin
      FPickLeft [i] := (aroutputs1[0] > 0);
      FPickRight[i] := (aroutputs1[1] > 0);
      FDropLeft [i] := (aroutputs1[2] > 0);
      FDropRight[i] := (aroutputs1[3] > 0);
      FEat[i]       := (aroutputs1[4] > 0);
      end;

    if FShouldPickLeft [i] then aroutputs2[0] := 1 else aroutputs2[0] := -1;
    if FShouldPickRight[i] then aroutputs2[1] := 1 else aroutputs2[1] := -1;
    if FShouldDropLeft [i] then aroutputs2[2] := 1 else aroutputs2[2] := -1;
    if FShouldDropRight[i] then aroutputs2[3] := 1 else aroutputs2[3] := -1;
    if FShouldEat[i]       then aroutputs2[4] := 1 else aroutputs2[4] := -1;


    sInput := '';
    sOutput := '';
    sDesOutput := '';
    if (i < Length(ShowAgents)) and (ShowAgents[i]) then
       begin
       for j := 0 to 3 do
         sInput := sInput + floattostr(ArInputs[j]) + ' ';
       for j := 0 to 4 do
         sOutput := sOutput + floattostr(ArOutputs1[j]) + ' ';
       if ShowDesired then
          for j := 0 to 4 do
            sDesOutput := sDesOutput + floattostr(ArOutputs2[j]) + ' '
       else
          sDesOutput := '';
       sFinal := sFinal + sInput + sOutput + sDesOutput;
       end;

    if correct then
       (FAgents.Objects[i] as TPhilosopher).Correct(aroutputs2);
    Ferrors[i] := 0;
    for j := 0 to 4 do
      Ferrors[i] :=  Ferrors[i] + sqr(Aroutputs2[j] - ArOutputs1[j]);
    FErrors[i] := sqrt(FErrors[i] / 5);
    end;
  result := sFinal;
  end;


constructor TDiningTable.Create(numberOfAgents: integer; Files : TStrings;
  KnowledgeLevel: array of integer);//; XMLDoc : TXMLDocument);
  var
    i : integer;
    Ag : TPhilosopher;
  begin
  randomize;
  FAgents := TStringList.Create;
  for i := 0 to numberOfAgents - 1 do
    begin
    Ag := TPhilosopher.Create(Files.Strings[KnowledgeLevel[i]]);//, XMLDoc);
    FAgents.AddObject('Philosopher' + inttostr(i), Ag);
    end;

  SetLength(FPickLeft, numberofAgents);
  SetLength(FPickRight, numberofAgents);
  SetLength(FGotLeft, numberofAgents);
  SetLength(FGotRight, numberofAgents);
  SetLength(FDropLeft, numberofAgents);
  SetLength(FEat, numberofAgents);
  SetLength(FDropRight, numberofAgents);
  SetLength(FTriggerEat, numberofAgents);
  SetLength(FTriggerThink, numberofAgents);
  SetLength(FShouldPickLeft, numberofAgents);
  SetLength(FShouldPickRight, numberofAgents);
  SetLength(FShouldDropLeft, numberofAgents);
  SetLength(FShouldDropRight, numberofAgents);
  SetLength(FShouldEat, numberofAgents);
  SetLength(FForks, numberofAgents);
  SetLength(FStomach, numberofAgents);
    SetLength(FErrors, numberofAgents);

  for i := 0 to numberofAgents - 1 do
    FStomach[i] := - abs(round(random * 5)) - 1;
  end;




destructor TDiningTable.Destroy;
  var i : integer;
  begin
  for i := 0 to FAgents.Count - 1 do
    FAgents.Objects[i].Free;
  FAgents.Free;
  inherited;
  end;


function TDiningTable.Execute(Offline, Correct : boolean) : string;
  var
    i, j : integer;
    arinputs : array[0..3] of double;
    aroutputs1, aroutputs2 : array[0..4] of double;
    sInput, sOutput : string;
  begin
  sInput := '';
  sOutput := '';
  for i := 0 to FAgents.Count - 1 do
    begin
    if FGotLeft[i]      then arinputs[0] := 1 else arinputs[0] := -1;
    if FGotRight[i]     then arinputs[1] := 1 else arinputs[1] := -1;
    if FTriggerEat[i]   then arinputs[2] := 1 else arinputs[2] := -1;
    if FTriggerThink[i] then arinputs[3] := 1 else arinputs[3] := -1;

    (FAgents.Objects[i] as TPhilosopher).InputData(arinputs, aroutputs1);

    for j := 0 to 3 do
      sInput := sInput + floattostr(ArInputs[j]) + ' ';
    for j := 0 to 4 do
      sOutput := sOutput + floattostr(ArOutputs1[j]) + ' ';

    if OffLine then
      begin
      FPickLeft [i] := FShouldPickLeft [i];
      FPickRight[i] := FShouldPickRight[i];
      FDropLeft [i] := FShouldDropLeft [i];
      FDropRight[i] := FShouldDropRight[i];
      FEat[i]       := FShouldEat[i];
      end
    else
      begin
      FPickLeft [i] := (aroutputs1[0] > 0);
      FPickRight[i] := (aroutputs1[1] > 0);
      FDropLeft [i] := (aroutputs1[2] > 0);
      FDropRight[i] := (aroutputs1[3] > 0);
      FEat[i]       := (aroutputs1[4] > 0);
      end;

    if FShouldPickLeft [i] then aroutputs2[0] := 1 else aroutputs2[0] := -1;
    if FShouldPickRight[i] then aroutputs2[1] := 1 else aroutputs2[1] := -1;
    if FShouldDropLeft [i] then aroutputs2[2] := 1 else aroutputs2[2] := -1;
    if FShouldDropRight[i] then aroutputs2[3] := 1 else aroutputs2[3] := -1;
    if FShouldEat[i]       then aroutputs2[4] := 1 else aroutputs2[4] := -1;

    if correct then
       (FAgents.Objects[i] as TPhilosopher).Correct(aroutputs2);
    Ferrors[i] := 0;
    for j := 0 to 4 do
      Ferrors[i] :=  Ferrors[i] + sqr(Aroutputs2[j] - ArOutputs1[j]);
    FErrors[i] := sqrt(FErrors[i] / 5);
    end;
  result := sInput + sOutput;
  end;

function TDiningTable.GetAgentsCount : integer;
  begin
  result := FAgents.Count;
  end;

function TDiningTable.GetResult(li: TStrings) : boolean;
  var
    i, j : integer;
    s : string;
  begin
  s := '';
  for i := 0 to FAgents.count - 1 do
    begin
    s := s + floattostr(FErrors[i]) + ' ';
    s := s + inttostr(FStomach[i]) + ' ';
    end;
  li.Add(s);

  j := 0;
  for i := 0 to FAgents.count - 1 do
    j := j + FForks[i];


  result := (abs(j) >= FAgents.count);

 end;


procedure TDiningTable.SaveNets(BaseFileName : string);
  var i : integer;
  begin
  for i := 0 to FAgents.Count - 1 do
    (FAgents.Objects[i] as TPhilosopher).SaveNet(BaseFileName + inttostr(i + 1) + '.xml');
  end;


procedure TDiningTable.SetNewTimePoint;
   var
     i : integer;
     XLeft, XRight : integer;
     DeadLock : boolean;

   begin
   //Corrige para ver quem está com cada garfo
   for i := 0 to FAgents.Count - 1 do
     begin
     XLeft  := (i + FAgents.Count - 1) mod FAgents.Count;
     XRight := i;
     FGotRight[XLeft] := false;
     FGotLeft[XRight] := false;
     if FForks[i] = 0 then
        begin
        if FPickRight[Xleft] then
           begin
           FGotRight[XLeft] := true;
           FForks[i] := -1;
           end
        else if FPickLeft[XRight] then
           begin
           FGotLeft[XRight] := true;
           FForks[i] := 1;
           end;
        end
     else
        begin
        if ((FForks[i] = 1)  and (FDropLeft[XRight])) or
           ((FForks[i] = -1) and (FDropRight[XLeft])) then
           FForks[i] := 0;
        end;
     end;

   deadlock := true;
   for i := 0 to FAgents.Count - 1 do
     deadlock := deadlock and (FGotRight[i] or (FForks[(i + 1) mod FAgents.Count] < 0));
   if deadlock then
      begin
      i := round(abs(random) * (FAgents.Count - 1));
      while FGotLeft[i] do
        i := (i + 1) mod FAgents.count;
      FGotRight[i] := false;
      FForks[(i + 1) mod FAgents.Count] := 0;
      end;


   deadlock := true;
   for i := 0 to FAgents.Count - 1 do
     deadlock := deadlock and (FGotLeft[i] or (Fforks[i] > 0));
   if deadlock then
      begin
      i := round(abs(random) * (FAgents.Count - 1));
      while not FGotLeft[i] do
        i := (i + 1) mod FAgents.count;
      FGotLeft[i] := false;
      FForks[i] := 0;
      end;


   //Corrige para ver quem está a fim de comer e quem está a fim de pensar

   for i := 0 to FAgents.Count - 1 do
     begin
     XLeft  := i;
     XRight := (i + 1) mod FAgents.Count;
     FTriggerThink[i] := false;
     FTriggerEat[i] := false;
     if (FForks[XLeft] = 1) and (FForks[XRight] = -1) and (FStomach[i] > 0) and FEat[i]then
        begin
        FStomach[i] := FStomach[i] - 1;
        if FStomach[i] = 0 then
           begin
           FStomach[i] := -abs(round(random * 3)) - 1;
           FTriggerThink[i] := true;
           end;
        end;
     if (FForks[XLeft] <> 1) and (FForks[XRight] <> -1) and (FStomach[i] < 0) then
        begin
        FStomach[i] := FStomach[i] + 1;
        if FStomach[i] = 0 then
           begin
           FStomach[i] := abs(round(random * 3)) + 1;
           FTriggerEat[i] := true;
           end;
        end;
     end;

   //Corrige para ver qual é deverá ser a próxima ação de cada um
   for i := 0 to FAgents.Count - 1 do
     begin
     XLeft  := i;
     XRight := (i + 1) mod FAgents.Count;
     FShouldPickLeft [i] := (FForks[XLeft] <> 1) and (FStomach[i] > 0);
     FShouldPickRight[i] := (FForks[XLeft] = 1) and (FForks[XRight] <> -1) and (FStomach[i] > 0);
     FShouldDropLeft [i] := (FForks[XLeft] = 1) and (FStomach[i] <= 0);
     FShouldDropRight[i] := (FForks[XRight] = -1) and (FStomach[i] <= 0);
     FShouldEat[i]       := (FForks[XLeft] = 1) and (FForks[XRight] = -1) and (FStomach[i] > 0);
     end;

   //Pensar prevenção para deadlocks...

   end;

procedure TFmOldPhil.Button1Click(Sender: TObject);
  var
  j, k : integer;
    st : TStringList;
    s : string;
    xIOStrList : TStringList;

  begin
  xIOStrList := TStringList.Create;
  if opendialog2.Execute then
    begin
    FCancela := false;
    St := TStringList.Create;
    k := 0;
    if CheckBox5.Checked then
       while (not FCancela) and (k < 100000)  do  //define the count externally
         begin
         DiningTable.SetNewTimePoint;
         s := DiningTable.Execute(false, true);
         DiningTable.GetResult(St);
         k := k + 1;
         Gauge1.Progress := round(k/1000);
         Application.ProcessMessages;
         if K >= 90000 then
            xIOStrList.Add(s);
         end
    else
       begin
       while (not FCancela) and (k < 1000)  do  //define the count externally
         begin
         j := 0;
         while (not FCancela) and (j < 100)  do  //define the count externally
           begin
           DiningTable.SetNewTimePoint;
           DiningTable.Execute(true, true);
//           DiningTable.GetResult(St);
           j := j + 1;
           Application.ProcessMessages;
           end;
         j := 0;
         while (not FCancela) and (j < 100)  do  //define the count externally
           begin
           DiningTable.SetNewTimePoint;
           s := DiningTable.Execute(false, false);
           DiningTable.GetResult(St);
           j := j + 1;
           Application.ProcessMessages;
//           if k >= 900 then
              xIOStrList.Add(s);
           end;
         k := k + 1;
         Gauge1.Progress := round(k/10);
         end
       end;
    DiningTable.SaveNets(Opendialog2.FileName);
    xIOStrList.SaveToFile(Opendialog2.FileName + '_hist.txt');
    st.SaveToFile(Opendialog2.FileName);
    st.Free;
    end;
  xIOStrList.Free;
  end;

procedure TFmOldPhil.N2agentes1Click(Sender: TObject);
  begin
  DiningTable := TDiningTable.Create(2, ListBox1.Items, [0, 0]);//, XMLDocument1);
  end;

procedure TFmOldPhil.Button2Click(Sender: TObject);
  begin
  if OpenDialog1.Execute then
     ListBox1.Items.Add(OpenDialog1.FileName);
  end;

procedure TFmOldPhil.Button3Click(Sender: TObject);
  begin
  FCancela := true;
  color := ClBtnFace;
  end;

procedure TFmOldPhil.Button4Click(Sender: TObject);
  var x : array of integer;
      i : integer;
  begin
  SetLength(x, SpinEdit1.Value);
  for i := 0 to SpinEdit1.Value - 1 do
     x[i] := (GroupBox2.Controls[i] as TComboBox).ItemIndex;
  DiningTable := TDiningTable.Create(SpinEdit1.Value, ListBox1.Items, x);//, XMLDocument1);
  end;

procedure TFmOldPhil.SpinEdit1Change(Sender: TObject);
  var i, j : integer;
  begin
  for i := GroupBox2.ControlCount - 1 downto 0 do
    GroupBox2.Controls[i].Free;
  for i := 0 to SpinEdit1.Value - 1 do
    begin
    with TComboBox.Create(self) do
      begin
      parent := GroupBox2;
      top := (i * 20) + 20;
      left := 8;
      width := 150;
      for j := 0 to ListBox1.Count - 1 do
        Items.Add(ListBox1.Items[j]);
      ItemIndex := 0;
      end;
    end;
  end;

procedure TFmOldPhil.Button5Click(Sender: TObject);
  begin
  DiningTable.Free;
  end;

procedure TFmOldPhil.Button6Click(Sender: TObject);
  var
    t : tLineSeries;
    data : array of array of double;
    Z : textFile;
    k, i, j, c, max, weat, deat: integer;
    b : boolean;
    v : double;
  begin
  if opendialog2.Execute then
    begin
    max := 100000;

    for i := Chart1.SeriesCount - 1 downto 0 do
      Chart1.Series[i].Free;

    if RadioButton1.Checked then
       begin
       //configure error chart
       Chart1.LeftAxis.Automatic := false;
       Chart1.LeftAxis.Minimum := 0;
       Chart1.LeftAxis.Maximum := 1;
       Chart1.LeftAxis.Increment := 0.001;
//       Chart2.LeftAxis.Automatic := false;
//       Chart2.LeftAxis.Minimum := 0;
//       Chart2.LeftAxis.Maximum := 1.0;
//       Chart2.LeftAxis.Increment := 0.1;
       end
    else if RadioButton3.Checked then
       begin
       //configure allocation chart
       Chart1.LeftAxis.Automatic := false;
       Chart1.LeftAxis.Minimum := 0;
       Chart1.LeftAxis.Maximum := 0.5;
       Chart1.LeftAxis.Increment := 0.1;
       Chart2.LeftAxis.Automatic := false;
       Chart2.LeftAxis.Minimum := 0;
       Chart2.LeftAxis.Maximum := 0.5;
       Chart2.LeftAxis.Increment := 0.1;
       end
    else
       begin
       Chart1.LeftAxis.Automatic := true;
       end;

//    if checkbox3.Checked then
//       Chart1.BottomAxis.Increment := 0.1;



    SetLength(data, SpinEdit1.Value);
    for i := 0 to SpinEdit1.Value - 1 do
       SetLength(Data[i], max);

    AssignFile(Z, OpenDialog2.FileName);
    reset(z);
    i := 0;
    b := radiobutton1.Checked;
    while (not eof(z)) and (i < max) do
      begin
      j := 0;
      while (not eof(z)) and (j < SpinEdit1.Value) do
        begin
        read(z, data[j, i]);
        if b then j := j + 1;
        b := not b;
        end;
      i := i + 1;
      end;

    c := 0;
    if radiobutton2.Checked then
       begin
       for j := 0 to SpinEdit1.Value - 1 do
         for i := 1 to max - 2 do
           begin
           if (data[j, i - 1] > 0) and (data[j, i - 1] <= data[j, i]) then
              c := c + 1
           else
              c := 0;
           data[j, i - 1] := c;
           end
       end;

    if Radiobutton3.checked then
       begin
       for i := 1 to max - 1 do
         for j := 0 to SpinEdit1.Value - 1 do
           begin
           if (data[j, i - 1] > 0) and (data[j, i - 1] > data[j, i]) then deat := 1 else deat := 0;
           if (data[j, i - 1] > 0) then  weat := 1 else weat := 0;
           data[j, i - 1] := (deat * 10000) + weat;
           end;
         end;



     if checkbox3.Checked then
         begin
         for k := 0 to SpinEdit1.Value - 1 do
           for i := 0 to (max - 1) div SpinEdit3.Value do
             begin
             c := 0;
             v := 0;
             for j := 0 to SpinEdit3.Value - 1 do
               begin
               if (i * SpinEdit3.Value) + j < max then
                 begin
                 if radiobutton3.Checked then
                    begin
                    if convert(round(data[k, (i * SpinEdit3.Value) + j])) >= 0 then
                       begin
                       c := c + 1;
                       v := v + convert(round(data[k, (i * SpinEdit3.Value) + j]));
                       end
                    end
                 else
                    begin
                    c := c + 1;
                    v := v + data[k, (i * SpinEdit3.Value) + j];
                    end;
                 end;
               end;
             if radiobutton3.Checked then
                data[k, i] := (v * 10000) + c
             else
                data[k, i] := v/c;
             end;
         max := ((max - 1) div SpinEdit3.Value) + 1;
         end;


//  if checkbox3.Checked then;
//     for j := 0 to SpinEdit1.Value - 1 do
//       for i := 0 to 999999 do
//         begin


  if checkbox4.Checked then
     begin
     if checkbox1.Checked then
        begin
        t := TLineSeries.Create(Chart1);
        chart1.AddSeries(t);
        for i := 0 to max - 1 do
           begin
           v := 0;
           c := 0;
           for j := 0 to SpinEdit1.Value - 1 do
             begin
             if radiobutton3.Checked then
                begin
                if convert(round(data[j, i])) >= 0 then
                   begin
                   c := c + 1;
                   v := v + convert(round(data[j, i]));
                   end;
                end
             else
                begin
                c := c + 1;
                v := v + data[j, i];
                end;
             end;
           if c > 0 then
              t.AddXY(i, v/c)
           else
              t.AddXY(i, 0);
           end;
        end
     else
        for j := 0 to SpinEdit1.Value - 1 do
          begin
          t := TLineSeries.Create(Chart1);
          chart1.AddSeries(t);
          for i := 0 to max - 1 do
            if radiobutton3.Checked then
               t.AddXY(i, convert(round(data[j, i])))
            else
               t.AddXY(i, data[j, i]);
          end
       end;
     end;
  end;

procedure TFmOldPhil.Button8Click(Sender: TObject);
  var i : integer;
     tmp : tmenuItem;
  begin
  chart1.Visible := true;
  GroupBox1.Visible := false;
  for i := Series2.Count - 1 downto 0 do
    Series2.Items[i].Free;
  for i := 0 to Chart1.SeriesCount - 1 do
    begin
    tmp := TMenuItem.Create(self);
    tmp.Checked := true;
    tmp.Tag := i;
    tmp.OnClick := QuandoCLicar;
    Series2.Add(tmp);
    end;


  //  for i := Chart1.SeriesCount - 1 downto 1 do
//    Chart1.Series[i].Free;
  end;

procedure TFmOldPhil.Configurar1Click(Sender: TObject);
  begin
  Chart1.Visible := false;
  Groupbox1.Visible := true;
  end;

procedure TFmOldPhil.Delete1Click(Sender: TObject);
  begin
  If ListBox1.ItemIndex >= 0 then
     ListBox1.Items.Delete(listbox1.ItemIndex);
  end;

function TFmOldPhil.convert(x: integer): double;
  begin
  if x mod 10000 = 0 then
     result := -1
  else
     result := (x div 10000) / (x mod 10000);
  end;

procedure TFmOldPhil.Corel1Click(Sender: TObject);
   begin
  chart1.SaveToMetafile(OpenDialog2.filename + '.wmf');
  end;

procedure TFmOldPhil.QuandoClicar(Sender: TObject);
  var i : integer;
  begin
  (Sender as TMenuItem).Checked := not (Sender as TMenuItem).Checked;
  for i := 0 to Series2.Count - 1 do
    Chart1.Series[i].Active := Series2.Items[i].Checked;

  end;

procedure TFmOldPhil.Button7Click(Sender: TObject);
  var i : integer;
  begin
  Chart2.Series[conta].Clear;
  for i := 0 to Chart1.Series[0].Count - 1 do
    Chart2.Series[conta].AddXY(i, Chart1.Series[0].YValues[i] {* 1000});

  Conta := (Conta + 1) mod 3;

  end;

procedure TFmOldPhil.FormCreate(Sender: TObject);
  begin
  Conta := 0;
  end;

procedure TFmOldPhil.Button9Click(Sender: TObject);
begin
Chart2.SaveToMetafile('UmGraficoLegal.wmf');
end;

procedure TFmOldPhil.Button10Click(Sender: TObject);
//var  i : integer;
begin
{  Series5.AddXY(0, 50);
  Series5.AddXY(500, 50);
  Series6.AddXY(0, 10);
  Series6.AddXY(500, 10);
  Series7.AddXY(0, 5);
  Series7.AddXY(500, 5);}

end;

procedure TFmOldPhil.Button11Click(Sender: TObject);
  var
    i, j, k : integer;
    st : TStringList;
    s : string;
    xIOStrList : TStringList;
    arDesired : array of boolean;

  begin
  xIOStrList := TStringList.Create;
  setLength(arDesired, DiningTable.getAgentsCount);
  for i := 0 to DiningTable.getAgentsCount - 1 do
    arDesired[i] := true;

  if opendialog2.Execute then
    begin
    FCancela := false;
    St := TStringList.Create;
    k := 0;
    while (not FCancela) and (k < 1000)  do  //define the count externally
      begin
      j := 0;
      while (not FCancela) and (j < 100)  do  //define the count externally
        begin
        DiningTable.SetNewTimePoint;
        s := DiningTable.ConfigExecute(true, false, true, arDesired);

        DiningTable.GetResult(St);
        j := j + 1;
        Application.ProcessMessages;
        xIOStrList.Add(s);
        end;
      k := k + 1;
      Gauge1.Progress := round(k/10);
      end;
    xIOStrList.SaveToFile(Opendialog2.FileName + '_hist.txt');
    st.SaveToFile(Opendialog2.FileName);
    st.Free;
    end;
  xIOStrList.Free;
  end;

end.


