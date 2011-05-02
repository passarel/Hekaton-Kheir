unit UnFmMuddyChildren;

//Possibilities of training
  //Online X offline: How does the system reacts if one or more of the children are not that smart
  //Supervision: What should be teached to the children: if she is muddy or if she should know that she is muddy???
  //Different number of kids on the environment
  //How information about time is kept: extra... or...
  //Apply more information when supervising learning may be useful (some of the recurrent links, for instance)


//------------------------------------------------------------------------------

interface

//------------------------------------------------------------------------------

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, UnNetRep, UnNetBehaviour, StdCtrls, ComCtrls, ExtCtrls, Spin,
  Menus, TeeProcs, TeEngine, Chart, Gauges;

//------------------------------------------------------------------------------

type TTemporalNSKind = (TKSCTL, TKCTLK, TKother);

//------------------------------------------------------------------------------

type TOutputPair = record
  Expected : integer;
  Obtained : double;
  end;

//------------------------------------------------------------------------------

type TChildBrain = class
  private
    FKind : TTemporalNSKind;
    FNetwork : TRafaANN; //Or something like it...
    FTimePoints : integer;
    FInputValues : array of double;

    FOutputValues : array of double;

    procedure NewGame;
    function GetKind: TTemporalNSKind;
    procedure SetKind(const Value: TTemporalNSKind);

  public
    constructor Create (TotalAgents : integer);
    destructor  Destroy; override;

    property kind : TTemporalNSKind read GetKind write SetKind;

    function Act(timePoint : integer) : double;
    procedure Sense(data : array of boolean; timePoint: integer);
    function Supervise (timepoint : integer; expectedAction : boolean) : boolean;

    procedure LoadNetwork(net : TNetworkRep);
  end;

//------------------------------------------------------------------------------

type TMuddyAgent = class
  private
    FX : integer;
    FY : integer;
    FImage : TImage;
    Fname : string;

    FBrain : TChildBrain;
    FIsMuddy : boolean;
    FWatcher : boolean;
    procedure SetMuddy(value : boolean);
    function  GetMuddy : boolean;
    function GetX: integer;
    function GetY: integer;
    procedure SetX(const Value: integer);
    procedure SetY(const Value: integer);
    function GetImage: TImage;
    function GetKind: TTemporalNSKind;
    procedure SetKind(const Value: TTemporalNSKind);
    function GetName: string;
    procedure SetName(const Value: string);
    function GetWatcher: boolean;
    procedure SetWatcher(const Value: boolean);

    procedure UpdatePicture(know : boolean = false);

  public
    constructor Create(Container : TControl; TotalAgents : integer);
    destructor  Destroy; override;
    property IsMuddy : boolean read GetMuddy write SetMuddy;
    property IsWatcher : boolean read GetWatcher write SetWatcher;
    property X : integer read GetX write SetX;
    property y : integer read GetY write SetY;
    property Image : TImage read GetImage;
    property Kind : TTemporalNSKind read GetKind write SetKind;
    property Name : string read GetName write SetName;

    function Act(timePoint : integer) : double;
    procedure Sense(data : array of boolean; timePoint : integer);
    function Supervise (timepoint : integer; expectedAction : boolean) : boolean;
    procedure LoadBrainFromFile(filename : string);
    procedure NewGame;

  end;

//------------------------------------------------------------------------------

type TMuddyConfig = class
  private
    FName : string;
    FBrain : integer;
    FWatch : boolean;
    FMuddyProb : integer;
    FWatched : integer;
    function GetName : string;
    function GetBrain : integer;
    function GetWatch : boolean;
    function GetMuddyProb : integer;
    function GetWatched : integer;
    procedure SetName (value : string);
    procedure SetBrain (value : integer);
    procedure SetWatch (value : boolean);
    procedure SetMuddyProb (value : integer);
    procedure SetWatched (value : integer);

  public
    property Name : string read GetName write Setname;
    property Brain : integer read GetBrain write SetBrain;
    property IsWatcher : boolean read GetWatch write SetWatch;
    property MuddyProb : integer read GetMuddyProb write SetMuddyProb;
    property Watched : integer read GetWatched write SetWatched;

    constructor Create;
    Destructor  Destroy; override;

  end;

//------------------------------------------------------------------------------

type TMuddyEnvironment = class
  private
    FTimePoint  : integer;
    FAgents     : TStringList;
    FWatchers   : TStringList;
    FProb       : array of double;
    FWatching   : array of integer;
    FGameHist   : array of array of TOutputPair;

    FOnlyLogic : boolean;
    FAlwaysQ1  : boolean;
    FStopCrit  : integer;
    FInitKind  : integer;

    FStrictMax : integer;

//    FMuddyChart : TMuddyChart;

//    FDesired  : array of array of array of double;
//    FObtained : array of array of array of double;

    procedure SetOnlyLogic(value : boolean);
    procedure SetAlwaysQ1(value : boolean);
    procedure SetStopCrit(value : integer);
    procedure SetInitKind(value : integer);

    Function GetOnlyLogic : boolean;
    Function GetAlwaysQ1  : boolean;
    Function GetStopCrit  : integer;
    Function GetInitKind  : integer;

    procedure Turn;
    function GetStrictMax: integer;
    procedure SetStrictMax(const Value: integer);

    function TmpExecute(strList : TStrings; gauge : TGauge) : boolean;
    function TmpIsKeyword(s : String) : boolean;
    function TmpLoadOptions(strList : TStrings) : boolean;
    function TmpLoadAgent(strList : TStrings) : TMuddyConfig;

  public


    constructor Create;
    destructor Destroy; override;

    property OnlyLogic : boolean read GetOnlyLogic write SetOnlyLogic;
    property AlwaysQ1  : boolean read GetAlwaysQ1  write SetAlwaysQ1;
    property StopCrit  : integer read GetStopCrit  write SetStopCrit;
    property InitKind  : integer read GetInitKind  write SetInitKind;

    property StrictMax : integer read GetStrictMax write SetStrictMax;

    function LoadDescription(parent : TControl; desc, NetList : TStrings) : boolean;
    procedure InitialiseAgents;

    procedure ClearAgents;

    function Step : integer;
    function Run(FileName : TFileName; gauge : TGauge; GameCount : integer) : integer;

    function TmpLoadFromFile(FileName : TFileName; parent : TControl; gauge : TGauge) : integer;

  end;

//------------------------------------------------------------------------------

type
  TFmMuddyChildren = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    GroupBox1: TGroupBox;
    ListBox1: TListBox;
    Edit1: TEdit;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    OpenDialog1: TOpenDialog;
    GroupBox2: TGroupBox;
    ListBox2: TListBox;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Panel1: TPanel;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    ComboBox1: TComboBox;
    CheckBox1: TCheckBox;
    ComboBox2: TComboBox;
    Label3: TLabel;
    SpinEdit1: TSpinEdit;
    Button5: TButton;
    Button9: TButton;
    Button10: TButton;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    ComboBox3: TComboBox;
    ComboBox4: TComboBox;
    Label4: TLabel;
    Label5: TLabel;
    Gauge1: TGauge;
    PopupMenu2: TPopupMenu;
    asdasd1: TMenuItem;
    SaveDialog1: TSaveDialog;
    SpinEdit2: TSpinEdit;
    Label6: TLabel;
    Label7: TLabel;
    SpinEdit3: TSpinEdit;
    Button11: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure ListBox2Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
  private
     FEnv : TMuddyEnvironment;
  public
    { Public declarations }
  end;

//------------------------------------------------------------------------------

var
  FmMuddyChildren: TFmMuddyChildren;

//------------------------------------------------------------------------------

implementation

{$R *.dfm}

uses UnRafaAux2007, Math, UnFunctions, series;

//------------------------------------------------------------------------------

{ TChildBrain }

//------------------------------------------------------------------------------

function TChildBrain.Act(timePoint: integer): double;
  var
    r : double;
  begin
  r := -1;
  case FKind of
    TKSCTL :
      begin
      FNetwork.GetOutputArray(FOutputValues, 1);
      r :=  FOutputValues[0];
      end;
    TKCTLK :
      if (timepoint >= 0) and (timepoint < FTimepoints) then //
         begin
         FNetwork.GetOutputArray(FOutputValues, FTimePoints);
         r := FOutputValues[timePoint];
         end;
    end;
  result := r;
  end;

//------------------------------------------------------------------------------

constructor TChildBrain.Create(TotalAgents: integer);
  begin
  inherited Create;
  FTimePoints := TotalAgents;
  //Initialise network
  end;

//------------------------------------------------------------------------------

destructor TChildBrain.Destroy;
  begin
  if FNetwork <> nil then
     FNetwork.Free;
  inherited;
  end;

//------------------------------------------------------------------------------

function TChildBrain.GetKind: TTemporalNSKind;
  begin
  result := FKind;
  end;

//------------------------------------------------------------------------------

procedure TChildBrain.LoadNetwork(net: TNetworkRep);
  begin
  if FNetwork = nil then
     begin
     FNetwork := TRafaANN.Create;
     FNetwork.AddRealFunction(logsig, logsig_linha);
     FNetwork.AddRealFunction(tansig, tansig_linha);
     FNetwork.AddRealFunction(Bilogsig, Bilogsig_linha);
     FNetwork.AddRealFunction(Threshold, Threshold_linha);
     FNetwork.AddRealFunction(BiThreshold, BiThreshold_linha);
     FNetwork.AddRealFunction(Linear, Linear_linha);
     FNetwork.AddRealFunction(CrazyA, CrazyA_Linha);
     end;
  if net.WorldCount > 1 then
     FKind := TKCTLK
  else
     FKind := TKSCTL;
  FNetwork.LoadDescription(net);
  FNetwork.ResetNetwork;

  if FKind = TKSCTL then
     begin
     SetLength(FInputValues, (FTimePoints * 2) - 1);
     SetLength(FOutputValues, 1)
     end
  else
     begin
     SetLength(FInputValues, FTimepoints * ((FTimePoints * 2) - 1));
     SetLength(FOutputValues, FTimePoints);
     end;
  end;

//------------------------------------------------------------------------------

procedure TChildBrain.NewGame;
  var
    i, j : integer;
  begin
  FNetwork.InitializeEpoch;
  if FKind = TKSCTL then
     j := (FTimePoints * 2) - 1
  else
     begin
     j := FTimePoints * ((FTimePoints * 2) - 1);
     for i := 0 to FTimePoints - 1 do
       FOutputValues[i] := -1;
     end;
  for i := 0 to j - 1 do
    FInputValues[i] := -1;
  end;

//------------------------------------------------------------------------------

procedure TChildBrain.Sense(data : array of boolean; timePoint: integer);
  var
    i, inps{, init} : integer;
  begin
  if TimePoint = 0 then NewGame;
  inps := (FTimePoints * 2) - 1;
  case FKind of
    TKSCTL :
      begin
      for i := 0 to inps - 1 do
        if data[i] then FInputValues[i] := 1 else FInputValues[i] := -1;
      FNetwork.ApplyInputArray(FInputValues, inps);
      end;
    TKCTLK :
      begin
//      init := timepoint * inps;
      for i := 0 to inps - 1 do
        if data[i] then
           FInputValues[(i * FTimePoints) + timepoint] := 1
        else
           FInputValues[(i * FTimePoints) + timepoint] := -1;
      FNetwork.ApplyInputArray(FInputValues, FTimePoints * inps);
      end;
    end;
  end;

//------------------------------------------------------------------------------

procedure TChildBrain.SetKind(const Value: TTemporalNSKind);
  begin
  FKind := value;
  end;

//------------------------------------------------------------------------------

function TChildBrain.Supervise(timepoint: integer;  expectedAction: boolean): boolean;
  begin
  case FKind of
    TKSCTL :
      begin
      if expectedAction then FOutputValues[0] := 1 else FOutputValues[0] := -1;
      FNetwork.ApplyOutputArray(FOutputValues, 1);
      end;
    TKCTLK :
      begin
      if expectedAction then FOutputValues[timePoint] := 1 else FOutputValues[timepoint] := -1;
      FNetwork.ApplyOutputArray(FOutputValues, 1);
      end;
    end;
  result := true;
  end;

//------------------------------------------------------------------------------

{ TMuddyAgent }

//------------------------------------------------------------------------------

function TMuddyAgent.Act(timePoint: integer): double;
  var d : double;
  begin
  d := FBrain.Act(timePoint);
  UpdatePicture(d > 0);
  result := d;
  end;

//------------------------------------------------------------------------------

constructor TMuddyAgent.Create(container : tcontrol; TotalAgents : integer);
  begin
  inherited Create;
  FImage := TImage.Create(container.Owner);
  FImage.Parent := Container as TWinControl;
  FImage.Picture.LoadFromFile(ExtractFileDir(Application.ExeName) + '\..\modules\muddychildren\smiley_01.bmp');
  FImage.AutoSize := true;
  X := 0;
  Y := 0;
  FWatcher := false;
  FBrain := TChildBrain.Create(TotalAgents);
  FIsMuddy := false;
  end;

//------------------------------------------------------------------------------

destructor TMuddyAgent.Destroy;
  begin
  FImage.Free;
  FBrain.Free;
  inherited;
  end;

//------------------------------------------------------------------------------

function TMuddyAgent.GetImage: TImage;
  begin
  Result := FImage;
  end;

//------------------------------------------------------------------------------

function TMuddyAgent.GetKind: TTemporalNSKind;
  begin
  result := FBrain.kind;
  end;

//------------------------------------------------------------------------------

function TMuddyAgent.GetMuddy: boolean;
  begin
  result := FIsMuddy;
  end;

//------------------------------------------------------------------------------

function TMuddyAgent.GetName: string;
  begin
  result := FName;
  end;

//------------------------------------------------------------------------------

function TMuddyAgent.GetWatcher: boolean;
  begin
  result := FWatcher;
  end;

//------------------------------------------------------------------------------

function TMuddyAgent.GetX: integer;
  begin
  result := FX;
  end;

//------------------------------------------------------------------------------

function TMuddyAgent.GetY: integer;
  begin
  result := FY;
  end;

//------------------------------------------------------------------------------

procedure TMuddyAgent.LoadBrainFromFile(filename: string);
  var XNet : TNetworkRep;
  begin
  XNet := TNetworkRep.Create(nil, nil);
  XNet.LoadFromXML(filename);
  FBrain.LoadNetwork(XNet);
  XNet.Free;
  end;

//------------------------------------------------------------------------------

procedure TMuddyAgent.newGame;
  begin
  FBrain.NewGame;
  end;

//------------------------------------------------------------------------------

procedure TMuddyAgent.Sense(data: array of boolean;  timePoint: integer);
  begin
  FBrain.Sense(data, timepoint);
  end;

//------------------------------------------------------------------------------

procedure TMuddyAgent.SetKind(const Value: TTemporalNSKind);
  begin
  Fbrain.Kind := value;
  end;

//------------------------------------------------------------------------------

procedure TMuddyAgent.SetMuddy(value: boolean);
  begin
  FIsMuddy := value;
  UpdatePicture;
  end;

//------------------------------------------------------------------------------

procedure TMuddyAgent.SetName(const Value: string);
  begin
  Fname := value;
  end;

//------------------------------------------------------------------------------

procedure TMuddyAgent.SetWatcher(const Value: boolean);
  begin
  FWatcher := value;
  UpdatePicture;
  end;

//------------------------------------------------------------------------------

procedure TMuddyAgent.SetX(const Value: integer);
  begin
  FX := value;
  FImage.Left := (FX + 250) - (FImage.Width div 2);
  end;

//------------------------------------------------------------------------------

procedure TMuddyAgent.SetY(const Value: integer);
  begin
  FY := value;
  FImage.Top := (250 - FY) - (FImage.Height div 2);
  end;

//------------------------------------------------------------------------------

function TMuddyAgent.Supervise(timepoint: integer; expectedAction: boolean): boolean;
  begin
  result := FBrain.Supervise(timepoint, expectedAction);
  end;

//------------------------------------------------------------------------------

procedure TMuddyAgent.UpdatePicture(know : boolean = false);
  var s : string;
  begin
  s := ExtractFileDir(Application.ExeName) + '\..\modules\muddychildren\';
  if FWatcher then
     if know then
        FImage.Picture.LoadFromFile(s + 'eyes2.bmp')
     else
        FImage.Picture.LoadFromFile(s + 'eyes.bmp')
  else
     if know then
        if FIsMuddy then
            FImage.Picture.LoadFromFile(s + 'smiley_04.bmp')
        else
            FImage.Picture.LoadFromFile(s + 'smiley_03.bmp')
     else
        if FIsMuddy then
            FImage.Picture.LoadFromFile(s + 'smiley_02.bmp')
        else
            FImage.Picture.LoadFromFile(s + 'smiley_01.bmp')
  end;

//------------------------------------------------------------------------------

{ TMuddyEnvironment }

//------------------------------------------------------------------------------

procedure TMuddyEnvironment.ClearAgents;
  var i : integer;
  begin
  for i := FAgents.Count - 1 downto 0 do
    FAgents.Objects[i].Free;
  FAgents.Clear;
  for i := FWatchers.Count - 1 downto 0 do
    FWatchers.Objects[i].Free;
  FWatchers.Clear;
  SetLength(FProb, 0);
  SetLength(FWatching, 0);
  end;

//------------------------------------------------------------------------------

constructor TMuddyEnvironment.Create;
  begin
  inherited Create;
  FAgents := TStringList.Create;
  FWatchers := TStringList.Create;
  FTimePoint := -1;
  FOnlyLogic := true;
  FAlwaysQ1 := false;
  FStopCrit := 0;
  FStrictMax := -1;
  FInitKind := 0;
  end;

//------------------------------------------------------------------------------

destructor TMuddyEnvironment.Destroy;
  var cnt1 : integer;
  begin
  for cnt1 := 0 to FWatchers.count - 1 do
    FWatchers.Objects[cnt1].Free;
  for cnt1 := 0 to FAgents.count - 1 do
    FAgents.Objects[cnt1].Free;
  FAgents.Free;
  FWatchers.Free;
  inherited;
  end;

//------------------------------------------------------------------------------

function TMuddyEnvironment.GetAlwaysQ1 : boolean;
  begin
  result := FAlwaysQ1;
  end;

//------------------------------------------------------------------------------

function TMuddyEnvironment.GetInitKind : integer;
  begin
  result := FInitKind;
  end;

//------------------------------------------------------------------------------

function TMuddyEnvironment.GetOnlyLogic : boolean;
  begin
  result := FOnlyLogic;
  end;

//------------------------------------------------------------------------------

function TMuddyEnvironment.GetStopCrit : integer;
  begin
  result := FStopCrit;
  end;

//------------------------------------------------------------------------------

function TMuddyEnvironment.GetStrictMax: integer;
  begin
  result := FStrictMax;
  end;

//------------------------------------------------------------------------------


procedure TMuddyEnvironment.InitialiseAgents;
  var
    init : array of boolean;
    i, j, nMuddy : integer;
    rrr : TRafaRandomiser;
  begin
  SetLength(init, FAgents.Count);
  rrr := TRafaRandomiser.Create;
  case FInitKind of
    0:
      for i := 0 to FAgents.Count - 1 do
         init[i] := random < FProb[i];
    1:
      for i := 0 to FAgents.Count - 1 do
         init[i] := random < FProb[0];
    2: //first agent different
      begin
      nMuddy := random(FAgents.Count);
      rrr.RandomBoolArray(init, 0, FAgents.Count, nMuddy + 1);
      end;
    3:
      begin
      init[0] := random < FProb[0];
      nMuddy := random(FAgents.Count - 1);
        rrr.RandomBoolArray(init, 1, FAgents.Count - 1, nMuddy);
      end;
    end;

  for i := 0 to FAgents.Count - 1 do
     begin
     (FAgents.Objects[i] as TMuddyAgent).IsMuddy := init[i];
     (FAgents.Objects[i] as TMuddyAgent).NewGame;
     end;
  for i := 0 to FWatchers.Count - 1 do
     (FWatchers.Objects[i] as TMuddyAgent).NewGame;
  if (FAgents.Count > FStrictMax) then
     SetLength(FGameHist, FAgents.Count, FAgents.Count + FWatchers.Count)
  else
     SetLength(FGameHist, FStrictMax, FAgents.Count + FWatchers.Count);
  ///might be unnecessary:
  for i := 0 to FAgents.Count - 1 do
    for j := 0 to FAgents.Count + FWatchers.Count - 1 do
      begin
      FGameHist[i, j].expected := 0;
      FGameHist[i, j].Obtained := 0;
      end;

  rrr.free;
  end;


//------------------------------------------------------------------------------

function TMuddyEnvironment.LoadDescription(parent : TControl; desc, NetList: TStrings): boolean;
  var
    i, j,  XwatchCount, Xagentcount, tmpW : integer;
    XAgent : TMuddyAgent;
  begin
  ClearAgents;
  XAgentCount := 0;
  XwatchCount := 0;
  for i := 0 to desc.Count - 1 do
    if (desc.Objects[i] as TMuddyConfig).IsWatcher then
       XWatchCount := XwatchCount + 1
    else
       Xagentcount := Xagentcount + 1;
    SetLength(FProb, XAgentCount);

  j := 0;
  for i := 0 to desc.Count - 1 do
    if not (desc.Objects[i] as TMuddyConfig).IsWatcher then
       begin
       XAgent := TMuddyAgent.Create(parent, XAgentCount);
       XAgent.LoadBrainFromFile(NetList.Strings[(desc.Objects[i] as TMuddyConfig).Brain]);
       FProb[j] := (desc.Objects[i] as TMuddyConfig).MuddyProb / 100;
       XAgent.Name := (desc.Objects[i] as TMuddyConfig).Name;
       XAgent.IsWatcher := false;
       FAgents.AddObject(XAgent.Name, XAgent);
       j := j + 1;
       end;

  for i := 0 to desc.Count - 1 do
    if (desc.Objects[i] as TMuddyConfig).IsWatcher then
       begin
       XAgent := TMuddyAgent.Create(parent, XAgentCount);
       XAgent.LoadBrainFromFile(NetList.Strings[(desc.Objects[i] as TMuddyConfig).Brain]);
       XAgent.Name := (desc.Objects[i] as TMuddyConfig).Name;
       XAgent.IsWatcher := true;
       FWatchers.AddObject(XAgent.Name, XAgent);
       SetLength(FWatching, FWatchers.Count);
       tmpW := (desc.Objects[i] as TMuddyConfig).Watched;
       if (tmpw >= 0) then
          FWatching[FWatchers.Count - 1] := FAgents.IndexOf(desc.Strings[tmpW])
       else
          FWatching[FWatchers.Count - 1] := -1;
       end;


  for i := 0 to XAgentCount - 1 do
    begin
    (FAgents.Objects[i] as TMuddyAgent).Y := round(200 * (cos((i * 2 * pi) / XAgentCount)));
    (FAgents.Objects[i] as TMuddyAgent).X := round(200 * (sin((i * 2 * pi) / XAgentCount)));
    end;

  for i := 0 to XwatchCount - 1 do
    begin
    if (FWatching[i] >= 0) and (FWatching[i] < Xagentcount) then
       begin
       (FWatchers.Objects[i] as TMuddyAgent).Y := (FAgents.Objects[FWatching[i]] as TMuddyAgent).y + 30;
       (FAgents.Objects[FWatching[i]] as TMuddyAgent).y :=
                                                  (FAgents.Objects[FWatching[i]] as TMuddyAgent).y - 10;
       (FWatchers.Objects[i] as TMuddyAgent).X := (FAgents.Objects[FWatching[i]] as TMuddyAgent).x;
       end;
    end;

  result := true;
  end;

//------------------------------------------------------------------------------

function TMuddyEnvironment.Run(FileName : TFileName; gauge : TGauge; GameCount : integer) : integer;
  var
    i, k: integer;
    XOut: TStringList;
    XFirst : boolean;
    s : string;
    XMaxtimepoint : integer;
    XActualTimePoint : integer;
  begin
  i := 0;
  XOut := TStringList.Create;
  XMaxtimepoint := 0;
//  stop := false;
  while (i <= GameCount) do //and not stop
    begin
    XActualTimePoint := FTimePoint;
    Step;
    if XActualTimePoint > XMaxTimePoint then
       XMaxTimePoint := XActualtimepoint;
    if XActualtimepoint < 0 then
       begin
       XOut.Add('');
       i := i + 1
       end
    else if XActualtimepoint >= 0 then
       begin
       s := '';
       XFirst := true;
       for k := 0 to FAgents.Count + FWatchers.Count - 1 do
         begin
         if XFirst then
            XFirst := false
         else
            s := s + ' ';
         s := s + inttostr(FGameHist[XActualtimepoint, k].Expected);
         s := s + ' ' + floattostr(FGameHist[XActualtimepoint, k].Obtained);
         end;
       XOut.Add(s);
       end;
    Application.ProcessMessages;
    Gauge.Progress := round((i * 100) / GameCount);
    end;
  s := '*??* ' + inttostr((XMaxtimepoint + 1) * ((FAgents.Count + FWatchers.count) * 2)) + ' 1 1';
  XOut.Insert(0, s);
  s := '';
  XOut.Insert(1, s);
  s := '*!!* ' + inttostr(GameCount);
  XOut.Insert(2, s);
  s := '';
  XOut.Insert(3, s);
  XOut.SaveToFile(FileName);
  result := GameCount;
  end;

//------------------------------------------------------------------------------

procedure TMuddyEnvironment.SetAlwaysQ1(value : boolean);
  begin
  FAlwaysQ1 := value;
  end;

//------------------------------------------------------------------------------

procedure TMuddyEnvironment.SetOnlyLogic(value : boolean);
  begin
  FOnlyLogic := value;
  end;

//------------------------------------------------------------------------------

procedure TMuddyEnvironment.SetInitKind(value : integer);
  begin
  FInitKind := value;
  end;

//------------------------------------------------------------------------------

procedure TMuddyEnvironment.SetStopCrit(value : integer);
  begin
  FStopCrit := value;
  end;

//------------------------------------------------------------------------------

procedure TMuddyEnvironment.SetStrictMax(const Value: integer);
  begin
  FStrictMax := Value;
  end;

//------------------------------------------------------------------------------

function TMuddyEnvironment.Step : integer;
  var
    i : integer;
    b1, b2, b3, b : boolean;
  begin
  if FTimePoint = -1 then
     begin
     InitialiseAgents;
     FTimePoint := 0;
     end
  else
     begin
     Turn;
     b1 := FTimePoint >= (FAgents.Count - 1);
     b2 := false;
     for i := 0 to FAgents.Count - 1 do
       b2 := b2 or ((FGameHist[FTimePoint, i].obtained > 0) and (FAgents.Objects[i] as TMuddyAgent).IsMuddy);
     if FStrictMax > 0 then
        b3 := FTimePoint >= (FStrictMax - 1)
     else
        b3 := false;
     case FStopCrit of
       0: b := b1 or b3;
       1: b := b2 or b3;
       2: b := (b1 or b2) or b3;
       3: b := (b1 and b2) or b3;
       else b := true;
       end;
     if b then
        FTimePoint := -1
     else
        FTimePoint := FTimePoint + 1;
     end;
  result := FTimePoint;
  end;

//------------------------------------------------------------------------------

function TMuddyEnvironment.TmpExecute(strList : TStrings; gauge : TGauge) : boolean;
  var
    XGameCount : integer;
    XReport : TFileName;
  begin
  if (strList.Count >= 3) and (trim(strList.Strings[0]) = '-') then
     begin
     try
       XGameCount := StrToInt(trim(strList.Strings[1]));
       XReport := trim(strList.Strings[2]);
       Run(XReport,gauge, XGameCount);
       result := true;
     except
       result := false
       end;
     end
  else
     result := false;
  end;

//------------------------------------------------------------------------------

function TMuddyEnvironment.TmpIsKeyword(s: String): boolean;
  begin
  result :=  (UpperCase(s) = '*MUDDY*') or
             (UpperCase(s) = '**NETWORKS**') or
             (UpperCase(s) = '**GAMES**') or
             (UpperCase(s) = '***AGENTS***') or
             (UpperCase(s) = '***OPTIONS***') or
             (UpperCase(s) = '***EXECUTIONS***');
  end;

//------------------------------------------------------------------------------

function TMuddyEnvironment.TmpLoadAgent(strList : TStrings) : TMuddyConfig;
  var
    XTmpDesc : TMuddyConfig;
  begin
  if (strList.Count >= 5) and (trim(strList.Strings[0]) = '-') then
     begin
     XTmpDesc := TMuddyConfig.Create;
     XTmpDesc.Name := trim(strList.Strings[1]);
     try
       XTmpDesc.Brain := strToInt(trim(strList.Strings[2]));
       if UpperCase(trim(strList.Strings[3])) = 'PLAY' then
          begin
          XTmpDesc.IsWatcher := false;
          XTmpDesc.MuddyProb := StrToInt(trim(strList.Strings[4]));
          end
       else if UpperCase(trim(strList.Strings[3])) = 'WATCH' then
          begin
          XTmpDesc.IsWatcher := true;
          XTmpDesc.Watched := StrToInt(trim(strList.Strings[4]));
          end
       else
          begin
          XTmpDesc.Free;
          XTmpDesc := nil;
          end;
     except
       XTmpDesc.Free;
       XTmpDesc := nil;
       end;
     end
  else
     XTmpDesc := nil;
  result := XTmpDesc;
  end;

//------------------------------------------------------------------------------

function TMuddyEnvironment.TmpLoadFromFile(FileName: TFileName; parent : TControl; gauge : TGauge): integer;
  var
    i_file, i_line, XCount : integer;
    XInput, XTmpLine : TStringList;
    XState : integer;
    XNetworks, XAgents : TStringList;
    XTmpDesc : TMuddyConfig;

  begin
  //------- Initialisation
  XInput := TStringList.Create;
  XTmpLine := TStringList.Create;
  XState := 0;
  try
    XInput.LoadFromFile(FileName)
  except
    XState := -1;
    end;
  XCount := 0;
  i_file := 0;
  XNetworks := TStringList.Create;
  XAgents := TStringList.Create;

  //------- Main Loop
  while (i_file < XInput.Count) and (XState >= 0) do
    begin
    XTmpLine.DelimitedText := XInput.Strings[i_file];
    case XState of
      //------- State 0 -> *MUDDY* -> State 1
      0:
        begin
        if (XTmpLine.Count >= 1) and (trim(XTmpLine.Strings[0]) = '*MUDDY*') then
           XState := 1;
        end;
      //------- State 1 -> **NETWORKS* -> State 2
      //------- State 1 -> *else -> State -1
      1:
        begin
        if (XTmpLine.Count >= 1) and (trim(XTmpLine.Strings[0]) = '**NETWORKS**') then
           XState := 2
        else if (XTmpLine.Count >= 1) and (TmpIsKeyWord(trim(XTmpLine.Strings[0]))) then
           XState := -1;
        end;
      //------- State 2 -> - ??? -> State 2
      //------- State 2 -> **GAMES** -> State 3
      //------- State 2 -> *else -> State -1
      2:
        begin
        if (XTmpLine.Count >= 2) and (trim(XTmpLine.Strings[0]) = '-') then
           begin
           if FileExists(trim(XTmpLine.Strings[1])) then
              XNetworks.Add(trim(XTmpLine.Strings[1]));
           end
        else if (XTmpLine.Count >= 1) and (trim(XTmpLine.Strings[0]) = '**GAMES**') then
           XState := 3
        else if (XTmpLine.Count >= 1) and (TmpIsKeyWord(trim(XTmpLine.Strings[0]))) then
           XState := -1;
        end;
      //------- State 3 -> ***OPTIONS*** -> State 3
      //------- State 3 -> ***AGENTS*** -> State 4
      //------- State 3 -> ***EXECUTIONS*** -> State 5
      //------- State 3 -> *else -> State -1
      3:
        begin
        if TmpLoadOptions(XTmpLine) then
           XState := 3
        else if (XTmpLine.Count >= 1) and (trim(XTmpLine.Strings[0]) = '***AGENTS***') then
           begin
           for i_line := XAgents.Count - 1 downto 0 do
             XAgents.Objects[i_line].Free;
           XAgents.Clear;
           XState := 4
           end
        else if (XTmpLine.Count >= 1) and (trim(XTmpLine.Strings[0]) = '***EXECUTIONS***') then
           begin
           LoadDescription(parent, XAgents, XNetworks);
           XState := 5;
           end
        else if (XTmpLine.Count >= 1) and (TmpIsKeyWord(trim(XTmpLine.Strings[0]))) then
           XState := -1;
        end;
      //------- State 4 -> - ??? ??? ??? ??? -> State 4
      //------- State 4 -> ***OPTIONS*** -> State 3
      //------- State 4 -> ***EXECUTIONS*** -> State 5
      //------- State 4 -> *else -> State -1
      4:
        begin
        XTmpDesc := TmpLoadAgent(XTmpLine);
        if XTmpDesc <> nil then
           begin
           XAgents.AddObject(XTmpDesc.Name, XTmpDesc);
           end
        else if TmpLoadOptions(XTmpLine) then
           XState := 3
        else if (XTmpLine.Count >= 1) and (trim(XTmpLine.Strings[0]) = '***EXECUTIONS***') then
           begin
           LoadDescription(parent, XAgents, XNetworks);
           XState := 5;
           end
        else if (XTmpLine.Count >= 1) and (TmpIsKeyWord(trim(XTmpLine.Strings[0]))) then
           XState := -1;
        end;
      //------- State 5 -> - ??? ??? -> State 5
      //------- State 5 -> ***AGENTS*** -> State 4
      //------- State 5 -> ***OPTIONS*** -> State 3
      //------- State 5 -> ***EXECUTIONS*** -> State 5
      //------- State 5 -> *else -> State -1
      5:
        begin
        if TmpExecute(XTmpLine, gauge) then
           XCount := XCount + 1
        else if TmpLoadOptions(XTmpLine) then
           XState := 3
        else if (XTmpLine.Count >= 1) and (trim(XTmpLine.Strings[0]) = '***AGENTS***') then
           begin
           for i_line := XAgents.Count - 1 downto 0 do
             XAgents.Objects[i_line].Free;
           XAgents.Clear;
           XState := 4;
           end
        else if (XTmpLine.Count >= 1) and (trim(XTmpLine.Strings[0]) = '***EXECUTIONS***') then
           LoadDescription(parent, XAgents, XNetworks)
        else if (XTmpLine.Count >= 1) and (TmpIsKeyWord(trim(XTmpLine.Strings[0]))) then
           XState := -1;
        end;
      end;
    i_file := i_file + 1;
    end;

  //------- Finalisation
  XTmpLine.Free;
  XInput.Free;
  result := XCount;
  for i_line := XAgents.Count - 1 downto 0 do
      XAgents.Objects[i_line].Free;
  XAgents.free;
  XNetworks.Free;
  end;

//------------------------------------------------------------------------------

function TMuddyEnvironment.TmpLoadOptions(strList : TStrings) : boolean;
  var
    i_line : integer;
  begin
  if (strList.Count >= 2) and (trim(strList.Strings[0]) = '***OPTIONS***') then
     begin
     for i_line := 1 to strList.Count - 1 do
       begin
       if UpperCase(trim(strList.Strings[i_line])) = 'LOG_SUP' then
          FOnlyLogic := true
       else if UpperCase(trim(strList.Strings[i_line])) = 'NAIVE_SUP' then
          FOnlyLogic := false
       else if UpperCase(trim(strList.Strings[i_line])) = 'ONLY_Q1' then
          FAlwaysQ1 := false
       else if UpperCase(trim(strList.Strings[i_line])) = 'ALL_TIME' then
          FAlwaysQ1 := true
       else if UpperCase(trim(strList.Strings[i_line])) = 'F1' then
          FInitKind := 0
       else if UpperCase(trim(strList.Strings[i_line])) = 'F2' then
          FInitKind := 1
       else if UpperCase(trim(strList.Strings[i_line])) = 'F3' then
          FInitKind := 2
       else if UpperCase(trim(strList.Strings[i_line])) = 'F4' then
          FInitKind := 3
       else if UpperCase(trim(strList.Strings[i_line])) = 'I1' then
          FStopCrit := 0
       else if UpperCase(trim(strList.Strings[i_line])) = 'I2' then
          FStopCrit := 1
       else if UpperCase(trim(strList.Strings[i_line])) = 'I3' then
          FStopCrit := 2
       else if UpperCase(trim(strList.Strings[i_line])) = 'I4' then
          FStopCrit := 3
       end;
     result := true;
     end
  else
     result := false;
  end;

//-------------------------------------------------------------------------------

procedure TMuddyEnvironment.Turn;
  var
    i, j, k : integer;
    Xarr : array of boolean;
    b : boolean;
  begin
  SetLength(Xarr, (FAgents.Count * 2) - 1);
  XArr[0] := false;
  for i := 0 to FAgents.Count - 1 do
    XArr[0] := XArr[0] or (FAgents.Objects[i] as TMuddyAgent).IsMuddy;
  if FAlwaysQ1 then
     Xarr[0] := XArr[0] and (FTimePoint >= 0)
  else
     Xarr[0] := XArr[0] and (FTimePoint = 0);

  for i := 0 to FAgents.Count - 1 do
    begin
    for j := 0 to FAgents.Count - 2 do
      begin
      k := (i + j + 1) mod FAgents.Count;
      Xarr[j + 1] := not (FAgents.Objects[k] as TMuddyAgent).IsMuddy;
      if FTimePoint = 0 then
         Xarr[j + FAgents.Count] := false
      else
         Xarr[j + FAgents.Count] := FGameHist[FTimePoint - 1, k].Obtained > 0;
      end;
    (FAgents.Objects[i] as TMuddyAgent).Sense(Xarr, FTimePoint);
    end;

  for i := 0 to FWatchers.Count - 1 do
    begin
    for j := 0 to FAgents.Count - 2 do
      begin
      k := (FWatching[i] + j + 1) mod FAgents.Count;
      Xarr[j + 1] := not (FAgents.Objects[k] as TMuddyAgent).IsMuddy;
      if FTimePoint = 0 then
         Xarr[j + FAgents.Count + 1] := false
      else
         Xarr[j + FAgents.Count + 1] := FGameHist[FTimePoint - 1, k].Obtained > 0;
      end;
    (FWatchers.Objects[i] as TMuddyAgent).Sense(Xarr, FTimePoint);
    end;

  for i := 0 to FAgents.Count - 1 do
    begin
    FGameHist[FTimePoint, i].Obtained :=  (FAgents.Objects[i] as TMuddyAgent).Act(FTimePoint);
    //updatescreen
    end;

  for i := 0 to FWatchers.Count - 1 do
    begin
    FGameHist[FTimePoint, FAgents.Count + i].Obtained :=  (FWatchers.Objects[i] as TMuddyAgent).Act(FTimePoint);
    //updatescreen
    end;

  for i := 0 to FAgents.Count - 1 do
    begin
    b := true;
    if FOnlyLogic then
       begin
       k := 0;
       for j := 0 to FAgents.Count - 1 do
         if (FAgents.Objects[j] as TMuddyAgent).IsMuddy then
            k := k + 1;
       b := (FTimePoint >= (k - 1));
       end;

    with FAgents.Objects[i] as TMuddyAgent do
      begin
      if b and fismuddy then
         FGameHist[FTimePoint, i].expected := 1
      else
         FGameHist[FTimePoint, i].expected := -1;
      Supervise(FTimePoint, b and IsMuddy);
      end;
    end;

  for i := 0 to FWatchers.Count - 1 do
    begin
    b := true;
    if FOnlyLogic then
       begin
       k := 0;
       for j := 0 to FAgents.Count - 1 do
         if (FAgents.Objects[j] as TMuddyAgent).IsMuddy then
            k := k + 1;
       b := (FTimePoint >= (k - 1));
       end;
    b := b and (FAgents.Objects[FWatching[i]] as TMuddyAgent).IsMuddy;
    if b then
       FGameHist[FTimePoint, i + FAgents.count].expected := 1
    else
       FGameHist[FTimePoint, i + FAgents.count].expected := -1;
    (FWatchers.Objects[i] as TMuddyAgent).Supervise(FTimePoint, b);
    end;
  end;

//------------------------------------------------------------------------------

{ TMuddyConfig }

//------------------------------------------------------------------------------

constructor TMuddyConfig.Create;
  begin
  inherited Create;
  FName := '';
  FBrain := -1;
  FWatch := false;
  FMuddyProb := 50;
  FWatched := -1;
  end;

//------------------------------------------------------------------------------

destructor TMuddyConfig.Destroy;
  begin
  inherited Destroy ;
  end;

//------------------------------------------------------------------------------

function TMuddyConfig.GetName : string;
  begin
  result := FName;
  end;

//------------------------------------------------------------------------------

function TMuddyConfig.GetBrain : integer;
  begin
  result := FBrain;
  end;

//------------------------------------------------------------------------------

function TMuddyConfig.GetWatch : boolean;
  begin
  result := FWatch;
  end;

//------------------------------------------------------------------------------

function TMuddyConfig.GetMuddyProb : integer;
  begin
  result := FMuddyProb
  end;

//------------------------------------------------------------------------------

function TMuddyConfig.GetWatched : integer;
  begin
  result := FWatched;
  end;

//------------------------------------------------------------------------------

procedure TMuddyConfig.SetName (value : string);
  begin
  FName := value;
  end;

//------------------------------------------------------------------------------

procedure TMuddyConfig.SetBrain (value : integer);
  begin
  FBrain := value;
  end;

//------------------------------------------------------------------------------

procedure TMuddyConfig.SetWatch (value : boolean);
  begin
  FWatch := value;
  end;

//------------------------------------------------------------------------------

procedure TMuddyConfig.SetMuddyProb (value : integer);
  begin
  FMuddyProb  := value;
  end;

//------------------------------------------------------------------------------

procedure TMuddyConfig.SetWatched (value : integer);
  begin
  FWatched  := value;
  end;

//------------------------------------------------------------------------------

{ TFmMuddyChildren }

//------------------------------------------------------------------------------

procedure TFmMuddyChildren.Button1Click(Sender: TObject);
  begin
  if OpenDialog1.Execute then
     Edit1.Text := OpenDialog1.FileName;
  end;

//------------------------------------------------------------------------------

procedure TFmMuddyChildren.Button2Click(Sender: TObject);
  var i : integer;
  begin
  ListBox1.DeleteSelected;
  ComboBox1.Items.Clear;
  for i := 0 to ListBox1.Count - 1 do
    ComboBox1.Items.Add(ListBox1.Items.Strings[i]);
  end;

//------------------------------------------------------------------------------

procedure TFmMuddyChildren.Button3Click(Sender: TObject);
  var i : integer;
  begin
  ListBox1.Items.Strings[ListBox1.ItemIndex] := edit1.Text;
  ComboBox1.Items.Clear;
  for i := 0 to ListBox1.Count - 1 do
    ComboBox1.Items.Add(ListBox1.Items.Strings[i]);
  end;

//------------------------------------------------------------------------------

 procedure TFmMuddyChildren.Button4Click(Sender: TObject);
  var i : integer;
  begin
  ListBox1.AddItem(Edit1.Text, nil);
  ComboBox1.Items.Clear;
  for i := 0 to ListBox1.Count - 1 do
    ComboBox1.Items.Add(ListBox1.Items.Strings[i]);
  end;

//------------------------------------------------------------------------------

procedure TFmMuddyChildren.Button5Click(Sender: TObject);
  begin
  if Button5.Caption = 'Create' then
     begin
     FEnv := TMuddyEnvironment.Create;
     FEnv.OnlyLogic := CheckBox2.Checked;
     FEnv.AlwaysQ1  := CheckBox3.Checked;
     Fenv.StopCrit  := ComboBox3.ItemIndex;
     FEnv.InitKind  := ComboBox4.ItemIndex;
     FEnv.LoadDescription(GroupBox3, ListBox2.Items, ListBox1.items);
     FEnv.StrictMax := SpinEdit3.Value;
     Button5.Caption := 'Finish';
     end
  else
     begin
     FEnv.Free;
     Button5.Caption := 'Create';
     end;
{  wcount := 0;
  for i := 0 to ListBox2.Count - 1 do
    with ListBox2.Items.Objects[i] as TMuddyConfig do
      if IsWatcher then
         begin

         end;
    }
  end;

//------------------------------------------------------------------------------

procedure TFmMuddyChildren.Button6Click(Sender: TObject);
  var i : integer;
  begin
  for i := 0 to Listbox2.Count - 1 do
    with ListBox2.Items.Objects[i] as TMuddyConfig do
      if IsWatcher and (Watched = ListBox2.ItemIndex) then
         begin
         IsWatcher := false;
         Watched := -1;
         end
      else if IsWatcher and (Watched > ListBox2.ItemIndex) then
         Watched := Watched - 1;
  ListBox2.DeleteSelected;
  ComboBox2.Clear;
  for i := 0 to ListBox2.Count - 1 do
    ComboBox2.AddItem(Listbox2.Items.Strings[i], nil);
  end;

//------------------------------------------------------------------------------

procedure TFmMuddyChildren.Button7Click(Sender: TObject);
  var i : integer;
  begin
  with ListBox2.Items.Objects[ListBox2.ItemIndex] as TMuddyConfig do
    begin
    Name := Edit2.Text;
    Brain := ComboBox1.ItemIndex;
    if CheckBox1.Checked  then
      begin
      IsWatcher := true;
      Watched := ComboBox2.ItemIndex;
      end
    else
      begin
      IsWatcher := false;
      MuddyProb := SpinEdit1.Value;
      end;
    end;
  ListBox2.Items.Strings[ListBox2.ItemIndex] := Edit2.text;
  ComboBox2.Clear;
  for i := 0 to ListBox2.Count - 1 do
    ComboBox2.AddItem(Listbox2.Items.Strings[i], nil);
  end;

//------------------------------------------------------------------------------

procedure TFmMuddyChildren.Button8Click(Sender: TObject);
  var
    tmp : TMuddyConfig;
    i : integer;
  begin
  tmp := TMuddyConfig.Create;
  with tmp do
    begin
    Name := Edit2.Text;
    Brain := ComboBox1.ItemIndex;
    if CheckBox1.Checked  then
      begin
      IsWatcher := true;
      Watched := ComboBox2.ItemIndex;
      end
    else
      begin
      IsWatcher := false;
      MuddyProb := SpinEdit1.Value;
      end;
    end;
  ListBox2.AddItem(tmp.Name, tmp);
  ComboBox2.Clear;
  for i := 0 to ListBox2.Count - 1 do
    ComboBox2.AddItem(Listbox2.Items.Strings[i], nil);
  end;

//------------------------------------------------------------------------------

procedure TFmMuddyChildren.Button9Click(Sender: TObject);
  begin
  Button9.caption:= 'Single Step (' + inttostr(FEnv.Step) + ')';
  end;

//------------------------------------------------------------------------------

procedure TFmMuddyChildren.CheckBox1Click(Sender: TObject);
  begin
  if CheckBox1.Checked then
     Label3.Caption := 'Agent to watch:'
  else
     Label3.Caption := 'Muddy Probability (%):';
  ComboBox2.Visible := CheckBox1.Checked;
  SpinEdit1.Visible := not CheckBox1.Checked;
  end;

//------------------------------------------------------------------------------

procedure TFmMuddyChildren.ListBox1Click(Sender: TObject);
  begin
  Edit1.Text := ListBox1.Items.Strings[ListBox1.ItemIndex];
  end;

//------------------------------------------------------------------------------

procedure TFmMuddyChildren.ListBox2Click(Sender: TObject);
  begin
  with ListBox2.Items.Objects[ListBox2.ItemIndex] as TMuddyConfig do
    begin
    Edit2.Text := Name;
    ComboBox1.ItemIndex := Brain;
    CheckBox1.Checked := IsWatcher;
    ComboBox2.ItemIndex := Watched;
    SpinEdit1.Value := MuddyProb;
    end;
  end;

//------------------------------------------------------------------------------

procedure TFmMuddyChildren.Button10Click(Sender: TObject);
  begin
  if SaveDialog1.execute then
     FEnv.Run(SaveDialog1.FileName, Gauge1, SpinEdit2.value);

  end;


////////////////

procedure TFmMuddyChildren.Button11Click(Sender: TObject);
  begin
  if OpenDialog1.Execute then
     begin
     if FEnv = nil then
        FEnv := TMuddyEnvironment.Create;
     FEnv.TmpLoadFromFile(OpenDialog1.FileName, GroupBox3, Gauge1);
     end;
  end;

end.

