{
  @abstract(Unit describing the structure of 'neural Moore Machine')
  @author(Rafa "Passarel" Borges <passarel@gmail.com)
  @created(London, March, 2009)
  @lastmod(S�o Louren�o do Sul, April, 2009)
}

unit UnMMStructure;

//-------------------------------------------------------------------------------------------------

interface

//-------------------------------------------------------------------------------------------------

uses Classes, SysUtils, SimpleGraph, UnNetBehaviour, UnRafaAux2007, JvComCtrls, UnLogSing;

//-------------------------------------------------------------------------------------------------

type

TDefaultCorrection = (dcCurrent, dcTrue, dcFalse, dcReinforce, dcNone);

TExecutionInfo = procedure(CurrentEpoch : integer; rmse : double) of object;

TRafaMooreMachine = class;

{
@abstract(Class with the attributes of the transition between states in a Moore Machine)
}

TPropSequence = class(TRafaComparableObject)
  private
    FStateSize : integer;
    FInputSize : integer;
    FNumberOfInputs : integer;
    FInitialState : array of integer;
    FFinalState : array of integer;
    FInputSequence : array of array of integer;
    FWeight : integer;

    function  GetInitialVar(index : integer) : integer;
    procedure SetInitialVar(index, value : integer);
    function  GetFinalVar(index : integer) : integer;
    procedure SetFinalVar(index, value : integer);
    function  GetInput(i, j : integer) : integer;
    procedure SetInput(i, j, value : integer);
    function  GetWeight : integer;
    procedure SetWeight(value : integer);
    function  GetNumberOfInputs : integer;


  public
    constructor Create(InputSize, StateSize : integer);

    //Method that compares two different transitions - inherited from TRafaComparableObject
    function Compare(other : TRafaComparableObject) : integer; override;

    function FloatDifference(other : TRafaComparableObject) : double; override;
    function GetInitialArray (out ar :  array of integer) : integer;
    function SetInitialArray(value : array of integer) : integer;
    function GetFinalArray (out ar :  array of integer) : integer;
    function SetFinalArray(value : array of integer) : integer;
    property InitialVar[index : integer] : integer read GetInitialVar write SetInitialVar;
    property FinalVar[index : integer] : integer read GetFinalVar write SetFinalVar;
    property Input[i, j : integer] : integer read GetInput write SetInput;
    property Weight : integer read GetWeight write SetWeight;
    property NumberOfInputs : integer read GetNumberOfInputs;
    procedure AddInput;
    procedure AddInputWithValues(value : array of integer);
    procedure RemoveInput(index : integer);
  end;

//-------------------------------------------------------------------------------------------------

TRafaTransition = class(TRafaComparableObject)
  private
    //Owner of the transition
    FOwner : TRafaMooreMachine;
    //Index of the source of the transition
    FSourceState : integer;
    //Index of the target of the transition
    FTargetState : integer;
    //Index of the input of the transition
    FInput : integer;
    //Weight of the transitions - defines a fuzzy treatment to transitions between states
    FWeight : double;
    //Count of occurrences of the transition during an execution
    FCount : integer;
    //Value that defines the goal of the learning process
    FGoal : integer;
    //Value representing to which transition it is unified
    FUnifiedTo : integer;
    //Link associated in the graph
    FGraphLink : TGraphLink;

    //Accessor of the SourceState property
    function GetSource : integer;
    //Accessor of the TargetState property
    function GetTarget : integer;
    //Accessor of the Input property
    function GetInput : integer;
    //Accessor of the Weight property
    function GetWeight : double;
    //Accessor of the Count property
    function GetCount : integer;
    //Accessor of the Goal property
    function GetGoal  : integer;
    //Mutator of the SourceState property
    procedure SetSource (value: integer);
    //Mutator of the TargetState property
    procedure SetTarget (value: integer);
    //Mutator of the Input property
    procedure SetInput  (value: integer);
    //Mutator of the Weight property
    procedure SetWeight (value: double);
    //Mutator of the Count property
    procedure SetCount  (value: integer);
    //Mutator of the Goal property
    procedure SetGoal   (value : integer);
    //Accessor to the UnifiedTo property
    function GetUnified : integer;
    //Accessor of the GraphLink property
    function GetGraphLink  : TGraphLink;
    //Mutator of the GraphLink property
    procedure SetGraphLink (value: TGraphLink);
    //Accessor of the Owner property
    function GetOwner  : TRafaMooreMachine;
    //Mutator of the Owner property
    procedure SetOwner (value: TRafaMooreMachine);

  public
    constructor Create(Owner : TRafaMooreMachine);
    //Method that compares two different transitions - inherited from TRafaComparableObject
    function Compare(other : TRafaComparableObject) : integer; override;


    function FloatDifference(other : TRafaComparableObject) : double; override;
    //Define to which state the current transition is unified

    function DefineUnified(index : integer = -1) : integer;
    //Index of the source of the transition
    property SourceState : integer read GetSource write SetSource;
    //Index of the target of the transition
    property TargetState : integer read GetTarget write SetTarget;
    //Index of the input of the transition
    property Input : integer read GetInput write SetInput;
    //Weight of the transitions - defines a fuzzy treatment to transitions between states
    property Weight : double read GetWeight write SetWeight;
    //Count of occurrences of the transition during an execution
    property Count : integer read GetCount write SetCount;
    //Value that defines the goal of the learning process
    property Goal : integer read GetGoal write SetGoal;
    //Value representing to which label it is unified
    property UnifiedTo : integer read GetUnified;
    //Return a string describing the transition
    function ToString : string;
    //Link associated in the graph
    property GraphLink : TGraphLink read GetGraphLink write SetGraphLink;
    //Owner of the transition
    property Owner : TRafaMooreMachine read GetOwner write SetOwner;
  end;

//-------------------------------------------------------------------------------------------------

{
@abstract(Class with the information of a label that defines a group of states)
}

TLabelInformation = class
  private
    //Value representing to which label it is unified
    FUnifiedTo : integer;
    //Position X in the chart (in case of State)
    FX : integer;
    //Position Y in the chart (in case of State)
    FY : integer;
    //Node associated in the Graph
    FGraphNode : TGraphNode;
    //Accessor to the UnifiedTo property
    function GetUnified : integer;
    //Mutator to the UnifiedTo property
    procedure SetUnified(value : integer);
    //Accessor to the X property
    function GetX : integer;
    //Mutator to the X property
    procedure SetX(value : integer);
    //Accessor to the Y property
    function GetY : integer;
    //Mutator to the Y property
    procedure SetY(value : integer);
    //Mutator to the GraphNode property
    procedure SetGraphNode (value: TGraphNode);
    //Accessor to the GraphNode property
    function  GetGraphNode : TGraphNode;


  public
    //Constructor of the class
    constructor Create(unification : integer = -1);
    //Value representing to which label it is unified
    property UnifiedTo : integer read GetUnified write SetUnified;
    //Position Y in the chart (in case of State)
    property X : integer read GetX write SetX;
    //Position Y in the chart (in case of State)
    property Y : integer read GetY write SetY;
    //Node associated in the Graph
    property GraphNode : TGraphNode read GetGraphNode write SetGraphNode;
  end;

//-------------------------------------------------------------------------------------------------

TRafaMooreMachine = class
  private
    //List of the input variables
    FInputVariables : TStringList;
    //List of state variables
    FStateVariables : TStringList;
    //List of input labels
    FInputLabels : TStringList;
    //List of the state labels
    FStateLabels : TStringList;

    //Network used to describe the transition between states
    FNetwork : TRafaAnn;
    //Graph component to visualise the transitions
    FGraph   : TSimpleGraph;

    //Field to implement scalar input variables
    FScalarInputGroup : array of integer;
    //Field to implement scalar state variables (***FILL IN LATER***)
    FScalarStateGroup : array of integer;

    //Field to define if scalar inputs are used
    FUseScalarInputs : boolean;
    //Field to define if scalar states are used
    FUseScalarStates : boolean;
    //Field to define if scalar inputs are used
    FAllowEmptyScalarInputs : boolean;
    //Field to define if scalar states are used
    FAllowEmptyScalarStates : boolean;

    FUseNetworkTransition : boolean;

    FKeepSupervisionHistory : boolean;

    FHistory : array of array of double;
    FSupervisionHistory : array of array of double;

    FHistoryStart : integer;
    FHistoryEnd   : integer;
    FHistorySize  : integer;

    FTransitions : TRafaSortableArray;

//    FtmpGoalList : TRafaSortableArray;

    FCustomInputSelection : double;

//    FInputFromTransition : array of integer;

    FOnGraphUpdate : boolean;

    FExecutionInfo : TExecutionInfo;
    FCanExecute : boolean;
    FExecutionCount : integer;
    FEpochCount : integer;
    FLastEpoch : integer;
    FEpochSize : integer;
    FDataList : TStringList;
//    FOutputSize : integer;
    FUseInputFromDataList : boolean;
    FUseOutputFromDataList : boolean;

    FUpdateRate : integer;
    FAllowLearning : boolean;

    FSequences : TRafaSortableArray;

    FCurrentSequences : array of integer;
    FCurrentSeqPos : array of integer;
    FCurrentSequencesCount : integer;

    //Value to define the output given in a file as true or false
    FFileOutputThresholdMin : double;
    FFileOutputThresholdMax : double;

    FRecurrentLearningRate : double;
    FRecurrentValues : array of double;

    FInitialState : integer;

    FDefaultCorrection : TDefaultCorrection;
    

    function GetSequence(index : integer) : TPropSequence;
    procedure SetSequence(index : integer; value : TPropSequence);


    //ShowSequenceInChart

    function  CompareStateWithArray(state : integer; arValues : array of integer) : boolean;

    function  GetInputUnification(index : integer) : integer;
    function  GetStateUnification(index : integer) : integer;
    procedure SetInputUnification(index, value : integer);
    procedure SetStateUnification(index, value : integer);
    function  GetHistorySize : integer;
    procedure SetHistorySize(value : integer);

    function GetUseNetworkTransition : boolean;
    procedure SetUseNetworkTransition (value : boolean);
    function GetDefaultCorrection : TDefaultCorrection;
    procedure SetDefaultCorrection(value : TDefaultCorrection);
    function GetInitialState : integer;
    procedure SetInitialState(value : integer);

    procedure CalculateWeightsFromHistory;
    procedure UpdateGraphStructure;
    procedure UpdateGraphWeights;



//*****    function GetNearestState(initial, current : integer; out ArState : array of double) : integer;

    procedure CreateLabels(input : boolean; values : array of boolean);

    procedure SetInputLabel(index : integer; value : string);
    procedure SetStateLabel(index : integer; value : string);
    function  GetInputLabel(index : integer) : string;
    function  GetStateLabel(index : integer) : string;

    function  GetTotalInputCount : integer;
    function  GetTotalStateCount : integer;
    function  GetInputCount : integer;
    function  GetStateCount : integer;

    function  GetXPosition(index : integer) : integer;
    procedure SetXPosition(index, value : integer);
    function  GetYPosition(index : integer) : integer;
    procedure SetYPosition(index, value : integer);

    procedure GraphNodeMoveResize(Graph: TSimpleGraph; Node: TGraphNode);

    procedure DefaultExecutionInfo(CurrentEpoch : integer; rmse : double);
    procedure SetExecutionInfo(value : TExecutionInfo);
    function GetExecutionInfo : TExecutionInfo;
    function GetInputFromDataList(out arr : array of double) : integer;
    function GetOutputFromDataList(out arr : array of double) : integer;
//  procedure SetOutputSize(value : integer);
//  function  GetOutputSize : integer;
    procedure SetInputFromData(value : boolean);
    function  GetInputFromData : boolean;
    procedure SetOutputFromData(value : boolean);
    function  GetOutputFromData : boolean;

    function  GetKeepSupHistory : boolean;
    procedure SetKeepSupHistory (value : boolean);

    function  GetUpdateRate : integer;
    procedure SetUpdateRate (value : integer);

    function  GetAllowLearning : boolean;
    procedure SetAllowLearning (value : boolean);

    procedure AddDataToHistory(arInput, arOutput : array of double);
    procedure AddDataToHistoryWithDes(arInput, arOutput, arDesOutput : array of double);

    function FilterScalarInputs (index : integer; var arInputs : array of double) : integer;

    function  GetUseScalarInputs : boolean;
    function  GetUseScalarStates : boolean;
    function  GetAllowEmptyScalarInputs : boolean;
    function  GetAllowEmptyScalarStates : boolean;
    procedure SetUseScalarInputs (value : boolean);
    procedure SetUseScalarStates (value : boolean);
    procedure SetAllowEmptyScalarInputs (value : boolean);
    procedure SetAllowEmptyScalarStates (value : boolean);

  public
    constructor Create(graph : TSimpleGraph);
    procedure   LoadNetwork(FileName : TFileName; extraNeurons : integer = 0);
    procedure   LoadFromVariables(InputVar, StateVar : TStrings; nHid : integer = -1);

    property  InputUnificationInfo[index : integer] : integer read GetInputUnification;
    property  StateUnificationInfo[index : integer] : integer read GetStateUnification;
    property  InputLabel[index : integer] : string read GetInputLabel write SetInputLabel;
    property  StateLabel[index : integer] : string read GetStateLabel write SetStateLabel;
    property  XPosition[index : integer] : integer read GetXPosition write SetXPosition;
    property  YPosition[index : integer] : integer read GetYPosition write SetYPosition;

    property  TotalInputCount : integer read GetTotalInputCount;
    property  TotalStateCount : integer read GetTotalStateCount;
    property  InputCount : integer read GetInputCount;
    property  StateCount : integer read GetStateCount;
    property  InitialState : integer read GetInitialState write SetInitialState;

    property  HistorySize : integer read GetHistorySize write SetHistorySize;
    property  ExampleSequences[index : integer] : TPropSequence read GetSequence write SetSequence;

    procedure UnifyInputs(inp : array of integer; NewLabel : string);
    procedure UnifyStates(sta : array of integer; NewLabel : string);

    function AddTransition(tr : TRafaTransition) : integer;
    function AddNodeToGraph(Tag, left, top : integer) : integer;
    procedure BreakInputGroup(index : integer);
    procedure BreakStateGroup(index : integer);

    property UseNetworkTransition : boolean read GetUseNetworkTransition write SetUseNetworkTransition;
    property DefaultCorrection : TDefaultCorrection read GetDefaultCorrection write SetDefaultCorrection;

    //Property to define if scalar inputs are used
    property UseScalarInputs : boolean read GetUseScalarInputs write SetUseScalarInputs;
    //Property to define if scalar states are used
    property UseScalarStates : boolean read GetUseScalarStates write SetUseScalarStates;
    //Property to define if scalar inputs are used
    property AllowEmptyScalarInputs : boolean read GetAllowEmptyScalarInputs write SetAllowEmptyScalarInputs;
    //Property to define if scalar states are used
    property AllowEmptyScalarStates : boolean read GetAllowEmptyScalarStates write SetAllowEmptyScalarStates;

    procedure TransitionsToText(selection : array of integer; text : TStrings; Simplified : boolean);

    //These methods will be regarding the definition of input values according to the current state;
    //Custom Input Selection: 0 (totally random) - 1(full custom definition)
    procedure SetCustomInputSelection(value : double);

    function DefineInput(state : integer; out ar : array of double) : integer;


    //Returns the transition in position index
    function GetTransition(index : integer): TRafaTransition;

    //Returns the number of input variables
    function GetInputVarCount : integer;

    //Returns the number of state variables
    function GetStateVarCount : integer;

    //Returns the number of transitions
    function GetTransitionCount : integer;

    //Set the Unification of the transitions
    procedure SetTransitionsUnification;

    function GetDesiredOutput(input : integer;
        inArray, obtState : array of double; out desState : array of double) : integer;

    function TransitionsAsLogicProgram : TSingleLogicProgram;
    function ReviewLogicProgram(logProg : TSingleLogicProgram) : Integer;

    procedure ResetEpoch;
    procedure ResetHistory;
    procedure SetLastEpoch(value : integer);
    procedure SetEpochSize(value : integer);
    procedure StopExecution;
    function  GetDataList : TStringList;
    property  OnExecutionInfo : TExecutionInfo read GetExecutionInfo write SetExecutionInfo;
//  property  OutputSize : integer read GetOutputSize write SetOutputSize;
    property  InputFromDataList : boolean read GetInputFromData write SetInputFromData;
    property  OutputFromDataList : boolean read GetOutputFromData write SetOutputFromData;
    property  KeepSupervisionHistort : boolean read GetKeepSupHistory write SetKeepSupHistory;
    property  UpdateRate : integer read GetUpdateRate write SetUpdateRate;
    property  AllowLearning : boolean read GetAllowLearning write SetAllowLearning;

    function GetNetwork : TRafaANN;

    function GetRMSE : double;

//****    function LoadFromFile(fileName : string) : boolean;
//****    function SaveToFile(fileName : string ): boolean;

    function SaveHistoryToFile(fileName : string) : boolean;

    procedure Execute;

    function AddSequence(value : TPropSequence) : integer;
    procedure AddEmptySequence;
    procedure AddEmptyInputToSequence(sequence : integer);

    function GetSequenceCount : integer;

    function DeleteSequence(index : integer) : integer;

    procedure AddNewCurrentSequences(state : integer);
    function  UpdateCurrentSequences(input : integer; out arr: array of integer) : integer;

    procedure UpdateSequencesTreeList(treeList : TJvTreeView);
    function UpdateSequencesFromTree(index1, index2, index3 : integer) : string;

    function LoadSequencesFromStringList(strList : TStringList; start : integer; count : integer = -1) : integer;
    function SaveSequencesToStringList(strList : TStringList) : integer;
//    function SaveSequencesToStringList(strList : TStringList) : boolean;

    function GetVariablesAsString(state : boolean) : string;

    function DefineScalarInput(group : array of integer) : integer;
    function DefineScalarState(group : array of integer) : integer;
    procedure ResetScalarInputs;
    procedure ResetScalarStates;

  end;

//-------------------------------------------------------------------------------------------------

implementation

//-------------------------------------------------------------------------------------------------

uses Math, Types, UnNetRep, UnFunctions, Forms;//****, ComCtrls;

//-------------------------------------------------------------------------------------------------

{TLabelInformation}

//-------------------------------------------------------------------------------------------------

constructor TLabelInformation.Create(unification : integer = -1);
  begin
  inherited Create;
  FUnifiedTo := unification;
  FX := 0;
  FY := 0;
  end;

//-------------------------------------------------------------------------------------------------

function  TLabelInformation.GetGraphNode : TGraphNode;
  begin
  result := FGraphNode
  end;

//-------------------------------------------------------------------------------------------------

function TLabelInformation.GetUnified : integer;
  begin
  result := FUnifiedTo;
  end;

//-------------------------------------------------------------------------------------------------
function TLabelInformation.GetX : integer;
  begin
  result := FX;
  end;

//-------------------------------------------------------------------------------------------------

function TLabelInformation.GetY : integer;
  begin
  result := FY;
  end;

//-------------------------------------------------------------------------------------------------

procedure TLabelInformation.SetGraphNode (value: TGraphNode);
  begin
  FGraphNode := value;
  end;

//-------------------------------------------------------------------------------------------------

procedure TLabelInformation.SetUnified(value : integer);
  begin
  FUnifiedTo := value;
  end;

//-------------------------------------------------------------------------------------------------

procedure TLabelInformation.SetX(value : integer);
  begin
  FX := value;
  end;

//-------------------------------------------------------------------------------------------------

procedure TLabelInformation.SetY(value : integer);
  begin
  FY := value;
  end;

//-------------------------------------------------------------------------------------------------

{TPropSequence}

//-------------------------------------------------------------------------------------------------

procedure TPropSequence.AddInput;
  var i : integer;
  begin
  FNumberOfInputs := FNumberOfInputs + 1;
  SetLength(FInputSequence, FNumberOfInputs, FInputSize);
  for i := 0 to FInputSize - 1 do
    FInputSequence[FNumberOfInputs - 1, i] := 0;
  end;

//-------------------------------------------------------------------------------------------------

procedure TPropSequence.AddInputWithValues(value : array of integer);
  var i : integer;
  begin
  if length(value) >= FInputSize then
     begin
     FNumberOfInputs := FNumberOfInputs + 1;
     SetLength(FInputSequence, FNumberOfInputs, FInputSize);
     for i := 0 to FInputSize - 1 do
       FInputSequence[FNumberOfInputs - 1, i] := value[i];
     end;
  end;

//-------------------------------------------------------------------------------------------------

function TPropSequence.Compare(other : TRafaComparableObject) : integer;
  begin
  result := 0;
  end;

//-------------------------------------------------------------------------------------------------

constructor TPropSequence.Create(InputSIze, StateSize : integer);
  var i : integer;
  begin
  FInputSize := InputSize;
  FStateSize := StateSize;
  FNumberOfInputs := 0;
  FWeight := 1;
  SetLength(FInitialState, FStateSize);
  SetLength(FFinalState, FStateSize);
  for i := 0 to FStateSize - 1 do
    begin
    FInitialState[i] := 0;
    FFinalState[i] := 0;
    end;
  end;

//-------------------------------------------------------------------------------------------------

function TPropSequence.FloatDifference(other : TRafaComparableObject) : double;
  begin
  result := 0;
  end;

//-------------------------------------------------------------------------------------------------

function TPropSequence.GetFinalArray (out ar :  array of integer) : integer;
  var i : integer;
  begin
  if Length(ar) >= FStateSize then
     begin
     for i := 0 to FStateSize - 1 do
       ar[i] := FFinalState[i];
     result := FStateSize;
     end
  else
     result := -1;
  end;

//-------------------------------------------------------------------------------------------------

function TPropSequence.GetFinalVar(index : integer) : integer;
  begin
  if (index >= 0) and (index < FStateSize) then
     result := FFinalState[index]
  else
     result := -1;
  end;

//-------------------------------------------------------------------------------------------------

function TPropSequence.GetInput(i, j : integer) : integer;
  begin
  if (i >= 0) and (i < FNumberOfInputs) and (j >= 0) and (j < FInputSize) then
     result := FInputSequence[i, j]
  else
     result := 0;
  end;

//-------------------------------------------------------------------------------------------------

function TPropSequence.GetInitialArray (out ar :  array of integer) : integer;
  var i : integer;
  begin
  if Length(ar) >= FStateSize then
     begin
     for i := 0 to FStateSize - 1 do
       ar[i] := FInitialState[i];
     result := FStateSize;
     end
  else
     result := -1;
  end;

//-------------------------------------------------------------------------------------------------

function TPropSequence.GetInitialVar(index : integer) : integer;
  begin
  if (index >= 0) and (index < FStateSize) then
     result := FInitialState[index]
  else
     result := -1;
  end;

//-------------------------------------------------------------------------------------------------

function  TPropSequence.GetNumberOfInputs : integer;
  begin
  result := FNumberOfInputs;
  end;

//-------------------------------------------------------------------------------------------------

function TPropSequence.GetWeight : integer;
  begin
  result := FWeight;
  end;

//-------------------------------------------------------------------------------------------------

procedure TPropSequence.RemoveInput(index : integer);
  var i, j : integer;
  begin
  if (index >= 0) and (index < FNumberOfInputs) then
     for i := index to FNumberOfInputs - 2 do
       for j := 0 to FInputSize - 1 do
         FInputSequence[i, j] := FInputSequence[i + 1, j];
  FNumberOfInputs := FNumberOfInputs - 1;
  SetLength(FInputSequence, FNumberOfInputs, FInputSize);
  end;

//-------------------------------------------------------------------------------------------------

function TPropSequence.SetFinalArray(value : array of integer) : integer;
  var i : integer;
  begin
  if length(value) <= FStateSize then
     begin
     for i := 0 to length(value) - 1 do
        FFinalState[i] := value[i];
     result := FStateSize;
     end
  else
     result := -1;
  end;

//-------------------------------------------------------------------------------------------------

procedure TPropSequence.SetFinalVar(index, value : integer);
  begin
  if (index >= 0) and (index < FStateSize) then
     FFinalState[index] := value;
  end;

//-------------------------------------------------------------------------------------------------

function TPropSequence.SetInitialArray(value : array of integer) : integer;
  var i : integer;
  begin
  if length(value) <= FStateSize then
     begin
     for i := 0 to length(value) - 1 do
        FInitialState[i] := value[i];
     result := FStateSize;
     end
  else
     result := -1;
  end;

//-------------------------------------------------------------------------------------------------

procedure TPropSequence.SetInitialVar(index, value : integer);
  begin
  if (index >= 0) and (index < FStateSize) then
     FInitialState[index] := value;
  end;

//-------------------------------------------------------------------------------------------------

procedure TPropSequence.SetInput(i, j, value : integer);
  var Xi : integer;
  begin
  if (i >= 0) and (i < FNumberOfInputs) and (j >= 0) and (j < FInputSize) then
     FInputSequence[i, j] := value
  else if (i > FNumberOfInputs) and (j >= 0) and (j < FInputSize) then
     begin
     for Xi := FNumberOfInputs to i do
       AddInput;
     FInputSequence[i, j] := value;
    end;
  end;

//-------------------------------------------------------------------------------------------------

procedure TPropSequence.SetWeight(value : integer);
  begin
  Weight := value;
  end;

//-------------------------------------------------------------------------------------------------

{TRafaMooreMachine}

//-------------------------------------------------------------------------------------------------

procedure TRafaMooreMachine.AddDataToHistory(arInput, arOutput : array of double);
  var
    i, XIndex : integer;
  begin
  if (FHistoryStart < 0) or (FHistoryEnd < 0) then
     begin
     FHistoryStart := 0;
     FHistoryEnd   := 1;
     end
  else if (FHistoryStart = FHistoryEnd) then
     begin
     if FHistorySize <> 0 then
        begin
        FHistoryStart := (FHistoryStart + 1) mod FHistorySize;
        FHistoryEnd := (FHistoryEnd + 1) mod FHistorySize;
        end;
     end
  else
     FHistoryEnd := (FHistoryEnd + 1) mod FHistorySize;
  if FHistorySize <> 0 then
     XIndex := ((FHistoryEnd + FHistorySize) - 1) mod FHistorySize
  else
     XIndex := 0;
  for i := 0 to FInputVariables.Count + FStateVariables.Count - 1 do
    FHistory[XIndex, i] := arInput[i];
  for i := 0 to FStateVariables.Count - 1 do
    FHistory[XIndex, FInputVariables.Count + FStateVariables.Count + i] := arOutput[i];
  end;

//-------------------------------------------------------------------------------------------------

procedure TRafaMooreMachine.AddDataToHistoryWithDes(arInput, arOutput, arDesOutput : array of double);
  var
    i, XIndex : integer;
  begin
  if (FHistoryStart < 0) or (FHistoryEnd < 0) then
     begin
     FHistoryStart := 0;
     FHistoryEnd   := 1;
     end
  else if (FHistoryStart = FHistoryEnd) then
     begin
     if FHistorySize <> 0 then
        begin
        FHistoryStart := (FHistoryStart + 1) mod FHistorySize;
        FHistoryEnd := (FHistoryEnd + 1) mod FHistorySize;
        end;
     end
  else
     FHistoryEnd := (FHistoryEnd + 1) mod FHistorySize;
  if FHistorySize <> 0 then
     XIndex := ((FHistoryEnd + FHistorySize) - 1) mod FHistorySize
  else
     XIndex := 0;
  for i := 0 to FInputVariables.Count + FStateVariables.Count - 1 do
    FHistory[XIndex, i] := arInput[i];
  for i := 0 to FStateVariables.Count - 1 do
    FHistory[XIndex, FInputVariables.Count + FStateVariables.Count + i] := arOutput[i];
  if FKeepSupervisionHistory then
     for i := 0 to FStateVariables.Count - 1 do
       FSupervisionHistory[XIndex, i] := arDesOutput[i];
  end;

//-------------------------------------------------------------------------------------------------

procedure TRafaMooreMachine.AddEmptySequence;
  begin
  AddSequence(TPropSequence.Create(FInputVariables.Count, FStateVariables.Count));
  AddEmptyInputToSequence(FSequences.Length - 1);
  end;

//-------------------------------------------------------------------------------------------------

procedure TRafaMooreMachine.AddEmptyInputToSequence(sequence : integer);
  begin
  if (sequence >= 0) and (sequence < FSequences.Length) then
     GetSequence(sequence).AddInput;
  end;

//-------------------------------------------------------------------------------------------------

function TRafaMooreMachine.AddNodeToGraph(Tag, left, top : integer) : integer;
  var
    r : TRect;
  begin
  FGraph.DefaultNodeClass := TRoundRectangularNode;
  r.Top := top;
  r.Bottom := top + 30;
  r.Left := left;
  r.Right := Left + 60;
  FGraph.InsertNode(r);
  FGraph.Objects[FGraph.ObjectsCount - 1].Tag := tag;
  result := FGraph.ObjectsCount - 1;
  end;

//-------------------------------------------------------------------------------------------------

procedure TRafaMooreMachine.AddNewCurrentSequences(state : integer);
  var
    i : integer;
    ar : array of integer;
  begin
  SetLength(ar, FStateVariables.Count);
  for i := 0 to FSequences.Length - 1 do
    begin
    GetSequence(i).GetInitialArray(ar);
    if CompareStateWithArray(state, ar) then
       begin
       FCurrentSequencesCount := FCurrentSequencesCount + 1;
       SetLength(FCurrentSequences, FCurrentSequencesCount);
       SetLength(FCurrentSeqPos, FCurrentSequencesCount);
       FCurrentSequences[FCurrentSequencesCount - 1] := i;
       FCurrentSeqPos[FCurrentSequencesCount - 1] := 0;
       end;
    end;
  end;

//-------------------------------------------------------------------------------------------------

function TRafaMooreMachine.AddSequence(value : TPropSequence) : integer;
  begin
  FSequences.Add(value, false);
  result := FSequences.Length - 1;
  end;

//-------------------------------------------------------------------------------------------------

function TRafaMooreMachine.AddTransition(tr : TRafaTransition) : integer;
  begin
  tr.Owner := self;
  FTransitions.Add(tr, false);
  UpdateGraphWeights;
  result := FTransitions.Length;
  end;

//-------------------------------------------------------------------------------------------------

procedure TRafaMooreMachine.BreakInputGroup(index : integer);
  var
    i : integer;
  begin
  for i := 0 to FInputLabels.Count - 1 do
    if (i <> index) and (GetInputUnification(i) = index) then
       SetInputUnification(i, -1);
  UpdateGraphStructure;
  end;

//-------------------------------------------------------------------------------------------------

procedure TRafaMooreMachine.BreakStateGroup(index : integer);
  var i : integer;
  begin
  for i := 0 to FStateLabels.Count - 1 do
    if (i <> index) and (GetStateUnification(i) = index) then
       begin
       SetStateUnification(i, -1);
       SetXPosition(i, GetXPosition(i) + 5);
       SetYPosition(i, GetYPosition(i) + 5);
       end;
  UpdateGraphStructure;
  end;

//-------------------------------------------------------------------------------------------------


procedure TRafaMooreMachine.CalculateWeightsFromHistory;
  var
    XTmpArr : TRafaSortableArray;
    XTmpObject : TRafaTransition;
    i, j, XTmpValue : integer;
//    XTmpWeight : double;
    XActualSize : integer;
  begin
  XActualSize := ((FHistoryEnd + FHistorySize) - FHistoryStart) mod FHistorySize;
  if (FHistoryStart < 0) or (FHistoryEnd < 0) then
     XActualSize := 0
  else if XActualSize = 0 then
     XActualSize := FHistorySize;
  XTmpArr := TRafaSortableArray.Create(XActualSize);
  for i := 0 to XActualSize - 1 do
    begin
    XTmpObject := TRafaTransition.Create(self);
    XTmpValue := 0;
    for j := 0 to FInputVariables.Count - 1 do
      if FHistory[(i + FHistoryStart) mod FHistorySize, j] > 0 then
         XTmpValue := (XTmpValue * 2) + 1
      else
         XTmpValue := (XTmpValue * 2);
    XTmpObject.Input := XTmpValue;
    XTmpValue := 0;
    for j := 0 to FStateVariables.Count - 1 do
      if FHistory[(i + FHistoryStart) mod FHistorySize, FInputVariables.Count + j] > 0 then
         XTmpValue := (XTmpValue * 2) + 1
      else
         XTmpValue := (XTmpValue * 2);
    XTmpObject.SourceState := XTmpValue;
    XTmpValue := 0;
    for j := 0 to FStateVariables.Count - 1 do
      if FHistory[(i + FHistoryStart) mod FHistorySize, FInputVariables.Count + FStateVariables.Count + j] > 0 then
         XTmpValue := (XTmpValue * 2) + 1
      else
         XTmpValue := (XTmpValue * 2);
    XTmpObject.TargetState := XTmpValue;
    XTmpObject.Weight := 0;
    for j := 0 to FStateVariables.Count - 1 do
      XTmpObject.Weight := XTmpObject.Weight + abs(FHistory[(i + FHistoryStart) mod FHistorySize,
                           FInputVariables.Count + FStateVariables.Count + j]);
    XTmpObject.Weight := XTmpObject.Weight / FStateVariables.Count;
    XTmpObject.Count := 1;
    XTmpObject.Goal := 0;
    XTmpArr.Data[i] := XTmpObject;
    end;

  for i := 0 to Ftransitions.Length - 1 do
    begin
    if (FTransitions.Data[i] as TRafaTransition).Goal <> 0 then
       begin
       XTmpObject := TRafaTransition.Create(self);
       XTmpObject.SourceState := (FTransitions.Data[i] as TRafaTransition).SourceState;
       XTmpObject.TargetState := (FTransitions.Data[i] as TRafaTransition).TargetState;
       XTmpObject.Input       := (FTransitions.Data[i] as TRafaTransition).Input;
       XTmpObject.Goal        := (FTransitions.Data[i] as TRafaTransition).Goal;
       XTmpObject.Weight      := 0;
       XTmpObject.Count       := 0;
       XTmpArr.Add(XTmpObject, false);
       end;
    end;

  XTmpArr.QuickSort;
  FTransitions.Clear;
  if (XTmpArr.Data[0] <> nil) then
     FTransitions.Add(XTmpArr.Data[0], false);
  j := 0;
  for i := 1 to XActualSize - 1 do
    begin
    if (XTmpArr.Data[i - 1] <> nil) and (XTmpArr.Data[i] <> nil) then
       begin
       if XTmpArr.Data[i].Compare(XTmpArr.Data[i - 1]) = 0 then
          with FTransitions.Data[j] as TRafaTransition do
            begin
            Goal := (XTmpArr.Data[i] as TRafaTransition).Goal;
            Count := Count + (XTmpArr.Data[i] as TRafaTransition).Count;
            Weight := Weight + (XTmpArr.Data[i] as TRafaTransition).Weight;
            end
       else
          begin
          j := j + 1;
          FTransitions.Add(XTmpArr.Data[i], false);
          end;
       end;
    end;
  UpdateGraphWeights;
  end;

//-------------------------------------------------------------------------------------------------

function TRafaMooreMachine.CompareStateWithArray(state : integer; arValues : array of integer) : boolean;
  var
    i : integer;
    XTmp : integer;
    different : boolean;
  begin
  different := false;
  i := 0;
  XTmp := state;
  while (not different) and (i < FStateVariables.count) do
    begin
    //**** define proper neighboorhood of 0, instead of 0 only
    if abs(arValues[i]) > 0 then
      different := (arValues[i] > 0) xor (XTmp >= intpower(2, (FStateVariables.count - 1) - i));
    if XTmp >= intpower(2, (FStateVariables.count - 1) - i) then
       XTmp := XTmp - round(intpower(2, (FStateVariables.count - 1) - i));
    i := i + 1;
    end;
  result := not different;
  end;

//-------------------------------------------------------------------------------------------------

constructor TRafaMooreMachine.Create(graph : TSimpleGraph);
  begin
  inherited Create;
  FInputVariables := TStringList.Create;
  FStateVariables := TStringList.Create;
  FInputLabels := TStringList.Create;
  FStateLabels := TStringList.Create;
  FGraph := graph;
  FGraph.OnNodeMoveResize := GraphNodeMoveResize;
  FHistoryStart := -1;
  FHistoryEnd   := -1;
  FHistorySize  := 0;
  FDataList := TStringList.Create;

  FExecutionInfo := DefaultExecutionInfo;
  FCanExecute := false;
  FExecutionCount := 0;
  FEpochCount := 0;
  FLastEpoch := 0;
//FOutputSize := 0;
  FUpdateRate := 1;
  FUseInputFromDataList := false;
  FUseOutputFromDataList := false;

  FOnGraphUpdate := false;
  FTransitions := TRafaSortableArray.Create();

  FNetwork := TRafaANN.Create;

  FAllowLearning := true;
  FKeepSupervisionHistory := true;

  FSequences := TRafaSortableArray.Create;
  FCurrentSequencesCount := 0;
  FCustomInputSelection := 0.5;
  FFileOutputThresholdMax := 0.7;
  FFileOutputThresholdMin := 0.1;
  FRecurrentLearningRate := 1;
  FDefaultCorrection := dcNone;
  FInitialState := 0;

  FUseScalarInputs := false;
  FUseScalarStates := false;
  FAllowEmptyScalarInputs := false;
  FAllowEmptyScalarStates := false;
  end;

//-------------------------------------------------------------------------------------------------

procedure TRafaMooreMachine.CreateLabels(input : boolean; values : array of boolean);
  var
    i : integer;
//    XFalse : boolean;
    XValues : array of boolean;
    s : string;
  begin
  if (input and (length(values) < FInputVariables.Count)) or
        (not input and (length(values) < FStateVariables.Count)) then
     begin
     SetLength(XValues, length(values) + 1);
     for i := 0 to length(values) - 1 do
       XValues[i] := values[i];
     XValues[length(values)] := false;
     CreateLabels(input, Xvalues);
     XValues[length(values)] := true;
     CreateLabels(input, Xvalues);
     end
  else if (input and (length(values) = FInputVariables.Count)) then
     begin
     s := '';
     for i := 0 to FInputVariables.Count - 1 do
       if values[i] then
          s := s + FInputVariables.Strings[i];
     if trim(s) = '' then s := '*empty*';
     FInputLabels.AddObject(s, TLabelInformation.Create);
     end
  else if (not input and (length(values) = FStateVariables.Count)) then
     begin
     s := '';
     for i := 0 to FStateVariables.Count - 1 do
       if values[i] then
          s := s + FStateVariables.Strings[i];
     if trim(s) = '' then s := '*empty*';
     FStateLabels.AddObject(s, TLabelInformation.Create);
     end;
  end;

//-------------------------------------------------------------------------------------------------

procedure TRafaMooreMachine.DefaultExecutionInfo(CurrentEpoch : integer; rmse : double);
  begin

  end;

//-------------------------------------------------------------------------------------------------

function TRafaMooreMachine.DefineInput(state : integer; out ar : array of double) : integer;
  var
    i, j, Xindex, XTmpInput, XTmpState : integer;
  begin
  AddNewCurrentSequences(state);
  Randomize;
  if FUseInputFromDataList then
     XIndex := GetInputFromDataList(ar)
  else
     begin
     if (random < FCustomInputSelection) and (FCurrentSequencesCount > 0) then
        begin
        j := random(FCurrentSequencesCount);
        XIndex := 0;
        for i := 0 to FInputVariables.Count - 1 do
          begin
          XIndex := XIndex * 2;
          if GetSequence(FCurrentSequences[j]).GetInput(FCurrentSeqPos[j], i) > 0 then
             XIndex := XIndex + 1
          else if GetSequence(FCurrentSequences[j]).GetInput(FCurrentSeqPos[j], i) = 0 then
             if random < 0.5 then
                XIndex := XIndex + 1;
          end;
        end
     else
        XIndex := -1;

     if XIndex < 0 then
        XIndex := random(round(IntPower(2, FInputVariables.Count)));

     XTmpInput := XIndex;

     for j := FInputVariables.Count - 1 downto 0 do
       begin
       if XTmpInput >= IntPower(2, j) then
          begin
          XTmpInput := XTmpInput - round(IntPower(2, j));
          ar[(FInputVariables.Count - 1) - j] := 1;
          end
       else
          ar[(FInputVariables.Count - 1) - j] := -1;
       end;
     end;

  XTmpState := state;
  for j := FStateVariables.Count - 1 downto 0 do
    begin
    if XTmpState >= IntPower(2, j) then
       begin
       XTmpState := XTmpState - round(IntPower(2, j));
       ar[(FInputVariables.Count + FStateVariables.Count - 1) - j] := 1;
       end
    else
       ar[(FInputVariables.Count + FStateVariables.Count - 1) - j] := -1;
    end;
  result := XIndex;
  end;

//-------------------------------------------------------------------------------------------------

function TRafaMooreMachine.DefineScalarInput(group : array of integer) : integer;
  var i, XMax : integer;
  begin
  XMax := 0;
  for i := 0 to FInputVariables.Count - 1 do
    if FScalarInputGroup[i] > XMax then
       XMax := FScalarInputGroup[i];
  for i := 0 to (length(group) - 1) do
    if (group[i] >= 0) and (group[i] < FInputVariables.Count) then
       FScalarInputGroup[group[i]] := XMax + 1
    else
       XMax := -1;
  if XMax < 0 then
     for i := 0 to (length(group) - 1) do
       if (group[i] >= 0) and (group[i] < FInputVariables.Count) then
          FScalarInputGroup[group[i]] := 0;

  //***Organize labels - FILL IN LATER
  result := XMax + 1;
  end;

//-------------------------------------------------------------------------------------------------

function TRafaMooreMachine.DefineScalarState(group : array of integer) : integer;
  var i, XMax : integer;
  begin
  XMax := 0;
  for i := 0 to FStateVariables.Count - 1 do
    if FScalarStateGroup[i] > XMax then
       XMax := FScalarStateGroup[i];
  for i := 0 to (length(group) - 1) do
    if (group[i] >= 0) and (group[i] < FStateVariables.Count) then
       FScalarStateGroup[group[i]] := XMax + 1
    else
       XMax := -1;
  if XMax < 0 then
     for i := 0 to (length(group) - 1) do
       if (group[i] >= 0) and (group[i] < FStateVariables.Count) then
          FScalarStateGroup[group[i]] := 0;

  //***Organize labels - FILL IN LATER
  result := XMax + 1;
  end;

//-------------------------------------------------------------------------------------------------

function TRafaMooreMachine.DeleteSequence(index : integer) : integer;
  begin
  if (index >= 0) and (index < FSequences.Length) then
     begin
     FSequences.Data[index].Free;
     FSequences.Delete(index);
     result := FSequences.Length;
     end
  else
     result := -1;
  end;

//-------------------------------------------------------------------------------------------------

procedure TRafaMooreMachine.Execute;
  var
    inAr, outAr1, outAr2 : array of double;
    i, XCurrentState, XCurrentInput, {XTmpState,} XDesiredOutput : integer;
    XExecCount, Xtmp1: integer;

  begin

  XCurrentState := FInitialState;
  SetLength(inAr, FInputVariables.Count + FStateVariables.Count);
  SetLength(outAr1, FStateVariables.Count);
  SetLength(outAr2, FStateVariables.Count);
  XExecCount := 0;

  FCanExecute := true;

  while FCanExecute and (FEpochCount < FLastEpoch) do
    begin
    if FUpdateRate > FEpochSize then
       FUpdateRate := FEpochSize;
    FExecutionCount := 0;
    SetLength(FRecurrentValues, FStateVariables.Count);
    for i := 0 to FStateVariables.Count - 1 do
      FRecurrentValues[i] := -1;

    if FUseInputFromDataList then
       XCurrentState := FInitialState;

    while FExecutionCount < FEpochSize do
      begin
      XCurrentInput := DefineInput(XCurrentState, inAr);
      if FUseScalarInputs then
         XCurrentInput := FilterScalarInputs(XCurrentInput, inAr);
      FNetwork.ApplyInputArray(inAr, FInputVariables.Count + FStateVariables.Count);
      FNetwork.GetOutputArray(outAr1, FStateVariables.Count);

      XDesiredOutput := GetDesiredOutput(XCurrentInput, inAr, outAr1, outAr2);
      AddDataToHistoryWithDes(inAr, OutAr1, OutAr2);
       //***********Calculate the new value of FRecurrentValues;
//      for i := 0 to FStateVariables.Count - 1 do
//        outAr2[i] := outAr2[i] + (FRecurrentLearningRate * FRecurrentValues[i]);
      if FAllowLearning then
         FNetwork.ApplyOutputArray(outAr2, FStateVariables.Count);


      if FUseNetworkTransition or not FAllowLearning then
         begin
         XCurrentState := 0;
         for i := 0 to FStateVariables.count - 1 do
           begin
           XCurrentState := XCurrentState * 2;
           if (outAr1[i] > 0) then
              XCurrentState := XCurrentState + 1;
           end;
         end
      else
         begin
         xtmp1 := XDesiredOutput;
         XCurrentState := xtmp1;
         end;


      FExecutionCount := FExecutionCount + 1;

      XExecCount := XExecCount + 1;
      if XExecCount = FUpdateRate then
         begin
         CalculateWeightsFromHistory;
         XExecCount := 0;
         Application.ProcessMessages;
         end
      end;
    FEpochCount := FEpochCount + 1;
    Application.ProcessMessages;

    FExecutionInfo(FEpochCount, GetRMSE);

    end;

//  FtmpGoalList.Free;
  end;

//-------------------------------------------------------------------------------------------------

function TRafaMooreMachine.FilterScalarInputs (index : integer; var arInputs : array of double) : integer;
  var
    i, j : integer;
    XMaxGr, XMaxVal : integer;
  begin
  XMaxGr := 0;
  for i := 0 to FInputVariables.Count - 1 do
    if FScalarInputGroup[i] > XMaxGr then
       XMaxGr := FScalarInputGroup[i];

  for j := 1 to XMaxGr do
    begin
    XMaxVal := -1;
    for i := 0 to FInputVariables.Count - 1 do
      if (FScalarInputGroup[i] = j) and ((XMaxVal < 0) or (arInputs[i] > arInputs[XMaxVal])) then
         if (not FAllowEmptyScalarInputs) or (arInputs[i] > 0) then
            XMaxVal := i;
    for i := 0 to FInputVariables.Count - 1 do
      if (FScalarInputGroup[i] = j) then
         if (i = XMaxVal) then
            arInputs[i] := 1
         else
            arInputs[i] := -1;
    end;
  j := 0;
  for i := 0 to FInputVariables.Count - 1 do
    begin
    j := j * 2;
    if arInputs[i] > 0 then
       j := j + 1;
    end;
  result := j;
  end;

//function TRafaMooreMachine.GetAllowLearning : boolean;

//-------------------------------------------------------------------------------------------------

function  TRafaMooreMachine.GetAllowEmptyScalarInputs : boolean;
  begin
  result := FAllowEmptyScalarInputs;
  end;

//-------------------------------------------------------------------------------------------------

function  TRafaMooreMachine.GetAllowEmptyScalarStates : boolean;
  begin
  result := FAllowEmptyScalarStates;
  end;

//-------------------------------------------------------------------------------------------------

function TRafaMooreMachine.GetAllowLearning : boolean;
  begin
  result := FAllowLearning;
  end;

//-------------------------------------------------------------------------------------------------

function TRafaMooreMachine.GetDataList : TStringList;
  begin
  result := FDataList;
  end;

//-------------------------------------------------------------------------------------------------

function TRafaMooreMachine.GetDefaultCorrection : TDefaultCorrection;
  begin
  result := FDefaultCorrection;
  end;

//-------------------------------------------------------------------------------------------------

function TRafaMooreMachine.GetDesiredOutput(input : integer;
        inArray, obtState : array of double; out desState : array of double) : integer;
  var
    i, XIndex, rValue : integer;
    XDesArray : Array of integer;
//    XUsedList : TRafaSortableArray;
//    XAllowed : boolean;
//     XPositiveSize, XNegativeSize : integer;
//    XPositive, XNegative : array of integer;

  begin
  //Load DesState from file and properties - define the goal as 1 or 0
  SetLength(XDesArray, FStateVariables.Count);
  UpdateCurrentSequences(input, XDesArray);
  if FUseOutputFromDataList then
     begin
     GetOutputFromDataList(desState);
    for i := 0 to FStateVariables.Count - 1 do
       begin
       if (XDesArray[i] <> 0) and (abs(desState[i]) < FFileOutputThresholdMax) then
          desState[i] := sign(XDesArray[i])
       else if abs(DesState[i]) >= FFileOutputThresholdMin then
          desState[i] := sign(desState[i])
       else
          desState[i] := 0;
       end;
     end
  else
    for i := 0 to FStateVariables.Count - 1 do
      DesState[i] := sign(XDesArray[i]);

  XIndex := 0;
  for i := 0 to FStateVariables.Count - 1 do
    begin
    if abs(DesState[i]) < FFileOutputThresholdMin then
       case FDefaultCorrection of
         dcCurrent : DesState[i] := sign(inArray[FInputVariables.Count + i]);
         dcTrue : DesState[i] := 1;
         dcFalse : desState[i] := -1;
         dcReinforce : DesState[i] := sign(obtState[i]);
         dcNone : DesState[i] := obtState[i];
         end;
    XIndex := XIndex * 2;
    if desState[i] >= 0 then
       XIndex := XIndex + 1
    end;
  result := XIndex;
  end;

//-------------------------------------------------------------------------------------------------

function TRafaMooreMachine.GetExecutionInfo : TExecutionInfo;
  begin
  result := FExecutionInfo;
  end;

//-------------------------------------------------------------------------------------------------

function TRafaMooreMachine.GetHistorySize : integer;
  begin
  result := FHistorySize;
  end;

//-------------------------------------------------------------------------------------------------

function TRafaMooreMachine.GetInitialState : integer;
  begin
  result := FInitialState;
  end;

//-------------------------------------------------------------------------------------------------

function  TRafaMooreMachine.GetInputCount : integer;
  var i, j : integer;
  begin
  j := 0;
  for i := 0 to FInputLabels.Count - 1 do
    if (GetInputUnification(i) = i) or (GetInputUnification(i) < 0) then
       j := j + 1;
  result := j;
  end;

//-------------------------------------------------------------------------------------------------


function TRafaMooreMachine.GetInputFromData : boolean;
  begin
  result := FUseInputFromDataList;
  end;

//-------------------------------------------------------------------------------------------------

function TRafaMooreMachine.GetInputFromDataList(out arr : array of double) : integer;
  var
    i, XIndex: integer;
    XTmpStrList : TStringList;

  begin
  XIndex := 0;
  XTmpStrList := TStringList.Create;
  if (FExecutionCount >= 0) and (FExecutionCount < FDataList.Count) then
     begin
     XTmpStrList.DelimitedText := FDataList.Strings[FExecutionCount];
     for i := 0 to FInputVariables.Count - 1 do
       begin
       if i < length(arr) then
          try
            arr[i] := StrToFloat(XTmpStrList.Strings[i]);
          except
            arr[i] := 0;
            end
       else
          arr[i] := 0;
       if (XIndex < 0) or (arr[i] = 0) then
          XIndex := -1
       else
          begin
          XIndex := XIndex * 2;
          if arr[i] > 0 then XIndex := XIndex + 1;
          end;
       end;
     end
  else
    XIndex := -1;
  XTmpStrList.Free;
  result := XIndex;
  end;

//-------------------------------------------------------------------------------------------------

function  TRafaMooreMachine.GetInputLabel(index : integer) : string;
  begin
  if (index >= 0) and (index < FInputLabels.Count) then
     result := FInputLabels.Strings[index];
  end;

//-------------------------------------------------------------------------------------------------

function TRafaMooreMachine.GetInputUnification(index : integer) : integer;
  begin
  result := (FInputLabels.Objects[index] as TLabelInformation).UnifiedTo;
  end;

//-------------------------------------------------------------------------------------------------

function TRafaMooreMachine.GetInputVarCount : integer;
  begin
  result := FInputVariables.Count;
  end;

//-------------------------------------------------------------------------------------------------

function TRafaMooreMachine.GetKeepSupHistory : boolean;
  begin
  result := FKeepSupervisionHistory;
  end;

//-------------------------------------------------------------------------------------------------

{*****function TRafaMooreMachine.GetNearestState(initial, current : integer; out ArState : array of double) : integer;
  var
    Ar1, Ar2, Ar3 : array of boolean;
    i, val : integer;
    XFirstTrue, XTotalTrue : integer;
  begin
  for i := FStateVariables.Count - 1 downto 0 do
    begin
    val := round(IntPower(2, i));
    if initial >= val then
       begin
       initial := initial - val;
       Ar1[i] := true;
       end
    else
       Ar1[i] := false;
    if current >= val then
       begin
       current := current - val;
       Ar2[i] := true;
       end
    else
       Ar2[i] := false;
    end;

  XTotalTrue := 0;
  for i := 0 to FStateVariables.Count - 1 do
    begin
    Ar3[i] := Ar1[i] xor Ar2[i];
    if Ar3[i] then XTotalTrue := XTotalTrue + 1;
    end;

  i := 0;
  while (i < FStateVariables.count) and not Ar3[i] do
    i := i + 1;
  XFirstTrue := i;
  while (i < FStateVariables.count) and Ar3[i] do
    i := i + 1;
  if i < FStateVariables.Count then
     begin
     Ar3[i] := true;
     Ar3[XFirstTrue] := false;
     end
  else
     for i := 0 to FStateVariables.count - 1 do
       Ar3[i] := (i <= XTotalTrue);


  val := 0;
  for i := 0 to FStateVariables.count - 1 do
    begin
    if (Ar3[i] xor Ar1[i]) then
       begin
       val := val + 1;
       ArState[i] := 1;
       end
    else
       ArState[i] := -1;
    val := val * 2;
    end;
  end;}

//-------------------------------------------------------------------------------------------------

function TRafaMooreMachine.GetNetwork : TRafaANN;
  begin
  result := FNetwork;
  end;

//-------------------------------------------------------------------------------------------------

function TRafaMooreMachine.GetOutputFromData : boolean;
  begin
  result := FUseOutputFromDataList;
  end;

//-------------------------------------------------------------------------------------------------

function TRafaMooreMachine.GetOutputFromDataList(out arr : array of double) : integer;
  var
    i, XIndex: integer;
    XTmpStrList : TStringList;
  begin
  XIndex := 0;
  XTmpStrList := TStringList.Create;
  if (FExecutionCount >= 0) and (FExecutionCount < FDataList.Count) then
     begin
     XTmpStrList.DelimitedText := FDataList.Strings[FExecutionCount];
     for i := FInputVariables.Count to XTmpStrList.Count - 1 do
       begin
       if (i - FInputVariables.Count) < length(arr) then
          try
            arr[i - FInputVariables.Count] := StrToFloat(XTmpStrList.Strings[i]);
          except
            arr[i - FInputVariables.Count] := 0;
            end
       else
          arr[i - FInputVariables.Count] := 0;
       if (XIndex < 0) or (arr[i - FInputVariables.Count] = 0) then
          XIndex := -1
       else
          begin
          XIndex := XIndex * 2;
          if arr[i - FInputVariables.Count] > 0 then XIndex := XIndex + 1;
          end;
       end;
     end
  else
    XIndex := -1;
  XTmpStrList.Free;
  result := XIndex;
  end;

//-------------------------------------------------------------------------------------------------

//function TRafaMooreMachine.GetOutputSize : integer;
//  begin
//  result := FOutputSize;
//  end;

//-------------------------------------------------------------------------------------------------

function TRafaMooreMachine.GetRMSE : double;
  var
    i, j, k : integer;
    d1, d2 : double;
    XBool : boolean;
  begin
  if FKeepSupervisionHistory then
     begin
     if FHistoryStart >= 0 then
        begin
        i := FHistoryStart;
        XBool := true
        end
     else
        begin
        i := FHistoryEnd;
        XBool := false;
        end;
     d1 := 0;
     k := 0;
     while (i <> FHistoryEnd) or XBool do
       begin
       d2 := 0;
       for j := 0 to FStateVariables.Count - 1 do
         d2 := d2 + sqr(FHistory[i, FInputVariables.count + FStateVariables.Count + j] -
                    FSupervisionHistory[i, j]);
       if FStateVariables.Count > 0 then
          d2 := d2 / FStateVariables.Count;
       d1 := d1 + d2;
       k := k + 1;
       i := (i + 1) mod FHistorySize;
       XBool := false;
       end;
     if k = 0 then result := 0 else result := sqrt(d1/k);
     end
  else
     result := 0;
  end;


//-------------------------------------------------------------------------------------------------

function TRafaMooreMachine.GetSequence(index : integer) : TPropSequence;
  begin
  if (index >= 0) and (index < FSequences.Length) then
     result := FSequences.Data[index] as TPropSequence
  else
     result := nil;
  end;

//-------------------------------------------------------------------------------------------------

function TRafaMooreMachine.GetSequenceCount : integer;
  begin
  result := FSequences.Length;
  end;

//-------------------------------------------------------------------------------------------------

function  TRafaMooreMachine.GetStateCount : integer;
  var i, j : integer;
  begin
  j := 0;
  for i := 0 to FStateLabels.Count - 1 do
    if (GetStateUnification(i) = i) or (GetStateUnification(i) < 0) then
       j := j + 1;
  result := j;
  end;

//-------------------------------------------------------------------------------------------------

function  TRafaMooreMachine.GetStateLabel(index : integer) : string;
  begin
  if (index >= 0) and (index < FStateLabels.Count) then
     result := FStateLabels.Strings[index];
  end;

//-------------------------------------------------------------------------------------------------

function TRafaMooreMachine.GetStateUnification(index : integer) : integer;
  begin
  result := (FStateLabels.Objects[index] as TLabelInformation).UnifiedTo;
  end;

//-------------------------------------------------------------------------------------------------

function TRafaMooreMachine.GetStateVarCount : integer;
  begin
  result := FStateVariables.Count;
  end;

//-------------------------------------------------------------------------------------------------

function  TRafaMooreMachine.GetTotalInputCount : integer;
  begin
  result := FInputLabels.Count;
  end;

//-------------------------------------------------------------------------------------------------

function  TRafaMooreMachine.GetTotalStateCount : integer;
  begin
  result := FStateLabels.Count;
  end;

//-------------------------------------------------------------------------------------------------

function TRafaMooreMachine.GetTransition(index : integer): TRafaTransition;
  begin
  if (index >= 0) and (index < FTransitions.Length) then
     result := FTransitions.Data[index] as TRafaTransition
  else
     result := nil;
  end;

//-------------------------------------------------------------------------------------------------

function TRafaMooreMachine.GetTransitionCount : integer;
  begin
  result := FTransitions.Length;
  end;

//-------------------------------------------------------------------------------------------------

function TRafaMooreMachine.GetUpdateRate : integer;
  begin
  result := FUpdateRate;
  end;

//-------------------------------------------------------------------------------------------------

function TRafaMooreMachine.GetUseNetworkTransition : boolean;
  begin
  result := FUseNetworkTransition;
  end;

//-------------------------------------------------------------------------------------------------

function  TRafaMooreMachine.GetUseScalarInputs : boolean;
  begin
  result := FUseScalarInputs;
  end;

//-------------------------------------------------------------------------------------------------

function  TRafaMooreMachine.GetUseScalarStates : boolean;
  begin
  result := FUseScalarStates;
  end;

//-------------------------------------------------------------------------------------------------

function TRafaMooreMachine.GetVariablesAsString(state : boolean) : string;
  var
    i : integer;
    s : string;
  begin
  if state then
     begin
     s := FStateVariables.Strings[0];
     for i := 1 to FStateVariables.Count - 1 do
       s := s + ' ' + FStateVariables.Strings[i];
     end
  else
     begin
     s := FInputVariables.Strings[0];
     for i := 1 to FInputVariables.Count - 1 do
       s := s + ' ' + FInputVariables.Strings[i];
     end;
  end;

//-------------------------------------------------------------------------------------------------

function  TRafaMooreMachine.GetXPosition(index : integer) : integer;
  begin
  if (index >= 0) and (index < FStateLabels.Count) then
     result := (FStateLabels.Objects[index] as TLabelInformation).X
  else
     result := 0;
  end;

//-------------------------------------------------------------------------------------------------

function  TRafaMooreMachine.GetYPosition(index : integer) : integer;
  begin
  if (index >= 0) and (index < FStateLabels.Count) then
     result := (FStateLabels.Objects[index] as TLabelInformation).Y
  else
     result := 0;
  end;

//-------------------------------------------------------------------------------------------------

procedure TRafaMooreMachine.GraphNodeMoveResize(Graph: TSimpleGraph; Node: TGraphNode);
  var i : integer;
  begin
  if not FOnGraphUpdate then
     if (Node.Tag >= 0) and (Node.Tag < FStateLabels.Count) then
        begin
        for i := 0 to FStateLabels.Count - 1 do
          begin
          if (i = node.Tag) or (GetStateUnification(i) = Node.Tag) then
             begin
             (FStateLabels.objects[i] as TLabelInformation).X := Node.Left;
             (FStateLabels.objects[i] as TLabelInformation).Y := Node.Top;
             end;
          end;
   //     if Graph.OnObjectSelect <> nil then
        Graph.OnObjectSelect(Graph, Node);
        end;
  end;

//-------------------------------------------------------------------------------------------------

//function TRafaMooreMachine.LoadFromFile(FileName : string): boolean;
  //begin
  //*****Implement later
  //end;

//-------------------------------------------------------------------------------------------------


procedure   TRafaMooreMachine.LoadFromVariables(InputVar, StateVar : TStrings; nHid : integer = -1);
  var
    i : integer;
    XNet : TNetworkRep;
    XNeu : TNeuronRep;
  begin
  FInputVariables.Clear;
  FStateVariables.Clear;
  for i := 0 to InputVar.Count - 1 do
    FInputVariables.Add(InputVar.Strings[i]);
  SetLength(FScalarInputGroup, FInputVariables.Count);
  for i := 0 to FInputVariables.Count - 1 do
    FScalarInputGroup[i] := 0;
  for i := 0 to StateVar.Count - 1 do
    FStateVariables.Add(StateVar.Strings[i]);
  SetLength(FScalarStateGroup, FStateVariables.Count);
  for i := 0 to FStateVariables.Count - 1 do
    FScalarStateGroup[i] := 0;
  if FNetwork = nil then
     FNetwork := TRafaANN.Create;
  XNet := TNetworkRep.Create(nil, nil);
  if nHid <= 0 then
     nHid := InputVar.Count + StateVar.Count;
  XNeu := TNeuronRep.Create('Temp');
  XNeu.Eta := 0.1;
  XNeu.AtivFunction := 3;
  XNeu.AddFunctionParam(1);
  XNeu.FixedBias := false;
  XNeu.HasBias := true;
  XNeu.BiasWeight := 0;
  XNeu.RandomBias := 1;
  XNet.LoadFeedForward(3, [InputVar.Count + StateVar.Count, nHid, StateVar.Count], XNeu);
  FNetwork.LoadDescription(XNet);
  XNet.Free;
  XNeu.Free;
//  XTotal := intPower(2, FInputVariables.Count) - 1;
  CreateLabels(true, []);
  CreateLabels(false, []);
  UpdateGraphStructure;
  end;

//-------------------------------------------------------------------------------------------------

procedure TRafaMooreMachine.LoadNetwork(FileName : TFileName; extraNeurons : integer = 0);
  var
    XNet : TNetworkRep;
    i : integer;
    //Xtotal : extended;
  begin
  XNet := TNetworkRep.Create(nil, nil);
  XNet.LoadFromXML(FileName);
  if extraNeurons > 0 then
     for i := 0 to extraNeurons - 1 do
       XNet.AddConnectedNeuron(1);
  //Every output is considered a state, and the number of inputs is given by total net inputs - states
  if ((XNet.GetInputCount - XNet.GetOutputCount) <> FInputVariables.Count) or
     (XNet.GetOutputCount <> FStateVariables.Count) then
     begin
     FInputVariables.Clear;
     FStateVariables.Clear;
     if XNet.GetInputCount > 20 then
        raise Exception.Create(' Sorry, but our model is bounded to a maximum of 20 variables (states + input).');
     for i := 0 to (XNet.GetInputCount - XNet.GetOutputCount) - 1 do
       FInputVariables.add(chr(ord('a') + i));
     SetLength(FScalarInputGroup, FInputVariables.Count);
     for i := 0 to FInputVariables.Count - 1 do
       FScalarInputGroup[i] := 0;
     for i := 0 to (XNet.GetOutputCount) - 1 do
       FStateVariables.add(chr(ord('A') + i));
     SetLength(FScalarStateGroup, FStateVariables.Count);
     for i := 0 to FStateVariables.Count - 1 do
       FScalarStateGroup[i] := 0;
     CreateLabels(false, []);
     CreateLabels(true, []);
     UpdateGraphStructure;
     end;
  FNetwork.Free;
  FNetwork := TRafaANN.Create;
  FNetwork.AddRealFunction(logsig, logsig_linha);
  FNetwork.AddRealFunction(tansig, tansig_linha);
  FNetwork.AddRealFunction(BiLogsig, BiLogsig_linha);
  FNetwork.AddRealFunction(Threshold, Threshold_Linha);
  FNetwork.AddRealFunction(BiThreshold, BiThreshold_Linha);
  FNetwork.AddRealFunction(Linear, Linear_Linha);
  FNetwork.AddRealFunction(CrazyA, CrazyA_Linha);
  FNetwork.LoadDescription(XNet);
  FNetwork.ResetNetwork;
  XNet.Free;
//  XTotal := intPower(2, FInputVariables.Count) - 1;
  end;

//-------------------------------------------------------------------------------------------------

function TRafaMooreMachine.LoadSequencesFromStringList(strList : TStringList; start : integer; count : integer = -1) : integer;
  var
    XTmpStrList : TStringList;
    i, j, k, XSequencesCount, XInputCount : integer;
    XTmpSequence : TPropSequence;
  begin
  if strList <> nil then
     begin
     FSequences.Clear(true);
     XTmpStrList := TStringList.Create;
     i := start;
     if count < 0 then
        begin
        while (i < strList.Count) and (trim(strList.Strings[i]) = '') do
          i := i + 1;
        if i < strList.Count then
          XTmpStrList.DelimitedText := StrList.Strings[i];
        i := i + 1;
        try
          XSequencesCount := strtoint(XTmpStrList.Strings[0]);
        except
          XSequencesCount := 0;
          end;
        end
     else
        XSequencesCount := count;

     while (i < strList.Count) and (FSequences.Length < XSequencesCount) do
       begin
       XInputCount := 0;
       XTmpSequence := TPropSequence.Create(FInputVariables.Count, FStateVariables.Count);
       while (i < strList.Count) and (trim(strList.Strings[i]) = '') do
         i := i + 1;
       if (i < strList.Count) then
          begin
          XTmpStrList.DelimitedText := StrList.Strings[i];
          try
            XInputCount := StrToInt(trim(XTmpStrList.Strings[0]));
          except
            XInputCount := 0;
            end;
          i := i + 1;
          end;

       while (i < strList.Count) and (trim(strList.Strings[i]) = '') do
         i := i + 1;
       if (i < strList.Count) then
          begin
          XTmpStrList.DelimitedText := StrList.Strings[i];
          j := 0;
          while (j < FStateVariables.Count) and (j < XTmpStrList.Count) do
            begin
            try
              XTmpSequence.InitialVar[j] := StrToInt(trim(XTmpStrList.Strings[j]));
              j := j + 1
            except
              j := XTmpStrList.Count;
              end;
            end;
          i := i + 1;
          end;

       while (i < strList.Count) and (trim(strList.Strings[i]) = '') do
         i := i + 1;
       if (i < strList.Count) then
          begin
          k := 0;
          while (i < strList.Count) and (k < XInputCount) do
            begin
            if (trim(strList.Strings[i]) <> '') then
               begin
               XTmpStrList.DelimitedText := strList.Strings[i];
               j := 0;
               XTmpSequence.AddInput;
               while (j < FInputVariables.Count) and (j < XTmpStrList.Count) do
                 begin
                 try
                   XTmpSequence.Input[k, j] := StrToInt(trim(XTmpStrList.Strings[j]));
                   j := j + 1
                 except
                   j := XTmpStrList.Count;
                   end
                 end;
               k := k + 1;
               end;
            i := i + 1;
            end;

          end;

       while (i < strList.Count) and (trim(strList.Strings[i]) = '') do
         i := i + 1;
       if (i < strList.Count) then
          begin
          XTmpStrList.DelimitedText := StrList.Strings[i];
          j := 0;
          while (j < FStateVariables.Count) and (j < XTmpStrList.Count) do
            begin
            try
              XTmpSequence.FinalVar[j] := StrToInt(trim(XTmpStrList.Strings[j]));
              j := j + 1
            except
              j := XTmpStrList.Count;
              end;
            end;
          i := i + 1;
          end;
       AddSequence(XTmpSequence);
       end;
     result := i;
     end
  else
    result := -1;
  end;

//-------------------------------------------------------------------------------------------------

procedure TRafaMooreMachine.ResetEpoch;
  begin
  FEpochCount := 0;
  FExecutionCount := 0;
  end;

//-------------------------------------------------------------------------------------------------

procedure TRafaMooreMachine.ResetHistory;
  begin
  FHistoryStart := -1;
  FHistoryEnd := -1;
  CalculateWeightsFromHistory;
  end;

//-------------------------------------------------------------------------------------------------

procedure TRafaMooreMachine.ResetScalarInputs;
  var i : integer;
  begin
  for i := 0 to FInputVariables.Count - 1 do
    FScalarInputGroup[i] := 0;
  end;


//-------------------------------------------------------------------------------------------------

procedure TRafaMooreMachine.ResetScalarStates;
  var i : integer;
  begin
  for i := 0 to FStateVariables.Count - 1 do
    FScalarStateGroup[i] := 0;
  end;

//-------------------------------------------------------------------------------------------------

function TRafaMooreMachine.ReviewLogicProgram(logProg : TSingleLogicProgram) : Integer;
  var
    i, j, k : integer;
    XConflictsCount : integer;
    XTmpClause : TSingleClause;
    XTmpSource, XTmpTarget, XTmpInput : integer;
  begin

  if (logProg.GetAtomCount = (FInputVariables.count + (2 * FStateVariables.Count))) then
    for i := 0 to FStateVariables.Count - 1 do
      logProg.AddStrucAtom('not_' + FStateVariables.Strings[i]);

  if logProg.GetAtomCount = (FInputVariables.Count + (3 * FStateVariables.Count)) then
     begin
     for j := 0 to FTransitions.Length - 1 do
       begin
       //Compare transition with prog TP (FP)
       for i := 0 to FStateVariables.Count - 1 do
         {
           If conflict than add the variable and increment XCOnflictCount
         }
       end;
     result := XConflictsCount
     end
  else
     result := -1;
          {XTmpClause := TSingleClause.Create(XProgram.GetStructure, nil);
          XTmpTarget := GetTransition(j).TargetState;
          XTmpSource := GetTransition(j).SourceState;
          XTmpInput  := GetTransition(j).Input;
    //      if XTmpTarget >= IntPower(2, FStateVariables.Count - i) then
          XTmpTarget := XTmpTarget mod round(IntPower(2, FStateVariables.Count - i));
          if XTmpTarget >= IntPower(2, (FStateVariables.Count - 1) - i) then
             XTmpClause.Head := FStateVariables.Count + FInputVariables.Count + i + 1
          else
             XTmpClause.Head := (2 * FStateVariables.Count) + FInputVariables.Count + i + 1;
          for k := 0 to FStateVariables.Count - 1 do
            if XTmpSource >= IntPower(2, (FStateVariables.Count - 1) - k) then
               begin
               XTmpClause.AddBodyLiteral(k + 1);
               XTmpSource := XTmpSource - round(IntPower(2, (FStateVariables.Count - 1) - k));
               end
            else
               XTmpClause.AddBodyLiteral(-(k + 1));
          for k := 0 to FInputVariables.Count - 1 do
            if XTmpInput >= IntPower(2, (FInputVariables.Count - 1) - k) then
               begin
               XTmpClause.AddBodyLiteral(FStateVariables.count + k + 1);
               XTmpInput := XTmpInput - round(IntPower(2, (FInputVariables.Count - 1) - k));
               end
            else
               XTmpClause.AddBodyLiteral(-(FStateVariables.count + k + 1));
          XTmpClause.Count := GetTransition(j).Count;
          XTmpClause.Weight := GetTransition(j).Weight;
          XProgram.AddClause(XTmpClause);
          end;
      result := XColisions Count;}
  end;

//-------------------------------------------------------------------------------------------------

function TRafaMooreMachine.SaveHistoryToFile(fileName : string) : boolean;
  var
    i, j : integer;
    XTmpStrList : TStringList;
    s : string;
    XBool : boolean;
  begin
  XTmpStrList := TStringList.Create;
  if (FHistoryStart >= 0) then
     begin
     i := FHistoryStart;
     XBool := true;
     end
  else
     begin
     XBool := false;
     i := FHistoryEnd;
     end;
  while (i <> FHistoryEnd) or XBool  do
    begin
    s := '';
    for j := 0 to FInputVariables.Count - 1 do
      s := s + floattostr(FHistory[i, j]) + ' ';
    XTmpStrList.Add(s);
    s := '';
    for j := 0 to FStateVariables.Count - 1 do
      s := s + floattostr(FHistory[i, FInputVariables.Count + j]) + ' ';
    XTmpStrList.Add(s);
    s := '';
    for j := 0 to FStateVariables.Count - 1 do
      s := s + floattostr(FHistory[i, FInputVariables.Count + FStateVariables.Count + j]) + ' ';
    XTmpStrList.Add(s);
    s := '';
    if FKeepSupervisionHistory then
       for j := 0 to FStateVariables.Count - 1 do
         s := s + floattostr(FSupervisionHistory[i, j]) + ' ';
    XTmpStrList.Add(s);
    XTmpStrList.Add('');
    i := (i + 1) mod FHistorySize;
    XBool := false;
    end;
  try
    try
      XTmpStrList.SaveToFile(fileName);
      result := true;
    except
      result := false
      end;
  finally
    XTmpStrList.Free
    end;

  end;

//-------------------------------------------------------------------------------------------------

function TRafaMooreMachine.SaveSequencesToStringList(strList : TStringList) : integer;
  var
    i, j, k : integer;
    s : string;
  begin
  for i := 0 to FSequences.Length - 1 do
    begin
    s := inttostr(GetSequence(i).NumberOfInputs);
    strList.Add(s);
    s := inttostr(GetSequence(i).InitialVar[0]);
    for j := 0 to FStateVariables.Count - 1 do
      s := s + ' ' + inttostr(GetSequence(i).InitialVar[j]);
    strList.Add(s);
    for k := 0 to GetSequence(i).NumberOfInputs - 1 do
      begin
      s := inttostr(GetSequence(i).Input[k, 0]);
      for j := 0 to FInputVariables.Count - 1 do
        s := s + ' ' + inttostr(GetSequence(i).Input[k, j]);
      strList.Add(s);
      end;
    s := inttostr(GetSequence(i).FinalVar[0]);
    for j := 0 to FStateVariables.Count - 1 do
      s := s + ' ' + inttostr(GetSequence(i).FinalVar[j]);
    strList.Add(s);
    end;
  result := strList.Count;
  end;

//-------------------------------------------------------------------------------------------------

procedure TRafaMooreMachine.SetAllowEmptyScalarInputs (value : boolean);
  begin
  FAllowEmptyScalarInputs := value;
  end;

//-------------------------------------------------------------------------------------------------

procedure TRafaMooreMachine.SetAllowEmptyScalarStates (value : boolean);
  begin
  FAllowEmptyScalarStates := value;
  end;

//-------------------------------------------------------------------------------------------------

procedure TRafaMooreMachine.SetAllowLearning(value : boolean);
  begin
  FAllowLearning := value;
  end;

//-------------------------------------------------------------------------------------------------

procedure TRafaMooreMachine.SetCustomInputSelection(value : double);
  begin
  if (value >= 0) and (value <= 1) then
     FCustomInputSelection := value;
  end;

//-------------------------------------------------------------------------------------------------

procedure TRafaMooreMachine.SetDefaultCorrection(value : TDefaultCorrection);
  begin
  FDefaultCorrection := value;
  end;

//-------------------------------------------------------------------------------------------------

procedure TRafaMooreMachine.SetEpochSize(value : integer);
  begin
  FEpochSize := value;
  SetHistorySize(FEpochSize);
  end;

//-------------------------------------------------------------------------------------------------

procedure TRafaMooreMachine.SetExecutionInfo(value : TExecutionInfo);
  begin
  FExecutionInfo := value;
  end;

//-------------------------------------------------------------------------------------------------

procedure TRafaMooreMachine.SetHistorySize(value : integer);
  begin
  if value >= 0 then
     begin
     FHistorySize := value;
     if FUpdateRate > value then
        FUpdateRate := FHistorySize;
     SetLength(FHistory, FHistorySize, (2 * FStateVariables.Count) + FInputVariables.Count);
     if FKeepSupervisionHistory then
        SetLength(FSupervisionHistory, FHistorySize, FStateVariables.Count);
     end;
  end;

//-------------------------------------------------------------------------------------------------

procedure TRafaMooreMachine.SetInitialState(value : integer);
  begin
  FInitialState := value;
  end;

//-------------------------------------------------------------------------------------------------

procedure TRafaMooreMachine.SetInputFromData(value : boolean);
  begin
  FUseInputFromDataList := value;
  if value then FEpochSize := FDataList.Count;
  end;

//-------------------------------------------------------------------------------------------------

procedure TRafaMooreMachine.SetInputLabel(index : integer; value : string);
  begin
  if (index >= 0) and (index < FInputLabels.Count) then
     FInputLabels.Strings[index] := value;
  end;

//-------------------------------------------------------------------------------------------------

procedure TRafaMooreMachine.SetInputUnification(index, value : integer);
  begin
  if (index >= 0) and (index < FInputLabels.Count) then
     begin
     if (value >= 0) and (value < index) then
        (FInputLabels.Objects[index] as TLabelInformation).UnifiedTo := value
     else
        (FInputLabels.Objects[index] as TLabelInformation).UnifiedTo := -1;
     end;
  end;

//-------------------------------------------------------------------------------------------------

procedure TRafaMooreMachine.SetKeepSupHistory(value : boolean);
  begin
  FKeepSupervisionHistory := value;
  if (FHistorySize > 0) then
     if value then
        SetLength(FSupervisionHistory, FHistorySize, FStateVariables.Count)
     else
        SetLength(FSupervisionHistory, 0, 0);
  end;

//-------------------------------------------------------------------------------------------------

procedure TRafaMooreMachine.SetLastEpoch(value : integer);
  begin
//  if not FUseInputFromDataList then
  FLastEpoch := value;
  end;

//-------------------------------------------------------------------------------------------------

procedure TRafaMooreMachine.SetOutputFromData(value : boolean);
  begin
  FUseOutputFromDataList := value;
  end;

//-------------------------------------------------------------------------------------------------

//procedure TRafaMooreMachine.SetOutputSize(value : integer);
//  begin
//  if value <= FStateVariables.Count then
//     FOutputSize := value;
//  end;

//-------------------------------------------------------------------------------------------------

procedure TRafaMooreMachine.SetSequence(index : integer; value : TPropSequence);
  begin
  if (index >= 0) and (index < FSequences.Length) then
     FSequences.Data[index] := value;
  end;

//-------------------------------------------------------------------------------------------------

procedure TRafaMooreMachine.SetStateLabel(index : integer; value : string);
  begin
  if (index >= 0) and (index < FStateLabels.Count) then
     FStateLabels.Strings[index] := value;
  end;

//-------------------------------------------------------------------------------------------------

procedure TRafaMooreMachine.SetStateUnification(index, value : integer);
  begin
  if (index >= 0) and (index < FStateLabels.Count) then
     begin
     if (value >= 0) and (value < index) then
        (FStateLabels.Objects[index] as TLabelInformation).UnifiedTo := value
     else
        (FStateLabels.Objects[index] as TLabelInformation).UnifiedTo := -1;
     end;
  end;

//-------------------------------------------------------------------------------------------------

procedure TRafaMooreMachine.SetTransitionsUnification;
  var i : integer;
  begin
  for i := 0 to FTransitions.Length - 1 do
    GetTransition(i).DefineUnified(i);
  end;

//-------------------------------------------------------------------------------------------------

procedure TRafaMooreMachine.SetUpdateRate(value : integer);
  begin
  if (value > 0) then
     FUpdateRate := value;
  end;

//-------------------------------------------------------------------------------------------------

procedure TRafaMooreMachine.SetUseNetworkTransition (value : boolean);
  begin
  FUseNetworkTransition := value;
  end;

//-------------------------------------------------------------------------------------------------

procedure TRafaMooreMachine.SetUseScalarInputs (value : boolean);
  begin
  FUseScalarInputs := value;
  end;

//-------------------------------------------------------------------------------------------------

procedure TRafaMooreMachine.SetUseScalarStates (value : boolean);
  begin
  FUseScalarStates := value;
  end;

//-------------------------------------------------------------------------------------------------

procedure TRafaMooreMachine.SetXPosition(index, value : integer);
  var
    i : integer;
    XUp : boolean;
  begin
  XUp := false;
  if (index >= 0) and (index < FStateLabels.Count) then
     begin
     for i := 0 to FStateLabels.COunt - 1 do
       if (i = index) or ((FStateLabels.Objects[index] as TLabelInformation).UnifiedTo = i) then
       begin
       if (FStateLabels.Objects[i] as TLabelInformation).X <> value then
          begin
          (FStateLabels.Objects[i] as TLabelInformation).X := value;
          XUp := true;
          end;
       end;
     if XUp then UpdateGraphStructure;
     end;
  end;

//-------------------------------------------------------------------------------------------------

procedure TRafaMooreMachine.SetYPosition(index, value : integer);
  var
    i : integer;
    XUp : boolean;
  begin
  XUp := false;
  if (index >= 0) and (index < FStateLabels.Count) then
     begin
     for i := 0 to FStateLabels.COunt - 1 do
       if (i = index) or ((FStateLabels.Objects[index] as TLabelInformation).UnifiedTo = i) then
       begin
       if (FStateLabels.Objects[i] as TLabelInformation).Y <> value then
          begin
          (FStateLabels.Objects[i] as TLabelInformation).Y := value;
          XUp := true;
          end;
       end;
     if XUp then UpdateGraphStructure;
     end;
  end;

//-------------------------------------------------------------------------------------------------

procedure TRafaMooreMachine.StopExecution;
  begin
  FCanExecute := false;
  end;

//-------------------------------------------------------------------------------------------------

function TRafaMooreMachine.TransitionsAsLogicProgram : TSingleLogicProgram;
  var
    i, j, k : integer;
    XProgram : TSingleLogicProgram;
    XTmpClause : TSingleClause;
    XTmpSource, XTmpTarget, XTmpInput : integer;
  begin
  XProgram := TSingleLogicProgram.Create;
  for i := 0 to FStateVariables.Count - 1 do
    XProgram.AddStrucAtom('st_' + FStateVariables.Strings[i]);
  for i := 0 to FInputVariables.Count - 1 do
    XProgram.AddStrucAtom('in_' + FInputVariables.Strings[i]);
  for i := 0 to FStateVariables.Count - 1 do
    XProgram.AddStrucAtom('next_' + FStateVariables.Strings[i]);
  for i := 0 to FStateVariables.Count - 1 do
    XProgram.AddStrucAtom('not_' + FStateVariables.Strings[i]);
  for i := 0 to FStateVariables.Count - 1 do
    for j := 0 to FTransitions.Length - 1 do
      begin
      XTmpClause := TSingleClause.Create(XProgram.GetStructure, nil);
      XTmpTarget := GetTransition(j).TargetState;
      XTmpSource := GetTransition(j).SourceState;
      XTmpInput  := GetTransition(j).Input;
//      if XTmpTarget >= IntPower(2, FStateVariables.Count - i) then
      XTmpTarget := XTmpTarget mod round(IntPower(2, FStateVariables.Count - i));
      if XTmpTarget >= IntPower(2, (FStateVariables.Count - 1) - i) then
         XTmpClause.Head := FStateVariables.Count + FInputVariables.Count + i + 1
      else
         XTmpClause.Head := (2 * FStateVariables.Count) + FInputVariables.Count + i + 1;
      for k := 0 to FStateVariables.Count - 1 do
        if XTmpSource >= IntPower(2, (FStateVariables.Count - 1) - k) then
           begin
           XTmpClause.AddBodyLiteral(k + 1);
           XTmpSource := XTmpSource - round(IntPower(2, (FStateVariables.Count - 1) - k));
           end
        else
           XTmpClause.AddBodyLiteral(-(k + 1));
      for k := 0 to FInputVariables.Count - 1 do
        if XTmpInput >= IntPower(2, (FInputVariables.Count - 1) - k) then
           begin
           XTmpClause.AddBodyLiteral(FStateVariables.count + k + 1);
           XTmpInput := XTmpInput - round(IntPower(2, (FInputVariables.Count - 1) - k));
           end
        else
           XTmpClause.AddBodyLiteral(-(FStateVariables.count + k + 1));
      XTmpClause.Count := GetTransition(j).Count;
      XTmpClause.Weight := GetTransition(j).Weight;
      XProgram.AddClause(XTmpClause);
      end;
  result := XProgram;
  end;

//-------------------------------------------------------------------------------------------------



procedure TRafaMooreMachine.TransitionsToText(selection : array of integer; text : TStrings;
         Simplified : boolean);
  var
    i, j, XTextPos : integer;
    XCountArr : array of integer;
    XWeightArr : array of double;
  begin
//  j := 0;
  text.Clear;
  SetLength(XCountArr, length(selection));
  SetLength(XWeightArr, length(selection));
{  for i := 0 to FTransitions.Length - 1 do
    begin
    if (GetTransition(i).UnifiedTo = -1) or (GetTransition(i).UnifiedTo = i) then
       begin
       SetLength(XTmpArr, j + 1);
       XTmpArr[j] := i;
       j := j + 1;
       end;
    end;}
  for j := 0 to length(selection) - 1 do
    begin
    XTextPos := text.Count;
    for i := 0 to FTransitions.Length - 1 do
      begin
      if (i = selection[j]) or (GetTransition(i).UnifiedTo = selection[j]) then
         begin
         if not Simplified then
            begin
            text.Add('  ' + GetTransition(i).ToString);
            text.Add('  ' + 'Weight: ' + FloatToStr(GetTransition(i).Weight));
            text.Add('  ' + 'Count: '  + IntToStr(GetTransition(i).Count));
            text.Add('');
            end;

         XWeightArr[j] := XWeightArr[j] + GetTransition(i).Weight;
         XCountArr[j]  := XCountArr[j] + GetTransition(i).Count;
         end;
      end;
    text.Insert(XTextPos, GetTransition(selection[j]).ToString);
    if XCountArr[j] = 0 then
       begin
       text.Insert(XTextPos + 1, 'No incidence');
       text.Insert(XTextPos + 2, '');
       end
    else
       begin
       text.Insert(XTextPos + 1, 'Weight: ' + FloatToStr(XWeightArr[j] / XCountArr[j]));
       text.Insert(XTextPos + 2, 'Count: '  + IntToStr(XCountArr[j]));
       text.Insert(XTextPos + 3, '');
       end;
    end;


  end;

//-------------------------------------------------------------------------------------------------

procedure TRafaMooreMachine.UnifyInputs(inp : array of integer; NewLabel : string);
  var i : integer;
  begin
  for i := 1 to length(inp) - 1 do
    (FInputLabels.Objects[inp[i]] as TLabelInformation).UnifiedTo := inp[0];
  UpdateGraphStructure;
  end;

//-------------------------------------------------------------------------------------------------

procedure TRafaMooreMachine.UnifyStates(sta : array of integer; NewLabel : string);
  var i : integer;
  begin
  for i := 1 to length(sta) - 1 do
    begin
    (FStateLabels.Objects[sta[i]] as TLabelInformation).UnifiedTo := sta[0];
    SetXPosition(sta[i], GetXPosition(sta[0]));
    SetYPosition(sta[i], GetYPosition(sta[0]));
    end;

  UpdateGraphStructure;
  end;

//-------------------------------------------------------------------------------------------------

function TRafaMooreMachine.UpdateCurrentSequences(input : integer; out arr: array of integer) : integer;
  var
    i, j, l, XTmp : integer;
    XSatisfy, XTmpBool : boolean;
  begin
  i := 0;
  j := 0;
  for l := 0 to FStateVariables.Count - 1 do
    arr[l] := 0;
  while i < FCurrentSequencesCount do
    begin
    l := 0;
    XSatisfy := true;
    XTmp := input;
    while XSatisfy and (l < FInputVariables.Count) do
      begin
      if XTmp >= IntPower(2, (FInputVariables.Count - 1) - l) then
         begin
         XTmpBool := true;
         XTmp := XTmp - round(IntPower(2, (FInputVariables.Count - 1) - l));
         end
      else
         XTmpBool := false;
      if GetSequence(FCurrentSequences[i]).Input[FCurrentSeqPos[i], l] <> 0 then
         begin
         XTmpBool := XTmpBool xor (GetSequence(FCurrentSequences[i]).Input[FCurrentSeqPos[i], l] > 0);
         XSatisfy := not XTmpBool;
         end;
      l := l + 1;
      end;
    if XSatisfy then
       begin
       FCurrentSeqPos[i] := FCurrentSeqPos[i] + 1;
       if FCurrentSeqPos[i] >= GetSequence(FCurrentSequences[i]).NumberOfInputs then
          begin
          for l := 0 to FStateVariables.Count - 1 do
            arr[l] := arr[l] + GetSequence(FCurrentSequences[i]).FinalVar[l];
          i := i + 1;
          end
       else
          begin
          FCurrentSequences[j] := FCurrentSequences[i];
          FCurrentSeqPos[j] := FCurrentSeqPos[i];
          j := j + 1;
          i := i + 1;
          end;
       end
    else
       i := i + 1;
    end;

  FCurrentSequencesCount := j;
  SetLength(FCurrentSequences, FCurrentSequencesCount);
  SetLength(FCurrentSeqPos, FCurrentSequencesCount);
  result := j;

  end;

//-------------------------------------------------------------------------------------------------

procedure TRafaMooreMachine.UpdateGraphStructure;
  var
    i : integer;
  begin
  FOnGraphUpdate := true;
  FGraph.Clear;
  for i := 0 to FStateLabels.Count - 1 do
    if (GetStateUnification(i) = -1) or (GetStateUnification(i) = i) then
       (FStateLabels.Objects[i] as TLabelInformation).GraphNode :=
                  FGraph.Objects[AddNodeToGraph(i, GetXPosition(i), GetYPosition(i))] as TGraphNode;
  SetTransitionsUnification;
  UpdateGraphWeights;
  FOnGraphUpdate := false;
  end;

//-------------------------------------------------------------------------------------------------

procedure TRafaMooreMachine.UpdateGraphWeights;
  var
    i, j, xTmpSize, xSou, xTar, xInp : integer;
    XBlue, Xgreen, XRed, XColor : integer;
    xMat: array of array of integer;
    XCouArr, xTmpArr : array of integer;
    XWeiArr : array of double;


  begin
  for i := FGraph.Objects.Count - 1 downto 0 do
    if FGraph.Objects.Items[i] is TGraphLink then
       FGraph.Objects.Delete(i);
  setLength(xMat, FStateLabels.Count, FStateLabels.Count);
  for i := 0 to GetStateCount - 1 do
    for j := 0 to GetStateCount - 1 do
      XMat[i, j] := 0;


  XTmpSize := 0;
  for i := 0 to FTransitions.Length - 1 do
    begin
    if (GetTransition(i).UnifiedTo = -1) or (GetTransition(i).UnifiedTo = i) then
       begin
       XTmpSize := XTmpSize + 1;
       SetLength(XTmpArr, XTmpSize);
       SetLength(XWeiArr, XTmpSize);
       SetLength(XCouArr, XTmpSize);
       XTmpArr[XTmpSize - 1] := i;
       XWeiArr[XTmpSize - 1] := GetTransition(i).Weight;
       XCouArr[XTmpSize - 1] := GetTransition(i).Count;
       end
    else
       begin
       for j := 0 to XTmpSize - 1 do
         if GetTransition(i).UnifiedTo = XTmpArr[j] then
            begin
            XWeiArr[j] := XWeiArr[j] + GetTransition(i).Weight;
            XCouArr[j] := XCouArr[j] + GetTransition(i).Count;
            end;
       end;
    end;

  for j := 0 to XTmpSize - 1 do
    begin
    XSou := GetTransition(XTmpArr[j]).SourceState;
    if (GetStateUnification(XSou) <> XSou) and (GetStateUnification(XSou) >= 0) then
       XSou := GetStateUnification(XSou);
    XTar := GetTransition(XTmpArr[j]).TargetState;
    if (GetStateUnification(XTar) <> XTar) and (GetStateUnification(XTar) >= 0) then
       XTar := GetStateUnification(XTar);
    XInp := GetTransition(XTmpArr[j]).Input;
    if (GetInputUnification(XInp) <> XInp) and (GetInputUnification(XInp) >= 0) then
       XInp := GetInputUnification(XInp);
    FGraph.InsertLink((FStateLabels.objects[XSou] as TLabelInformation).GraphNode,
                      (FStateLabels.objects[XTar] as TLabelInformation).GraphNode);
    GetTransition(xTmpArr[j]).GraphLink := FGraph.Objects.Items[FGraph.Objects.Count - 1] as TGraphLink;
    with FGraph.Objects.Items[FGraph.Objects.Count - 1] as TGraphLink do
      begin
      if XCouArr[j] = 0 then
         XGreen := 0
      else
         XGreen := round((XWeiArr[j] / XCouArr[j]) * 255) ;
      if GetTransition(xTmpArr[j]).Goal > 0 then
         begin
         XBlue := 255;
         XRed  := 0;
         end
      else if GetTransition(xTmpArr[j]).Goal < 0 then
         begin
         XBlue := 0;
         XRed  := 255;
         end
      else
         begin
         XGreen := 255 - XGreen;
         XBlue := XGreen;
         XRed  := XGreen;
         end;
      XColor := XBlue;
      XColor := (XColor * 256) + XGreen;
      XColor := (XColor * 256) + XRed;
      Pen.Color := XColor;
      Tag := 0;
//****      Tag := XMat[XSou, xTar];
      Text := InputLabel[XInp];
      XMat[XSou, xTar] := XMat[XSou, xTar] + 1;
      end;
    end;
  end;

//-------------------------------------------------------------------------------------------------

function TRafaMooreMachine.UpdateSequencesFromTree(index1, index2, index3 : integer) : string;
  var s : string;
  begin
  s := '';
  if (index1 >= 0) and (index1 < FSequences.Length) then
     if (index2 = 0) then
        begin
        if (index3 >= 0) and (index3 < FStateVariables.Count) then
           begin
           if GetSequence(index1).InitialVar[index3] < 0 then
              begin
              GetSequence(index1).InitialVar[index3] := 0;
              s := FStateVariables.Strings[index3] + ': ?';
              end
           else if GetSequence(index1).InitialVar[index3] = 0 then
              begin
              GetSequence(index1).InitialVar[index3] := 1;
              s := FStateVariables.Strings[index3] + ': true';
              end
           else
              begin
              GetSequence(index1).InitialVar[index3] := -1;
              s := FStateVariables.Strings[index3] + ': false';
              end
           end;
        end
     else if (index2 > 0) and (index2 < GetSequence(index1).NumberOfInputs + 1) then
        begin
        if (index3 >= 0) and (index3 < FInputVariables.Count) then
           begin
           if GetSequence(index1).Input[index2 - 1, index3] < 0 then
              begin
              GetSequence(index1).Input[index2 - 1, index3] := 0;
              s := FInputVariables.Strings[index3] + ': ?';
              end
           else if GetSequence(index1).Input[index2 - 1, index3] = 0 then
              begin
              GetSequence(index1).Input[index2 - 1, index3] := 1;
              s := FInputVariables.Strings[index3] + ': true';
              end
           else
              begin
              GetSequence(index1).Input[index2 - 1, index3] := -1;
              s := FInputVariables.Strings[index3] + ': false';
              end
           end;
        end
     else if (index2 = GetSequence(index1).NumberOfInputs + 1) then
        begin
        if (index3 >= 0) and (index3 < FStateVariables.Count) then
           begin
           if GetSequence(index1).FinalVar[index3] < 0 then
              begin
              GetSequence(index1).FinalVar[index3] := 0;
              s := FStateVariables.Strings[index3] + ': ?';
              end
           else if GetSequence(index1).FinalVar[index3] = 0 then
              begin
              GetSequence(index1).FinalVar[index3] := 1;
              s := FStateVariables.Strings[index3] + ': true';
              end
           else
              begin
              GetSequence(index1).FinalVar[index3] := -1;
              s := FStateVariables.Strings[index3] + ': false';
              end
           end;
        end;
  result := s;
  end;

//-------------------------------------------------------------------------------------------------

procedure TRafaMooreMachine.UpdateSequencesTreeList(treeList : TJvTreeView);
  var
//  XTmpNode1, XTmpNode2, XTmpNode3 : TTreeNode;
    i, j, k, XInd1, XInd2 : integer;
    s, XVar, XInitial, XFinal, XInput : string;
  begin
  treeList.Items.Clear;
  for i := 0 to FSequences.Length - 1 do
    begin
    XInitial := '';
    XFinal := '';
    for k := 0 to FStateVariables.Count - 1 do
      begin
      s := '';
      if GetSequence(i).InitialVar[k] < 0 then
         s := '�' + FStateVariables.Strings[k]
      else if GetSequence(i).InitialVar[k] > 0 then
         s := FStateVariables.Strings[k];
      if s <> '' then
         if XInitial = '' then Xinitial := s else XInitial := XInitial + ', ' + s;
      s := '';
      if GetSequence(i).FinalVar[k] < 0 then
         s := '�' + FStateVariables.Strings[k]
      else if GetSequence(i).FinalVar[k] > 0 then
         s := FStateVariables.Strings[k];
      if s <> '' then
         if XFinal = '' then XFinal := s else XFinal := XFinal + ', ' + s;
      end;

    s := XInitial + ' -> ' + inttostr(GetSequence(i).NumberOfInputs) + ' -> ' + XFinal;
    treelist.Items.Add(nil, s);
    XInd1 := treelist.Items.Count - 1;
    treeList.Items.AddChild(treeList.Items.Item[XInd1], XInitial);
    XInd2 := treelist.Items.Count - 1;
    for k := 0 to FStateVariables.Count - 1 do
      begin
      if GetSequence(i).InitialVar[k] < 0 then
         XVar := FStateVariables.Strings[k] + ': false'
      else if GetSequence(i).InitialVar[k] > 0 then
         XVar := FStateVariables.Strings[k] + ': true'
      else
         XVar := FStateVariables.Strings[k] + ': ?';
      treeList.Items.AddChild(treeList.Items.Item[XInd2], XVar);
      end;

    for j := 0 to GetSequence(i).NumberOfInputs - 1 do
      begin
      treeList.Items.AddChild(treeList.Items.Item[XInd1], '');
      XInd2 := treelist.Items.Count - 1;
      XInput := '';
      for k := 0 to FInputVariables.Count - 1 do
        begin
        s := '';
        if GetSequence(i).Input[j, k] < 0 then
           begin
           s := '�' + FInputVariables.Strings[k];
           XVar := FInputVariables.Strings[k] + ': false';
           end
        else if GetSequence(i).Input[j, k] > 0 then
           begin
           s := FInputVariables.Strings[k];
           XVar := FInputVariables.Strings[k] + ': true';
           end
        else
           XVar :=  FInputVariables.Strings[k] + ': ?';
        if s <> '' then
           if XInput = '' then XInput := s else XInput := XInput + ', ' + s;
        treeList.Items.AddChild(treeList.Items.Item[XInd2], XVar);
        end;
      treeList.Items.Item[XInd2].Text := XInput;
      end;

    treeList.Items.AddChild(treeList.Items.Item[XInd1], XFinal);
    XInd2 := treelist.Items.Count - 1;
    for k := 0 to FStateVariables.Count - 1 do
      begin
      if GetSequence(i).FinalVar[k] < 0 then
         XVar := FStateVariables.Strings[k] + ': false'
      else if GetSequence(i).FinalVar[k] > 0 then
         XVar := FStateVariables.Strings[k] + ': true'
      else
         XVar := FStateVariables.Strings[k] + ': ?';
      treeList.Items.AddChild(treeList.Items.Item[XInd2], XVar);
      end;
    end;
  end;

//-------------------------------------------------------------------------------------------------

{ TRafaTransition }

//-------------------------------------------------------------------------------------------------


function TRafaTransition.Compare(other : TRafaComparableObject) : integer;
  var
    Xtmp : TRafaTransition;
     XComp : integer;
  begin
  if other is TRafaTransition then
     begin
     XTmp := other as TRafaTransition;
     XComp := XTmp.SourceState - FSourceState;
     if XComp = 0 then XComp := XTmp.Input - FInput;
     if XComp = 0 then XComp := XTmp.TargetState - FTargetState;
     result := sign(XComp);
     end
  else
     result := -2;
  end;

//-------------------------------------------------------------------------------------------------

constructor TRafaTransition.Create(Owner: TRafaMooreMachine);
  begin
  FOwner := Owner;
  FSourceState := -1;
  FTargetState := -1;
  FInput := -1;
  FWeight := 0;
  FCount := 0;
  FGoal := 0;
  FUnifiedTo := -1;
  end;

//-------------------------------------------------------------------------------------------------

function TRafaTransition.DefineUnified(index : integer = -1) : integer;
  var
    i : integer;
    found : boolean;
    XSou, XTar, XInp : integer;
  begin
  if index < 0 then index := FOwner.GetTransitionCount;
  i := 0;
  found := false;
  while (not found) and (i < index) do
    begin
    XSou := FOwner.GetStateUnification(FSourceState);
    if XSou = -1 then XSou := FSourceState;
    XTar := FOwner.GetStateUnification(FTargetState);
    if XTar = -1 then XTar := FTargetState;
    XInp := FOwner.GetInputUnification(FInput);
    if XInp = -1 then XInp := FInput;
    found := (XSou = FOwner.GetTransition(i).SourceState);
    found := found and (XTar = FOwner.GetTransition(i).TargetState);
    found := found and (XInp = FOwner.GetTransition(i).Input);
    i := i + 1;
    end;
  if found then
     begin
     FUnifiedTo := i - 1;
     FGoal := FOwner.GetTransition(i - 1).Goal;
     end
  else
     FUnifiedTo := -1;
  result := FUnifiedTo;
  end;

//-------------------------------------------------------------------------------------------------

function TRafaTransition.FloatDifference(other : TRafaComparableObject) : double;
  begin
  result := 0;
  end;

//-------------------------------------------------------------------------------------------------

function TRafaTransition.GetCount : integer;
  begin
  result := FCount;
  end;

//-------------------------------------------------------------------------------------------------


function TRafaTransition.GetGoal : integer;
  begin
  result := FGoal;
  end;

//-------------------------------------------------------------------------------------------------

function TRafaTransition.GetGraphLink  : TGraphLink;
  begin
  result := FGraphLink;
  end;

//-------------------------------------------------------------------------------------------------

function TRafaTransition.GetInput : integer;
  begin
  result := FInput;
  end;

//-------------------------------------------------------------------------------------------------

function TRafaTransition.GetOwner  : TRafaMooreMachine;
  begin
  result := FOwner;
  end;

//-------------------------------------------------------------------------------------------------

function TRafaTransition.GetSource : integer;
  begin
  result := FSourceState;
  end;

//-------------------------------------------------------------------------------------------------

function TRafaTransition.GetTarget : integer;
  begin
  result := FTargetState;
  end;

//-------------------------------------------------------------------------------------------------

function TRafaTransition.GetWeight : double;
  begin
  result := FWeight;
  end;

//-------------------------------------------------------------------------------------------------

function TRafaTransition.GetUnified : integer;
  begin
  result := FUnifiedTo;
  end;

//-------------------------------------------------------------------------------------------------

procedure TRafaTransition.SetCount(value: integer);
  begin
  FCount := value;
  end;

//-------------------------------------------------------------------------------------------------

procedure TRafaTransition.SetGoal(value: integer);
  begin
  FGoal := value;
  end;

//-------------------------------------------------------------------------------------------------

procedure TRafaTransition.SetGraphLink (value: TGraphLink);
  begin
  FGraphLink := value;
  end;

//-------------------------------------------------------------------------------------------------

procedure TRafaTransition.SetInput(value: integer);
  begin
  FInput := value;
  end;

//-------------------------------------------------------------------------------------------------

procedure TRafaTransition.SetOwner (value: TRafaMooreMachine);
  begin
  FOwner := value;
  end;

//-------------------------------------------------------------------------------------------------


procedure TRafaTransition.SetSource(value: integer);
  begin
  FSourceState := value;
  end;

//-------------------------------------------------------------------------------------------------

procedure TRafaTransition.SetTarget(value: integer);
  begin
  FTargetState := value;
  end;

//-------------------------------------------------------------------------------------------------

procedure TRafaTransition.SetWeight(value: double);
  begin
  FWeight := value;
  end;

//-------------------------------------------------------------------------------------------------

function TRafaTransition.ToString : string;
  var s : string;
  begin
  s := FOwner.StateLabel[FSourceState];
  s := s + ' -> ' + FOwner.InputLabel[FInput];
  s := s + ' -> ' + FOwner.StateLabel[FTargetState];
  result := s;
  end;

end.

