{
  @abstract(Unit that describes the behaviour of the neural network)
  @author(Rafa "Passarel" Borges <passarel@gmail.com)
  @created(Porto Alegre, September 30, 2005)
  @lastmod(London, April, 2008)

  This unit contains the classes to model the behaviour of a network, including
the propagation of values through the net and the correction of weights
(supervised training)

   It is necessary  to review all the items marked with ???

}
unit UnNetBehaviour;

interface

uses Classes, SysUtils, UnNetRep;

//Dialogs;

{------------------------------------------------------------------------------}

{Defines the mode of execution of the network (Training, test and check)}
type TNetworkMode = (NMTraining, NMTest, NMCheck);

{------------------------------------------------------------------------------}

{
@abstract(Abstract class encapsulating the units of a network)

  The abstract class @name describes the main methods for the propagation
(forth and backwards) of the values through the units of the network.

  Any unit to be inserted on the network must be an instance of a subclass of
@name.
}
type TRafaItem = class
  private
  public
    //Procedure to apply an input value to the unit (forward execution)
    //@param(data : Value to be applied)
    //@param(Sender : Unit that has applied the value)
    procedure InputData(data : double; Sender : TRafaItem); virtual; abstract;

    //Procedure to apply an error value to the unit (backward execution)
    //@param(Pdelta: Value to be applied)
    //@param(PWeight: Weight of the connection where the value was applied)
    //@param(Sender : Unit that has applied the value)
    procedure BackInput(Pdelta : double; Pweight : double; Sender : TRafaItem); virtual; abstract;

    //Function to get the weight of a specific arc connected on the input
    //@param(arcIndex: General index of the arc)
    //@return(The index of the arc)
    function  GetWeightByArcIndex(arcIndex : integer) : double; virtual; abstract;

    //Procedure to initialize the weights according the defaults
    procedure Initialize; virtual; abstract;

  end;

{------------------------------------------------------------------------------}

{
@abstract(Class encapsulating the input units of a network)

  The class @name encapsulates the input interface of the network, contaning
the routines to give the treatment of values inserted on it. The instances of
this class have a static behaviour, only addapting the input value to an
adequated interval described on its properties.
}
type TInputUnit = class(TRafaItem)
  private
    //Count of units connected on the output
    FOutputConnections : integer;

    //Array with the units connected on the output of the unit
    FOutputs : array of TRafaItem;

    //Array keeping the indexes of the arcs on the output of the network
    FOutputArcsIndex : array of integer;

    //Min value that can be aplied on the unit
    FInMin : double;

    //Max value that can be aplied on the unit
    FInMax : double;

    //Min value that the unit apply on its output
    FOutMin : double;

    //Max value that the unit apply on its output
    FOutMax : double;

    //Read method for the property InMin
    function GetInMin: double;

    //Read method for the property InMax
    function GetInMax: double;

    //Read method for the property OutMin
    function GetOutMin: double;

    //Read method for the property OutMax
    function GetOutMax: double;

    //Write method for the property InMax
    procedure SetInMax(const Value: double);

    //Write method for the property InMin
    procedure SetInMin(const Value: double);

    //Write method for the property OutMax
    procedure SetOutMax(const Value: double);

    //Write method for the property OutMin
    procedure SetOutMin(const Value: double);

  public
    //Constuctor of the class
    //@param(PRep: Description of the unit to be loaded)
    constructor Create(PRep : TIOUnitRep);

    //Add an unit on the output
    //@param(index: index of the connection)
    //@param(PUnit: Unit to be inserted)
    procedure AddOutput(index : integer; PUnit : TRafaItem);

    //Procedure to apply an input value to the unit (forward execution)
    //@param(data : Value to be applied)
    //@param(Sender : Unit that has applied the value)
    procedure InputData(data : double; Sender : TRafaItem); override;

    //Procedure to apply an error value to the unit (backward execution)
    //@param(Pdelta: Value to be applied)
    //@param(PWeight: Weight of the connection where the value was applied)
    //@param(Sender : Unit that has applied the value)
    procedure BackInput(Pdelta : double; Pweight : double; Sender : TRafaItem); override;

    //Function to get the weight of a specific arc connected on the input
    //@param(arcIndex: General index of the arc)
    //@return(The index of the arc)
    function  GetWeightByArcIndex(arcIndex : integer) : double; override;

    //Procedure to initialize the weights according the defaults
    procedure Initialize; override;

    //Min value that can be aplied on the unit
    property  InMin : double  read GetInMin  write SetInMin;

    //Max value that can be aplied on the unit
    property  InMax : double  read GetInMax  write SetInMax;

    //Min value that the unit apply on its output
    property  OutMin : double read GetOutMin write SetOutMin;

    //Max value that the unit apply on its output
    property  OutMax : double read GetOutMax write SetOutMax;

  end;

{------------------------------------------------------------------------------}

{
@abstract(Class encapsulating the output units of a network)

  The class @name encapsulates the output interface of the network, contaning
routine to treat the return values of the net and to calculate the output error
for the training, based on the application of the expected value.

  The instances of this class adapt the value received from the other units to
an interval described on its properties, and receives an external value
containing the expected result, and starts the backpropagation of the errors
}
type TOutputUnit = class(TRafaItem)
  private
    //Mode of execution of the network
    FMode: TNetworkMode;

    //Value expected on the output
    FDesiredValue : double;

    //Output calculated in function of the value received from the network
    FObtainedValue : double;

    //Indicates if a desired value was already applied
    FGetDesired : boolean;

    //Indicates if the obtained value was already applied
    FGetObtained : boolean;

    //Unit connected on the input
    FUnit : TRafaItem;

    //Index of the connection on the input
    FArcIndex : integer;

    //Min value that can be aplied on the unit
    FInMin : double;

    //Max value that can be aplied on the unit
    FInMax : double;

    //Min value that the unit apply on its output
    FOutMin : double;

    //Max value that the unit apply on its output
    FOutMax : double;

    //Propagate the calculated error to the unit connected on the input
    procedure exec;

    //Write method for the property Mode
    procedure SetMode(value : TNetworkMode);

    //Read method for the property Mode
    function  GetMode : TNetworkMode;

    //Read method for the property ObtainedValue
    Function  GetObtainedValue : double;

    //Read method for the property DesiredValue
    Function  GetDesiredValue : double;

    //Read method for the property InMax
    function GetInMax: double;

    //Read method for the property InMin
    function GetInMin: double;

    //Write method for the property InMax
    procedure SetInMax(const Value: double);

    //Write method for the property InMin
    procedure SetInMin(const Value: double);

    //Read method for the property OutMax
    function GetOutMax: double;

    //Read method for the property OutMin
    function GetOutMin: double;

    //Write method for the property OutMax
    procedure SetOutMax(const Value: double);

    //Write method for the property OutMin
    procedure SetOutMin(const Value: double);

  public
    //Constuctor of the class
    //@param(Description of the unit to be loaded)
    constructor Create(PRep : TIOUnitRep);

    //Defined the unit connected on the input
    //@param(index: index of the connection)
    //@param(PUnit: unit to be connected)
    procedure DefineUnit(index : integer; PUnit : TRafaItem);


    //Procedure to apply an input value to the unit (forward execution)
    //@param(data : Value to be applied)
    //@param(Sender : Unit that has applied the value)
    procedure InputData(data : double; Sender : TRafaItem); override;

    //Procedure to apply an error value to the unit (backward execution)
    //@param(Pdelta: Value to be applied)
    //@param(PWeight: Weight of the connection where the value was applied)
    //@param(Sender : Unit that has applied the value)
    procedure BackInput(Pdelta : double; Pweight : double; Sender : TRafaItem); override;

    //Function to get the weight of a specific arc connected on the input
    //@param(arcIndex: General index of the arc)
    //@return(The index of the arc)
    function  GetWeightByArcIndex(arcIndex : integer) : double; override;

    //Procedure to initialize the weights according the defaults
    procedure Initialize; override;

    //Mode of execution of the network
    property Mode : TNetworkMode read getMode write SetMode;

    //Output calculated in function of the value received from the network
    property ObtainedValue : double read GetObtainedValue;

    //Indicates if a desired value was already applied
    property DesiredValue  : double read GetDesiredValue;

    //Min value that can be aplied on the unit
    property  InMin : double  read GetInMin  write SetInMin;

    //Max value that can be aplied on the unit
    property  InMax : double  read GetInMax  write SetInMax;

    //Max value that the unit apply on its output
    property  OutMin : double read GetOutMin write SetOutMin;

    //Max value that the unit apply on its output
    property  OutMax : double read GetOutMax write SetOutMax;

  end;

{------------------------------------------------------------------------------}

{
@abstract(Class encapsulating the behaviour of an artificial neuron)

  The class @name encapsulates all the funcionalities of an artificial neuron.
These are the dynamic units on the network, and the main responsible for
its behavior.

  The instances of its classes receives a set of values from its inputs, process
these values and propagate the result to the units connected to its output. The
inverse behavior occurs to execute the backpropagation of the error.
}
type TNeuron = class(TRafaItem)
  private
    //Pondered sum of inputs
    FSum : double;

    //Value to be multiplied for the bias weight (usually 0 or 1)
    FExternalBias : integer;

    //Learning rate for the neuron
    FEta    : double;

    //Momentum
    FMomentum : double;

    //Measurement of error on the neuron
    FDelta  : double;

    //Array with the errors received from output units
    FErrors : array of double;

    //Sum of the errors received from output units
    FSumErrors : double;

    //Array of values applied on the input
    FInput             : array of double;

    //Array of the default values of weights of the synapses
    FDefaultWeights    : array of double;

    //Boolean values representing which weights are random on initialization
    FRandomWeights     : array of double;

    //Boolean values representing which weights are not changed on the training
    FFixedWeights      : array of boolean;

    //Array of the actual values of weights of the synapses
    FWeights           : array of double;

    //Array of the last difference of weights
    FLastDeltaWeights  : array of double;

    //Count of input connections
    FInputConnections  : integer;

    //Boolean values representing which inputs were already received
    FFlagInputs        : array of boolean;

    //Array keeping the indexes of the arcs on the input
    FInputArcsIndex    : array of integer;

    //Count of the inputs that already received a value
    FReceivedInputs    : integer;

    //Array with the units connected on the input of the network
    FInputUnits        : array of TRafaItem;

    //Calculated value of output to be propagated
    FOutput             : double;

    //Count of units connected on the output
    FOutputConnections  : integer;

    //Boolean values marking which outputs alredy received an error information
    FFlagOutputs        : array of boolean;

    //Count of outputs that received the error information
    FReceivedOutputs    : integer;

    //Array keeping the indexes of the arcs on the output of the network
    FOutputArcsIndex    : array of integer;

    //Array with the units connected on the output of the neuron
    FOutputUnits        : array of TRafaItem;

    //Array with the params of the activation function
    FParams             : array of double;

    //Count of the params of the activation function
    FParamCount         : integer;

    //Activation function of the neuron
    FActivation         : TParamFunction;

    //Derivated of the activation function
    FDerivated          : TParamFunction;

    //Size of the history
//    FHistorySize : integer;

    //Function that returns the index of a unit on the input arrays
    //@param(The input unit)
    //@return(The index)
    function GetInputIndex(un : TRafaItem) : integer;

    //Function that returns the index of a unit on the output arrays
    //@param(un: The output unit)
    //@return(The index)
    function GetOutputIndex(un : TRafaItem) : integer;

    //Procedure describing the process of activation of the neuron
    function Activate : double;

    //Procedure describing the backpropagation processing on the neuron
    procedure BackProc;

    //Accessor to the Eta (learning rate) property
    function getEta : double;

    //Mutator to the Eta (learning rate) property
    procedure setEta(value : double);

  public
    //Constructor of the class
    //@param(PRep: The description of the neuron)
    //@param(PAtiv: List of activation functions allowed on the network)
    //@param(PAtiv: List of the derivated of the activation functions)
    constructor Create(PRep : TNeuronRep; PAtiv, PDeriv : array of TParamFunction; HistSize : integer = 1);

    //Procedure to insert a new input connection on the neuron
    //@param(index: Index of the arc)
    //@param(PUnit: Unit to be connected)
    //@param(PWeight: Weight of the connection)
    //@param(fix: Defines if the weight is fixed)
    //@param(ran: Defines if the weight is random)
    procedure AddInput(index : integer; Punit : TRafaItem; PWeight : double; fix : boolean; ran : double);


    //Procedure to insert a new output connection on the neuron
    //@param(index: Index of the arc)
    //@param(PUnit: Unit to be connected)
    procedure AddOutput(index : integer; Punit : TRafaItem);

    //Procedure to apply an input value to the unit (forward execution)
    //@param(data : Value to be applied)
    //@param(Sender : Unit that has applied the value)
    procedure InputData(data : double; Sender : TRafaItem); override;

    //Procedure to apply an error value to the unit (backward execution)
    //@param(Pdelta: Value to be applied)
    //@param(PWeight: Weight of the connection where the value was applied)
    //@param(Sender : Unit that has applied the value)
    procedure BackInput(Pdelta : double; Pweight : double; Sender : TRafaItem); override;

    //Function to get the weight of a specific arc connected on the input)
    //@param(arcIndex: General index of the arc)
    //@return(The index of the arc)
    function  GetWeightByArcIndex(arcIndex : integer) : double; override;

    //Function to get the weight of the bias
    //@return(The weight of bias)
    function  GetBiasWeight : double;

    //Procedure to initialize the weights according the defaults
    procedure Initialize; override;

    //Learning rate to the neuron
    property Eta : double read getEta write setEta;

    //Read only property to get the count of inputs
    property InputsCount : integer read FInputConnections;

    //Load the weights from an array
    //@param(WAr: array to be loaded)
    //@param(pos: initial posistion to be read on the array)
    procedure LoadWeightsArray(WAr: array of double; pos : integer);

    //Save the weights to an array
    //@param(WAr: array to save the data)
    //@param(pos: initial posistion to write on the array)
    procedure SaveWeightsArray(var WAr: array of double; pos : integer);

    //Correct Eta Value
    procedure CorrectEta(value : double);

  end;

{------------------------------------------------------------------------------}

{
@abstract(Class encapsulating a delay unit on the network)

  The class @name encapsulates the behaviour of a delay unit. These units acts
as memory dispositives, keeping the last values applied to it and propagation
the result of a delay function applied to these values.
}
type TDelayUnit = class(TRafaItem)
  private
    //Numbers of times allowed to be kept in the memory
    FTimes : integer;

    //Unit connected on the input
    FInput : array of double;

    //Array of delta kept for error propagation
    FDelta : array of double;

    //Number of delta already received
    FFilledDelta : integer;

    //Number of inputs already received
    FFilledInputs : integer;

    //Calculated value of output to be propagated
    FOutput : double;

    //Delta value to be propagated as error on backpropagation
    FDeltaOutput : double;

    //Delay function of the unit
    FDelayFunction : TDelayFunction;

    //Unit connected on the input
    FInputUnit         : TRafaItem;

    //Index of the arcs on the input
    FInputArcIndex     : integer;

    //Array keeping the indexes of the arcs on the output of the unit
    FOutputArcsIndex   : array of integer;

    //Array with the units connected on the output
    FOutputUnits       : array of TRafaItem;

    //Array flagging the values of delta already received
    FOutputFlag       : array of boolean;

    //Count of delta values received for backpropagation
    FReceivedOutputs : integer;

    //Count of units connected on the output
    FOutputConnections : integer;

    //Function that returns the index of a unit on the output arrays
    //@param(un: The output unit)
    //@return(The index)
    function GetOutputIndex(un : TRafaItem) : integer;

    //Routine to execute the backpropagation step
    //@param(value: value to be propagated ???)
    procedure BackExecution(value : double);


  public
    //Constructor of the class
    //@param(PRep: Description of the unit to be loaded)
    //@param(PFunc: Array of delay functions)
    constructor Create(PRep : TDelayRep; PFunc : array of TDelayFunction);

    //Defines the unit connected on the input
    //@param(index: index of the connection)
    //@param(PUnit: Unit to be connected)
    procedure  DefineInput(index : integer; PUnit : TRafaItem);

    //Adds a unit on the output
    //@param(index: index of the connection to be inserted)
    //@param(PUnit: Unit to be connected)
    procedure  AddOutput(index : integer; PUnit : TRafaItem);

    //Procedure to apply an input value to the unit (forward execution)
    //@param(data : Value to be applied)
    //@param(Sender : Unit that has applied the value)
    procedure InputData(data : double; Sender : TRafaItem); override;

    //Procedure to apply an error value to the unit (backward execution)
    //@param(Pdelta: Value to be applied)
    //@param(PWeight: Weight of the connection where the value was applied)
    //@param(Sender : Unit that has applied the value)
    procedure BackInput(Pdelta : double; Pweight : double; Sender : TRafaItem); override;

    //Function to get the weight of a specific arc connected on the input
    //@param(arcIndex: General index of the arc)
    //@return(The index of the arc)
    function  GetWeightByArcIndex(arcIndex : integer) : double; override;

    //Procedure to initialize the weights according the defaults
    procedure Initialize; override;

    //Clear all the past informations
    procedure ResetTimes;

  end;

{------------------------------------------------------------------------------}

{
@abstract(Class encapsulating units to control the syncronism of the network)

  The class @name encapsulates simple units, that propagate the same value
applied to their inputs. The main utility of the instances of this class is a
triggering mechanism, that allows the control of the flow of values through
the network and the syncronism of the behaviour of the units.
}
type TLinkPoint = class(TRafaItem)
  private
    //Value to be propagated on forward step
    FInputValue              : double;

    //Value to be propagated on forward step
    FOutputValue              : double;

    //Old value of forward step
    FOldValue           : double;

    //Value to be propagated on backward step
    FInputBackValue          : double;

    //Value to be propagated on backward step
    FOutputBackValue          : double;

    //Old value of backward step
    FOldBackValue       : double;

    //Level of the unit
    FLevel              : integer;

    //Initialization value of the link (almost not used);
    FInitValue : double;

    //Unit connected on the input
    FInputUnit          : TRafaItem;

    //Indicates if the value applied to the unot has stabilized
    FStable             : boolean;

    //Indicates if the unit already received a value
    FReceived           : boolean;

    //Indicates if the unit needs of a exteral trigger to propagate the forward value
    FTriggered          : boolean;

    //Indicates if the unit needs of a exteral trigger to propagate the backward value
    FBackTriggered      : boolean;

    //Style of execution of backpropagation
    FLinkBackStyle      : TLinkBackStyle;

    //Count of units connected on the output
    FOutputConnections  : integer;

    //Array with the units connected on the output
    FOutputUnits        : array of TRafaItem;

    //Count of backprop values already received
    FOutputCount        : integer;

    //Array with flags indicating the outputs that already sent backprop value
    FOutputMarks        : array of boolean;

    //Sum of the backprop values received
    FOutputSum          : double;

    //Read method for the property level
    function GetLevel: integer;

    //Write method for the property level
    procedure SetLevel(const Value: integer);

    //Read method for the property InitValue
    function GetInitValue: double;

    //Write method for the property InitValue
    procedure SetInitValue(const Value: double);

    //Function to get the index of an unit connected on the output
    //@param(item: Unit to get the index)
    //return(index of the unit)
    function GetOutputIndex(item : TRafaItem) : integer;


    //Read method for the property Received
    function GetReceived: boolean;

    //Write method for the property Received
    procedure SetReceived(const Value: boolean);

    //Read method for the property Triggered
    function GetTriggered: boolean;

    //Write method for the property Triggered
    procedure SetTriggered(const Value: boolean);

    //Read method for the property BackTriggered
    function GetBackTriggered: boolean;

    //Write method for the property Triggered
    procedure SetBackTriggered(const Value: boolean);
    function GetinputValue: double;
    function GetOutputValue: double;

  public
    //Constructor of the class
    //@param(orig: Description of the unit to be loaded)
    constructor Create(orig : TRecLinkRep);

    //Level of the unit
    property  Level : integer read GetLevel write SetLevel;

    //Initialization value of the link (almost not used);
    property InitValue : double read GetInitValue write SetInitValue;

    //Indicates if the unit already received a value
    property  Received  : boolean read GetReceived write SetReceived;

    //Indicates if the unit needs of a exteral trigger to propagate the forward value
    property  IsTriggered : boolean read GetTriggered write SetTriggered;

    //Indicates if the unit needs of a exteral trigger to propagate the backward value
    property  IsBackTriggered : boolean read GetBackTriggered write SetBackTriggered;

    //Set the input unit
    //@param(PUnit: input unit)
    procedure SetInput(Punit : TRafaItem);

    //Add a unit connected on the output
    //@param(PUnit: unit to be connected)
    procedure AddOutput(Punit : TRafaItem);

    //Procedure to apply an input value to the unit (forward execution)
    //@param(data : Value to be applied)
    //@param(Sender : Unit that has applied the value)
    procedure InputData(data : double; Sender : TRafaItem); override;

    //Procedure to apply an error value to the unit (backward execution)
    //@param(Pdelta: Value to be applied)
    //@param(PWeight: Weight of the connection where the value was applied)
    //@param(Sender : Unit that has applied the value)
    procedure BackInput(Pdelta : double; Pweight : double; Sender : TRafaItem); override;

    //Function to get the weight of a specific arc connected on the input
    //@param(arcIndex: General index of the arc)
    //@return(The index of the arc)
    function  GetWeightByArcIndex(arcIndex : integer) : double; override;

    //Procedure to initialize the weights according the defaults
    procedure Initialize; override;

    //Trigger the unit to the forward propagation
    //@param(Tag: sinalizes for definition the value that must be propagated)
    procedure Trigger(Kind : char = 'a'; value : integer = 0);

    //Trigger the unit to the forward propagation
    procedure PreTrigger;

    //Trigger the unit to the backward propagation
    //@param(Tag: sinalizes for definition the value that must be propagated)
    procedure BackTrigger(Kind : char = 'a'; value : integer = 0);

    //Trigger the unit to the backward propagation
    procedure PreBackTrigger;

    //Read property returning the input value of the unity
    property InputValue : double read GetinputValue;

    //Read property returning the output value of the unity
    property OutputValue : double read GetOutputValue;


  end;

{------------------------------------------------------------------------------}

{
@abstract(Class encapsulating the general behaviour of a neural network)

  The class @name contains a description of the set of units on the network, and
encapsulate the routines to coordinate the flow of information through these
units.
}
type TRafaANN = class
  private
    //Number of activation functions allowed
    FNoRealF    : integer;

    //Number of delay functions allowed
    FNoDelayF   : integer;

    //Array of activation functions
    FRealFuncs  : array of TParamFunction;

    //Array with the derivates of the activation functions
    FDerivFuncs : array of TParamFunction;

    //Factor to multiply the Eta value
    FEtaCorrection : double;

    //Array of the delay functions
    FDelayFuncs : array of TDelayFunction;

    //Number of input units
    FNoInputs    : integer;

    //Array with the input units of the network
    FInputUnits  : array of integer;

    //Number of the output units
    FNoOutputs   : integer;

    //Array with the output units of the network
    FOutputUnits : array of integer;

    //Number of units on the network
    FNoUnits    : integer;

    //Array with the units on the network
    FUnits      : array of TRafaItem;

    //Count of levels of link points
    FLevelCount : integer;

    //Count of link points
    FLinksCount : array of integer;

    //Matrix with rows indicating the levels and cols representing the index of link points
    FLinkUnits  : array of array of integer;

    //Flow of values on the link points
    FLinkFlow   : TLinkFlow;

    //Count of correct cases on a validation
    FCasesCount : integer;

    //Number of FeedForward executions
    FNu : integer;

    //Number of backpropagation steps per time point (simmilar to nu)
    FUpsilon : integer;

    //Number of worlds in a CML-like network
    FWorldCount : integer;

    //Array to keep information about weights of the network
    FWeightsArray : array of double;

    //Add a unit on the network
    //@param(pos ???)
    //@param(PUnit: description of the unit to be inserted)
    procedure AddUnit(pos : integer; PUnit : TUnitRep);

    //Trigger all the links of a level
    //@param(level to be triggered)
    //@param(parameter to be passed on trigger)
    procedure Trigger (lv : integer; Kind : char = 'a'; value : integer = 0);

    //Procedure that calls PreBackTrigger for all link points of level lv
    procedure PreBackTrigger (lv : integer);

    //Procedure that calls PreTrigger for all link points of level lv
    procedure PreTrigger (lv : integer);

    //Trigger all the links of a level backwards
    //@param(level to be triggered)
    //@param(parameter to be passed on trigger)
    procedure BackTrigger(lv : integer; Kind : char = 'a'; value : integer = 0);

    //Executes the routines to initialization of CML networks
    procedure InitializeCML;

    //Executes the routines to initialization of Elman networks
    procedure InitializeElman;

    //Executes the routines to initialization of SCTL networks
    procedure InitializeSCTL;

    //Executes the routines to initialization of CILP networks
    procedure InitializeCILP;

    //Executes the routines to initialization of CILP networks
    function InitializeSaving : string;

    //Execute the propagation of values of Elman networks
    procedure ExecuteElman;

    //Execute the propagation of values of SCTL networks
    procedure ExecuteSCTL;

    //Execute the propagation of values of CILP networks
    procedure ExecuteCILP;

    //Execute the propagation of values of CML networks
    procedure ExecuteCML;

    //Execute the propagation of values of networks saving temporary data
    function ExecuteSaving : string;

    //Execute the backpropagation of values of Elman networks
    procedure BackExecuteElman;

    //Execute the backpropagation of values of SCTL networks
    procedure BackExecuteSCTL;

    //Execute the backpropagation of values of CILP networks
    procedure BackExecuteCILP;

    //Execute the backpropagation of values of CML networks
    procedure BackExecuteCML;

    //Execute the backpropagation of values of networks saving temporary data
    procedure BackExecuteSaving;


    //Executes the training of an epoch
    //@param(F: File with the training set)
    //@param(SelectiveMSE: Indicates if only the selected pattens are used to calculate the error)
    //return(The mean square error on the training)
    function  EpochTr(var F : TextFile; SelectiveMSE, SelTr : boolean; svFileName : string = '') : double;

    //Executes the test of an epoch
    //@param(F: File with the test set)
    //@param(SelectiveMSE: Indicates if only the selected pattens are used to calculate the error)
    //return(The mean square error on the test)
    function  EpochTs(var F : TextFile; SelectiveMSE : boolean; svFileName : string = '') : double;

    //Executes the training of an epoch
    //@param(F: File with the training set)
    //@param(SelectiveMSE: Indicates if only the selected pattens are used to calculate the error)
    //return(The mean square error on the training)
    function  NewEpochTr(F : TFileName; SelectiveMSE, SelTr, randGroup : boolean; svFileName : string = '') : double;

    //Executes the test of an epoch
    //@param(F: File with the test set)
    //@param(SelectiveMSE: Indicates if only the selected pattens are used to calculate the error)
    //return(The mean square error on the test)
    function  NewEpochTs(F : TFileName; SelectiveMSE, diffOrder : boolean; svFileName : string = '') : double;

    //Read method for the property LinkFlow
    function GetLinkFlow: TLinkFlow;

    //Write method for the property LinkFlow
    procedure SetLinkFlow(const Value: TLinkFlow);

    //Read method for the property Nu
    function GetNu: integer;

    //Write method for the property Nu
    procedure SetNu(const Value: integer);

    //Read method for the property WorldCount
    function GetWorldCount: integer;

    //Write method for the property Worldcount
    procedure SetWorldCount(const Value: integer);

    //Read method for the property Upsilon
    function GetUpsilon: integer;

    //Write method for the property Upsilon
    procedure SetUpsilon(const Value: integer);

  public
    //Constructor of the class
    constructor Create{(PNet : TNetworkRep)};

    //destructor of the class
    destructor  Destroy; override;

    //Flow of values on the link points
    property LinkFlow : TLinkFlow read GetLinkFlow write SetLinkFlow;

    //Number of FeedForward executions
    property Nu : integer read GetNu write SetNu;

    //Number of backpropagation steps per time point (simmilar to nu)
    property Upsilon : integer read GetUpsilon write SetUpsilon;

    //Number of Worlds in a CML-like Network
    property WorldCount : integer read GetWorldCount write SetWorldCount;

    //Clear the array with delay functions
    procedure   ClearDelayFunctions;

    //Clear the arrays with activation functions and their derivates
    procedure   ClearRealFunctions;

    //Add a delay function
    //@param(Function to be added)
    procedure   AddDelayFunction(func : TDelayFunction);

    //Initializes a training epoch the network
    procedure InitializeEpoch;

    //Add an activation function
    //@param(Function to be added)
    //@param(Derivate of the function)
    procedure   AddRealFunction(func, deriv : TParamFunction);

    //Load the network from a description
    //@param(desc: Description to be loaded)
    //return(true if the load was suceeded)
    function    LoadDescription(desc : TNetworkRep) : boolean;

    //Save the network to a description
    //@param(desc : description to be saved)
    //@return(true if the save was suceeded)
    function    SaveDescription(desc : TNetworkRep) : boolean;

    //procedure to load the weights configuration that was saved
    procedure LoadBestEpoch;

    //Reset all the delay units
    procedure   ResetDelays;

    //Reset all the units on the network
    procedure ResetNetwork(InitializeWeights : boolean = true);

    //Routine to execute a training
    //@param(TrFile: File with the training set)
    //@param(TsFile: File with the test set)
    //@param(RelatFile: File to generate a report)
    //@param(MaxEp: Max number of epochs)
    //@param(CCount: Count of the current validation)
    //@param(CTimes: number of validations to be performed)
    //@param(SelectiveMSE: Indicates if only the selected pattens are used to calculate the error)
    function    Training(TrFile, TsFile, RelatFile: TFileName; MaxEp, CCount, CTimes : integer;
                         SelectiveMSE, SelTr, BestTr : boolean) : integer;

    //New Routine to execute a training
    //@param(TrFile: File with the training set)
    //@param(TsFile: File with the test set)
    //@param(RelatFile: File to generate a report)
    //@param(MaxEp: Max number of epochs)
    //@param(CCount: Count of the current validation)
    //@param(CTimes: number of validations to be performed)
    //@param(SelectiveMSE: Indicates if only the selected pattens are used to calculate the error)
    //@param(SelTr: Indicates if the training should be selective)
    //@param(BestTr: Indicates if the best epoch in training should be kept (instead of testing))
    //@param(RandGroup: indicates if the groups of patterns should be randomized)
    function NewTraining(TrFile, TsFile, RelatFile: TFileName; MaxEp, CCount, CTimes : integer;
                         SelectiveMSE, SelTr, BestTr, RandGroup: boolean) : integer;


{    procedure GenerateCentreFile(OriginalFileName, NewFileName : TFileName;
                                OldAux, NeuAux : array of double);}


    //Routine to execute a checking
    //@param(ChkFile: File with the checking set)
    //@param(RelatFile: File to generate a report)
    procedure   Checking(ChkFile, RelatFile : TFileName; CorrInt : double = 0.5);

    function NewChecking(ChkFile, RelatFile : TFileName; SelectiveMSE, diffOrder : boolean; CorrInt : double = 0.5) : integer;

    //Changes the InMin property of the input units
    //@param(newValues: New values to be assigned to the properties)
    procedure ChangeInputMin(newValues : array of double);

    //Changes the InMan property of the input units
    //@param(newValues: New values to be assigned to the properties)
    procedure ChangeInputMax(newValues : array of double);

    //Changes the OutMin property of the input units
    //@param(newValues: New values to be assigned to the properties)
    procedure ChangeOutputMin(newValues : array of double);

    //Changes the OutMax property of the input units
    //@param(newValues: New values to be assigned to the properties)
    procedure ChangeOutputMax(newValues : array of double);

    //Applies an array on the input of the network
    //@param(ArInp: Array of data to apply on the network)
    //@param(Size: Size of the array)
    //@return('!' if incorrect, the output of linkpoints if saving, '' in other cases)
    function ApplyInputArray(ArInput : array of double; Size : integer) : string;

    function ApplyOutputArray(ArInput : array of double; Size : integer) : boolean;

    //Gets an array of values of the output of the network
    //@param(ArInp(out): Array of data to fill with the output data)
    //@param(Size: Size of the array)
    //@return(true if the function was correctly executed)
    function GetOutputArray(out ArOutput : array of double; Size : integer) : boolean;

    //Gets an array of the values
    function GetLinkValuesArray(out ArOutput: array of double; level, size: integer): boolean;

    //Sets the network mode (training, test or check);
    procedure SetMode(NewMode : TNetworkMode);

    //Test Function
    procedure GetNeuronActivation(x : TStringList);

    //Correct the value of Eta for all neurons
    procedure CorrectEta;

    //Change the value of Eta for all neurons
    procedure ChangeEta(value : double);


    //Functions to use array of weights....
    function GetWeightCount : integer;

    function GetWeightArray(var ar : array of double; size : integer) : boolean;

  end;

{------------------------------------------------------------------------------}

implementation

uses UnDialTrEvol, Forms, UnRafaAux2007;

{------------------------------------------------------------------------------}

{ TInputUnit }

{------------------------------------------------------------------------------}

procedure TInputUnit.AddOutput(index: integer; PUnit: TRafaItem);
  begin
  FOutputConnections := FOutPutConnections + 1;
  SetLength(Foutputs, FOutputConnections);
  FOutputs[FOutputConnections - 1] := PUnit;
  SetLength(FoutputArcsIndex, FOutputConnections);
  FOutputArcsIndex[FOutputConnections - 1] := index;
  end;


{------------------------------------------------------------------------------}

procedure TInputUnit.BackInput(Pdelta, Pweight: double; Sender: TRafaItem);
  begin
  inherited;
  end;

{------------------------------------------------------------------------------}

constructor TInputUnit.Create(PRep: TIOUnitRep);
  begin
  inherited Create;
  FInMax := PRep.InMax;
  FInMin := PRep.InMin;
  FOutMax := PRep.OutMax;
  FOutMin := PRep.OutMin;
  end;

{------------------------------------------------------------------------------}

function TInputUnit.GetInMax: double;
  begin
  result := FInMax;
  end;

{------------------------------------------------------------------------------}

function TInputUnit.GetInMin: double;
  begin
  result := FInMin;
  end;

{------------------------------------------------------------------------------}

function TInputUnit.GetOutMax: double;
  begin
  result := FOutMax;
  end;

{------------------------------------------------------------------------------}

function TInputUnit.GetOutMin: double;
  begin
  result := FOutMin;
  end;

{------------------------------------------------------------------------------}

function TInputUnit.GetWeightByArcIndex(arcIndex: integer): double;
  begin
  result := 0;
  end;

{------------------------------------------------------------------------------}

procedure TInputUnit.Initialize;
  begin
  inherited;
  end;

{------------------------------------------------------------------------------}

procedure TInputUnit.InputData(data: double; Sender: TRafaItem);
  var
    i : integer;
    d : double;
  begin
  inherited;
  //Passa para o intervalo de [0..1]
  d := (data - FInMin) / (FInMax - FInMin);
  //Passa para o intervalo de [FOutMin..FOutMax]
  d := (d * (FOutMax - FOutMin)) + FOutMin;
  for i := 0 to FOutputConnections - 1 do
    FOutputs[i].InputData(d, self);
  end;

{------------------------------------------------------------------------------}

procedure TInputUnit.SetInMax(const Value: double);
  begin
  FInMax := value;
  end;

{------------------------------------------------------------------------------}

procedure TInputUnit.SetInMin(const Value: double);
  begin
  FInMin := value;
  end;

{------------------------------------------------------------------------------}

procedure TInputUnit.SetOutMax(const Value: double);
  begin
  FOutMax := Value;
  end;

{------------------------------------------------------------------------------}

procedure TInputUnit.SetOutMin(const Value: double);
  begin
  FOutMin := value;
  end;

{------------------------------------------------------------------------------}

{ TOutputUnit }

{------------------------------------------------------------------------------}

procedure TOutputUnit.BackInput(Pdelta, Pweight: double; Sender: TRafaItem);
  begin
  inherited;
  FDesiredValue := (PDelta - FOutMin) / (FOutMax - FOutMin);
  FDesiredValue :=  (FDesiredValue * (FInMax - FInMin)) + FInMin;
  FGetDesired := true;
  exec;
  end;

{------------------------------------------------------------------------------}

constructor TOutputUnit.Create(PRep: TIOUnitRep);
  begin
  inherited Create;
  FGetDesired := false;
  FGetObtained := false;

  FInMax := PRep.InMax;
  FInMin := PRep.InMin;
  FOutMax := PRep.OutMax;
  FOutMin := PRep.OutMin;

  FMode := NmTraining;
  end;

{------------------------------------------------------------------------------}

procedure TOutputUnit.DefineUnit(index: integer; PUnit: TRafaItem);
  begin
  FUnit := PUnit;
  FArcIndex := index;
  end;

{------------------------------------------------------------------------------}

procedure TOutputUnit.exec;
  begin
  if FGetObtained and FGetDesired then
     begin
     FGetObtained := false;
     FGetDesired  := false;
     if (FMode = NMTraining) then
        FUnit.BackInput(FDesiredValue - FObtainedValue, 1, self);
     end

  end;


{------------------------------------------------------------------------------}

function TOutputUnit.GetDesiredValue: double;
  var
    tmp : double;
  begin
  tmp    := (FDesiredValue - FInMin) / (FInMax - FInMin);
  result := (tmp * (FOutMax - FOutMin)) + FOutMin;
  end;

{------------------------------------------------------------------------------}

function TOutputUnit.GetInMax: double;
  begin
  result := FInMax;
  end;

{------------------------------------------------------------------------------}

function TOutputUnit.GetInMin: double;
  begin
  result := FInMin;
  end;

{------------------------------------------------------------------------------}

function TOutputUnit.GetMode: TNetworkMode;
  begin
  result := FMode;
  end;

{------------------------------------------------------------------------------}

function TOutputUnit.GetObtainedValue: double;
  var
    tmp : double;
  begin
  tmp    := (FObtainedValue - FInMin) / (FInMax - FInMin);
  result := (tmp * (FOutMax - FOutMin)) + FOutMin;
  end;

{------------------------------------------------------------------------------}

function TOutputUnit.GetOutMax: double;
  begin
  result := FOutMax;
  end;

{------------------------------------------------------------------------------}

function TOutputUnit.GetOutMin: double;
  begin
  result := FOutMin;
  end;

{------------------------------------------------------------------------------}

function TOutputUnit.GetWeightByArcIndex(arcIndex: integer): double;
  begin
  result := 0;
  end;

{------------------------------------------------------------------------------}

procedure TOutputUnit.Initialize;
  begin
  inherited;
  FGetDesired := false;
  FGetObtained := false;
  end;

{------------------------------------------------------------------------------}

procedure TOutputUnit.InputData(data: double; Sender: TRafaItem);
  begin
  inherited;
  FObtainedValue := data;
  FgetObtained := true;
  exec;
  //???
  end;

{------------------------------------------------------------------------------}

procedure TOutputUnit.SetInMax(const Value: double);
  begin
  FInMax := Value;
  end;

{------------------------------------------------------------------------------}

procedure TOutputUnit.SetInMin(const Value: double);
  begin
  FInMin := value;
  end;

{------------------------------------------------------------------------------}

procedure TOutputUnit.SetMode(value: TNetworkMode);
  begin
  FMode := value;
  end;

{------------------------------------------------------------------------------}

procedure TOutputUnit.SetOutMax(const Value: double);
  begin
  FOutMax := Value
  end;

{------------------------------------------------------------------------------}

procedure TOutputUnit.SetOutMin(const Value: double);
  begin
  FOUtMin := Value;
  end;

{------------------------------------------------------------------------------}

{ TNeuron }

{------------------------------------------------------------------------------}

function TNeuron.Activate: double;
  var
    i : integer;
  begin
  FSum := FExternalBias * FWeights[0];
  for i := 1 to FInputConnections do
    FSum := FSum + (FInput[i - 1] * FWeights[i]);
  FOutput := FActivation(FSum, FParams);

  for i := 0 to FOutputConnections - 1 do
    FOutputUnits[i].InputData(FOutput, self);
  result := FOutput;
  end;

{------------------------------------------------------------------------------}

procedure TNeuron.AddInput(index: integer; Punit: TRafaItem;
  PWeight: double; fix: boolean; ran : double);
  begin
  FInputConnections  := FInputConnections + 1;
  SetLength(FInput, FinputConnections);
  SetLength(FWeights, FinputConnections + 1);
  SetLength(FLastDeltaWeights, FinputConnections + 1);
  SetLength(FFixedWeights, FinputConnections + 1);
  SetLength(FRandomWeights, FinputConnections + 1);
  SetLength(FDefaultWeights, FinputConnections + 1);
  SetLength(FFlagInputs, FinputConnections);
  SetLength(FInputArcsIndex, FinputConnections);
  SetLength(FInputUnits, FinputConnections);
  FDefaultWeights[FInputConnections] := PWeight;
  FFlagInputs[FInputConnections - 1] := false;
  FInputArcsIndex[FInputConnections - 1] := index;
  FInputUnits[FInputConnections - 1] := PUnit;
  FfixedWeights[FInputConnections] := fix;
  FrandomWeights[FInputConnections] := ran;
  end;

{------------------------------------------------------------------------------}

procedure TNeuron.AddOutput(index: integer; Punit: TRafaItem);
  begin
  FOutputConnections  := FOutputConnections + 1;
  SetLength(FFlagOutputs, FOutputConnections);
  SetLength(FOutputUnits, FOutputConnections);
  SetLength(FOutputArcsIndex, FOutputConnections);
  SetLength(FErrors, FOutputConnections);
  FFlagOutputs[FOutputConnections - 1] := false;
  FOutputArcsIndex[FOutputConnections - 1] := index;
  FOutputUnits[FOutputConnections - 1] := PUnit;
  end;

{------------------------------------------------------------------------------}

procedure TNeuron.BackInput(Pdelta, Pweight: double; Sender: TRafaItem);
  var
    i, ind : integer;
  begin
  inherited;
  ind := GetOutputIndex(sender);
  if not FFlagOutputs[ind] then
     begin
     FFlagOutputs[ind] := true;
     FReceivedOutputs := FReceivedOutputs + 1;
     FErrors[ind] := PDelta * PWeight;

     if FReceivedOutputs = FOutputConnections then
        begin
        for i := 0 to FOutputConnections - 1 do
          FFlagOutputs[i] := false;
        FReceivedOutputs := 0;
        BackProc;
        end;
     end;
  end;


{------------------------------------------------------------------------------}

procedure TNeuron.BackProc;
  var
    i : integer;
    tmpValue : double;
  begin
  FSumErrors := 0;
  for i := 0 to FOutputConnections - 1 do
    FSumErrors := FSumErrors + FErrors[i];
  FDelta := FDerivated(FSum, FParams) * FSumErrors;
  for i := 0 to FInputConnections - 1 do
    begin
    FInputUnits[i].BackInput(FDelta, FWeights[i + 1], self);
    if not FFixedWeights[i + 1] then
       begin
       tmpValue := ((1 - FMomentum) * (FEta * FDelta * FInput[i])) + (FLastDeltaWeights[i] * FMomentum);
       FWeights[i + 1] := FWeights[i + 1] + tmpValue;
       FLastDeltaWeights[i] := tmpValue;
       end;
    end;
  if not FFixedWeights[0] then
     begin
     tmpValue := ((1 - FMomentum) * (FEta * FDelta * FExternalBias{=1})) + (FLastDeltaWeights[0] * FMomentum);
     FWeights[0] := FWeights[0] + tmpValue;
     FLastDeltaWeights[0] := tmpValue;
     end;
  end;

{------------------------------------------------------------------------------}

procedure TNeuron.CorrectEta(value: double);
  begin
  FEta := FEta * value;
  end;

{------------------------------------------------------------------------------}

constructor TNeuron.Create(PRep: TNeuronRep; PAtiv, PDeriv: array of TParamFunction; HistSize : integer = 1);
  var
    i : integer;
  begin
  inherited create;
  FEta := PRep.Eta;
  FMomentum := PRep.Momentum;
  FActivation   := PAtiv[PRep.AtivFunction];
  FDerivated    := PDeriv[PRep.AtivFunction];

  FInputConnections := 0;
  FOutputConnections := 0;

  FReceivedInputs := 0;
  FReceivedOutputs := 0;
  FSum := 0;
  FSumErrors := 0;

  FParamCount := PRep.ParamCount;
  SetLength(FParams, FParamCount);
  for i := 0 to FParamCount - 1 do
    FParams[i] := PRep.Param[i];
  SetLength(FWeights, 1);
  SetLength(FLastDeltaWeights, 1);
  SetLength(FRandomWeights, 1);
  SetLength(FFixedWeights, 1);
  SetLength(FDefaultWeights, 1);

  FDefaultWeights[0] := PRep.BiasWeight;
  FRandomWeights[0] := PRep.RandomBias;
  FFixedWeights[0] := PRep.FixedBias;

  if PRep.HasBias then
     FExternalBias := 1
  else
     FExternalBias := 0;

  FDelta := 0;
  end;

{------------------------------------------------------------------------------}

function TNeuron.GetBiasWeight: double;
  begin
  result := FWeights[0];
  end;

{------------------------------------------------------------------------------}

function TNeuron.getEta: double;
  begin
  result := FEta;
  end;

{------------------------------------------------------------------------------}

function TNeuron.GetInputIndex(un: TRafaItem): integer;
  var
    i : integer;
  begin
    i := 0;
    while (i < FInputConnections) and (FInputUnits[i] <> un) do
      i := i + 1;
    if FInputUnits[i] = un then
       result := i
    else
       result := -1;
  end;

{------------------------------------------------------------------------------}

function TNeuron.GetOutputIndex(un: TRafaItem): integer;
  var
    i : integer;
  begin
    i := 0;
    while (i < FOutputConnections) and (FOutputUnits[i] <> un) do
      i := i + 1;
    if FOutputUnits[i] = un then
       result := i
    else
       result := -1;
  end;

{------------------------------------------------------------------------------}

function TNeuron.GetWeightByArcIndex(arcIndex: integer): double;
  var
    i : integer;
    b : boolean;
  begin
  i := 0;
  b := false;
  while (not b) and (i < FInputConnections) do
    begin
    if FInputArcsIndex[i] = ArcIndex then
       b := true;
    i := i + 1;
    end;
  if b then
     result := FWeights[i]
  else
     result := 0;
  end;

{------------------------------------------------------------------------------}

procedure TNeuron.Initialize;
  var i : integer;
  begin
  inherited;
  for i := 0 to FInputConnections do
    FWeights[i] := FDefaultWeights[i] + (random - 0.5) * (2 * FRandomWeights[i]);   //???
  end;

{------------------------------------------------------------------------------}

procedure TNeuron.InputData(data: double; Sender: TRafaItem);
  var
    i, ind : integer;
  begin
  inherited;
  ind := GetInputIndex(Sender); //???
  if not FFlagInputs[ind] then
     begin
     FFlagInputs[ind] := true;
     FReceivedInputs := FReceivedInputs + 1;
     FInput[ind] := data;
     if FReceivedInputs = FInputConnections then
        begin
        for i := 0 to FInputConnections - 1 do
          FFlagInputs[i] := false;
        FReceivedInputs := 0;
        Activate;
        end;
     end;
  end;

{------------------------------------------------------------------------------}

procedure TNeuron.LoadWeightsArray(WAr: array of double; pos: integer);
  var i : integer;
  begin
  for i := 0 to FInputConnections do
    if not FFixedWeights[i] and ((pos + i) < length(WAr)) then
       FWeights[i] := WAr[pos + i];
  end;

{------------------------------------------------------------------------------}

procedure TNeuron.SaveWeightsArray(var WAr: array of double; pos: integer);
  var i : integer;
  begin
  for i := 0 to FInputConnections do
    WAr[pos + i] := FWeights[i];
  end;

{------------------------------------------------------------------------------}

procedure TNeuron.setEta(value: double);
  begin
  FEta := value;
  end;


{------------------------------------------------------------------------------}

{ TDelayUnit }

{------------------------------------------------------------------------------}

procedure TDelayUnit.AddOutput(index: integer; PUnit: TRafaItem);
  begin
  FOutputConnections := FOutputConnections + 1;
  SetLength(FOutputArcsIndex, FOutputConnections);
  SetLength(FOutputUnits, FOutputConnections);
  FOutputArcsIndex[FOutputConnections - 1] := index;
  FOutputUnits[FOutputConnections - 1] := PUnit;
  end;

{------------------------------------------------------------------------------}

procedure TDelayUnit.BackExecution(value: double);
  var
    i : integer;
    XoutValue : double;
  begin
  if FFilledDelta < FTimes then
     FFilledDelta := FFilledDelta + 1;
  for i := 0 to FTimes - 2 do
    FDelta[i + 1] := FDelta[i];
  FDelta[0] := value;
  XoutValue := 0;
  if FFilledDelta >= FTimes then
     for i := 0 to FTimes - 1 do
       XoutValue := XoutValue + FDelayFunction(i, FDelta[i]);
  FInputUnit.BackInput(FDeltaOutput, 1, self);
  for i := 0 to FOutputConnections - 1 do
    FOutputFlag[i] := false;
  FReceivedOutputs := 0;
  FDeltaOutput := 0;
  end;

{------------------------------------------------------------------------------}

procedure TDelayUnit.BackInput(Pdelta, Pweight: double; Sender: TRafaItem);
  var
    ind : integer;
  begin
  //This is merely making a pondered sum of the received delta.
  inherited;
  ind := GetOutputIndex(Sender);
  if (ind >= 0) and not FOutputFlag[ind] then
     begin
     FDeltaOutput := FDeltaOutput + (PDelta * PWeight);
     FOutputFlag[ind] := true;
     FReceivedOutputs := FReceivedOutputs + 1;
     if FReceivedOutputs >= FOutputConnections then
        BackExecution(FDeltaOutput);
     end;
  end;

{------------------------------------------------------------------------------}

constructor TDelayUnit.Create(PRep: TDelayRep; PFunc: array of TDelayFunction);
  begin
  inherited Create;
  FTimes := PRep.Times;
  SetLength(FInput, FTimes);
  SetLength(FDelta, FTimes);
  FFilledDelta := 0;
  FFilledInputs := 0;
  FReceivedOutputs := 0;
  FDelayFunction := PFunc[PRep.DelayFunction];
  end;

{------------------------------------------------------------------------------}

procedure TDelayUnit.DefineInput(index: integer; PUnit: TRafaItem);
  begin
  FInputUnit := PUnit;
  FInputArcIndex := index;
  end;

{------------------------------------------------------------------------------}

function TDelayUnit.GetOutputIndex(un: TRafaItem): integer;
  var
    i : integer;
  begin
    i := 0;
    while (i < FOutputConnections) and (FOutputUnits[i] <> un) do
      i := i + 1;
    if FOutputUnits[i] = un then
       result := i
    else
       result := -1;
  end;

{------------------------------------------------------------------------------}

function TDelayUnit.GetWeightByArcIndex(arcIndex: integer): double;
  begin
  result := 1;
  end;

{------------------------------------------------------------------------------}

procedure TDelayUnit.Initialize;
  var i : integer;
  begin
  inherited;
  ResetTimes;
  for i := 0 to FOutputConnections - 1 do
    FOutputFlag[i] := false;
  end;

{------------------------------------------------------------------------------}

procedure TDelayUnit.InputData(data: double; Sender: TRafaItem);
  var i : integer;
  begin
  inherited;
  if Sender = FInputUnit then
     begin
     if FFilledInputs < FTimes then
        FFilledInputs := FFilledInputs + 1;
     for i := 0 to FTimes - 2 do
       FInput[i + 1] := FInput[i];
     Finput[0] := data;
     FOutput := 0;
     if FFilledInputs >= FTimes then
        for i := 0 to FTimes - 1 do
          FOutput := FOutput + FDelayFunction(i, FInput[i]);
     for i := 0 to FOutputConnections - 1 do
       FOutputUnits[i].InputData(FOutput, Self);
     end;
  end;

{------------------------------------------------------------------------------}

procedure TDelayUnit.ResetTimes;
  begin
  FFilledDelta := 0;
  FFilledInputs := 0;
  FReceivedOutputs := 0;
  end;

{------------------------------------------------------------------------------}

{ TLinkPoint }

{------------------------------------------------------------------------------}

procedure TLinkPoint.AddOutput(Punit: TRafaItem);
  begin
  FOutputConnections := FOutputConnections + 1;
  SetLength(FOutputUnits, FOutputConnections);
  SetLength(FOutputMarks, FOutputConnections);
  FOutputUnits[FOutputConnections - 1] := PUnit;
  end;

{------------------------------------------------------------------------------}

procedure TLinkPoint.BackInput(Pdelta, Pweight: double; Sender: TRafaItem);
  begin
  inherited;
  Case FLinkBackStyle of
    LbsOutput :
      begin
      if Sender is TOutputUnit then
         begin
         FOldBackValue := FOutputBackValue;
         FInputBackValue := PDelta * PWeight;
         end;
      if not FBackTriggered then
         BackTrigger;
      end;
    LbsZero :
      begin
      FOldBackValue := FOutputBackValue;
      FInputBackValue := 0;
      if not FBackTriggered then
         BackTrigger;
      end;
    LbsEach :
      begin
      FOldBackValue := FOutputBackValue;
      FInputBackValue := PDelta;
      if not FBackTriggered then
         BackTrigger;
      end;
    LbsAllZero :
      begin
      if not FOutputMarks[GetOutputIndex(Sender)] then
        begin
        FOutputMarks[GetOutputIndex(Sender)] := true;
        FOutputCount := FOutputCount + 1;
        if FOutputCount = FOutputConnections then
           begin
           FOldBackValue := FOutputBackValue;
           FInputBackValue := 0;
           if not FBackTriggered then
              BackTrigger;
           end;
        end;
      end;
    LbsAllMean :
      begin
      if not FOutputMarks[GetOutputIndex(Sender)] then
        begin
        FOutputMarks[GetOutputIndex(Sender)] := true;
        FOutputCount := FOutputCount + 1;
        FoutputSum   := FOutputSum + (PDelta * PWeight);
        if FOutputCount = FOutputConnections then
           begin
           FOldBackValue := FOutputBackValue;
           FInputBackValue := FOutputSum / FOutputConnections;
           if not FBackTriggered then
              BackTrigger;
           FOutputSum := 0;
           end;
        end;
      end;
    end;
  end;

{------------------------------------------------------------------------------}

procedure TLinkPoint.BackTrigger(Kind : char = 'a'; value : integer = 0);
  var i : integer;
  begin
  if (kind = 'a') then
     begin
     for i := 0 to FOutputConnections - 1 do
       FOutputMarks[i] := false;
     FOutputCount := 0;
//     FOldBackValue := FBackValue;
     FinputUnit.BackInput(FOutputBackValue, 1, self);
     end
  else if (Kind = 'o') then
     begin
     for i := 0 to FOutputConnections - 1 do
       FOutputMarks[i] := false;
     FOutputCount := 0;
     FinputUnit.BackInput(FOldBackValue, 1, self);
     end
  else
     begin
     for i := 0 to FOutputConnections - 1 do
       FOutputMarks[i] := false;
     FOutputCount := 0;
     FinputUnit.BackInput(value, 1, self);
     end;
  end;

{------------------------------------------------------------------------------}

constructor TLinkPoint.Create(orig: TRecLinkRep);
  begin
  inherited Create;
  FLevel := orig.Level;
  FStable := false;
  FReceived := false;
  FLinkBackStyle := orig.LinkBackStyle;
  FInitValue := orig.InitValue;
  FOutputConnections := 0;
  FInputValue := 0;
  FOutputValue := 0;
  FOldValue := 0;
  FInputBackValue := 0;
  FOutputBackValue := 0;
  FOldBackValue := 0;
  FTriggered := false;
  FBackTriggered := false;
  end;

{------------------------------------------------------------------------------}

function TLinkPoint.GetBackTriggered: boolean;
  begin
  result := FBackTriggered;
  end;

{------------------------------------------------------------------------------}

function TLinkPoint.GetInitValue: double;
  begin
  result := FInitValue;
  end;

{------------------------------------------------------------------------------}

function TLinkPoint.GetinputValue: double;
  begin
  result := FInputValue;
  end;

{------------------------------------------------------------------------------}

function TLinkPoint.GetLevel: integer;
  begin
  result := FLevel;
  end;

{------------------------------------------------------------------------------}

function TLinkPoint.GetOutputIndex(item: TRafaItem): integer;
  var
    i : integer;
  begin
    i := 0;
    while (i < FOutputConnections) and (FOutputUnits[i] <> item) do
      i := i + 1;
    if FOutputUnits[i] = item then
       result := i
    else
       result := -1;
  end;

{------------------------------------------------------------------------------}

function TLinkPoint.GetOutputValue: double;
  begin
  result := FOutputValue;
  end;

{------------------------------------------------------------------------------}

function TLinkPoint.GetReceived: boolean;
  begin
  result := FReceived;
  end;

{------------------------------------------------------------------------------}

function TLinkPoint.GetTriggered: boolean;
  begin
  result := FTriggered;
  end;

{------------------------------------------------------------------------------}

function TLinkPoint.GetWeightByArcIndex(arcIndex: integer): double;
  begin
  result := 1;
  end;

{------------------------------------------------------------------------------}

procedure TLinkPoint.Initialize;
  begin
  inherited;
  //???
  end;

{------------------------------------------------------------------------------}

procedure TLinkPoint.InputData(data: double; Sender: TRafaItem);
  var i : integer;
  begin
  inherited;
  if not FTriggered then
     begin
     FOutputValue := data;
     for i := 0 to FOutputConnections - 1 do
       FOutputUnits[i].InputData(data, self);
     end;
  FInputValue := data;
  FReceived := true;
  end;

{------------------------------------------------------------------------------}

procedure TLinkPoint.PreBackTrigger;
  begin
  FOutputBackValue := FInputBackvalue;
  end;

{------------------------------------------------------------------------------}

procedure TLinkPoint.PreTrigger;
  begin
  FOutputValue := FInputValue;
  end;

{------------------------------------------------------------------------------}

procedure TLinkPoint.SetBackTriggered(const Value: boolean);
  begin
  FBackTriggered := value;
  end;

{------------------------------------------------------------------------------}

procedure TLinkPoint.SetInitValue(const Value: double);
  begin
  InitValue := value;
  end;

{------------------------------------------------------------------------------}

procedure TLinkPoint.SetInput(Punit: TRafaItem);
  begin
  FInputUnit := PUnit;
  end;

{------------------------------------------------------------------------------}

procedure TLinkPoint.SetLevel(const Value: integer);
  begin
  FLevel := value;
  end;

{------------------------------------------------------------------------------}

procedure TLinkPoint.SetReceived(const Value: boolean);
  begin
  FReceived := value;
  end;

{------------------------------------------------------------------------------}

procedure TLinkPoint.SetTriggered(const Value: boolean);
  begin
  FTriggered := Value;
  end;

{------------------------------------------------------------------------------}

procedure TLinkPoint.Trigger(Kind : char = 'a'; value : integer = 0);
  var i : integer;
  begin
  if (Kind = 'a') then
     begin
     FReceived := false;
     FOldValue := FOutputValue;
     for i := 0 to FOutputConnections - 1 do
       FOutputUnits[i].InputData(FOutputValue, self);
     end
  else if (Kind = 'o') then
     begin
     FReceived := false;
     for i := 0 to FOutputConnections - 1 do
       FOutputUnits[i].InputData(FOldValue, self);
     end
  else
     begin
     FReceived := false;
     for i := 0 to FOutputConnections - 1 do
       FOutputUnits[i].InputData(value, self);
     end
  end;

{------------------------------------------------------------------------------}

{ TRafaANN }

{------------------------------------------------------------------------------}

procedure TRafaANN.AddDelayFunction(func: TDelayFunction);
  begin
  FNoDelayF := FNoDelayF + 1;
  SetLength(FDelayFuncs, FNoDelayF);
  FDelayFuncs[FNoDelayF - 1] := func;
  end;

{------------------------------------------------------------------------------}

procedure TRafaANN.AddRealFunction(func, deriv: TParamFunction);
  begin
  FNoRealF := FNoRealF + 1;
  SetLength(FRealFuncs, FNoRealF);
  SetLength(FDerivFuncs, FNoRealF);
  FRealFuncs[FNoRealF - 1] := func;
  FDerivFuncs[FNoRealF - 1] := deriv;
  end;

{------------------------------------------------------------------------------}

procedure TRafaANN.AddUnit(pos: integer; PUnit: TUnitRep);
  var lv : integer;
  begin
  case PUnit.Kind of
    UKNeuron : FUnits[pos] := TNeuron.Create(PUnit as TNeuronRep, FRealFuncs, FDerivFuncs);
    UKDelay  : FUnits[pos] := TDelayUnit.Create(PUnit as TDelayRep, FDelayFuncs);
    UKInput  :
      begin
      FUnits[pos] := TInputUnit.Create (PUnit as TIOUnitRep);
      FNoInputs := FNoInputs + 1;
      SetLength(FInputUnits, FnoInputs);
      FInputUnits[FNoInputs - 1] := pos;
      end;
    UKOutput :
      begin
      FUnits[pos] := TOutputUnit.Create(PUnit as TIOUnitRep);
      FNoOutputs := FNoOutputs + 1;
      SetLength(FOutputUnits, FnoOutputs);
      FOutputUnits[FNoOutputs - 1] := pos;
      end;
    UkRec :
      begin
      FUnits[pos] := TLinkPoint.Create(PUnit as TRecLinkRep);
      lv := (FUnits[pos] as TLinkPoint).Level;
      if lv >= FLevelCount then
         begin
         FLevelCount := lv + 1;
         SetLength(FLinksCount, FLevelCount);
         FLinksCount[lv] := 1;
         SetLength(FLinkUnits, FLevelCount);
         SetLength(FLinkUnits[lv], 1);
         FLinkUnits[lv, 0] := pos;
         end
      else
         begin
         FLinksCount[lv] := FLinksCount[lv] + 1;
         SetLength(FLinkUnits[lv], FLinksCount[lv]);
         FLinkUnits[lv, FLinksCount[lv] - 1] := pos;
         end;
      end;
    end;
  end;

{------------------------------------------------------------------------------}

function TRafaANN.ApplyInputArray(ArInput: array of double; Size : integer) : string;
  var
    i : integer;
    s : string;
  begin
  if size = FNoInputs then
     begin
     s := '';
     for i := 0 to Size - 1 do
        FUnits[FInputUnits[i]].InputData(ArInput[i], nil);
     case LinkFlow of
       LfElman  : ExecuteElman;
       LfSCTL   : ExecuteSCTL;
       LfCILP   : ExecuteCILP;
       LfCML    : ExecuteCML;
       LfSaving : s := ExecuteSaving;
       end;
     result := s;
     end
  else
     result := '!';
  end;

{------------------------------------------------------------------------------}

function TRafaANN.ApplyOutputArray(ArInput: array of double;
  Size: integer): boolean;
  var i : integer;
  begin
  if size = FNoOutputs then
     begin
     for i := 0 to Size - 1 do
        FUnits[FOutputUnits[i]].BackInput(ArInput[i], 1, nil);
     case LinkFlow of
       LfElman  : BackExecuteElman;
       LfSCTL   : BackExecuteSCTL;
       LfCILP   : BackExecuteCILP;
       LfCML    : BackExecuteCML;
       LfSaving : BackExecuteSaving;
       end;
     result := true;
     end
  else
     result := false;
  end;

{------------------------------------------------------------------------------}

procedure TRafaANN.BackExecuteCILP;
  var i : integer;
  begin
  PreBackTrigger(2);
  PreBackTrigger(0);
  BackTrigger(2, 'a');
  BackTrigger(0, 'v', 0);
  for i := 0 to FUpsilon - 2 do
    begin
    PreBackTrigger(2);
    PreBackTrigger(0);
    BackTrigger(2, 'v', 0);
    BackTrigger(0, 'a');
    end;
  end;

{------------------------------------------------------------------------------}

procedure TRafaANN.BackExecuteCML;
  var i, j : integer;
  begin
  for j := 0 to FWorldCount - 1 do
    begin
    PreBackTrigger(2);
    PreBackTrigger(1);
    PreBackTrigger(0);
    BackTrigger(2, 'a');
    if j = 0 then
       BackTrigger(1, 'v', 0)
    else
       BackTrigger(1, 'a', 0);
    BackTrigger(0, 'v', 0);
    for i := 0 to FUpsilon - 2 do
      begin
      PreBackTrigger(2);
      PreBackTrigger(1);
      PreBackTrigger(0);
      BackTrigger(2, 'v', 0);
      BackTrigger(1, 'v', 0);
      BackTrigger(0, 'a');
      end;
    end;
  end;

{------------------------------------------------------------------------------}

procedure TRafaANN.BackExecuteElman;
  begin
  //An attempt to verify the difference in Elman performances
  //BackTrigger(0, 'v', 0);
  BackTrigger(0, 'a');
  end;

{------------------------------------------------------------------------------}

procedure TRafaANN.BackExecuteSaving;
  begin
  BackTrigger(0, 'v', 0);
  end;

{------------------------------------------------------------------------------}

procedure TRafaANN.BackExecuteSCTL;
  var i : integer;
  begin
  PreBackTrigger(2);
  PreBackTrigger(1);
  PreBackTrigger(0);
  BackTrigger(2, 'a');
  //An attempt to verify the importance of recurrent Backpropagation values 
  //    BackTrigger(1, 'v', 0);
  BackTrigger(1, 'o');
  BackTrigger(0, 'v', 0);
  for i := 0 to FUpsilon - 2 do
    begin
    PreBackTrigger(2);
    PreBackTrigger(1);
    PreBackTrigger(0);
    BackTrigger(2, 'v', 0);
    BackTrigger(1, 'v', 0);
    BackTrigger(0, 'a');
    end;
  end;

{------------------------------------------------------------------------------}

procedure TRafaANN.BackTrigger(lv : integer; Kind : char = 'a'; value : integer = 0);
  var
    i : integer;
  begin
  if (lv >= 0) and (lv < FLevelCount) then
     for i := 0 to FLinksCount[lv] - 1 do
      (FUnits[FLinkUnits[lv, i]] as TLinkPoint).BackTrigger(kind, value);
  end;

{------------------------------------------------------------------------------}

procedure TRafaANN.ChangeEta(value : double);
  var i : integer;
  begin
  for i := 0 to FNoUnits - 1 do
    if FUnits[i] is TNeuron then
       (FUnits[i] as TNeuron).Eta := value;
  end;

{------------------------------------------------------------------------------}

procedure TRafaANN.ChangeInputMax(newValues: array of double);
   var i : integer;
   begin
   if length(NewValues) = FNoInputs then
      for i := 0 to FNoInputs - 1  do
       (FUnits[FInputUnits[i]] as TInputUnit).InMax := NewValues[i];
   end;

{------------------------------------------------------------------------------}

procedure TRafaANN.ChangeInputMin(newValues: array of double);
   var i : integer;
   begin
   if length(NewValues) = FNoInputs then
      for i := 0 to FNoInputs - 1  do
       (FUnits[FInputUnits[i]] as TInputUnit).InMin := NewValues[i];
   end;

{------------------------------------------------------------------------------}

procedure TRafaANN.ChangeOutputMax(newValues: array of double);
   var i : integer;
   begin
   if length(NewValues) = FNoOutputs then
      for i := 0 to FNoOutputs - 1  do
       (FUnits[FOutputUnits[i]] as TOutputUnit).OutMax := NewValues[i];
   end;

{------------------------------------------------------------------------------}

procedure TRafaANN.ChangeOutputMin(newValues: array of double);
   var i : integer;
   begin
   if length(NewValues) = FNoOutputs then
      for i := 0 to FNoOutputs - 1  do
       (FUnits[FOutputUnits[i]] as TOutputUnit).OutMin := NewValues[i];
   end;

{------------------------------------------------------------------------------}

procedure TRafaANN.Checking(ChkFile, RelatFile: TFileName; CorrInt : double = 0.5);
  var
    F, RF : TextFile;
    Amin, Amax : double;
    i, correct, totality : integer;
    c, c1 : char;
    d : double;
    tmpMSE, tmp : double;
    XSavingStrings : TStringList;

  begin
  Assign(F, ChkFile);
  Assign(RF, RelatFile);
  ResetDelays;
  FCasesCount := 0;
  tmpMSE := 0;
  XSavingStrings := TStringList.Create;

  for i := 0 to FNoOutputs - 1 do
    (FUnits[FOutputUnits[i]] as TOutputUnit).Mode := NMTest;

  reset(F);
  rewrite(RF);
  case FLinkFlow of
    LfElman  : InitializeElman;
    LfCILP   : InitializeCILP;
    LfSCTL   : InitializeSCTL;
    LfCml    : InitializeCML;
    LfSaving : XSavingStrings.Add(InitializeSaving);
    end;

  read(F, c);
  c1 := c;
  read(F, d);
  correct := 0;
  totality := 0;

  while not EOF(F) do
    begin
    if linkflow = LfElman then ExecuteElman;
    if LinkFlow = LfSctl then
       if (c = '-') then
          for i := 0 to FLinksCount[1] - 1 do   //execute the proper treatment ???
            with FUnits[FLinkUnits[1, i]] as TLinkPoint do
              InputData(InitValue, nil);
    i := 0;

    while not EOF(F) and (i < FNoInputs) do
      begin
      FUnits[FInputUnits[i]].InputData(d, nil);
      read(F, d);
      i := i + 1;
      end;

    case LinkFlow of
      LfElman : ExecuteElman;
      LfSCTL : ExecuteSCTL;
      LfCILP : ExecuteCILP;
      LfCML  : ExecuteCML;
      LfSaving : XSavingStrings.Add(ExecuteSaving);
      end;

    i := 0;
    while not EOF(F) and (i < FNoOutputs) do
      begin
      FUnits[FOutputUnits[i]].BackInput(d, 1, nil);
      i := i + 1;
      if i = FNoOutputs then
         begin
         readln(F);
         c1 := c;
         read(F, c);
         read(F, d);
         end
      else
         read(F, D);
      end;

//    write(RF, FCasesCount, '   ');
    tmp := 0;
    for i := 0 to FNoOutputs - 1 do
      with (FUnits[FOutputUnits[i]] as TOutputUnit) do
        writeln(RF, ObtainedValue, '     ', DesiredValue);

    if (c1 = '+') then
       begin
       totality := totality + 1;
       for i := 0 to FNoOutputs - 1 do
         with (FUnits[FOutputUnits[i]] as TOutputUnit) do
            begin
            tmp := tmp + (ObtainedValue - DesiredValue) * (ObtainedValue - DesiredValue);
            Amin := OutMax - ((OutMax - OutMin) * CorrInt);
            Amax := OutMin + ((OutMax - OutMin) * CorrInt);
            if (ObtainedValue >= Amin) and (DesiredValue >= Amin) or
               (ObtainedValue <= Amax) and (DesiredValue <= Amax) then
               correct := correct + 1;
            end;

       tmpMse := tmpMse + (tmp / FNoOutPuts);
       FCasesCount := FCasesCount + 1;
       end;
    writeln(RF);
    end;

  writeln(RF);
  if FCasesCount <> 0 then
     writeln(RF, 'MSE = ', tmpMse/FCasesCount)
  else
     writeln(RF, 'MSE = 0');
  writeln(RF, inttostr(correct) + 'right classifications in a total of ' + inttostr(totality));

  writeLn(RF);
  writeLn(RF);
  writeLn(RF);

  for i := 0 to XSavingStrings.Count - 1 do
    writeLn(RF, XSavingStrings.strings[i]);

  XSavingStrings.Free;

  writeln(RF);
  closeFile(F);
  closeFile(RF);
  end;

{------------------------------------------------------------------------------}

procedure TRafaANN.ClearDelayFunctions;
  begin
  SetLength(FDelayFuncs, 0);
  FNoDelayF := 0;
  end;


{------------------------------------------------------------------------------}

procedure TRafaANN.ClearRealFunctions;
  begin
  SetLength(FRealFuncs, 0);
  SetLength(FDerivFuncs, 0);
  FNoRealF := 0;
  end;

{------------------------------------------------------------------------------}

procedure TRafaANN.CorrectEta;
  var i : integer;
  begin
  for i := 0 to FNoUnits - 1 do
    if FUnits[i] is TNeuron then
       (FUnits[i] as TNeuron).CorrectEta(FEtaCorrection);
  end;

{------------------------------------------------------------------------------}

constructor TRafaANN.Create;
  begin
  inherited Create;
  FNoDelayF := 0;
  FNoRealF := 0;
  FNoInputs := 0;
  FNoOutputs := 0;
  FNoUnits := 0;
  FLevelCount := 0;
  FLinkFlow := LfNone;
  FNu := 1;
  FUpsilon := 1;
  FWorldCount := 0;
  end;

{------------------------------------------------------------------------------}

destructor TRafaANN.Destroy;
  var i : integer;
  begin
  for i := 0 to FNoUnits - 1 do
    FUnits[i].Free;
  inherited;
  end;

{------------------------------------------------------------------------------}

function TRafaANN.EpochTr(var F: TextFile; SelectiveMSE, SelTr: boolean; svFileName : string = ''): double;
  var
    i : integer;
    d : double;
    c, c1 : char;
    tmpMSE, tmp : double;
    XSavingStrings : TStringList;

  begin
  ResetDelays;
  FCasesCount := 0;
  tmpMSE := 0;
  for i := 0 to FNoOutputs - 1 do
    (FUnits[FOutputUnits[i]] as TOutputUnit).Mode := NMTraining;
  reset(F);
  c1 := '*';

  XSavingStrings := TStringList.Create;

  case FLinkFlow of
    LfElman  : InitializeElman;
    LfCILP   : InitializeCILP;
    LfSCTL   : InitializeSCTL;
    LfCML    : InitializeCML;
    LfSaving : XSavingStrings.Add(InitializeSaving);
    end;

  read(F, c);
  read(F, d);
  while not EOF(F) do
    begin
    if linkflow = LfElman then ExecuteElman;
    if LinkFlow = LfSctl then
       if (c = '-') then
          for i := 0 to FLinksCount[1] - 1 do   //execute the proper treatment ???
            with FUnits[FLinkUnits[1, i]] as TLinkPoint do
              InputData(InitValue, nil);
    i := 0;

    while not EOF(F) and (i < FNoInputs) do
      begin
      FUnits[FInputUnits[i]].InputData(d, nil);
      read(F, d);
      i := i + 1;
      end;
    i := 0;

    case LinkFlow of
      LfSCTL   : ExecuteSCTL;
      LfCILP   : ExecuteCILP;
      LfCML    : ExecuteCML;
      LfSaving : XSavingStrings.Add(ExecuteSaving);
      end;

    if LinkFlow = LfElman then BackExecuteElman;

    while not EOF(F) and (i < FNoOutputs) do
      begin
      if SelTr and (c = '-') then
         d := (FUnits[FOutputUnits[i]] as TOutputUnit).ObtainedValue;
      FUnits[FOutputUnits[i]].BackInput(d, 1, nil);
      i := i + 1;
      if i = FNoOutputs then
         begin
         c1 := c;
         readln(F);
         read(F, c);
         read(F, d);
         end
      else
         read(F, D);
      end;

    case LinkFlow of
      LfSCTL   : BackExecuteSCTL;
      LfCILP   : BackExecuteCILP;
      LfCML    : BackExecuteCML;
      LfSaving  : BackExecuteSaving;
      end;


    if not SelectiveMSE or (c1 = '+') then
       if (i = FNoOutputs) then
          begin
          tmp := 0;
          for i := 0 to FNoOutputs - 1 do
            with (FUnits[FOutputUnits[i]] as TOutputUnit) do
              begin
              tmp := tmp + ((ObtainedValue - DesiredValue) * (ObtainedValue - DesiredValue));
              end;
          tmpMse := tmpMse + (tmp / FNoOutPuts);
          FCasesCount := FCasesCount + 1;
          end;
    end;
  closeFile(F);

  if (LinkFlow = LfSaving) and (svFileName <> '') then
     XSavingStrings.SaveToFile(svFileName);

  XSavingStrings.Free;

  result := tmpMse / FCasesCount;
  end;

{------------------------------------------------------------------------------}

function TRafaANN.EpochTs(var F: TextFile; SelectiveMSE: boolean; svFileName : string = ''): double;
  var
    i : integer;
    d : double;
    c, c1 : char;
    tmpMSE, tmp : double;
    XSavingStrings : TStringList;
  begin
  ResetDelays;
  FCasesCount := 0;
  tmpMSE := 0;
  for i := 0 to FNoOutputs - 1 do
    (FUnits[FOutputUnits[i]] as TOutputUnit).Mode := NMTest;
  reset(F);

  XSavingStrings := TStringList.Create;

  case FLinkFlow of
    LfElman : InitializeElman;
    LfCILP  : InitializeCILP;
    LfSCTL  : InitializeSCTL;
    LfCML   : InitializeCML;
    LfSaving : XSavingStrings.Add(InitializeSaving);
    end;

  c1 := '*';
  read (F, c);
  read(F, d);

  while not EOF(F) do
    begin
    if linkflow = LfElman then ExecuteElman;
    if LinkFlow = LfSctl then
       if (c = '-') then
          for i := 0 to FLinksCount[1] - 1 do   //execute the proper treatment ???
            with FUnits[FLinkUnits[1, i]] as TLinkPoint do
              InputData(InitValue, nil);
    i := 0;

    while not EOF(F) and (i < FNoInputs) do
      begin
      FUnits[FInputUnits[i]].InputData(d, nil);
      read(F, d);
      i := i + 1;
      end;
    i := 0;

    case LinkFlow of
      LfSCTL : ExecuteSCTL;
      LfCILP : ExecuteCILP;
      LfCML  : ExecuteCML;
      LfSaving : XSavingStrings.Add(ExecuteSaving);
      end;

    while not EOF(F) and (i < FNoOutputs) do
      begin
      FUnits[FOutputUnits[i]].BackInput(d, 1, nil);
      i := i + 1;
      if i = FNoOutputs then
         begin
         c1 := c;
         readln(F);
         read(F, c);
         read(F, d);
         end
      else
         read(F, D);
      end;

    if not SelectiveMSE or (c1 = '+') then
       if (i = FNoOutputs) then
          begin
          tmp := 0;
          for i := 0 to FNoOutputs - 1 do
            with (FUnits[FOutputUnits[i]] as TOutputUnit) do
              begin
              tmp := tmp + ((ObtainedValue - DesiredValue) * (ObtainedValue - DesiredValue));
              end;
          tmpMse := tmpMse + (tmp / FNoOutPuts);
          FCasesCount := FCasesCount + 1;
          end;
    end;
  closeFile(F);

  if (LinkFlow = LfSaving) and (svFileName <> '') then
     XSavingStrings.SaveToFile(svFileName);

  XSavingStrings.Free;

  if FCasesCount = 0 then
     result := 0
  else
     result := tmpMse / FCasesCount;
  end;

{------------------------------------------------------------------------------}

procedure TRafaANN.ExecuteCILP;
  var i : integer;
  begin
  PreTrigger(0);
  PreTrigger(3);
  Trigger(0, 'a');
  Trigger(3, 'a');
  for i := 0 to FNu - 2 do
    begin
    PreTrigger(0);
    PreTrigger(3);
    Trigger(0, 'a');
    Trigger(3, 'a');
    end;
  PreTrigger(2);
  Trigger(2, 'a');
  end;

{------------------------------------------------------------------------------}

procedure TRafaANN.ExecuteCML;
  var i, j : integer;
  begin
  for j := 0 to FWorldCount - 1 do
    begin
    PreTrigger(0);
    PreTrigger(1);
    PreTrigger(3);
    Trigger(0, 'a');
    Trigger(1, 'a');
    Trigger(3, 'a');
    for i := 1 to FNu - 1 do
      begin
      PreTrigger(0);
      PreTrigger(1);
      PreTrigger(3);
      Trigger(0, 'a');
      Trigger(1, 'o');
      Trigger(3, 'a');
      end;
    end;
  PreTrigger(2);
  Trigger(2, 'a');
  end;

{------------------------------------------------------------------------------}

procedure TRafaANN.ExecuteElman;
  begin
  Trigger(0, 'a');
  end;

{------------------------------------------------------------------------------}

function TRafaANN.ExecuteSaving: string;
  var
    i : integer;
    s : string;
  begin
  s := '';
  if FLevelCount > 0 then
     begin
     for i := 0 to FLinksCount[0] - 2 do
       s := s + floattostr((FUnits[FLinkUnits[0, i]] as TLinkPoint).OutputValue) + #9;
     if FLinksCount[0] > 0 then
       s := s + floattostr((FUnits[FLinkUnits[0, FLinksCount[0] - 1]] as TLinkPoint).OutputValue);
     end;
  result := s;
  end;

{------------------------------------------------------------------------------}

procedure TRafaANN.ExecuteSCTL;
  var i : integer;
  begin
  PreTrigger(0);
  PreTrigger(1);
  PreTrigger(3);
  Trigger(0, 'a');
  Trigger(1, 'a');
  Trigger(3, 'a');
  for i := 1 to FNu - 1 do
    begin
    PreTrigger(0);
    PreTrigger(1);
    PreTrigger(3);
    Trigger(0, 'a');
    Trigger(1, 'o');
    Trigger(3, 'a');
    end;
  PreTrigger(2);
  Trigger(2, 'a');
  end;

{------------------------------------------------------------------------------}

{procedure TRafaANN.GenerateCentreFile(OriginalFileName, NewFileName : TFileName;
                                     OldAux, NeuAux : array of double; nColumns : double);
  var
    XDataFile, XNewFile, XTmpStrList : TStringList;
    i, j, xLine : integer;
    XGroup : TRafaDoubleArray;
  begin
  XDataFile := TStringList.Create;
  XTmpStrList := TStringList.Create;
  XDataFile.LoadFromFile(OriginalFileName);
  i := 0;
  j := 0;
  for xLine := 0 to XDataFile.Count - 1 do
    begin
    XGroup := TRafaDoubleArray.Create();
    if trim(XDataFile.Strings[xLine]) <> '' then  //Temporarily considered the reset condition
       begin
       XTmpStrList.DelimitedText := XDataFile.Strings[XLine];
       i := i + 1;
          SetLength(XArray, i);
          XArray[i - 1] := i - 1;
          end
       else
          begin
          XNewFile.Add(s);
          XNewFile.Add('');
          end;
       end;
     XRandom.Free;
     //*********Remove later:****************
     //XDataFile.SaveToFile(F + 'rand');
     SetLength(XStringArray, 0)
     end;


  end;}

{------------------------------------------------------------------------------}

function TRafaANN.GetLinkFlow: TLinkFlow;
  begin
  result := FLinkFlow;
  end;

{------------------------------------------------------------------------------}

function TRafaANN.GetLinkValuesArray(out ArOutput: array of double; level, size: integer): boolean;
  var i : integer;
  begin
  if (level >= 0) and (level < FLevelCount) then
     begin
     if FLinksCount[level] < size then
        for i := 0 to FLinksCount[level] do
          ArOutput[i] := (FUnits[FLinkUnits[level, i]] as TLinkPoint).OutputValue;
     result := true;
     end
  else
     result := false;
  end;

{------------------------------------------------------------------------------}

procedure TRafaANN.GetNeuronActivation(x : TStringList);
  var i : integer;
  begin
  x.Clear;
  for i := 0 to FNoUnits - 1 do
    begin
    if FUnits[i] is TNeuron then
       x.Add(floattostr((FUnits[i] as Tneuron).FOutput));
    end

  end;

{------------------------------------------------------------------------------}

function TRafaANN.GetNu: integer;
  begin
  result := FNu;
  end;

{------------------------------------------------------------------------------}

function TRafaANN.GetOutputArray(out ArOutput: array of double;
                                 Size: integer): boolean;
  var i : integer;
  begin
  if size = FNoOutputs then
     begin
     for i := 0 to Size - 1 do
        ArOutput[i] := (FUnits[FOutputUnits[i]] as TOutputUnit).ObtainedValue;
     result := true;
     end
  else
     result := false;
  end;

{------------------------------------------------------------------------------}

function TRafaANN.GetUpsilon: integer;
  begin
  result := FUpsilon;
  end;

{------------------------------------------------------------------------------}

function TRafaANN.GetWeightArray(var ar: array of double; size: integer): boolean;
  var i, j : integer;
  begin
  if size >= GetWeightCount then
     begin
     j := 0;
     for i := 0 to FNoUnits - 1 do
       if (FUnits[i] is TNeuron) then
          begin
          (FUnits[i] as TNeuron).SaveWeightsArray(ar, j);
          j := j + (FUnits[i] as TNeuron).InputsCount + 1;
          end;
     result := true;
     end
  else
     result := false;
  end;

{------------------------------------------------------------------------------}

function TRafaANN.GetWeightCount: integer;
  var
    i, j : integer;
  begin
  j := 0;
  for i := 0 to FNoUnits - 1 do
    if (FUnits[i] is TNeuron) then
       j := j + (FUnits[i] as TNeuron).InputsCount + 1;
  result := 0;
  end;

{------------------------------------------------------------------------------}

function TRafaANN.GetWorldCount: integer;
  begin
  result := FWorldCount;
  end;

{------------------------------------------------------------------------------}

procedure TRafaANN.InitializeCILP;
  var
    i : integer;
  begin
  for i := 0 to FLinksCount[0] - 1 do
    begin
    (FUnits[FLinkUnits[0, i]] as TLinkPoint).IsTriggered := true;
    (FUnits[FLinkUnits[0, i]] as TLinkPoint).IsBackTriggered := true;
    (FUnits[FLinkUnits[0, i]] as TLinkPoint).inputData(-1, nil);
    (FUnits[FLinkUnits[0, i]] as TLinkPoint).BackInput(0, 0, nil);
    end;

  for i := 0 to FLinksCount[2] - 1 do
    begin
    (FUnits[FLinkUnits[2, i]] as TLinkPoint).IsTriggered := true;
    (FUnits[FLinkUnits[2, i]] as TLinkPoint).IsBackTriggered := true;
    end;

  for i := 0 to FLinksCount[3] - 1 do
    begin
    (FUnits[FLinkUnits[3, i]] as TLinkPoint).IsTriggered := true;
    (FUnits[FLinkUnits[3, i]] as TLinkPoint).IsBackTriggered := true;
    end;
  end;

{------------------------------------------------------------------------------}

procedure TRafaANN.InitializeCML;
  var
    i : integer;
  begin
  for i := 0 to FLinksCount[0] - 1 do
    begin
    (FUnits[FLinkUnits[0, i]] as TLinkPoint).IsTriggered := true;
    (FUnits[FLinkUnits[0, i]] as TLinkPoint).IsBackTriggered := true;
    (FUnits[FLinkUnits[0, i]] as TLinkPoint).inputData(-1, nil);
    (FUnits[FLinkUnits[0, i]] as TLinkPoint).BackInput(0, 0, nil);
    end;

  for i := 0 to FLinksCount[1] - 1 do
    begin
    (FUnits[FLinkUnits[1, i]] as TLinkPoint).IsTriggered := true;
    (FUnits[FLinkUnits[1, i]] as TLinkPoint).IsBackTriggered := true;
    (FUnits[FLinkUnits[1, i]] as TLinkPoint).inputData(-1, nil);
    (FUnits[FLinkUnits[1, i]] as TLinkPoint).BackInput(0, 0, nil);
    end;

  for i := 0 to FLinksCount[2] - 1 do
    begin
    (FUnits[FLinkUnits[2, i]] as TLinkPoint).IsTriggered := true;
    (FUnits[FLinkUnits[2, i]] as TLinkPoint).IsBackTriggered := true;
    end;

  for i := 0 to FLinksCount[3] - 1 do
    begin
    (FUnits[FLinkUnits[3, i]] as TLinkPoint).IsTriggered := true;
    (FUnits[FLinkUnits[3, i]] as TLinkPoint).IsBackTriggered := true;
    end;
  end;

{------------------------------------------------------------------------------}

procedure TRafaANN.InitializeElman;
  var
  i : integer;
  begin
  if FLevelCount > 0 then
     for i := 0 to FLinksCount[0] - 1 do
       begin
       (FUnits[FLinkUnits[0, i]] as TLinkPoint).IsTriggered := false;
       (FUnits[FLinkUnits[0, i]] as TLinkPoint).IsBackTriggered := false;
       (FUnits[FLinkUnits[0, i]] as TLinkPoint).FLinkBackStyle := LbsZero;
       (FUnits[FLinkUnits[0, i]] as TLinkPoint).InputData(0.5, nil);
//       (FUnits[FLinkUnits[0, i]] as TLinkPoint).BackInput(0, 0, nil);
       end;
//  Trigger(0, 0);
//  BackTrigger(0, 0);
  end;

{------------------------------------------------------------------------------}

procedure TRafaANN.InitializeEpoch;
  begin
  case FLinkFlow of
    LfElman : InitializeElman;
    LfCILP  : InitializeCILP;
    LfSCTL  : InitializeSCTL;
    LfCML   : InitializeCML;
    end;
  end;

{------------------------------------------------------------------------------}  

function TRafaANN.InitializeSaving : string;
  var
    i : integer;
    s : string;
  begin
  s := '';
  if FLevelCount > 0 then
     begin
     for i := 0 to FLinksCount[0] - 2 do
       begin
       (FUnits[FLinkUnits[0, i]] as TLinkPoint).IsTriggered := false;
       (FUnits[FLinkUnits[0, i]] as TLinkPoint).IsBackTriggered := true;
       s := s + 'Value' + inttostr(i) + #9;
       end;
     if FLinksCount[0] > 0 then
       begin
       i := FLinksCount[0] - 1;
       (FUnits[FLinkUnits[0, i]] as TLinkPoint).IsTriggered := false;
       (FUnits[FLinkUnits[0, i]] as TLinkPoint).IsBackTriggered := true;
       s := s + 'Value' + inttostr(i);
       end;
     end;
  result := s;
  end;

{------------------------------------------------------------------------------}


procedure TRafaANN.InitializeSCTL;
  var
    i : integer;
  begin
  for i := 0 to FLinksCount[0] - 1 do
    begin
    (FUnits[FLinkUnits[0, i]] as TLinkPoint).IsTriggered := true;
    (FUnits[FLinkUnits[0, i]] as TLinkPoint).IsBackTriggered := true;
    (FUnits[FLinkUnits[0, i]] as TLinkPoint).inputData(-1, nil);
    (FUnits[FLinkUnits[0, i]] as TLinkPoint).BackInput(0, 0, nil);
    end;

  for i := 0 to FLinksCount[1] - 1 do
    begin
    (FUnits[FLinkUnits[1, i]] as TLinkPoint).IsTriggered := true;
    (FUnits[FLinkUnits[1, i]] as TLinkPoint).IsBackTriggered := true;
    (FUnits[FLinkUnits[1, i]] as TLinkPoint).inputData(-1, nil);
    (FUnits[FLinkUnits[1, i]] as TLinkPoint).BackInput(0, 0, nil);
    end;

  for i := 0 to FLinksCount[2] - 1 do
    begin
    (FUnits[FLinkUnits[2, i]] as TLinkPoint).IsTriggered := true;
    (FUnits[FLinkUnits[2, i]] as TLinkPoint).IsBackTriggered := true;
    end;

  for i := 0 to FLinksCount[3] - 1 do
    begin
    (FUnits[FLinkUnits[3, i]] as TLinkPoint).IsTriggered := true;
    (FUnits[FLinkUnits[3, i]] as TLinkPoint).IsBackTriggered := true;
    end;

  end;

{------------------------------------------------------------------------------}

procedure TRafaANN.LoadBestEpoch;
  var j, k : integer;
  begin
  k := 0;
  for j := 0 to FNoUnits - 1 do
    if (FUnits[j] is TNeuron) then
       begin
       (FUnits[j] as TNeuron).LoadWeightsArray(FWeightsArray, k);
       k := k + (FUnits[j] as TNeuron).InputsCount + 1;
       end;
  end;

{------------------------------------------------------------------------------}

function TRafaANN.LoadDescription(desc: TNetworkRep): boolean;
  var
    i  : integer;
    w : double;
  begin
  FNoInputs := 0;
  FNoOutputs := 0;
  FNoUnits  := desc.NoNodes;
  FLinkFlow := desc.LinkFlow;
  FNu       := Desc.Nu;
  FUpsilon  := Desc.Upsilon;
  FWorldCount := desc.WorldCount;
  FEtaCorrection := desc.EtaCorrection;
  SetLength(FUnits, FNoUnits);

  for i := 0 to desc.NoNodes - 1 do
    AddUnit(i, desc.Node[i]);

  for i := 0 to desc.NoArcs - 1 do
    begin
    w := desc.Arc[i].Weight;
    case desc.Node[desc.Arc[i].Target].Kind of
      UKNeuron : (FUnits[desc.Arc[i].Target] as TNeuron).AddInput(i, FUnits[desc.Arc[i].Source], w, desc.Arc[i].isFixed, desc.Arc[i].RandomRange);
      UKDelay  : (FUnits[desc.Arc[i].Target] as TDelayUnit).DefineInput(i, FUnits[desc.Arc[i].Source]);
      UKInput  : ;//Fill in later;
      UKOutput : (FUnits[desc.Arc[i].Target] as TOutputUnit).DefineUnit(i, FUnits[desc.Arc[i].Source]);
      UKRec    : (FUnits[Desc.Arc[i].Target] as TLinkPoint).SetInput(FUnits[desc.Arc[i].Source]);
      end;

    case desc.Node[desc.Arc[i].Source].Kind of
      UKNeuron : (FUnits[desc.Arc[i].Source] as TNeuron).AddOutput(i, FUnits[desc.Arc[i].Target]);
      UKDelay  : (FUnits[desc.Arc[i].Source] as TDelayUnit).AddOutput(i, FUnits[desc.Arc[i].Target]);
      UKInput  : (FUnits[desc.Arc[i].Source] as TInputUnit).AddOutput(i, FUnits[desc.Arc[i].Target]);
      UKOutput : ;//Fill in later;
      UKRec    : (FUnits[Desc.Arc[i].Source] as TLinkPoint).AddOutput(FUnits[desc.Arc[i].Target]);
      end;

    end;

  result := true;
  end;

{------------------------------------------------------------------------------}

function  TRafaANN.NewChecking(ChkFile, RelatFile : TFileName; SelectiveMSE, diffOrder : boolean; CorrInt : double = 0.5) : integer;
  var
    i, j, k, xOrder, xLine : integer;
    d : double;
    c1 : char;
    XAllCorrect : boolean;
    tmpMSE, tmp, XError, Amin, Amax : double;
    XSavingStrings : TStringList;
    XLineHeader : String;
    XDataFile, XTmpData, XReportList : TStringList;

    XCorrect : array of integer;

    XInDataArray : array of double;
    XOutDataArray1, XOutDataArray2 : array of double;
    XOutHaveValue : array of boolean;

    XArray : TRafaSortableArray;
    XStringArray : array of String;

       XPatternsCount : integer;

    s : String;


  begin
  ResetDelays;
  FCasesCount := 0;
  SetLength(XCorrect, FNoOutputs + 1);
  tmpMSE := 0;
  for i := 0 to FNoOutputs - 1 do
    begin
    (FUnits[FOutputUnits[i]] as TOutputUnit).Mode := NMTest;
    XCorrect[i] := 0;
    end;
  XCorrect[FNoOutputs] := 0;
  c1 := '*';

  XSavingStrings := TStringList.Create;
  XDataFile := TStringList.Create;
  XTmpData := TStringList.Create;
  XDataFile.LoadFromFile(ChkFile);

  SetLength(XInDataArray, FNoInputs);
  SetLength(XOutDataArray1, FNoOutputs);
  SetLength(XOutDataArray2, FNoOutputs);
  SetLength(XOutHaveValue, FNoOutputs);

  XReportList := TStringList.Create;
  XArray := TRafaSortableArray.Create;

  case FLinkFlow of
    LfElman  : InitializeElman;
    LfCILP   : InitializeCILP;
    LfSCTL   : InitializeSCTL;
    LfCML    : InitializeCML;
    LfSaving : XSavingStrings.Add(InitializeSaving);
    end;

  if diffOrder then
     begin
     i := 0;
     j := 0;
     xLine := 0;
     while XLine < XDataFile.Count do
       begin
       if trim(XDataFile.Strings[xLine]) <> '' then  //Temporarily considered the reset condition
          begin
          XArray.Add(TRafaInteger.Create(i), false);
          i := i + 1;
          end
       else
          begin
          SetLength(XStringArray, i);
          XOrder := XArray.CustomOrder + 1;
          XArray.CustomOrder := XOrder;
          while (XArray.CustomOrder = XOrder) do
            begin
            for k := 0 to i - 1 do
              XStringArray[k] := XDataFile.Strings[j + (XArray.Data[i] as TRafaInteger).data];
            XDataFile.Insert(XLine, '');
            XLine := XLine + 1;
            for k := 0 to i - 1 do
              begin
              XDataFile.Insert(XLine, XStringArray[k]);
              XLine := XLine + 1;
              end;
            XOrder := XArray.CustomOrder + 1;
            XArray.CustomOrder := XOrder;
            end;
          i := 0;
          j := XLine + 1;
          end;
       XLine := XLine + 1;
       end;
     //*********Remove later:****************
     XDataFile.SaveToFile(ChkFile + '1');
     SetLength(XStringArray, 0)
     end;

  XPatternsCount := 0;

  XArray.Clear(true);
  XArray.Free;

  for xLine := 0 to XDataFile.Count - 1 do
    begin
    XTmpData.Clear;
    XTmpData.DelimitedText := XDataFile.Strings[xLine];
    if linkflow = LfElman then ExecuteElman;
    if XTmpData.Count <= FNoInputs then //Temporarily considered the reset condition
       begin
       XReportList.Add('-***-');
       XReportList.Add('');
       if LinkFlow = LfSctl then
          for i := 0 to FLinksCount[1] - 1 do
            with FUnits[FLinkUnits[1, i]] as TLinkPoint do
              InputData(InitValue, nil);
       XPatternsCount := XPatternsCount + 1;
       end
    else
       begin
       XLineHeader := XTmpData.Strings[0];
       for i := 0 to FNoInputs - 1 do
         try
           XInDataArray[i] := StrToFloat(XTmpData[1 + i]);
         except
           XInDataArray[i] := 0;
           end;
       i := 0;
       while (i < FNoOutputs) and (1 + FNoInputs + i < XTmpData.Count) do
         begin
         try
           XOutDataArray1[i] := StrToFloat(XTmpData[1 + FNoInputs + i]);
           XOutHaveValue[i] := true;
           i := i + 1;
         except
           XOutDataArray1[i] := 0;
           XOutHaveValue[i] := false;
           i := i + 1;
           end;
         end;
       while (i < FNoOutputs) do
         begin
         XOutDataArray1[i] := 0;
         XOutHaveValue[i] := false;
         i := i + 1;
         end;
       ApplyInputArray(XInDataArray, FNoInputs);

       GetOutputArray(XOutDataArray2, FNoOutputs);

       for i := 0 to FNoOutputs - 1 do
         begin
         s := floattoStr(XOutDataArray2[i]) + #9 + floattoStr(XOutDataArray1[i]);
         XReportList.Add(s);
         end;

       XReportList.Add('');

       if (not SelectiveMSE) or (c1 = '+') then
          begin
          tmp := 0;
          XAllCorrect := true;
          for i := 0 to FNoOutputs - 1 do
            begin
            XError := XOutDataArray2[i] - XOutDataArray1[i];
            tmp := tmp + (XError * XError);
            with (FUnits[FOutputUnits[i]] as TOutputUnit) do
              begin
              Amin := OutMax - ((OutMax - OutMin) * CorrInt);
              Amax := OutMin + ((OutMax - OutMin) * CorrInt);
              end;
            if ((XOutDataArray2[i] >= Amin) and (XOutDataArray1[i] >= Amin))or
               ((XOutDataArray2[i] <= Amax) and (XOutDataArray1[i] <= Amax)) then
               XCorrect[i] := XCorrect[i] + 1
            else
               XAllCorrect := false;
            end;
          if XAllCorrect then XCorrect[FNoOutputs] := 0;
          tmpMse := tmpMse + (tmp / FNoOutPuts);
          FCasesCount := FCasesCount + 1;
          end;
       end;
    end;

  XReportList.Add('');

  if FCasesCount > 0 then
     begin
     XReportList.Add('-***-');
     XReportList.Add('');
     s := '*** RMSE = ' + floattostr(sqrt(tmpMse/FCasesCount))
     end
  else
     s := '*** RMSE = 0';
  XReportList.Add(s);

  s := '*** In the ' + inttostr(FCasesCount) + ' patterns, the number of correct classification per input was:';
  XReportList.Add(s);
  s := '*** ' + IntToStr(XCorrect[0]);
  for i := 1 to FNoOutputs - 1 do
    s := s + #9 + IntToStr(XCorrect[i]);
  XReportList.Add(s);
  s := '*** In the ' + inttostr(FCasesCount) + ' patterns, ' + inttostr(XCorrect[FNoOutputs]) +
       ' of them had all outputs correctly classified.';
  XReportList.Add(s);
  XReportList.Add('');
  XReportList.SaveToFile(RelatFile);
  XReportList.Free;

  //I'm not sure about this
//  if (LinkFlow = LfSaving) and (svFileName <> '') then
//     XSavingStrings.SaveToFile(svFileName);
  XSavingStrings.Free;
  if FCasesCount > 0 then
     result := FCasesCount
  else
     result := 0;

  XDataFile.Free;
  XTmpData.Free;


  end;

{------------------------------------------------------------------------------}

function  TRafaANN.NewEpochTr(F : TFilename; SelectiveMSE, SelTr, randGroup : boolean; svFileName : string = '') : double;
  var
    i, j, k, xLine : integer;
//    d : double;
    c1 : char;
    tmpMSE, tmp, XError : double;
    XSavingStrings : TStringList;
    XLineHeader : String;
    XDataFile, XTmpData : TStringList;

    XInDataArray : array of double;
    XOutDataArray1, XOutDataArray2 : array of double;
    XOutHaveValue : array of boolean;

    XArray : array of integer;
    XStringArray : array of String;
    XRandom : TRafaRandomiser;


  begin
  ResetDelays;
  FCasesCount := 0;
  tmpMSE := 0;
  for i := 0 to FNoOutputs - 1 do
    (FUnits[FOutputUnits[i]] as TOutputUnit).Mode := NMTraining;
  c1 := '*';

  XSavingStrings := TStringList.Create;
  XDataFile := TStringList.Create;
  XTmpData := TStringList.Create;
  XDataFile.LoadFromFile(F);

  SetLength(XInDataArray, FNoInputs);
  SetLength(XOutDataArray1, FNoOutputs);
  SetLength(XOutDataArray2, FNoOutputs);
  SetLength(XOutHaveValue, FNoOutputs);

  case FLinkFlow of
    LfElman  : InitializeElman;
    LfCILP   : InitializeCILP;
    LfSCTL   : InitializeSCTL;
    LfCML    : InitializeCML;
    LfSaving : XSavingStrings.Add(InitializeSaving);
    end;

  if randGroup then
     begin
     i := 0;
     j := 0;
     XRandom := TRafaRandomiser.Create;
     for xLine := 0 to XDataFile.Count - 1 do
       begin
       if trim(XDataFile.Strings[xLine]) <> '' then  //Temporarily considered the reset condition
          begin
          i := i + 1;
          SetLength(XArray, i);
          XArray[i - 1] := i - 1;
          end
       else
          begin
          XRandom.RandomIntegerArray(XArray, 0, i);
          SetLength(XStringArray, i);
          for k := 0 to i - 1 do
            XStringArray[k] := XDataFile.Strings[j + XArray[k]];
          for k := 0 to i - 1 do
            XDataFile.Strings[j + k] := XStringArray[k];
          i := 0;
          j := XLine + 1;
          end;
       end;
     XRandom.Free;
     //*********Remove later:****************
     //XDataFile.SaveToFile(F + 'rand');
     SetLength(XStringArray, 0)
     end;



  for xLine := 0 to XDataFile.Count - 1 do
    begin
    XTmpData.Clear;
    XTmpData.DelimitedText := XDataFile.Strings[xLine];

    if linkflow = LfElman then ExecuteElman;

    if XTmpData.Count <= FNoInputs then //Temporarily considered the reset condition
       begin

       if LinkFlow = LfSctl then
          for i := 0 to FLinksCount[1] - 1 do
            with FUnits[FLinkUnits[1, i]] as TLinkPoint do
              InputData(InitValue, nil);
       end
    else
       begin
       XLineHeader := XTmpData.Strings[0];
       for i := 0 to FNoInputs - 1 do
         try
           XInDataArray[i] := StrToFloat(XTmpData[1 + i]);
         except
           XInDataArray[i] := 0;
           end;
       i := 0;
       while (i < FNoOutputs) and (1 + FNoInputs + i < XTmpData.Count) do
         begin
         try
           XOutDataArray1[i] := StrToFloat(XTmpData[1 + FNoInputs + i]);
           XOutHaveValue[i] := true;
           i := i + 1;
         except
           XOutDataArray1[i] := 0;
           XOutHaveValue[i] := false;
           i := i + 1;
           end;
         end;
       while (i < FNoOutputs) do
         begin
         XOutDataArray1[i] := 0;
         XOutHaveValue[i] := false;
         i := i + 1;
         end;
       ApplyInputArray(XInDataArray, FNoInputs);

       if LinkFlow = LfElman then BackExecuteElman;
       GetOutputArray(XOutDataArray2, FNoOutputs);
       for i := 0 to FNoOUtputs - 1 do
         if (not XOutHaveValue[i]) or (SelTr and (trim(XLineHeader) = '-')) then
            XOutDataArray1[i] := XOutDataArray2[i];
       ApplyOutputArray(XOutDataArray1, FNoOutputs);

       if (not SelectiveMSE) or (c1 = '+') then
          begin
          tmp := 0;
          for i := 0 to FNoOutputs - 1 do
            begin
            XError := XOutDataArray2[i] - XOutDataArray1[i];
            tmp := tmp + (XError * XError);
            end;
          tmpMse := tmpMse + (tmp / FNoOutPuts);
          FCasesCount := FCasesCount + 1;
          end;
       end;
    end;

  //I'm not sure about this
  if (LinkFlow = LfSaving) and (svFileName <> '') then
     XSavingStrings.SaveToFile(svFileName);



  XSavingStrings.Free;
  XDataFile.free;
  XTmpData.free;

  if FCasesCount > 0 then
     result := sqrt(tmpMse / FCasesCount)
  else
     result := 0;

  end;

{------------------------------------------------------------------------------}

function  TRafaANN.NewEpochTs(F : TFileName; SelectiveMSE, diffOrder : boolean; svFileName : string = '') : double;
  var
    i, j, k, xOrder, xLine : integer;
//    d : double;
    c1 : char;
    tmpMSE, tmp, XError : double;
    XSavingStrings : TStringList;
    XLineHeader : String;
    XDataFile, XTmpData : TStringList;

    XInDataArray : array of double;
    XOutDataArray1, XOutDataArray2 : array of double;
    XOutHaveValue : array of boolean;

    XArray : TRafaSortableArray;
    XStringArray : array of String;

  begin
  ResetDelays;
  FCasesCount := 0;
  tmpMSE := 0;
  for i := 0 to FNoOutputs - 1 do
    (FUnits[FOutputUnits[i]] as TOutputUnit).Mode := NMTest;
  c1 := '*';

  XSavingStrings := TStringList.Create;
  XDataFile := TStringList.Create;
  XTmpData := TStringList.Create;
  XDataFile.LoadFromFile(F);

  SetLength(XInDataArray, FNoInputs);
  SetLength(XOutDataArray1, FNoOutputs);
  SetLength(XOutDataArray2, FNoOutputs);
  SetLength(XOutHaveValue, FNoOutputs);

  XArray := TRafaSortableArray.Create;

  case FLinkFlow of
    LfElman  : InitializeElman;
    LfCILP   : InitializeCILP;
    LfSCTL   : InitializeSCTL;
    LfCML    : InitializeCML;
    LfSaving : XSavingStrings.Add(InitializeSaving);
    end;

  if diffOrder then
     begin
     i := 0;
     j := 0;
     xLine := 0;
     while XLine < XDataFile.Count do
       begin
       if trim(XDataFile.Strings[xLine]) <> '' then  //Temporarily considered the reset condition
          begin
          XArray.Add(TRafaInteger.Create(i), false);
          i := i + 1;
          end
       else
          begin
          SetLength(XStringArray, i);
          XOrder := XArray.CustomOrder + 1;
          XArray.CustomOrder := XOrder;
          while (XArray.CustomOrder = XOrder) do
            begin
            for k := 0 to i - 1 do
              XStringArray[k] := XDataFile.Strings[j + (XArray.Data[i] as TRafaInteger).data];
            XDataFile.Insert(XLine, '');
            XLine := XLine + 1;
            for k := 0 to i - 1 do
              begin
              XDataFile.Insert(XLine, XStringArray[k]);
              XLine := XLine + 1;
              end;
            XOrder := XArray.CustomOrder + 1;
            XArray.CustomOrder := XOrder;
            end;
          i := 0;
          j := XLine + 1;
          end;
       XLine := XLine + 1;
       end;
     //*********Remove later:****************
     XDataFile.SaveToFile(F + '1');
     SetLength(XStringArray, 0)
     end;

  XArray.Clear(true);
  XArray.Free;

  for xLine := 0 to XDataFile.Count - 1 do
    begin
    XTmpData.Clear;
    XTmpData.DelimitedText := XDataFile.Strings[xLine];

    if linkflow = LfElman then ExecuteElman;
    if XTmpData.Count <= FNoInputs then //Temporarily considered the reset condition
       begin
       if LinkFlow = LfSctl then
          for i := 0 to FLinksCount[1] - 1 do
            with FUnits[FLinkUnits[1, i]] as TLinkPoint do
              InputData(InitValue, nil);
       end
    else
       begin
       XLineHeader := XTmpData.Strings[0];
       for i := 0 to FNoInputs - 1 do
         try
           XInDataArray[i] := StrToFloat(XTmpData[1 + i]);
         except
           XInDataArray[i] := 0;
           end;
       i := 0;
       while (i < FNoOutputs) and (1 + FNoInputs + i < XTmpData.Count) do
         begin
         try
           XOutDataArray1[i] := StrToFloat(XTmpData[1 + FNoInputs + i]);
           XOutHaveValue[i] := true;
           i := i + 1;
         except
           XOutDataArray1[i] := 0;
           XOutHaveValue[i] := false;
           i := i + 1;
           end;
         end;
       while (i < FNoOutputs) do
         begin
         XOutDataArray1[i] := 0;
         XOutHaveValue[i] := false;
         i := i + 1;
         end;
       ApplyInputArray(XInDataArray, FNoInputs);

       GetOutputArray(XOutDataArray2, FNoOutputs);

       if (not SelectiveMSE) or (c1 = '+') then
          begin
          tmp := 0;
          for i := 0 to FNoOutputs - 1 do
            begin
            XError := XOutDataArray2[i] - XOutDataArray1[i];
            tmp := tmp + (XError * XError);
            end;
          tmpMse := tmpMse + (tmp / FNoOutPuts);
          FCasesCount := FCasesCount + 1;
          end;
       end;
    end;

  //I'm not sure about this
  if (LinkFlow = LfSaving) and (svFileName <> '') then
     XSavingStrings.SaveToFile(svFileName);
  XSavingStrings.Free;
  if FCasesCount > 0 then
     result := sqrt(tmpMse / FCasesCount)
  else
     result := 0;
  XDataFile.Free;
  XTmpData.Free;

  end;

{------------------------------------------------------------------------------}

function TRafaANN.NewTraining(TrFile, TsFile, RelatFile: TFileName; MaxEp, CCount, CTimes : integer;
                         SelectiveMSE, SelTr, BestTr, RandGroup : boolean) : integer;
  var
    i, j, k : integer;
//    FTr, FTs, FRelat : TextFile;
    XTsFile : TFileName;
    XRelatList : TStringList;
    d1, d2, d3 : double;
    BestEpoc : integer;
    BestError : double;
    stop : boolean;

  begin
  If TsFile <> '' then
     XTsFile := TsFile
  else
     XTsFile := TrFile;
  XRelatList := TStringList.Create;
  FmTrEvol.ResetCanc;
//  FmTrEvol.SetFileName(RelatFile);
  j := 0;
  for i := 0 to FNoUnits - 1 do
    if (FUnits[i] is TNeuron) then
        j := j + (FUnits[i] as TNeuron).InputsCount + 1;
  SetLength(FWeightsArray, j);
  i := 0;
  BestError := 10000;
  bestEpoc := -1;

  if TsFile <> '' then
     d2 := NewEpochTs(TsFile, SelectiveMSE, false)
  else
     d2 := NewEpochTs(TrFile, SelectiveMSE, false);
  d1 := d2;
  stop := FmTrEvol.Atualiza(i, MaxEp, d1, d2, CCount, CTimes);
  Application.ProcessMessages;
  while (i < MaxEp) and (i >= 0) and not stop do
    begin
    d1 := NewEpochTr(TrFile, SelectiveMSE, SelTr, RandGroup);
    if TsFile <> '' then
       d2 := NewEpochTs(TsFile, SelectiveMSE, false)
    else
       d2 := d1;
    if BestTr then d3 := d1 else d3 := d2;
    CorrectEta;
    XRelatList.Add(FloatToStr(d1) + ' ' + FloatToStr(d2));
    if (i = 0) or (d3 < BestError) then
       begin
       BestEpoc := i;
       BestError := d3;
       k := 0;
       for j := 0 to FNoUnits - 1 do
         if (FUnits[j] is TNeuron) then
            begin
            (FUnits[j] as TNeuron).SaveWeightsArray(FWeightsArray, k);
            k := k + (FUnits[j] as TNeuron).InputsCount + 1;
            end;
       end;
    stop := FmTrEvol.Atualiza(i, MaxEp, d1, d2, CCount, CTimes);
    Application.ProcessMessages;
    if i >= 0 then
       i := i + 1;
    end;

  XRelatList.SaveToFile(RelatFile);
  XRelatList.Free;
  if (i >= MaxEp) then
     result := BestEpoc
  else
     result := i;
  end;

{------------------------------------------------------------------------------}

procedure TRafaANN.PreBackTrigger(lv: integer);
  var i : integer;
  begin
  if (lv >= 0) and (lv < FLevelCount) then
     for i := 0 to FLinksCount[lv] - 1 do
       (FUnits[FLinkUnits[lv, i]] as TLinkPoint).PreBackTrigger;
  end;

{------------------------------------------------------------------------------}

procedure TRafaANN.PreTrigger(lv: integer);
  var i : integer;
  begin
  if (lv >= 0) and (lv < FLevelCount) then
     for i := 0 to FLinksCount[lv] - 1 do
       (FUnits[FLinkUnits[lv, i]] as TLinkPoint).PreTrigger;
  end;

{------------------------------------------------------------------------------}

procedure TRafaANN.ResetDelays;
  var i : integer;
  begin
  for i := 0 to FNoUnits - 1 do
    if FUnits[i] is TDelayUnit then
       (FUnits[i] as TDelayUnit).ResetTimes;
  end;

{------------------------------------------------------------------------------}

procedure TRafaANN.ResetNetwork(InitializeWeights : boolean = true);
  var i : integer;
  begin
  if InitializeWeights then
     for i := 0 to FNoUnits - 1 do
       FUnits[i].Initialize;
  case FLinkFlow of
    LfElman : InitializeElman;
    LfCILP  : InitializeCILP;
    LfSCTL  : InitializeSCTL;
    LfCML   : InitializeCML;
    end;
  end;

{------------------------------------------------------------------------------}

function TRafaANN.SaveDescription(desc: TNetworkRep): boolean;
  var
    i : integer;
  begin
  result := false;
  if (desc.NoNodes = FNoUnits) then
     begin
     result := true;
     For i := 0 to desc.NoArcs - 1 do
       begin
       desc.Arc[i].Weight := FUnits[desc.Arc[i].Target].GetWeightByArcIndex(i);
       desc.Arc[i].RandomRange := 0;
       end;
     for i := 0 to FNoUnits - 1 do
       if (FUnits[i] is TNeuron) and (desc.Node[i].Kind = UKNeuron) then
          begin
          (desc.Node[i] as TNeuronRep).BiasWeight := (FUnits[i] as TNeuron).GetBiasWeight;
          (desc.Node[i] as TNeuronRep).RandomBias := 0;
          end
       else if (FUnits[i] is TNeuron) or (desc.Node[i].Kind = UKNeuron) then
          result := false;
     end;
  end;

{------------------------------------------------------------------------------}

procedure TRafaANN.SetLinkFlow(const Value: TLinkFlow);
  begin
  FLinkFlow := Value;
  end;

{------------------------------------------------------------------------------}

procedure TRafaANN.SetMode(NewMode: TNetworkMode);
  var i : integer;
  begin
  for i := 0 to FNoOutputs - 1 do
    (FUnits[FOutputUnits[i]] as TOutputUnit).Mode := NewMode;
  end;
{------------------------------------------------------------------------------}

procedure TRafaANN.SetNu(const Value: integer);
  begin
  if Value > 0 then
     FNu := Value;
  end;

{------------------------------------------------------------------------------}

procedure TRafaANN.SetUpsilon(const Value: integer);
  begin
  if Value > 0 then
     FUpsilon := Value;
  end;

{------------------------------------------------------------------------------}

procedure TRafaANN.SetWorldCount(const Value: integer);
  begin
  if Value > 0 then
     FWorldCOunt := Value;
  end;

{------------------------------------------------------------------------------}

function TRafaANN.Training(TrFile, TsFile, RelatFile: TFileName; MaxEp,
 CCount, CTimes: integer; SelectiveMSE, SelTr, BestTr: boolean): integer;

  var
    i, j, k : integer;
    FTr, FTs, FRelat : TextFile;
    d1, d2, d3 : double;
    BestEpoc : integer;
    BestError : double;
    stop : boolean;

  begin
  assignFile(FTr, TrFile);
  If TsFile <> '' then
     assignFile(FTs, TsFile);
  assignFile(FRelat, RelatFile);
  FmTrEvol.ResetCanc;
//  FmTrEvol.SetFileName(RelatFile);
  rewrite(FRelat);
  j := 0;
  for i := 0 to FNoUnits - 1 do
    if (FUnits[i] is TNeuron) then
        j := j + (FUnits[i] as TNeuron).InputsCount + 1;
  SetLength(FWeightsArray, j);
  i := 0;
  BestError := 10000;
  bestEpoc := -1;
  if TsFile <> '' then
     d2 := EpochTs(FTs, SelectiveMSE)
  else
     d2 := EpochTs(FTr, SelectiveMSE);
  d1 := d2;
  stop := FmTrEvol.Atualiza(i, MaxEp, d1, d2, CCount, CTimes);
  Application.ProcessMessages;
  while (i < MaxEp) and (i >= 0) and not stop do
    begin
    d1 := EpochTr(FTr, SelectiveMSE, SelTr);
    if TsFile <> '' then
       d2 := EpochTs(FTs, SelectiveMSE)
    else
       d2 := d1;
    if BestTr then d3 := d1 else d3 := d2;
    CorrectEta;
    writeln(FRelat, d1, d2);
    if (i = 0) or (d3 < BestError) then
       begin
       BestEpoc := i;
       BestError := d3;
       k := 0;
       for j := 0 to FNoUnits - 1 do
         if (FUnits[j] is TNeuron) then
            begin
            (FUnits[j] as TNeuron).SaveWeightsArray(FWeightsArray, k);
            k := k + (FUnits[j] as TNeuron).InputsCount + 1;
            end;
       end;
    stop := FmTrEvol.Atualiza(i, MaxEp, d1, d2, CCount, CTimes);
    Application.ProcessMessages;
    if i >= 0 then
       i := i + 1;
    end;

  closeFile(FRelat);
  if (i >= MaxEp) then
     result := BestEpoc
  else
     result := i;
  end;

{------------------------------------------------------------------------------}

procedure TRafaANN.Trigger(lv : integer; Kind : char = 'a'; value : integer = 0);
  var
    i : integer;
  begin
  if (lv >= 0) and (lv < FLevelCount) then
     for i := 0 to FLinksCount[lv] - 1 do
       (FUnits[FLinkUnits[lv, i]] as TLinkPoint).Trigger(kind, value);
  end;

{------------------------------------------------------------------------------}


end.

