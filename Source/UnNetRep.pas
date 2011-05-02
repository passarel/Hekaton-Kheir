{
  @abstract(Unit that describes the structures to represent neural networks)
  @author(Rafa "Passarel" Borges <passarel@gmail.com)
  @created(Porto Alegre, September 30, 2005 )
  @lastmod(Porto Alegre, April, 2008)

  The unit @name contains the classes to represent all the features on a network.
This representation is the core of the HekatonKhire project, since it can be
loaded from different formats and also save to different structures.

  Between these formats already implemented, we may cite the XML description,
a visual interface and the structures that encapsulate the behavior of the
network. The representation can also be loaded from a logic program, through the
different translation algorithms implemented.
                      
  The next idea for represent the network is use the "dot" format to generate
different graph representations for the network, and save as an image file.
There's still the need of a deep revision of the "VisualOrg" procedures, and the
test of the "LoadElman".
}
unit UnNetRep;

interface

uses Classes, SysUtils;

{------------------------------------------------------------------------------}

{Type encapsulating real functions with real parameters}
type TParamFunction = function (x : double; params : array of double) : double;

{------------------------------------------------------------------------------}

{Type encapsulating the delay functions}
type TDelayFunction = function(delay : integer; data : double) : double;

{------------------------------------------------------------------------------}

{Enumeration of the possible kinds of units on a network}
type TUnitKind = (UKInput, UKOutput, UKNeuron, UKDelay, UkRec);

{------------------------------------------------------------------------------}

{Enumeration of the possible style for back propagate values in recursive links}
type TLinkBackStyle = (LbsOutput, LbsZero, LbsEach, LbsAllOutput, LbsAllZero, LbsAllMean);

{------------------------------------------------------------------------------}

{Enumeration of the possible styles of flow of the data on the network}
type TLinkFlow= (LfNone, LfElman, LfCILP, LfSCTL, LfCML, LfSaving);

{------------------------------------------------------------------------------}

{Enumeration of the possible ways to merge two different networks}
type TMergeStyle = (MsNone, MsInput, MsOutput);

{------------------------------------------------------------------------------}

{
@abstract(Class encapsulating the representation of a connection between units)

  The class @name contains a set of structures to keep the information about
the connections between units and their weights (when used).
}
type TConnecRep = class
  private
    //Index of the source unit
    FSource   : integer;

    //Index of the target unit
    FTarget   : integer;

    //Weight of the connection
    FWeight   : double;

    //Random variation of the weight
    FRandom   : double;

    //Indicates if the weight is not changed on training
    FFixed    : boolean;

    //Write method for property Source
    procedure SetSource  (value : integer);

    //Write method for property Target
    procedure SetTarget  (value : integer);

    //Write method for property Weight
    procedure SetWeight  (value : double);

    //Write method for property RandomRange
    procedure SetRandom  (value : double);

    //Write method for property Source
    function GetSource   : integer;

    //Read method for property Target
    function GetTarget   : integer;

    //Read method for property Weight
    function GetWeight   : double;

    //Read method for property RandomRange
    function GetRandom   : double;

    //Read method for property Fixed
    function GetFixed: boolean;

    //Write method for property Fixed
    procedure SetFixed(const Value: boolean);


  public
    //Constructor of the class
    //@param(PSource: index of the source of the connection)
    //@param(PTarget: index of the target of the connection)
    constructor Create(Psource, Ptarget : integer);

    //Constructor of the class: creates a copy of an existing instance
    //@param(POrig: Object to be copied)
    constructor CreateCopy(POrig : TConnecRep);

    //Destructor of the class
    destructor Destroy; override;

    //Index of the source unit
    property Source     : integer read GetSource   write SetSource;

    //Index of the target unit
    property Target     : integer read GetTarget   write SetTarget;

    //Weight of the connection
    property Weight     : double  read GetWeight   write SetWeight;

    //Random variation of the weight
    property RandomRange : double read GetRandom   write SetRandom;

    //Indicates if the weight is not changed on training
    property isFixed    : boolean read GetFixed    write SetFixed;

  end;

{------------------------------------------------------------------------------}

{
@abstract(Abstract class that encapsulates a unit on the network)

  The abstract class @name contains a set of structures that are necessary for
any kind of unit to be used on a network.
}
type TUnitRep = class
  private
    //Tag identifying the unit
    FTag  : integer;

    //X position of the unit (on visual representation)
    FX    : integer;

    //Y position of the unit (on visual representation)
    FY    : integer;

    //Name of the unit
    FName : String;

    //World where the network is (in representation of modal envronments)
    FWorld : integer;

    //Read method for property X
    function GetX : integer;

    //Read method for property Y
    function GetY : integer;

    //Write method for property X
    procedure SetX(value : integer);

    //Write method for property Y
    procedure SetY(value : integer);

    //Read method for property  name
    function  GetName : string;

    //Write method for property Name
    procedure SetName (value : string);

    //Read method for property Tag
    function GetTag : integer;

    //Write method for property Tag
    procedure SetTag(value : integer);

    //Abstract function: Read method for property Kind
    function  getKind : TUnitKind; virtual; abstract;

    //Read method for property World
    function GetWorld: integer;

    //Write method for property World
    procedure SetWorld(const Value: integer);

  public
    //Constructor of the class
    //@param(Pname: Name of the unit)
    constructor Create(PName : string);

    //Constructor of the class: creates a copy of an existing instance
    //@param(POrig: Object to be copied)
    constructor CreateCopy(POrig : TUnitRep);

    //X position of the unit (on visual representation)
    property X : integer read getX write SetX;

    //Y position of the unit (on visual representation)
    property Y : integer read getY write SetY;

    //Name of the unit
    property Name : String read getName write SetName;

    //Tag identifying the unit
    property Tag : integer read getTag write SetTag;

    //Kind of the unit
    property Kind : TUnitKind read GetKind;

    //World where the network is (in representation of modal envronments)
    property World : integer read GetWorld write SetWorld;

  end;

{------------------------------------------------------------------------------}

{
@abstract(Class encapsulating a delay unit)

  The class @name describes the configuration of a unit that works as a memory
dispositive, i.e. keeps the last values applied to it and propagate the result
of a function over these values.
}
type TDelayRep = class(TUnitRep)
  private
    //Numbers of times allowed to be kept in the memory
    FTimes : integer;

    //Index of the delay function for the unit
    FDelayFunction : integer;

    //Read method for property Times
    function GetTimes : integer;

    //Read method for property DelayFunction
    function GetDelayFunction : integer;

    //Write method for property Times
    procedure SetTimes(value : integer);

    //Write method for property DelayFunction
    procedure SetDelayFunction(value : integer);

    //Implementation of abstract function: Read method for property Kind
    function  getKind : TUnitKind; override;

  public
    //Constructor of the class
    //@param(Pname: Name of the unit)
    constructor Create(PName : string);

    //Constructor of the class: creates a copy of an existing instance
    //@param(POrig: Object to be copied)
    constructor CreateCopy(POrig : TDelayRep);

    //Destructor of the class
    destructor Destroy; override;

    //Numbers of times allowed to be kept in the memory
    property Times : integer read GetTimes write SetTimes;

    //Index of the delay function for the unit
    property DelayFunction : integer read GetDelayFunction write SetDelayFunction;

  end;

{------------------------------------------------------------------------------}

{
@abstract(Class encapsulating an artificial neuron)

  The class @name describes all the features of an artificial neuron, simmilar
to the perceprton. The neuron receives a set of input values, realizes a
pondered sum between these values, apply an activation function over this sum
and propagate the value to the other units.
}
type TNeuronRep = class(TUnitRep)
  private
    //Index of the activation function of the neuron
    FAtivFunction   : integer;

    //Parameters for the activation function
    FFunctionParams : array of double;

    //Number of parameters for the activation function
    FParamCount    : integer;

    //Learning rate
    FEta            : double;

    //Momentum Rate
    FMomentum       : double;

    //Indicates if the neuron has an external bias applied
    FBiased         : boolean;

    //Weight of the link with external bias
    FBiasWeight     : double;

    //Random variatiom of bias' weight
    FRandomBias     : double;

    //Indicates if the bias' weight is fixed
    FFixedBias     : boolean;

    //Read method for the property AtivFunction
    function GetAtivFunction : integer;

    //Read method for the property Eta
    function GetEta : double;

    //Read method for the property Momentum
    function GetMomentum : double;

    //Read method for the property HasBias
    function GetBiased: boolean;

    //Read method for the property BiasWeight
    function GetBiasWeight: Double;

    //Write method for the property AtivFunction
    procedure SetAtivFunction(value : integer);

    //Write method for the property Eta
    procedure SetEta(value : double);

    //Write method for the property Momentum
    procedure SetMomentum(const Value: double);

    //Write method for the property HasBias
    procedure SetBiased(value : boolean);

    //Write method for the property BiasWeight
    procedure SetBiasWeight(value : double);

    //Implementation of abstract function: Read method for property Kind
    function  getKind : TUnitKind; override;

    //Read method for the property RandomBias
    function GetRandomBias: double;

    //Write method for the property RandomBias
    procedure SetRandomBias(const Value: double);

    //Read method for the property Param
    function GetFunctionParam(i: integer): double;

    //Write method for the property Param
    procedure SetFunctionParam(i: integer; const Value: double);

    //Read method for the property ParamCount
    function GetParamCount: integer;

    //Read method for the property FixedBias
    function GetFixedBias: boolean;

    //Write method for the property FixedBias
    procedure SetFixedBias(const Value: boolean);

  public
    //Constructor of the class
    //@param(Pname: Name of the unit)
    constructor Create(PName : string);

    //Constructor of the class: creates a copy of an existing instance
    //@param(POrig: Object to be copied)
    constructor CreateCopy(POrig : TNeuronRep);

    //Destructor of the class
    destructor Destroy; override;

    //Index of the activation function of the neuron
    property AtivFunction : integer read GetAtivFunction write SetAtivFunction;

    //Learning rate
    property Eta  : double read GetEta write SetEta;

    //Momentum
    property Momentum  : double read GetMomentum write SetMomentum;

    //Indicates if the neuron has an external bias applied
    property HasBias : boolean read GetBiased write SetBiased;

    //Weight of the link with external bias
    property BiasWeight : double read GetBiasWeight write SetBiasWeight;

    //Random variation of bias' weight
    property RandomBias : double read GetRandomBias write SetRandomBias;

    //Indicates if the bias' weight is fixed
    property FixedBias : boolean read GetFixedBias write SetFixedBias;

    //Parameters for the activation function
    property Param[i : integer] : double read GetFunctionParam write SetFunctionParam;

    //Number of parameters for the activation function
    property ParamCount : integer read GetParamCount;

    //Add a parameter for the activation function
    //@param(param: Parameter to be added)
    procedure AddFunctionParam(param : double);

    //Clear the parameters
    procedure ClearFunctionParams;

  end;

{------------------------------------------------------------------------------}

{
@abstract(Class encapsulating the input and output units)

  Differently of most of the implementations of neural networks, the Hekaton
project uses a different kind of unit exclusively to realize the interface with
the external environment.

  The class @name describes the features of the units to realize these interface.
An instance of this class contains information about the interval of input and
output values, to realize a adequation of the values according the needs of the
network and the environment.
}
type TIOUnitRep = class(TUnitRep)
  private
    //Indicates if it is an input unit
    FInput : boolean;

    //Max value that can be aplied on the unit
    FInMax : double;

    //Min value that can be aplied on the unit
    FInMin : double;

    //Max value that the unit apply on its output
    FOutMax : double;

    //Min value that the unit apply on its output
    FOutMin : double;

    //Read method for the property IsInput
    function  GetInput : boolean;

    //Write method for the property IsInput
    procedure SetInput(value : boolean);

    //Implementation of abstract function: Read method for property Kind
    function  getKind : TUnitKind; override;

    //Read method for the property InMin
    function  GetInMin : double;

    //Read method for the property InMax
    function  GetInMax : double;

    //Write method for the property InMin
    procedure SetInMin(value : double);

    //Write method for the property InMax
    procedure SetInMax(value: double);

    //Read method for the property OutMin
    function  GetOutMin : double;

    //Read method for the property OutMax
    function  GetOutMax : double;

    //Write method for the property OutMin
    procedure SetOutMin(value : double);

    //Write method for the property OutMax
    procedure SetOutMax(value: double);

  public
    //Constructor of the class
    //@param(Pname: Name of the unit)
    //@param(PInput: Indicates if the unit is an input unit)
    constructor Create(PName : string; PInput : boolean);

    //Constructor of the class: creates a copy of an existing instance
    //@param(POrig: Object to be copied)
    constructor CreateCopy(POrig : TIOUnitRep);

    //Destructor of the class
    destructor Destroy; override;

    //Indicates if it is an input unit
    property IsInput : boolean read GetInput  write SetInput;

    //Max value that can be aplied on the unit
    property InMax   : double  read GetInMax  write SetInMax;

    //Min value that can be aplied on the unit
    property InMin   : double  read GetInMin  write SetInMin;

    //Max value that the unit apply on its output
    property OutMax  : double  read GetOutMax write SetOutMax;

    //Min value that the unit apply on its output
    property OutMin  : double  read GetOutMin write SetOutMin;

  end;

{------------------------------------------------------------------------------}

{
@abstract(Class encapsulating the recursive links of the network)

  The class @name encapsulates simple units to realize the recursive links on the
network. These units are used to allow a global control of the propagation of
the values through these kind of links.
}
type TRecLinkRep = class(TUnitRep)
  private
    //Level of the link
    FLevel  : integer;

    //Style of the style of BackPropagation to be performed by the link
    FLinkBackStyle : TLinkBackStyle;

    //Initialization value of the link (almost not used);
    FInitValue : double;

    //Implementation of abstract function: Read method for property Kind
    function  getKind : TUnitKind; override;

    //Read method for the property Level
    function GetLevel: integer;

    //Write method for the property Level
    procedure SetLevel(const Value: integer);

    //Read method for the property LinkBackStyle
    function GetLinkBackStyle: TLinkBackStyle;

    //Write method for the property LinkBackStyle
    procedure SetLinkBackStyle(const Value: TLinkBackStyle);

    //Read method for the property InitValue
    function GetInitValue: double;

    //Write method for the property InitValue
    procedure SetInitValue(const Value: double);

  public
    //Constructor of the class
    //@param(Pname: Name of the unit)
    constructor Create(PName : string);

    //Constructor of the class: creates a copy of an existing instance
    //@param(POrig: Object to be copied)
    constructor CreateCopy(POrig : TRecLinkRep);

    //Destructor of the class
    destructor Destroy; override;

    //Initialization value of the link (almost not used);
    property InitValue : double read GetInitValue write SetInitValue;

    //Level of the link
    property Level  : integer read GetLevel write SetLevel;

    //Style of the style of BackPropagation to be performed by the link
    property LinkBackStyle : TLinkBackStyle read GetLinkBackStyle write SetLinkBackStyle;

  end;

{------------------------------------------------------------------------------}

{
@abstract(Class encapsulating the whole neural network)

The class @name describes all the configuration of the network, through the list
of units and connections and other auxiliar structures. It also contain some
routines to convert this configuration to a different format.
}
type TNetworkRep = class
  private
    //List of the nodes on the network
    FNodes : TStrings;

    //List of arcs on the network
    FArcs  : TStrings;

    //Defines if FNodes and FArcs should also be destroyed when the destructor is called
    FDestroyLists : boolean;

    //Factor to multiply the Eta value during the training;
    FEtaCorrection : double;

    //Number of FeedForward executions
    FNu : integer;

    //Number of backpropagation steps per time point (simmilar to nu)
    FUpsilon : integer;

    //Number of worlds (on modal representations)
    FWorldCount : integer;

    //Number of links between worlds (on modal representations)
    FWorldLinkPoints : integer;

    //Top position for the visual representation of each world
    FWorldTops    : array of integer;

    //Left position for the visual representation of each world
    FWorldLefts   : array of integer;

    //Height of the visual representation of each world
    FWorldHeights : array of integer;

    //Width of the visual representation of each world
    FWorldWidths  : array of integer;

    //Flow of the values on the network
    FLinkFlow : TLinkFlow;

    //Read method for the property NoNodes
    function  GetNoNodes : integer;

    //Read method for the property NoArcs
    function  GetNoArcs  : integer;

    //Read method for the property Node
    function  GetNode(index : integer) : TUnitRep;

    //Read method for the property Arc
    function  GetArc(index : integer)  : TConnecRep;

    //Read method for the property WorldCount
    function GetWorldCount: integer;

    //Read method for the property WorldHeights
    function GetWorldHeights(index: integer): integer;

    //Read method for the property WorldLefts
    function GetWorldLefts(index: integer): integer;

    //Read method for the property WorldLinks
    function GetWorldLinks: integer;

    //Read method for the property WorldTops
    function GetWorldTops(index: integer): integer;

    //Read method for the property WorldWidths
    function GetWorldWidths(index: integer): integer;

    //Read method for the property LinkFlow
    function GetLinkFlow : TLinkFlow;

    //Write method for the property WorldCount
    procedure SetWorldCount(const Value: integer);

    //Write method for the property WorldHeights
    procedure SetWorldHeights(index: integer; const Value: integer);

    //Write method for the property WorldLefts
    procedure SetWorldLefts(index: integer; const Value: integer);

    //Write method for the property WorldLinks
    procedure SetWorldLinks(const Value: integer);

    //Write method for the property WorldTops
    procedure SetWorldTops(index: integer; const Value: integer);

    //Write method for the property WorldWidths
    procedure SetWorldWidths(index: integer; const Value: integer);

    //Write method for the property LinkFlow
    procedure SetLinkFlow (const value : TLinkFlow);

    //Read method for the property RecLevels
    function GetRecLevels: integer;

    //Connect all the of each layer to all the units of the next
    //@param(layers: array with the layer of each unit)
    //@param(size: number of layers)
    procedure doFullyConnect(layers : array of integer; size : integer;
                             ConWeight, ConRange : double; ConFixed : boolean);

    //Generate a graph representation of the network
    //@param(layers: array with the layer of each unit)
    //@param(size: number of layers)
    procedure doSaveDot(layers : array of integer; size : integer; FileName : TFileName);

    //Read method for property EtaCorrection
    function GetEtaCorrection: double;

    //Write method for property EtaCorrection
    procedure SetEtaCorrection(const Value: double);

    //Read method for property Nu
    function GetNu: integer;

    //Write method for property Nu
    procedure SetNu(const Value: integer);

    //Read method for property Upsilon
    function GetUpsilon: integer;

    //Write method for property Upsilon
    procedure SetUpsilon(const Value: integer);
    function GetInput(index: integer): TIOUnitRep;
    function GetOutput(index: integer): TIOUnitRep;

  public
    //Constructor of the class
    //@param(PNodesList: List of nodes on the network)
    //@param(PArcsList: List of arcs on the network)
    //@param(ExternalSource: Declares that the lists come from an external (should not be destroyed))
    constructor Create(PNodesList, PArcsList: TStrings; ExternalSource : boolean = false);

    //Destructor of the class
    destructor  Destroy; override;

    //Number of nodes on the network
    property NoNodes : integer read GetNoNodes;

    //Number of arcs on the network
    property NoArcs  : integer read GetNoArcs;

    //Node on the network specified by an index
    property Node[index : integer] : TUnitRep   read GetNode;

    //Arc on the network specified by an index
    property Arc [index : integer] : TConnecRep read GetArc;

    //Input unit specified by an index
    property Input [index : integer] : TIOUnitRep read GetInput;

    //Input unit specified by an index
    property Output [index : integer] : TIOUnitRep read GetOutput;

    //Number of recursive levels
    property RecLevels  : integer read GetRecLevels;

    //Number of worlds on the network
    property WorldCount : integer   read GetWorldCount write SetWorldCount;

    //Number of links between worlds
    property WorldLinks : integer   read GetWorldLinks write SetWorldLinks;

    //Top position for the visual representation of the world specified by the index
    property WorldTops[index : integer]   : integer read GetWorldTops    write SetWorldTops;

    //Left position for the visual representation of the world specified by the index
    property WorldLefts[index : integer] : integer read GetWorldLefts   write SetWorldLefts;

    //Height of the visual representation of the world specified by the index
    property WorldHeights[index : integer]: integer read GetWorldHeights write SetWorldHeights;

    //Width of for the visual representation of the world specified by the index
    property WorldWidths[index : integer] : integer read GetWorldWidths  write SetWorldWidths;

    //Flow of the values on the network
    property LinkFlow : TLinkFlow read GetLinkFlow write SetLinkFlow;

    //Factor to multiply the Eta value during the training;
    property EtaCorrection : double read GetEtaCorrection write SetEtaCorrection;

    //Number of FeedForward executions
    property Nu : integer read GetNu write SetNu;

    //Number of backpropagation steps per time point (simmilar to nu)
    property Upsilon : integer read GetUpsilon write SetUpsilon;

    //List of nodes on the network
    function  GetNodeList : TStrings;

    //List of arcs on the network
    function  GetArcList  : TStrings;

    //Load a list with the arcs from or to some specific unit
    //@param(list: list to be filled)
    //@param(index: index of the unit to be considered)
    //@param(source: indicates if the list is of arcs which source is the unit)
    procedure GetSpecArcsList(list : TStrings; indexes : array of integer; source : boolean);

    //Clear the list of nodes
    procedure ClearNodes;

    //Clear the list of arcs
    procedure ClearArcs;

    //Add a new node on the network
    //@param(Pname: Name of the node)
    //@param(PKind: Kind of the node)
    //return(Index of the inserted node)
    function AddNode(PName : string; PKind : TUnitKind) : integer;

    //Add a new ark on the network
    //@param(PSource: specify the index of the source of the arc)
    //@param(PTarget: specify the index of the target of the arc)
    //@return(Index of the inserted arc)
    function AddArc (PSource, PTarget : integer) : integer;

    //Add a new node already represented by an object
    //@param(Object to be inserted)
    //@return(index of the inserted node)
    function AddNodeObject(PObj: TUnitRep) : integer;

    //Add a new arc already represented by an object
    //@param(Object to be inserted)
    //@return(index of the inserted arc)
    function AddArcObject(PObj: TConnecRep) : integer;

    //Delete a node of the network
    //@param(index: index of the node to be deleted)
    procedure DeleteNode(index : integer);

    //Delete an arc of the network
    //@param(index: index of the arc to be deleted)
    procedure DeleteArc (index : integer);

    //Rename an node on the network
    //@param(index: index of the node to be renamed)
    //@param(NewName: name to be applied to the node)
    procedure RenameNode(index : integer; NewName : string);

    //Organize alphabetically the list of nodes
    procedure SortNodes;

    //Gets a simmilar network
    //@param(cp: New network to be created)
    procedure GetCopy(cp : TNetworkRep);

    //Save the network to a XML file
    //@param(Filename : name(and path) of the file to be saved)
    procedure SaveToXML(Filename : TFileName);

    //Load the network from aa XML file
    //@param(Filename : name(and path) of the file to be loaded)
    procedure LoadFromXML(Filename : TFileName);

    //Organize visually the network according to its worlds
    //@param(w: number of worlds)
    procedure VisualOrgWorlds(w : integer);

    //Organize visually the network without considering the worlds
    //@param(x: width of the visual representation of the network)
    //@param(y: height of the visual representation of the network)
    procedure VisualOrgXY(x, y : integer);

    //Broad definition of the Organisation
    //@param(Height: Height of the visual representation og the network)
    //@param(LayerCount : Number of layers)
    //@param(LayerXPositions: Individual Position of each layer)
    procedure VisualOrg(Height, LayerCount : integer; LayerXPositions : array of integer);

    //Update the strings labelling the arcs on the fieldFArcs (TStringList instance)
    procedure UpdateArcs;

    //Set the Eta property of all neurons on the network
    //@param(value: New eta for the neurons)
    procedure SetNEta(value : double);

    //Set the Momentum property of all neurons on the network
    //@param(value: Momentum eta for the neurons)
    procedure SetNMomentum(value : double);

    //Connect all the of each layer to all the units of the next
    procedure FullyConnectDefault;

    //Connect all the of each layer to all the units of the next
    procedure FullyConnect(ConWeight, ConRange : double; ConFixed : boolean);

    //Add a neuron on an specific layer fully connected with the prior and the next layers
    //@param(lay: Layer to be inserted the neuron)
    function  AddConnectedNeuron(lay : integer) : integer;

    //Add a neuron on the first layer, fully connected to the first hidden layer
    //@param(ExtInput: Insert also an input unit)
    //@param(link: Insert also an TRecLink object)
    function  AddConnectedInputNeuron(ExtInput, link : boolean) : integer;

    //Add a neuron on the last layer, fully connected from the lasthidden layer
    //@param(ExtOutput: Insert also an output unit)
    //@param(link: Insert also an TRecLink object)
    function  AddConnectedOutputNeuron(ExtOutput, link : boolean) : integer;

    //Generate a neuron for each input unit and connect them)
    //@param(Full: indicates if the neuron is fully connected to the next layer)
    procedure ConnectFreeInputUnits(Full : boolean);

    //Save network on .dot format (graph)
    //@param(FileName: Name of the file to be saved)
    procedure SaveToDotFile(FileName : TFileName);

    //Load a new feed forward configuration on the network
    //@param(layers: number of layers on the network)
    //@param(neurons: number of neurons on each layer)
    //@param(pattern: neuron containing the default configuration for the units on the network)
    procedure LoadFeedForward(layers: integer; neurons : array of integer; Pattern : TNeuronRep);

    //Load a new Elman configuration on the network
    //@param(inputs: number of inputs)
    //@param(hidden: number of hidden neurons (same as context units))
    //@param(outputs: number of outputs)
    //@param(pattern: neuron containing the default configuration for the units on the network)
    procedure LoadElman(inputs, hidden, outputs : integer; Pattern : TNeuronRep);

    //Load a new CILP/SCTL-like configuration on the network
    //@param(inputs: number of inputs)
    //@param(hidden: number of hidden neurons)
    //@param(outputs: number of outputs)
    //@param(cont:  number of context units)
    //@param(conti: number of context units receiving links from actual outputs)
    //@param(temp:  number of "temporal" units)
    //@param(tempi: number of "temporal" units receiving links from actual outputs)
    //@param(pattern: neuron containing the default configuration for the units on the network)
    procedure LoadCILPLike(inputs, hidden, outputs, cont, ci, temp, tempi : integer; Pattern : TNeuronRep);

    //Gets the number of input units on the network
    //@return(number of input units on the network)
    function GetInputCount : integer;

    //Gets the number of output units on the network
    //@return(number of output units on the network)
    function GetOutputCount : integer;

    //Merge two networks
    //@param(TmpNet: Net to be integrated to the original)
    //@param(Style: Way to perform the merging)
    //@param(Level: level of the links to connect both the networks)
    procedure MergeNetwork(TmpNet : TNetworkRep; Style : TMergeStyle; Level : integer);


    //Group the units on the network in layers
    //@param(layers(out): array to be loaded with the layer of each unit);
    //@return(Number of layers on the network)
    function GetLayers(out layers : array of integer) : integer;

    //Add RecLinkRep units receiving connection from each hidden neuron
    function AddLinksToHiddenNeurons(level : integer) : integer;

    //Exchange the position of two different Nodes in the list
    //@param(position1, position2: positions to be exchanged)
    //@param(justMove: if true, the node in position 1 will move to position 2, and all the nodes
    //       between the positions will be moved relocated)
    //@return(true if the operation was successfuol)
    function ExchangeNodes(position1, position2 : integer; justMove : boolean = false) : boolean;

    procedure AlterSCTLtoExtraction;


  end;

{------------------------------------------------------------------------------}

implementation

uses UnRafaAux2007, ADomCore;

{------------------------------------------------------------------------------}

{ TConnecRep }

{------------------------------------------------------------------------------}

constructor TConnecRep.Create(Psource, Ptarget: integer);
  begin
  inherited Create;
  FRandom := 1;
  FFixed  := false;
  FWeight := 0;
  FSource := PSource;
  FTarget := PTarget;
  end;

{------------------------------------------------------------------------------}

constructor TConnecRep.CreateCopy(POrig: TConnecRep);
  begin
  Create(POrig.Source, POrig.Target);
  FWeight   := POrig.Weight;
  FRandom   := POrig.RandomRange;
  FFixed    := POrig.isFixed;
  end;

{------------------------------------------------------------------------------}

destructor TConnecRep.Destroy;
  begin
  inherited;
  end;

{------------------------------------------------------------------------------}

function TConnecRep.GetFixed: boolean;
  begin
  result := FFixed;
  end;

{------------------------------------------------------------------------------}

function TConnecRep.GetRandom: double;
  begin
  result := FRandom;
  end;

{------------------------------------------------------------------------------}

function TConnecRep.GetSource: integer;
  begin
  result := FSource;
  end;

{------------------------------------------------------------------------------}

function TConnecRep.GetTarget: integer;
  begin
  result := FTarget;
  end;

{------------------------------------------------------------------------------}

function TConnecRep.GetWeight: double;
  begin
  result := FWeight;
  end;
{------------------------------------------------------------------------------}

procedure TConnecRep.SetFixed(const Value: boolean);
  begin
  FFixed := value;
  end;

{------------------------------------------------------------------------------}

procedure TConnecRep.SetRandom(value: double);
  begin
  FRandom := value;
  end;

{------------------------------------------------------------------------------}

procedure TConnecRep.SetSource(value: integer);
  begin
  FSource := value;
  end;

{------------------------------------------------------------------------------}

procedure TConnecRep.SetTarget(value: integer);
  begin
  FTarget := value;
  end;

{------------------------------------------------------------------------------}

procedure TConnecRep.SetWeight(value: double);
  begin
  FWeight := value;
  end;

{------------------------------------------------------------------------------}

{ TUnitRep }

{------------------------------------------------------------------------------}

constructor TUnitRep.Create(PName: string);
  begin
  inherited Create;
  FWorld := -1;
  FName := PName;
  FX := 0;
  FY := 0;
  FTag := -1;
  end;

{------------------------------------------------------------------------------}

constructor TUnitRep.CreateCopy(POrig: TUnitRep);
  begin
  Create(POrig.Name);
  FX := POrig.X;
  FY := POrig.Y;
  Ftag := POrig.Tag;
  FWorld := POrig.World;
  end;

{------------------------------------------------------------------------------}

function TUnitRep.GetName: string;
  begin
  result := FName;
  end;

{------------------------------------------------------------------------------}

function TUnitRep.GetTag: integer;
  begin
  result := FTag;
  end;

{------------------------------------------------------------------------------}

function TUnitRep.GetWorld: integer;
  begin
  result := FWorld;
  end;

{------------------------------------------------------------------------------}

function TUnitRep.GetX: integer;
  begin
  result := FX;
  end;

{------------------------------------------------------------------------------}

function TUnitRep.GetY: integer;
  begin
  result := FY;
  end;

{------------------------------------------------------------------------------}

procedure TUnitRep.SetName(value: string);
  begin
  Fname := value;
  end;

{------------------------------------------------------------------------------}

procedure TUnitRep.SetTag(value: integer);
  begin
  FTag := value;
  end;

{------------------------------------------------------------------------------}

procedure TUnitRep.SetWorld(const Value: integer);
  begin
  FWorld := Value;
  end;

{------------------------------------------------------------------------------}

procedure TUnitRep.SetX(value: integer);
  begin
  FX := value;
  end;

{------------------------------------------------------------------------------}

procedure TUnitRep.SetY(value: integer);
  begin
  FY := value;
  end;

{------------------------------------------------------------------------------}

{ TDelayRep }

{------------------------------------------------------------------------------}

constructor TDelayRep.Create(PName: string);
  begin
  inherited Create(PName);
  FDelayFunction := 0;
  FTimes := 2;
  end;
{------------------------------------------------------------------------------}

constructor TDelayRep.CreateCopy(POrig: TDelayRep);
  begin
  inherited CreateCopy(POrig);
  FDelayFunction := POrig.DelayFunction;
  FTimes := POrig.Times;
  end;

{------------------------------------------------------------------------------}

destructor TDelayRep.Destroy;
  begin
  inherited;
  end;

{------------------------------------------------------------------------------}

function TDelayRep.GetDelayFunction: integer;
  begin
  Result := FDelayFunction;
  end;

{------------------------------------------------------------------------------}

function TDelayRep.getKind: TUnitKind;
  begin
  result := UKDelay;
  end;

{------------------------------------------------------------------------------}

function TDelayRep.GetTimes: integer;
  begin
  result := FTimes;
  end;

{------------------------------------------------------------------------------}

procedure TDelayRep.SetDelayFunction(value: integer);
  begin
  FDelayFunction := value;
  end;

{------------------------------------------------------------------------------}

procedure TDelayRep.SetTimes(value: integer);
  begin
  FTimes := value;
  end;

{------------------------------------------------------------------------------}

{ TNeuronRep }

{------------------------------------------------------------------------------}

procedure TNeuronRep.AddFunctionParam(param: double);
  begin
  FParamCount := FParamCount + 1;
  SetLength(FFunctionParams, FParamCount);
  FFunctionParams[FParamCount - 1] := param;
  end;

{------------------------------------------------------------------------------}

procedure TNeuronRep.ClearFunctionParams;
  begin
  FParamCount := 0;
  SetLength(FFunctionParams, 0);
  end;

{------------------------------------------------------------------------------}

constructor TNeuronRep.Create(PName: string);
  begin
  inherited Create(PName);
  FEta := 0.01;
  FMomentum := 0;
  FBiased := True;
  FBiasWeight := 0;
  FRandomBias := 1;
  FFixedBias := false;
  FAtivFunction := 0;
  FParamCount := 0;
  end;

{------------------------------------------------------------------------------}

constructor TNeuronRep.CreateCopy(POrig: TNeuronRep);
  var i : integer;
  begin
  inherited CreateCopy(POrig);
  FBiased := POrig.HasBias;
  FRandomBias := POrig.RandomBias;
  FFixedBias := POrig.FixedBias;
  FBiasWeight := POrig.BiasWeight;
  FAtivFunction := POrig.AtivFunction;
  FParamCount   := POrig.ParamCount;
  SetLength(FFunctionParams, FParamCount);
  for i := 0 to FParamCount - 1 do
    FFunctionParams[i] := POrig.Param[i];
  FEta := POrig.Eta;
  FMomentum := POrig.Momentum;
  end;

{------------------------------------------------------------------------------}

destructor TNeuronRep.Destroy;
  begin
  SetLength(FFunctionParams, 0);
  inherited;
  end;

{------------------------------------------------------------------------------}

function TNeuronRep.GetAtivFunction: integer;
  begin
  result := FAtivFunction;
  end;

{------------------------------------------------------------------------------}

function TNeuronRep.GetBiased: boolean;
  begin
  result := FBiased;
  end;

{------------------------------------------------------------------------------}

function TNeuronRep.GetBiasWeight: Double;
  begin
  result := FBiasWeight;
  end;

{------------------------------------------------------------------------------}

function TNeuronRep.GetEta: double;
  begin
  result := FEta;
  end;

{------------------------------------------------------------------------------}

function TNeuronRep.GetFixedBias: boolean;
  begin
  result := FFixedBias;
  end;

{------------------------------------------------------------------------------}

function TNeuronRep.GetFunctionParam(i: integer): double;
  begin
  if (i >= 0) and (i < FParamCount) then
     result := FFunctionParams[i]
  else
     result := 0;
  end;

{------------------------------------------------------------------------------}

function TNeuronRep.getKind: TUnitKind;
  begin
  result := UKNeuron;
  end;

{------------------------------------------------------------------------------}

function TNeuronRep.GetMomentum: double;
  begin
  result := FMomentum;
  end;

{------------------------------------------------------------------------------}

function TNeuronRep.GetParamCount: integer;
  begin
  result := FParamCount;
  end;

{------------------------------------------------------------------------------}

function TNeuronRep.GetRandomBias: double;
  begin
  result:= FRandomBias;
  end;

{------------------------------------------------------------------------------}


procedure TNeuronRep.SetAtivFunction(value: integer);
  begin
  if value >= 0 then
     FAtivFunction := value;
  end;

{------------------------------------------------------------------------------}

procedure TNeuronRep.SetBiased(value: boolean);
  begin
  FBiased := value;
  end;

{------------------------------------------------------------------------------}

procedure TNeuronRep.SetBiasWeight(value: double);
  begin
  FBiasWeight := value;
  end;

{------------------------------------------------------------------------------}

procedure TNeuronRep.SetEta(value: double);
  begin
  if (value >= 0) and (value <= 1) then
     FEta := value;
  end;
{------------------------------------------------------------------------------}

procedure TNeuronRep.SetFixedBias(const Value: boolean);
  begin
  FFixedBias := value;
  end;


{------------------------------------------------------------------------------}

procedure TNeuronRep.SetFunctionParam(i: integer; const Value: double);
  begin
  if (i >= 0) and (i < FParamCount) then
     FFunctionParams[i] := value
  else
     if i = FParamCount then
        AddFunctionParam(value);
  end;
{------------------------------------------------------------------------------}

procedure TNeuronRep.SetMomentum(const Value: double);
  begin
  if (value >= 0) and (value <= 1) then
     FMomentum := value;
  end;

{------------------------------------------------------------------------------}

procedure TNeuronRep.SetRandomBias(const Value: double);
  begin
  FRandomBias := value;
  end;

{------------------------------------------------------------------------------}

{ TIOUnitRep }

{------------------------------------------------------------------------------}

constructor TIOUnitRep.Create(PName: string; PInput: boolean);
  begin
  inherited Create(PName);
  FInput  := PInput;
  FInMax  := 1;
  FInMin  := -1;
  FOutMax := 1;
  FOutMin := -1;
  end;

{------------------------------------------------------------------------------}

constructor TIOUnitRep.CreateCopy(POrig: TIOUnitRep);
  begin
  inherited CreateCopy(POrig);
  FInput  := POrig.IsInput;
  FInMax  := POrig.InMax;
  FInMin  := POrig.InMin;
  FOutMax := POrig.OutMax;
  FOutMin := POrig.OutMin;
  end;

{------------------------------------------------------------------------------}

destructor TIOUnitRep.Destroy;
  begin
  inherited;
  end;

{------------------------------------------------------------------------------}

function TIOUnitRep.GetInMax: double;
  begin
  result := FInMax;
  end;

{------------------------------------------------------------------------------}

function TIOUnitRep.GetInMin: double;
  begin
  result := FInMin;
  end;

{------------------------------------------------------------------------------}

function TIOUnitRep.GetInput: boolean;
  begin
  result := FInput;
  end;

{------------------------------------------------------------------------------}

function TIOUnitRep.getKind: TUnitKind;
  begin
  if Finput then
     result := UKInput
  else
     result := UKOutput;
  end;

{------------------------------------------------------------------------------}

function TIOUnitRep.GetOutMax: double;
  begin
  result := FOutMax;
  end;

{------------------------------------------------------------------------------}

function TIOUnitRep.GetOutMin: double;
  begin
  result := FOutMin;
  end;

{------------------------------------------------------------------------------}

procedure TIOUnitRep.SetInMax(value: double);
  begin
  FInMax := value;
  end;

{------------------------------------------------------------------------------}

procedure TIOUnitRep.SetInMin(value: double);
  begin
  FInMin := value;
  end;

{------------------------------------------------------------------------------}

procedure TIOUnitRep.SetInput(value: boolean);
  begin
  FInput := value;
  end;

{------------------------------------------------------------------------------}

procedure TIOUnitRep.SetOutMax(value: double);
  begin
  FOutMax := value;
  end;

{------------------------------------------------------------------------------}

procedure TIOUnitRep.SetOutMin(value: double);
  begin
  FOutMin := value;
  end;

{------------------------------------------------------------------------------}

{ TRecLinkRep }

{------------------------------------------------------------------------------}

constructor TRecLinkRep.Create(PName: string);
  begin
  inherited Create(PName);
  FLevel := -1;
  FInitValue := 0;
  LinkBackStyle := LbsEach;
  end;

{------------------------------------------------------------------------------}

constructor TRecLinkRep.CreateCopy(POrig: TRecLinkRep);
  begin
  inherited CreateCopy(POrig);
  FLevel  := POrig.Level;
  FInitValue  := POrig.InitValue;
  FLinkBackStyle := POrig.LinkBackStyle;
  end;

{------------------------------------------------------------------------------}

destructor TRecLinkRep.Destroy;
  begin
  inherited;
  end;

{------------------------------------------------------------------------------}

function TRecLinkRep.GetInitValue: double;
  begin
  result := FInitValue;
  end;

{------------------------------------------------------------------------------}

function TRecLinkRep.getKind: TUnitKind;
  begin
  result := UkRec;
  end;

{------------------------------------------------------------------------------}

function TRecLinkRep.GetLevel: integer;
  begin
  result := FLevel;
  end;

{------------------------------------------------------------------------------}

function TRecLinkRep.GetLinkBackStyle: TLinkBackStyle;
  begin
  result := FLinkBackStyle;
  end;

{------------------------------------------------------------------------------}

procedure TRecLinkRep.SetInitValue(const Value: double);
  begin
  FInitValue := value;
  end;

{------------------------------------------------------------------------------}

procedure TRecLinkRep.SetLevel(const Value: integer);
  begin
  FLevel := value;
  end;

{------------------------------------------------------------------------------}

procedure TRecLinkRep.SetLinkBackStyle(const Value: TLinkBackStyle);
  begin
  FLinkBackStyle := value;
  end;

{------------------------------------------------------------------------------}

{ TNetworkRep }

{------------------------------------------------------------------------------}

function TNetworkRep.AddArc(PSource, PTarget: integer): integer;
  var
    tmp : TConnecRep;
    s : string;
  begin
  if (PSource >= 0) and (PSource < FNodes.Count) and
     (PTarget >= 0) and (PTarget < FNodes.Count) then
     begin
     tmp := TConnecRep.Create(PSource, PTarget);
     s := FNodes.Strings[PSource] + ' --> ' + FNodes.Strings[PTarget];
     FArcs.AddObject(s, tmp);
     result := FArcs.Count - 1;
     end
  else
     result := -1;
  end;

{------------------------------------------------------------------------------}

function TNetworkRep.AddArcObject(PObj: TConnecRep): integer;
  var s : string;
  begin
  if (PObj.Source >= 0) and (PObj.Source < FNodes.Count) and
     (PObj.Target >= 0) and (PObj.Target < FNodes.Count) then
     begin
     s := FNodes.Strings[PObj.Source] + ' --> ' + FNodes.Strings[PObj.Target];
     FArcs.AddObject(s, PObj);
     result := FArcs.Count - 1;
     end
  else
     result := -1;
  end;

{------------------------------------------------------------------------------}

function  TNetworkRep.AddConnectedInputNeuron(ExtInput, link : boolean) : integer;
  var
    layers  : array of integer;
    i, j, k, l : integer;
  begin
  SetLength(layers, FNodes.Count + 1);
  GetLayers(layers);
  j  := AddNode('Extra' + inttostr(FNodes.Count), UkNeuron);
  (FNodes.Objects[j] as TUnitRep).World := 0;
  layers[j] := 0;
  for i := 0 to FNodes.Count - 1 do
    if layers[i] = 1 then
       begin
       k := AddArc(j, i);
       Arc[k].Weight := 0;
       Arc[k].isFixed := false;
       Arc[k].RandomRange := 1;
       end;

  if link then
     begin
     i := AddNode('Extra' + inttostr(FNodes.Count), UkRec);
     (FNodes.Objects[i] as TRecLinkRep).World := 0;
     (FNodes.Objects[i] as TRecLinkRep).Level := 3;
     k := AddArc(i, j);
     Arc[k].Weight := 1;
     Arc[k].isFixed := true;
     Arc[k].RandomRange := 0;
     if ExtInput then
        begin
        l := AddNode('Extra' + inttostr(FNodes.Count), UKInput);
        (FNodes.Objects[l] as TIOUnitRep).World := 0;
        k := AddArc(l, i);
        Arc[k].Weight := 1;
        Arc[k].isFixed := true;
        Arc[k].RandomRange := 0;
        end;
     end
  else if ExtInput then
     begin
     i := AddNode('Extra' + inttostr(FNodes.Count), UkInput);
     (FNodes.Objects[i] as TIOUnitRep).World := 0;
     k := AddArc(i, j);
     Arc[k].Weight := 1;
     Arc[k].isFixed := true;
     Arc[k].RandomRange := 0;
     end;
  result := j;
  end;

{------------------------------------------------------------------------------}

function TNetworkRep.AddConnectedNeuron(lay: integer): integer;
  var
    layers  : array of integer;
    i, j, k, tot : integer;

  begin
  SetLength(layers, FNodes.Count + 1);
  tot := GetLayers(layers);
  j := -1;
  if (lay >= 0) and (lay <= tot) then
     begin
     j := AddNode('Extra' + inttostr(FNodes.Count), UkNeuron);
     (FNodes.Objects[j] as TUnitRep).World := 0;
     (FNodes.Objects[j] as TNeuronRep).RandomBias := 0.1;
     layers[j] := lay;

     if lay > 0 then
        for i := 0 to FNodes.Count - 1 do
          if layers[i] = lay - 1 then
             begin
             k := AddArc(i, j);
             Arc[k].Weight := 0;
             Arc[k].isFixed := false;
             Arc[k].RandomRange := 0.1;
             end;

     if lay < tot then
        for i := 0 to FNodes.Count - 1 do
          if layers[i] = lay + 1 then
             begin
             k := AddArc(j, i);
             Arc[k].Weight := 0;
             Arc[k].isFixed := false;
             Arc[k].RandomRange := 0.1;
             end;
     end;
  result := j;
  end;

{------------------------------------------------------------------------------}

function  TNetworkRep.AddConnectedOutputNeuron(ExtOutput, link : boolean) : integer;
  var
    layers  : array of integer;
    i, j, k, l, tot : integer;
  begin
  SetLength(layers, FNodes.Count + 1);
  tot := GetLayers(layers);
  j  := AddNode('Extra' + inttostr(FNodes.Count), UkNeuron);
  (FNodes.Objects[j] as TUnitRep).World := 0;
  layers[j] := tot - 1;
  for i := 0 to FNodes.Count - 1 do
    if layers[i] = tot - 2 then
       begin
       k := AddArc(i, j);
       Arc[k].Weight := 0;
       Arc[k].isFixed := false;
       Arc[k].RandomRange := 1;
       end;

  if link then
     begin
     i := AddNode('Extra' + inttostr(FNodes.Count), UkRec);
     (FNodes.Objects[i] as TRecLinkRep).World := 0;
     (FNodes.Objects[i] as TRecLinkRep).Level := 2;
     k := AddArc(j, i);
     Arc[k].Weight := 1;
     Arc[k].isFixed := true;
     Arc[k].RandomRange := 0;
     if ExtOutput then
        begin
        l := AddNode('Extra' + inttostr(FNodes.Count), UKOutput);
        (FNodes.Objects[l] as TIOUnitRep).World := 0;
        k := AddArc(i, l);
        Arc[k].Weight := 1;
        Arc[k].isFixed := true;
        Arc[k].RandomRange := 0;
        end;
     end
  else if ExtOutput then
     begin
     i := AddNode('Extra' + inttostr(FNodes.Count), UkOutput);
     (FNodes.Objects[i] as TIOUnitRep).World := 0;
     k := AddArc(j, i);
     Arc[k].Weight := 1;
     Arc[k].isFixed := true;
     Arc[k].RandomRange := 0;
     end;
  result := j;
  end;

{------------------------------------------------------------------------------}

function TNetworkRep.AddLinksToHiddenNeurons(level: integer): integer;
  var
    ar : array of integer;
    Xtmp : TRecLinkRep;
    i, j, k, Xend, lay : integer;

  begin
  XEnd := FNodes.Count;
  SetLength(ar, XEnd);
  lay := GetLayers(ar);
  j := 0;
  for i := 0 to XEnd - 1 do
    begin
    if FNodes.Objects[i] is TNeuronRep then
       if (ar[i] > 0) and (ar[i] < (lay - 1)) then
          begin
          j := j + 1;
          XTmp := TRecLinkRep.Create('Extra' + inttostr(j));
          Xtmp.Level := level;
          k := AddNodeObject(Xtmp);
          AddArc(i, k);
          end;
    end;
  result := j;
  end;

{------------------------------------------------------------------------------}

function TNetworkRep.AddNode(PName: string; PKind: TUnitKind): integer;
  var
    tmp : TUnitRep;
  begin
  case PKind of
    UKInput  : tmp := TIOUnitRep.Create(PName, true);
    UKOutput : tmp := TIOUnitRep.Create(PName, false);
    UKNeuron : tmp := TNeuronRep.Create(PName);
    UKDelay  : tmp := TDelayRep.Create(PName);
    UKRec    : tmp := TRecLinkRep.Create(PName);
  else
    tmp := nil
    end;
  FNodes.AddObject(PName, tmp);
  result := FNodes.Count - 1;
  end;


{------------------------------------------------------------------------------}

function TNetworkRep.AddNodeObject(PObj: TUnitRep): integer;
  begin
  FNodes.AddObject(PObj.Name, PObj);
  result := FNodes.Count - 1;
  end;

{------------------------------------------------------------------------------}

procedure TNetworkRep.AlterSCTLtoExtraction;
  var
    i : integer;
    XInInd, XOutInd, XLinkInd, XArcInd, XInNeuInd, XState: integer;
    XTmpStrList : TStringList;
  begin
  XState := 0;
  XTmpStrList := TStringList.Create;
  for i := 0 to FNodes.Count - 1 do
    if GetNode(i) is TRecLinkRep then
       if (GetNode(i) as TRecLinkRep).Level = 1 then
          begin
          XTmpStrList.Clear;
          GetSpecArcsList(XTmpStrList, [i], true);
          if (XTmpStrList.Count = 1) then
             begin
             XState := XState + 1;
             (GetNode(i) as TRecLinkRep).Level := 2;
             XarcInd := (XTmpStrList.Objects[0] as TRafaInteger).Data;
             XInNeuInd := GetArc(XArcInd).Target;
             XInInd := AddNode('InState' + inttostr(XState), UKInput);
             XOutInd := AddNode('OutState' + inttostr(XState), UKOutput);
             XLinkInd := AddNode('RecState' + inttostr(XState), UkRec);
             (GetNode(XLinkInd) as TRecLinkRep).Level := 3;
             GetArc(XArcInd).Target := XOutInd;
             AddArc(XInInd, XLinkInd);
             GetArc(FArcs.Count - 1).Weight := 1;
             GetArc(FArcs.Count - 1).RandomRange := 0;
             AddArc(XLinkInd, XInNeuInd);
             GetArc(FArcs.Count - 1).Weight := 1;
             GetArc(FArcs.Count - 1).RandomRange := 0;
             end;
          end;
  XTmpStrList.Free;
  end;

{------------------------------------------------------------------------------}

procedure TNetworkRep.ClearArcs;
  var
    i : integer;
  begin
  for i := 0 to FArcs.Count - 1 do
    FArcs.Objects[i].Free;
  FArcs.Clear;
  end;

{------------------------------------------------------------------------------}

procedure TNetworkRep.ClearNodes;
  var
    i : integer;
  begin
  ClearArcs;
  for i := 0 to FNodes.Count - 1 do
    FNodes.Objects[i].Free;
  FNodes.Clear;
  end;

{------------------------------------------------------------------------------}

procedure TNetworkRep.ConnectFreeInputUnits(Full: boolean);
  var
    IsFree : array of boolean;
    i, j, k, tot : integer;
  begin
  tot := FNodes.Count;
  SetLength (isfree, tot);
  for i := 0 to tot - 1 do
    IsFree[i] := (((FNodes.Objects[i] as TUnitRep).Kind = UkInput) or
                 ((FNodes.Objects[i] as TUnitRep).Kind = UkRec));

  for i := 0 to FArcs.Count - 1 do
    begin
    j := (FArcs.Objects[i] as TConnecRep).Source;
    k := (FArcs.Objects[i] as TConnecRep).Target;
    if IsFree[k] and ((FNodes.Objects[k] as TUnitRep).Kind = UkRec) then
       IsFree[k] := (FNodes.Objects[j] as TUnitRep).Kind = UkInput
    end;

  for i := 0 to FArcs.Count - 1 do
    begin
    j := (FArcs.Objects[i] as TConnecRep).Source;
    if IsFree[j] then
       IsFree[j] := false;
    end;

  for i := 0 to tot - 1 do
    if IsFree[i] then
       begin
       if full then
          j := AddConnectedNeuron(0)
       else
          begin
          j := AddNode('InNeuron' + inttostr(i), UKNeuron);
          (FNodes.Objects[j] as TUnitRep).World := 0;
          end;
       AddArc(i, j);
       end;
  end;

{------------------------------------------------------------------------------}

constructor TNetworkRep.Create(PNodesList, PArcsList: TStrings; ExternalSource : boolean = false);
  begin
  inherited Create;
  if PNodesList = nil then
     FNodes := TStringList.Create
  else
     FNodes := PNodesList;
  if PArcsList = nil then
     FArcs := TStringList.Create
  else
     FArcs := PArcsList;
  FNodes.Clear;
  FArcs.Clear;
  FWorldCount := 0;
  FWorldLinkPoints := 0;
  FLinkFlow := LfNone;
  FNu := 1;
  FUpsilon := 1;
  FEtaCorrection := 1;
  FDestroyLists := not ExternalSource;
  end;

{------------------------------------------------------------------------------}

procedure TNetworkRep.DeleteArc(index: integer);
  begin
  if (index >= 0) and (index < FArcs.Count) then
     begin
     FArcs.Objects[index].Free;
     FArcs.Delete(index);
     end;
  end;

{------------------------------------------------------------------------------}

procedure TNetworkRep.DeleteNode(index: integer);
  var
    i : integer;
  begin
  if (index >= 0) and (index < FNodes.Count) then
     begin
     for i := FArcs.Count - 1 downto 0 do
       with (FArcs.Objects[i] as TConnecRep) do
         begin
         if (Source = index) or (Target = index) then
            DeleteArc(i)
         else
            begin
            if Source > index then
               source := source - 1;
            if Target > index then
               Target := Target - 1;
            end;
         end;
     FNodes.Objects[index].Free;
     FNodes.Delete(index);
     end;
  end;

{------------------------------------------------------------------------------}

destructor TNetworkRep.Destroy;
  begin
  ClearArcs;
  ClearNodes;
  if FDestroyLists then
     begin
     FNodes.Free;
     FArcs.Free;
     end;
  inherited;
  end;

{------------------------------------------------------------------------------}

procedure TNetworkRep.doFullyConnect(layers: array of integer; size: integer;
                                     ConWeight, ConRange : double; ConFixed : boolean);
  var
    Xdef    : array of array of boolean;
    i, j, k, l : integer;


  begin
  SetLength(XDef, FNodes.Count, FNodes.Count);

  for i := 0 to FNodes.Count - 1 do
    for j := 0 to FNodes.Count - 1 do
      if i = j then
         XDef[i, j] := true
      else
         XDef[i, j] := false;

  for i := 0 to FArcs.Count - 1 do
    with (FArcs.Objects[i]) as TConnecRep do
      XDef[Source, Target] := true;

  for i := 0 to FNodes.Count - 1 do
    begin
    l := layers[i];
    if (l >= 0) and (l < size - 1) then
       for j := 0 to FNodes.Count - 1 do
         if (layers[j] = l + 1) and not XDef[i, j] and (GetNode(i).World = GetNode(j).World) then
            begin
            k := AddArc(i, j);
            Arc[k].Weight := ConWeight;
            Arc[k].isFixed := ConFixed;
            Arc[k].RandomRange := ConRange;
            XDef[i, j] := true;
            end;
    end;
  end;

{------------------------------------------------------------------------------}

procedure TNetworkRep.doSaveDot(layers: array of integer; size: integer;
      FileName: TFileName);
  var
    StL : TstringList;
    s   : string;
    src, tgt : TUnitRep;
    i, j, l, cont : integer;
    XArr : array of boolean;
  begin
  StL := TStringList.Create;
  StL.Clear;



  Stl.Add('digraph Network {');
  Stl.Add('ranksep=0.5; rankdir = LR;');
  //Escreve rotulo dos labels
  Stl.Add('');
  Stl.Add('node [shape = plaintext, fontsize = 12]');


  s := '{"Inputs" -> ';
  for i := 1 to size - 1 do
    s := s + inttostr(i) + ' -> "' + inttostr(i) + '-' + inttostr(i + 1) + '" -> ';
  s := s + inttostr(size) + ' -> "Outputs"}';
  Stl.Add(s);

  s := '{rank = same; "Inputs";';
  for i := 0 to FNodes.Count - 1 do
    if (FNodes.Objects[i] as TUnitRep).kind = UKInput then
       s := s + ' "' + (FNodes.Objects[i] as TUnitRep).Name + '";';
  s := s + '}';
  Stl.Add(s);

  s := '{rank = same; "Outputs";';
  for i := 0 to FNodes.Count - 1 do
    if (FNodes.Objects[i] as TUnitRep).kind = UKOutput then
       s := s + ' "' + (FNodes.Objects[i] as TUnitRep).Name + '";';
  s := s + '}';
  Stl.Add(s);
  Stl.Add('');

  //Add Clusters
  cont := 1;
  l := -1;
  while (cont > 0) or (l < 3) do
    begin
    Stl.Add('subgraph cluster' + inttostr(l));
    stl.add('{');
    stl.add('  node [shape=ellipse,fontsize = 10];');
    stl.add('  color=grey;');
    stl.add('  label = "Level ' + inttostr(l) + '";');
    cont := 0;
    s := '{rank = same; ';
    for i := 0 to FArcs.Count - 1 do
      begin
      j := (FArcs.Objects[i] as TConnecRep).Source;
      if (FNodes.Objects[j] as TUnitRep).Kind = UKRec then
         if (FNodes.Objects[j] as TRecLinkRep).Level = l then
            begin
            j := (FArcs.Objects[i] as TConnecRep).Target;
            s := s + ' "' + (FNodes.Objects[j] as TUnitRep).Name + '"; ';
            cont := cont + 1;
            end;
      end;
    s := s + '}';
    if cont > 0 then
       Stl.Add(s);
    l := l + 1;
    stl.add('}');
    end;

  //Add Layers
  Stl.Add('node [shape = ellipse, fontsize = 10]');
  for i := 0 to size - 1 do
    begin
    s := '{rank = same;' + inttostr(i + 1) + ';';
    for j := 0 to FNodes.Count - 1 do
      if layers[j] = i then
         s := s + ' "' + (FNodes.Objects[j] as TUnitRep).Name + '";';
    s := s + '}';
    Stl.Add(s);
    end;
  Stl.Add('');

  Stl.Add('');
  Stl.Add('node [shape = point, fontsize = 6]');
  for i := 0 to FArcs.Count - 1 do
    begin
    src := FNodes.objects[(FArcs.Objects[i] as TConnecRep).Source] as TUnitRep;
    tgt := FNodes.objects[(FArcs.Objects[i] as TConnecRep).Target] as TUnitRep;
    if (Src.Kind <> UkRec) and (Tgt.Kind <> UKRec) then
       Stl.Add('"' + Src.Name + '" -> "' + Tgt.Name + '"')
    else if (Src.Kind = UkRec) and (Tgt.Kind = UKOutput) then
       Stl.Add('"' + Src.Name + '" -> "' + Tgt.Name + '"')
    else if (Src.Kind = UkInput) and (Tgt.Kind = UKRec) then
       Stl.Add('"' + Src.Name + '" -> "' + Tgt.Name+ '"');
    end;

  //Add Links
  Stl.Add('');
  Stl.Add('node [shape = plaintext, fontsize = 10]');

  SetLength(XArr, FNodes.Count);
  for i := 0 to FNodes.Count - 1 do
    if (FNodes.Objects[i] as TUnitRep).Kind = UKRec then
       XArr[i] := true
    else
       XArr[i] := false;

  for i := 0 to FArcs.Count - 1 do
    Xarr[(FArcs.Objects[i] as TConnecRep).target] := false;

  for i := 0 to FArcs.Count - 1 do
    begin
    src := FNodes.objects[(FArcs.Objects[i] as TConnecRep).Source] as TUnitRep;
    tgt := FNodes.objects[(FArcs.Objects[i] as TConnecRep).Target] as TUnitRep;
    if (Src.Kind = UkNeuron) and (Tgt.Kind = UKRec) then
       begin
       Stl.Add('"' + Src.Name + '" -> "' + Tgt.Name + '"');
       XArr[(FArcs.Objects[i] as TConnecRep).Target] := true;
       end;
    end;

  for i := 0 to FArcs.Count - 1 do
    begin
    src := FNodes.objects[(FArcs.Objects[i] as TConnecRep).Source] as TUnitRep;
    tgt := FNodes.objects[(FArcs.Objects[i] as TConnecRep).Target] as TUnitRep;
    if (Src.Kind = UKRec) and (Tgt.Kind = UkNeuron) then
       if XArr[(FArcs.Objects[i] as TConnecRep).Source] then
        Stl.Add('"' + Src.Name + '''" -> "' + Tgt.Name + '"')
     else
        Stl.Add('"' + Src.Name + '" -> "' + Tgt.Name + '"');
    end;

  for i := 0 to FArcs.Count - 1 do
    begin
    src := FNodes.objects[(FArcs.Objects[i] as TConnecRep).Source] as TUnitRep;
    tgt := FNodes.objects[(FArcs.Objects[i] as TConnecRep).Target] as TUnitRep;
    if (Src.Kind = UkRec) and (Tgt.Kind = UKOutput) then
      XArr[(FArcs.Objects[i] as TConnecRep).Source] := false;
    end;

  s := '{rank = same; "Outputs";';
  for i := 0 to FNodes.Count - 1 do
    if XArr[i] then
       s := s + ' "' + (FNodes.Objects[i] as TUnitRep).Name + '";';
  s := s + '}';
  Stl.Add(s);

  Stl.Add('}');

  Stl.SaveToFile(FileName);
  Stl.Free;

  end;

{------------------------------------------------------------------------------}

function TNetworkRep.ExchangeNodes(position1, position2 : integer; justMove : boolean = false) : boolean;
  var
    XTmpNode : TUnitRep;
    XTmpName : string;
    i, j, XInc : integer;

  begin
  if (position1 > 0) and (position1 < FNodes.Count) and
     (position2 > 0) and (position2 < FNodes.Count) then
     begin
     if position1 <> position2 then
        if justMove then
           begin
           XTmpName := FNodes.Strings[position1];
           XTmpNode := FNodes.Objects[position1] as TUnitRep;
           if position2 > position1 then XInc := 1 else XInc := -1;
           i := position1;
           while (i <> position2) do
             begin
             FNodes.Strings[i] := FNodes.Strings[i + XInc];
             FNodes.Objects[i] := FNodes.Objects[i + XInc];
             i := i + XInc;
             end;
           FNodes.Strings[position2] := XTmpName;
           FNodes.Objects[position2] := XTmpNode;
           for j := 0 to FArcs.Count - 1 do
             begin
             if GetArc(j).Source = position1 then
                GetArc(j).Source := position2
             else if (GetArc(j).Source > position1) and (GetArc(j).Source <= position2) then
                GetArc(j).Source := GetArc(j).Source - 1
             else if (GetArc(j).Source < position1) and (GetArc(j).Source >= position2) then
                GetArc(j).Source := GetArc(j).Source + 1;
             if GetArc(j).Target = position1 then
                GetArc(j).Target := position2
             else if (GetArc(j).Target > position1) and (GetArc(j).Target <= position2) then
                GetArc(j).Target := GetArc(j).Target - 1
             else if (GetArc(j).Target < position1) and (GetArc(j).Target >= position2) then
                GetArc(j).Target := GetArc(j).Target + 1;
             end;
           end
        else
           begin
           XTmpName := FNodes.Strings[position1];
           XTmpNode := FNodes.Objects[position1] as TUnitRep;
           FNodes.Strings[position1] := FNodes.Strings[position2];
           FNodes.Objects[position1] := FNodes.Objects[position2];
           FNodes.Strings[position2] := XTmpName;
           FNodes.Objects[position2] := XTmpNode;
           for i := 0 to FArcs.Count - 1 do
             begin
             if GetArc(i).Source = position1 then
                GetArc(i).Source := position2
             else if GetArc(i).Source = position2 then
                GetArc(i).Source := position1;
             if GetArc(i).Target = position1 then
                GetArc(i).Target := position2
             else if GetArc(i).Target = position2 then
                GetArc(i).Target := position1;
             end;
           end;
     result := true;
     end
  else
     result := false
  end;

{------------------------------------------------------------------------------}

procedure TNetworkRep.FullyConnectDefault;
  var
    layers  : array of integer;
    tot : integer;
  begin
  setLength(layers, FNodes.Count);
  tot := GetLayers(layers);
  DoFullyConnect(Layers, tot, 0, 1, false);
  end;

{------------------------------------------------------------------------------}

procedure TNetworkRep.FullyConnect(ConWeight, ConRange : double; ConFixed : boolean);
  var
    layers  : array of integer;
    tot : integer;
  begin
  setLength(layers, FNodes.Count);
  tot := GetLayers(layers);
  DoFullyConnect(Layers, tot, ConWeight, ConRange, ConFixed);
  end;

{------------------------------------------------------------------------------}


function TNetworkRep.GetArc(index: integer): TConnecRep;
  begin
  if (index >= 0) and (index < FArcs.Count) then
     result := FArcs.objects[index] as TConnecRep
  else
     result := nil;
  end;

{------------------------------------------------------------------------------}

function TNetworkRep.GetArcList: TStrings;
  begin
  result := FArcs;
  end;

{------------------------------------------------------------------------------}

procedure TNetworkRep.GetCopy(cp: TNetworkRep);
  var
    i : integer;
    tmp : TUnitRep;

  begin
  cp.ClearArcs;
  cp.ClearNodes;
  cp.LinkFlow := FLinkFlow;
  cp.WorldCount := FWorldCount;
  cp.WorldLinks := FWorldLinkPoints;
  cp.EtaCorrection := FEtaCorrection;
  cp.Nu := FNu;
  cp.Upsilon := FUpsilon;

  for i := 0 to FNodes.Count - 1 do
    begin
    if FNodes.Objects[i] is TNeuronRep then
       tmp := TNeuronRep.CreateCopy(TNeuronRep(FNodes.Objects[i]))
    else if FNodes.Objects[i] is TDelayRep then
       tmp := TDelayRep.CreateCopy(TDelayRep(FNodes.Objects[i]))
    else if FNodes.Objects[i] is TIOUnitRep then
       tmp := TIOUnitRep.CreateCopy(TIOUnitRep(FNodes.Objects[i]))
    else if FNodes.Objects[i] is TRecLinkRep then
       tmp := TRecLinkRep.CreateCopy(TRecLinkRep(FNodes.Objects[i]))
    else
       tmp := nil;
    cp.AddNodeObject(tmp);
    end;

  for i := 0 to FArcs.Count - 1 do
    cp.AddArcObject(TConnecRep.CreateCopy(FArcs.Objects[i] as TConnecRep));
  end;

{------------------------------------------------------------------------------}

function TNetworkRep.GetEtaCorrection: double;
  begin
  result := FEtaCorrection;
  end;

{------------------------------------------------------------------------------}

function TNetworkRep.GetInput(index: integer): TIOUnitRep;
  var
    i, j : integer;
    Xtmp : TIoUnitRep;
    XStop : boolean;
  begin
  XTmp := nil;
  i := 0;
  j := 0;
  XStop := false;
  while not XStop and (i < FNodes.Count) do
    begin
    if (FNodes.Objects[i] as TUnitRep).Kind = UkInput then
       begin
       if j = index then
          begin
          XTmp := FNodes.Objects[i] as TIOUnitRep;
          XStop := true;
          end
       else
          j := j + 1;
       end;
    i := i + 1;
    end;
  result := XTmp;
  end;

{------------------------------------------------------------------------------}

function TNetworkRep.GetInputCount: integer;
  var i, j : integer;
  begin
  j := 0;
  for i := 0 to FNodes.Count - 1 do
    if (FNodes.Objects[i] as TUnitRep).Kind = UkInput then
       j := j + 1;
  result := j;
  end;

{------------------------------------------------------------------------------}

function TNetworkRep.GetLayers(out layers: array of integer): integer;
  var
    i, j, tot, tmp, max: integer;
  begin
  for i:= 0 to FNodes.Count - 1 do
    if (FNodes.Objects[i] is TNeuronRep) then
       layers[i] := -2
    else
       layers[i] := -3;

  for i := 0 to FArcs.Count - 1 do
    if FNodes.Objects[(FArcs.Objects[i] as TConnecRep).FTarget] is TNeuronRep then
       if layers[(FArcs.Objects[i] as TConnecRep).FSource] = -3 then
          layers[(FArcs.Objects[i] as TConnecRep).FTarget] := 0
       else
          if layers[(FArcs.Objects[i] as TConnecRep).FTarget] < 0 then
             layers[(FArcs.Objects[i] as TConnecRep).FTarget] := -1;

  for i:= 0 to FNodes.Count - 1 do
    if layers[i] = -2 then layers[i] := 0;

  tot := 0;
  j   := 0;
  max := 0;

  while tot < FArcs.Count do
    begin
    tmp := layers[(FArcs.Objects[j] as TConnecRep).FSource];
    if (tmp > -1) then
       begin
       i := (FArcs.Objects[j] as TConnecRep).FTarget;
       if (layers[i] = -1) or (layers[i] > tmp + 1) then
          begin
          tot := 0;
          layers[i] := tmp + 1;
          if (tmp >= max) then max := tmp + 1;
          end;
       end;
    j := (j + 1) mod FArcs.Count;
    tot := tot + 1;
    end;

  result := max + 1;
  end;

{------------------------------------------------------------------------------}

function TNetworkRep.GetLinkFlow: TLinkFlow;
  begin
  result := FLinkFlow;
  end;

{------------------------------------------------------------------------------}

function TNetworkRep.GetNoArcs: integer;
  begin
  result := FArcs.Count;
  end;

{------------------------------------------------------------------------------}

function TNetworkRep.GetNode(index: integer): TUnitRep;
  begin
  if (index >= 0) and  (index < FNodes.Count) then
     result := FNodes.objects[index] as TUnitRep
  else
     result := nil;
  end;

{------------------------------------------------------------------------------}

function TNetworkRep.GetNodeList: TStrings;
  begin
  result := FNodes;
  end;

{------------------------------------------------------------------------------}

function TNetworkRep.GetNoNodes: integer;
  begin
  result := FNodes.Count;
  end;

{------------------------------------------------------------------------------}

function TNetworkRep.GetNu: integer;
  begin
  result := FNu;
  end;

{------------------------------------------------------------------------------}

function TNetworkRep.GetOutput(index: integer): TIOUnitRep;
  var
    i, j : integer;
    Xtmp : TIoUnitRep;
    XStop : boolean;
  begin
  XTmp := nil;
  i := 0;
  j := 0;
  XStop := false;
  while not XStop and (i < FNodes.Count) do
    begin
    if (FNodes.Objects[i] as TUnitRep).Kind = UkOutput then
       begin
       if j = index then
          begin
          XTmp := FNodes.Objects[i] as TIOUnitRep;
          XStop := true;
          end
       else
          j := j + 1;
       end;
    i := i + 1;
    end;
  result := XTmp;
  end;

{------------------------------------------------------------------------------}

function TNetworkRep.GetOutputCount: integer;
  var i, j : integer;
  begin
  j := 0;
  for i := 0 to FNodes.Count - 1 do
    if (FNodes.Objects[i] as TUnitRep).Kind = UkOutput then
       j := j + 1;
  result := j;
  end;

{------------------------------------------------------------------------------}
                                                   
function TNetworkRep.GetRecLevels: integer;
  var i, j : integer;
  begin
  j := 0;
  for i := 0 to FNodes.Count - 1 do
    if (FNodes.Objects[i] as TUnitRep).Kind = UkRec then
       if (FNodes.Objects[i] as TRecLinkRep).Level >=  j then
          j := (FNodes.Objects[i] as TRecLinkRep).Level + 1;
  result := j;
  end;

{------------------------------------------------------------------------------}

procedure TNetworkRep.GetSpecArcsList(list: TStrings; indexes: array of integer; source: boolean);
  var
    i, j : integer;
    XIsIndex : boolean;
  begin
  for i := list.Count - 1 downto 0 do
    list.Objects[i].Free;
  list.Clear;
  if source then
    for i := 0 to FArcs.Count - 1 do
      begin
      XIsIndex := false;
      j := 0;
      while not XIsIndex and (j < length(indexes)) do
        begin
        XIsIndex := ((FArcs.Objects[i] as TConnecRep).source = indexes[j]);
        j := j + 1;
        end;
      if XIsIndex then
         list.AddObject('--> ' + FNodes.strings[(FArcs.Objects[i] as TConnecRep).target], TRafaInteger.Create(i));
      end
  else
    for i := 0 to FArcs.Count - 1 do
      begin
      XIsIndex := false;
      j := 0;
      while not XIsIndex and (j < length(indexes)) do
        begin
        XIsIndex := ((FArcs.Objects[i] as TConnecRep).Target = indexes[j]);
        j := j + 1;
        end;
      if XIsIndex then
         list.AddObject(FNodes.strings[(FArcs.Objects[i] as TConnecRep).source] + ' -->', TRafaInteger.Create(i));
      end;
  end;

{------------------------------------------------------------------------------}

function TNetworkRep.GetUpsilon: integer;
  begin
  result := FUpsilon;
  end;

{------------------------------------------------------------------------------}

function TNetworkRep.GetWorldCount: integer;
  begin
  result := FWorldCount;
  end;

{------------------------------------------------------------------------------}

function TNetworkRep.GetWorldHeights(index: integer): integer;
  begin
  if (index >= 0) and (index < FWorldCount) then
     result := FWorldHeights[index]
  else
     result := 0;
  end;

{------------------------------------------------------------------------------}

function TNetworkRep.GetWorldLefts(index: integer): integer;
  begin
  if (index >= 0) and (index < FWorldCount) then
     result := FWorldLefts[index]
  else
     result := 0;
  end;

{------------------------------------------------------------------------------}

function TNetworkRep.GetWorldLinks: integer;
  begin
  result := FWorldLinkPoints;
  end;

{------------------------------------------------------------------------------}

function TNetworkRep.GetWorldTops(index: integer): integer;
  begin
  if (index >= 0) and (index < FWorldCount) then
     result := FWorldTops[index]
  else
     result := 0;
  end;

{------------------------------------------------------------------------------}

function TNetworkRep.GetWorldWidths(index: integer): integer;
  begin
  if (index >= 0) and (index < FWorldCount) then
     result := FWorldWidths[index]
  else
     result := 0;
  end;

{------------------------------------------------------------------------------}

procedure TNetworkRep.LoadCILPLike(inputs, hidden, outputs, cont, ci, temp,
  tempi: integer; Pattern: TNeuronRep);
  var
    i, j, k, DelCount, Xin, Xout, Xout2 : integer;
    ArLinks, ArDeletable : array of integer;

  begin
  SetLength(ArLinks, inputs + cont + temp + outputs);
  LoadFeedForward(3, [inputs + cont + temp, hidden,
                            (outputs + (cont - ci) + (temp - tempi))], pattern);
  DelCount := 0;
  for i := 0 to inputs + cont + temp + outputs - 1 do
    begin
    if i < inputs then
       begin
       ArLinks[i] := AddNode('Rec_' + inttostr(i), UkRec);
       (Node[ArLinks[i]] as TRecLinkRep).Level := 3;
       (Node[ArLinks[i]] as TRecLinkRep).World := -1;
       (Node[ArLinks[i]] as TRecLinkRep).linkbackStyle := LbsEach;
       end
    else if i < inputs + cont then
       begin
       ArLinks[i] := AddNode('Rec_' + inttostr(i), UkRec);
       (Node[ArLinks[i]] as TRecLinkRep).Level := 0;
       (Node[ArLinks[i]] as TRecLinkRep).World := -1;
       (Node[ArLinks[i]] as TRecLinkRep).linkbackStyle := LbsEach;
       end
    else if i < inputs + cont + temp then
       begin
       ArLinks[i] := AddNode('Rec_' + inttostr(i), UkRec);
       (Node[ArLinks[i]] as TRecLinkRep).Level := 1;
       (Node[ArLinks[i]] as TRecLinkRep).World := -1;
       (Node[ArLinks[i]] as TRecLinkRep).linkbackStyle := LbsEach;
       end
    else
       begin
       ArLinks[i] := AddNode('Rec_' + inttostr(i), UkRec);
       (Node[ArLinks[i]] as TRecLinkRep).Level := 2;
       (Node[ArLinks[i]] as TRecLinkRep).World := -1;
       (Node[ArLinks[i]] as TRecLinkRep).linkbackStyle := LbsEach;
       end
    end;

  k := FArcs.Count;
  Xin  := 0;
  Xout := 0;
  XOut2 := inputs + cont + temp;
  for i := 0 to k - 1 do
    begin
    if Node[Arc[i].Source].Kind = UkInput then
       begin
       if Xin < inputs then
          begin
          j := AddArc(Arc[i].Source, ArLinks[Xin]);
          Arc[j].Weight := 1;
          Arc[j].RandomRange := 0;
          Arc[j].isFixed := true;
          Arc[i].Source := ArLinks[Xin];
          Arc[i].Weight := 1;
          Arc[i].RandomRange := 0;
          Arc[i].isFixed := true;
          end
       else if Xin < inputs + cont + temp then
          begin
          DelCount := DelCount + 1;
          SetLength(ArDeletable, DelCount);
          ArDeletable[DelCount - 1] := Arc[i].Source;
          Arc[i].Source := ArLinks[Xin];
          Arc[i].Weight := 1;
          Arc[i].RandomRange := 0;
          Arc[i].isFixed := true;
          end;
//     else raise exception
       Xin := Xin + 1;
       end;

    if Node[Arc[i].Target].Kind = UkOutput then
       begin
       if XOut < ci then
          begin
          j := AddArc(Arc[i].Source, ArLinks[XOut2]);
          Arc[j].Weight := 1;
          Arc[j].RandomRange := 0;
          Arc[j].isFixed := true;

          j := AddArc(ArLinks[XOut2], Arc[i].Target);
          Arc[j].Weight := 1;
          Arc[j].RandomRange := 0;
          Arc[j].isFixed := true;

          Arc[i].Target := ArLinks[XOut + Inputs];
          Arc[i].Weight := 1;
          Arc[i].RandomRange := 0;
          Arc[i].isFixed := true;

          XOut2 := Xout2 + 1;
          end

       else if XOut < cont then
          begin
          DelCount := DelCount + 1;
          SetLength(ArDeletable, DelCount);
          ArDeletable[DelCount - 1] := Arc[i].Target;

          Arc[i].Target := ArLinks[XOut + Inputs];
          Arc[i].Weight := 1;
          Arc[i].RandomRange := 0;
          Arc[i].isFixed := true;
          end

       else if XOut < cont + tempi then
          begin
          j := AddArc(Arc[i].Source, ArLinks[XOut2]);
          Arc[j].Weight := 1;
          Arc[j].RandomRange := 0;
          Arc[j].isFixed := true;

          j := AddArc(ArLinks[XOut2], Arc[i].Target);
          Arc[j].Weight := 1;
          Arc[j].RandomRange := 0;
          Arc[j].isFixed := true;

          Arc[i].Target := ArLinks[XOut + Inputs];
          Arc[i].Weight := 1;
          Arc[i].RandomRange := 0;
          Arc[i].isFixed := true;

          Xout2 := XOut2 + 1;
          end

       else if XOut < cont + temp then
          begin
          DelCount := DelCount + 1;
          SetLength(ArDeletable, DelCount);
          ArDeletable[DelCount - 1] := Arc[i].Target;

          Arc[i].Target := ArLinks[XOut + Inputs];
          Arc[i].Weight := 1;
          Arc[i].RandomRange := 0;
          Arc[i].isFixed := true;
          end

       else
          begin
          j := AddArc(Arc[i].Source, ArLinks[XOut2]);
          Arc[j].Weight := 1;
          Arc[j].RandomRange := 0;
          Arc[j].isFixed := true;

          Arc[i].Source := ArLinks[XOut2];
          Arc[j].Weight := 1;
          Arc[j].RandomRange := 0;
          Arc[j].isFixed := true;

          Xout2 := XOut2 + 1;
          end;
       Xout := Xout + 1;
       end;
    end;

  for i := 0 to DelCount - 1 do
    begin
    DeleteNode(ArDeletable[i]);
    for j := (i + 1) to (DelCount - 1) do
      if ArDeletable[j] > ArDeletable[i] then
         ArDeletable[j] := ArDeletable[j] - 1;
    end;
  end;

{------------------------------------------------------------------------------}

procedure TNetworkRep.LoadElman(inputs, hidden, outputs: integer;
                                Pattern : TNeuronRep);
  var
    i, j, k{, tot} : integer;
  begin
  LoadFeedForward(3, [inputs + hidden, hidden, outputs], pattern);
//  tot := inputs + (2 * hidden) + outputs;
  j := 0;
  for i := FNodes.Count - 1 downto 0 do
    begin
    if (Node[i].Kind = UkInput) and (j < hidden) then
       begin
       DeleteNode(i);
       j := j + 1;
       end;
    end;
  for i := 0 to hidden - 1 do
    begin
    j := AddNode('Rec_' + inttostr(i), UkRec);
    (Node[j] as TRecLinkRep).Level := 0;
    (Node[j] as TRecLinkRep).World := -1;
    (Node[j] as TRecLinkRep).linkbackStyle := LbsEach;
    k := AddArc(inputs + hidden + i, j);
    Arc[k].isFixed  := true;
    Arc[k].RandomRange := 0;
    Arc[k].Weight   := 1;
    k := AddArc(j, inputs + i);
    Arc[k].isFixed  := true;
    Arc[k].RandomRange := 0;
    Arc[k].Weight   := 1;
    end;
  FLinkFlow := LfElman;
  end;

{------------------------------------------------------------------------------}

procedure TNetworkRep.LoadFeedForward(layers:integer;
                     neurons : array of integer; Pattern : TNeuronRep);
  var
    i, j, l, k, tot, Xpar : integer;
    Xlay : array of integer;

  begin
  clearArcs;
  ClearNodes;

  tot := 0;
  for i := 0 to layers - 1 do
    tot := tot + neurons[i];
  SetLength(XLay, tot);

  i := 0;
  l := 0;
  j := neurons[l];
  while (i < tot) and (l < layers) do
    begin
    if (j > 0) then
       begin
       XLay[i] := l;
       j := j - 1;
       i := i + 1;
       k := AddNode('Neu_'+ inttostr(l) + '-' + inttostr(neurons[l] - j), UKNeuron);
       if pattern <> nil then
          with Node[k] as TNeuronRep do
            begin
            Eta           := Pattern.Eta;
            Momentum      := Pattern.Momentum;
            AtivFunction  := pattern.AtivFunction;
            FixedBias     := Pattern.FixedBias;
            RandomBias    := Pattern.RandomBias;
            HasBias       := Pattern.HasBias;
            BiasWeight    := Pattern.BiasWeight;
            for XPar := 0 to Pattern.ParamCount - 1 do
              AddFunctionParam(Pattern.Param[XPar]);
            end;
       end
    else
       begin
       l := l + 1;
       j := neurons[l];
       end;
    end;

  DoFullyConnect(XLay, layers, 0, 1, false);

  for i := 0 to neurons[0] - 1 do
    begin
    k := AddNode('Input' + inttostr(i), UkInput);
    with Node[k] as TIOUnitRep do
      begin
      InMin := -1;
      InMax := 1;
      OutMin := -1;
      OutMax := 1;
      World := -1;
      end;
    l := AddArc(k, i);
    Arc[l].Weight := 1;
    Arc[l].isFixed := true;
    Arc[l].RandomRange := 0;
    end;

  for i := 1 to neurons[layers - 1] do
    begin
    k := AddNode('Output' + inttostr(neurons[layers - 1]  - i), UkOutput);
    with Node[k] as TIOUnitRep do
      begin
      InMin := -1;
      InMax := 1;
      OutMin := -1;
      OutMax := 1;
      World := -1;
      end;
    l := AddArc(tot - i, k);
    Arc[l].Weight := 1;
    Arc[l].isFixed := true;
    Arc[l].RandomRange := 0;
    end;
  FLinkFlow := LfNone;
  end;

{------------------------------------------------------------------------------}

procedure TNetworkRep.LoadFromXML(Filename: TFileName);
  const bufsize=1023;
  var
    i, j, k, qtNodes    : integer;
    XRoot : TDomNode;
    Parser : TXmlToDomParser;
    Implem : TDomImplementation;
    XValidFile : boolean;
    XHasMomentum : boolean;

    inFS, outFS : TFileStream;
    Xbuffer1, XBuffer2 : array[0..bufsize] of byte;
    XReadSize : integer;

  begin
  //Initialization
  Implem := TDomImplementation.Create(nil);
  Parser := TXmlToDomParser.Create(nil);
  Parser.DOMImpl := Implem;



  ClearNodes;

  //Validation
  XRoot := nil;
  XValidFile := true;
  try
{    AssignFile(t1, FileName);
    AssignFile(t2, );
    Reset(t1);
    rewrite(t2);
    while not eof(t1) do
      begin
      read(t1, c);
      if (c <> #13) and (c <> #10) then
         write(t2, c);
      end;
    closeFile(t1);
    closeFile(t2);
    }

    inFS := TFileStream.Create(FileName, fmOpenRead);
    inFS.Position := 0;
    outFS := TFileStream.Create(Filename + 'tmp.xml', fmCreate);
    while (inFS.Position < inFS.Size) do
      begin
      XReadSize := inFS.Read(XBuffer1, bufSize);
      j := 0;
      for i := 0 to XReadSize - 1 do
        begin
        if (XBuffer1[i] <> 13) and (Xbuffer1[i] <> 10) then
           begin
           XBuffer2[j] := XBuffer1[i];
           j := j + 1;
           end;
        end;
      outFS.Write(XBuffer2, j);
    end;
    inFS.Free;
    outFS.Free;

    Parser.ParseFile(FileName + 'tmp.xml', true);

    XRoot := Implem.Documents.Item(0).ChildNodes.Item(0);
  except
    XValidFile := false;
    end;
  if XRoot <> nil then
     begin
     XValidFile := XValidFile and (XRoot.NodeName = 'neural_network');
     XValidFile := XValidFile and (XRoot.ChildNodes.Item(0).NodeName = 'nodes');
  // XValidFile := XValidFile and XRoot.ChildNodes.Item(1).NodeName = 'synapses';
     end;

  //Loading the network:

  if XValidFile then
     begin
     try
       begin
       XHasMomentum := false;
       if XRoot.Attributes.GetNamedItem('LinkFlow') <> nil then
          FLinkFlow := TLinkFlow(strtoint(XRoot.Attributes.GetNamedItem('LinkFlow').NodeValue));
       if XRoot.Attributes.GetNamedItem('EtaCorrection') <> nil then
          FEtaCorrection := strtofloat(XRoot.Attributes.GetNamedItem('EtaCorrection').NodeValue);
       if XRoot.Attributes.GetNamedItem('Nu') <> nil then
          FNu := StrToInt(XRoot.Attributes.GetNamedItem('Nu').NodeValue);
       if XRoot.Attributes.GetNamedItem('Upsilon') <> nil then
          FUpsilon := StrToInt(XRoot.Attributes.GetNamedItem('Upsilon').NodeValue);
       if XRoot.Attributes.GetNamedItem('HasMomentum') <> nil then
          XHasMomentum := XRoot.Attributes.GetNamedItem('HasMomentum').NodeValue <> '';
       QtNodes := strtoint(XRoot.ChildNodes.Item(0).Attributes.item(0).NodeValue);
       for i := 0 to qtnodes - 1 do
          with XRoot.ChildNodes.Item(0).ChildNodes.Item(i) do
            begin
            if Attributes.GetNamedItem('kind').NodeValue = 'Neuron' then
              begin
              AddNode(Attributes.GetNamedItem('name').NodeValue, UKNeuron);
              with FNodes.Objects[i] as TNeuronRep do
                begin
                X := strtoint(Attributes.GetNamedItem('posX').NodeValue);
                Y := strtoint(Attributes.GetNamedItem('posY').NodeValue);
                World := strtoint(Attributes.GetNamedItem('world').NodeValue);
                k := 0;
                Eta := strtofloat(ChildNodes.Item(k).ChildNodes.Item(0).NodeValue);
                if XHasMomentum then
                   begin
                   k := k + 1;
                   Momentum := strtofloat(ChildNodes.Item(k).ChildNodes.Item(0).NodeValue);
                   end;
                k := k + 1;
                AtivFunction := strtoint(ChildNodes.Item(k).ChildNodes.Item(0).NodeValue);
                k := k + 1;
                HasBias := ChildNodes.Item(k).ChildNodes.Item(0).NodeValue = '-1';
                k := k + 1;
                BiasWeight := StrToFloat(ChildNodes.Item(k).ChildNodes.Item(0).NodeValue);
                k := k + 1;
                RandomBias := StrToFloat(ChildNodes.Item(k).ChildNodes.Item(0).NodeValue);
                k := k + 1;
                for j := k to ChildNodes.Length - 2 do
                  AddFunctionParam(strtoint(ChildNodes.Item(j).ChildNodes.Item(0).NodeValue));
                FixedBias := ChildNodes.Item(ChildNodes.Length - 1).ChildNodes.Item(0).NodeValue = '-1';
                end;
              end
            else if Attributes.GetNamedItem('kind').NodeValue = 'Delay' then
              begin
              AddNode(Attributes.GetNamedItem('name').NodeValue, UKDelay);
              with FNodes.Objects[i] as TDelayRep do
                begin
                X := strtoint(Attributes.GetNamedItem('posX').NodeValue);
                Y := strtoint(Attributes.GetNamedItem('posY').NodeValue);
                World := strtoint(Attributes.GetNamedItem('world').NodeValue);
                Times := strtoint(ChildNodes.Item(0).ChildNodes.Item(0).NodeValue);
                DelayFunction := strtoint(ChildNodes.Item(1).ChildNodes.Item(0).NodeValue);
                end;
              end
            else if Attributes.GetNamedItem('kind').NodeValue = 'Input' then
              begin
              AddNode(Attributes.GetNamedItem('name').NodeValue, UKInput);
              with FNodes.Objects[i] as TIOUnitRep do
                begin
                X := strtoint(Attributes.GetNamedItem('posX').NodeValue);
                Y := strtoint(Attributes.GetNamedItem('posY').NodeValue);
                World  := strtoint(Attributes.GetNamedItem('world').NodeValue);
                InMax  := strtofloat(ChildNodes.Item(0).ChildNodes.Item(0).NodeValue);
                InMin  := strtofloat(ChildNodes.Item(1).ChildNodes.Item(0).NodeValue);
                OutMax := strtofloat(ChildNodes.Item(2).ChildNodes.Item(0).NodeValue);
                OutMin := strtofloat(ChildNodes.Item(3).ChildNodes.Item(0).NodeValue);
                end;
              end
            else if Attributes.GetNamedItem('kind').NodeValue = 'Output' then
              begin
              AddNode(Attributes.GetNamedItem('name').NodeValue, UKOutput);
              with FNodes.Objects[i] as TIOUnitRep do
                begin
                X := strtoint(Attributes.GetNamedItem('posX').NodeValue);
                Y := strtoint(Attributes.GetNamedItem('posY').NodeValue);
                World := strtoint(Attributes.GetNamedItem('world').NodeValue);
                InMax  := strtofloat(ChildNodes.Item(0).ChildNodes.Item(0).NodeValue);
                InMin  := strtofloat(ChildNodes.Item(1).ChildNodes.Item(0).NodeValue);
                OutMax := strtofloat(ChildNodes.Item(2).ChildNodes.Item(0).NodeValue);
                OutMin := strtofloat(ChildNodes.Item(3).ChildNodes.Item(0).NodeValue);
                end;
              end
            else if Attributes.GetNamedItem('kind').NodeValue = 'RecLink' then
              begin
              AddNode(Attributes.GetNamedItem('name').NodeValue, UkRec);
              with FNodes.Objects[i] as TRecLinkRep do
                begin
                X := strtoint(Attributes.GetNamedItem('posX').NodeValue);
                Y := strtoint(Attributes.GetNamedItem('posY').NodeValue);
                World := strtoint(Attributes.GetNamedItem('world').NodeValue);
                Level := strtoint(ChildNodes.Item(0).ChildNodes.Item(0).NodeValue);
                LinkBackStyle := TLinkBackStyle(strtoint(ChildNodes.Item(1).ChildNodes.Item(0).NodeValue));
                if ChildNodes.Length > 2 then
                   InitValue := strtofloat(ChildNodes.Item(2).ChildNodes.Item(0).NodeValue);
                end;
              end
            end;

       QtNodes := strtoint(XRoot.ChildNodes.Item(1).Attributes.item(0).NodeValue);
       for i := 0 to qtnodes - 1 do
         with XRoot.ChildNodes.Item(1).ChildNodes.Item(i).Attributes do
           begin
           AddArc(strtoint(GetNamedItem('source').NodeValue), strtoint(GetNamedItem('target').NodeValue));
           (FArcs.Objects[i] as TConnecRep).Weight := strToFloat(GetNamedItem('weight').NodeValue);
           (FArcs.Objects[i] as TConnecRep).RandomRange := strToFloat(GetNamedItem('random').NodeValue);
           (FArcs.Objects[i] as TConnecRep).IsFixed := GetNamedItem('fixed').NodeValue = '-1';
           end;

       WorldCount := strtoint(XRoot.ChildNodes.Item(2).Attributes.GetNamedItem('count').NodeValue);
       FWorldLinkPoints := strtoint(XRoot.ChildNodes.Item(2).Attributes.GetNameditem('links').NodeValue);
       for i := 0 to WorldCount - 1 do
         with XRoot.ChildNodes.Item(2).ChildNodes.Item(i).Attributes do
           begin
           FWorldHeights[i] := strtoint(GetNamedItem('height').NodeValue);
           FWorldLefts[i]   := strtoint(GetNamedItem('left').NodeValue);
           FWorldTops[i]    := strtoint(GetNamedItem('top').NodeValue);
           FWorldWidths[i]  := strtoint(GetNamedItem('width').NodeValue);
           end;
       end;
       //---------------------------------------
     //****except
     //****Raise Some message
     //****  end;
     finally
       Implem.Free;
       Parser.Free;
//       XRoot.Free;
        if FileExists(Filename + 'tmp.xml') then
           DeleteFile(Filename + 'tmp.xml');
       end;
     end
  else
     begin
     Implem.Free;
     Parser.Free;
     if FileExists(Filename + 'tmp.xml') then
        DeleteFile(Filename + 'tmp.xml');

//     XRoot.Free;
     end;
  end;

{------------------------------------------------------------------------------}

procedure TNetworkRep.MergeNetwork(TmpNet : TNetworkRep; Style : TMergeStyle; Level : integer);
  var
    tmp : integer;
    i, j, k, OutCount, InCount, DelCount : integer;
    tmpNode : TUnitRep;
    tmpArc : TConnecRep;
    XOutSource, XInTarget, ArDeletable : array of integer;
  begin
  tmp := FNodes.Count;

  for i := 0 to TmpNet.NoNodes - 1 do
    begin
    case TmpNet.Node[i].Kind of
      UkNeuron : TmpNode := TNeuronRep.CreateCopy (TmpNet.Node[i] as TNeuronRep);
      UkDelay  : TmpNode := TDelayRep.CreateCopy  (TmpNet.Node[i] as TDelayRep);
      UkInput  : TmpNode := TIOUnitRep.CreateCopy (TmpNet.Node[i] as TIOUnitRep);
      UkOutput : TmpNode := TIoUnitRep.CreateCopy (TmpNet.Node[i] as TIoUnitRep);
      UkRec    : TmpNode := TRecLinkRep.CreateCopy(TmpNet.Node[i] as TRecLinkRep);
    else
      tmpNode := nil;
      end;
    AddNodeObject(TmpNode);
    end;

  for i := 0 to TmpNet.NoArcs - 1 do
    begin
    tmpArc := TConnecRep.CreateCopy(TmpNet.Arc[i]);
    tmpArc.Source := tmpArc.Source + tmp;
    tmpArc.Target := tmpArc.Target + tmp;
    AddArcObject(TmpArc);
    end;

  InCount  := 0;
  OutCount := 0;

  if Style = MsInput then
     begin
     for i := 0 to tmp - 1 do
       if Node[i].Kind = UkInput then
          begin
          InCount := InCount + 1;
          SetLength(XInTarget, InCount);
          XInTarget[InCount - 1] := i;
          end;
     for i := tmp to FNodes.Count - 1 do
       if Node[i].Kind = UkOutput then
          begin
          OutCount := OutCount + 1;
          SetLength(XOutSource, OutCount);
          XOutSource[OutCount - 1] := i;
          end;
     end
  else if Style = MsOutput then
     begin
     for i := 0 to tmp - 1 do
       if Node[i].Kind = UkOutput then
          begin
          OutCount := OutCount + 1;
          SetLength(XOutSource, OutCount);
          XOutSource[OutCount - 1] := i;
          end;
     for i := tmp to FNodes.Count - 1 do
       if Node[i].Kind = UkInput then
          begin
          InCount := InCount + 1;
          SetLength(XInTarget, InCount);
          XInTarget[InCount - 1] := i;
          end;
     end;

  if OutCount < InCount then
     InCount := OutCount;

  DelCount := 0;

  for i := 0 to InCount - 1 do
    begin
    j := 0;
    while not (Node[XOutSource[i]].Kind in [UkNeuron, UkDelay]) do
      begin
      if Arc[j].Target = XOutSource[i] then
         begin
         DelCount := DelCount + 1;
         SetLength(ArDeletable, DelCount);
         ArDeletable[DelCount - 1] := XOutSource[i];
         XOutSource[i] := Arc[j].Source;
         end;
      j := (j + 1) mod FArcs.Count;
      end;

    while not (Node[XInTarget[i]].Kind in [UkNeuron, UkDelay]) do
      begin
      if Arc[j].Source = XInTarget[i] then
         begin
         DelCount := DelCount + 1;
         SetLength(ArDeletable, DelCount);
         ArDeletable[DelCount - 1] := XInTarget[i];
         XInTarget[i] := Arc[j].Target;
         end;
      j := (j + 1) mod FArcs.Count;
      end;
    end;

  for i := 0 to inCount - 1 do
    begin
    j := AddNode('J_' + inttostr(i), UkRec);
    (Node[j] as TRecLinkRep).Level := Level;

    k := AddArc(XOutSource[i], j);
    Arc[k].Weight := 1;
    Arc[k].RandomRange := 0;
    Arc[k].isFixed := true;

    k := AddArc(j, XInTarget[i]);
    Arc[k].Weight := 1;
    Arc[k].RandomRange := 0;
    Arc[k].isFixed := true;
    end;

  for i := 0 to DelCount - 1 do
    begin
    DeleteNode(ArDeletable[i]);
    for j := (i + 1) to (DelCount - 1) do
      if ArDeletable[j] > ArDeletable[i] then
         ArDeletable[j] := ArDeletable[j] - 1;
    end;
  end;


{------------------------------------------------------------------------------}

procedure TNetworkRep.RenameNode(index: integer; NewName: string);
  begin
  if (index >= 0) and (index < FNodes.Count) then
     begin
     FNodes.Strings[index] := NewName;
     (FNodes.Objects[index] as TUnitRep).Name := NewName;
     end;
  end;

{------------------------------------------------------------------------------}

procedure TNetworkRep.SaveToDotFile(FileName: TFileName);
  var
    layers  : array of integer;
    tot : integer;
  begin
  setLength(layers, FNodes.Count);
  tot := GetLayers(layers);
  DoSaveDot(Layers, tot, FileName);
  end;

{------------------------------------------------------------------------------}

procedure TNetworkRep.SaveToXML(Filename: TFileName);
  var
    XMain : TStringList;
    s : string;
    i, j : integer;
  begin
  XMain := TStringList.Create;
  XMain.Clear;

  s := '<neural_network ';
  s := s + 'LinkFlow="' + IntToStr(integer(FLinkFlow)) + '" ';
  s := s + 'EtaCorrection="' + FloatToStr(FEtaCorrection) + '" ';
  s := s + 'Nu="' + IntToStr(FNu) + '" ';
  s := s + 'Upsilon="' + IntToStr(FUpsilon) + '" ';
  s := s + 'HasMomentum="YES">';
  XMain.Add(s);

  s := '<nodes ';
  s := s + 'number="' + IntToStr(NoNodes) + '">';
  XMain.Add(s);
  for i := 0 to FNodes.Count - 1 do
    case (FNodes.Objects[i] as TUnitRep).Kind of
      UKNeuron :
        with FNodes.Objects[i] as TNeuronRep do
          begin
          s := '<node ';
          s := s + 'index="' + IntToStr(i) + '" ';
          s := s + 'name="' + Name + '" ';
          s := s + 'posX="' + IntToStr(X) + '" ';
          s := s + 'posY="' + IntToStr(Y) + '" ';
          s := s + 'world="' + IntToStr(World) + '" ';
          s := s + 'kind="Neuron">';
          XMain.Add(s);
          s := '<eta>' + floattostr(Eta) + '</eta>';
          XMain.Add(s);
          s := '<momentum>' + FloatToStr(Momentum) + '</momentum>';
          XMain.Add(s);
          s := '<activation>' + IntToStr(AtivFunction) + '</activation>';
          XMain.Add(s);
          if HasBias then
             s := '<has_bias>-1</has_bias>'
          else
             s := '<has_bias>0</has_bias>';
          XMain.Add(s);
          s := '<bias_weight>' + floattostr(BiasWeight) + '</bias_weight>';
          XMain.Add(s);
          s := '<random_bias>' + floattostr(RandomBias) + '</random_bias>';
          XMain.Add(s);
          for j := 0 to ParamCount - 1 do
            begin
            s := '<param_' + inttostr(j) + '>';
            s := s + FloatToStr(Param[j]);
            s := s + '</param_' + inttostr(j) + '>';
            XMain.Add(s);
            end;
          if FixedBias then
             s := '<fixed_bias>-1</fixed_bias>'
          else
             s := '<fixed_bias>0</fixed_bias>';
          XMain.Add(s);
          s := '</node>';
          XMain.Add(s);
          end;
      UKDelay :
        with FNodes.Objects[i] as TDelayRep do
          begin
          s := '<node ';
          s := s + 'index="' + IntToStr(i) + '" ';
          s := s + 'name="' + Name + '" ';
          s := s + 'posX="' + IntToStr(X) + '" ';
          s := s + 'posY="' + IntToStr(Y) + '" ';
          s := s + 'world="' + IntToStr(World) + '" ';
          s := s + 'kind="Delay">';
          XMain.Add(s);
          s := '<times>' + inttostr(Times) + '</times>';
          XMain.Add(s);
          s := '<delay>' + inttostr(DelayFunction) + '</delay>';
          XMain.Add(s);
          s := '</node>';
          XMain.Add(s);
          end;
      UKInput :
        with FNodes.Objects[i] as TIOUnitRep do
          begin
          s := '<node ';
          s := s + 'index="' + IntToStr(i) + '" ';
          s := s + 'name="' + Name + '" ';
          s := s + 'posX="' + IntToStr(X) + '" ';
          s := s + 'posY="' + IntToStr(Y) + '" ';
          s := s + 'world="' + IntToStr(World) + '" ';
          s := s + 'kind="Input">';
          XMain.Add(s);
          s := '<InMax>' + floattostr(InMax) + '</InMax>';
          XMain.Add(s);
          s := '<InMin>' + floattostr(InMin) + '</InMin>';
          XMain.Add(s);
          s := '<OutMax>' + floattostr(OutMax) + '</OutMax>';
          XMain.Add(s);
          s := '<OutMin>' + floattostr(OutMin) + '</OutMin>';
          XMain.Add(s);
          s := '</node>';
          XMain.Add(s);
          end;
      UKOutput :
        with FNodes.Objects[i] as TIOUnitRep do
          begin
          s := '<node ';
          s := s + 'index="' + IntToStr(i) + '" ';
          s := s + 'name="' + Name + '" ';
          s := s + 'posX="' + IntToStr(X) + '" ';
          s := s + 'posY="' + IntToStr(Y) + '" ';
          s := s + 'world="' + IntToStr(World) + '" ';
          s := s + 'kind="Output">';
          XMain.Add(s);
          s := '<InMax>' + floattostr(InMax) + '</InMax>';
          XMain.Add(s);
          s := '<InMin>' + floattostr(InMin) + '</InMin>';
          XMain.Add(s);
          s := '<OutMax>' + floattostr(OutMax) + '</OutMax>';
          XMain.Add(s);
          s := '<OutMin>' + floattostr(OutMin) + '</OutMin>';
          XMain.Add(s);
          s := '</node>';
          XMain.Add(s);
          end;
      UkRec :
        with FNodes.Objects[i] as TRecLinkRep do
          begin
          s := '<node ';
          s := s + 'index="' + IntToStr(i) + '" ';
          s := s + 'name="' + Name + '" ';
          s := s + 'posX="' + IntToStr(X) + '" ';
          s := s + 'posY="' + IntToStr(Y) + '" ';
          s := s + 'world="' + IntToStr(World) + '" ';
          s := s + 'kind="RecLink">';
          XMain.Add(s);
          s := '<Level>' + inttostr(Level) + '</Level>';
          XMain.Add(s);
          s := '<BackMode>' + inttostr(integer(LinkBackStyle)) + '</BackMode>';
          XMain.Add(s);
          s := '<InitValue>' + floattostr(InitValue) + '</InitValue>';
          XMain.Add(s);
          s := '</node>';
          XMain.Add(s);
          end;
      end;
  s := '</nodes>';
  XMain.Add(s);

  s := '<synapses ';
  s := s + 'number="' + IntToStr(NoArcs) + '">';
  XMain.Add(s);
  for i := 0 to FArcs.Count - 1 do
    with FArcs.Objects[i] as TConnecRep do
      begin
      s := '<synapse ';
      s := s + 'index="' + inttostr(i) + '" ';
      s := s + 'source="' + inttostr(Source) + '" ';
      s := s + 'target="' + inttostr(Target) + '" ';
      s := s + 'weight="' + floattostr(Weight) + '" ';
      s := s + 'random="' + floattostr(RandomRange) + '" ';
      if isFixed then
        s := s + 'fixed="-1"/>'
      else
        s := s + 'fixed="0"/>';
      XMain.Add(s);
      end;
  s := '</synapses>';
  XMain.Add(s);


  s := '<worlds ';
  s := s + 'count="' + IntToStr(WorldCount) + '" ';
  s := s + 'links="' + IntToStr(WorldLinks) + '">';
  XMain.Add(s);
  for i := 0 to FWorldCount - 1 do
    begin
    s := '<world ';
    s := s + 'index="' + inttostr(i) + '" ';
    s := s + 'height="' + inttostr(FWorldHeights[i]) + '" ';
    s := s + 'left="' + inttostr(FWorldLefts[i]) + '" ';
    s := s + 'top="' + inttostr(FWorldTops[i]) + '" ';
    s := s + 'width="' + inttostr(FWorldWidths[i]) + '"/>';
    XMain.Add(s);
    end;
  s := '</worlds>';
  XMain.Add(s);


  s := '</neural_network>';
  XMain.Add(s);

  XMain.SaveToFile(Filename);

  XMain.Free;
  end;

{------------------------------------------------------------------------------}

procedure TNetworkRep.SetEtaCorrection(const Value: double);
  begin
  FEtaCorrection := value;
  end;

{------------------------------------------------------------------------------}

procedure TNetworkRep.SetLinkFlow(const value: TLinkFlow);
  begin
  FLinkFlow := value;
  end;

{------------------------------------------------------------------------------}

procedure TNetworkRep.SetNEta(value: double);
  var i : integer;
  begin
  for i := 0 to FNodes.Count - 1 do
    if (FNodes.Objects[i] as TUnitRep).Kind = UKNeuron then
       (FNodes.Objects[i] as TNeuronRep).Eta := value;
  end;

{------------------------------------------------------------------------------}

procedure TNetworkRep.SetNMomentum(value: double);
  var i : integer;
  begin
  for i := 0 to FNodes.Count - 1 do
    if (FNodes.Objects[i] as TUnitRep).Kind = UKNeuron then
       (FNodes.Objects[i] as TNeuronRep).Momentum := value;
  end;

{------------------------------------------------------------------------------}

procedure TNetworkRep.SetNu(const Value: integer);
  begin
  if value > 0 then
     FNu := value;
  end;

{------------------------------------------------------------------------------}

procedure TNetworkRep.SetUpsilon(const Value: integer);
  begin
  if value > 0 then
     FUpsilon := value;
  end;

{------------------------------------------------------------------------------}

procedure TNetworkRep.SetWorldCount(const Value: integer);
  begin
  if value >= 0 then
    begin
    FWorldCount := value;
    SetLength(FWorldLefts, FWorldCount);
    SetLength(FWorldTops, FWorldCount);
    SetLength(FWorldWidths, FWorldCount);
    SetLength(FWorldHeights, FWorldCount);
    end;
  end;

{------------------------------------------------------------------------------}

procedure TNetworkRep.SetWorldHeights(index: integer; const Value: integer);
  begin
  if (index >= 0) and (index < FWorldCount) then
     FWorldHeights[index] := value;
  end;

{------------------------------------------------------------------------------}

procedure TNetworkRep.SetWorldLefts(index: integer; const Value: integer);
  begin
  if (index >= 0) and (index < FWorldCount) then
     FWorldLefts[index] := value;
  end;

{------------------------------------------------------------------------------}

procedure TNetworkRep.SetWorldLinks(const Value: integer);
  begin
  if value >= 0 then
     FWorldLinkPoints := value;
  end;

{------------------------------------------------------------------------------}

procedure TNetworkRep.SetWorldTops(index: integer; const Value: integer);
  begin
  if (index >= 0) and (index < FWorldCount) then
     FWorldTops[index] := value;
  end;

{------------------------------------------------------------------------------}

procedure TNetworkRep.SetWorldWidths(index: integer; const Value: integer);
  begin
  if (index >= 0) and (index < FWorldCount) then
     FWorldWidths[index] := value;
  end;

{------------------------------------------------------------------------------}

procedure TNetworkRep.SortNodes;
  var
    i, tmpCount : integer;
    arr : array of integer;
    x : TStringList;
  begin
  SetLength(arr, FNodes.Count);
  tmpcount := FNodes.Count;
  x := TStringList.Create;
  x.Clear;
  for i := 0 to FNodes.Count do
    begin
    (FNodes.Objects[i] as TUnitRep).Tag := i;
    x.AddObject(FNodes.Strings[i], FNodes.Objects[i]);
    end;
  x.Sort;
  FNodes.Clear;
  for i := 0 to tmpCount - 1 do
    begin
    arr[(x.Objects[i] as TUnitRep).Tag] := i;
    FNodes.AddObject(x.Strings[i], x.Objects[i]);
    end;
  for i := 0 to FArcs.Count - 1 do
    begin
    (FArcs.Objects[i] as TConnecRep).Source := arr[(FArcs.Objects[i] as TConnecRep).Source];
    (FArcs.Objects[i] as TConnecRep).Target := arr[(FArcs.Objects[i] as TConnecRep).Target];
    end;
  x.Free;
  end;

{------------------------------------------------------------------------------}

procedure TNetworkRep.UpdateArcs;
  var
    i, Src, Tgt : integer;
  begin
  for i := 0 to FArcs.Count - 1 do
    begin
    Src := (FArcs.Objects[i] as TConnecRep).Source;
    Tgt := (FArcs.Objects[i] as TConnecRep).Target;
    FArcs.Strings[i] := FNodes.Strings[Src] + ' --> ' + FNodes.Strings[Tgt];
    end;
  end;

{------------------------------------------------------------------------------}

procedure TNetworkRep.VisualOrg(Height, LayerCount : integer; LayerXPositions : array of integer);
  var
    i, j, XLayerCount : integer;
    XLayersArray : array of integer;
    XNodesPerLayer : array of integer;
    XLayerYPositions : array of integer;

  begin
  SetLength(XLayersArray, FNodes.Count);
  XLayerCount := GetLayers(XLayersArray);
  for i := 0 to FNodes.Count - 1 do
    if XLayersArray[i] < 0 then
       case Node[i].Kind of
         UKInput  : XLayersArray[i] := XLayerCount;
         UKOutput : XLayersArray[i] := XLayerCount + 1;
         UkDelay  : XLayersArray[i] := XLayerCount + 2;
         UkRec    : XLayersArray[i] := XLayerCount + 3 + (Node[i] as TRecLinkRep).Level;
         end;

  for i := 0 to FNodes.Count - 1 do
    if XLayersArray[i] >= XLayerCount then
       XLayerCount := XLayersArray[i] + 1;

  SetLength(XNodesPerLayer, XLayerCount);
  SetLength(XLayerYPositions, XLayerCount);

  for j := 0 to XLayerCount - 1 do
    XNodesPerLayer[j] := 0;

  for i := 0 to FNodes.Count - 1 do
    XNodesPerLayer[XLayersArray[i]] := XNodesPerLayer[XLayersArray[i]] + 1;

  for j := 0 to XLayerCount - 1 do
    begin
    if XNodesPerLayer[j] > 0 then
       XLayerYPositions[j] := ((height div XNodesPerLayer[j]) div 2)
    else
       XLayerYPositions[j] := 0;
    end;

  for i := 0 to FNodes.Count - 1 do
    begin
    j := XLayersArray[i];
    if j < LayerCount then
       GetNode(i).X := LayerXPositions[j];
    GetNode(i).Y := XLayerYPositions[j];
    XLayerYPositions[j] := XLayerYPositions[j] + (height div XNodesPerLayer[j])
    end;

  end;


{------------------------------------------------------------------------------}

procedure TNetworkRep.VisualOrgWorlds(w: integer);
  begin
  //What the hell????
  end;

{------------------------------------------------------------------------------}

procedure TNetworkRep.VisualOrgXY(x, y: integer);
  begin
  //Fill in later!!!!
  end;
{------------------------------------------------------------------------------}

{  var
//    Stop : boolean;
    max, maxB, tmp, i, j, tot: integer;
    NeurX, NeurY, DelayX, DelayY, IoX, IoY : integer;
    CalcX, layer : array of integer;
  begin
  //Tamanhos das unidades;
  NeurX  := 62;
  NeurY  := 43;
  DelayX := 25;
  DelayY := 23;
  IoX    := 40;
  IoY    := 28;

  FWorldWidths[w] := (NeurX * 3) + 60;

  //Define layers;

  SetLength(layer, FNodes.Count);

  for i:= 0 to FNodes.Count - 1 do
    layer[i] := -1;

  for i:= 0 to FNodes.Count - 1 do
    if (FNodes.Objects[i] as TUnitRep).World = w then
       begin
       tot := 0;
       //verifica se o layer eh 0
       for j := 0 to FArcs.Count - 1 do
         with FArcs.Objects[j] as TConnecRep do
            if (FTarget = i) and (GetNode(FSource).World = w) then
             tot := tot + 1;
       if tot = 0 then layer[i] := 0;
       end;

  j := 0;
  tot := 0;

  while tot < FArcs.Count do
    begin
    tmp := (FArcs.Objects[j] as TConnecRep).FSource;
    if (FNodes.Objects[tmp] as TUnitRep).World = w then
       begin
       tmp := layer[tmp];
       if (tmp > -1) then
          begin
          i := (FArcs.Objects[j] as TConnecRep).FTarget;
          if (layer[i] <= -1) or (layer[i] > tmp + 1) then
             begin
             tot := 0;
             layer[i] := tmp + 1;
             end;
          end;
       end;
    j := (j + 1) mod FArcs.Count;
    tot := tot + 1;
    end;

  //Posição dos layers
{  max := 0;
  for i := 0 to FNodes.Count - 1 do
    if layer[i] > max then
       max := layer[i];
  max := max + 1;}{
  max := 3;
  SetLength(CalcX, max);
  tmp := (FWorldWidths[w] div (max ));
  CalcX[0] := tmp;
  for i := 1 to max - 1 do
    CalcX[i] := CalcX[i - 1] + tmp;

  //Definition of the Height
  maxB := 0;
  for i := 0 to max - 1 do
    begin
    tot := 0;
    for j := 0 to FNodes.Count - 1 do
      if layer[j] = i then
         tot := tot + 1;
    if tot > MaxB then
       MaxB := tot;
    end;

  FWorldHeights[w] := (MaxB * NeurY) + ((MaxB + 1) * 3);

  //Posição das unidades em cada layer
  for i := 0 to max - 1 do
    begin
    tot := 0;
    maxB := 0;
    for j := 0 to FNodes.Count - 1 do
      if layer[j] = i then
         begin
         maxB := maxB + 1;
         case (FNodes.Objects[j] as TUnitRep).Kind of
           UkNeuron : tot := tot + NeurY;
           UkDelay  : tot := tot + DelayY;
           UkInput  : tot := tot + IoY;
           UkOutput : tot := tot + IoY;
           end;
         end;

    tmp := (FWorldHeights[w] - tot) div (maxB + 1);

    for j := 0 to FNodes.Count - 1 do
      begin
      if layer[j] = i then
         begin
         (FNodes.Objects[j] as TUnitRep).Y := tmp;
         case (FNodes.Objects[j] as TUnitRep).Kind of
           UkNeuron :
             begin
             tmp := tmp + NeurY + ((FWorldHeights[w] - tot) div (maxB + 1));
             (FNodes.Objects[j] as TUnitRep).X := CalcX[i] - (NeurX div 2)
             end;
           UkDelay :
             begin
             tmp := tmp + DelayY + ((FWorldHeights[w] - tot) div (maxB + 1));
             (FNodes.Objects[j] as TUnitRep).X := CalcX[i] - (DelayX div 2)
             end;
           UkInput :
             begin
             tmp := tmp + IOY + ((FWorldHeights[w] - tot) div (maxB + 1));
             (FNodes.Objects[j] as TUnitRep).X := CalcX[i] - (IoX div 2)
             end;
           UkOutput :
             begin
             tmp := tmp + IOY + ((FWorldHeights[w] - tot) div (maxB + 1));
             (FNodes.Objects[j] as TUnitRep).X := CalcX[i] - (IoX div 2)
             end;
           end;
         end;
      end;
    end;
  end;

{------------------------------------------------------------------------------}{
  var
//    Stop : boolean;
    max, tmp, i, j, tot: integer;
    NeurX, NeurY, DelayX, DelayY, IoX, IoY : integer;
    CalcX, layer : array of integer;
  begin
  //Tamanhos das unidades;
  NeurX  := 62;
  NeurY  := 43;
  DelayX := 25;
  DelayY := 23;
  IoX    := 40;
  IoY    := 28;

  //Define layers;

  SetLength(layer, FNodes.Count);

  for i:= 0 to FNodes.Count - 1 do
    layer[i] := -1;

  for i:= 0 to FNodes.Count - 1 do
    if FNodes.Objects[i] is TIOUnitRep then
       if (FNodes.Objects[i] as TIoUnitRep).IsInput then
          layer[i] := 0;

  j := 0;
  tot := 0;

  while tot < FArcs.Count do
    begin
    tmp := layer[(FArcs.Objects[j] as TConnecRep).FSource];
    if (tmp > -1) then
       begin
       i := (FArcs.Objects[j] as TConnecRep).FTarget;
       if (layer[i] <= -1) or (layer[i] > tmp + 1) then
          begin
          tot := 0;
          layer[i] := tmp + 1;
          end;
       end;
    j := (j + 1) mod FArcs.Count;
    tot := tot + 1;
    end;

  //Posição dos layers
  max := 0;
  for i := 0 to FNodes.Count - 1 do
    if layer[i] > max then
       max := layer[i];
  max := max + 1;
  SetLength(CalcX, max);
  tmp := (x div (max + 1));
  CalcX[0] := tmp;
  for i := 1 to max - 1 do
    CalcX[i] := CalcX[i - 1] + tmp;

  //Posição das unidades em cada layer
  for i := 0 to max - 1 do
    begin
    tot := 0;
    max := 0;
    for j := 0 to FNodes.Count - 1 do
      if layer[j] = i then
         begin
         max := max + 1;
         case (FNodes.Objects[j] as TUnitRep).Kind of
           UkNeuron : tot := tot + NeurY;
           UkDelay  : tot := tot + DelayY;
           UkInput  : tot := tot + IoY;
           UkOutput : tot := tot + IoY;
           end;
         end;

    tmp := (y - tot) div (max + 1);

    for j := 0 to FNodes.Count - 1 do
      begin
      if layer[j] = i then
         begin
         (FNodes.Objects[j] as TUnitRep).Y := tmp;
         case (FNodes.Objects[j] as TUnitRep).Kind of
           UkNeuron :
             begin
             tmp := tmp + NeurY + ((y - tot) div (max + 1));
             (FNodes.Objects[j] as TUnitRep).X := CalcX[i] - (NeurX div 2)
             end;
           UkDelay :
             begin
             tmp := tmp + DelayY + ((y - tot) div (max + 1));
             (FNodes.Objects[j] as TUnitRep).X := CalcX[i] - (DelayX div 2)
             end;
           UkInput :
             begin
             tmp := tmp + IOY + ((y - tot) div (max + 1));
             (FNodes.Objects[j] as TUnitRep).X := CalcX[i] - (IoX div 2)
             end;
           UkOutput :
             begin
             tmp := tmp + IOY + ((y - tot) div (max + 1));
             (FNodes.Objects[j] as TUnitRep).X := CalcX[i] - (IoX div 2)
             end;
           end;
         end;
      end;
    end;
  end;

{------------------------------------------------------------------------------}


end.



