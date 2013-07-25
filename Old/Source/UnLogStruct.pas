{
  @abstract(Unit that contains the structures for the manipulation of the logic programs)
  @author(Rafa "Passarel" Borges <passarel@gmail.com)
  @created()
  @lastmod()
}
unit UnLogStruct; //1166 lines

interface

uses Classes;

{------------------------------------------------------------------------------}

{Enumeration of the different logic operators allowed on the programs}
type TOperator = (OpNone, OpNeg, OpBox, OpDiamond, OpK, OpNextTime, OpPreviousTime, OpFutureDiam,
                  OpPastDiam, OpFutureBox, OpPastBox, OpSince, OpZince, OpUntil, OpUnless);

{------------------------------------------------------------------------------}

{Enumeration of the transformation of atoms related to the operators}
type TOpTransf = (OtError, OtBoxE, OtBoxI, OtDiamondE, OtDiamondI, OtKE, OtKI,
                  OtNtE, OtNtI, OtPtE, OtPtI, OtExt);

{------------------------------------------------------------------------------}

{Enumeration of the possible style of logic programs }
type TStyle = (StClassic, StModal, StExtra, StSCTL, StCTLK);

{------------------------------------------------------------------------------}

{Enumeration of the kinds of links (I don't know if it's used)}
type TLinkKind = (LkInput, LkOutput, LkWorlds, LkRecursive, LkNone);

{------------------------------------------------------------------------------}

{
@abstract(Class encapsulating a literal on different logics)

The class @name contain the structures that describes an literal for any logic
allowed to be represented. Each instance of the class contains an operator and
a recursive definition of the atom.
}
type TGenLiteral = class
  private
    //Atom of the literal
    FAtom1       : integer;

    //Atom of the literal
    //FAtom2       : integer;

    //Agent of the literal (if it has a Knowledge operator)
    FAgent      : integer;

    //Operator of the literal
    FOperator   : TOperator;

    //Atom following the operator, in case of a recursive literal
    FChild1      : TGenLiteral;

    //Atom following the operator, in case of a recursive literal
    FChild2      : TGenLiteral;

    //Indicates if the literal is recursive (more than one operator)
    FRecursive  : boolean;

    //Read method of the property Atom
    function GetAtom1: integer;

    //Read method of the property Atom
    //function GetAtom2: integer;

    //Read method of the property Agent
    function GetAgent: integer;

    //Read method of the property Child
    function GetChild1: TGenLiteral;

    //Read method of the property Child
    function GetChild2: TGenLiteral;

    //Read method of the property Operator
    function GetOperator: TOperator;

    //Read method of the property Recursive
    function GetRecursive: boolean;

    //Write method of the property Atom
    procedure SetAtom1(const Value: integer);

    //Write method of the property Atom
    //procedure SetAtom2(const Value: integer);

    //Write method of the property Agent
    procedure SetAgent(const Value: integer);

    //Write method of the property Child
    procedure SetChild1(const Value: TGenLiteral);

    //Write method of the property Child
    procedure SetChild2(const Value: TGenLiteral);


    //Write method of the property Operator
    procedure SetOperator(const Value: TOperator);

    //Write method of the property Recursive
    procedure SetRecursive(const Value: boolean);

    //Read method of the property Negation
    function GetNegation: boolean;

    //Read method of the property Knowledge
    function GetKnowledge: boolean;

  public
    //Constructor of the class
    constructor Create;

    //Destructor of the class
    destructor  Destroy; override;

    //Atom of the literal
    property Atom1       : integer   read GetAtom1    write SetAtom1;

    //Atom of the literal
    //property Atom2       : integer   read GetAtom2    write SetAtom2;

    //Agent of the literal (if it has a Knowledge operator)
    property Agent       : integer   read GetAgent    write SetAgent;

    //Literal following the first operator, in case of a recursive literal
    property Child1       : TGenLiteral  read GetChild1    write SetChild1;

    //Literal following the first operator, in case of a recursive literal
    property Child2       : TGenLiteral  read GetChild2    write SetChild2;

    //Operator of the literal
    property Operator    : TOperator read GetOperator write SetOperator;

    //Indicates if the literal is recursive (more than one operator)
    property IsRecursive : boolean   read GetRecursive write SetRecursive;

    //Indicates if the literal has a negation operator
    property HasNegation : boolean   read GetNegation;

    //Indicates if the literal has a Knowledge operator
    property HasKnowledge : boolean   read GetKnowledge;

    //Gets a text representation of the literal
    //@param(AtomList: List of atoms which the literal is based)
    //@param(AgentList: List of agents which the literal is based)
    //@return(The string representing the literal)
    function GetText(AtomList, AgentList : TStrings) : string;

    //Verify if the literal is equal to an other
    //@param(VLiteral: Literal to be compared)
    //@return(true if the two literals are equal)
    function IsEqual(VLiteral : TGenLiteral) : boolean;

    //Gets an literal with a representation without negations
    //@return(Literal represented in the atomic form)
    function GetAtomicForm : TGenLiteral;

    //Gets the count of modal operators
    function GetModalCount : integer;

    //Gets the count of nextTime operators
//    function GetNextTimeCount : integer;
  end;

{------------------------------------------------------------------------------}

{
@abstract(Class encapsulating the structure of logic programs)

  The class @name contain different informations about the structure of a logic
program. Each instance of this class contains a list of atoms, worlds and agents
referenced on the program, and other informations that are important to describe
the program (except by its clauses).
}
type TStaticDefinitions = class
  private
    //List of atoms
    FAtoms  : TStringList;

    //List of agents
    FAgents : TStringList;

    //List of labels
    FLabels : TStringList;

    //Array of relations between worlds (each 2 integers represent a relation)
    FRelations    : array of integer;

    //Count of relations
    FRelationsCount : integer;

    //Array with the associations between atoms and agents(simmilar to FRelations)
    FAssociations : array of integer;

    //Count of associations
    FAssociationsCount : integer;

    //Array with the kind of each identifier
    FIdKind     : array of integer;

    //Array with the index of each identifier on its respective list
    FIdIndex    : array of integer;

    //Count of identifiers
    FIdCount    : integer;

    //Array representing the atoms in the decomposed list of atoms of the program
    FAtomReferences  : array of integer;

    //Array representing the second atoms in the decomposed list of atoms of the program
    FSecondAtomReferences  : array of integer;

    //Array representing the agents (for K operator) in the decomposed list of atoms of the program
    FAgentReferences : array of integer;

    //Array representing the operators in the decomposed list of atoms of the program
    FOperReferences  : array of TOperator;

    //Array of atoms defined as input of the program (associated with a world)
    FDefInputAtoms   : array of integer;

    //Array of labels defined as input of the program
    FDefInputLabels  : array of integer;

    //Array of atoms defined as output of the program (associated with a world)
    FDefOutputAtoms  : array of integer;

    //Array of labels defined as output of the program
    FDefOutputLabels : array of integer;

    //Count of atoms/labels defined as input
    FDefInputCount   : integer;

    //Count of atoms/labels defined as output
    FDefOutputCount  : integer;

    //Count of links between different worlds
    FLinksCount  : integer;

    //Links between different worlds
    FLinkPoints  : array of TLinkKind;

    //Read method of the property Agents
    function GetAgent(index: integer): string;

    //Read method of the property Atoms
    function GetAtom(index: integer): string;

    //Read method of the property Labels
    function GetLabel(index: integer): string;

    //Write method of the property Agents
    procedure SetAgent(index: integer; const Value: string);

    //Write method of the property Atoms
    procedure SetAtom(index: integer; const Value: string);

    //Write method of the property Labels
    procedure SetLabel(index: integer; const Value: string);

    //Read method of the property IdCount
    function GetIdCount: integer;

    //Write method of the property IdCount
    procedure SetIdCount(const Value: integer);

    //Read method of the property RefAtom
    function GetRefAtom(index: integer): integer;

    //Read method of the property RefAtom2
    function GetRefAtom2(index: integer): integer;

    //Write method of the property RefOperator
    function GetRefOperator(index: integer): TOperator;

    //Read method of the property RefAgent
    function GetRefAgent(index: integer): integer;

    //Read method of the property DefInputAtom
    function GetDefInputAtom(index: integer): integer;

    //Read method of the property DefInputCount
    function GetDefInputCount: integer;

    //Read method of the property DefInputLabel
    function GetDefInputLabel(index: integer): integer;

    //Read method of the property DefOutputAtom
    function GetDefOutputAtom(index: integer): integer;

    //Read method of the property DefOutputCount
    function GetDefOutputCount: integer;

    //Read method of the property DefOutputLabel
    function GetDefOutputLabel(index: integer): integer;

    //Read method of the property LinksCount
    function GetLinksCount : integer;

    //Read method of the property LinkPoints
    function GetLinkPoints(index: integer): TLinkKind;


  public
    //Constructor of the class
    constructor Create;

    //Destructor of the class
    destructor  Destroy; override;

    //Access the atoms on the structure
    property  Atoms[index : integer]  : string read GetAtom  write SetAtom;

    //Access the agents on the structure
    property  Agents[index : integer] : string read GetAgent write SetAgent;

    //Access the labels on the structure
    property  Labels[index : integer] : string read GetLabel write SetLabel;

    //Count of identifiers
    property  IdentifierCount : integer read GetIdCount write SetIdCount;

    //???
    property RefOperator[index : integer] : TOperator read GetRefOperator;

    //???
    property RefAtom[index : integer]     : integer   read GetRefAtom;

     //???
    property RefAtom2[index : integer]    : integer   read GetRefAtom2;

    //???
    property RefAgent[index : integer]    : integer   read GetRefAgent;

    //Count of atoms/labels defined as input
    property DefInputCount  : integer read GetDefInputCount;

    //Count of atoms/labels defined as output
    property DefOutputCount : integer read GetDefOutputCount;

    //Access the atoms defined as input of the program (associated with a world)
    property DefInputLabel[index : integer]  : integer read GetDefInputLabel;

    //Access the labels defined as input of the program
    property DefOutputLabel[index : integer] : integer read GetDefOutputLabel;

    //Access the atoms defined as input of the program (associated with a world)
    property DefInputAtom[index : integer]   : integer read GetDefInputAtom;

    //Access the labels defined as input of the program
    property DefOutputAtom[index : integer]  : integer read GetDefOutputAtom;

    //Count of links between different worlds
    property  LinksCount : integer read GetLinksCount;

    //Links between different worlds
    property  LinkPoints[index : integer] : TLinkKind read GetLinkPoints;

    //Return the count of atoms in the structure
    function GetAtomsCount : integer;

    //Gets a list of labels that are target of relations which source is passed as parameter
    //@param(PLabel: Label to be considered as source)
    //@param(List(output): array to be filled with the found labels)
    //@return(Count of found labels)
    function GetRelatedFrom(PLabel : integer; out list : array of integer) : integer;

    //Gets a list of labels that are source of relations which target is passed as parameter
    //@param(PLabel: Label to be considered as target)
    //@param(List(output): array to be filled with the found labels)
    //@return(Count of found labels)
    function GetRelatedTo(PLabel : integer; out list : array of integer) : integer;

    //Gets a list of the atoms associated with a specific agent
    //@param(PAgent: Agent to be considered)
    //@param(List(output): array to be filled with the found atoms)
    //@return(Count of found atoms)
    function GetAssocAtoms(PAgent : integer; out list : array of integer) : integer;

    //Gets a list of the agents associated with a specific atom
    //@param(PAgent: Atom to be considered)
    //@param(List(output): array to be filled with the found agents)
    //@return(Count of found agents)
    function GetAssocAgents(PAtom : integer; out list : array of integer) : integer;

    //Add an atom on the structure
    //@param(item: string representing the atom)
    //@param(index: ???)
    procedure AddAtom(item : string; index : integer);

    //Add an agent on the structure
    //@param(item: string representing the agent)
    //@param(index: ???)
    procedure AddAgentInPosition(item : string; index : integer);

    //Add an agent on the structure
    //@param(item: string representing the agent)
    procedure AddAgent(item : string);

    //Add a label on the structure
    //@param(item: string representing the label)
    //@param(index: ???)
    procedure AddLabelInPosition(item : string; index : integer);

    //Add a label on the structure
    //@param(item: string representing the label)
    procedure AddLabel(item : string); 

    //Add a relation between two worlds
    //@param(source: source of the relation)
    //@param(target: target of the relation)
    procedure AddRelation(source, target : integer);

    //Associates an atom to an agen
    //@param(PAgent : Agent to be associated)
    //@param(PAtom : Atom to be associated)
    procedure AddAssociation(PAgent, PAtom : integer);

    //Add a definition of input atom
    //@param(PLabel: Label of the atom defined as input)
    //@param(PAtom: Atom defined as input)
    procedure AddDefInput(PLabel, PAtom : integer);

    //Add a definition of output atom
    //@param(PLabel: Label of the atom defined as output)
    //@param(PAtom: Atom defined as output)
    procedure AddDefOutput(PLabel, PAtom : integer);

    //Delete an atom of the structure
    //@param(index: index of the atom to be deleted)
    procedure DeleteAtom(index : integer);

    //Delete an agent of the structure
    //@param(index: index of the agent to be deleted)
    procedure DeleteAgent(index : integer);

    //Delete a label of the structure
    //@param(index: index of the label to be deleted)
    procedure DeleteLabel(index : integer);

    //Delete a relation of the structure
    //@param(index: index of the relation to be deleted)
    procedure DeleteRelation(index : integer);

    //Delete an association of the structure
    //@param(index: index of the association to be deleted)
    procedure DeleteAssociation(index : integer);

    //Count of the atoms in the structure
    function  AtomCount : integer;

    //Count of the agents in the structure
    function  AgentCount : integer;

    //Count of the labels in the structure
    function  LabelCount : integer;

    //Count of the relations in the structure
    function  RelationCount : integer;

    //Count of the associations in the structure
    function  AssociationCount : integer;

    //Verify if an identifier is an atom
    //@param(index of the identifier)
    function  IsAtom (index : integer) : boolean;

    //Verify if an identifier is an agent
    //@param(index of the identifier)
    function  IsAgent(index : integer) : boolean;

    //Verify if an identifier is an label
    //@param(index of the identifier)
    function  IsLabel(index : integer) : boolean;

    //???
    //@param()
    //@return()
    function  GetIndex(i : integer)    : integer;

    //Gets the text of a literal
    //@param(Literal: Literal to extract the name)
    //@return(The text)
    function  GetLiteralText(Literal : TGenLiteral) : string;

    //Add an composite (recursively defined) atom
    //@param(item: Atom (Literal) to be added)
    //@return(index of the inserted atom)
    function  AddCompAtom  (item : TGenLiteral) : integer;

    //Clear all the lists of the structure
    procedure Clear;

    //Add a linkPoint
    //@param(Lind of the linkpoint)
    procedure AddLinkPoint (Kind : TLinkKind);

    //Write the structure as the header of a logic program on a StringList
    //@param(txt: StringList to be filled)
    //@param(Style: Style of the logic program)
    procedure WriteHeader(txt : TStringList; Style : TStyle);

    //Get the index of a specific atom
    function GetAtomIndex(atom : String) : integer;

  end;

{------------------------------------------------------------------------------}

{
@abstract(Class encapsulating a clause for different kinds of logic programs)

  The class @name contain the description of a logic clause. Each instance of
this clause contains a literal as head and a set of body literal. It also
contains a reference to an object of the class @link(TStaticDefinition).
}
type TGenClause = class
  private
    //Structure to be used to describe the items on the clause
    FStructure : TStaticDefinitions;

    //Text representation of the clause
    FText : string;

    //Identify if it is a default clause (represented by the operator ::- instead of :-)
    FDefault : boolean;

    //Label of the clause
    FLabel : integer;

    //Literal(atom) on the head of the clause
    FHead : TGenLiteral;

    //Array of the body literals of the clause
    FBody : array of TGenLiteral;

    //Count of literals in the body of the clause
    FBodyCount : integer;

    //Indicates if it is a disjunctive clause (???)
    FDisjunctive : boolean;

    //Loads the text representation of the clause
    procedure LoadText;

    //Read method of the property Body
    function GetBody(i: integer): TGenLiteral;

    //Read method of the property BodySize
    function GetBodySize: integer;

    //Read method of the property Disjunctive
    function GetDisjunctive: boolean;

    //Read method of the property Head
    function GetHead: TGenLiteral;

    //Read method of the property Label
    function GetLabel: integer;

    //Read method of the property Text
    function GetText: string;

    //Read method of the property Default
    function GetDefault: boolean;

    //Write method of the property Body
    procedure SetBody(i: integer; const Value: TGenLiteral);

    //Write method of the property Disjunctive
    procedure SetDisjunctive(const Value: boolean);

    //Write method of the property Head
    procedure SetHead(const Value: TGenLiteral);

    //Write method of the property Label
    procedure SetLabel(const Value: integer);

    //Write method of the property Default
    procedure SetDefault(const Value: boolean);


  public
    //Constructor of the clause
    constructor Create (Structure : TStaticDefinitions);

    //Destructor of the clause
    Destructor  Destroy; override;

    //Label of the clause
    property ClauseLabel : integer     read GetLabel       write SetLabel;

    //Literal(atom) on the head of the clause
    property Head        : TGenLiteral read GetHead        write SetHead;

    //Indicates if it is a disjunctive clause (???)
    property Disjunctive : boolean     read GetDisjunctive write SetDisjunctive;

    //Access a body literals of the clause
    property Body[i : integer] : TGenLiteral read GetBody  write SetBody;

    //Count of literals in the body of the clause
    property BodySize : integer read GetBodySize;

    //Text representation of the clause
    property Text     : string  read GetText;

    //Identify if it is a default clause (represented by the operator ::- instead of :-)
    property IsDefault : boolean read GetDefault write SetDefault;

    //Add a literal on the body of the clause
    //@param(literal: Literal to be added on the body)
    procedure AddBodyLiteral (literal : TGenLiteral);

    //Gets the max number of modal operator between the literals on head and body
    function  MaxModalOperators : integer;

    //Gets the max number of next time operator between the literals on head and body
    function  MaxNextTimeOperators : integer;

  end;

{------------------------------------------------------------------------------}

implementation

//uses

{------------------------------------------------------------------------------}

{ TGenLiteral }

{------------------------------------------------------------------------------}

constructor TGenLiteral.Create;
  begin
  FAtom1      := -1;
//  FAtom2      := -1;
  FAgent      := -1;
  FOperator   := OpNone;
  FRecursive  := false;
  FChild1     := nil;
  FChild2     := nil;
  end;

{------------------------------------------------------------------------------}

destructor TGenLiteral.Destroy;
  begin
  if FRecursive then
     begin
     if FChild1 <> nil then FChild1.Free;
     if FChild2 <> nil then FChild2.Free;
     end;
  inherited;
  end;

{------------------------------------------------------------------------------}

function TGenLiteral.GetAgent: integer;
  begin
  if FAgent >= 0 then
     result := FAgent
  else
     if FRecursive then
        result := FChild1.Agent
     else
        result := -1;
  end;


{------------------------------------------------------------------------------}

function TGenLiteral.GetAtom1: integer;
  begin
  if FAtom1 >= 0 then
     result := FAtom1
  else
     if FRecursive and (FChild1 <> nil) then
        result := FChild1.Atom1
     else
        result := -1;
  end;

{------------------------------------------------------------------------------

function TGenLiteral.GetAtom2: integer;
  begin
  if FAtom2 >= 0 then
     result := FAtom2
  else
     if FRecursive and (FChild2 <> nil) then
        result := FChild2.Atom1
     else
        result := -1;
  end;

{------------------------------------------------------------------------------}

function TGenLiteral.GetAtomicForm: TGenLiteral;
  var x : TGenLiteral;
  begin
  if FOperator = OpNeg then
     if FRecursive then
        x := FChild1.GetAtomicForm
     else
        begin
        x := TGenLiteral.Create;
        x.Operator := OpNone;
        x.Atom1 := FAtom1;
        end
  else
     if FRecursive then
        begin
        x := TGenLiteral.Create;
        x.Operator := FOperator;
        if FChild1 <> nil then x.Child1 := FChild1.GetAtomicForm;
        if FChild2 <> nil then x.Child2 := FChild2.GetAtomicForm;
        end
     else
        begin
        x := TGenLiteral.Create;
        x.Operator := FOperator;
        x.Atom1 := FAtom1;
        end;
  result := x;
  end;

{------------------------------------------------------------------------------}

function TGenLiteral.GetChild1: TGenLiteral;
  begin
  result := FChild1;
  end;

{------------------------------------------------------------------------------}

function TGenLiteral.GetChild2: TGenLiteral;
  begin
  result := FChild2;
  end;

{------------------------------------------------------------------------------}

function TGenLiteral.GetKnowledge: boolean;
  begin
  if FOperator = OpK then
     result := true
  else
     if FRecursive then
        result := FChild1.GetKnowledge
     else
        result := false;
  end;

{------------------------------------------------------------------------------}

function TGenLiteral.GetModalCount: integer;
  var tmp : integer;
  begin
  if FRecursive then
     begin
     if FChild1 <> nil then tmp := FChild1.GetModalCount;
     if FChild2 <> nil then tmp := tmp + FChild2.GetModalCount;
     end
  else
     tmp := 0;
  if FOperator in [OpNeg, OpNone] then
     result := tmp
  else
     result := tmp + 1;
  end;

{------------------------------------------------------------------------------}

function TGenLiteral.GetNegation: boolean;
  begin
  if FOperator = OpNeg then
     result := true
  else
     if FRecursive and (FChild1 <> nil) then
        result := FChild1.GetNegation
     else
        result := false;
  end;

{------------------------------------------------------------------------------}

{function TGenLiteral.GetNextTimeCount: integer;
  var tmp : integer;
  begin
  if FRecursive then
     tmp := FChild.GetNextTimeCount
  else
     tmp := 0;
  if FOperator = OpNextTime then
     result := tmp + 1
  else
     result := tmp;
  end;}

{------------------------------------------------------------------------------}

function TGenLiteral.GetOperator: TOperator;
  begin
  result := FOperator;
  end;

{------------------------------------------------------------------------------}

function TGenLiteral.GetRecursive: boolean;
  begin
  result := FRecursive;
  end;

{------------------------------------------------------------------------------}

function TGenLiteral.GetText(AtomList, AgentList: TStrings) : string;
  var s : string;
  begin
  case operator of
    OpNone          : s := '';
    OpNeg           : s := '~';
    OpBox           : s := '[]';
    OpDiamond       : s := '<>';
    OpNextTime      : s := '(+)';
    OpPreviousTime  : s := '(-)';
    OpFutureBox     : s := '[+]';
    OpPastBox       : s := '[-]';
    OpFutureDiam    : s := '<+>';
    OpPastDiam      : s := '<->';
    OpK             : s := '{K}';
{      if (FAgent >= 0) and (FAgent < AgentList.Count) then
         s := 'K ' + AgentList.Strings[FAgent] + ' '
      else
         s := '?'}
    else
      s := '';
    end;

  if FRecursive then
     case operator of
       OpSince :  result := '(' + Child1.GetText(AtomList, AgentList) + '{S}' +
                                  Child2.GetText(AtomList, AgentList) +')';
       OpZince :  result := '(' + Child1.GetText(AtomList, AgentList) + '{Z}' +
                                  Child2.GetText(AtomList, AgentList) +')';
       OpUntil :  result := '(' + Child1.GetText(AtomList, AgentList) + '{U}' +
                                  Child2.GetText(AtomList, AgentList) +')';
       OpUnless : result := '(' + Child1.GetText(AtomList, AgentList) + '{W}' +
                                  Child2.GetText(AtomList, AgentList) +')'
       else
          result := s + FChild1.GetText(AtomList, AgentList)
       end
  else
     if (FAtom1 >= 0) and (FAtom1 < AtomList.Count) then
        result := s + AtomList.Strings[FAtom1]
     else
        result := s + '?';
  end;

{------------------------------------------------------------------------------}

function TGenLiteral.IsEqual(VLiteral: TGenLiteral): boolean;
  var b : boolean;
  begin
  if Vliteral = nil then
     result := false
  else
    if FRecursive then
       begin
       b := (FOperator = VLiteral.FOperator);
       b := b and ((FOperator <> OpK) or (FAgent = VLiteral.Agent));
       if FChild1 <> nil then result := b and FChild1.IsEqual(VLiteral.Child1);
       if FChild2 <> nil then result := b and FChild2.IsEqual(VLiteral.Child2);
       end
    else
       begin                   //??? (Changed)
       b := (FOperator = VLiteral.FOperator);
       b := b and ((FOperator <> OpK) or (FAgent = VLiteral.Agent));
       b := b and (FAtom1 = (VLiteral.Atom1));
       result := b;     // and (FAtom2 = (VLiteral.Atom2));       

       end;
  end;

{------------------------------------------------------------------------------}

procedure TGenLiteral.SetAgent(const Value: integer);
  begin
  if (value >= 0) then
     FAgent := Value;
  //else raise exception
  end;

{------------------------------------------------------------------------------}

procedure TGenLiteral.SetAtom1(const Value: integer);
  begin
  if (Value >= 0) then
     FAtom1 := Value;
  end;

{------------------------------------------------------------------------------

procedure TGenLiteral.SetAtom2(const Value: integer);
  begin
  if (Value >= 0) then
     FAtom2 := Value;
  end;

{------------------------------------------------------------------------------}


procedure TGenLiteral.SetChild1(const Value: TGenLiteral);
  begin
  if FOperator <> OpNone then
     begin
     FChild1 := Value;
     FRecursive := true;
     end;
  //else raise exception;
  end;

{------------------------------------------------------------------------------}

procedure TGenLiteral.SetChild2(const Value: TGenLiteral);
  begin
  if FOperator in [OpSince, OpUntil, OpZince, OpUnless] then
     begin
     FChild2 := Value;
     FRecursive := true;
     end;
  //else raise exception;
  end;

{------------------------------------------------------------------------------}

procedure TGenLiteral.SetOperator(const Value: TOperator);
  begin
  if ((value <> OpNone) or (FChild1 = nil)) and
     ((value in [OpSince, OpUntil, OpZince, OpUnless]) or (FChild2 = nil)) then
     FOperator := value
   //else raise exception;
  end;

{------------------------------------------------------------------------------}

procedure TGenLiteral.SetRecursive(const Value: boolean);
  begin
  if value then
     FRecursive := true
  else
    if not (FOperator in [OpSince, OpUntil, OpZince, OpUnless]) then
       begin
       Child1.Free;
       FRecursive := false
       end;

  end;

{------------------------------------------------------------------------------}

{ TStaticDefinitions }

{------------------------------------------------------------------------------}

procedure TStaticDefinitions.AddAgent(item: string);
  begin
  FAgents.Add(item);
  end;

{------------------------------------------------------------------------------}

procedure TStaticDefinitions.AddAgentInPosition(item: string; index: integer);
  begin
  if (index >= 0) and (index < FIdCount) then
     begin
     FAgents.Add(item);
     FIdIndex[index] := FAgents.Count - 1;
     FIdKind[index]  := 1;
     end;
  end;

{------------------------------------------------------------------------------}

procedure TStaticDefinitions.AddAssociation(PAgent, PAtom: integer);
  begin
  FAssociationsCount := FAssociationsCount + 1;
  SetLength(FAssociations, FAssociationsCount * 2);
  FAssociations[(FAssociationsCount * 2) - 2] := PAtom;
  FAssociations[(FAssociationsCount * 2) - 1] := PAgent;
  end;

{------------------------------------------------------------------------------}

procedure TStaticDefinitions.AddAtom(item: string; index: integer);
  begin
  FAtoms.Add(item);
  SetLength(FOperReferences, FAtoms.Count);
  SetLength(FAtomReferences, FAtoms.Count);
  SetLength(FSecondAtomReferences, FAtoms.Count);
  FAtomReferences[FAtoms.Count - 1] := FAtoms.Count - 1;
  SetLength(FAgentReferences, FAtoms.Count);
  if (index >= 0) and (index < FIdCount) then
     begin
     FIdIndex[index] := FAtoms.Count - 1;
     FIdKind[index]  := 0;
     end;
  end;

{------------------------------------------------------------------------------}

function TStaticDefinitions.AddCompAtom(item: TGenLiteral): integer; //???
  var
    index1, index2, tmp, i : integer;
    addNew : boolean;
  begin
  tmp := FAtoms.Count;

  if item.IsRecursive then
     begin
     if item.Child1 <> nil then index1 := AddCompAtom(item.Child1) else index1 := -1;
     if item.Child2 <> nil then index2 := AddCompAtom(item.Child2) else index2 := -1;
     end
  else
     begin
     index1 := item.Atom1;
     index2 := -1;
     end;

  if (item.Operator = OpNeg) or (Item.Operator = OpNone) then
     result := index1
  else
     begin
     addNew := true;
     i := 0;
     if index1 < tmp then
        begin
        while (i < FAtoms.Count) and AddNew do
          begin
          AddNew := not(FOperReferences[i] = Item.Operator);
          AddNew := AddNew or not(FAtomReferences[i] = index1);
          AddNew := AddNew or not(FSecondAtomReferences[i] = index2);
          if FOperReferences[i] = OpK then
             AddNew := AddNew or not(FAgentReferences[i] = Item.Agent);
          i := i + 1;
          end;
        end;
     if addNew then
        begin
        AddAtom(item.GetText(FAtoms, FAgents), -1);
        FOperReferences[FAtoms.Count - 1] := item.Operator;
        FAtomReferences[FAtoms.Count - 1] := index1;
        FSecondAtomReferences[FAtoms.Count - 1] := index2;
        if item.Operator = OpK then
           FAgentReferences[FAtoms.Count - 1] := item.Agent;
        result := FAtoms.Count - 1;
        end
     else
        result := i - 1;
     end;
   end;

{------------------------------------------------------------------------------}

procedure TStaticDefinitions.AddDefInput(PLabel, PAtom: integer);
  begin
  FDefInputCount := FDefInputCount + 1;
  SetLength(FDefInputAtoms, FDefInputCount);
  SetLength(FDefInputLabels, FDefInputCount);
  FDefInputAtoms[FDefInputCount - 1]  := PAtom;
  FDefInputLabels[FDefInputCount - 1] := PLabel;
  end;

{------------------------------------------------------------------------------}

procedure TStaticDefinitions.AddDefOutput(PLabel, PAtom: integer);
  begin
  FDefOutputCount := FDefOutputCount + 1;
  SetLength(FDefOutputAtoms, FDefOutputCount);
  SetLength(FDefOutputLabels, FDefOutputCount);
  FDefOutputAtoms[FDefOutputCount - 1]  := PAtom;
  FDefOutputLabels[FDefOutputCount - 1] := PLabel;
  end;

{------------------------------------------------------------------------------}

procedure TStaticDefinitions.AddLabelInPosition(item: string; index: integer);
  begin
  if (index >= 0) and (index < FIdCount) then
     begin
     FLabels.Add(item);
     FIdIndex[index] := FLabels.Count - 1;
     FIdKind[index]  := 2;
     end;
  end;

{------------------------------------------------------------------------------}

procedure TStaticDefinitions.AddLabel(item: string);
  begin
  FLabels.Add(item);
  end;

{------------------------------------------------------------------------------}

procedure TStaticDefinitions.AddLinkPoint(Kind: TLinkKind);
  begin
  FLinksCount := FLinksCount + 1;
  SetLength(FLinkPoints, FLinksCount);
  FLinkPoints[FLinksCount - 1] := kind;
  end;

{------------------------------------------------------------------------------}

procedure TStaticDefinitions.AddRelation(source, target: integer);
  begin
  FRelationsCount := FRelationsCount + 1;
  SetLength(FRelations, FRelationsCount * 2);
  FRelations[(FRelationsCount * 2) - 2] := source;
  FRelations[(FRelationsCount * 2) - 1] := target;
  end;

{------------------------------------------------------------------------------}

function TStaticDefinitions.AgentCount: integer;
  begin
  result := FAgents.Count;
  end;

{------------------------------------------------------------------------------}

function TStaticDefinitions.AssociationCount: integer;
  begin
  result := FAssociationsCount;
  end;

{------------------------------------------------------------------------------}

function TStaticDefinitions.AtomCount: integer;
  begin
  result := FAtoms.Count;
  end;

{------------------------------------------------------------------------------}

procedure TStaticDefinitions.Clear;
  begin
  FAtoms.Clear;
  FAgents.Clear;
  FLabels.Clear;
  FRelationsCount    := 0;
  FAssociationsCount := 0;

  FDefInputCount := 0;
  SetLength(FDefInputAtoms, 0);
  SetLength(FDefInputLabels, 0);
  FDefOutputCount := 0;
  SetLength(FDefOutputAtoms, 0);
  SetLength(FDefOutputLabels, 0);

  FLinksCount := 0;
  //??? References and Ids
  end;

{------------------------------------------------------------------------------}

constructor TStaticDefinitions.Create;
  begin
  inherited Create;
  FAtoms  := TStringList.Create;
  FAgents := TStringList.Create;
  FLabels := TStringList.Create;
  FRelationsCount    := 0;
  FAssociationsCount := 0;

  FDefInputCount := 0;
  SetLength(FDefInputAtoms, 0);
  SetLength(FDefInputLabels, 0);
  FDefOutputCount := 0;
  SetLength(FDefOutputAtoms, 0);
  SetLength(FDefOutputLabels, 0);
  FLinksCount := 0;
  SetLength(FLinkPoints, 0);

  //??? Defs, Ids and References
  end;

{------------------------------------------------------------------------------}

procedure TStaticDefinitions.DeleteAgent(index: integer);
  begin
  if (index >= 0) and (index < FAgents.Count) then
     FAgents.Delete(index);
  end;

{------------------------------------------------------------------------------}

procedure TStaticDefinitions.DeleteAssociation(index: integer);
  var i : integer;
  begin
  if (index >= 0) and (index < FAssociationsCount) then
     begin
     FAssociationsCount := FAssociationsCount - 1;
     for i := (index * 2) to (FAssociationsCount * 2) - 1 do
       FAssociations[i] := FAssociations[i + 2];
     //SetLength(FAssociations, FAssociationCount);
     end;
  end;

{------------------------------------------------------------------------------}

procedure TStaticDefinitions.DeleteAtom(index: integer);
  begin
  if (index >= 0) and (index < FAtoms.Count) then
     FAtoms.Delete(index);
  end;

{------------------------------------------------------------------------------}

procedure TStaticDefinitions.DeleteLabel(index: integer);
  begin
  if (index >= 0) and (index < FLabels.Count) then
     FLabels.Delete(index);
  end;

{------------------------------------------------------------------------------}

procedure TStaticDefinitions.DeleteRelation(index: integer);
  var i : integer;
  begin
  if (index >= 0) and (index < FRelationsCount) then
     begin
     FRelationsCount := FRelationsCount - 1;
     for i := (index * 2) to (FRelationsCount * 2) - 1 do
       FRelations[i] := FRelations[i + 2];
     //SetLength(FRelations, FRelationsCount);
     end;
  end;

{------------------------------------------------------------------------------}

destructor TStaticDefinitions.Destroy;
  begin
  FLabels.Free;
  FAtoms.Free;
  FAgents.Free;
  inherited;
  end;

{------------------------------------------------------------------------------}

function TStaticDefinitions.GetAgent(index: integer): string;
  begin
  if (index >= 0) and (index < FAgents.Count) then
     result := FAgents.Strings[index]
  else
     result := '';
  end;

{------------------------------------------------------------------------------}

function TStaticDefinitions.GetAssocAgents(PAtom: integer;
       out list: array of integer): integer;
  var i, j : integer;
  begin
  j := 0;
  for i := 0 to FAssociationsCount - 1 do
    if FAssociations[i * 2] = PAtom then
       begin
       list[j] := FAssociations[(i * 2) + 1];
       j := j + 1;
       end;
  result := j;
  end;

{------------------------------------------------------------------------------}

function TStaticDefinitions.GetAssocAtoms(PAgent: integer;
  out list: array of integer): integer;
  var i, j : integer;
  begin
  j := 0;
  for i := 0 to FAssociationsCount - 1 do
    if FAssociations[(i * 2) + 1] = PAgent then
       begin
       list[j] := FAssociations[i * 2];
       j := j + 1;
       end;
  result := j;
  end;

{------------------------------------------------------------------------------}

function TStaticDefinitions.GetAtom(index: integer): string;
  begin
  if (index >= 0) and (index < FAtoms.Count) then
     result := FAtoms.Strings[index]
  else
     result := '';
  end;

{------------------------------------------------------------------------------}

function TStaticDefinitions.GetAtomIndex(atom: String): integer;
  var
    i : integer;
    b : boolean;
  begin
  b := false;
  i := 0;
  while (not b) and (i < FAtoms.Count) do
    begin
    b := FAtoms.Strings[i] = atom;
    i := i + 1;
    end;
  if b then result := i - 1 else result := 1;
  end;

{------------------------------------------------------------------------------}

function TStaticDefinitions.GetAtomsCount : integer;
  begin
  result := FAtoms.Count;
  end;

{------------------------------------------------------------------------------}  

function TStaticDefinitions.GetDefInputAtom(index: integer): integer;
  begin
  if (index >= 0) and (index < FDefInputCount) then
    result := FDefInputAtoms[index]
  else
    result := -1;
  end;

{------------------------------------------------------------------------------}

function TStaticDefinitions.GetDefInputCount: integer;
  begin
  result := FDefInputCount;
  end;

{------------------------------------------------------------------------------}

function TStaticDefinitions.GetDefInputLabel(index: integer): integer;
  begin
  if (index >= 0) and (index < FDefInputCount) then
    result := FDefInputLabels[index]
  else
    result := -1;
  end;

{------------------------------------------------------------------------------}

function TStaticDefinitions.GetDefOutputAtom(index: integer): integer;
  begin
  if (index >= 0) and (index < FDefOutputCount) then
    result := FDefOutputAtoms[index]
  else
    result := -1;
  end;

{------------------------------------------------------------------------------}

function TStaticDefinitions.GetDefOutputCount: integer;
  begin
  result := FDefOutputCount;
  end;

{------------------------------------------------------------------------------}

function TStaticDefinitions.GetDefOutputLabel(index: integer): integer;
  begin
  if (index >= 0) and (index < FDefOutputCount) then
    result := FDefOutputLabels[index]
  else
    result := -1;
  end;

{------------------------------------------------------------------------------}

function TStaticDefinitions.GetIdCount: integer;
  begin
  result := FIdCount;
  end;

{------------------------------------------------------------------------------}

function TStaticDefinitions.GetIndex(i: integer): integer;
  begin
  if (i >= 0) and (i < FIdCount) then
     result := FIdIndex[i]
  else
     result := -1;
  end;

{------------------------------------------------------------------------------}

function TStaticDefinitions.GetLabel(index: integer): string;
  begin
  if (index >= 0) and (index < FLabels.Count) then
     result := FLabels.Strings[index]
  else
     result := '';
  end;

{------------------------------------------------------------------------------}

function TStaticDefinitions.GetLinkPoints(index: integer): TLinkKind;
  begin
  if (index >= 0) and (index < FLinksCount) then
     result := FLinkPoints[index]
  else
     result := LkNone;
  end;

{------------------------------------------------------------------------------}

function TStaticDefinitions.GetLinksCount: integer;
  begin
  result := FLinksCount;
  end;

{------------------------------------------------------------------------------}

function TStaticDefinitions.GetLiteralText(Literal: TGenLiteral): string;
  begin
  result := Literal.GetText(FAtoms, FAgents);
  end;

{------------------------------------------------------------------------------}

function TStaticDefinitions.GetRefAgent(index: integer): integer; //???
  begin
  if (index >= 0) and (index < FAtoms.Count) then
     result := FAgentReferences[index]
  else
     result := -1;
  end;

{------------------------------------------------------------------------------}

function TStaticDefinitions.GetRefAtom(index: integer): integer; //???
  begin
  if (index >= 0) and (index < FAtoms.Count) then
     result := FAtomReferences[index]
  else
     result := -1;
  end;

{------------------------------------------------------------------------------}

function TStaticDefinitions.GetRefAtom2(index: integer): integer; //???
  begin
  if (index >= 0) and (index < FAtoms.Count) then
     result := FSecondAtomReferences[index]
  else
     result := -1;
  end;

{------------------------------------------------------------------------------}

function TStaticDefinitions.GetRefOperator(index: integer): TOperator; //???
  begin
  if (index >= 0) and (index < FAtoms.Count) then
     result := FOperReferences[index]
  else
     result := OpNone;
  end;

{------------------------------------------------------------------------------}

function TStaticDefinitions.GetRelatedFrom(PLabel: integer;
  out list: array of integer): integer;
  var i, j : integer;
  begin
  j := 0;
  for i := 0 to FRelationsCount - 1 do
    if FRelations[i * 2] = PLabel then
       begin
       list[j] := FRelations[(i * 2) + 1];
       j := j + 1;
       end;
  result := j;
  end;

{------------------------------------------------------------------------------}

function TStaticDefinitions.GetRelatedTo(PLabel: integer;
  out list: array of integer): integer;
  var i, j : integer;
  begin
  j := 0;
  for i := 0 to FRelationsCount - 1 do
    if FRelations[(i * 2) + 1] = PLabel then
       begin
       list[j] := FRelations[i * 2];
       j := j + 1;
       end;
  result := j;
  end;

{------------------------------------------------------------------------------}

function TStaticDefinitions.IsAgent(index: integer): boolean;
  begin
  result := (index >= 0) and (index < FIdCount) and (FIdKind[index] = 1);
  end;

{------------------------------------------------------------------------------}

function TStaticDefinitions.IsAtom(index: integer): boolean;
  begin
  result := (index >= 0) and (index < FIdCount) and (FIdKind[index] = 0);
  end;

{------------------------------------------------------------------------------}

function TStaticDefinitions.IsLabel(index: integer): boolean;
  begin
  result := (index >= 0) and (index < FIdCount) and (FIdKind[index] = 2);
  end;

{------------------------------------------------------------------------------}

function TStaticDefinitions.LabelCount: integer;
  begin
  result := FLabels.Count;
  end;

{------------------------------------------------------------------------------}

function TStaticDefinitions.RelationCount: integer;
  begin
  result := FRelationsCount;
  end;

{------------------------------------------------------------------------------}

procedure TStaticDefinitions.SetAgent(index: integer; const Value: string);
  begin
  if (index >= 0) and (index < FAgents.Count) then
     FAgents.Strings[index] := value;
  end;

{------------------------------------------------------------------------------}

procedure TStaticDefinitions.SetAtom(index: integer; const Value: string);
  begin
  if (index >= 0) and (index < FAtoms.Count) then
     FAtoms.Strings[index] := value;
  end;

{------------------------------------------------------------------------------}

procedure TStaticDefinitions.SetIdCount(const Value: integer);
  var i, inic : integer;
  begin
  inic := FIdCount;
  if inic < 0 then inic := 0;
  FIdCount := value;
  SetLength(FIdKind, FIdCount);
  SetLength(FIdIndex, FIdCount);
  for i := inic to FIdCount - 1 do
    begin
    FIdKind[i] := -1;
    FIdIndex[i] := -1;
    end
  end;

{------------------------------------------------------------------------------}

procedure TStaticDefinitions.SetLabel(index: integer; const Value: string);
  begin
  if (index >= 0) and (index < FLabels.Count) then
     FLabels.Strings[index] := value;
  end;

{------------------------------------------------------------------------------}

procedure TStaticDefinitions.WriteHeader(txt: TStringList; Style: TStyle);
  var i : integer;
  begin
  case Style of
    StClassic :
      begin
      txt.Add('style classic');
      for i := 0 to FAtoms.Count - 1 do
        txt.Add('atom ' + FAtoms.Strings[i]);
      end;

    StModal :
      begin
      txt.Add('style modal');
      for i := 0 to FAtoms.Count - 1 do
        txt.Add('atom ' + FAtoms.Strings[i]);
      for i := 0 to FLabels.Count - 1 do
        txt.Add('label ' + FLabels.Strings[i]);
      for i := 0 to FRelationsCount - 1 do
        txt.Add('relation ' + FLabels.Strings[FRelations[i * 2]] + ' ' +
                FLabels.Strings[FRelations[(i * 2) + 1]]);
      end;

    StSCTL :
      begin
      txt.Add('style SCTL');
      for i := 0 to FAtoms.Count - 1 do
        txt.Add('atom ' + FAtoms.Strings[i]);
      end;

{    StTemporal2 :
      begin
      txt.Add('style temporal2');
      for i := 0 to FAtoms.Count - 1 do
        txt.Add('atom ' + FAtoms.Strings[i]);
      end;

    StEpistemic :
      begin
      txt.Add('style epistemic');
      for i := 0 to FAtoms.Count - 1 do
        txt.Add('atom ' + FAtoms.Strings[i]);
      for i := 0 to FAgents.Count - 1 do
        txt.Add('agent ' + FAgents.Strings[i]);
      for i := 0 to FRelationsCount -1 do
        txt.Add('knows ' + FAgents.Strings[FAssociations[i * 2]] + ' ' +
                           FAgents.Strings[FAssociations[(i * 2) + 1]]);
      for i := 0 to FAssociationsCount -1 do
        txt.Add('associate ' + FAtoms.Strings[FAssociations[(i * 2) + 1]] + ' '
                             + FAgents.Strings[FAssociations[(i * 2)]]);
      end;}

    StCTLK :
      begin
      txt.Add('style CTLK');
      for i := 0 to FAtoms.Count - 1 do
        txt.Add('atom ' + FAtoms.Strings[i]);
      for i := 0 to FAgents.Count - 1 do
        txt.Add('agent ' + FAgents.Strings[i]);
      for i := 0 to FLabels.Count - 1 do
        txt.Add('time ' + FLabels.Strings[i]);
      for i := 0 to FRelationsCount -1 do
        txt.Add('knows ' + FAgents.Strings[FAssociations[i * 2]] + ' ' +
                           FAgents.Strings[FAssociations[(i * 2) + 1]]);
      for i := 0 to FAssociationsCount -1 do
        txt.Add('associate ' + FAtoms.Strings[FAssociations[(i * 2) + 1]] + ' '
                             + FAgents.Strings[FAssociations[(i * 2)]]);
      end;
    end;

  for i := 0 to FDefInputCount - 1 do
    txt.Add('input ' + FAtoms.Strings[FDefInputAtoms[i]]);

  for i := 0 to FDefOutputCount - 1 do
    txt.Add('output ' + FAtoms.Strings[FDefOutputAtoms[i]]);
  end;

{------------------------------------------------------------------------------}

{ TGenClause }

{------------------------------------------------------------------------------}

procedure TGenClause.AddBodyLiteral(literal: TGenLiteral);
  begin
  FBodyCount := FBodyCount + 1;
  SetLength(FBody, FBodyCount);
  FBody[FBodyCount - 1] := literal;
  LoadText;
  end;

{------------------------------------------------------------------------------}

constructor TGenClause.Create(Structure: TStaticDefinitions);
  begin
  FStructure   := Structure;
  FBodyCount   := 0;
  FDisjunctive := false;
  FDefault     := false;
  FHead        := nil;
  FLabel       := -1;
  FText        := '';
  end;

{------------------------------------------------------------------------------}

destructor TGenClause.Destroy;
  var i : integer;
  begin
  FHead.Free;
  for i := 0 to FBodyCount - 1 do
    FBody[i].Free;
  inherited;
  end;

{------------------------------------------------------------------------------}

function TGenClause.GetBody(i: integer): TGenLiteral;
  begin
  if (i >= 0) and (i < FBodyCount) then
     result := FBody[i]
  else
     result := nil;
  end;

{------------------------------------------------------------------------------}

function TGenClause.GetBodySize: integer;
  begin
  result := FBodyCount
  end;

{------------------------------------------------------------------------------}

function TGenClause.GetDefault: boolean;
  begin
  result := FDefault;
  end;

{------------------------------------------------------------------------------}

function TGenClause.GetDisjunctive: boolean;
  begin
  result := FDisjunctive;
  end;

{------------------------------------------------------------------------------}

function TGenClause.GetHead: TGenLiteral;
  begin
  result := FHead;
  end;

{------------------------------------------------------------------------------}

function TGenClause.GetLabel: integer;
  begin
  result := FLabel;
  end;

{------------------------------------------------------------------------------}

function TGenClause.GetText: string;
  begin
  result := FText;
  end;

{------------------------------------------------------------------------------}

procedure TGenClause.LoadText;
  var
    SBody, SHead : string;
    i : integer;
  begin
  SBody := '';
  if FBodyCount > 0 then
     begin
     SBody := FStructure.GetLiteralText(FBody[0]);
     for i := 1 to FBodyCount - 1 do
       if FDisjunctive then
          SBody := SBody + '; ' + FStructure.GetLiteralText(FBody[i])
       else
          SBody := SBody + ', ' + FStructure.GetLiteralText(FBody[i]);
     end;

  if FHead = nil then
     Shead := ''
  else
     if FDefault then
        SHead := FStructure.GetLiteralText(FHead) + ' ::- '
     else
        SHead := FStructure.GetLiteralText(FHead) + ' :- ';

  if FLabel >= 0 then
     FText := FStructure.Labels[FLabel] + ': ' + SHead + SBody
  else
     FText := SHead + SBody;
  end;

{------------------------------------------------------------------------------}

function TGenClause.MaxModalOperators: integer;
  var
    i : integer;
    m : integer;
  begin
  m := Head.GetModalCount;
  for i := 0 to FBodyCount - 1 do
    if m < FBody[i].GetModalCount then
       m := FBody[i].GetModalCount;
  result := m;
  end;

{------------------------------------------------------------------------------}

function TGenClause.MaxNextTimeOperators: integer;
  var
    i : integer;
    m : integer;
  begin
{//  m := Head.GetNextTimeCount;  ????****
  m := 0;
  for i := 0 to FBodyCount - 1 do
    if m < FBody[i].GetNextTimeCount then
       m := FBody[i].GetNextTimeCount;
  result := m;}
  end;

{------------------------------------------------------------------------------}

procedure TGenClause.SetBody(i: integer; const Value: TGenLiteral);
  begin
  if (i >= 0) and (i < FBodyCount) then
     begin
     FBody[i] := value;
     LoadText;
     end;
  //else raise exception
  end;

{------------------------------------------------------------------------------}

procedure TGenClause.SetDefault(const Value: boolean);
  begin
  FDefault := value;
  LoadText;
  end;

{------------------------------------------------------------------------------}

procedure TGenClause.SetDisjunctive(const Value: boolean);
  begin
  FDisjunctive := value;
  LoadText;
  end;

{------------------------------------------------------------------------------}

procedure TGenClause.SetHead(const Value: TGenLiteral);
  begin
  if not Value.HasNegation then
     begin
     FHead := value;
     LoadText;
     end;
  //else raise exception
  end;

{------------------------------------------------------------------------------}

procedure TGenClause.SetLabel(const Value: integer);
  begin
  if (value >= 0) and (value < FStructure.LabelCount) then
     begin
     FLabel := value;
     LoadText;
     end;
  end;

{------------------------------------------------------------------------------}

end.
