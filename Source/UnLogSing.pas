{
  @abstract(Unit with the routines to manipulate classical (single-world) logic programs)
  @author(Rafa "Passarel" Borges <passarel@gmail.com)
  @created()
  @lastmod(Porto Alegre, June 21, 2006)
}
unit UnLogSing; //1174 lines

interface

uses Classes, UnLogStruct, UnNetRep;

{------------------------------------------------------------------------------}

type TSimplificationEvent = procedure(fileName : string; size : integer; p1, p2 : double) of Object;

{
@abstract(Class encapsulating a (classical) logic clause)

Each instance of the class @name represents a clause on the form A :- L0, ..., Ln,
where A is an atom and Li (0 <= i <= n) is an literal (atom or its negation).
Each atom is defined by an index (index >= 1) and the negation of an atom is
represented by the negative form of its index.
}
type TSingleClause = class
  private
    //Text representation of the clause
    FText : string;

    //Head of the clause
    FHead : integer;

    //Array of body literals of the clause
    FBody : array of integer;

    //Count of literals in the body of the clause
    FBodyCount : integer;

    //Identify if it is a default clause (represented by the operator ::- instead of :-)
    FDefault : boolean;

    //Indicates if it is a disjunctive clause (???)
    FDisjunctive : boolean;

    //Structure to be used to describe the items on the clause
    FStructure : TStaticDefinitions;

    //List of dynamic facts inserted on the clause
    FDynamicFacts : TStringList;

    //Extra value keeping an integer count
    FCount : integer;

    //Extra value keeping an weight
    FWeight : double;

    //Indicates whether the extra information should be returned as text
    FPrintExtraInfo : boolean;



    //Loads the text representation of the clause
    procedure LoadText;

    //Read method of the property Body
    function GetBody(i: integer): integer;

    //Read method of the property Head
    function GetHead: integer;

    //Read method of the property Text
    function GetText: string;

    //Write method of the property Head
    procedure SetHead(const Value: integer);

    //Write method of the property Body
    procedure SetBody(i: integer; const Value: integer);

    //Read method of the property Disjunctive
    function GetDisjunctive: boolean;

    //Write method of the property Disjunctive
    procedure SetDisjunctive(const Value: boolean);

    //Read method of the property BodySize
    function GetBodySize: integer;

    //Read method of the property Default
    function GetDefault: boolean;

    //Write method of the property Default
    procedure SetDefault(const Value: boolean);

    //Write method of the property Text
    procedure SetText(const Value: string);

    //Read Method of the property Count
    function GetCount : integer;

    //Read Method of the property Weight
    function GetWeight : double;

    //Read Method of the property PrintExtraInfo
    function GetPrintExtraInfo : boolean;

    //Write Method of the property Count
    procedure SetCount(const Value : integer);

    //Write Method of the property Weight
    procedure SetWeight(const Value : double);

    //Write Method of the property PrintExtraInfo
    procedure SetPrintExtraInfo(const Value : boolean);




  public
    //Constructor of the class
    constructor Create(struct : TStaticDefinitions; DynFacts : TStringList);

    //Destructor of the class
    destructor  Destroy; override;

    //Destructor of the class
    procedure ClearBody;

    //Count of literals in the body of the clause
    property BodySize : integer read GetBodySize;

    //Text representation of the clause
    property Text : string  read GetText write SetText;

    //Index of the atom on the head of the clause
    property Head : integer read GetHead write SetHead;

    //Access a body literal of the clause
    property Body[i : integer] : integer read GetBody write SetBody;

    //Indicates if it is a disjunctive clause (???)
    property Disjunctive : boolean read GetDisjunctive write SetDisjunctive;

    //Identify if it is a default clause (represented by the operator ::- instead of :-)
    property IsDefault : boolean read GetDefault write SetDefault;

    //Extra value keeping an integer count
    property Count : integer read GetCount write SetCount;

    //Extra value keeping a weight
    property Weight : double read GetWeight write SetWeight;

    //Indicates whether the extra information should be returned as text
    property PrintExtraInfo : boolean read GetPrintExtraInfo write SetPrintExtraInfo;




    //Set the list of Dynamic Facts
    //@param(list: New list ???)
    procedure SetDynamicFacts(list : TStringList);

    //Adds a literal on the body of the clause
    //@param(literal: literal to be added)
    procedure AddBodyLiteral (literal : integer);

    //Loads the clause from a general one
    //@param(cl: General clause with configuration to br loaded)
    procedure LoadFromGeneral(cl : TGenClause);

    //Verify if an atom is on the body of the clause, returning the index of atom
    //@param(atom: atom to be verified)
    function IsOnBody (atom : integer) : integer;

    //Verify if the clause has a dynamic fact
    function IsDynamic : boolean;

    //Delete a body literal, specified by the index
    function DeleteByIndex(index : integer) : boolean;

    //Delete a body literal by the code of the atom
    function DeleteByAtom(code : integer) : boolean;

    //Set the interpretation of the head as true if the body is satisfied
    procedure TP(size: integer; inputInt : array of boolean; var outputInt : array of boolean);

    function GetCopy : TSingleClause;




  end;

{------------------------------------------------------------------------------}

{
@abstract(Class encapsulating a set of (classical)logic clauses)

Each instance of the class @name represents a set of logic clauses, and also
presents an structure that identify all the atoms on the clauses. The class also
presents routines to manipulate the program and translate it to a neural network
structure
}
type TSingleLogicProgram = class
  private

      FMegaCountArray : array of integer;
      FMegaWeightArray : array of double;


    //Struct which the program is based
    FStructure : TStaticDefinitions;

    //Clauses on the program
    FClauses : TStringList;

    //Dynamic Facts on the program (Generated according the "dynamic ideas")
    FDynamicFacts      : TStringList;

    //Count of input links on the program
    FInputLinksCount   : integer;

    //Linked atoms on the input of the program
    FInputLinksAtoms   : array of integer;

    //???
    FInputLinksRef     : array of integer;

    //Kind of the input link
    FInputLinksTransf  : array of TOpTransf;

    //Index of the linked atom in the array of Dynamic Facts
    FInputLinksDynFact : array of integer;

    //???
    FDynFactAtoms      : array of integer;

    //Count of output links on the program
    FOutputLinksCount  : integer;

    //Linked atoms on the output of the program
    FOutputLinksAtoms  : array of integer;

    //???
    FOutputLinksRef    : array of integer;

    //Kind of the output link
    FOutputLinksTransf : array of TOpTransf;

    FKarnCountMaps : array of array of integer;
    FKarnWeightMaps : array of array of double;
    FKarnaughMapsCount : integer;
    FKarnaughMapSize : integer;

    FDebugFile : string;
    FSimplificationEvent : TSimplificationEvent;

    //Gets the index of a existing dynamic fact if it is in the list. Else, adds it.
    //@param(index: Reference??? of the Dynamic Fact)
    //return(Ihe position where the dynFact were found/inserted)
    function  GetDynFact(index : integer) : integer;

    //Gets a string representing the Transformation of operator
    //@param(OpTransf: A Transformator of modal operator)
    function  GetTransfText(OpTransf : TOpTransf) : string;

    //Update the strings representing the clauses according the objects
    //@param(PrintExtraInfo : Defines if weight and count of each clause will appear, default = false)
    procedure UpdateClauses(PrintExtraInfo : boolean = false);

    function LittleReduction(cl1, cl2 : TSingleClause; out ClOut : TSingleClause) : boolean;

    function IsContained(cl1, cl2: TSingleClause) : boolean;

    //Perform the reduction operation between two clauses
    //@param(ClauseA: index of first clause)
    //@param(ClauseB: index of second clause)
    //return(Index representing the kind of operation performed).
    function JoinClauses(ClauseA, ClauseB : integer) : integer;
    function GetCount: integer;
    function GetClauseCopy(index: integer): TSingleClause;

    procedure MarkArray(currentMap : integer; inputs : array of integer; index, position,
            clCount : integer; clWeight : double);





    function KarnNewClauses(CurrentMap : integer; positive : boolean; variable : integer;
                            inputVar : array of integer) : integer;
    function GetFirstKarnPos(varCount, total : integer) : integer;
    function GetNextKarnPos(currentPos, varCount, total : integer) : integer;
    function GetRelatedArray(out arRelated : array of integer; position, varCount, total : integer; moreVar : boolean) : integer;
    function GetLeaves(var arLeaves : array of integer; initial, position, total : integer) : integer;

    procedure NullSimpEvent(fileName : string; size : integer; p1, p2 : double);


  public
    //Return the number of atoms in the program
    function GetAtomCount : integer;

    //Return the number of atoms linked as input in the program
    function GetInputLinkCount : integer;

    //Return the number of atoms linked as output in the program
    function GetOutputLinkCount : integer;


    //Constructor of the class
    constructor Create;

    //Constructor of the class
    //@param(Structure: Structure of the program)
    constructor CreateFromStruct(structure : TStaticDefinitions);

    property SimplificationEvent : TSimplificationEvent read FSimplificationEvent write FSimplificationEvent;

    //Destructor of the class
    destructor  Destroy; override;

    //Destructor of the class
    destructor  freeStructure;

    //Clear the list of clauses
    procedure ClearClauses;

    //Add a clause
    //@param(FClause: Clause to be added)
    procedure AddClause(FClause : TSingleClause);

    //Delete a Clause
    //@param(i: Index of the clause to be deleted)
    procedure DeleteClause(i : integer);

    //Add a link on the output
    //@param(atom: Atom to be linked)
    //@param(ref: Reference of the link???)
    //@param(transf: kind of the link)
    procedure AddOutputLink(atom : integer; ref : integer; transf : TOpTransf);

    //Add a link on the input
    //@param(atom: Atom to be linked)
    //@param(ref: Reference of the link???)
    //@param(transf: kind of the link)
    procedure AddInputLink(atom : integer; ref : integer; transf : TOpTransf);

    procedure AddStrucAtom(Atom: string);

    //Save the program to a string List for verification
    //@param(StrList: StrList to be filled with the program)
    //@param(b: Indicates if the dynamic facts must also be saved)
    procedure LoadStringList(strList : TStringList; b : boolean);

    //Save the set of clauses to a file
    //@param(FileName : Name of the file to save)
    //@param(PrintExtraInfo : Defines if weight and count of each clause will appear, default = false)
    procedure SaveToFile(FileName : string; PrintExtraInfo : boolean = false);

    //Verify if an atom is body of some clause
    //@param(atom: Atom to be verified)
    //return(true if the atom is body of some clause)
    function  isBody(atom : integer) : boolean;

    //Verify if an atom is body of some clause
    //@param(atom: Atom to be verified)
    //return(true if the atom is head of some clause)
    function  isHead(atom : integer) : boolean;

    //Verify if an atom can be removed on the simplification process
    //@param(atom: Atom to be verified)
    //return(true if the atom can be removed)
    function  isRemovable(atom : integer) : boolean;

    //Verify if an atom can be replaced on the simplification process
    //@param(atom: Atom to be verified)
    //return(true if the atom can be replaced)
    function  isReplaceable(atom : integer) : boolean;

    //Dynamize the program (according the "dynamic ideas")
    //@param(unified : ???)
    procedure DynamizeProgram(unified : boolean);

    //Simplify the program (according the "dynamic ideas")
    procedure SimplifyProgram;

    //Cut other atoms, removing processing chains
    procedure NewSimplification;

    //Reduce the number of clauses, by joining them and eliminating redundances
    procedure ReduceClauses;

    //Reduce the number of clauses, by joining them and eliminating redundances
    procedure KarnaughBased;

    //Reduce the number of clauses, by joining them and eliminating redundances
    procedure KarnaughBased2(OutputVar, InputVar : array of integer);

    //Translate the program to an CILP network
    //@param(rede: Representation of the network to be saved)
    //@param(world: Reference of a possible world in modal environments)
    //@param(Beta: Parameter of the activation function (default - 1))
    procedure SaveToCILPNetwork(rede : TNetworkRep; world : integer; Beta : double = 1);

    //Execute the Immediate Consequence Operator of the program over a interpretation inputInt
    procedure TP(size: integer; inputInt : array of boolean; var outputInt : array of boolean);

    //Save the program to a string list
    //@param(StringList : Defines the stringList where the data will be saved into)
    //@param(PrintExtraInfo : Defines if weight and count of each clause will appear, default = false)

    procedure SaveToStringList(StringList: TStrings; PrintExtraInfo : boolean = false);

    property ClausesCount : integer read GetCount;

    property ClauseCopy[index : integer] : TSingleClause read GetClauseCopy;

    procedure SpecialConcat(tmpProg : TSingleLogicProgram; head : integer = -2);

    //Return the Structure
    function GetStructure : TStaticDefinitions;

    procedure SetDebugFile(fileName : string);
  end;

{------------------------------------------------------------------------------}

implementation

uses SysUtils, Math, Forms;

{------------------------------------------------------------------------------}

{ TSingleClause }

procedure TSingleClause.AddBodyLiteral(literal: integer);
  begin
  FBodyCount := FBodyCount + 1;
  SetLength(FBody, FBodyCount);
  FBody[FBodyCount - 1] := literal;
  LoadText;
  end;

{------------------------------------------------------------------------------}

procedure TSingleClause.ClearBody;
  begin
  FBodyCount := 0;
  SetLength(FBody, 0);
  end;

{------------------------------------------------------------------------------}

constructor TSingleClause.Create(struct: TStaticDefinitions; DynFacts: TStringList);
  begin
  inherited Create;
  FBodyCount      := 0;
  FDynamicFacts   := DynFacts;
  FStructure      := struct;
  FDisjunctive    := false;
  FDefault        := false;
  FHead           := 0;
  FCount          := 1;
  FWeight         := 0;
  FText           := '';
  FPrintExtraInfo := false;
  end;

{------------------------------------------------------------------------------}

function TSingleClause.DeleteByAtom(code: integer): boolean;
  begin
  result := DeleteByIndex(IsOnBody(code));
  end;

{------------------------------------------------------------------------------}

function TSingleClause.DeleteByIndex(index: integer): boolean;
  var i : integer;
  begin
  if (index >= 0) and (index < FBodyCount) then
     begin
     for i := index to FBodyCount - 2 do
       FBody[i] := FBody[i + 1];
     SetLength(FBody, FBodyCount - 1);
     FBodyCount := FBodyCount - 1;
     result := true;
     end
  else
     result := false;
  LoadText;
  end;

{------------------------------------------------------------------------------}

destructor TSingleClause.Destroy;
  begin
  SetLength(FBody, 0);
  inherited;
  end;

{------------------------------------------------------------------------------}

function TSingleClause.GetBody(i: integer): integer;
  begin
  if (i >= 0) and (i < FBodyCount) then
     result := FBody[i]
  else
     result := 0;
  end;

{------------------------------------------------------------------------------}

function TSingleClause.GetBodySize: integer;
  begin
  result := FBodyCount;
  end;

{------------------------------------------------------------------------------}

function TSingleClause.GetCopy: TSingleClause;
  var
    cl : TSingleClause;
    i : integer;
  begin
  cl := TSingleClause.Create(FStructure, FDynamicFacts);
  cl.Head := FHead;
  for i := 0 to FBodyCOunt - 1 do
    cl.AddBodyLiteral(FBody[i]);
  cl.IsDefault := fDefault;
  cl.Disjunctive := FDisjunctive;
  cl.LoadText;
  cl.Count := FCount;
  cl.Weight := FWeight;
  result := cl;
  end;

{------------------------------------------------------------------------------}

function TSingleClause.GetCount : integer;
  begin
  result := FCount;
  end;

{------------------------------------------------------------------------------}

function TSingleClause.GetDefault: boolean;
  begin
  result := FDefault
  end;

{------------------------------------------------------------------------------}

function TSingleClause.GetDisjunctive: boolean;
  begin
  result := FDisjunctive;
  end;

{------------------------------------------------------------------------------}

function TSingleClause.GetHead: integer;
  begin
  result := FHead;
  end;

{------------------------------------------------------------------------------}

function TSingleClause.GetPrintExtraInfo : boolean;
  begin
  result := FPrintExtraInfo;
  end;

{------------------------------------------------------------------------------}  

function TSingleClause.GetText: string;
  begin
  result := FText;
  end;

{------------------------------------------------------------------------------}

function TSingleClause.GetWeight : double;
  begin
  result := FWeight;
  end;

{------------------------------------------------------------------------------}

function TSingleClause.IsDynamic: boolean;
  var
    i : integer;
    b : boolean;
  begin
  b := false;
  i := 0;
  while (i < FBodyCount) and (not b) do
    begin
    b := abs(FBody[i]) > FStructure.AtomCount;
    i := i + 1;
    end;
  result := b;
  end;

{------------------------------------------------------------------------------}

function TSingleClause.IsOnBody(atom: integer): integer;
  var
    b : boolean;
    i : integer;
  begin
  i := 0;
  b := false;
  while (i < FBodyCount) and (not b) do
    begin
    b := (atom = abs(FBody[i]));
    i := i + 1;
    end;
  if b then
     result := i - 1
  else
     result := -1;
  end;

{------------------------------------------------------------------------------}

procedure TSingleClause.LoadFromGeneral(cl: TGenClause);
 var i : integer;
 begin
 FHead := FStructure.AddCompAtom(Cl.Head.GetAtomicForm) + 1;
 for i := 0 to cl.BodySize - 1 do
   if cl.Body[i].HasNegation then
      AddBodyLiteral(- (FStructure.AddCompAtom(cl.Body[i].GetAtomicForm) + 1))
   else
      AddBodyLiteral(FStructure.AddCompAtom(cl.Body[i].GetAtomicForm) + 1);
 end;

{------------------------------------------------------------------------------}

procedure TSingleClause.LoadText;
  var
    SBody, ss : string;
    i : integer;
  begin
  SBody := '';
  if FBodyCount > 0 then
     begin
     if abs(FBody[0]) <= FStructure.AtomCount then
        ss := FStructure.Atoms[abs(FBody[0]) -1]
     else
        ss := FDynamicFacts.Strings[abs(FBody[0]) - (FStructure.AtomCount + 1)];
     if FBody[0] < 0 then
        SBody := '~' + ss
     else
        SBody := ss;

     for i := 1 to FBodyCount - 1 do
       begin
       if abs(FBody[i]) <= FStructure.AtomCount then
          ss := FStructure.Atoms[abs(FBody[i]) - 1]
       else
          ss := FDynamicFacts.Strings[abs(FBody[i]) - (FStructure.AtomCount + 1)];
       if FDisjunctive then
          begin
          if FBody[i] < 0 then
             SBody := SBody +  '; ~' + ss
          else
             SBody := SBody + '; ' + ss;
          end
       else
          begin
          if FBody[i] < 0 then
             SBody := SBody +  ', ~' + ss
          else
             SBody := SBody + ', ' + ss;
          end;
       end;

     if FDefault then
        FText := FStructure.Atoms[FHead - 1] + ' ::- ' + SBody
     else
        FText := FStructure.Atoms[FHead - 1] + ' :- ' + SBody
     end
  else
     FText := FStructure.Atoms[FHead - 1];
  if FPrintExtraInfo then
     FText := FText + ' _count ' + inttostr(FCount) + ' _weight ' + floattostr(FWeight);
  end;

{------------------------------------------------------------------------------}

procedure TSingleClause.SetBody(i: integer; const Value: integer);
  begin
  if (i >= 0) and (i < FBodyCount) then
     FBody[i] := value;
  LoadText;
  end;

{------------------------------------------------------------------------------}

procedure TSingleClause.SetCount(const Value : integer);
  begin
  FCount := Value;
  end;

{------------------------------------------------------------------------------}

procedure TSingleClause.SetDefault(const Value: boolean);
  begin
  FDefault := value;
  end;

{------------------------------------------------------------------------------}

procedure TSingleClause.SetDisjunctive(const Value: boolean);
  begin
  FDisjunctive := value;
  end;

{------------------------------------------------------------------------------}

procedure TSingleClause.SetDynamicFacts(list: TStringList);
  begin
  if FDynamicFacts = nil then      //???
     FDynamicFacts := list;
  end;

{------------------------------------------------------------------------------}

procedure TSingleClause.SetHead(const Value: integer);
  begin
  FHead := value;
  LoadText;
  end;

{------------------------------------------------------------------------------}

procedure TSingleClause.SetPrintExtraInfo(const Value : boolean);
  begin
  FPrintExtraInfo := Value;
  LoadText;
  end;

{------------------------------------------------------------------------------}

procedure TSingleClause.SetText(const Value: string);
  var
    b : boolean;
    i : integer;
    SHead, Sbody : string;
    XBody : TStringList;
  begin
  b := false;
  i := 1;
  while (not b) and (i < Length(value)) do
    begin
    b := (value[i] = ':') and (value[i + 1] = '-');
    i := i + 1;
    end;

  if b then
     begin
     SHead := Copy(value, 1, i - 2);
     SBody := Copy(value, i + 1, Length(value));
     FHead := FStructure.GetAtomIndex(trim(SHead)) + 1;
     XBody := TStringList.Create;
     XBody.Delimiter := ',';
     XBody.DelimitedText := SBody;
     ClearBody;
     for i := 0 to XBody.Count - 1 do
       begin
       SBody := trim(XBody.Strings[i]);
       if (SBody[1] = '¬') or (SBody[1] = '~') then
          AddBodyLiteral(- (FStructure.GetAtomIndex(trim(Copy(SBody, 2, length(sbody)))) + 1))
       else
          AddBodyLiteral(FStructure.GetAtomIndex(trim(sbody)) + 1);
       end;
     FText := value;
     end
  else
     FText := '';
  end;

{------------------------------------------------------------------------------}

procedure TSingleClause.SetWeight(const Value : double);
  begin
  FWeight := Value;
  end;

{------------------------------------------------------------------------------}

procedure TSingleClause.TP(size: integer; inputInt: array of boolean;
  var outputInt: array of boolean);
  var
    i, j : integer;
    b, XBodyForm: boolean;
  begin
  if ((FHead - 1) < size) and (not OutputInt[FHead - 1]) then
     begin
     i := 0;
     b := true;
     while b and (i < FBodyCount) do
       begin
       j := IsOnBody(i + 1);
       if j >= 0 then
          begin
          XBodyForm := FBody[j] > 0;
          b := (InputInt[i] = XBodyForm);
          end;
       i := i + 1;
       end;
     OutputInt[FHead - 1] := b;
     end;
  end;

{------------------------------------------------------------------------------}

{ TSingleLogicProgram }

{------------------------------------------------------------------------------}

procedure TSingleLogicProgram.AddClause(FClause: TSingleClause);
  begin
  if FClause <> nil then
     FClauses.AddObject(FClause.Text, FClause);
  end;

{------------------------------------------------------------------------------}

procedure TSingleLogicProgram.AddInputLink(atom,ref: integer; transf: TOpTransf);
  begin
  FInputLinksCount := FInputLinksCount + 1;
  setLength(FInputLinksAtoms, FInputLinksCount);
  setLength(FInputLinksRef, FInputLinksCount);
  setLength(FInputLinksTransf, FInputLinksCount);
  FInputLinksAtoms[FInputLinksCount - 1] := atom + 1;
  FInputLinksRef[FInputLinksCount - 1] := ref;
  FInputLinksTransf[FInputLinksCount - 1] := transf;
  end;

{------------------------------------------------------------------------------}

procedure TSingleLogicProgram.AddOutputLink(atom,ref: integer; transf: TOpTransf);
  begin
  FOutputLinksCount := FOutputLinksCount + 1;
  setLength(FOutputLinksAtoms, FOutputLinksCount);
  setLength(FOutputLinksRef, FOutputLinksCount);
  setLength(FOutputLinksTransf, FOutputLinksCount);
  FOutputLinksAtoms[FOutputLinksCount - 1] := atom + 1;
  FOutputLinksRef[FOutputLinksCount - 1] := ref;
  FOutputLinksTransf[FOutputLinksCount - 1] := transf;
  end;

{------------------------------------------------------------------------------}

procedure TSingleLogicProgram.AddStrucAtom(Atom: string);
  begin
  FStructure.AddAtom(atom, -1);
  end;

{------------------------------------------------------------------------------}

procedure TSingleLogicProgram.ClearClauses;
  var i : integer;
  begin
  for i := 0 to FClauses.Count - 1 do
    FClauses.Objects[i].Free;
  FClauses.Clear;
  end;

{------------------------------------------------------------------------------}

constructor TSingleLogicProgram.CreateFromStruct(structure: TStaticDefinitions);
  begin
  inherited Create;
  FStructure := structure;
  FClauses := TStringList.Create;
  FInputLinksCount := 0;
  FOutputLinksCount := 0;
  FSimplificationEvent := NullSimpEvent;  
  //??? Dynamic Facts
  end;

{------------------------------------------------------------------------------}

constructor TSingleLogicProgram.Create;
  begin
  inherited Create;
  FStructure := TStaticDefinitions.Create;
  FClauses := TStringList.Create;
  FInputLinksCount := 0;
  FOutputLinksCount := 0;
  //??? Dynamic Facts
  end;

{------------------------------------------------------------------------------}

procedure TSingleLogicProgram.DeleteClause(i: integer);
  begin
  if (i >= 0) and (i < FClauses.Count) then
     begin
     FClauses.Objects[i].Free;
     FClauses.Delete(i);
     end;
  end;

{------------------------------------------------------------------------------}

destructor TSingleLogicProgram.Destroy;
  var i : integer;
  begin
  for i := 0 to FClauses.Count - 1 do
    FClauses.Objects[i].Free;
  FClauses.Free;
  end;

{------------------------------------------------------------------------------}

procedure TSingleLogicProgram.DynamizeProgram(unified: boolean);
  var
    i : integer;
    s : string;
    cl : TSingleClause;

  begin
  //Define the Dynamic Fact for each (linked) input
  FDynamicFacts := TStringList.Create;
  SetLength(FInputLinksDynFact, FInputLinksCount);

  for i := 0 to FInputLinksCount - 1 do
    begin
    if unified then
       FInputLinksDynFact[i] := GetDynFact(i)
    else
       begin
       s := FStructure.Atoms[FInputLinksAtoms[i] - 1] + '#';
       s := s + IntToStr(FInputLinksRef[i]);
       FDynamicFacts.Add(s);
       FInputLinksDynFact[i] := i;
       end;
    end;

  //Define the atom of each Dynamic Fact
  SetLength(FDynFactAtoms, FDynamicFacts.Count);
  for i := 0 to FInputLinksCount - 1 do
    begin
    FDynFactAtoms[FInputLinksDynFact[i]] := FInputLinksAtoms[i];
    end;

  //Generate the Dynamic Clauses
  for i := 0 to FDynamicFacts.Count - 1 do
    begin
    cl := TSingleClause.Create(FStructure, FDynamicFacts);
    cl.Head := FDynFactAtoms[i];
    cl.AddBodyLiteral(FStructure.AtomCount + i + 1);
    AddClause(cl);
    end;
  end;

{------------------------------------------------------------------------------}

destructor TSingleLogicProgram.freeStructure;
  var i : integer;
  begin
  for i := 0 to FClauses.Count - 1 do
    FClauses.Objects[i].Free;
  FClauses.Free;
  FStructure.Free;
  inherited Destroy;

  end;

{------------------------------------------------------------------------------}

function TSingleLogicProgram.GetAtomCount : integer;
  begin
  result := FStructure.GetAtomsCount;
  end;

{------------------------------------------------------------------------------}


function TSingleLogicProgram.GetClauseCopy(index: integer): TSingleClause;
  begin
  if (index >= 0) and (index < FClauses.Count) then
     result := (FClauses.Objects[index] as TSingleClause)
  else
     result := nil;

  end;

{------------------------------------------------------------------------------}

function TSingleLogicProgram.GetCount: integer;
  begin
  result := FClauses.Count;
  end;

{------------------------------------------------------------------------------}

function TSingleLogicProgram.GetDynFact(index: integer): integer;
  var
    i : integer;
    b : boolean;
    s : string;
  begin
  s := FStructure.Atoms[FInputLinksAtoms[index] - 1];
  if FInputLinksTransf[index] in [OtBoxI, OtDiamondI, OtNtI, OtKI, OtPtI] then
     s := S + '*'
  else if FInputLinksTransf[index] = OtExt then
     s := S + ''''
  else
     s := S + '#';

  i := 0;
  b := false;
  while (i < FDynamicFacts.Count) and not b do
    begin
    b := FDynamicFacts.Strings[i] = s;
    i := i + 1;
    end;

  if b then
     result := i - 1
  else
     begin
     FDynamicFacts.Add(s);
     result := FDynamicFacts.Count - 1;
     end;
  end;

{------------------------------------------------------------------------------}

function TSingleLogicProgram.GetFirstKarnPos(varCount, total : integer) : integer;
  var
    i, XOut : integer;
  begin
  if varCount > 0 then
     begin
     XOut := 1;
     for i := 1 to varCount - 1 do
       Xout := (XOut * 3) + 1;
     result := XOut;
     end
  else
     result := 0;
  end;

{------------------------------------------------------------------------------}

function TSingleLogicProgram.GetInputLinkCount : integer;
  begin
  result := FInputLinksCount;
  end;

{------------------------------------------------------------------------------}

function TSingleLogicProgram.GetLeaves(var arLeaves : array of integer; initial, position,
              total : integer) : integer;
  var
    Xarr : array of integer;
    i, j, k, XCount, XTmpPosition : integer;
    XFound : boolean;
  begin
  SetLength(XArr, Total);
  j := position;
  for i := 0 to total - 1 do
    begin
    if (j / intPower(3, (total - 1) - i)) >= 2 then
       begin
       XArr[i] := 1;
       j := j - (2 * round(intPower(3, (total - 1) - i)));
       end
    else if (j / intPower(3, (total - 1) - i)) >= 1 then
       begin
       XArr[i] := -1;
       j := j - round(intPower(3, (total - 1) - i));
       end
    else
       XArr[i] := 0;
    end;
  XCount := initial;
  XFound := false;
  i := 0;
  while (i < total) and not XFound do
    begin
    if XArr[i] = 0 then
       begin
       XArr[i] := -1;
       XTmpPosition := 0;
       for k := 0 to total - 1 do
         begin
         if Xarr[k] = 0 then XTmpPosition := XTmpPosition * 3
         else if Xarr[k] = -1 then XTmpPosition := (XTmpPosition * 3) + 1
         else if Xarr[k] =  1 then XTmpPosition := (XTmpPosition * 3) + 2;
         end;
       XCount := GetLeaves(arLeaves, XCount, XTmpPosition, total);
       XArr[i] := 1;
       XTmpPosition := 0;
       for k := 0 to total - 1 do
         begin
         if Xarr[k] = 0 then XTmpPosition := XTmpPosition * 3
         else if Xarr[k] = -1 then XTmpPosition := (XTmpPosition * 3) + 1
         else if Xarr[k] =  1 then XTmpPosition := (XTmpPosition * 3) + 2;
         end;
       XCount := GetLeaves(arLeaves, XCount, XTmpPosition, total);
       XArr[i] := 0;
       XFound := true;
       end;
    i := i + 1;
    end;
  if not XFound then
     begin
     arLeaves[XCount] := position;
     XCount := XCount + 1;
     end;
  result := XCount;
  end;

{------------------------------------------------------------------------------}

function TSingleLogicProgram.GetNextKarnPos(currentPos, varCount, total : integer) : integer;
  var
    Xarr, Xpositions : array of integer;
    i, j, k, XCarry, XCarry2, XTmp : integer;
    XFinish : boolean;
  begin
  SetLength(XArr, Total);
  SetLength(XPositions, varCount);
  j := currentPos;
  k := 0;
  for i := 0 to total - 1 do
    begin
    if (j / intPower(3, (total - 1) - i)) >= 2 then
       begin
       XArr[i] := 1;
       j := j - (2 * round(intPower(3, (total - 1) - i)));
       XPositions[k] := i;
       k := k + 1;
       end
    else if (j / intPower(3, (total - 1) - i)) >= 1 then
       begin
       XArr[i] := -1;
       j := j - round(intPower(3, (total - 1) - i));
       XPositions[k] := i;
       k := k + 1;
       end
    else
       XArr[i] := 0;
    end;
  XCarry := 1;
  for k := varCount - 1 downto 0 do
    begin
    if (XCarry > 0) and (Xarr[Xpositions[k]] = -1) then
       begin
       XArr[XPositions[k]] := 1;
       XCarry := 0;
       end
    else if (XCarry > 0) and (Xarr[Xpositions[k]] = 1) then
       begin
       XArr[XPositions[k]] := -1;
       XCarry := 1;
       end;
    end;
  XFinish := false;
  if XCarry > 0 then
     begin
     for k := 0 to VarCount - 1 do
       Xarr[XPositions[k]] := 0;
     XFinish := true;
     for k := 0 to VarCount - 1 do
       XFinish := XFinish and (XPositions[k] = k);
     if not XFinish then
        begin
        XCarry2 := 1;
        for k := 0 to VarCount - 1 do
          if XCarry2 > 0 then
             begin
             if k < VarCount - 1 then
                Xtmp := XPositions[(VarCount - 1) - (k + 1)] + 1
             else
                Xtmp := 0;
             if XPositions[(VarCount - 1) - k] > Xtmp then
                begin
                XPositions[(VarCount - 1) - k] := XPositions[(VarCount - 1) - k] - 1;
                XCarry2 := 0;
                end
             else
                begin
                XPositions[(VarCount - 1) - k] := (total - 1) - k;
                XCarry2 := 1;
                end;
             end;
        for k := 0 to VarCount - 1 do
          Xarr[XPositions[k]] := -1;
        end;
     end;
  if XFinish then
     result := -1
  else
     begin
     j := 0;
     for i := 0 to total - 1 do
       begin
       if Xarr[i] = 0 then j := j * 3
       else if Xarr[i] = -1 then j := (j * 3) + 1
       else if Xarr[i] =  1 then j := (j * 3) + 2;
       end;
     result := j;
     end;
  end;

{------------------------------------------------------------------------------}

function TSingleLogicProgram.GetOutputLinkCount : integer;
  begin
  result := FOutputLinksCount;
  end;

{------------------------------------------------------------------------------}


function TSingleLogicProgram.GetRelatedArray(out arRelated : array of integer;
                             position, varCount, total : integer; moreVar : boolean) : integer;
  var
    Xarr : array of integer;
    i, j, k, XValue : integer;
  begin
  SetLength(XArr, Total);
  j := position;
  for i := 0 to total - 1 do
    begin
    if (j / intPower(3, (total - 1) - i)) >= 2 then
       begin
       XArr[i] := 1;
       j := j - (2 * round(intPower(3, (total - 1) - i)));
       end
    else if (j / intPower(3, (total - 1) - i)) >= 1 then
       begin
       XArr[i] := -1;
       j := j - round(intPower(3, (total - 1) - i));
       end
    else
       XArr[i] := 0;
    end;
  if moreVar then
     begin
     j := 0;
     for i := 0 to total - 1 do
       if XArr[i] = 0 then
          begin
          XArr[i] := -1;
          arRelated[j] := 0;
          for k := 0 to total - 1 do
            begin
            if Xarr[k] = 0 then arRelated[j] := arRelated[j] * 3
            else if Xarr[k] = -1 then arRelated[j] := (arRelated[j] * 3) + 1
            else if Xarr[k] =  1 then arRelated[j] := (arRelated[j] * 3) + 2;
            end;
          XArr[i] := 1;
          arRelated[j + 1] := 0;
          for k := 0 to total - 1 do
            begin
            if Xarr[k] = 0 then arRelated[j + 1 ] := arRelated[j + 1] * 3
            else if Xarr[k] = -1 then arRelated[j + 1] := (arRelated[j + 1] * 3) + 1
            else if Xarr[k] =  1 then arRelated[j + 1] := (arRelated[j + 1] * 3) + 2;
            end;
          XArr[i] := 0;
          j := j + 2;
          end;
     result := j;
     end
  else
     begin
     j := 0;
     for i := 0 to total - 1 do
       if XArr[i] <> 0 then
          begin
          XValue := XArr[i];
          XArr[i] := 0;
          arRelated[j] := 0;
          for k := 0 to total - 1 do
            begin
            if Xarr[k] = 0 then arRelated[j] := arRelated[j] * 3
            else if Xarr[k] = -1 then arRelated[j] := (arRelated[j] * 3) + 1
            else if Xarr[k] =  1 then arRelated[j] := (arRelated[j] * 3) + 2;
            end;
          XArr[i] := XValue;
          j := j + 1;
          end;
     result := j;
     end;
  end;

{------------------------------------------------------------------------------}

function TSingleLogicProgram.GetStructure : TStaticDefinitions;
  begin
  result := FStructure;
  end;

{------------------------------------------------------------------------------}

function TSingleLogicProgram.GetTransfText(OpTransf: TOpTransf): string;
  begin
  case OpTransf of
    OtError    : result := 'Error';
    OtBoxE     : result := '[]E';
    OtBoxI     : result := '[]I';
    OtDiamondE : result := '<>E';
    OtDiamondI : result := '<>I';
    OtKE       : result := 'K_E';
    OtKI       : result := 'K_I';
    OtNtE      : result := '(+)E';
    OtNtI      : result := '(+)I';
    OtPtE      : result := '(-)E';
    OtPtI      : result := '(-)I';
    OtExt      : result := 'External';
  else
    result := 'This crap doesn''t have any chances to work!!!'
    end;
  end;

{------------------------------------------------------------------------------}

function TSingleLogicProgram.isBody(atom: integer): boolean;
  var
    i : integer;
    b : boolean;
  begin
  b := false;
  i := 0;
  while (i < FClauses.Count) and (not b) do
    begin
    b := ((FClauses.Objects[i] as TSingleClause).IsOnBody(atom + 1) >= 0);
    i := i + 1;
    end;
  result := b;
  end;

{------------------------------------------------------------------------------}

function TSingleLogicProgram.IsContained(cl1, cl2: TSingleClause): boolean;
  var
    i : integer;
    XC1, XC2 : array of integer;
    b : boolean;
    Max : integer;
  begin
  Max := 0;
  for i := 0 to cl1.BodySize - 1 do
    if Abs(cl1.Body[i]) > Max then Max := round(Abs(Cl1.Body[i]));
  for i := 0 to cl2.BodySize - 1 do
    if Abs(cl2.Body[i]) > Max then Max := round(Abs(Cl2.Body[i]));
  SetLength(XC1, Max);
  SetLength(XC2, Max);
  for i := 0 to Max - 1 do
    begin
    XC1[i] := 0;
    XC2[i] := 0;
    end;
  for i := 0 to cl1.BodySize - 1 do
    XC1[round(abs(cl1.Body[i]) - 1)] := Sign(cl1.Body[i]);
  for i := 0 to cl2.BodySize - 1 do
    XC2[round(abs(cl2.Body[i]) - 1)] := Sign(cl2.Body[i]);
  b := true;
  i := 0;
  while b and (i < max) do
    begin
    if (XC1[i] <> XC2[i]) and (XC2[i] <> 0) then b := false;
    i := i + 1;
    end;
  result := b;
  end;

{------------------------------------------------------------------------------}

function TSingleLogicProgram.isHead(atom: integer): boolean;
  var
    i : integer;
    b : boolean;
  begin
  b := false;
  i := 0;
  while (i < FClauses.Count) and (not b) do
    begin
    b := (FClauses.Objects[i] as TSingleClause).Head = atom + 1;
    b := b and not (FClauses.Objects[i] as TSingleClause).IsDynamic;
    i := i + 1;
    end;
  result := b;
  end;

{------------------------------------------------------------------------------}

function TSingleLogicProgram.isRemovable(atom: integer): boolean;
  var
    b : boolean;
    i, cont : integer;
  begin
  i := 0;
  b := true;

  while (i < FOutputLinksCount) and b do
    begin
    b := not (atom = FOutputLinksAtoms[i]);
    i := i + 1;
    end;

  i := 0;
  cont := 0;
  while (i < FClauses.Count) and b do
    begin
    if (FClauses.Objects[i] as TSingleClause).head = atom then
       cont := cont + 1;
    b := (cont < 2);
    i := i + 1;
    end;

  i := 0;
  while (i < FClauses.Count) and b do
    begin
    if (FClauses.Objects[i] as TSingleClause).head = atom then
        b := (FClauses.Objects[i] as TSingleClause).BodySize = 1;
    i := i + 1;
    end;

  result := b;
  end;

{------------------------------------------------------------------------------}

function TSingleLogicProgram.isReplaceable(atom: integer): boolean;
  var
    b : boolean;
    i, cont : integer;
  begin
  b := true;

{  while (i < FOutputLinksCount) and b do
    begin
//    b := not (atom = FOutputLinksAtoms[i]);
    i := i + 1;
    end;}

  i := 0;
  cont := 0;
  while (i < FClauses.Count) and b do
    begin
    if (FClauses.Objects[i] as TSingleClause).head = atom then
       cont := cont + 1;
    b := (cont < 2);
    i := i + 1;
    end;

  i := 0;
  while (i < FClauses.Count) and b do
    begin
    if (FClauses.Objects[i] as TSingleClause).head = atom then
        b := (FClauses.Objects[i] as TSingleClause).BodySize = 1;
    i := i + 1;
    end;

  result := b;
  end;

{------------------------------------------------------------------------------}

function TSingleLogicProgram.JoinClauses(ClauseA, ClauseB: integer): integer;
  var
    arA, arB : array of integer;
    i, state, inverse : integer;
  begin
  setLength(ArA, FStructure.AtomCount);
  SetLength(ArB, FStructure.AtomCount);
  for i := 0 to FStructure.AtomCount - 1 do
    begin
    ArA[i] := 0;
    ArB[i] := 0;
    end;
  with FClauses.Objects[ClauseA] as TSingleClause do
    for i := 0 to BodySize - 1 do
      ArA[abs(Body[i]) - 1] := sign(Body[i]);
  with FClauses.Objects[ClauseB] as TSingleClause do
    for i := 0 to BodySize - 1 do
      ArB[abs(Body[i]) - 1] := sign(Body[i]);

  state := 0;
  //State -1: Halt (No operation to do)
  //State 0: A = B, State 1: A < B, State 2: A > B
  //State 3: A' = B', State 4: A' < B', State 5: A' > B'

  inverse := 0;

  i := 0;
  while (i < FStructure.AtomCount) and (state >= 0) do
    begin
    if ArA[i] <> ArB[i] then
       begin
       if (ArA[i] = 0) and (ArB[i] <> 0) then
          begin
          if (state mod 3) = 0 then state := state + 1;
          if (state mod 3) = 2 then state := -1;
          end
       else if (ArA[i] <> 0) and (ArB[i] = 0) then
          begin
          if (state mod 3) = 0 then state := state + 2;
          if (state mod 3) = 1 then state := -1;
          end
       else if (ArA[i] = - ArB[i]) then
          begin
          if state < 3 then
             begin
             state := state + 3;
             inverse := i + 1;
             end
          else
             state := -1;
          end;
       end;
    i := i + 1
    end;

  case state of
    0 :
      begin
      DeleteClause(ClauseB);
      end;
    1 :
      begin
      DeleteClause(ClauseB);
      end;
    2 :
      begin
      DeleteClause(ClauseA);
      end;
    3 :
      begin
      (FClauses.Objects[ClauseA] as TSingleClause).DeleteByAtom(inverse);
      DeleteClause(ClauseB);
      end;
    4 :
      begin
      (FClauses.Objects[ClauseB] as TSingleClause).DeleteByAtom(inverse);
      end;
    5 :
      begin
      (FClauses.Objects[ClauseA] as TSingleClause).DeleteByAtom(inverse);
      end;
    end;
  result := state;
  end;

{------------------------------------------------------------------------------}

procedure TSingleLogicProgram.KarnaughBased;
  var
//    XPr1, XPr2 : TSingleLogicProgram;
    XCl1 : TSingleClause;
    XMaxSize : integer;
    i, j, k, cnt1 : integer;
    b : boolean;
    XUnnecessary : array of boolean;

  begin
  XMaxSize := 0;
  for i := 0 to FClauses.Count - 1 do
    begin
    j := (FClauses.Objects[i] as TSingleClause).BodySize;
    if j > XMaxSize then XMaxSize := j;
    end;
  for k := XMaxSize downto 1 do
    begin
    FSimplificationEvent('Filename', k, 0, 0);
    cnt1 := FClauses.Count;
    SetLength(XUnnecessary, cnt1);
    for i := 0 to cnt1 - 1 do
      XUnnecessary[i] := false;
    for i := 0 to cnt1 - 1 do
      begin
      if (FClauses.Objects[i] as TSingleClause).BodySize = k then
         for j := i + 1 to cnt1 - 1 do
            if (FClauses.Objects[j] as TSingleClause).BodySize = k then
               begin
               b := (FClauses.Objects[i] as TSingleClause).Head = (FClauses.Objects[j] as TSingleClause).Head;
//               b := b and not XUnnecessary[i] and not XUnnecessary[j];
               b := b and LittleReduction(FClauses.Objects[i] as TSingleClause,
                                          FClauses.Objects[j] as TSingleClause, XCl1);
               if b then
                  begin
                  XUnnecessary[i] := true;
                  XUnnecessary[j] := true;
                  AddClause(XCl1);
                  FSimplificationEvent('Filename', k, i/cnt1, 0);
                  Application.ProcessMessages;
                  end;
               end;
      FSimplificationEvent('Filename', k, i/cnt1, 0);
      Application.ProcessMessages;
      end;


    for i := cnt1 - 1 downto 0 do
      if XUnnecessary[i] then
        DeleteClause(i);

    cnt1 := FClauses.Count;
    SetLength(XUnnecessary, cnt1);
    for i := 0 to cnt1 - 1 do
      XUnnecessary[i] := false;
    for i := 0 to cnt1 - 1 do
      begin
      for j := i + 1 to cnt1 - 1 do
        begin
        if IsContained(FClauses.Objects[i] as TSingleClause, FClauses.Objects[j] as TSingleClause) then
           XUnnecessary[i] := true
        else if IsContained(FClauses.Objects[j] as TSingleClause, FClauses.Objects[i] as TSingleClause) then
           XUnnecessary[j] := true
        end;
      FSimplificationEvent('Filename', k, 1, i/cnt1);
      Application.ProcessMessages;
      end;
    for i := cnt1 - 1 downto 0 do
      if XUnnecessary[i] then DeleteClause(i);
    end;

  end;

{------------------------------------------------------------------------------}

procedure TSingleLogicProgram.KarnaughBased2(OutputVar, InputVar : array of integer);
  var
    i, j, k : integer;
    XTmpCountValue : integer;
    XTmpWeightValue : double;
    XClause : TSingleClause;
    XTmpArBody : array of integer;

  begin
  FKarnaughMapsCount := length(OutputVar) div 2;
  FKarnaughMapSize := round(intPower(2, length(InputVar)));
  SetLength(FKarnCountMaps, FKarnaughMapsCount, FKarnaughMapSize);
  SetLength(FKarnWeightMaps, FKarnaughMapsCount, FKarnaughMapSize);
  SetLength(XTmpArBody, length(InputVar));
  for i := 0 to FKarnaughMapsCount - 1 do
    for j := 0 to FKarnaughMapSize - 1 do
      begin
      FKarnCountMaps[i, j] := 0;
      FKarnWeightMaps[i, j] := 0;
      FSimplificationEvent('Filename', 0, ((i * FKarnaughMapSize) + j) / (FKarnaughMapsCount * FKarnaughMapSize), 0);
      Application.ProcessMessages;
      end;
  for i := 0 to FKarnaughMapsCount - 1 do
    begin
    for j := 0 to FClauses.Count - 1 do
      begin
      XClause := FClauses.Objects[j] as TSingleClause;
      XTmpCountValue := 0;
      XTmpWeightValue := 0;
      if OutputVar[i * 2] = XClause.Head - 1 then
         begin
         XTmpCountValue := XClause.Count;
         XTmpWeightValue := XClause.Weight;
         end
      else if OutputVar[(i * 2) + 1] = XClause.Head - 1 then
         begin
         XTmpCountValue := - XClause.Count;
         XTmpWeightValue := - XClause.Weight;
         end;
      if XTmpCountValue <> 0 then
         begin
         for k := 0 to Length(InputVar) - 1 do
           begin
           XTmpArBody[k] := 0;
           if XClause.IsOnBody(InputVar[k] + 1) >= 0 then
              XTmpArBody[k] := sign(XClause.Body[XClause.IsOnBody(InputVar[k] + 1)]);
           end;
         MarkArray(i, XTmpArBody, 0, 0, XTmpCountValue, XTmpWeightValue);
         end;
      FSimplificationEvent('Filename', 0, 1, ((i * FClauses.Count) + j) / (FKarnaughMapsCount * FClauses.count));
      Application.ProcessMessages;
      end;
    end;



  ClearClauses;
  SetLength(FMegaCountArray, round(intPower(3, length(InputVar))));
  SetLength(FMegaWeightArray, round(intPower(3, length(InputVar))));

  for i := 0 to FKarnaughMapsCount - 1 do
    begin
    KarnNewClauses(i, true, OutputVar[i * 2], InputVar);
    KarnNewClauses(i, false, OutputVar[(i * 2) + 1], InputVar);
    FSimplificationEvent('Filename', 0, 1, i /FKarnaughMapsCount);
    Application.ProcessMessages;
    end;
  SetLength(FMegaCountArray, 0);
  SetLength(FMegaWeightArray, 0);
  end;

{------------------------------------------------------------------------------}

function TSingleLogicProgram.KarnNewClauses(CurrentMap : integer; positive : boolean;
                  variable : integer; InputVar : array of Integer) : integer;
  var
    i, j, k, n, m, XInCount : integer;
    XTmpArray : array of integer;
    XTmpClause : TSingleClause;

{    XDebugArray : array of array of integer;
    XDebugMax : array of integer;
    XGeneralMax : integer;
    XDebugStrList : TstringList;
    s : string;}

  begin
  XInCount := Length(InputVar);
  for i := 0 to round(intPower(3, XInCount)) - 1 do
    begin
    FMegaCountArray[i] := 0;
    FMegaWeightArray[i] := 0;    
    end;
  i := GetFirstKarnPos(XInCount, XInCount);
  for j := 0 to FKarnaughMapSize - 1 do
    begin
    if positive then
       begin
       FMegaCountArray[i] := FKarnCountMaps[CurrentMap, j];
       FMegaWeightArray[i] := FKarnWeightMaps[CurrentMap, j];
       end
    else
       begin
       FMegaCountArray[i] := - FKarnCountMaps[CurrentMap, j];
       FMegaWeightArray[i] := - FKarnWeightMaps[CurrentMap, j];
       end;
    i := GetNextKarnPos(i, XInCount, XInCount);
    end;

  for n := XInCount downto 2 do
    begin
    i := GetFirstKarnPos(n, XInCount);
    while i >= 0 do
      begin
      if FMegaCountArray[i] < 0 then
         begin
         setLength(XtmpArray, n);
         m := GetRelatedArray(XTmpArray, i, n, XInCount, false);
         for j := 0 to m - 1 do
           FMegaCountArray[XTmpArray[j]] := -1;
         end
      else if FMegaCountArray[i] > 0 then
         begin
         setLength(XtmpArray, n);
         m := GetRelatedArray(XTmpArray, i, n, XInCount, false);
         for j := 0 to m - 1 do
           if FMegaCountArray[XTmpArray[j]] <> -1 then
              begin
              FMegaCountArray[XTmpArray[j]] := FMegaCountArray[XTmpArray[j]] + FMegaCountArray[i];
              FMegaWeightArray[XTmpArray[j]] := FMegaWeightArray[XTmpArray[j]] + FMegaWeightArray[i];
              end;
         end;
      i := GetNextKarnPos(i, n, XInCount);
      end;
    end;

  for n := XInCount -1 downto 1 do
    begin
    i := GetFirstKarnPos(n, XInCount);
    while i >= 0 do
      begin
      if FMegaCountArray[i] > 0 then
         begin
         setLength(XTmpArray, (XInCOunt - n) * 2);
         m := GetRelatedArray(XTmpArray, i, n, XInCount, true);
         for j := 0 to m - 1 do
           FMegaCountArray[XTmpArray[j]] := 0;
         end;
      i := GetNextKarnPos(i, n, XInCount);
      end;
    end;

{  SetLength(XTmpArray, round(intPower(2, XInCount)));
  for i := 0 to round(intPower(3, XInCount) - 1) do
    if XMegaArray[i] > 0 then
       begin
       m := GetLeaves(XTmpArray, 0, i, XInCount);
       for j := 0 to m - 1 do
         if XTmpArray[j] <> i then XMegaArray[XTmpArray[j]] := XMegaArray[XTmpArray[j]] + 1;
       end;
  for i := 0 to round(intPower(3, XInCount) - 1) do
    if XMegaArray[i] > 0 then
       begin
       m := GetLeaves(XTmpArray, 0, i, XInCount);
       if m > 0 then
          if XTmpArray[0] <> i then
             begin
             k := XMegaArray[XTmpArray[0]];
             for j := 1 to m - 1 do
               if XMegaArray[XTmpArray[j]] < k then k := XMegaArray[XTmpArray[j]] - 1;
             if k > 1 then
               begin
               for j := 0 to m - 1 do XMegaArray[XTmpArray[j]] := XMegaArray[XTmpArray[j]] - 1;
               XMegaArray[i] := 0;
               end;
             end;
       end;
  for i := 0 to round(intPower(3, XInCount) - 1) do
    if XMegaArray[i] > 0 then
       begin
       m := GetLeaves(XTmpArray, 0, i, XInCount);
       for j := 0 to m - 1 do
         if XTmpArray[j] <> i then XMegaArray[XTmpArray[j]] := 0;
       end;}

{  //************ Only here for debugging reasons ***********
  XDebugStrList := TStringList.Create;
  SetLength(XDebugArray, XInCount, round(intPower(3, XInCount)));
  SetLength(XDebugMax, XInCount);
  for n := 1 to XInCount do
    begin
    i := GetFirstKarnPos(n, XInCount);
    j := 0;
    while i >= 0 do
      begin
      XDebugArray[n - 1, j] := XMegaArray[i];
      i := GetNextKarnPos(i, n, XInCount);
      j := j + 1;
      end;
    XDebugMax[n - 1] := j;
    end;

  XGeneralMax := 0;
  for i := 0 to XInCount - 1 do
    if XDebugMax[i] > XGeneralMax then XGeneralMax := XDebugMax[i];

  for j := 0 to XGeneralMax - 1 do
    begin
    s := '';
    for i := 0 to XInCount - 1 do
      begin
      if j >= XDebugMax[i] then
         s := s + 'X '
      else
         s := s + inttostr(XDebugArray[i, j]) + ' ';
      end;
    XDebugStrList.Add(s);
    end;

  XDebugStrList.saveToFile(FDebugFile + inttostr(variable));
  XDebugStrList.free;
  //********************************************************}


{  for n := 1 to XInCount - 1 do
    begin
    j := GetFirstKarnPos(n, XInCount);
    while j >= 0 do
      begin
      if XMegaArray[j] > 0 then
         begin
         setLength(XTmpArray, 2 * (XInCount - n));
         m := GetRelatedArray(XTmpArray, j, n, XInCount, true);
         for i := 0 to m - 1 do
           if XMegaArray[XTmpArray[i]] > 0 then
              XMegaArray[XTmpArray[i]] := 0;
         end;
      j := GetNextKarnPos(j, n, XInCount);
      end;
    end;}

  for i := 0 to round(intPower(3, XInCount) - 1) do
    if FMegaCountArray[i] > 0 then
       begin
       XTmpClause := TSingleClause.Create(FStructure, nil);
       XtmpClause.Head := variable + 1;
       k := i;
       for j := 0 to XInCount - 1 do
         begin
         if (k / intpower(3, (XInCount - 1) - j)) >= 2 then
            begin
            XTmpClause.AddBodyLiteral(InputVar[j] + 1);
            k := k - (round(intpower(3, (XInCount - 1) - j)) * 2);
            end
         else if (k / intpower(3, (XInCount - 1) - j)) >= 1 then
            begin
            XTmpClause.AddBodyLiteral(-(InputVar[j] + 1));
            k := k - round(intpower(3, (XInCount - 1) - j));
            end
         end;
       XTmpClause.Count := FMegaCountArray[i];
       XTmpClause.Weight := FMegaWeightArray[i];
       AddClause(XTmpClause);
       end;
  //while n > 1 do
    //begin
    //for each position p_i(n) with n variables do
      //if negative then Mark All related position p_j(n - 1) with n - 1 variables as negative
    //for each not negative position p_j(n - 1) with n - 1 variables do
      //for each related position p_i(n) with n variables do
        //if p_i > 0 then begin p_j := 1; p_i := 0;
    //end
  //for each positive position, create associated clause

  result := 0;
  end;

{------------------------------------------------------------------------------}

function TSingleLogicProgram.LittleReduction(cl1, cl2: TSingleClause;
               out ClOut: TSingleClause): boolean;
  var
    i, j : integer;
    XC1, XC2, XCout : array of integer;
    b : boolean;
    Max : integer;
  begin
  Max := 0;
  for i := 0 to cl1.BodySize - 1 do
    if Abs(cl1.Body[i]) > Max then Max := round(Abs(Cl1.Body[i]));
  for i := 0 to cl2.BodySize - 1 do
    if Abs(cl2.Body[i]) > Max then Max := round(Abs(Cl2.Body[i]));
  SetLength(XC1, Max);
  SetLength(XC2, Max);
  SetLength(XCOut, Max);
  for i := 0 to Max - 1 do
    begin
    XC1[i] := 0;
    XC2[i] := 0;
    XCOut[i] := 0;
    end;
  for i := 0 to cl1.BodySize - 1 do
    XC1[round(abs(cl1.Body[i]) - 1)] := Sign(cl1.Body[i]);
  for i := 0 to cl2.BodySize - 1 do
    XC2[round(abs(cl2.Body[i]) - 1)] := Sign(cl2.Body[i]);
  j := -1;
  b := true;
  i := 0;
  while b and (i < max) do
    begin
    if (Xc1[i] * Xc2[i]) = -1 then
       if j < 0 then
          j := i
       else
          b := false
    else
       if Xc1[i] = Xc2[i] then
          XcOut[i] := Xc1[i]
       else
          b := false;
    i := i + 1;
    end;

  if b then
     begin
     ClOut := TSingleClause.Create(FStructure, nil);
     ClOut.Head := cl1.Head;
     for i := 0 to Max - 1 do
       if XcOut[i] <> 0 then
          ClOut.AddBodyLiteral(XcOut[i] * (i + 1));
     end
  else
     ClOut := nil;
  result := b;
  end;

{------------------------------------------------------------------------------}

procedure TSingleLogicProgram.LoadStringList(strList: TStringList;
  b: boolean);
  var
    i : integer;
    s : string;
  begin
  for i := 0 to FInputLinksCount - 1 do
    begin
    s := 'Input: ';
    s := s + intToStr(FInputLinksRef[i]) + ' - ';
    s := s + FStructure.Atoms[FInputLinksAtoms[i] - 1] + ' - ';
    s := s + GetTransfText(FInputLinksTransf[i]) + ' - ';
    if b then
       begin
       s := s + FDynamicFacts.Strings[FInputLinksDynFact[i]] + ' - ';
       s := s + FStructure.Atoms[FDynFactAtoms[FInputLinksDynFact[i]] - 1];
       end;
    StrList.Add(s);
    end;
  StrList.Add('');
  for i := 0 to FOutputLinksCount - 1 do
    begin
    s := 'Output: ';
    s := s + intToStr(FOutputLinksRef[i]) + ' - ';
    s := s + FStructure.Atoms[FOutputLinksAtoms[i] - 1] + ' - ';
    s := s + GetTransfText(FOutputLinksTransf[i]);
    StrList.Add(s);
    end;
  StrList.Add('');
  for i := 0 to FClauses.Count - 1 do
    StrList.Add(FClauses.Strings[i]);
  end;

{------------------------------------------------------------------------------}

procedure TSingleLogicProgram.MarkArray(currentMap : integer; inputs : array of integer;
          index, position, clCount : integer; clWeight : double);
  begin
  if index >= length(inputs) then
     begin
     FKarnCountMaps[currentMap, position] := FKarnCountMaps[currentMap, position] + clCount;
     FKarnWeightMaps[currentMap, position] := FKarnWeightMaps[currentMap, position] + clWeight;
     end
  else
     begin
     if inputs[index] >= 0 then
        MarkArray(CurrentMap, inputs, index + 1, (position * 2) + 1, clCount, clWeight);
     if inputs[index] <= 0 then
        MarkArray(CurrentMap, inputs, index + 1, (position * 2), clCount, clWeight);
     end;
  end;

{------------------------------------------------------------------------------}

procedure TSingleLogicProgram.NewSimplification;
  var
    i, j, k, atom : integer;
  begin
  for i := 1 to FStructure.AtomCount  do
    begin
    if IsReplaceable(i) then
       begin
       atom := i;
       for j := FClauses.Count - 1 downto 0 do
         if (FClauses.Objects[j] as TSingleClause).Head = i then
            atom := (FClauses.Objects[j] as TSingleClause).Body[0];

       if atom <> i then
          begin
          for j := 0 to FClauses.Count - 1 do
            for k := 0 to (FClauses.Objects[j] as TSingleClause).BodySize - 1 do
              if (FClauses.Objects[j] as TSingleClause).Body[k] = i then
                 (FClauses.Objects[j] as TSingleClause).Body[k] := atom
              else if (FClauses.Objects[j] as TSingleClause).Body[k] = -i then
                 (FClauses.Objects[j] as TSingleClause).Body[k] := - atom;
          end;
       end;
    end;
  UpdateClauses;
  end;
{------------------------------------------------------------------------------}

procedure TSingleLogicProgram.NullSimpEvent(fileName : string; size : integer; p1, p2 : double);
  begin

  end;

{------------------------------------------------------------------------------}

procedure TSingleLogicProgram.ReduceClauses;
  var i, j, k : integer;
  begin
  i := 0;
  while i < FClauses.Count do
    begin
    j := i + 1;
    while j < FClauses.Count do
      begin
      //First Version
      k := JoinClauses(i, j);
      case k of
        -1 : j := j + 1;
        //0  : j := j;
        //1 : j := j
        2 : j := i + 1;
        3 : begin i := 0; j := 1; end;
        4 : begin i := 0; j := 1; end;
        5 : begin i := 0; j := 1; end;
        else
           j := i + 1;
        end;
      end;
    i := i + 1;
    end;
  UpdateClauses;


  end;

{------------------------------------------------------------------------------}

procedure TSingleLogicProgram.SaveToCILPNetwork(rede: TNetworkRep;
  world: integer; Beta: double);
  var
    XLocalAtoms   : array of integer;
    XLocalCount   : integer;
    XLocalClauses : TStringList;
    XLocalBody, XLocalHead : array of boolean;
    XLocalBodyN, XLocalHeadN : array of integer;
    TmpCl         : TSingleClause;
    b, bbb : boolean;
    i, j, max_ku, l, tmp, Xinps : integer;
    k, u : array of integer;
    W, Amin : double;
    s : string;

  begin

  //??? It needs a verification....

  XLocalCount   := 0;
  XLocalClauses := TStringList.Create;
  for i := 0 to FClauses.Count - 1 do
    begin
    TmpCl := TSingleClause.Create(FStructure, FDynamicFacts);
    tmp := (FClauses.Objects[i] as TSingleClause).Head;
    b   := false;
    l   := 0;
    while (l < XLocalCount) and (not b) do
      begin
      b := XLocalAtoms[l] = abs(tmp);
      l := l + 1;
      end;
    if b then
       begin
       l := l - 1;
       XLocalHead[l] := true;
       end
    else
       begin
       XLocalCount := XLocalCount + 1;
       SetLength(XLocalAtoms, XLocalCount);
       SetLength(XLocalHead, XLocalCount);
       SetLength(XLocalBody, XLocalCount);
       l := XLocalCount - 1;
       XLocalAtoms[l] := abs(tmp);
       XLocalHead[l] := true;
       end;
    tmpCl.Head := l;

    for j := 0 to (FClauses.Objects[i] as TSingleClause).BodySize - 1 do
      begin
      tmp := (FClauses.Objects[i] as TSingleClause).Body[j];
      b   := false;
      l   := 0;
      while (l < XLocalCount) and (not b) do
        begin
        b := XLocalAtoms[l] = abs(tmp);
        l := l + 1;
        end;
      if b then
         begin
         l := l - 1;
         XLocalBody[l] := true;
         end
      else
         begin
         XLocalCount := XLocalCount + 1;
         SetLength(XLocalAtoms, XLocalCount);
         SetLength(XLocalHead, XLocalCount);
         SetLength(XLocalBody, XLocalCount);
         l := XLocalCount - 1;
         XLocalAtoms[l] := abs(tmp);
         XLocalBody[l] := true;
         end;
      if tmp > 0 then
         tmpCl.AddBodyLiteral(l)
      else
         tmpCl.AddBodyLiteral(-l);
      end;
    XLocalClauses.AddObject('', tmpCl);
    end;

  SetLength(XLocalHeadN, XLocalCount);
  SetLength(XLocalBodyN, XLocalCount);

  SetLength(k, XLocalClauses.Count);
  SetLength(u, XLocalCount);

  for j := 0 to XLocalClauses.Count - 1 do
    k[j] := (XLocalClauses.Objects[j] as TSingleClause).BodySize;
  for i := 0 to XLocalCount - 1 do
    u[i] := 0;
  for i := 0 to FStructure.AtomCount - 1 do
    for j := 0 to XLocalClauses.Count - 1 do
      if ((XLocalClauses.Objects[j] as TSingleClause).Head) = i then
         u[i] := u[i] + 1;
  max_ku := 0;
  for j := 0 to XLocalClauses.Count - 1 do
    if k[j] >= max_ku then
       max_ku := k[j];
  for i := 0 to XLocalCount - 1 do
    if u[i] >= max_ku then
       max_ku := u[i];
  Amin := max_ku / (max_ku + 1);    //Definir Amin > (max_ku - 1) / (max_ku + 1)
  W := (ln(1 + Amin) - ln(1 - Amin));
  W := (2/Beta) * W / ((max_ku * (Amin - 1)) + (Amin + 1) );

  for i := 0 to XLocalCount - 1 do
    begin
    tmp := 0;
    bbb := false;
    if XLocalBody[i] then
       begin
       if XLocalAtoms[i] > FStructure.AtomCount then
          begin
          s := FDynamicFacts[XLocalAtoms[i] - (FStructure.AtomCount + 1)];
          rede.AddNode('(' + inttostr(world) + ')i:' + s, UKNeuron);
          rede.Node[rede.NoNodes - 1].World := world;
          XLocalBodyN[i] := rede.NoNodes - 1;
          XInps := 0;
          for j := 0 to FInputLinksCount - 1 do
            if FInputLinksDynFact[j] = XLocalAtoms[i] - (FStructure.AtomCount + 1) then
               begin
               Rede.AddArc(FInputLinksRef[j], rede.NoNodes - 1);
               rede.Arc[rede.NoArcs - 1].Weight := W;
               rede.Arc[rede.NoArcs - 1].RandomRange := 0;
               rede.Arc[rede.NoArcs - 1].isFixed  := true;
               XInps := XInps + 1;
               end;

          (rede.Node[XLocalBodyN[i]] as TNeuronRep).AtivFunction := 2;
          (rede.Node[XLocalBodyN[i]] as TNeuronRep).AddFunctionParam(Beta);
          //If it is conjunctive, I have to change;
          (rede.Node[XLocalBodyN[i]] as TNeuronRep).BiasWeight := -((1 + Amin) * (1 - XInps) * (W / 2));
          (rede.Node[XLocalBodyN[i]] as TNeuronRep).RandomBias := 0;
          (rede.Node[XLocalBodyN[i]] as TNeuronRep).HasBias    := true;
          (rede.Node[XLocalBodyN[i]] as TNeuronRep).FixedBias  := true;

          bbb := false;
          end
       else
          begin
          s := FStructure.Atoms[XLocalAtoms[i] - 1];
          rede.AddNode('(' + inttostr(world) + ')i:' + s, UKNeuron);
          rede.Node[rede.NoNodes - 1].World := world;
          XLocalBodyN[i] := rede.NoNodes - 1;
          (rede.Node[XLocalBodyN[i]] as TNeuronRep).AtivFunction := 2;
          (rede.Node[XLocalBodyN[i]] as TNeuronRep).AddFunctionParam(Beta);
          (rede.Node[XLocalBodyN[i]] as TNeuronRep).BiasWeight := 0;// -((1 + Amin) * (1 - u[i]) * (W / 2));
          (rede.Node[XLocalBodyN[i]] as TNeuronRep).RandomBias := 0;
          (rede.Node[XLocalBodyN[i]] as TNeuronRep).HasBias    := true;
          bbb := true;
          rede.AddNode('r:' + s, UkRec);
          tmp := rede.NoNodes - 1;
          (rede.Node[tmp] as TRecLinkRep).Level := 0;
          rede.AddArc(tmp, XLocalBodyN[i]);
          rede.Arc[rede.NoArcs - 1].Weight := W;
          rede.Arc[rede.NoArcs - 1].RandomRange := 0;
          rede.Arc[rede.NoArcs - 1].isFixed  := true;
          end;
       end;

    if XLocalHead[i] then
       begin
       s := FStructure.Atoms[XLocalAtoms[i] - 1];
       rede.AddNode('(' + inttostr(world) + ')o:' + s, UKNeuron);
       rede.Node[rede.NoNodes - 1].World := world;
       XLocalHeadN[i] := rede.NoNodes - 1;
       for j := 0 to FOutputLinksCount - 1 do
         if FOutputLinksAtoms[j] = XLocalAtoms[i] then
            begin
            rede.AddArc(Rede.NoNodes - 1, FOutputLinksRef[j]);
            rede.Arc[rede.NoArcs - 1].Weight := 1;
            rede.Arc[rede.NoArcs - 1].RandomRange := 0;
            rede.Arc[rede.NoArcs - 1].isFixed  := true;
            end;
       (rede.Node[XLocalHeadN[i]] as TNeuronRep).AtivFunction := 2;
       (rede.Node[XLocalHeadN[i]] as TNeuronRep).AddFunctionParam(Beta);
       (rede.Node[XLocalHeadN[i]] as TNeuronRep).BiasWeight := -((1 + Amin) * (1 - u[i]) * (W / 2));
       (rede.Node[XLocalHeadN[i]] as TNeuronRep).RandomBias := 0;
       (rede.Node[XLocalHeadN[i]] as TNeuronRep).HasBias    := true;
       if bbb then
          begin
          rede.AddArc(XLocalHeadN[i], tmp);
          rede.Arc[rede.NoArcs - 1].Weight := 1;
          rede.Arc[rede.NoArcs - 1].RandomRange := 0;
          rede.Arc[rede.NoArcs - 1].isFixed  := true;
          end;
       end
    end;

  for j := 0 to XLocalClauses.Count - 1 do
    begin
    rede.AddNode('(' + inttostr(world) + ')' + FClauses.Strings[j], UkNeuron);
    rede.Node[rede.NoNodes - 1].World := world;
    with XLocalClauses.Objects[j] as TSingleClause do
      begin
      for i := 0 to BodySize - 1 do
        begin
        rede.AddArc(XLocalBodyN[abs(FBody[i])], rede.NoNodes - 1);
        rede.Arc[Rede.NoArcs - 1].RandomRange := 0;
        rede.Arc[Rede.NoArcs - 1].isFixed  := IsDefault;
        if FBody[i] > 0 then
           rede.Arc[Rede.NoArcs - 1].Weight := W
        else
           rede.Arc[Rede.NoArcs - 1].Weight := -W;
        end;
      rede.AddArc(rede.NoNodes - 1, XLocalHeadN[abs(Head)]);
      rede.Arc[rede.NoArcs - 1].Weight := W;
      rede.Arc[rede.NoArcs - 1].RandomRange := 0;
      rede.Arc[rede.NoArcs - 1].isFixed  := IsDefault;
      (rede.Node[rede.NoNodes - 1] as TNeuronRep).AtivFunction := 2;
      (rede.Node[rede.NoNodes - 1] as TNeuronRep).AddFunctionParam(Beta);
      (rede.Node[rede.NoNodes - 1] as TNeuronRep).BiasWeight := -((1 + Amin) * (k[j] - 1) * (W / 2));
      (rede.Node[rede.NoNodes - 1] as TNeuronRep).HasBias := true;
      (rede.Node[rede.NoNodes - 1] as TNeuronRep).RandomBias := 0;
      end;
    end;

  for i := 0 to XLocalClauses.Count - 1 do
    XLocalClauses.Objects[i].Free;
  XLocalClauses.Free;
  end;

{------------------------------------------------------------------------------}

procedure TSingleLogicProgram.SaveToFile(FileName: string; PrintExtraInfo : boolean = false);
  begin
  UpdateClauses(PrintExtraInfo);
  FClauses.SaveToFile(FileName);
  end;

{------------------------------------------------------------------------------}

procedure TSingleLogicProgram.SaveToStringList(StringList: TStrings; PrintExtraInfo : boolean = false);
  var i : integer;
  begin
  UpdateClauses(PrintExtraInfo);
  StringList.clear;
  for i := 0 to FClauses.Count - 1 do
     StringList.Add(FClauses.Strings[i]);
  end;


{------------------------------------------------------------------------------}

procedure TSingleLogicProgram.SetDebugFile(fileName : string);
  begin
  FDebugFile := fileName;
  end;

{------------------------------------------------------------------------------}

procedure TSingleLogicProgram.SimplifyProgram;
  var
    i, j, k, atom : integer;

  begin
  for i := 1 to FStructure.AtomCount  do
    begin
    if IsRemovable(i) then
       begin
       atom := i;
       for j := FClauses.Count - 1 downto 0 do
         if (FClauses.Objects[j] as TSingleClause).Head = i then
            begin
            atom := (FClauses.Objects[j] as TSingleClause).Body[0];
            DeleteClause(j);
            end
         else
            (FClauses.Objects[j] as TSingleClause).SetDynamicFacts(FDynamicFacts); //???

       if atom <> i then
          begin
          for j := 0 to FClauses.Count - 1 do
            for k := 0 to (FClauses.Objects[j] as TSingleClause).BodySize - 1 do
              if (FClauses.Objects[j] as TSingleClause).Body[k] = i then
                 (FClauses.Objects[j] as TSingleClause).Body[k] := atom
              else if (FClauses.Objects[j] as TSingleClause).Body[k] = -i then
                 (FClauses.Objects[j] as TSingleClause).Body[k] := - atom;
          end;
       end;
    end;
  UpdateClauses;
  end;

{------------------------------------------------------------------------------}

procedure TSingleLogicProgram.SpecialConcat(tmpProg: TSingleLogicProgram;
  head: integer);
  var
    i : integer;
    cl : TSingleClause;
  begin
  for i := 0 to tmpprog.ClausesCount - 1 do
    begin
    cl := tmpprog.GetClauseCopy(i);
    if (head > -2) then
       cl.Head := Cl.Head + head;
    AddClause(cl);
    end;
  end;

{------------------------------------------------------------------------------}

procedure TSingleLogicProgram.TP(size: integer; inputInt: array of boolean;
  var outputInt: array of boolean);
  var i : integer;
  begin
  if size = FStructure.AtomCount then
     begin
     for i := 0 to size - 1 do
       OutputInt[i] := false;
     for i := 0 to FClauses.Count - 1 do
       (FClauses.Objects[i] as TSingleClause).TP(size, InputInt, outputInt);
     end;
  end;

{------------------------------------------------------------------------------}

procedure TSingleLogicProgram.UpdateClauses(PrintExtraInfo : boolean = false);
  var i : integer;
  begin
  for i := 0 to FClauses.Count - 1 do
    begin
    (FClauses.Objects[i] as TSingleClause).PrintExtraInfo := PrintExtraInfo;
    FClauses.Strings[i] := (FClauses.Objects[i] as TSingleClause).Text;
    end;
  end;

{------------------------------------------------------------------------------}

end.
