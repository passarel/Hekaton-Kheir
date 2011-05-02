{
  @abstract(Unit containing the routines to execute the parsing
    (syntactic analysis) of a logic program)
  @author(Rafa "Passarel" Borges <passarel@gmail.com)
  @created(Porto Alegre, March, 2006)
  @lastmod(Porto Alegre, June 25, 2006)
}
unit UnLogPars; //1588 lines

interface

uses Classes, UnLogScan, UnLogStruct;

{------------------------------------------------------------------------------}

{
@abstract(Class to encapsulate the parser for logic programs)

The class @name contains structures and routines to realize the syntactic analysis
of a logic program, receiving as input from the scanner a list of tokens and
returning a list of logic clauses and the structure of the program. This parsing
mechanism is based on an finite automata (implemented with case structures),
where each transition between states can also realizes a particular procedure.

Obs: In case of errors during the parsing process, the method Execute returns as
output a negative number which module is the token where the error ocurred.
The same value is returned by the private methods ProcessHeader, ProcessClauses
as its outputs an by the privat method ProcessLiteral on the variable parameter i.
}
type TLogicParser = class
  private
    //Name of the file containing the description of the scanner's automata
    FAutomataName : string;

    //Name of the file containing the description of the scanner
    FScannerName  : string;

    //Scanner used to the lexical analysis of the program
    FScanner      : TScanner;

    //Style of the logic program
    FStyle     : TStyle;

    //List of symbols on the program
    FSymbols   : TStringList;

    //Structure of the logic program
    FStructure : TStaticDefinitions;

    //List of clauses on the logic program
    FClauses   : TStringList;

    //Verify if a token is an atom or a modal operator
    //@param(i, j: Fields IndTab and RefInt of the token to be analysed)
    //@return(true if the token is an atom or a modal operator)
    function IsUn_Literal(i, j : integer) : boolean;

    //Verify if a token is an atom, a negation or a modal operator
    //@param
    //@param
    //@return(true if the token is an atom, a negation or a modal operator)
    function IsUn_Atom(i, j : integer) : boolean;

    //Process the header of the logic program
    //@param(i : initial token to be verified);
    //@return(First token after the end of the header);
    function ProcessHeader  (i : integer): integer;

    //Process the clauses of the logic program
    //@param(i: initial token of the main part of the program (list of clauses)
    //@return(Count of tokens processed on the entire program);
    function ProcessClauses (i : integer): integer;

    //Process an specific literal
    //@param(index of the current token at the beginning an the end of the process)
    //@return(the literal found on the list of tokens);
    function ProcessLiteral (var i, par : integer) : TGenLiteral;

    //Process an specific 'atom'
    //@param(index of the current token at the beginning an the end of the process)
    //@return(the literal found on the list of tokens);
    function ProcessUnaryAtom (var i, par : integer) : TGenLiteral;

    //Process an specific 'atom'
    //@param(index of the current token at the beginning an the end of the process)
    //@return(the literal found on the list of tokens);
    function ProcessBinaryAtom (var i, par : integer) : TGenLiteral;    

  public
    //Constructor of the class
    //@param(Name of the file containing the description of the scanner's automata)
    //@param(Name of the file containing the description of the scanner)
    constructor Create(automataName, scannerName : string);

    //Destructor of the class
    destructor  Destroy; override;

    //Main function of the class: Execute the syntactic analysis of a logic program
    //@param(filename: Name of the file with the source code of the logic program);
    //@param(Structure: Structure: structure of a logic program to be filled in the process)
    //@param(Clauses: List of classes to bi filled on the process)
    //@param(Style(output): style of the logic program);
    //@return(The count of tokens on the program);
    function Execute(filename : string; Structure : TStaticDefinitions;
                        Clauses : TStringList; out style : TStyle) : integer;
  end;

{------------------------------------------------------------------------------}

implementation

//uses

{------------------------------------------------------------------------------}

{ TLogicParser }

constructor TLogicParser.Create(automataName, scannerName: string);
  begin
  inherited Create;
  FAutomataName := AutomataName;
  FScannerName  := ScannerName;
  FScanner      := TScanner.Create;
  FScanner.LoadFromFile(ScannerName, AutomataName);
  end;

{------------------------------------------------------------------------------}

destructor TLogicParser.Destroy;
  begin
  FScanner.Free;
  FSymbols.Free;
  inherited;
  end;

{------------------------------------------------------------------------------}

function TLogicParser.Execute(filename: string;  Structure: TStaticDefinitions;
              Clauses: TStringList; out style: TStyle): integer;
  var
    i : integer;
  begin
  FScanner.Executa(filename);
  FSymbols := TStringList.Create;
  FScanner.RetornaSimbolos(FSymbols);
  FClauses   := Clauses;
  FStructure := Structure;
  FStructure.IdentifierCount := FSymbols.Count;
  i := 0;
  i := ProcessHeader(i);
  if i >= 0 then
     i := ProcessClauses(i);
  //if i < 0 then raise exception
  style := FStyle;
  FStructure := nil;
  FClauses   := nil;
  result := i;
  end;

{------------------------------------------------------------------------------}

function TLogicParser.IsUn_Atom(i, j: integer): boolean;
  begin
  result := ((i = 0) and FStructure.IsAtom(j))   or
            ((i = 2) and (j >= 6)  and (j < 11)) or
            ((i = 2) and (j >= 13) and (j < 20));
//            ((i = 2) and (j >= 16) and (j < 20)) or
//            ((;
  end;

{------------------------------------------------------------------------------}

function TLogicParser.IsUn_Literal(i, j: integer): boolean;
  begin
  result := IsUn_Atom(i, j) or ((i = 2) and ((j = 2) or (j = 3)));
  end;

{------------------------------------------------------------------------------}

function TLogicParser.ProcessBinaryAtom(var i, par: integer): TGenLiteral;
  var
    state, count, j : integer;
    stop : boolean;
    ArBinOp : array of TOperator;
    ArAtoms : array of TGenLiteral;
    tok : TToken;
    XLiteral1, XLiteral2 : TGenLiteral;

  begin
  stop := false;
  state := 0;
  count := 0;
  SetLength(ArAtoms, 1);
  while (not stop) and (i < FScanner.GetCount) and (par >= 0)do
    begin
    tok := FScanner.GetSaida(i);
    case state of
      0 :
        begin
        if IsUn_Atom(tok.RefInt, tok.IndTab) then
           begin
           ArAtoms[count] := ProcessUnaryAtom(i, par);
           state := 1;
           end
        else
           begin
           state := -1;
           stop := true;
           end;
        end;
      1 :
        begin
        //Treat binary operator
        if (tok.RefInt = 2) and (tok.IndTab >= 20) and (tok.IndTab < 28) then
           begin
           count := count + 1;
           setLength(arBinOp, count);
           setLength(arAtoms, count + 1);
           case tok.IndTab of
             20..21 : ArBinOp[count - 1] := OpSince;
             22..23 : ArBinOp[count - 1] := OpUntil;
             24..25 : ArBinOp[count - 1] := OpZince;
             26..27 : ArBinOp[count - 1] := OpUnless;
             end;
           i := i + 1;
           state := 0;
           end
        else if (tok.RefInt = 2) and (tok.IndTab = 15) then
           begin
           if (par > 0) then
              begin
              i := i + 1;
              par := par - 1;
              end;
           if (par = 0) then
              begin
              state := 2;
              stop := true
              end;
           end
        else
           begin
           state := 2;
           stop := true;
           end;
        end;
      end;
    end;
  if (par = 0) and ((state = 2) or (not stop)) then
     begin
     if count = 0 then
        XLiteral1 := ArAtoms[0]
     else
        begin
        XLiteral1 := TGenLiteral.Create;
        XLiteral1.Operator := ArBinOp[0];
        XLiteral1.Child1   := ArAtoms[0];
        XLiteral1.Child2   := ArAtoms[1];
        for j := 1 to count - 1 do
          begin
          XLiteral2 := XLiteral1;
          XLiteral1 := TGenLiteral.Create;
          XLiteral1.operator := ArBinOp[j];
          XLiteral1.Child1 := XLiteral2;
          XLiteral1.Child2 := ArAtoms[j + 1];
         end;
        end;
     result := XLiteral1;
     end
  else
    result := nil;
  end;

{------------------------------------------------------------------------------}

function TLogicParser.ProcessClauses(i: integer): integer; //??? Needs a revision
  var
    state, prlabel : integer;
    tok   : TToken;
    stop  : boolean;
    tmpPar : integer;
    tmpClause  : TGenClause;
    tmpLiteral : TGenLiteral;

  begin
  state := 0;
  stop  := false;
  TmpClause := TGenClause.Create(FStructure);
  while not stop do
    if i >= FScanner.GetCount then
       begin
       stop := true;
       if (state = 0) or (state = 3) or (state = 4) or (state = 5) then;
          begin
          state := -1;
          if TmpClause.Text <> '' then
             FClauses.AddObject(TmpClause.Text, TmpClause);
          end;
       end
    else
       begin
       tok := FScanner.GetSaida(i);
       case state of
         0 :
           begin
           //if token is label (or time)
           if (tok.RefInt = 0) and FStructure.IsLabel(tok.IndTab) then
              begin
              tmpClause.ClauseLabel := FStructure.GetIndex(tok.IndTab);
              i := i + 1;
              state := 1;
              end
           //if token is modality or atom (head)
           else if IsUn_Atom(tok.RefInt, tok.IndTab) then
              begin
              TmpLiteral := ProcessLiteral(i, tmpPar);
              if (TmpLiteral = nil) or (TmpLiteral.HasNegation) then
                 begin
                 stop := true;
                 if TmpLiteral <> nil then
                    TmpLiteral.Free;
                 TmpClause.Free;
                 end
              else
                 begin
                 TmpClause.Head := TmpLiteral;
                 state := 3;
                 end;
              end
           //if token is line break
           else if tok.RefInt = 3 then
              begin
              i := i + 1;
              end
           //else
           else
              begin
              TmpClause.Free;
              stop := true;
              end;
           end;
         1 :
           begin
           //if token is ':'
           if (tok.RefInt = 2) and (tok.IndTab = 11) then
              begin
              state := 2;
              i := i + 1;
              end
           //else
           else
              begin
              TmpClause.Free;
              stop := true;
              end;
           end;
         2 :
           begin
           //if token is modality or atom (head)
           if IsUn_Atom(tok.RefInt, tok.IndTab) then
              begin
              TmpLiteral := ProcessLiteral(i, tmpPar);
              if (TmpLiteral = nil) or (TmpLiteral.HasNegation) then
                 begin
                 stop := true;
                 if TmpLiteral <> nil then
                    TmpLiteral.Free;
                 TmpClause.Free;
                 end
              else
                 begin
                 TmpClause.Head := TmpLiteral;
                 state := 3;
                 end;
              end
           //else
           else
              begin
              TmpClause.Free;
              stop := true;
              end;
           end;
         3 :
           begin
           //if token is '<-' or ':-'
           if (tok.RefInt = 2) and ((tok.IndTab = 0) or (tok.IndTab = 1)) then
              begin
              state := 4;
              i := i + 1;
              TmpClause.IsDefault := false;
              end

           //if token is '::-'
           else if (tok.RefInt = 2) and (tok.IndTab = 12) then
              begin
              state := 4;
              i := i + 1;
              TmpClause.IsDefault := true;
              end

           //if token is line break
           else if tok.RefInt = 3 then
              begin
              i := i + 1;
              state := 0;
              FClauses.AddObject(TmpClause.Text, TmpClause);
              TmpClause := TGenClause.Create(FStructure);
              end
           //else
           else
              begin
              TmpClause.Free;
              stop := true;
              end;
           end;
         4 :
           begin
           //if token is modality, negation or atom
           if IsUn_Literal(tok.RefInt, tok.IndTab) then
              begin
              TmpLiteral := ProcessLiteral(i, tmpPar);
              if TmpLiteral = nil then
                 begin
                 stop := true;
                 TmpClause.Free;
                 end
              else
                 begin
                 TmpClause.AddBodyLiteral(TmpLiteral);
                 state := 5;
                 end;
              end
           //if token is line break
           else if tok.RefInt = 3 then
              begin
              i := i + 1;
              state := 0;
              FClauses.AddObject(TmpClause.Text, TmpClause);
              TmpClause := TGenClause.Create(FStructure);
              end
           else
              begin
              TmpClause.Free;
              stop := true;
              end;
           end;
         5 :
           begin
           //if token is ','
           if (tok.RefInt = 2) and (tok.IndTab = 4) then
              begin
              i := i + 1;
              state := 4;
              end
           //if token is ';'
           else if (tok.RefInt = 2) and (tok.IndTab = 5) then
              begin
              i := i + 1;
              state := 4;
              FClauses.AddObject(TmpClause.Text, TmpClause);
              TmpLiteral:= tmpClause.Head;
              PrLabel := tmpClause.ClauseLabel;
              TmpClause := TGenClause.Create(FStructure);
              tmpClause.Head := TmpLiteral;
              tmpClause.ClauseLabel := PrLabel;
              end
           //if token is line break
           else if tok.RefInt = 3 then
              begin
              i := i + 1;
              state := 0;
              FClauses.AddObject(TmpClause.Text, TmpClause);
              TmpClause := TGenClause.Create(FStructure);
              end
           //else
           else
              begin
              TmpClause.Free;
              stop := true;
              end;
           end;
         else
           stop := true;
         end;
       end;
  if state = -1 then
     result := i
  else
     result := -i;
  end;


{------------------------------------------------------------------------------}

function TLogicParser.ProcessHeader(i: integer): integer; // Needs a revision
  var
    stop  : boolean;
    state : integer;
    tok   : TToken;
    mark  : integer;
    tmp   : integer;
    inp   : boolean;

  begin
  state := 0;
  stop  := false;
  mark := -1;
  tmp := -1;
  inp := false;
  while not stop do
    if i >= FScanner.GetCount then
       stop := true
    else
       begin
       tok := FScanner.GetSaida(i);
       case state of
         0:
           begin
           //If token is 'header'
           if (tok.RefInt = 1) and (tok.IndTab = 0) then
              begin
              state := 1;
              i := i + 1;
              end
           //if token is 'program'
           else if (tok.RefInt = 1) and (tok.IndTab = 1) then
              begin
              FStyle := StClassic;
              state := -1;
              i := i + 1;
              stop := true;
              end
           //if token is a symbol
           else if (tok.RefInt = 0) then
              begin
              FStyle := StClassic;
              state := -1;
              stop := true;
              end
           //if token is a line break
           else if (tok.RefInt = 3) then
              i := i + 1
           //else
           else
              stop := true;
           end;

         1:
           begin
           //if token is line break
           if (tok.RefInt = 3) then
              begin
              i := i + 1;
              state := 2
              end
           //else
           else
              stop := true;
           end;

         2:
           begin
           //if token is 'style'
           if (tok.RefInt = 1) and (tok.IndTab = 2) then
              begin
              state := 3;
              i := i + 1;
              end
           //if token is 'program'
           else if (tok.RefInt = 1) and (tok.IndTab = 1) then
              begin
              FStyle := StClassic;
              state := -1;
              i := i + 1;
              stop := true;
              end
           //if token is line break
           else if (tok.RefInt = 3) then
              i := i + 1
           //else
           else
              state := 4;
           end;

         3:
           begin
           //if token is 'classic'
           if (tok.RefInt = 1) and (tok.IndTab = 3) then
              begin
              FStyle := StClassic;
              state := 4;
              i := i + 1;
              end
           //if token is 'modal'
           else if (tok.RefInt = 1) and (tok.IndTab = 4) then
              begin
              FStyle := StModal;
              state := 4;
              i := i + 1;
              end
           //if token is 'extra'
           else if (tok.RefInt = 1) and (tok.IndTab = 5) then
              begin
              FStyle := StExtra;
              state := 4;
              i := i + 1;
              end
           //if token is 'SCTL'
           else if (tok.RefInt = 1) and (tok.IndTab = 6) then
              begin
              FStyle := StSCTL;
              state := 4;
              i := i + 1;
              end

           //if token is 'CTLK'
           else if (tok.RefInt = 1) and (tok.IndTab = 7) then
              begin
              FStyle := StCTLK;
              state := 4;
              i := i + 1;
              end
           //else
           else
              stop := true;
           end;

         4:
           begin
           //if token is line break
           if (tok.RefInt = 3) then
              begin
              i := i + 1;
              state := 5;
              end
           //else
           else
              stop := true;
           end;
         5:
           begin
           //if token is 'atom'
           if (tok.RefInt = 1) and (tok.IndTab = 8) then
              begin
              mark := 0;
              state := 6;
              i := i + 1;
              end
           //if token is 'agent'
           else if (tok.RefInt = 1) and (tok.IndTab = 9) and
                              ((FStyle = StExtra) or (FStyle = StCTLK)) then
              begin
              mark := 1;
              state := 6;
              i := i + 1;
              end
           //if token is 'label' and style is modal
           else if (tok.RefInt = 1) and (tok.IndTab = 10) and
                               ((FStyle = StExtra) or (FStyle = StModal)) then
              begin
              mark := 2;
              state := 6;
              i := i + 1;
              end
           //if token is 'time' and style is temporal or CTLK
           else if (tok.RefInt = 1) and (tok.IndTab = 11) and
                               ((FStyle = StExtra) or (FStyle = StCTLK)) then
              begin
              mark := 2;
              state := 6;
              i := i + 1;
              end
           //if token is 'relation' and style is modal
           else if (tok.RefInt = 1) and (tok.IndTab = 12) and
                                    ((FStyle = StExtra) or (FStyle = StModal)) then
              begin
              state := 8;
              i := i + 1;
              end
           //if token is 'knows' and style is epistemic or CTLK
           else if (tok.RefInt = 1) and (tok.IndTab = 13) and
                                                      (FStyle = StCTLK) then
              begin
              state := 8;
              i := i + 1;
              end

           //if token is 'associate' and style is epistemic or CTLK
           else if (tok.RefInt = 1) and (tok.IndTab = 14) and
                              ((FStyle = StExtra) or (FStyle = StCTLK)) then
              begin
              state := 11;
              i := i + 1;
              end

           //if token is 'input'
           else if (tok.RefInt = 1) and (tok.IndTab = 15) then
              begin
              state := 13;
              i := i + 1;
              inp := true;
              end

           //if token is 'output'
           else if (tok.RefInt = 1) and (tok.IndTab = 16) then
              begin
              state := 13;
              i := i + 1;
              inp := false;
              end

           //if token is 'program'
           else if (tok.RefInt = 1) and (tok.IndTab = 1) then
              begin
              i := i + 1;
              state := -1;
              stop := true;
              end
           //if token is line break
           else if (tok.RefInt = 3) then
              i := i + 1
           //else
           else
              stop := true;
           end;

         6:
           begin
           //if token is an identifier
           if (tok.RefInt = 0) then
              begin
              case mark of
                0 : FStructure.AddAtom(FSymbols.Strings[tok.IndTab], tok.IndTab);
                1 : FStructure.AddAgentInPosition(FSymbols.Strings[tok.IndTab], tok.IndTab);
                2 : FStructure.AddLabelInPosition(FSymbols.Strings[tok.IndTab], tok.IndTab);
                end;

              i := i + 1;
              state := 7;
              end
           //else
           else
             stop := true;
           end;

         7:
           begin
           //if token is line break
           if (tok.RefInt = 3) then
              begin
              i := i + 1;
              state := 5;
              end
           //else
           else
              stop := true;
           end;

         8:
           begin
           //if token is an identifier (label in modal)
           if (tok.RefInt = 0) and (FStructure.IsLabel(tok.IndTab)) and
                                  ((FStyle = StExtra) or (FStyle = StModal)) then
              begin
              tmp := FStructure.GetIndex(tok.IndTab);
              i := i + 1;
              state := 9;
              end
           //if token is an identifier (agent in epistemic or CTLK)
           else if (tok.RefInt = 0) and (FStructure.IsAgent(tok.IndTab)) and
                                                          (FStyle = StCTLK) then
              begin
              tmp := FStructure.GetIndex(tok.IndTab);
              i := i + 1;
              state := 9;
              end
           //else
           else
             stop := true;
           end;

         9:
           begin
           //if token is an identifier (label in modal)
           if (tok.RefInt = 0) and (FStructure.IsLabel(tok.IndTab)) and
                                  ((FStyle = StExtra) or (FStyle = StModal)) then
              begin
              FStructure.AddRelation(tmp, FStructure.GetIndex(tok.IndTab));
              i := i + 1;
              state := 5;
              end
           //if token is an identifier (agent in epistemic or CTLK)
           else if (tok.RefInt = 0) and (FStructure.IsAgent(tok.IndTab)) and
                                                          (FStyle = StCTLK) then
              begin
              FStructure.AddRelation(tmp, FStructure.GetIndex(tok.IndTab));
              i := i + 1;
              state := 5;
              end
           //else
           else
             stop := true;
           end;

         10:
           begin
           //if token is line break
           if (tok.RefInt = 3) then
              begin
              i := i + 1;
              state := 5;
              end
           //else
           else
              stop := true;
           end;

         11:
           begin
           //if token is an atom
           if (tok.RefInt = 0) and (FStructure.IsAtom(tok.IndTab)) then
              begin
              tmp := FStructure.GetIndex(tok.IndTab);
              i := i + 1;
              state := 12;
              end
           //else
           else
             stop := true;
           end;

         12:
           begin
           //if token is an identifier (agent in epistemic or CTKL)
           if (tok.RefInt = 0) and (FStructure.IsAgent(tok.IndTab)) then
              begin
              FStructure.AddAssociation(FStructure.GetIndex(tok.IndTab), tmp);
              i := i + 1;
              state := 5;
              end
           //else
           else
              stop := true;
           end;
         13:
           begin
           //if token is an atom
           if (tok.RefInt = 0) and (FStructure.IsAtom(tok.IndTab)) then
              begin
              if inp then
                 FStructure.AddDefInput(-1, tok.IndTab)
              else
                 FStructure.AddDefOutput(-1, tok.IndTab);
              i := i + 1;
              state := 5;
              end

           //if token is a label and style in {Modal, Temporal, CTLK}
           else if (tok.RefInt = 0) and (FStructure.IsAtom(tok.IndTab))
                              and (FStyle in [StModal, StExtra, StCTLK]) then
              begin
              tmp := tok.IndTab;
              i := i + 1;
              state := 14;
              end

           //else
           else
              stop := true;
           end;

         14:
           begin
           //if token is an atom
           if (tok.RefInt = 0) and (FStructure.IsAtom(tok.IndTab)) then
              begin
              if inp then
                 FStructure.AddDefInput(tmp, tok.IndTab)
              else
                 FStructure.AddDefInput(tmp, tok.IndTab);
              i := i + 1;
              state := 5;
              end
            //else
           else
              stop := true;
            end;
         else
           stop := true;
         end;
       end;

  if state = -1 then
     result := i
  else
     result := -i;
  end;

{------------------------------------------------------------------------------}

function TLogicParser.ProcessLiteral(var i, par: integer): TGenLiteral; //??? (Needs a revision)
  var
    Xpar    : integer;
    stop     : boolean;
    tok      : TToken;
    XLiteral : TGenLiteral;
  begin

  stop := false;
  XLiteral := nil;

  while not Stop do
    begin
    if (i >= FScanner.GetCount) or (par < 0)then
      stop := true
    else
      begin
      tok := FScanner.GetSaida(i);
      //if token is negation
      if (tok.RefInt = 2) and ((tok.IndTab = 2) or (tok.IndTab = 3)) then
         begin
         i := i + 1;
         XLiteral := TGenLiteral.Create;
         XLiteral.Operator := OpNeg;
         XLiteral.Child1 := ProcessLiteral(i, par);
         end
      //if token is '('
      else if (tok.RefInt = 2) and (tok.IndTab = 6) then
         begin
         i := i + 1;
         par := par + 1;
         XLiteral := ProcessLiteral(i, par);
         end
      //if token is ')'
      else if (tok.RefInt = 2) and (tok.IndTab = 15) then
         begin
         i := i + 1;
         par := par - 1;
         end
      //if token is atom or unary operator
      else if IsUn_Atom(tok.RefInt, tok.IndTab) then
         begin
         XPar := 0;
         XLiteral := ProcessBinaryAtom(i, XPar);
         par := par + XPar
         end
      else
         stop := true;
      end;
    end;
  result := XLiteral;
  end;

  {------------------------------------------------------------------------------}

function TLogicParser.ProcessUnaryAtom(var i, par: integer): TGenLiteral;
  var
    tok : TToken;
    XLiteral1, XLiteral2 : TGenLiteral;
    par2 : integer;
  begin
  tok := FScanner.GetSaida(i);
  //If is unary operator
  XLiteral1 := nil;
  if (tok.RefInt = 2) and (tok.IndTab in [7, 8, 9, 10, 13, 14, 16, 17, 18, 19]) then
     begin
     XLiteral1 := TGenLiteral.Create;
     case tok.IndTab of
       7  : XLiteral1.Operator := OpDiamond;
       8  : XLiteral1.Operator := OpBox;
       9  : XLiteral1.Operator := OpK;
       10 : XLiteral1.Operator := OpK;
       13 : XLiteral1.Operator := OpPreviousTime;
       14 : XLiteral1.Operator := OpNextTime;
       16 : XLiteral1.Operator := OpFutureDiam;
       17 : XLiteral1.Operator := OpPastDiam;
       18 : XLiteral1.Operator := OpFutureBox;
       19 : XLiteral1.Operator := OpPastBox;
       end;
     i := i + 1;
     XLiteral2 := ProcessUnaryAtom(i, par);
     XLiteral1.Child1 := XLiteral2;
     end

  //if is atom
  else if (tok.RefInt = 0) and FStructure.IsAtom(tok.IndTab) then
    begin
    XLiteral1 := TGenLiteral.Create;
    XLiteral1.Operator := OpNone;
    XLiteral1.Atom1 := tok.IndTab;
    i := i + 1;
    end

  //If is '('
  else if (tok.RefInt = 2) and (tok.IndTab = 6) then
    begin
    i := i + 1;
    par2 := 1;
    XLiteral1 := ProcessBinaryAtom(i, par2);
    par := par + par2;
    end;
  result := XLiteral1;
  end;

end.
