{
  @abstract(Unit with the routines to manipulate non-classical logic programs)
  @author(Rafa "Passarel" Borges <passarel@gmail.com)
  @created()
  @lastmod()
}
unit UnLogGen; //717 lines

interface

uses Classes, UnLogStruct, UnLogSing, UnNetRep;

{------------------------------------------------------------------------------}

{
@abstract(Class that encapsulates general logic programs)

The class @name presents all the structures to represent logic programs with
in different styles, (as modal, temporal and also the classic). The class
describes diferents routines to manipulate these programs and also to convert it
in one (or a set of) single programs. It also realize the translation of
non-classic operators to neural networks architectures.
}

type TGenLogicProgram = class
  private
    //Style of the program
    FStyle     : TStyle;

    //Structure which the program is based
    FStructure : TStaticDefinitions;

    //List of clauses of the program
    FClauses   : TStringList;

    //Array with the association between clauses and worlds ???
    FAssocArray : array of integer;

    //Number of associations between clauses and worlds ???
    FAssocArraySize : integer;

    //Auxiliar structure //???
    FDefArray  : array of array of array of integer;

    //Array keeping the sub programs representing each possible world
    FSubPrograms : array of array of TSingleLogicProgram;

    //Clear the list of clauses
    procedure ClearClauses;

    //Update the strings on the stringlist according to the clauses
    procedure UpdateClauses;

    //Generate the auxiliar structures needed to the generation of the network //???
    procedure GenerateSubStructures;

    //Generate the auxiliar structures needed to the generation of the network //???
    procedure FillOtherArrays;

    //Generate the auxiliar structures needed to the generation of the network //???
    procedure FillAssocArray;

    //Generate the auxiliar structures needed to the generation of the network //???
    procedure FillWorldLinks;

    //Generate clauses to compute the semantic of temporal operators in SCTL
    procedure GenerateSCTLClauses;

    //Treat individual SCTL literal, verifying if already treated and adding clause if necessary
    procedure TreatSCTLLiteral(Lit : TGenLiteral; list : TStringList);

    //Returns the position of a literal on a list, or -1 if the literal is not on the list
    function ReturnLitPosition(Lit : TGenLiteral; list : TStringList) : integer;

    //Subdivision of FillWorldLinks for modal programs
    procedure FillWorldLinksModal;

    //Subdivision of FillWorldLinks for CTLK programs
    procedure FillWorldLinksCTLK;


  public
    //Constructor of the class
    constructor Create;

    //Destructor of the class
    Destructor Destroy; override;

    //Loads the program from a file
    //@param(FileName : Name of the file)
    //@return(number of tokens on the program (or a negative number indicating error))
    function  LoadFromFile(fileName : string) : integer;

    //Savess the program to a file
    //@param(FileName : Name of the file)
    procedure SaveToFile(FileName : string);

    //Generate files describing the substructures (for verification)
    //@param(FileName: Name of the file to be generated)
    procedure SaveSubToFile(FileName : string);

    //Translates the program to a neural network
    //@param(Net: network to be generated)
    procedure GenerateNetwork(Net : TNetworkRep);

    //Generate another file for verification
    procedure SaveAtomsToFile(fileName : string);

    //Generate another file for verification
    procedure FillSCTLLinks(HowTo : integer);

  end;

{------------------------------------------------------------------------------}

implementation

uses Forms, SysUtils, UnLogPars;

{------------------------------------------------------------------------------}

{ TGenLogicProgram }

{------------------------------------------------------------------------------}

procedure TGenLogicProgram.ClearClauses;
  var i : integer;
  begin
  for i := 0 to FClauses.Count - 1 do
    FClauses.Objects[i].Free;
  FClauses.Clear;
  end;

{------------------------------------------------------------------------------}

constructor TGenLogicProgram.Create;
  begin
  inherited Create;
  FStructure := TStaticDefinitions.Create;
  FClauses   := TStringList.Create;
  //And about the others???
  end;

{------------------------------------------------------------------------------}

destructor TGenLogicProgram.Destroy;
  begin
  FStructure.Free;
  ClearClauses;
  FClauses.Free;
  inherited;
  end;

{------------------------------------------------------------------------------}

procedure TGenLogicProgram.FillAssocArray;
  var
    i, j, k, k0, kn, i2, tot: integer;
    tmpArray : array of integer;
  begin
  case FStyle of
    StClassic, StSCTL:
      begin
      FAssocArraySize := FClauses.Count;
      SetLength(FAssocArray, FAssocArraySize * 3);
      for i := 0 to FClauses.Count - 1 do
        begin
        FAssocArray[i * 3] := i;
        FAssocArray[(i * 3) + 1] := 0;
        FAssocArray[(i * 3) + 2] := 0;
        end;
      end;
    StModal :
      begin
      FAssocArraySize := 0;
      i2 := 0;
      for i := 0 to FClauses.Count - 1 do
        begin
        if (FClauses.Objects[i] as TGenClause).ClauseLabel < 0 then
           begin
           k0 := 0;
           kn := FStructure.LabelCount - 1;
           end
        else
           begin
           k0 := (FClauses.Objects[i] as TGenClause).ClauseLabel;
           kn := k0;
           end;
        for k := k0 to kn do
          begin
          FAssocArraySize := FAssocArraySize + 1;
          SetLength(FAssocArray, FAssocArraySize * 3);
          FAssocArray[i2] := i;
          FAssocArray[i2 + 1] := 0;
          FAssocArray[i2 + 2] := k;
          i2 := i2 + 3;
          end;
        end;
      end;
    StCTLK, StExtra :
      begin
      FAssocArraySize := 0;
      i2 := 0;
      for i := 0 to FClauses.Count - 1 do
        if (FClauses.Objects[i] as TGenClause).Head.HasKnowledge then
           begin
           if (FClauses.Objects[i] as TGenClause).ClauseLabel < 0 then
              begin
              k0 := 0;
              kn := FStructure.LabelCount - 1;
              end
           else
              begin
              k0 := (FClauses.Objects[i] as TGenClause).ClauseLabel;
              kn := k0;
              end;
           for k := k0 to kn do
             begin
             FAssocArraySize := FAssocArraySize + 1;
             SetLength(FAssocArray, FAssocArraySize * 3);
             FAssocArray[i2] := i;
             FAssocArray[i2 + 1] := (FClauses.Objects[i] as TGenClause).Head.Agent;
             FAssocArray[i2 + 2] := k;
             i2 := i2 + 3;
             end;
           end
        else
           begin
           SetLength(tmpArray, FStructure.AgentCount);
           tot := FStructure.GetAssocAgents((FClauses.Objects[i] as TGenClause).Head.Atom1, TmpArray);
           if tot = 0 then
              begin
              for j := 0 to FStructure.AgentCount - 1 do
                tmpArray[j] := j;
              tot := FStructure.AgentCount;
              end;
           for j := 0 to tot - 1 do
             begin
             if (FClauses.Objects[i] as TGenClause).ClauseLabel < 0 then
                begin
                k0 := 0;
                kn := FStructure.LabelCount - 1;
                end
             else
                begin
                k0 := (FClauses.Objects[i] as TGenClause).ClauseLabel;
                kn := k0;
                end;
             for k := k0 to kn do
               begin
               FAssocArraySize := FAssocArraySize + 1;
               SetLength(FAssocArray, FAssocArraySize * 3);
               FAssocArray[i2] := i;
               FAssocArray[i2 + 1] := TmpArray[j];
               FAssocArray[i2 + 2] := k;
               i2 := i2 + 3;
               end;
             end;
           end;
      end;
    end;
  end;

{------------------------------------------------------------------------------}

procedure TGenLogicProgram.FillOtherArrays;
  var
    i, j, k, l, count : integer;
    ar : array of integer;
    b : boolean;

  begin
  SetLength(FDefArray, FSTructure.AtomCount, FSTructure.AgentCount, FSTructure.LabelCount);

  if FStructure.LabelCount > FStructure.AgentCount then
     SetLength(ar, FStructure.LabelCount)
  else
     SetLength(ar, FStructure.AgentCount);

  for i := 0 to FSTructure.AtomCount - 1 do
    for j := 0 to FStructure.AgentCount - 1 do
      for k := 0 to FStructure.LabelCount - 1 do
        if FSubPrograms[j, k].isBody(i) or FSubPrograms[j, k].isHead(i) then
           FDefArray[i, j, k] := 2
        else
           FDefArray[i, j, k] := 0;

  for i := 0 to FStructure.DefInputCount - 1 do
    for j := 0 to FStructure.AgentCount - 1 do
      for k := 0 to FStructure.LabelCount - 1 do
        if FStructure.DefInputLabel[i] = -1 then   //Or something ???
           FDefArray[FStructure.DefInputAtom[i], j, k] := 2;

  for i := 0 to FStructure.DefOutputCount - 1 do
    for j := 0 to FStructure.AgentCount - 1 do
      for k := 0 to FStructure.LabelCount - 1 do
        if FStructure.DefOutputLabel[i] = -1 then  //Or something ???
           FDefArray[FStructure.DefOutputAtom[i], j, k] := 2;

  case FStyle of
    //StClassic: do nothing
    //StExtra  : Not planned yet
    StModal :
      begin
      for i := FStructure.AtomCount - 1 downto 0 do
        for k := 0 to FStructure.LabelCount - 1 do
          if FDefArray[i, 0, k] > 0 then
             case FStructure.RefOperator[i] of
               OpBox :
                 begin
                 count := FStructure.GetRelatedFrom(k, ar);
                 for l := 0 to count - 1 do
                   FDefArray[FStructure.RefAtom[i], 0, ar[l]] :=
                                   FDefArray[FStructure.RefAtom[i], 0, ar[l]] + 1;
                 end;
               OpDiamond :
                 begin
                 count := FStructure.GetRelatedFrom(k, ar);
                 for l := 0 to count - 1 do
                   FDefArray[FStructure.RefAtom[i], 0, ar[l]] :=
                                   FDefArray[FStructure.RefAtom[i], 0, ar[l]] + 1;
                 end;
             end;
      for i := 0 to FStructure.AtomCount - 1 do
        if FStructure.RefOperator[i] <> OpNone then
           for k := 0 to FStructure.LabelCount - 1 do
             case FStructure.RefOperator[i] of
               OpBox :
                 begin
                 count := FStructure.GetRelatedFrom(k, ar);
                 b := false;
                 l := 0;
                 while (l < count) and not b do
                   begin
                   b := FDefArray[FStructure.RefAtom[i], 0, ar[l]] > 1;
                   l := l + 1;
                   end;
                 if b then
                    FDefArray[i, 0, k] := FDefArray[i, 0, k] + 1;
                 end;
               OpDiamond :
                 begin
                 count := FStructure.GetRelatedFrom(k, ar);
                 b := false;
                 l := 0;
                 while (l < count) and not b do
                   begin
                   b := FDefArray[FStructure.RefAtom[i], 0, ar[l]] > 1;
                   l := l + 1;
                   end;
                 if b then FDefArray[i, 0, k] := FDefArray[i, 0, k] + 1;
                 end;
               end;
      end;
    StCTLK :
      begin
      for i := FStructure.AtomCount - 1 downto 0 do
        for j := 0 to FStructure.AgentCount - 1 do
          for k := 0 to FStructure.LabelCount - 1 do
            if FDefArray[i, j, k] > 0 then
               case FStructure.RefOperator[i] of
                 //OpK: Still Undefined;
                 OpPreviousTime :
                   if k > 0 then
                      FDefArray[FStructure.RefAtom[i], j, k - 1] :=
                                 FDefArray[FStructure.RefAtom[i], j, k - 1] + 1;
                 OpNextTime :
                   if k < FStructure.LabelCount - 1 then
                      FDefArray[FStructure.RefAtom[i], j, k + 1] :=
                                 FDefArray[FStructure.RefAtom[i], j, k + 1] + 1;
                 end;
      for i := 0 to FStructure.AtomCount - 1 do
        if FStructure.RefOperator[i] <> OpNone then
           for j := 0 to FStructure.AgentCount - 1 do
             for k := 0 to FStructure.LabelCount - 1 do
               case FStructure.RefOperator[i] of
                 //OpK: Still Undefined
                 OpPreviousTime :
                   if k > 0 then
                      if FDefArray[FStructure.RefAtom[i], j, k - 1] > 1 then
                         FDefArray[i, j, k] := FDefArray[i, j, k] + 1;
                 OpNextTime :
                   if k < FStructure.LabelCount - 1 then
                      if FDefArray[FStructure.RefAtom[i], j, k + 1] > 1 then
                         FDefArray[i, j, k] := FDefArray[i, j, k] + 1;
                 end;
      end;
    StSCTL :
      begin
      //if not delayed
      for i := FStructure.AtomCount - 1 downto 0 do
        if FStructure.RefOperator[i] = OpPreviousTime then
           FDefArray[FStructure.RefAtom[i], 0, 0] := FDefArray[FStructure.RefAtom[i], 0, 0] + 1;
      for i := 0 to FStructure.AtomCount - 1 do
        if FStructure.RefOperator[i] = OpPreviousTime then
           if FDefArray[FStructure.RefAtom[i], 0, 0] > 1 then
              FDefArray[i, 0, 0] := FDefArray[i, 0, 0] + 1;
       end;
    end;
  end;

{------------------------------------------------------------------------------}

procedure TGenLogicProgram.FillSCTLLinks(HowTo : integer);
  var i : integer;
  begin
  for i := FStructure.AtomCount - 1 downto 0 do
    if FStructure.RefOperator[i] = OpPreviousTime then
       begin
       FStructure.AddLinkPoint(LkRecursive);
       FSubprograms[0, 0].AddOutputLink(FStructure.RefAtom[i], FStructure.LinksCount - 1, OtPtI);
       FSubprograms[0, 0].AddinputLink (i, FStructure.LinksCount - 1, OtPtI);
       end;



  end;

{------------------------------------------------------------------------------}

procedure TGenLogicProgram.FillWorldLinks;
  var
    i, j, k, l : integer;
  begin
  //Define External Input
  for i := 0 to FStructure.DefInputCount - 1 do
    if FStructure.DefInputLabel[i] = -1 then                   //Or Something???
       for j := 0 to FStructure.AgentCount - 1 do
         for k := 0 to FStructure.LabelCount - 1 do
           begin
           FStructure.AddLinkPoint(LkInput);
           FSubprograms[j, k].AddInputLink(FStructure.DefInputAtom[i], FStructure.LinksCount - 1, OtExt);
           end;

  //Define External Output
  for i := 0 to FStructure.DefOutputCount - 1 do
    if FStructure.DefOutputLabel[i] = -1 then                  //OrSomething
       for j := 0 to FStructure.AgentCount - 1 do
         for k := 0 to FStructure.LabelCount - 1 do
{           if FStyle = stSCTL then
              begin
              for l := 0 to FStructure.AtomCount - 1 do
                if FStructure.RefAtom[l] = FStructure.DefOutputAtom[i] then
                   begin
                   FStructure.AddLinkPoint(LkOutput);
                   FSubPrograms[j, k].AddOutputLink(l, FStructure.LinksCount - 1, OtExt);
                   end;
              end
           else}
              begin
              FStructure.AddLinkPoint(LkOutput);
              FSubprograms[j, k].AddOutputLink(FStructure.DefOutputAtom[i], Fstructure.LinksCount - 1, OtExt);
              end;
  case FStyle of
    StModal : FillWorldLinksModal;
    StCTLK  : FillWorldLinksCTLK;
    StSCTL  : FillSCTLLinks(0);
    end;

  end;

{------------------------------------------------------------------------------}

procedure TGenLogicProgram.FillWorldLinksCTLK;
  var
    i, j, k : integer;
    b : boolean;
  begin
  for i := FStructure.AtomCount - 1 downto 0 do
    begin
    for j := 0 to FStructure.AgentCount - 1 do
      for k := 0 to FStructure.LabelCount - 1 do
        begin
        if FDefArray[i, j, k] >= 2 then
           case FStructure.RefOperator[i] of
             OpNextTime :
               begin
               b := (k < FStructure.LabelCount - 1) and
                    (FDefArray[FStructure.RefAtom[i], j, k + 1] >= 2);
               if b then
                  begin
                  FStructure.AddLinkPoint(LkWorlds);
                  FSubPrograms[j, k].AddInputLink(i, FStructure.LinksCount - 1, OtNtI);
                  FSubPrograms[j, k + 1].AddOutputLink(FStructure.RefAtom[i], FStructure.LinksCount - 1, OtNtI);
                  FStructure.AddLinkPoint(LkWorlds);
                  FSubPrograms[j, k].AddOutputLink(i,FStructure.LinksCount - 1, OtNtE);
                  FSubPrograms[j, k + 1].AddInputLink(FStructure.RefAtom[i], FStructure.LinksCount - 1, OtNtE);
                  end
               end;
             OpPreviousTime :
               begin
               b := (k > 0) and (FDefArray[FStructure.RefAtom[i], j, k - 1] >= 2);
               if b then
                  begin
                  FStructure.AddLinkPoint(LkWorlds);
                  FSubPrograms[j, k].AddOutputLink(i, FStructure.LinksCount - 1, OtPtE);
                  FSubPrograms[j, k - 1].AddInputLink(FStructure.RefAtom[i], FStructure.LinksCount - 1, OtPtE);
                  FStructure.AddLinkPoint(LkWorlds);
                  FSubPrograms[j, k].AddInputLink(i, FStructure.LinksCount - 1, OtPtI);
                  FSubPrograms[j, k - 1].AddOutputLink(FStructure.RefAtom[i], FStructure.LinksCount - 1, OtPtI);
                  end;
               end;
             end;
        end;
    end;
  end;

{------------------------------------------------------------------------------}

procedure TGenLogicProgram.FillWorldLinksModal;
  var
    i, j, k, l : integer;
    count : integer;
    ar : array of integer;
    b : boolean;

  begin
  SetLength(ar, FStructure.LabelCount);
  for i := FStructure.AtomCount - 1 downto 0 do
    begin
    for j := 0 to FStructure.AgentCount - 1 do
      for k := 0 to FStructure.LabelCount - 1 do
        begin
        if FDefArray[i, j, k] >= 2 then
           case FStructure.RefOperator[i] of
             OpBox :
               begin
               count := FStructure.GetRelatedFrom(k, ar);
               b := true;
               if count > 0 then
                  begin
                  FStructure.AddLinkPoint(LkWorlds);
                  FSubPrograms[0, k].AddOutputLink(i, FStructure.LinksCount - 1, OtBoxE);
                  end;
               for l := 0 to count - 1 do
                 if FDefArray[FStructure.RefAtom[i], j, ar[l]] >= 2 then
                    FSubPrograms[0, ar[l]].AddInputLink(FStructure.RefAtom[i], FStructure.LinksCount - 1, OtBoxE)
                 else
                    b := false;

               if b then
                  for l := 0 to count - 1 do
                    begin
                    FStructure.AddLinkPoint(LkWorlds);
                    FSubPrograms[0, k].AddInputLink(i, FStructure.LinksCount - 1, OtBoxI);
                    FSubPrograms[0, ar[l]].AddOutputLink(FStructure.RefAtom[i], FStructure.LinksCount - 1, OtBoxI);
                    end;
               end;

             OpDiamond :
               begin
               count := FStructure.GetRelatedFrom(k, ar);
               b := true;
               for l := 0 to count - 1 do
                 if FDefArray[FStructure.RefAtom[i], j, ar[l]] >= 2 then
                    begin
                    FStructure.AddLinkPoint(LkWorlds);
                    FSubPrograms[0, k].AddInputLink(i, FStructure.LinksCount - 1, OtDiamondI);
                    FSubPrograms[0, ar[l]].AddOutputLink(FStructure.RefAtom[i], FStructure.LinksCount - 1, OtDiamondI);
                    end
                 else
                    b := false;
               if b then
                  begin
                  for l := 0 to count - 1 do
                    begin
                    //Não sei o q vou fazer: DiamondE
                    end;
                  end;
               end;
             end;
        end;
    end;
  end;

{------------------------------------------------------------------------------}

procedure TGenLogicProgram.GenerateNetwork(Net: TNetworkRep);
//Needs to be verified ???
  var i, j : integer;
  begin
  Net.ClearArcs;
  Net.ClearNodes;
  Net.WorldCount := FStructure.AgentCount * FStructure.LabelCount;
  for i := 0 to FStructure.LinksCount - 1 do
    begin
    Net.AddNode('Link' + inttostr(i), UkRec);
    if FStructure.LinkPoints[i] = LkOutput then
       (Net.Node[i] as TRecLinkRep).Level := 2
    else if FStructure.LinkPoints[i] = LkInput then
       (Net.Node[i] as TRecLinkRep).Level := 3
    else
       (Net.Node[i] as TRecLinkRep).Level := 1;
    end;


  for i := 0 to FStructure.LinksCount - 1 do
    begin
    case FStructure.LinkPoints[i] of
      LkInput  :
        begin
        Net.AddNode('Input' + inttostr(i), UkInput);
        Net.AddArc(Net.NoNodes - 1, i);
        end;
      LkOutput :
        begin
        Net.AddNode('Output' + inttostr(i), UkOutput);
        Net.AddArc(i, Net.NoNodes - 1);
        end;
      end;
    end;

  for i := 0 to FStructure.AgentCount - 1 do
    for j := 0 to FStructure.LabelCount - 1 do
      FSubPrograms[i, j].SaveToCILPNetwork(Net, (i * FStructure.AgentCount) + j);
  end;

{------------------------------------------------------------------------------}

procedure TGenLogicProgram.GenerateSCTLClauses;
  var
    XLiterals : TStringList;
//    TmpLiteral : TGenLiteral;
    i, j : integer;
  begin
  XLiterals := TStringList.Create;
  for i := 0 to FClauses.Count - 1 do
    with FClauses.Objects[i] as TGenClause do
      begin
      TreatSCTLLiteral(Head, XLiterals);
      for j := 0 to BodySize - 1 do
        TreatSCTLLiteral(Body[j], XLiterals);
      end;
  end;

{------------------------------------------------------------------------------}

procedure TGenLogicProgram.GenerateSubStructures;
//Needs to be verified ???
  var
    dimX, dimY : integer;
    i, j, MaxT : integer;
    tmp : TSingleClause;
  begin
  DimX := 1;
  DimY := 1;
  case FStyle of
    StClassic :
      begin
      FStructure.AddAgent('Unique');
      FStructure.AddLabel('Unique');
      end;
    StModal :
      begin
      FStructure.AddAgent('Unique');
      if FStructure.LabelCount = 0 then
         FStructure.AddLabel('Unique');
      DimY := FStructure.LabelCount;
      end;
    StSCTL :
      begin
      FStructure.AddAgent('Unique');
      FStructure.AddLabel('Unique');
      end;
    StExtra :
      begin
      if FStructure.AgentCount = 0 then
         FStructure.AddAgent('Unique');
      if FStructure.LabelCount = 0 then
         FStructure.AddLabel('Unique');
      DimX := FStructure.AgentCount;
      DimY := FStructure.LabelCount;
      end;
    StCTLK :
      begin
      if FStructure.AgentCount = 0 then
         FStructure.AddAgent('Unique');
      if FStructure.LabelCount = 0 then
         FStructure.AddLabel('Unique');
      DimX := FStructure.AgentCount;
      DimY := FStructure.LabelCount;
      end;
    end;
  SetLength(FSubPrograms, DimX, DimY);

  for i := 0 to DimX - 1 do
    for j := 0 to DimY - 1 do
      FSubPrograms[i, j] := TSingleLogicProgram.CreateFromStruct(FStructure);

  if FStyle = StSCTL then
     GenerateSCTLClauses;

  FillAssocArray;

  for i := 0 to FAssocArraySize - 1 do
    begin
    tmp := TSingleClause.Create(FStructure, nil);
    tmp.LoadFromGeneral(FClauses.Objects[FAssocArray[i * 3]] as TGenClause);
    FSubPrograms[FAssocArray[(i * 3) + 1], FAssocArray[(i * 3) + 2]].AddClause(tmp);
    end;

  FillOtherArrays;
  FillWorldLinks;
  end;

{------------------------------------------------------------------------------}

function TGenLogicProgram.LoadFromFile(fileName: string): integer;
  var
    Par : TLogicParser;
    s : string;
  begin
  ClearClauses;
  FStructure.Clear;

  s := ExtractFileDir(application.ExeName);
  Par := TLogicParser.Create(S + '\arq\logic2008.aut', S + '\arq\logic2008.scn');
  result := Par.Execute(fileName, FStructure, FClauses, FStyle);
  UpdateClauses;
  Par.Free;
  end;

{------------------------------------------------------------------------------}

function TGenLogicProgram.ReturnLitPosition(Lit: TGenLiteral;  list: TStringList) : integer;
  var
    i : integer;
    b : boolean;
  begin
  i := 0;
  b := false;
  while not b and (i < List.Count) do
    begin
    b := (List.Objects[i] as TGenLiteral).IsEqual(Lit);
    i := i + 1;
    end;
  if b then
    result := i
  else
    result := -1;
  end;

{------------------------------------------------------------------------------}

procedure TGenLogicProgram.SaveAtomsToFile(fileName : string);
  var
  strL : TStringList;
  s : string;
  i : integer;
  begin
  Strl := TStringList.Create;
  for i := 0 to FStructure.AtomCount - 1 do
    begin
    s := inttostr(i) + ': ';
    s := s + FStructure.Atoms[i] + ' --> Operator: ';
    case FStructure.RefOperator[i] of
      OpNone         : s := s + 'OpNone, Atom1: ';
      OpNeg          : s := s + 'OpNeg, Atom1: ';
      OpBox          : s := s + 'OpBox, Atom1: ';
      OpDiamond      : s := s + 'OpDiamond, Atom1: ';
      OpK            : s := s + 'OpK, Atom1: ';
      OpNextTime     : s := s + 'OpNextTime, Atom1: ';
      OpPreviousTime : s := s + 'OpPreviousTime, Atom1: ';
      OpFutureDiam   : s := s + 'OpFutureDiam, Atom1: ';
      OpPastDiam     : s := s + 'OpPastDiam, Atom1: ';
      OpFutureBox    : s := s + 'OpFutureBox, Atom1: ';
      OpPastBox      : s := s + 'OpPastBox, Atom1: ';
      OpSince        : s := s + 'OpSince, Atom1: ';
      OpZince        : s := s + 'OpZince, Atom1: ';
      OpUntil        : s := s + 'OpUntil, Atom1: ';
      OpUnless       : s := s + 'OpUnless, Atom1: ';
      end;
    s := s + inttostr(FStructure.RefAtom[i]) + ', Atom2: ';
    s := s + IntToStr(FStructure.RefAtom2[i]);
    Strl.Add(s);
    end;
  strl.SaveToFile(filename);
  strl.Free;
  end;

{------------------------------------------------------------------------------}


procedure TGenLogicProgram.SaveSubToFile(FileName: string);
  var
    i, j : integer;
    StrList : TStringList;
  begin
  GenerateSubStructures; //???
  StrList := TStringlist.Create;
  for j := 0 to FStructure.LabelCount - 1 do
    for i := 0 to FStructure.AgentCount - 1 do
      begin
      StrList.Add('Agent = ' + FStructure.Agents[i] + ' Label = ' + FStructure.Labels[j]);
      StrList.Add('');
      FSubPrograms[i, j].LoadStringList(StrList, false);
      StrList.Add('');
      end;
  StrList.SaveToFile(FileName);

  StrList.Clear;
  for j := 0 to FStructure.LabelCount - 1 do
    for i := 0 to FStructure.AgentCount - 1 do
      begin
      FSubPrograms[i, j].DynamizeProgram(true);
      StrList.Add('Agent = ' + FStructure.Agents[i] + ' Label = ' + FStructure.Labels[j]);
      StrList.Add('');
      FSubPrograms[i, j].LoadStringList(StrList, true);
      StrList.Add('');
      end;
  StrList.SaveToFile(FileName + 'a');

  StrList.Clear;
  for j := 0 to FStructure.LabelCount - 1 do
    for i := 0 to FStructure.AgentCount - 1 do
      begin
      FSubPrograms[i, j].SimplifyProgram;
      StrList.Add('Agent = ' + FStructure.Agents[i] + ' Label = ' + FStructure.Labels[j]);
      StrList.Add('');
      FSubPrograms[i, j].LoadStringList(StrList, true);
      StrList.Add('');
      end;
  StrList.SaveToFile(FileName + 'b');

  StrList.Clear;
  for j := 0 to FStructure.LabelCount - 1 do
    for i := 0 to FStructure.AgentCount - 1 do
      begin
      FSubPrograms[i, j].NewSimplification;
      StrList.Add('Agent = ' + FStructure.Agents[i] + ' Label = ' + FStructure.Labels[j]);
      StrList.Add('');
      FSubPrograms[i, j].LoadStringList(StrList, true);
      StrList.Add('');
      end;
  StrList.SaveToFile(FileName + 'c');
  end;

{------------------------------------------------------------------------------}

procedure TGenLogicProgram.SaveToFile(FileName: string);
  var
    txt : TStringList;
    i : integer;
  begin
  UpdateClauses;
  txt := TStringList.Create;
  txt.Add('header');
  FStructure.WriteHeader(txt, FStyle);
  txt.Add('program');
  for i := 0 to FClauses.Count - 1 do
    txt.Add((FClauses.Objects[i] as TGenClause).Text);
  txt.SaveToFile(FileName);
  end;

{------------------------------------------------------------------------------}

procedure TGenLogicProgram.TreatSCTLLiteral(Lit: TGenLiteral; list: TStringList);
  var
    i : integer;
    TmpClause : TGenClause;
    TmpLiteral, TmpLiteral2 : TGenLiteral;
  begin
  if (lit <> nil) and (Lit.IsRecursive) then
     if lit.Operator in [OpNone, OpNeg, OpPreviousTime] then
        TreatSCTLLiteral(lit.Child1, list)
     else
        begin
        i := ReturnLitPosition(lit, list);
        if i < 0 then
           case Lit.Operator of
             OpPastDiam :
               begin
               TmpLiteral := TGenLiteral.Create;
               TmpLiteral.Operator := OpPreviousTime;
               TmpLiteral.Child1 := lit;
               TmpClause := TGenClause.Create(FStructure);
               TmpClause.Head := lit;
               TmpClause.AddBodyLiteral(lit.Child1);
               FClauses.AddObject(TmpClause.Text, TmpClause);
               TmpClause := TGenClause.Create(FStructure);
               TmpClause.Head := lit;
               TmpClause.AddBodyLiteral(TmpLiteral);
               FClauses.AddObject(TmpClause.Text, TmpClause);
               list.AddObject('', Lit);
               end;
             OpPastBox :
               begin
               TmpLiteral := TGenLiteral.Create;
               TmpLiteral.Operator := OpPreviousTime;
               TmpLiteral.Child1 := lit;
               TmpClause := TGenClause.Create(FStructure);
               TmpClause.Head := lit;
               TmpClause.AddBodyLiteral(lit.Child1);
               TmpClause.AddBodyLiteral(TmpLiteral);
               FClauses.AddObject(TmpClause.Text, TmpClause);
               list.AddObject('', Lit);
               end;
             OpSince, OpZince:
               begin
               TmpLiteral := TGenLiteral.Create;
               TmpLiteral.Operator := OpPreviousTime;
               TmpLiteral.Child1 := lit;
               TmpClause := TGenClause.Create(FStructure);
               TmpClause.Head := lit;
               TmpClause.AddBodyLiteral(lit.Child2);
               FClauses.AddObject(TmpClause.Text, TmpClause);
               TmpClause := TGenClause.Create(FStructure);
               TmpClause.Head := lit;
               TmpClause.AddBodyLiteral(lit.Child1);
               TmpClause.AddBodyLiteral(TmpLiteral);
               FClauses.AddObject(TmpClause.Text, TmpClause);
               list.AddObject('', Lit);
               end;
             OpNextTime:
               begin
               TmpLiteral := TGenLiteral.Create;
               TmpLiteral.Operator := OpPreviousTime;
               TmpLiteral.Child1 := lit;
               TmpClause := TGenClause.Create(FStructure);
               TmpClause.Head := lit.Child1;
               TmpClause.AddBodyLiteral(TmpLiteral);
               FClauses.AddObject(TmpClause.Text, TmpClause);
               list.AddObject('', Lit);
               end;
             OpFutureDiam:
               begin
               TmpClause := TGenClause.Create(FStructure);
               TmpClause.Head := lit;
               TmpLiteral := TGenLiteral.Create;
               TmpLiteral.Operator := OpPreviousTime;
               TmpLiteral.Child1 := lit;
               TmpClause.AddBodyLiteral(TmpLiteral);
               TmpLiteral := TGenLiteral.Create;
               TmpLiteral.Operator := OpNeg;
               TmpLiteral.Child1 := lit.Child1;
               TmpClause.AddBodyLiteral(TmpLiteral);
               FClauses.AddObject(TmpClause.Text, TmpClause);
               list.AddObject('', Lit);
               end;
             OpFutureBox :
               begin
               TmpClause := TGenClause.Create(FStructure);
               TmpClause.Head := lit.Child1;
               TmpClause.AddBodyLiteral(lit);
               FClauses.AddObject(TmpClause.Text, TmpClause);
               TmpClause := TGenClause.Create(FStructure);
               TmpLiteral := TGenLiteral.Create;
               TmpLiteral.Operator := OpPreviousTime;
               TmpLiteral.Child1 := lit;
               TmpClause.Head := lit;
               TmpClause.AddBodyLiteral(TmpLiteral);
               FClauses.AddObject(TmpClause.Text, TmpClause);
               list.AddObject('', Lit);
               end;
             OpUntil :
               begin
               TmpClause := TGenClause.Create(FStructure);
               TmpClause.Head := lit;
               TmpLiteral := TGenLiteral.Create;
               TmpLiteral.Operator := OpPreviousTime;
               TmpLiteral.Child1 := lit;
               TmpClause.AddBodyLiteral(TmpLiteral);
               TmpLiteral := TGenLiteral.Create;
               TmpLiteral.Operator := Opneg;
               TmpLiteral.Child1 := TGenLiteral.Create;
               TmpLiteral.Child1.Operator := OpPreviousTime;
               TmpLiteral.Child1.Child1 := Lit.Child2;
               TmpClause.AddBodyLiteral(TmpLiteral);
               FClauses.AddObject(TmpClause.Text, TmpClause);
               TmpClause := TGenClause.Create(FStructure);
               TmpClause.Head := lit.Child2;
               TmpLiteral := TGenLiteral.Create;
               TmpLiteral.Operator := Opneg;
               TmpLiteral.Child1 := Lit.Child1;
               TmpClause.AddBodyLiteral(lit);
               TmpClause.AddBodyLiteral(TmpLiteral);
               FClauses.AddObject(TmpClause.Text, TmpClause);
               list.AddObject('', Lit);
               end;
             OpUnless :
               begin
               TmpClause := TGenClause.Create(FStructure);
               TmpClause.Head := lit;
               TmpLiteral := TGenLiteral.Create;
               TmpLiteral.Operator := OpPreviousTime;
               TmpLiteral.Child1 := lit;
               TmpClause.AddBodyLiteral(TmpLiteral);
               TmpLiteral := TGenLiteral.Create;
               TmpLiteral.Operator := Opneg;
               TmpLiteral.Child1 := TGenLiteral.Create;
               TmpLiteral.Child1.Operator := OpPreviousTime;
               TmpLiteral.Child1.Child1 := Lit.Child2;
               TmpClause.AddBodyLiteral(TmpLiteral);
               FClauses.AddObject(TmpClause.Text, TmpClause);
               TmpClause := TGenClause.Create(FStructure);
               TmpClause.Head := lit.Child1;
               TmpLiteral := TGenLiteral.Create;
               TmpLiteral.Operator := Opneg;
               TmpLiteral.Child1 := Lit.Child2;
               TmpClause.AddBodyLiteral(lit);
               TmpClause.AddBodyLiteral(TmpLiteral);
               FClauses.AddObject(TmpClause.Text, TmpClause);
               list.AddObject('', Lit);               
               end;
             end;
        end;
  end;

{------------------------------------------------------------------------------}

procedure TGenLogicProgram.UpdateClauses;
  var i : integer;
  begin
  for i := 0 to FClauses.Count -1 do
    FClauses.Strings[i] := (FClauses.Objects[i] as TGenClause).Text;
  end;

{------------------------------------------------------------------------------}





(*

Old FillOtherArrays





{  for i := FStructure.AtomCount - 1 downto 0 do
    begin
    for j := 0 to FStructure.AgentCount - 1 do
      for k := 0 to FStructure.LabelCount - 1 do
        begin
        if FDefArray[i, j, k] > 0 then
           case FStructure.RefOperator[i] of
             OpBox :
               begin
               count := FStructure.GetRelatedFrom(k, ar);
               for l := 0 to count - 1 do
                 FDefArray[FStructure.RefAtom[i], j, ar[l]] :=
                                 FDefArray[FStructure.RefAtom[i], j, ar[l]] + 1;
               end;
             OpDiamond :
               begin
               count := FStructure.GetRelatedFrom(k, ar);
               for l := 0 to count - 1 do
                 FDefArray[FStructure.RefAtom[i], j, ar[l]] :=
                                 FDefArray[FStructure.RefAtom[i], j, ar[l]] + 1;
              end;
             OpNextTime :
               begin
               if FStyle = StSCTL then
                  FDefArray[FStructure.RefAtom[i], j, k] :=
                                 FDefArray[FStructure.RefAtom[i], j, k] + 1
               else
                  FDefArray[FStructure.RefAtom[i], j, k + 1] :=
                                 FDefArray[FStructure.RefAtom[i], j, k + 1] + 1;
               end;

             OpPreviousTime :
               begin
               if FStyle = StSCTL then
                  FDefArray[FStructure.RefAtom[i], j, k] :=
                                 FDefArray[FStructure.RefAtom[i], j, k] + 1
               else
                  FDefArray[FStructure.RefAtom[i], j, k - 1] :=
                                 FDefArray[FStructure.RefAtom[i], j, k - 1] + 1;
               end;

             OpK :
               begin
               count := FStructure.GetRelatedFrom(j, ar);
               for l := 0 to count - 1 do
                 FDefArray[FStructure.RefAtom[i], ar[l], k] :=
                                 FDefArray[FStructure.RefAtom[i], ar[l], k] + 1;
               //This depends of: Association of atoms and definition of K
               end;
             end;
        end;
    end;

  for i := 0 to FStructure.AtomCount - 1 do
    begin
    if FStructure.RefOperator[i] <> OpNone then
       for j := 0 to FStructure.AgentCount - 1 do
         for k := 0 to FStructure.LabelCount - 1 do
           begin
           case FStructure.RefOperator[i] of
             OpBox :
               begin
               count := FStructure.GetRelatedFrom(k, ar);
               b := false;
               l := 0;
               while (l < count) and not b do
                 begin
                 b := FDefArray[FStructure.RefAtom[i], j, ar[l]] > 1;
                 l := l + 1;
                 end;
               if b then
                  FDefArray[i, j, k] := FDefArray[i, j, k] + 1;
               end;

             OpDiamond :
               begin
               count := FStructure.GetRelatedFrom(k, ar);
               b := false;
               l := 0;
               while (l < count) and not b do
                 begin
                 b := FDefArray[FStructure.RefAtom[i], j, ar[l]] > 1;
                 l := l + 1;
                 end;
               if b then
                  FDefArray[i, j, k] := FDefArray[i, j, k] + 1;
               end;

             OpNextTime :
               begin
               if FStyle = StSCTL then
                  if FDefArray[FStructure.RefAtom[i], j, k] > 1 then
                     FDefArray[i, j, k] := FDefArray[i, j, k] + 1
               else
                  if FDefArray[FStructure.RefAtom[i], j, k + 1] > 1 then
                     FDefArray[i, j, k] := FDefArray[i, j, k] + 1;
               end;

             OpPreviousTime :
               begin
               if FStyle = StSCTL then
                  if FDefArray[FStructure.RefAtom[i], j, k] > 1 then
                     FDefArray[i, j, k] := FDefArray[i, j, k] + 1
               else
                  if FDefArray[FStructure.RefAtom[i], j, k - 1] > 1 then
                     FDefArray[i, j, k] := FDefArray[i, j, k] + 1;
               end;

             OpK :
               begin
               count := FStructure.GetRelatedFrom(j, ar);
               b := false;
               l := 0;
               while (l < count) and not b do
                 begin
                 b := FDefArray[FStructure.RefAtom[i], ar[l], k] > 1;
                 l := l + 1;
                 end;
               if b then
                  FDefArray[i, j, k] := FDefArray[i, j, k] + 1;
               //This depends of: Association of atoms and definition of K
               end;
             end;
        end;
    end;}

  if FDefArray[0, 0, 0] <= -1 then
     FStyle := StClassic;



*)

end.
