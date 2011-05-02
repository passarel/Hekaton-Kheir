unit UnMCDiningTable;

//------------------------------------------------------------------------------------------------

interface

uses SysUtils, Classes, UnMCPhilosopher;

//------------------------------------------------------------------------------------------------

type TAllocationPolicy = (ApAlways, ApAvoidDeadLock, ApOneFork);

//------------------------------------------------------------------------------------------------

type TDiningTableStepEvent = procedure(TimePoint, MetricsLength : integer; Metrics : array of double) of Object;

//------------------------------------------------------------------------------------------------

type TDiningTable = class
   private
     FAgents     : TStringList;

     FAllocationPolicy : TAllocationPolicy;
     FProbToLeft : double;

     FCancel : boolean;
     FCurrentTimePoint : integer;

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

     FStepEvent : TDiningTableStepEvent;

     FForks   : array of Integer;
     FStomach : array of integer;

     function GetStepEvent: TDiningTableStepEvent;
     procedure SetStepEvent(const Value: TDiningTableStepEvent);

     procedure DefaultStepEvent(TimePoint, MetricsLength : integer; Metrics : array of double);

     procedure ResetIfDeadlock;

   public
     Constructor Create(numberOfAgents : integer; Files : TStrings;
                   KnowledgeLevel : array of integer);

     Constructor CreatePar(numberOfAgents : integer; Files : TStrings; KnowledgeLevel : array of integer;
                    leftProb : double; allocation : TAllocationPolicy); 

     destructor Destroy; override;
     procedure SetNewTimePoint;
     function  GetAgent(index : integer) : TPhilosopher;
     procedure Execute(Offline, Correct : boolean);

     function BlackBoxOffline(timeCount : integer; RefNetwork, FileName : TFileName; SelAgent : integer = 0) : boolean;
     function BlackBoxOnline(timeCount : integer; RefNetwork, FileName : TFileName; LearnType : integer; SelAgent : integer = 0) : boolean;
     function IndividualEvolution(timeCount : integer; FileName : TFileName; SelAgent : integer = 0) : boolean;
     function OverallEvolution(timeCount : integer; FileName : TFileName) : boolean;

     procedure CancelExperiment;
     function GetCurrentTimePoint : integer;

     function GetResult(li : TStrings) : boolean;

     property StepEvent : TDiningTableStepEvent read GetStepEvent write SetStepEvent;
   end;

//------------------------------------------------------------------------------------------------

implementation

uses Math, Forms;

//------------------------------------------------------------------------------------------------

{ TDiningTable }

//------------------------------------------------------------------------------------------------

function TDiningTable.BlackBoxOffline(timeCount: integer; RefNetwork, FileName: TFileName;
                                      SelAgent : integer = 0): boolean;

  var
    i, j, XExtraInf : integer;
    XTmp : double;
    XArInputs, XArOutputs : array of double;
    XResults : array of TStringList;
    XMetrics, XFullInputs, XRefOutputs : array of double;
    XRefAgent : TPhilosopher;


  begin
  FCancel := false;
  FCurrentTimePoint := 0;
  //******
  SetLength(XArInputs, 4);
  //******
  SetLength(XArOutputs, 5);
  //************
  SetLength(XFullInputs, 4);
  XRefAgent := TPhilosopher.CreatePar(RefNetwork, [-1, -1, -1, 0, 0]);
  XRefAgent.ResetNetwork;

  SetLength(XResults, FAgents.Count);
  for i := 0 to FAgents.Count - 1 do
    begin
    XResults[i] := TStringList.Create;
    GetAgent(i).resetNetwork;
    end;

  SetLength(XMetrics, 1);
  SetLength(XRefOutputs, 5);

  while (FCurrentTimePoint < timeCount) and not FCancel do
    begin
    SetNewTimePoint;
    for i := 0 to FAgents.Count - 1 do
      begin
      if FGotLeft[i]      then XArInputs[0] := 1 else XArInputs[0] := -1;
      if FGotRight[i]     then XArInputs[1] := 1 else XArInputs[1] := -1;
      if (GetAgent(i).ExtraInfo[0] < 0) then
         begin
         XArInputs[2] := -1;
         XArInputs[3] := -1;
         GetAgent(i).ExtraInfo[0] := GetAgent(i).ExtraInfo[0] + 1;
         end
      else if (GetAgent(i).ExtraInfo[0] = 0) and (FEat[i]) then
         begin
         XArInputs[2] := -1;
         XArInputs[3] := 1;
         GetAgent(i).ExtraInfo[0] := -(random(5) + 1);
         end
      else if (GetAgent(i).ExtraInfo[0] = 0) and (not FEat[i]) then
         begin
         XArInputs[2] := 1;
         XArInputs[3] := -1;
         GetAgent(i).ExtraInfo[0] := (random(5) + 1);
         end
      else
         begin
         XArInputs[2] := -1;
         XArInputs[3] := -1;
         if FEat[i] then GetAgent(i).ExtraInfo[0] := GetAgent(i).ExtraInfo[0] - 1;
         end;

      //****Define if it will be specified
      //****if FTriggerThink[i] then XArInputs[3] := 1 else XArInputs[3] := -1;

      //****Define if it will be specified
      //****if FTriggerEat[i]   then XArInputs[2] := 1 else XArInputs[2] := -1;

      XExtraInf := GetAgent(i).InputData(XArInputs, XArOutputs, XResults[i]);

      if (i = SelAgent) then
         begin
         for j := GetAgent(i).GetInputCount - 1 downto 0 do
           XFullInputs[j] := XArInputs[j];

         XRefAgent.InputData(XFullInputs, XRefOutputs, XResults[i]);
         XResults[i].Add('');
         XTmp := 0;
         for j := 0 to XRefAgent.GetOutputCount - 1 do
           Xtmp := XTmp + sqr(XArOutputs[j] - XRefOutputs[j]);
         XMetrics[0] := sqrt(XTmp);
         GetAgent(i).Supervise(XRefOutputs);
         FPickLeft [i] := (XRefOutputs[0] > 0);
         FPickRight[i] := (XRefOutputs[1] > 0);
         FDropLeft [i] := (XRefOutputs[2] > 0);
         FDropRight[i] := (XRefOutputs[3] > 0);
         FEat[i]       := (XRefOutputs[4] > 0);
         end
      else
         begin
         FPickLeft [i] := (XArOutputs[0] > 0);
         FPickRight[i] := (XArOutputs[1] > 0);
         FDropLeft [i] := (XArOutputs[2] > 0);
         FDropRight[i] := (XArOutputs[3] > 0);
         FEat[i]       := (XArOutputs[4] > 0);

         end;
      end;


    FCurrentTimePoint := FCurrentTimePoint + 1;
    FStepEvent(FCurrentTimePoint, 1, XMetrics);
    Application.ProcessMessages;
    end;

  XResults[SelAgent].Insert(0, '*??* ' + inttostr((XRefAgent.GetInputCount + XRefAgent.GetOutputCount) * 2) + ' 1 1');
  XResults[SelAgent].Insert(1, '');
  XResults[SelAgent].Insert(2, '*!!* ' + inttostr(FCurrentTimePoint));
  XResults[SelAgent].Insert(3, '');
  XResults[SelAgent].SaveToFile(FileName);

  for i := 0 to FAgents.Count - 1 do
    XResults[i].Free;

  XRefAgent.Free;
  result := not FCancel;
  end;

//------------------------------------------------------------------------------------------------

function TDiningTable.BlackBoxOnline(timeCount: integer; RefNetwork, FileName: TFileName; LearnType : integer;
                                     SelAgent : integer = 0): boolean;
  var
    i, j, XExtraInf : integer;
    XTmp : double;
    XArInputs, XArOutputs : array of double;
    XResults : array of TStringList;
    XMetrics, XFullInputs, XRefOutputs : array of double;
    XRefAgent : TPhilosopher;

    XOutPickLeft, XOutPickRight : double;
    s : string;


  begin
  randomize;
  FCancel := false;
  FCurrentTimePoint := 0;
  //******
  SetLength(XArInputs, 4);
  //******
  SetLength(XArOutputs, 5);
  //************
  SetLength(XFullInputs, 4);
  XRefAgent := TPhilosopher.CreatePar(RefNetwork, [-1, -1, -1, 0, 0]);
  XRefAgent.ResetNetwork;
  SetLength(XResults, FAgents.Count);
  for i := 0 to FAgents.Count - 1 do
    begin
    XResults[i] := TStringList.Create;
    GetAgent(i).resetNetwork;
    end;
  if LearnType < 2 then
     SetLength(XMetrics, 1)
  else
     begin
     SetLength(XMetrics, 2);
     XMetrics[0] := -1;
     XMetrics[1] := -1;
     end;


  SetLength(XRefOutputs, 5);

  XOutPickLeft  := -1;
  XOutPickRight := 1;

  while (FCurrentTimePoint < timeCount) and not FCancel do
    begin
    SetNewTimePoint;
    for i := 0 to FAgents.Count - 1 do
      begin

      if FGotLeft[i]      then XArinputs[0] := 1 else XArinputs[0] := -1;
      if FGotRight[i]     then XArinputs[1] := 1 else XArinputs[1] := -1;

      if FTriggerEat[i]   then XArinputs[2] := 1 else XArinputs[2] := -1;
      if FTriggerThink[i] then XArinputs[3] := 1 else XArinputs[3] := -1;



      XExtraInf := GetAgent(i).InputData(XArInputs, XArOutputs, XResults[i]);

      if (i = SelAgent) then
         begin
         if LearnType = 0 then
            XRefAgent.InputData(XArInputs, XRefOutputs, XResults[i])
         else if LearnType < 4 then
            begin
            if FShouldDropLeft [i] then XRefOutputs[2] := 1 else XRefOutputs[2] := -1;
            if FShouldDropRight[i] then XRefOutputs[3] := 1 else XRefOutputs[3] := -1;
            if FShouldEat[i]       then XRefOutputs[4] := 1 else XRefOutputs[4] := -1;
            end
         else
            begin
            XRefOutputs[0] := XArOutputs[0];
            XRefOutputs[1] := XArOutputs[1];
            XRefOutputs[2] := XArOutputs[2];
            end;
         case LearnType of
            1 :
              begin
              if FShouldPickLeft [i] then XRefOutputs[0] := 1 else XRefOutputs[0] := -1;
              if FShouldPickRight[i] then XRefOutputs[1] := 1 else XRefOutputs[1] := -1;
              end;
            2,5 :
              begin
              if FShouldPickRight[i] then XRefOutputs[4] := 1 else XRefOutputs[4] := -1;
              if FTriggerEat[i] then
                 begin
                 if XOutPickLeft <= 0 then XOutPickLeft := 0.2 else XOutPickLeft := XOutPickLeft + 0.2;
                 if XOutPickLeft >= 1 then XOutPickLeft := 1;
                 end
              else
                 begin
                 if XOutPickLeft > 0 then
                    begin
                    if XArOutputs[3] > 0.5 then
                       XOutPickLeft := -1
                    else
                       begin
                       XOutPickLeft := XOutPickLeft + 0.2;
                       if XOutPickLeft >= 1 then XOutPickLeft := 1;
                       end;
                    end;
                 end;
              if XOutPickLeft > XArOutputs[3]  then XRefOutputs[3] := XOutPickLeft;
              end;
            3,6 :
              begin
              XRefOutputs[4] := XArOutputs[4];
              if FTriggerEat[i] then
                 begin
                 if XOutPickLeft <= 0 then XOutPickLeft := 0.2 else XOutPickLeft := XOutPickLeft + 0.2;
                 if XOutPickLeft >= 1 then XOutPickLeft := 1;
                 end
              else
                 begin
                 if XOutPickLeft > 0 then
                    begin
                    if XArOutputs[3] > 0.5 then
                       XOutPickLeft := -1
                    else
                       begin
                       XOutPickLeft := XOutPickLeft + 0.2;
                       if XOutPickLeft >= 1 then XOutPickLeft := 1;
                       end;
                    end;
                 end;
              if XOutPickLeft > XArOutputs[3]  then XRefOutputs[3] := XOutPickLeft;
              end;
            4,7 :
              begin
              if FTriggerEat[i] then
                 begin
                 if XOutPickLeft <= 0 then XOutPickLeft := 0.2 else XOutPickLeft := XOutPickLeft + 0.2;
                 if XOutPickRight <= 0 then XOutPickRight := 0.2 else XOutPickRight := XOutPickLeft + 0.2;
                 if XOutPickLeft >= 1 then XOutPickLeft := 1;
                 if XOutPickRight >= 1 then XOutPickRight := 1;
                 end
              else
                 begin
                 if XOutPickLeft > 0 then
                    begin
                    if XArOutputs[3] > 0.5 then
                       XOutPickLeft := -1
                    else
                       begin
                       XOutPickLeft := XOutPickLeft + 0.2;
                       if XOutPickLeft >= 1 then XOutPickLeft := 1;
                       end;
                    end;

                 if XOutPickRight > 0 then
                    begin
                    if XArOutputs[4] > 0.5 then
                       XOutPickRight := -1
                    else
                       begin
                       XOutPickRight := XOutPickRight + 0.2;
                       if XOutPickRight >= 1 then XOutPickRight := 1;
                       end;
                    end;
                 end;

              //Represent Diamond Pickfork
              if XOutPickLeft > XArOutputs[3]  then XRefOutputs[3] := XOutPickLeft;
              if XOutPickRight > XArOutputs[4] then XRefOutputs[4] := XOutPickRight;
              end;

            end;


         GetAgent(i).Supervise(XRefOutputs);

         s := floattoStr(XRefOutputs[0]);
         for j := 1 to 4 do
           s := s + ' ' +floattoStr(XRefOutputs[j]);
         XResults[i].Add(s);
         XResults[i].Add('');

         if LearnType < 2 then
            begin
            XTmp := 0;
            for j := 0 to XRefAgent.GetOutputCount - 1 do
              Xtmp := XTmp + sqr(XArOutputs[j] - XRefOutputs[j]);
            XMetrics[0] := sqrt(XTmp /  XRefAgent.GetOutputCount);
            end
         else
            begin
            if (XMetrics[0] < 0) and (FTriggerEat[i]) then
               XMetrics[0] := 0
            else if not FTriggerEat[i] and not
            ((FForks[(i + FAgents.Count - 1) mod FAgents.Count] = 1) and (FForks[i] = -1)) then
               XMetrics[0] := XMetrics[0] + 1
            else
               XMetrics[0] := -1;

            if (XMetrics[1] < 0) and (FTriggerEat[i]) then
               XMetrics[1] := 0
            else if not FTriggerEat[i] then
               begin
               if ((FForks[(i + FAgents.Count - 1) mod FAgents.Count] = 1) and (FForks[i] = -1)) then
                  XMetrics[1] := -1
               else if (XArOutputs[3] > 0) or (XArOutputs[4] > 0) then
                  XMetrics[1] := XMetrics[1] + 1;
               end;
            end;

         if LearnType = 0 then
            for j := 0 to XRefAgent.GetOutputCount - 1 do
               XArOutputs[j] := XRefOutputs[j];
         end;

      FPickLeft [i] := (XArOutputs[0] > 0);
      FPickRight[i] := (XArOutputs[1] > 0);
      FDropLeft [i] := (XArOutputs[2] > 0);
      FDropRight[i] := (XArOutputs[3] > 0);
      FEat[i]       := (XArOutputs[4] > 0);
      end;

    FCurrentTimePoint := FCurrentTimePoint + 1;
    if LearnType < 2 then
       FStepEvent(FCurrentTimePoint, 1, XMetrics)
    else
       FStepEvent(FCurrentTimePoint, 2, XMetrics);
    ResetIfDeadlock;
    Application.ProcessMessages;
    end;

  XResults[SelAgent].Insert(0, '*??* ' + inttostr((XRefAgent.GetInputCount + XRefAgent.GetOutputCount) * 2)
                            + ' 1 1');
  XResults[SelAgent].Insert(1, '');
  XResults[SelAgent].Insert(2, '*!!* ' + inttostr(FCurrentTimePoint));
  XResults[SelAgent].Insert(3, '');
  XResults[SelAgent].SaveToFile(FileName);
  for i := 0 to FAgents.Count - 1 do
    XResults[i].Free;
  XRefAgent.Free;
  result := not FCancel;
  end;

//------------------------------------------------------------------------------------------------

procedure TDiningTable.CancelExperiment;
  begin
  FCancel := true;
  end;

//------------------------------------------------------------------------------------------------

constructor TDiningTable.Create(numberOfAgents: integer; Files : TStrings;
  KnowledgeLevel: array of integer);
  var
    i : integer;
    Ag : TPhilosopher;
  begin
  randomize;
  FAgents := TStringList.Create;
  for i := 0 to numberOfAgents - 1 do
    begin
//****redefine later
    Ag := TPhilosopher.CreatePar(Files.Strings[KnowledgeLevel[i]], [-1, -1, -1, 0, 0]);
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
    FStomach[i] := - (random(5) + 1);

  FStepEvent := DefaultStepEvent;
  end;

//------------------------------------------------------------------------------------------------

constructor TDiningTable.CreatePar(numberOfAgents: integer; Files: TStrings;
        KnowledgeLevel: array of integer; leftProb: double; allocation: TAllocationPolicy);
  begin
  Create(numberOfAgents, files, KnowledgeLevel);
  FProbToLeft := leftProb;
  FAllocationPolicy := allocation;
  end;

//------------------------------------------------------------------------------------------------

procedure TDiningTable.DefaultStepEvent(TimePoint, MetricsLength: integer; Metrics: array of double);
  begin
  end;

//------------------------------------------------------------------------------------------------

destructor TDiningTable.Destroy;
  var i : integer;
  begin
  for i := 0 to FAgents.Count - 1 do
    FAgents.Objects[i].Free;
  FAgents.Free;
  inherited;
  end;

//------------------------------------------------------------------------------------------------

procedure TDiningTable.Execute(Offline, Correct : boolean);
  var
    i, j : integer;
    arinputs : array[0..3] of double;
    aroutputs1, aroutputs2 : array[0..4] of double;
  begin
  for i := 0 to FAgents.Count - 1 do
    begin
    if FGotLeft[i]      then arinputs[0] := 1 else arinputs[0] := -1;
    if FGotRight[i]     then arinputs[1] := 1 else arinputs[1] := -1;
    if FTriggerEat[i]   then arinputs[2] := 1 else arinputs[2] := -1;
    if FTriggerThink[i] then arinputs[3] := 1 else arinputs[3] := -1;

    (FAgents.Objects[i] as TPhilosopher).InputData(arinputs, aroutputs1, nil);

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

//    if correct then
//       (FAgents.Objects[i] as TPhilosopher).Correct(aroutputs2);
    Ferrors[i] := 0;
    for j := 0 to 4 do
      Ferrors[i] :=  Ferrors[i] + sqr(Aroutputs2[j] - ArOutputs1[j]);
    FErrors[i] := sqrt(FErrors[i] / 5);
    end;
  end;

//------------------------------------------------------------------------------------------------

function TDiningTable.GetAgent(index: integer): TPhilosopher;
  begin
  if (index >= 0) and (index < FAgents.Count) and (FAgents.Objects[index] is TPhilosopher) then
     result := FAgents.Objects[index] as TPhilosopher
  else
     result := nil;
  end;

//------------------------------------------------------------------------------------------------

function TDiningTable.GetCurrentTimePoint: integer;
  begin
  result := FCurrentTimePoint;
  end;

//------------------------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------------------------

function TDiningTable.GetStepEvent: TDiningTableStepEvent;
  begin
  result := FStepEvent;
  end;

//------------------------------------------------------------------------------------------------

function TDiningTable.IndividualEvolution(timeCount: integer; FileName: TFileName; SelAgent : integer = 0): boolean;
  var
    i, j, k, XTmp, XExtraInf : integer;
    XArInputs, XArOutputs : array of double;
    XResults : array of TStringList;
    s : string;
    XInMatches , XOutMatches, XIsDiamond: boolean;
    XMetrics, XFullInputs : array of double;

  begin
  FCancel := false;
  FCurrentTimePoint := 0;
  //******
  SetLength(XArInputs, 4);
  //******
  SetLength(XArOutputs, 5);

  SetLength(XResults, FAgents.Count);
  for i := 0 to FAgents.Count - 1 do
    begin
    XResults[i] := TStringList.Create;
    GetAgent(i).resetNetwork;
    end;

  SetLength(XMetrics, GetAgent(SelAgent).GetPropertyCount);
  SetLength(XFullInputs, GetAgent(SelAgent).GetInputCount);

  while (FCurrentTimePoint < timeCount) and not FCancel do
    begin
    SetNewTimePoint;
    for i := 0 to FAgents.Count - 1 do
      begin
      if FGotLeft[i]      then XArInputs[0] := 1 else XArInputs[0] := -1;
      if FGotRight[i]     then XArInputs[1] := 1 else XArInputs[1] := -1;

      if (GetAgent(i).ExtraInfo[0] < 0) then
         begin
         XArInputs[2] := -1;
         XArInputs[3] := -1;
         GetAgent(i).ExtraInfo[0] := GetAgent(i).ExtraInfo[0] + 1;
         end
      else if (GetAgent(i).ExtraInfo[0] = 0) and (FEat[i]) then
         begin
         XArInputs[2] := 1;
         XArInputs[3] := -1;
         GetAgent(i).ExtraInfo[0] := -random(5);
         end
      else if (GetAgent(i).ExtraInfo[0] = 0) and (not FEat[i]) then
         begin
         XArInputs[2] := -1;
         XArInputs[3] := 1;
         GetAgent(i).ExtraInfo[0] := random(5);
         end
      else
         begin
         XArInputs[2] := -1;
         XArInputs[3] := -1;
         if FEat[i] then GetAgent(i).ExtraInfo[0] := GetAgent(i).ExtraInfo[0] - 1;
         end;

      XExtraInf := GetAgent(i).InputData(XArInputs, XArOutputs, XResults[i]);

      if (i = SelAgent) then
        with GetAgent(i) do
          begin
          for j := GetInputCount - 1 downto 0 do
            if j > 1 then
               begin
               XFullInputs[j] := ((XExtraInf mod 2) * 2) - 1;
               XExtraInf := XExtraInf div 2;
               end
            else
               XFullInputs[j] := XArInputs[j];

          for k := 0 to GetPropertyCount - 1 do
            begin
            j := 0;
            XInMatches := true;
            XOutMatches := true;
            XIsDiamond := true;
            while XInMatches and (j < GetInputCount) do
              begin
              XInMatches := (SpecProperty[k, j] = 0);
              XInMatches := XInMatches or (sign(XFullInputs[j]) = sign(SpecProperty[k, j]));
              j := j + 1;
              end;
            j := 0;
            while (j < GetOutputCount) do
              begin
              XTmp := SpecProperty[k, j + GetInputCount];
              XOutMatches := XOutMatches and ((XTmp = 0) or (sign(XArOutputs[j]) = sign(XTmp)));
              XIsDiamond := XIsDiamond and (abs(XTmp) < 2);
              j := j + 1;
              end;
            if not XIsDiamond then
               begin
               if not XInMatches  then
                  XMetrics[i] := 0
               else if XOutMatches then
                  XMetrics[k] := 0
               else
                  XMetrics[k] := 1
               end
            else
               begin
               if XOutMatches then XMetrics[k] := 0
               else if (XMetrics[k] > 0) then XMetrics[k] := XMetrics[k] + 1
               else if not XinMatches then XMetrics[k] := 1
               else XMetrics[k] := 0;
               end;
            end;
          end;

      s:= XResults[i].Strings[FCurrentTimePoint];
      s := s + ' ' + inttoStr(FForks[i]) + ' ';
      s := s + ' ' + inttoStr(- FForks[(i + 1) mod FAgents.Count]);
      XResults[i].Strings[FCurrentTimePoint] := s;

      FEat[i]       := (XArOutputs[0] > 0);
      FDropLeft [i] := (XArOutputs[1] > 0);
      FDropRight[i] := (XArOutputs[2] > 0);
      FPickLeft [i] := (XArOutputs[3] > 0);
      FPickRight[i] := (XArOutputs[4] > 0);

      GetAgent(i).SuperviseByProperty;
      ResetIfDeadlock;
      end;

    FCurrentTimePoint := FCurrentTimePoint + 1;
    FStepEvent(FCurrentTimePoint, GetAgent(SelAgent).GetPropertyCount, XMetrics);
    Application.ProcessMessages;
    end;

  XResults[0].Insert(0, '*??* 11 ' + inttostr(FAgents.Count) + ' 1');
  XResults[0].Insert(1, '');
  XResults[0].Insert(2, '*!!* ' + inttostr(FCurrentTimePoint));
  XResults[0].Insert(3, '');
  for i := 1 to FAgents.Count - 1 do
    begin
    XResults[0].Add('');
    XResults[0].Add('*!!* ' + inttostr(FCurrentTimePoint));
    XResults[0].Add('');
    XResults[0].AddStrings(XResults[i]);
    XResults[i].Free;
    end;
  XResults[0].SaveToFile(FileName);
  XResults[0].Free;
  result := not FCancel;
  end;

//------------------------------------------------------------------------------------------------

function TDiningTable.OverallEvolution(timeCount: integer;  FileName: TFileName): boolean;
{  var
    i, j, XExtraInf : integer;
    XTmp : double;
    XArInputs, XArOutputs : array of double;
    XResults : array of TStringList;
    XMetrics, XFullInputs, XRefOutputs : array of double;
    XRefAgent : TPhilosopher;

    XOutPickLeft, XOutPickRight : double;
    s : string;

 }
  begin
  {randomize;
  FCancel := false;
  FCurrentTimePoint := 0;
  //******
  SetLength(XArInputs, 4);
  //******
  SetLength(XArOutputs, 5);
  //************
  SetLength(XFullInputs, 4);
  XRefAgent := TPhilosopher.Create(RefNetwork, [-1, -1, -1, 0, 0]);
  XRefAgent.ResetNetwork;
  SetLength(XResults, FAgents.Count);
  for i := 0 to FAgents.Count - 1 do
    begin
    XResults[i] := TStringList.Create;
    GetAgent(i).resetNetwork;
    end;
  if LearnType < 2 then
     SetLength(XMetrics, 1)
  else
     begin
     SetLength(XMetrics, 2);
     XMetrics[0] := -1;
     XMetrics[1] := -1;
     end;


  SetLength(XRefOutputs, 5);

  XOutPickLeft  := -1;
  XOutPickRight := 1;

  while (FCurrentTimePoint < timeCount) and not FCancel do
    begin
    SetNewTimePoint;
    for i := 0 to FAgents.Count - 1 do
      begin

      if FGotLeft[i]      then XArinputs[0] := 1 else XArinputs[0] := -1;
      if FGotRight[i]     then XArinputs[1] := 1 else XArinputs[1] := -1;

      if FTriggerThink[i] then XArinputs[2] := 1 else XArinputs[2] := -1;
      if FTriggerEat[i]   then XArinputs[3] := 1 else XArinputs[3] := -1;


      XExtraInf := GetAgent(i).InputData(XArInputs, XArOutputs, XResults[i]);

      if (i = SelAgent) then
         begin
         //

         if LearnType = 0 then
            XRefAgent.InputData(XArInputs, XRefOutputs, XResults[i])
         else if LearnType < 4 then
            begin
            if FShouldEat[i]       then XRefOutputs[0] := 1 else XRefOutputs[0] := -1;
            if FShouldDropLeft [i] then XRefOutputs[1] := 1 else XRefOutputs[1] := -1;
            if FShouldDropRight[i] then XRefOutputs[2] := 1 else XRefOutputs[2] := -1;
            end
         else
            begin
            XRefOutputs[0] := XArOutputs[0];
            XRefOutputs[1] := XArOutputs[1];
            XRefOutputs[2] := XArOutputs[2];
            end;
         case LearnType of
            1 :
              begin
              if FShouldPickLeft [i] then XRefOutputs[3] := 1 else XRefOutputs[3] := -1;
              if FShouldPickRight[i] then XRefOutputs[4] := 1 else XRefOutputs[4] := -1;
              end;
            2,5 :
              begin
              if FShouldPickRight[i] then XRefOutputs[4] := 1 else XRefOutputs[4] := -1;
              if FTriggerEat[i] then
                 begin
                 if XOutPickLeft <= 0 then XOutPickLeft := 0.2 else XOutPickLeft := XOutPickLeft + 0.2;
                 if XOutPickLeft >= 1 then XOutPickLeft := 1;
                 end
              else
                 begin
                 if XOutPickLeft > 0 then
                    begin
                    if XArOutputs[3] > 0.5 then
                       XOutPickLeft := -1
                    else
                       begin
                       XOutPickLeft := XOutPickLeft + 0.2;
                       if XOutPickLeft >= 1 then XOutPickLeft := 1;
                       end;
                    end;
                 end;
              if XOutPickLeft > XArOutputs[3]  then XRefOutputs[3] := XOutPickLeft;
              end;
            3,6 :
              begin
              XRefOutputs[4] := XArOutputs[4];
              if FTriggerEat[i] then
                 begin
                 if XOutPickLeft <= 0 then XOutPickLeft := 0.2 else XOutPickLeft := XOutPickLeft + 0.2;
                 if XOutPickLeft >= 1 then XOutPickLeft := 1;
                 end
              else
                 begin
                 if XOutPickLeft > 0 then
                    begin
                    if XArOutputs[3] > 0.5 then
                       XOutPickLeft := -1
                    else
                       begin
                       XOutPickLeft := XOutPickLeft + 0.2;
                       if XOutPickLeft >= 1 then XOutPickLeft := 1;
                       end;
                    end;
                 end;
              if XOutPickLeft > XArOutputs[3]  then XRefOutputs[3] := XOutPickLeft;
              end;
            4,7 :
              begin
              if FTriggerEat[i] then
                 begin
                 if XOutPickLeft <= 0 then XOutPickLeft := 0.2 else XOutPickLeft := XOutPickLeft + 0.2;
                 if XOutPickRight <= 0 then XOutPickRight := 0.2 else XOutPickRight := XOutPickLeft + 0.2;
                 if XOutPickLeft >= 1 then XOutPickLeft := 1;
                 if XOutPickRight >= 1 then XOutPickRight := 1;
                 end
              else
                 begin
                 if XOutPickLeft > 0 then
                    begin
                    if XArOutputs[3] > 0.5 then
                       XOutPickLeft := -1
                    else
                       begin
                       XOutPickLeft := XOutPickLeft + 0.2;
                       if XOutPickLeft >= 1 then XOutPickLeft := 1;
                       end;
                    end;

                 if XOutPickRight > 0 then
                    begin
                    if XArOutputs[4] > 0.5 then
                       XOutPickRight := -1
                    else
                       begin
                       XOutPickRight := XOutPickRight + 0.2;
                       if XOutPickRight >= 1 then XOutPickRight := 1;
                       end;
                    end;
                 end;

              //Represent Diamond Pickfork
              if XOutPickLeft > XArOutputs[3]  then XRefOutputs[3] := XOutPickLeft;
              if XOutPickRight > XArOutputs[4] then XRefOutputs[4] := XOutPickRight;
              end;

            end;


         GetAgent(i).Supervise(XRefOutputs);

         s := floattoStr(XRefOutputs[0]);
         for j := 1 to 4 do
           s := s + ' ' +floattoStr(XRefOutputs[j]);
         XResults[i].Add(s);
         XResults[i].Add('');

         if LearnType < 2 then
            begin
            XTmp := 0;
            for j := 0 to XRefAgent.GetOutputCount - 1 do
              Xtmp := XTmp + sqr(XArOutputs[j] - XRefOutputs[j]);
            XMetrics[0] := sqrt(XTmp /  XRefAgent.GetOutputCount);
            end
         else
            begin
            if (XMetrics[0] < 0) and (FTriggerEat[i]) then
               XMetrics[0] := 0
            else if not FTriggerEat[i] and not
            ((FForks[(i + FAgents.Count - 1) mod FAgents.Count] = 1) and (FForks[i] = -1)) then
               XMetrics[0] := XMetrics[0] + 1
            else
               XMetrics[0] := -1;

            if (XMetrics[1] < 0) and (FTriggerEat[i]) then
               XMetrics[1] := 0
            else if not FTriggerEat[i] then
               begin
               if ((FForks[(i + FAgents.Count - 1) mod FAgents.Count] = 1) and (FForks[i] = -1)) then
                  XMetrics[1] := -1
               else if (XArOutputs[3] > 0) or (XArOutputs[4] > 0) then
                  XMetrics[1] := XMetrics[1] + 1;
               end;
            end;

         if LearnType = 0 then
            for j := 0 to XRefAgent.GetOutputCount - 1 do
               XArOutputs[j] := XRefOutputs[j];
         end;

      FEat[i]       := (XArOutputs[0] > 0);
      FDropLeft [i] := (XArOutputs[1] > 0);
      FDropRight[i] := (XArOutputs[2] > 0);
      FPickLeft [i] := (XArOutputs[3] > 0);
      FPickRight[i] := (XArOutputs[4] > 0);
      end;

    FCurrentTimePoint := FCurrentTimePoint + 1;
    if LearnType < 2 then
       FStepEvent(FCurrentTimePoint, 1, XMetrics)
    else
       FStepEvent(FCurrentTimePoint, 2, XMetrics);
    ResetIfDeadlock;
    Application.ProcessMessages;
    end;

  XResults[SelAgent].Insert(0, '*??* ' + inttostr((XRefAgent.GetInputCount + XRefAgent.GetOutputCount) * 2)
                            + ' 1 1');
  XResults[SelAgent].Insert(1, '');
  XResults[SelAgent].Insert(2, '*!!* ' + inttostr(FCurrentTimePoint));
  XResults[SelAgent].Insert(3, '');
  XResults[SelAgent].SaveToFile(FileName);
  for i := 0 to FAgents.Count - 1 do
    XResults[i].Free;
  XRefAgent.Free;
  result := not FCancel;}
  end;


//------------------------------------------------------------------------------------------------

procedure TDiningTable.ResetIfDeadlock;
  var
    i, tmp : integer;
    b : boolean;
  begin
  i := 1;
  tmp := FForks[0];
  b := tmp <> 0;
  while b and (i < FAgents.Count) do
    begin
    b := (FForks[i] = tmp);
    i := i + 1;
    end;
  if b then
    for i := 0 to FAgents.Count - 1 do
      GetAgent(i).ResetNetwork(false);
  end;

//------------------------------------------------------------------------------------------------

procedure TDiningTable.SetNewTimePoint;
   var
     i, j : integer;
     XLeft, XRight : integer;
     DeadLock : boolean;
     XExit : boolean;
   begin
   //Verify which forks are allocated
   for i := 0 to FAgents.Count - 1 do
     begin
     XLeft  := (i + FAgents.Count - 1) mod FAgents.Count;
     XRight := i;
     FGotRight[XLeft] := false;
     FGotLeft[XRight] := false;
     if FForks[i] = 0 then
        begin
        if FPickRight[Xleft] and FPickLeft[XRight] then
           begin
           if random < FProbToLeft then
              begin
              FGotRight[XLeft] := true;
              FForks[i] := -1;
              end
           else
              begin
              FGotLeft[XRight] := true;
              FForks[i] := 1;
              end;
           end
        else if FPickRight[Xleft] then
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

   if FAllocationPolicy = ApOneFork then
      begin
      i := random(FAgents.Count);
      XExit := false;
      while not Xexit do
        begin
        XLeft  := (i + FAgents.Count - 1) mod FAgents.Count;
        XRight := i;
        if FGotRight[XLeft] then
           begin
           for j := 0 to FAgents.Count do
             begin
             if j <> XLeft then
                if FGotRight[j] then
                   begin
                   FGotRight[j] := false;
                   FForks[(j + 1) mod FAgents.Count] := 0;
                   end;
             if FGotLeft[j] then
                begin
                FGotLeft[j] := false;
                FForks[j] := 0;
                end;
             end;
           XExit := true;
           end
        else if FGotLeft[XRight] then
           begin
           for j := 0 to FAgents.Count do
             begin
             if j <> XRight then
                if FGotLeft[j] then
                   begin
                   FGotLeft[j] := false;
                   FForks[j] := 0;
                   end;
             if FGotRight[j] then
                begin
                FGotRight[j] := false;
                FForks[(j + 1) mod FAgents.Count] := 0;
                end;
             end;
           XExit := true;
           end;
        i := (i + FAgents.Count - 1) mod FAgents.Count;
        end;
      end

   else if FAllocationPolicy = ApAvoidDeadLock then
      begin
      deadlock := true;
      for i := 0 to FAgents.Count - 1 do
        deadlock := deadlock and (FGotRight[i] or (FForks[(i + 1) mod FAgents.Count] < 0));
      if deadlock then
         begin
         i := random(FAgents.Count);
         while not FGotRight[i] do
           i := (i + 1) mod FAgents.count;
         FGotRight[i] := false;
         FForks[(i + 1) mod FAgents.Count] := 0;
         end;
      deadlock := true;
      for i := 0 to FAgents.Count - 1 do
        deadlock := deadlock and (FGotLeft[i] or (Fforks[i] > 0));
      if deadlock then
         begin
         i := random(FAgents.Count);
         while not FGotLeft[i] do
           i := (i + 1) mod FAgents.count;
         FGotLeft[i] := false;
         FForks[i] := 0;
         end;
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
           FStomach[i] :=  -(random(5) + 1);
           FTriggerThink[i] := true;
           end;
        end
     else if (FStomach[i] < 0) then
        begin
        FStomach[i] := FStomach[i] + 1;
        if FStomach[i] = 0 then
           begin
           FStomach[i] :=  random(5) + 1;
           FTriggerEat[i] := true;
           end;
        end;
     end;

   //Corrige para ver qual rdeverá ser a próxima ação de cada um
   for i := 0 to FAgents.Count - 1 do
     begin
     XLeft  := i;
     XRight := (i + 1) mod FAgents.Count;
     FShouldPickLeft [i] := (FForks[XLeft] <> 1) and (FStomach[i] > 0);
     FShouldPickRight[i] := (FForks[XLeft] = 1) and (FForks[XRight] <> -1) and (FStomach[i] > 0);
     FShouldDropLeft [i] := (FForks[XLeft] = 1) and (FStomach[i] <= 0);
     FShouldDropRight[i] := (FForks[XRight] = -1) and ((FStomach[i] <= 0) or (FForks[XLeft] <> 1));
     FShouldEat[i]       := (FForks[XLeft] = 1) and (FForks[XRight] = -1) and (FStomach[i] > 0);
     end;
   end;

//------------------------------------------------------------------------------------------------

procedure TDiningTable.SetStepEvent(const Value: TDiningTableStepEvent);
  begin
  FStepEvent := value;
  end;

//------------------------------------------------------------------------------------------------

end.
