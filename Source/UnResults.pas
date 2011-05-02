unit UnResults;

interface

uses SysUtils, Classes, Chart, series;

//-------------------------------------------------------------------------------------------------

type TResultGrouping = (RgSingle, RgDiff);

type TResultTransf = (RtSimple, RtMean, RtMin, RtMax, RtMeanSq, RtRootMeanSq,
                      RtStdDev, RtRightCount, RtSumCount);

//-------------------------------------------------------------------------------------------------

type TResultGroup = class
  private
    FTotalCols    : integer;
    FColMarks     : array of boolean;
    FGroupKind    : TResultGrouping;
    FTransf       : TResultTransf;
    FSelData      : double;
    FMaxDiff      : double;

    FVSSum        : double;
    FVSSquaredSum : double;
    FVSCount      : integer;
    FVSRightCount : integer;
    FVSMax        : double;
    FVSMin        : double;

    FColAuxValues : array of integer;
    function GetColMarks(index: integer): boolean;
    function GetGroupKind: TResultGrouping;
    function GetSelData: double;
    function GetTotalColCount: integer;
    function GetTransf: TResultTransf;
    procedure SetColMarks(index: integer; const Value: boolean);
    procedure SetGroupKind(const Value: TResultGrouping);
    procedure SetSelData(const Value: double);
    procedure SetTransf(const Value: TResultTransf);
    function GetMaxDiff: double;
    procedure SetMaxDiff(const Value: double);
    procedure SetTotalColCount(const Value: integer);
    function GetColAuxValue(index: integer): integer;
    procedure SetColAuxValue(index: integer; const Value: integer);
    function GetVariableSize : integer;

  public
    constructor Create(colCount : integer);
    destructor  Destroy; override;
    property TotalColCount : integer read GetTotalColCount write SetTotalColCount;
    property ColMarks[index : integer] : boolean read GetColMarks write SetColMarks;
    property ColAuxValue[index : integer] : integer read GetColAuxValue write SetColAuxValue;
    property GroupKind : TResultGrouping read GetGroupKind write SetGroupKind;
    property Transf : TResultTransf read GetTransf write SetTransf;
    property SelData : double read GetSelData write SetSelData;
    property MaxDiff : double read GetMaxDiff write SetMaxDiff;

    function  AddVarSizeValue(value : double; OnlyLast : boolean = false) : integer;
    procedure ClearVarSizeValues;
    function  GetOutVarSizeValue : double;

    function GetOutValue(values : array of double; IsGiven : array of boolean; size : integer;
             out OutValue : double) : boolean;

    procedure SetAllMarks(value : boolean);

    function LoadFromString(data : string) : boolean;
    function LoadMarksFromString(data : string) : boolean;

    property VariableSize : integer read GetVariableSize;

  end;

//-------------------------------------------------------------------------------------------------

type THekatonResults = class
  private
    FInputFileName : TFileName;
    FColumns : TStringList;
    FGroups  : TStringList;
//    FMarks   : array of array of boolean;

    FSubsetCount : integer;
    FSubSetMarks : array of boolean;

//    FUseLabels     : boolean;
    FConsiderEmptyLine : boolean;
    FSequencesSize : integer;
    FMaxSequenceSize : integer;
    FSequenceSeparator : string;
    FUseSequenceSeparator : boolean;
    FNoDataMark : string;

    FGroupSequence : boolean;
    FSequenceMarks : array of boolean;
    FSequenceFunction : TResultTransf;
    FSequenceGrouping : TResultGrouping;

    FFileAsOutput  : boolean;
    FChartAsOutput : boolean;

    FChart : TChart;
    FOutputFileName : TFileName;
    FInputStrList  : TStringList;
    FOutputStrList  : TStringList;

    FMaxLinesPerSubset : integer;

    FLongGroups : array of array of TResultGroup;
    FLongLastMark : boolean;

//    FMode    : integer;

    procedure LoadHeaderFromFile;
    function GetInputFileName: TFileName;
    procedure SetInputFileName(const Value: TFileName);
    function GetColumns(index: integer): string;
    function GetGroupName(index: integer): string;
    function GetMarks(i, j: integer): boolean;
    procedure SetColumns(index: integer; const Value: string);
    procedure SetGroupName(index: integer; const Value: string);
    procedure SetMarks(i, j: integer; const Value: boolean);
    function GetColumnCount: integer;
    function GetGroupCount: integer;
    function GetSubsetCount: integer;
    function GetSubsetMarks(index: integer): boolean;
    procedure SetSubsetCount(const Value: integer);
    procedure SetSubsetMarks(index: integer; const Value: boolean);
    function GetConsiderEmptyLine: boolean;
    function GetGroupSequence: boolean;
    function GetSequenceSize: integer;
    procedure SetConsiderEmptyLine(const Value: boolean);
    procedure SetGroupSequence(const Value: boolean);
    procedure SetSequenceSize(const Value: integer);
    function GetSequenceFunction: TResultTransf;
    function GetSequenceGrouping: TResultGrouping;
    function GetSequenceMarks(index: integer): boolean;
    procedure SetSequenceFunction(const Value: TResultTransf);
    procedure SetSequenceGrouping(const Value: TResultGrouping);
    procedure SetSequenceMarks(index: integer; const Value: boolean);
    function GetGroup(index: integer): TResultGroup;
    procedure SetGroup(index: integer; const Value: TResultGroup);

    function GetLongLastMark : boolean;
    procedure SetLongLastMark (value : boolean);

    function GetMaxSequenceSize : integer;
    procedure SetMaxSequenceSize(value : integer);

    function GetSequenceSeparator : string;

    procedure EndSubsetAtOutput(subset, nOfLines : integer);
    function  EmptyOutputLine : string;
    procedure AddOutput(subset, LineNumber : integer; value : array of double; IsGiven : array of boolean);


    function ProcessLine(ArLine : array of double; ArIsGiven : array of boolean;
                         var OutputIndex : integer; subset, longPosition : integer) : integer;
    function GetLine(var ArLine : array of double; var ArIsGiven : array of boolean;
                     position, inputLine : integer; var IsSequenceBreak : boolean) : integer;
    function FindSubsetStart(position : integer; var numberOfLines : integer) : integer;

    function GetUseSequenceBreak : boolean;
    procedure SetUseSequenceBreak (value : boolean);


  public
    property InputFileName : TFileName read GetInputFileName write SetInputFileName;
    property Columns[index : integer] : string  read GetColumns write SetColumns;
    property Groups [index : integer] : TResultGroup  read GetGroup  write SetGroup;
    property GroupNames [index : integer] : string  read GetGroupName  write SetGroupName;
    property Marks  [i, j  : integer] : boolean read GetMarks   write SetMarks;
    property SubSetCount : integer read GetSubsetCount write SetSubsetCount;
    property SubSetMarks[index : integer] : boolean read GetSubsetMarks write SetSubsetMarks;
    property ColumnCount : integer read GetColumnCount;
    property GroupCount : integer read  GetGroupCount;
//    property UseLabel : boolean read GetUseLabel write SetUseLabel;
    property ConsiderEmptyLine : boolean read GetConsiderEmptyLine write SetConsiderEmptyLine;
    property SequenceSize  : integer read GetSequenceSize write SetSequenceSize;
    property GroupSequence : boolean read GetGroupSequence write SetGroupSequence;
    property SequenceMarks[index : integer] : boolean read GetSequenceMarks write SetSequenceMarks;
    property SequenceFunction : TResultTransf read GetSequenceFunction write SetSequenceFunction;
    property SequenceGrouping : TResultGrouping read GetSequenceGrouping write SetSequenceGrouping;
    property MaxSequenceSize : integer read GetMaxSequenceSize write SetMaxSequenceSize;
    property SequenceSeparator : string read GetSequenceSeparator;
    property LongLastMark : boolean read GetLongLastMark write SetLongLastMark;
    property UseSequenceBreak : boolean read GetUseSequenceBreak write SetUseSequenceBreak;

    function AddColumn(name : string) : integer;
    function AddGroup (name : string; gr : TResultGroup) : integer;
    procedure DeleteColumn(index : integer);
    procedure DeleteGroup(index : integer);

    constructor Create;
    destructor Destroy; override;

    procedure ResetOutputChoices;

    procedure SetupChartAsOutput(chart : TChart; owner : TComponent = nil);
    procedure SetupFileAsOutput(FileName : TFileName);

    procedure GenerateOutput;
//    procedure OldGenerateOutput;

    function ProcessScript(fileName : TFileName) : integer;

//    procedure ConcatenateSubsets(NewFile : TFileName);
    procedure ConcatenateFiles(Filelist : TStrings);

  end;

//-------------------------------------------------------------------------------------------------

implementation

uses dialogs, Math;

//-------------------------------------------------------------------------------------------------

{ TResultGroup }

//-------------------------------------------------------------------------------------------------

function TResultGroup.AddVarSizeValue(value: double; OnlyLast : boolean = false): integer;
  begin
  if OnlyLast then
     begin
     FVSMax := value;
     FVSMin := value;
     FVSSquaredSum := value * value;
     FVSSum := value;
     FVsCount := 1;
     if abs(value - FSelData) < FMaxDiff then
        FVSRightCount := 1
     else
        FVSRightCount := 0;
     result := 1;
     end
  else
     begin
     if FVscount = 0 then
        begin
        FVSMax := value;
        FVSMin := value;
        end
     else
        begin
        if value > FVSMax then FVSMax := value;
        if value < FVSMin then FVSMin := value;
        end;
     FVSSquaredSum := FVSSquaredSum + (value * value);
     FVSSum := FVSSum + value;
     FVSCount := FVsCount + 1;
     if abs(value - FSelData) < FMaxDiff then
        FVSRightCount := FVSRightCount + 1;
     result := FVSCount;
     end;
  end;

//-------------------------------------------------------------------------------------------------

procedure TResultGroup.ClearVarSizeValues;
  begin
  FVsMax := 0;
  FVSMin := 0;
  FVSSquaredSum := 0;
  FVSSum := 0;
  FVSCount := 0;
  FVSRightCount := 0;
  end;

//-------------------------------------------------------------------------------------------------

constructor TResultGroup.Create(colCount: integer);
  begin
  inherited Create;
  FTotalCols := colCount;
  SetLength(FColMarks, FTotalCols);
  SetLength(FColAuxValues, FTotalCols);
  SetAllMarks(false);
  FTransf := RtSimple;
  FGroupKind := RgSingle;
  FSelData := 0;
  FMaxDiff := 0;
  end;

//-------------------------------------------------------------------------------------------------

destructor TResultGroup.Destroy;
  begin
  inherited;
  end;

//-------------------------------------------------------------------------------------------------

function TResultGroup.GetColAuxValue(index: integer): integer;
  begin
  if (index >= 0) and (index < FTotalCols) then
     result := FColAuxValues[index]
  else
     result := 0;
  end;

//-------------------------------------------------------------------------------------------------

function TResultGroup.GetColMarks(index: integer): boolean;
  begin
  if (index >= 0) and (index < FTotalCols) then
     result := FColMarks[index]
  else
     result := false;
  end;

//-------------------------------------------------------------------------------------------------

function TResultGroup.GetGroupKind: TResultGrouping;
  begin
  result := FGroupKind
  end;

//-------------------------------------------------------------------------------------------------

function TResultGroup.GetMaxDiff: double;
  begin
  result := FMaxDiff;
  end;

//-------------------------------------------------------------------------------------------------

function TResultGroup.GetOutValue(values: array of double; IsGiven : array of boolean; size: integer;
                 out OutValue : double): boolean;
  var
    XActualValues : array of double;
    XActualCount  : integer;
    i, j : integer;
    XAfterFirst, XFirstIsGiven : boolean;
    Xtmp, XAvg : double;
  begin
  XactualCount := 0;
  XAfterFirst := false;
  XFirstIsGiven := true;
  OutValue := 0;
  if size >= FTotalCols then
     begin
     case FGroupKind of
       RgSingle :
         begin
         for i := 0 to FTotalCols - 1 do
           if FColMarks[i] and IsGiven[i] then
              begin
              XactualCount := XactualCount + 1;
              SetLength(XactualValues, XactualCount);
              XactualValues[XactualCount - 1] := values[i];
              end;
         end;
       RgDiff :
         begin
         for i := 0 to FTotalCols - 1 do
           if FColMarks[i] then
              if XAfterFirst then
                 begin
                 if XFirstIsGiven and IsGiven[i] then
                    XactualValues[XactualCount - 1] := XactualValues[XactualCount - 1] - values[i]
                 else
                    XActualCount := XActualCount - 1;
                 XAfterFirst := false;
                 end
              else
                 begin
                 if IsGiven[i] then
                    begin
                    XactualCount := XactualCount + 1;
                    SetLength(XactualValues, XactualCount);
                    XactualValues[XactualCount - 1]  := values[i];
                    XFirstIsGiven := true;
                    end
                 else
                    XFirstIsGiven := false;
                 XAfterFirst := true;
                 end;
         if XAfterFirst then
            XActualCount := XActualCount - 1;
         end;
       else
         XActualCount := 0;
       end;
     if XActualCount > 0 then
        case FTransf of
          RtSimple :
            begin
            Xtmp := 0;
            for i := 0 to XActualCount - 1 do
              Xtmp := Xtmp + XActualValues[i];
            outValue := XTmp;
            result := true;
            end;
          RtMean :
            begin
            Xtmp := 0;
            for i := 0 to XActualCount - 1 do
              Xtmp := Xtmp + XActualValues[i];
            outValue := XTmp / XActualCount;
            result := true;
            end;
          RtMin :
            begin
            XTmp := XActualValues[0];
            for i := 1 to XActualCount - 1 do
              if XActualValues[i] < Xtmp then
                 Xtmp := XActualValues[i];
            outValue := XTmp;
            result := true;
            end;
          RtMax :
            begin
            XTmp := XActualValues[0];
            for i := 1 to XActualCount - 1 do
              if XActualValues[i] > Xtmp then
                 Xtmp := XActualValues[i];
            OutValue := XTmp;
            result := true;
            end;
          RtMeanSq :
            begin
            Xtmp := 0;
            for i := 0 to XActualCount - 1 do
              Xtmp := Xtmp + (XActualValues[i] * XActualValues[i]);
            OutValue := XTmp / XActualCount;
            result := true;
            end;
          RtRootMeanSq :
            begin
            Xtmp := 0;
            for i := 0 to XActualCount - 1 do
              Xtmp := Xtmp + (XActualValues[i] * XActualValues[i]);
            OutValue := sqrt(XTmp / XActualCount);
            result := true;
            end;
          RtStdDev :
            begin
            Xtmp := 0;
            for i := 0 to XActualCount - 1 do
              Xtmp := Xtmp + XActualValues[i];
            XAvg := XTmp / XActualCount;
            XTmp := 0;
            for i := 0 to XActualCount - 1 do
              Xtmp := (XActualValues[i] - XAvg) * (XActualValues[i] - XAvg);
            OutValue := sqrt(XTmp / XActualCount);
            result := true;
            end;
          RtRightCount :
            begin
            j := 0;
            for i := 0 to XActualCount - 1 do
              if abs(XActualValues[i] - FSelData) < abs(FMaxDiff) then
                 j := j + 1;
            OutValue := j;
            result := true;
            end;
          RtSumCount :
            begin
            XTmp := 0;
            for i := 0 to XActualCount - 1 do
              Xtmp := Xtmp + XActualValues[i];
            if abs(XTmp - FSelData) < abs(FMaxDiff) then
               j := 1
            else
               j := 0;
            OutValue := j;
            result := true;
            end;
        else
          result := false;
        end
     else
        result := false;
     end
  else
     result := false;

  end;

  //Sd = sqrt(E(x - m)^2) = sqrt(E(x^2 - 2xm + m^2)) = sqrt(E(x^2) - 2m(Ex) + E(m^2)) =
//            = sqrt(FVSQuaredSum - 2 * avg * FVSSum + FVsCount * avg * avg)
// avg = FVsSum / FVsCount => Sd = sqrt(FVsSquaredSum - (2 * FvsSum * VsSum/ FVsCount) +
//                          (FVsCount * FvsSum / FVsCount * FVsSum / FVsCount))
// Sd = sqrt(FVsSquaredSum - (2 * FVsSum * FVsSum / FVsCount) + (FVsSum * FVsSum / FVsCount))
// Sd = sqrt(FvsSquaredSum - (FVsSum * FVsSum / FVsCount));

//-------------------------------------------------------------------------------------------------

function TResultGroup.GetOutVarSizeValue: double;
  begin
  if FGroupKind = RgSingle then
     begin
     case FTransf of
       RtSimple :
         result := FVSSum;
       RtMean :
         result := FVSSum/FVSCount;
       RtMin :
         result := FVSMin;
       RtMax :
         result := FVSMax;
       RtMeanSq :
         result := FVSSquaredSum / FVSCount;
       RtRootMeanSq :
         result := sqrt(FVSSquaredSum / FVSCount);
       RtStdDev :
         result := sqrt(FVSSquaredSum - (FVsSum * FVsSum / FVsCount));
       RtRightCount :
         result := FVSRightCount;
       RtSumCount :
         if abs(FSelData - FVSSum) < abs(FMaxDiff) then
            result := 1
         else
            result := 0;
       else
         result := 0;
       end;
     end
  else
     result := 0;
  end;

//-------------------------------------------------------------------------------------------------

function TResultGroup.GetSelData: double;
  begin
  result := FSelData;
  end;

//-------------------------------------------------------------------------------------------------

function TResultGroup.GetTotalColCount: integer;
  begin
  result := FTotalCols;
  end;

//-------------------------------------------------------------------------------------------------

function TResultGroup.GetTransf: TResultTransf;
  begin
  result := FTransf;
  end;

//-------------------------------------------------------------------------------------------------

function TResultGroup.GetVariableSize : integer;
  begin
  result := FVSCount;
  end;

//-------------------------------------------------------------------------------------------------

function TResultGroup.LoadFromString(data : string) : boolean;
  var
    TmpStrList : TStringList;
  begin
  TmpStrList := TStringList.Create;
  TmpStrList.DelimitedText := data;
  if (TmpStrList.Count > 0) and (trim(TmpStrList.Strings[0]) <> '') then
     begin
     if LowerCase(trim(TmpStrList.Strings[0])) = 'simple' then
        FTransf := RtSimple
     else if LowerCase(trim(TmpStrList.Strings[0])) = 'average' then
        FTransf := RtMean
     else if LowerCase(trim(TmpStrList.Strings[0])) = 'minimum' then
        FTransf := RtMin
     else if LowerCase(trim(TmpStrList.Strings[0])) = 'maximum' then
        FTransf := RtMax
     else if LowerCase(trim(TmpStrList.Strings[0])) = 'meansquare' then
        FTransf := RtMeanSq
     else if LowerCase(trim(TmpStrList.Strings[0])) = 'rms' then
        FTransf := RtRootMeanSq
     else if LowerCase(trim(TmpStrList.Strings[0])) = 'stddev' then
        FTransf := RtStdDev
     else if LowerCase(trim(TmpStrList.Strings[0])) = 'rightcount' then
        FTransf := RtRightCount
     else if LowerCase(trim(TmpStrList.Strings[0])) = 'sumcount' then
        FTransf := RtSumCount;
     end;
  if (TmpStrList.Count > 1) and (trim(TmpStrList.Strings[1]) <> '') then
     begin
     if LowerCase(trim(TmpStrList.Strings[1])) = 'single' then
        FGroupKind := RgSingle
     else if LowerCase(trim(TmpStrList.Strings[1])) = 'difference' then
        FGroupKind := RgDiff;
     end;
  if (TmpStrList.Count > 2) and (trim(TmpStrList.Strings[2]) <> '') then
     begin
     try
       FSelData := strtofloat(trim(TmpStrList.Strings[2]));
     except
       end;
     end;
  if (TmpStrList.Count > 3) and (trim(TmpStrList.Strings[3]) <> '') then
     begin
     try
       FMaxDiff := strtofloat(trim(TmpStrList.Strings[3]));
     except
       end;
     end;

  //****Optional: handle exceptions
  result := true;
  end;

//-------------------------------------------------------------------------------------------------

function TResultGroup.LoadMarksFromString(data : string) : boolean;
  var
    i, tot : integer;
  begin
  if length(data) < FTotalCols then
     tot := length(data)
  else
     tot := FTotalCols;
  for i := 0 to tot - 1 do
    if data[i + 1] = '1' then
       SetColMarks(i, true)
    else if data[i + 1] = '0' then
       SetColMarks(i, false);

  //****Optional: handle exceptions
  result := true;
  end;

//-------------------------------------------------------------------------------------------------

procedure TResultGroup.SetAllMarks(value: boolean);
  var i : integer;
  begin
  for i := 0 to FTotalCols - 1 do
    FColMarks[i] := value;
  end;

//-------------------------------------------------------------------------------------------------

procedure TResultGroup.SetColAuxValue(index: integer; const Value: integer);
  begin
  if (index >= 0) and (index < FTotalCols) then
     FColAuxValues[index] := value;
  end;

//-------------------------------------------------------------------------------------------------

procedure TResultGroup.SetColMarks(index: integer; const Value: boolean);
  begin
  if (index >= 0) and (index < FTotalCols) then
     FColMarks[index] := value;
  end;

//-------------------------------------------------------------------------------------------------

procedure TResultGroup.SetGroupKind(const Value: TResultGrouping);
  begin
  FGroupKind := value;
  end;

//-------------------------------------------------------------------------------------------------

procedure TResultGroup.SetMaxDiff(const Value: double);
  begin
  FMaxDiff := value;
  end;

//-------------------------------------------------------------------------------------------------

procedure TResultGroup.SetSelData(const Value: double);
  begin
  FSelData := value;
  end;

//-------------------------------------------------------------------------------------------------

procedure TResultGroup.SetTotalColCount(const Value: integer);
  begin
  FTotalCols := Value;
  SetLength(FColMarks, FTotalCols);
  end;

//-------------------------------------------------------------------------------------------------

procedure TResultGroup.SetTransf(const Value: TResultTransf);
  begin
  FTransf := value;
  end;

//-------------------------------------------------------------------------------------------------

{ THekatonResults }

//-------------------------------------------------------------------------------------------------

function THekatonResults.AddColumn(name: string): integer;
  var i : integer;
  begin
  result := FColumns.Add(name);
  for i := 0 to FGroups.Count - 1 do
   with FGroups.Objects[i] as TResultGroup do
     TotalColCount := TotalColCount + 1;
//  SetLength(FMarks, FGroups.Count, FColumns.Count);
  end;

//-------------------------------------------------------------------------------------------------

function THekatonResults.AddGroup(name: string; gr : TResultGroup): integer;
  begin
  result := FGroups.AddObject(name, gr);
//  SetLength(FMarks, FGroups.Count, FColumns.Count);
  end;

//-------------------------------------------------------------------------------------------------

procedure THekatonResults.AddOutput(subset, LineNumber: integer; value: array of double; IsGiven : array of boolean);
  var
    i : integer;
    s : string;
  begin
  if FChartAsOutput then
     for i := 0 to FGroups.Count - 1 do
       if IsGiven[i] then
          FChart.Series[(subset * FGroups.Count) + i].AddXY(linenumber, value[i]);
  if FFileAsOutput then
     begin
     if isGiven[0] then
        s := floattostr(value[0])
     else
        s := FNoDataMark;
     for i := 1 to FGroups.Count - 1 do
       if IsGiven[i] then
          s := s + ' ' + floattostr(value[i])
       else
          s := s + ' ' + FNoDataMark;
     if LineNumber < FMaxLinesPerSubset then
        FOutputStrList.Insert((LineNumber * (subset + 1)) + subset, s)
     else
        begin
        for i := 0 to subset - 1 do
          FOutputStrList.Add(EmptyOutputLine);
        FOutputStrList.Add(s);
        end;
     end;

  end;

//-------------------------------------------------------------------------------------------------

{procedure THekatonResults.ConcatenateSubsets(NewFile : TFileName);
  var
    i, j, XLines, XNextPos : integer;
    XStrList : TStringList;
  begin
  //WriteHeader;
  XStrList := TStringList.Create;
  i := 0;
  j := 0;
  XNextPos := 0;
  while i < FInputStrList.Count do
    begin
    XNextPos := FindSubsetStart(i, XLines);
    end;




  end;}

//-------------------------------------------------------------------------------------------------

procedure THekatonResults.ConcatenateFiles(Filelist : TStrings);
  var
    i, j : integer;
    XTmpStrList1, XTmpStrList2 : TStringList;
    XIsNewHeaderLoaded : boolean;
    XIsNewHeaderCompatible : boolean;
  begin
  XTmpStrList1 := TStringList.Create;
  //*****load header
  for i := 1 to Filelist.Count - 1 do
    begin
    XTmpStrList2 := TStringList.Create;
    XTmpStrList2.LoadFromFile(FileList.Strings[i]);
    j := 0;
    XIsNewHeaderLoaded := false;
    XIsNewHeaderCompatible := false;
    while not XIsNewHeaderLoaded do
      begin
      if pos(XTmpStrList2.Strings[j], '*??*') > 0 then
         begin
         XIsNewHeaderLoaded := true;
         //*****Verify if new header is compatible
         end;
      j := j + 1;
      end;
    if XIsNewHeaderCompatible then
       while (j < XTmpStrList2.Count) do
         begin
         XTmpStrList1.Add(XTmpStrList2.Strings[j]);
         j := j + 1;
         end;
    XTmpStrList2.Free;
    end;



  end;

//-------------------------------------------------------------------------------------------------

constructor THekatonResults.Create;
  begin
  inherited Create;
  FInputFileName := '';
  FMaxSequenceSize := 0;
  FColumns         := TStringList.Create;
  FGroups          := TStringList.Create;
  FOutputStrList   := TStringList.Create;
  FFileAsOutput    := false;
  FChartAsOutput   := false;
  FGroupSequence   := true;
  SetSequenceSize(1);
  FLongLastMark := false;
  FNoDataMark := '**No*Data**';
  end;

//-------------------------------------------------------------------------------------------------

procedure THekatonResults.DeleteColumn(index: integer);
  var i : integer;
  begin
  FColumns.Delete(index);
  for i := 0 to FGroups.Count - 1 do
   with FGroups.Objects[i] as TResultGroup do
     TotalColCount := TotalColCount - 1;
  end;

//-------------------------------------------------------------------------------------------------

procedure THekatonResults.DeleteGroup(index: integer);
  begin
  if (index >= 0) and (index < FGroups.Count) then
     begin
     FGroups.Objects[index].Free;
     FGroups.Delete(index);
     end;
  end;

//-------------------------------------------------------------------------------------------------

destructor THekatonResults.Destroy;
  var i : integer;
  begin
  for i := FGroups.Count - 1 downto 0 do
    FGroups.Objects[i].Free;
  FColumns.Free;
  FGroups.Free;
  FOutputStrList.Free;
  inherited;
  end;

//-------------------------------------------------------------------------------------------------

function THekatonResults.EmptyOutputLine: string;
  var
    i : integer;
    s : string;
  begin
  s := FNoDataMark;
  for i := 1 to FGroups.Count - 1 do
    s := s + ' ' + FNoDataMark;
  result := s;
  end;

//-------------------------------------------------------------------------------------------------

procedure THekatonResults.EndSubsetAtOutput(subset, nOfLines: integer);
  var i : integer;
  begin
  if nOfLines > FMaxLinesPerSubset then
     FMaxLinesPerSubset := nOfLines
  else
     for i := nOfLines to FMaxLinesPerSubset - 1 do
       FOutputStrList.Insert((i * (subset + 1)) + subset, EmptyOutputLine);
  end;

//-------------------------------------------------------------------------------------------------

function THekatonResults.FindSubsetStart(position : integer; var numberOfLines : integer) : integer;
  var
    i : integer;
    XFound : boolean;
    XTmpStrList : TStringList;
  begin
  i := position;
  XTmpStrList := TStringList.Create;
  numberOfLines := -1;
  XFound := false;
  while (i < FInputStrList.Count) and not XFound do
    begin
    XTmpStrList.DelimitedText := FInputStrList.Strings[i];
    if (XTmpStrList.Count > 0) and
       ((trim(XTmpStrList.Strings[0]) = '*!*') or (trim(XTmpStrList.Strings[0]) = '*!!*')) then
       begin
       XFound := true;
       try
         numberOfLines := strtoint(trim(XTmpStrList.Strings[1]));  //???? Verify if the index is alright
       except
         numberOfLines := 0;
         end;
       end;
    i := i + 1;
    end;
  XTmpStrList.Free;
  result := i;
  end;

//-------------------------------------------------------------------------------------------------

procedure THekatonResults.GenerateOutput;
  var
    s : string;
    i, j, k, XLongPosition, XMaxLines : integer;
    XCurrentSubset, XActualSubset, XSubsetSize : integer;
    XLine, XLongValues : array of double;
    XIsGiven1, XIsGiven2 : array of boolean;
    XisSequenceBreak : boolean;

  begin
  //Initialise variables
  FMaxLinesPerSubset := 0;

  XisSequenceBreak := false;
  FInputStrList := TStringList.Create;
  FInputStrList.LoadFromFile(FInputFileName);
  if (trim(FSequenceSeparator) <> '') and (FUseSequenceSeparator) then
     FSequencesSize := FMaxSequenceSize;
  SetLength(FLongGroups, FGroups.Count, FSequencesSize);
  SetLength(XLine, FColumns.Count);
  SetLength(XIsGiven1, FColumns.Count);
  for i := 0 to FGroups.Count - 1 do
    for j := 0 to FSequencesSize - 1 do
      begin
      FLongGroups[i, j] := TResultGroup.Create(0);
      FLongGroups[i, j].Transf := FSequenceFunction;
      FLongGroups[i, j].GroupKind := RgSingle;
      end;

  i := 0;
  XCurrentSubset := 0;
  XActualSubset := 0;
  XMaxLines := 0;

  SetLength(XLongValues, FGroups.count);
  SetLength(XIsGiven2, FGroups.count);

  while (i < FInputStrList.Count) and (XCurrentSubset < FSubsetCount) do
    begin
    i := FindSubsetStart(i, XSubsetSize);
    if FSubSetMarks[XCurrentSubset] then
       begin
       j := 0;
       XLongPosition := 0;
       while (i > 0) and (i < FInputStrList.Count) and (j < XSubsetSize) do
         begin
         i := GetLine(XLine, XIsGiven1, 0, i, XisSequenceBreak);
         if XIsSequenceBreak then
           begin
           if XLongPosition > 0 then
              XLongPosition := ProcessLine(XLine, XIsGiven1, j, XActualSubset, -1);
           i := i + 1;
           end
         else if i > 0 then
           XLongPosition := ProcessLine(XLine, XIsGiven1, j, XActualSubset, XLongPosition)
         end;

       if GroupSequence then
          begin
          if j > XMaxLines then XMaxLines := j;
          end
       else
          begin
          j := 0;
          for XLongPosition := 0 to FSequencesSize - 1 do
              begin
              for k := 0 to FGroups.Count - 1 do
                 begin
                 if FLongGroups[k, XLongPosition].VariableSize > 0 then
                    begin
                    XLongValues[k] := FLongGroups[k, XLongPosition].GetOutVarSizeValue;
                    XIsGiven2[k] := true;
                    end
                 else
                    begin
                    XLongValues[k] := 0;
                    XIsGiven2[k] := false;
                    end;
                 FLongGroups[k, XLongPosition].ClearVarSizeValues;
                 end;
              AddOutput(XActualSubset, XLongPosition, XLongValues, XIsGiven2);
              j := j + 1;
              end;
          XMaxLines := j;
          end;
       if FFileAsOutput then
          EndSubsetAtOutput(XActualSubset, j);

       if i < 0 then i := -i;
       XActualSubset := XActualSubset + 1;
       end;
    XCurrentSubset := XCurrentSubset + 1;
    end;

  if FFileAsOutput then
     begin
     s := '*??* ' + inttostr(FGroups.count * XActualSubset) + ' 1 0';
     FOutputStrList.Insert(0, s);
     s := '';
     FOutputStrList.Insert(1, s);
     s := '*!!* ' + inttostr(XMaxLines);
     FOutputStrList.Insert(2, s);
     s := '';
     FOutputStrList.Insert(3, s);
     if XActualSubset > 1 then
        begin
        i := 4;
        j := 0;
        while (i < FOutputStrList.Count) do
          begin
          if (j >= XActualSubset) then
             begin
             j := 0;
             FOutputStrList.Insert(i, '');
             i := i + 1;
             end
          else
             begin
             j := j + 1;
             i := i + 1;
             end;
          end;
        end;
     FOutputStrList.SaveToFile(FOutputFileName);
     end;
  end;

//-------------------------------------------------------------------------------------------------

{procedure THekatonResults.OldGenerateOutput;
  var
    XTmpStrList : TStringList;
    i_line, i_subset, i_validSubset, i_vector, XSubsetSize, i_StrListPos, i1, i2 : integer;
    i_long, XlinesPerSubset, XMaxLines : integer;
    XStartSubSet : boolean;
    XLine, XLineAfter : array of double;
    XtmpValue : double;
    XEmptyLine: boolean;
    s : string;

//    XLongGroups : array of array of TResultGroup;

  begin
  XMaxLines := 0;
  FInputStrList := TStringList.Create;
  FInputStrList.LoadFromFile(FInputFileName);
  XTmpStrList := TStringList.Create;

  SetLength(XLine, FColumns.Count);
  SetLength(XLineAfter, FGroups.Count);
  SetLength(FLongGroups, FGroups.Count, FSequencesSize);

  if FInputFileName <> '' then
     begin
     i_line := 0;
     i_subset := 0;
     i_validSubset := 0;
     XSubsetSize := 0;
     while (i_line < FInputStrList.Count) and (i_subset < FSubSetCount) do
       begin
       XStartSubSet := false;
       //Find start of a Subset
       while (i_Line < FInputStrList.Count) and not XStartSubSet do
         begin
         XTmpStrList.DelimitedText := FInputStrList.Strings[i_line];
         if (XTmpStrList.Count > 0) and
            ((trim(XTmpStrList.Strings[0]) = '*!*') or (trim(XTmpStrList.Strings[0]) = '*!!*')) then
            begin
            if FSubSetMarks[i_Subset] then
               begin
               XStartSubset := true;
               try
                 XSubsetSize := strtoint(trim(XTmpStrList.Strings[1]));  //???? Verify if the index is alright
               except
                 XSubsetSize := 0;
                 end;
               end
            end;
         i_line := i_line + 1;
         end;

       i_vector := 0;
       XLinesPerSubset := 0;
       XMaxLines := 0;
       i_long := 0;

       //Run through the subset
       while XStartSubset and (i_vector < XSubsetSize) do
         begin
         i1 := 0;
         i_StrListPos := 0;

         //Read all columns...
         while (i1 < FColumns.Count) and (i_line < FInputStrList.Count) do
           begin
           XTmpStrList.DelimitedText := FInputStrList.Strings[i_line];
           XEmptyLine := (XTmpStrList.Count <= 0) or (trim(XTmpStrList.Strings[0]) = '');
           if FConsiderEmptyLine and XEmptyLine then
              if i1 > 0 then
                 while i1 < FColumns.Count do
                   begin
                   XLine[i1] := 0;
                   i1 := i1 + 1;
                   end;
           while (i_StrListPos < XTmpStrList.Count) and (i1 < FColumns.Count) do
             begin
             try
               XLine[i1] := strtofloat(XTmpStrList.Strings[i_StrListPos]);
             except
               XLine[i1] := 0;
               end;
             i1 := i1 + 1;
             i_StrListPos := i_StrListPos + 1;
             end;
           if (i_StrListPos >= XTmpStrList.Count) then
              begin
              i_line := i_line + 1;
              i_StrListPos := 0;
              end;
           end;

         //if all columns were read, store the values properly in FLongGroups
         if i1 >= FColumns.Count then
            begin
            for i2 := 0 to FGroups.Count - 1 do
              begin
              XTmpValue := (FGroups.Objects[i2] as TResultGroup).GetOutValue(XLine, FColumns.Count);
              if GroupSequence then
                 FLongGroups[i2, 0].AddVarSizeValue(XTmpValue)
              else
                 FLongGroups[i2, i_long].AddVarSizeValue(XTmpValue);
              end;
            i_long := i_long + 1;
            if (i_long >= FSequencesSize) then
               begin
               i_long := 0;
               if GroupSequence then
                  begin
                  for i2 := 0 to FGroups.Count - 1 do
                    begin
                    XLineAfter[i2] := FLongGroups[i2, 0].GetOutVarSizeValue;
                    FLongGroups[i2, 0].ClearVarSizeValues;
                    end;
                  AddOutput(i_Validsubset, XLinesPerSubset, XLineAfter);
                  XlinesPerSubset := XLinesPerSubset + 1;
                  if XlinesPerSubset > XMaxLines then XMaxLines := XLinesPerSubset;
                  end;
               end;
            end
         else
            XStartSubset := false;
         i_vector := i_vector + 1;
         end;

       if FSubSetMarks[i_Subset] then
          begin
          if not GroupSequence then
             begin
             XlinesPerSubset := 0;
             for i_long := 0 to FSequencesSize - 1 do
               begin
               for i2 := 0 to FGroups.Count - 1 do
                 begin
                 XLineAfter[i2] := FLongGroups[i2, i_long].GetOutVarSizeValue;
                 FLongGroups[i2, i_long].ClearVarSizeValues;
                 end;
               AddOutput(i_Validsubset, XLinesPerSubset, XLineAfter);
               XlinesPerSubset := XLinesPerSubset + 1;
               end;
             end;
          if FFileAsOutput then
             EndSubsetAtOutput(i_Validsubset, XlinesPerSubset);
          i_subset := i_subset + 1;
          i_validSubset := i_validSubset + 1;
          end
       else
          i_subset := i_subset + 1;
       end;
     end;

  for i1 := 0 to FGroups.Count - 1 do
    for i2 := 0 to FSequencesSize - 1 do
      FLongGroups[i1, i2].Free;

  SetLength(FLongGroups, 0, 0);

  if FFileAsOutput then
     begin
//     if FSubsetCount = 1 then


     s := '*??* ' + inttostr(FGroups.count) + ' 1 0';
     FOutputStrList.Insert(0, s);
     s := '';
     FOutputStrList.Insert(1, s);
     if GroupSequence then
       s := '*!!* ' + inttostr(XMaxLines)
     else
       s := '*!!* ' + inttostr(FSequencesSize);
     FOutputStrList.Insert(2, s);
     s := '';
     FOutputStrList.Insert(3, s);
     FOutputStrList.SaveToFile(FOutputFileName);
     end;

  end;}

//-------------------------------------------------------------------------------------------------

function THekatonResults.GetColumnCount: integer;
  begin
  result := FColumns.Count;
  end;

//-------------------------------------------------------------------------------------------------

function THekatonResults.GetColumns(index: integer): string;
  begin
  if (index >= 0) and (index < FColumns.Count) then
     result := FColumns.Strings[index]
  else
     result := '';
  end;

//-------------------------------------------------------------------------------------------------

function THekatonResults.GetConsiderEmptyLine: boolean;
  begin
  result := FConsiderEmptyLine;
  end;

//-------------------------------------------------------------------------------------------------

function THekatonResults.GetGroup(index: integer): TResultGroup;
  begin
  if (index >= 0) and (index < FGroups.Count) then
     result := FGroups.Objects[index] as TResultGroup
  else
     result := nil;
  end;

//-------------------------------------------------------------------------------------------------

function THekatonResults.GetGroupCount: integer;
  begin
  result := FGroups.Count;
  end;

//-------------------------------------------------------------------------------------------------

function THekatonResults.GetGroupName(index: integer): string;
  begin
  if (index >= 0) and (index < FGroups.Count) then
     result := FGroups.Strings[index]
  else
     result := '';
  end;

//-------------------------------------------------------------------------------------------------

function THekatonResults.GetGroupSequence: boolean;
  begin
  result := FGroupSequence
  end;

//-------------------------------------------------------------------------------------------------

function THekatonResults.GetInputFileName: TFileName;
  begin
  result := FInputFileName;
  end;

//-------------------------------------------------------------------------------------------------

function THekatonResults.GetLine(var ArLine : array of double; var ArIsGiven : array of boolean;
                                 Position, InputLine : integer; var IsSequenceBreak : boolean) : integer;
  var
    i_Input : integer;
    i, j : integer;
    XTmpStrList : TStringList;
    XEmptyInputStr, XEndLine : boolean;
    XSeqBreak : boolean;
  begin
  XSeqBreak := false;
  i_Input := InputLine;
  XTmpStrList := TStringList.Create;
  IsSequenceBreak := false;
  if (i_input < FInputStrList.Count) then
     begin
     XTmpStrList.DelimitedText := FInputStrList.Strings[i_input];
     if (XTmpStrList.Count > 0) and (trim(XTmpStrList.Strings[0]) <> '') and (trim(XTmpStrList.Strings[0]) <> '***') then
        begin
        if (trim(XTmpStrList.Strings[0]) = FSequenceSeparator) then
           begin
           if FUseSequenceSeparator then
              begin
              XEndLine := true;
              XSeqBreak := true;
              XEmptyInputStr:= false;
              end
           else
              begin
              XEmptyInputStr := true;
              XEndLine := (FConsiderEmptyLine) and (position > 0);
              i_input := i_input + 1;
              XSeqBreak := false;
              end
           end
        else
           begin
           XEndLine := (trim(XTmpStrList.Strings[0]) = '*!*') or (trim(XTmpStrList.Strings[0]) = '*!!*');
           if XEndLine then i_Input := -i_Input;
           XEmptyInputStr:= false;
           XSeqBreak := false;
           end
        end
     else
        begin
        XEmptyInputStr := true;
        XEndLine := (FConsiderEmptyLine) and (position > 0);
        i_input := i_input + 1;
        XSeqBreak := false;
        end;

     if XEndLine then
        begin
        for i := position to FColumns.count - 1 do
          begin
          ArLine[i] := 0;
          ArIsGiven[i] := false;
          end;
        IsSequenceBreak := XSeqBreak;
        end
     else if XEmptyInputStr then
        i_input := GetLine(ArLine, ArIsGiven, position, i_Input, IsSequenceBreak)
     else
        begin
        i := position;
        j := 0;
        while (i < FColumns.Count) and (j < XTmpStrList.Count) do
          begin
          if LowerCase(trim(XTmpStrList.Strings[j])) = LowerCase(FNoDataMark) then
             begin
             ArLine[i] := 0;
             ArIsGiven[i] := false;
             end
          else
             try
               ArLine[i] := strtofloat(XTmpStrList.Strings[j]);
               ArIsGiven[i] := true;
             except
               ArLine[i] := 0;
               ArIsGiven[i] := false;
               end;
          i := i + 1;
          j := j + 1;
          end;
        if (i < FColumns.Count) then
           i_input := GetLine(ArLine, ArIsGiven, i, i_Input + 1, XSeqBreak)
        else
           i_input := i_input + 1;
        end
     end
  else
     i_input := - i_input;
  XTmpStrList.Free;
  result := i_input;
  end;

//-------------------------------------------------------------------------------------------------

function THekatonResults.GetLongLastMark : boolean;
  begin
  result := FLongLastMark;
  end;

//-------------------------------------------------------------------------------------------------

function THekatonResults.GetMarks(i, j: integer): boolean;
  begin
  if (i >= 0) and (i < FGroups.count) and (j >= 0) and (j < FColumns.Count) then
//     result := FMarks[i, j]
     result := (FGroups.Objects[i] as TResultGroup).ColMarks[j]
  else
     result := false;
  end;

//-------------------------------------------------------------------------------------------------

function THekatonResults.GetMaxSequenceSize : integer;
  begin
  result := FMaxSequenceSize;
  end;

//-------------------------------------------------------------------------------------------------

function THekatonResults.GetSequenceFunction: TResultTransf;
  begin
  result := FSequenceFunction;
  end;

//-------------------------------------------------------------------------------------------------

function THekatonResults.GetSequenceGrouping: TResultGrouping;
  begin
  result := FSequenceGrouping;
  end;

//-------------------------------------------------------------------------------------------------

function THekatonResults.GetSequenceMarks(index: integer): boolean;
  begin
  if (index >= 0) and (index < FSequencesSize) then
     result := FSequenceMarks[index]
  else
     result := false;
  end;

//-------------------------------------------------------------------------------------------------

function THekatonResults.GetSequenceSeparator : string;
  begin
  result := FSequenceSeparator;
  end;

//-------------------------------------------------------------------------------------------------

function THekatonResults.GetSequenceSize: integer;
  begin
  result := FSequencesSize;
  end;

//-------------------------------------------------------------------------------------------------

function THekatonResults.GetSubsetCount: integer;
  begin
  result := FSubsetCount;
  end;

//-------------------------------------------------------------------------------------------------

function THekatonResults.GetSubsetMarks(index: integer): boolean;
  begin
  if (index >= 0) and (index < FSubsetCount) then
     result := FSubSetMarks[index]
  else
     result := false;
  end;

//-------------------------------------------------------------------------------------------------

function THekatonResults.GetUseSequenceBreak : boolean;
  begin
  result := FUseSequenceSeparator;
  end;

//-------------------------------------------------------------------------------------------------

procedure THekatonResults.LoadHeaderFromFile;
  var
    XFileStrl   : TStringList;
    XHeaderStrl : TStringList;
    XLabelsStrl : TStringList;
    i, XColumnCount: integer;
  begin
  XFileStrl := TStringList.Create;
  XHeaderStrl := TStringList.Create;
  XLabelsStrl := TStringList.Create;
  XFileStrl.LoadFromFile(FInputFileName);

  i := 0;
  while (i < XFileStrl.Count) and (trim(XFileStrl.Strings[i]) = '') do
    i := i + 1;
  XHeaderStrl.DelimitedText := XFileStrl.Strings[i];
  i := i + 1;
  while (i < XFileStrl.Count) and (trim(XFileStrl.Strings[i]) = '') do
    i := i + 1;
  if i < XFileStrl.Count then
    XLabelsStrl.DelimitedText := XFileStrl.Strings[i];

  for i := FColumns.Count - 1 downto 0 do
    DeleteColumn(i);

  if trim(XHeaderStrl.Strings[0]) = '*?*' then
     begin
     //Define Mode
     try
       /////////////////////Define proper values
       XColumnCount := strtoint(XHeaderStrl.Strings[1]);
       SetSubsetCount(strtoint(XHeaderStrl.Strings[2]));
     except
       XColumnCount := 0;
       SetSubsetCount(0);
       end;
     end
  else if trim(XHeaderStrl.Strings[0]) = '*??*' then
     begin
     //Define Mode
     try
       XColumnCount := strtoint(XHeaderStrl.Strings[1]);
       SetSubsetCount(strtoint(XHeaderStrl.Strings[2]));
     except
       XColumnCount := 0;
       SetSubsetCount(0);
       end;
     try
       FConsiderEmptyLine := strtoint(XHeaderStrl.Strings[3]) > 0;
     except
       FConsiderEmptyLine := false;
       end;
     if XHeaderStrl.Count > 4 then
        FSequenceSeparator := trim(XHeaderStrl.Strings[4])
     else
        FSequenceSeparator := '';
     if XHeaderStrl.Count > 5 then
        try
          FMaxSequenceSize := strtoint(XHeaderStrl.Strings[5]);
        except
          FMaxSequenceSize := 0;
        end
     else
        FMaxSequenceSize := 0;
     end
  else
      begin
      //Define Mode
       XColumnCount := 0;
       FSubsetCount := 0;
       end;


  i := 0;
  while (i < XColumnCount) do
    begin
    if (i < XLabelsStrl.Count) and (XLabelsStrl.Strings[0] <> '*!*')
                               and (XLabelsStrl.Strings[0] <> '*!!*')then
       AddColumn(XLabelsStrl[i])
    else
       AddColumn('Column' + inttostr(i));
    i := i + 1;
    end;

  XFileStrl.Free;
  XHeaderStrl.Free;
  XLabelsStrl.Free;
  end;

//-------------------------------------------------------------------------------------------------

function THekatonResults.ProcessLine(ArLine : array of double; ArIsGiven : array of boolean;
                                     var OutputIndex : integer; subset, longPosition : integer) : integer;
   var
     i, longCount : integer;
     XLineAfter : array of double;
     XTmpValue : double;
     XLongIsGiven : array of boolean;
   begin
   SetLength(XLongIsGiven, FGroups.Count);
   for i := 0 to FGroups.Count - 1 do
     begin
     if (FGroups.Objects[i] as TResultGroup).GetOutValue(ArLine, ArIsGiven, FColumns.Count, XTmpValue) then
        begin
        if FGroupSequence then
           begin
           if FSequenceMarks[longPosition] then
              FLongGroups[i, 0].AddVarSizeValue(XTmpValue, FLongLastMark);
           end
        else
           FLongGroups[i, longPosition].AddVarSizeValue(XTmpValue);
        end;
     end;
   longCount := longPosition + 1;
   if (longCount >= FSequencesSize) or (longPosition < 0) then
      begin
      longCount := 0;
      if GroupSequence then
         begin
         setLength(XLineAfter, FGroups.count);
         for i := 0 to FGroups.Count - 1 do
           begin
           if FLongGroups[i, 0].VariableSize > 0 then
              begin
              XLineAfter[i] := FLongGroups[i, 0].GetOutVarSizeValue;
              XLongIsGiven[i] := true;
              end
           else
              begin
              XLineAfter[i] := 0;
              XLongIsGiven[i] := false;
              end;
           FLongGroups[i, 0].ClearVarSizeValues;
           end;
         AddOutput(subset, OutputIndex, XLineAfter, XLongIsGiven);
         OutputIndex := OutputIndex + 1;
         end;
      end;
   result := longCount;
   end;

//-------------------------------------------------------------------------------------------------

function THekatonResults.ProcessScript(fileName : TFileName) : integer;
  var
    TmpStrList1, TmpStrList2 : TStringList;
    i, j, k, Xpos : integer;
    strCommand, tmpStr : string;
    tmpGroup : TResultGroup;
    XForCount : integer;
    XForTmpStrings, XForDirectories : TStringList;
    XForSearchRecs : array of TSearchRec;
    XForPositions : array of integer;
  begin
  TmpStrList1 := TStringList.Create;
  XForTmpStrings := TStringList.Create;
  XForDirectories := TStringList.Create;
  XForCount := 0;

  if FileExists(FileName) then
     begin
     TmpStrList1.LoadFromFile(FileName);
     TmpStrList2 := TStringList.Create;
     i := 0;
     while i < TmpStrList1.Count do
       begin
       TmpStrList2.DelimitedText := TmpStrList1.Strings[i];
       for j := 0 to TmpStrList2.Count - 1 do
         for k := 0 to XForCount - 1 do
           begin
           if trim(lowercase(TmpStrList2.Strings[j])) = trim(lowercase(XForTmpStrings.Strings[k])) then
              TmpStrList2.Strings[j] := XForSearchRecs[k].Name
           else if (length(TmpStrList2.Strings[j]) > 1) and (TmpStrList2.Strings[j][1] = '#') then
              begin
              XPos := pos(lowercase(XForTmpStrings.Strings[k]), lowercase(TmpStrList2.Strings[j]));
              if XPos > 0 then
                 begin
                 tmpStr := copy(TmpStrList2.Strings[j], 2, XPos - 2);
                 tmpStr := tmpStr + XForSearchRecs[k].Name;
                 tmpStr := tmpStr + copy(TmpStrList2.Strings[j], XPos +
                                 length(XForTmpStrings.Strings[k]), length(TmpStrList2.Strings[j]));
                 TmpStrList2.Strings[j] := tmpStr;
                 end;
              end;
           end;

       if TmpStrList2.Count > 0 then
          begin
          strCommand := LowerCase(Trim(TmpStrList2.Strings[0]));
          if strCommand = 'foreachfile' then
             begin
             XForCount := XForCount + 1;
             SetLength(XForSearchRecs, XForCount);
             SetLength(XForPositions, XForCount);
             if (TmpStrList2.Count > 3) and (lowercase(TmpStrList2.Strings[2]) = 'in') then
                begin
                XForTmpStrings.Add(TmpStrList2.Strings[1]);
                XForDirectories.Add(TmpStrList2.Strings[3]);
                XForPositions[XForCount - 1] := i;
                FindFirst(XForDirectories[XForCount - 1], 0, XForSearchRecs[XForCount - 1]);
                end
             else
                begin
                XForTmpStrings.Add('');
                XForDirectories.Add('');
                //*****Raise exception
                end;
             end

          else if strCommand = 'endforeach' then
             begin
             if FindNext(XForSearchRecs[XForCount - 1]) = 0 then
                begin
                i := XForPositions[XForCount - 1];
                end
             else
                begin
                FindClose(XForSearchRecs[XForCount - 1]);
                XForTmpStrings.Delete(XForCount - 1);
                XForDirectories.Delete(XForCount - 1);
                XForCount := XForCount - 1;
                end;
             end

          else if strCommand = 'showmessage' then
             begin
             if (TmpStrList2.Count > 1) then
                ShowMessage(TmpStrList2.Strings[1]);
             end

          else if strCommand = 'sourcefile' then
             begin
             if (TmpStrList2.Count > 1) then
                SetInputFileName(trim(TmpStrList2.Strings[1]));
             end

          else if strCommand = 'addgroup' then
             begin
             tmpGroup := TResultGroup.Create(FColumns.Count);
             TmpStrList2.Delete(0);
             if (TmpStrList2.Count > 0) then
                tmpGroup.LoadFromString(TmpStrList2.DelimitedText);
             AddGroup('Group' + inttostr(FGroups.count), tmpGroup);
             end

          else if strCommand = 'namegroup' then
             begin
             if (TmpStrList2.Count > 2) then
                try
                  SetGroupName(StrToInt(trim(TmpStrList2.Strings[1])), trim(TmpStrList2.Strings[1]));
                except
                  end;
             end

          else if strCommand = 'changegroup' then
             begin
             j := -1;
             if (TmpStrList2.Count > 2) then
                try
                  j := StrToInt(trim(TmpStrList2.Strings[1]))
                except
                  end;
             if (j >= 0) and (j < FGroups.Count) then
                begin
                TmpStrList2.Delete(1);
                TmpStrList2.Delete(0);
                GetGroup(j).LoadFromString(TmpStrList2.DelimitedText);
                end;
             end

          else if strCommand = 'markgroup' then
             begin
             if (TmpStrList2.Count > 2) then
                begin
                try
                  j := StrToInt(trim(TmpStrList2.Strings[1]));
                  GetGroup(j).LoadMarksFromString(TmpStrList2.Strings[2]);
                except
                  end;
                end
             end

          else if strCommand = 'deletegroup' then
             begin
             if (TmpStrList2.Count > 1) then
                begin
                try
                  j := StrToInt(trim(TmpStrList2.Strings[1]));
                  DeleteColumn(j);
                except
                  end;
                end;
             end

          else if strCommand = 'cleargroups' then
             begin
             for j := FGroups.Count - 1 downto 0 do
               DeleteGroup(j);
             end

          else if strCommand = 'marksubsets' then
             begin
             if (TmpStrList2.Count > 1) then
                begin
                tmpStr := trim(TmpStrList2.Strings[1]);
                for j := 0 to Min(Length(tmpStr), FSubsetCount) - 1 do
                  if tmpStr[j + 1] = '1' then
                     SubSetMarks[j] := true
                  else if tmpStr[j + 1] = '0' then
                     SubSetMarks[j] := false;
                end;
             end

          else if strCommand = 'longgroupfunction' then
             begin
             if (TmpStrList2.Count > 1) and (trim(TmpStrList2.Strings[1]) <> '') then
               begin
               if LowerCase(trim(TmpStrList2.Strings[1])) = 'simple' then
                  FSequenceFunction := RtSimple
               else if LowerCase(trim(TmpStrList2.Strings[1])) = 'average' then
                  FSequenceFunction := RtMean
               else if LowerCase(trim(TmpStrList2.Strings[1])) = 'minimum' then
                  FSequenceFunction := RtMin
               else if LowerCase(trim(TmpStrList2.Strings[1])) = 'maximum' then
                  FSequenceFunction := RtMax
               else if LowerCase(trim(TmpStrList2.Strings[1])) = 'meansquare' then
                  FSequenceFunction := RtMeanSq
               else if LowerCase(trim(TmpStrList2.Strings[1])) = 'rms' then
                  FSequenceFunction := RtRootMeanSq
               else if LowerCase(trim(TmpStrList2.Strings[1])) = 'stddev' then
                  FSequenceFunction := RtStdDev
               else if LowerCase(trim(TmpStrList2.Strings[1])) = 'rightcount' then
                  FSequenceFunction := RtRightCount
               else if LowerCase(trim(TmpStrList2.Strings[1])) = 'sumcount' then
                  FSequenceFunction := RtSumCount;
               end;
             end

          else if strCommand = 'longsize' then
             begin
             if (TmpStrList2.Count > 1) then
                begin
                try
                  SetSequenceSize(StrToInt(trim(TmpStrList2.Strings[1])));
                except
                  end;
                end;
             end

          else if strCommand = 'longgroupmode' then
             begin
             if (TmpStrList2.Count > 1) and (trim(TmpStrList2.Strings[1]) <> '') then
               begin
               if LowerCase(trim(TmpStrList2.Strings[1])) = 'group' then
                  FGroupSequence := true
               else if LowerCase(trim(TmpStrList2.Strings[1])) = 'position' then
                  FGroupSequence := false;
               end;
             end

          else if strCommand = 'longmarkssetalltrue' then
             begin
             for j := 0 to FSequencesSize - 1 do
               SetSequenceMarks(j, true);
             end

          else if strCommand = 'longmarkssetallfalse' then
             begin
             for j := 0 to FSequencesSize - 1 do
               SetSequenceMarks(j, false);
             end

          else if strCommand = 'longmarkssettrue' then
             begin
             for j := 1 to TmpStrList2.Count - 1 do
               begin
               try
                 SetSequenceMarks(StrToInt(trim(TmpStrList2.Strings[j])), true)
               except
                 end;
               end;
             end

          else if strCommand = 'longuseseparator' then
             begin
             if FSequenceSeparator <> '' then
                FUseSequenceSeparator := true;
             end

          else if strCommand = 'notlonguseseparator' then
             begin
             FUseSequenceSeparator := false;
             end

          else if strCommand = 'longmarklastline' then
             begin
             FLongLastMark := true;
             end

          else if strCommand = 'notlongmarklastline' then
             begin
             FLongLastMark := false;
             end

          else if strCommand = 'longmarkssetfalse' then
             begin
             for j := 1 to TmpStrList2.Count - 1 do
               begin
               try
                 SetSequenceMarks(StrToInt(trim(TmpStrList2.Strings[j])), true)
               except
                 end;
               end;
             end

          else if strCommand = 'generateoutputfile' then
             begin
             ResetOutputChoices;
             if (TmpStrList2.Count > 1) and (trim(TmpStrList2.Strings[1]) <> '') then
                begin
                try
                  SetupFileAsOutput(trim(TmpStrList2.Strings[1]));
                  Generateoutput;
                except
                  end;
                end;
             end
          end;
       i := i + 1;
       end;
     TmpStrList2.Free;
     end;
  TmpStrList1.Free;
  XForTmpStrings.Free;
  //****Define result strategy;
  result := 0;
  end;

procedure THekatonResults.ResetOutputChoices;
  begin
  FChartAsOutput := false;
  FFileAsOutput := false;
  end;

//-------------------------------------------------------------------------------------------------

procedure THekatonResults.SetColumns(index: integer; const Value: string);
  begin
  if (index >= 0) and (index < FColumns.Count) then
     FColumns.Strings[index] := value;
  end;

//-------------------------------------------------------------------------------------------------

procedure THekatonResults.SetConsiderEmptyLine(const Value: boolean);
  begin
  FConsiderEmptyLine := value;
  end;

//-------------------------------------------------------------------------------------------------

procedure THekatonResults.SetGroup(index: integer; const Value: TResultGroup);
  begin
  if (index >= 0) and (index < FGroups.Count) then
     FGroups.Objects[index] := value;
  end;

//-------------------------------------------------------------------------------------------------

procedure THekatonResults.SetGroupName(index: integer; const Value: string);
  begin
  if (index >= 0) and (index < FGroups.Count) then
     FGroups.Strings[index] := value;
  end;

//-------------------------------------------------------------------------------------------------

procedure THekatonResults.SetGroupSequence(const Value: boolean);
  begin
  FGroupSequence := Value;
  end;

//-------------------------------------------------------------------------------------------------

procedure THekatonResults.SetInputFileName(const Value: TFileName);
  begin
  if FileExists(value) then
     begin
     FInputFileName := value;
     LoadHeaderFromFile;
     end;
  end;

//-------------------------------------------------------------------------------------------------  

procedure THekatonResults.SetLongLastMark(value : boolean);
  begin
  FLongLastMark := value;
  end;

//-------------------------------------------------------------------------------------------------

procedure THekatonResults.SetMarks(i, j: integer; const Value: boolean);
  begin
  if (i >= 0) and (i < FGroups.count) and (j >= 0) and (j < FColumns.Count) then
//     FMarks[i, j] := value;
     (FGroups.Objects[i] as TResultGroup).ColMarks[j] := value;
  end;

//-------------------------------------------------------------------------------------------------

procedure THekatonResults.SetMaxSequenceSize(value : integer);
  begin
  FMaxSequenceSize := value;
  end;

//-------------------------------------------------------------------------------------------------

procedure THekatonResults.SetSequenceFunction(const Value: TResultTransf);
  begin
  FSequenceFunction := value;
  end;

//-------------------------------------------------------------------------------------------------

procedure THekatonResults.SetSequenceGrouping(const Value: TResultGrouping);
  begin
  FSequenceGrouping := value;
  end;

//-------------------------------------------------------------------------------------------------

procedure THekatonResults.SetSequenceMarks(index: integer;   const Value: boolean);
  begin
  if (index >= 0) and (index < FSequencesSize) then
     FSequenceMarks[index] := value;
  end;

//-------------------------------------------------------------------------------------------------

procedure THekatonResults.SetSequenceSize(const Value: integer);
  begin
  FSequencesSize := value;
  SetLength(FSequenceMarks, FSequencesSize);
  end;

//-------------------------------------------------------------------------------------------------

procedure THekatonResults.SetSubsetCount(const Value: integer);
  begin
  FSubsetCount := value;
  SetLength(FSubsetMarks, FSubsetCount);
  end;

//-------------------------------------------------------------------------------------------------

procedure THekatonResults.SetSubsetMarks(index: integer; const Value: boolean);
  begin
  if (index >= 0) and (index < FSubsetCount) then
     FSubsetMarks[index] := value;
  end;

//-------------------------------------------------------------------------------------------------

procedure THekatonResults.SetupChartAsOutput(chart: TChart; owner : TComponent = nil);
  var
    i, j : integer;
    XTmpSeries : TLineSeries;
  begin
  FChartAsOutput := true;
  if chart = nil then
     FChart := TChart.Create(owner)
  else
     FChart := chart;
  Fchart.SeriesList.Clear;
  j := 0;
  for i := 0 to FSubsetCount - 1 do
    if FSubSetMarks[i] then j := j + 1;
  for i := 0 to (FGroups.Count * j) - 1 do
    begin
    XTmpSeries := TLineSeries.Create(FChart.Owner);
    XTmpSeries.Clear;
    Fchart.AddSeries(XTmpSeries);
    end;
  end;

//-------------------------------------------------------------------------------------------------

procedure THekatonResults.SetupFileAsOutput(FileName: TFileName);
  begin
  FFileAsOutput := true;
  FOutputFileName := FileName;
  FOutputStrList.Clear;
  //save both headers
  end;

//-------------------------------------------------------------------------------------------------

procedure THekatonResults.SetUseSequenceBreak (value : boolean);
  begin
  FUseSequenceSeparator := value;
  if value and (FMaxSequenceSize > 0) then
     SetSequenceSize(FMaxSequenceSize);
  end;

//-------------------------------------------------------------------------------------------------


end.
