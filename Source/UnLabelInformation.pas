unit UnLabelInformation;

interface

uses SimpleGraph;

{
@abstract(Class with the information of a label that defines a group of states)
}

type TLabelInformation = class
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

implementation

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


end.
