{
  @abstract(Unit that contains a set of activation functions and their derivates)
  @author(Rafa "Passarel" Borges <passarel@gmail.com)
  @created(Porto Alegre, 2006)
  @lastmod(London, April, 2008)
}
unit UnFunctions; 

interface
//Activation Function: Sigmoidal Logarithmic
function logsig(x : double; p : array of double) : double;
//Derivated of Activation Function: Sigmoidal Logarithmic
function logsig_linha(x : double; p : array of double) : double;

//Activation Function: Sigmoidal Hyperbolic Tangent
function tansig(x : double; p : array of double) : double;
//Derivated of Activation Function: Sigmoidal Hyperbolic Tangent
function tansig_linha(x : double; p : array of double) : double;

//Activation Function: Bipolar Sigmoidal Logarithmic
function BiLogsig(x : double; p : array of double) : double;
//Derivated of Activation Function: Bipolar Sigmoidal Logarithmic
function BiLogsig_linha(x : double; p : array of double) : double;

//Activation Function: Threshold
function Threshold(x : double; p : array of double) : double;
//Derivated of Activation Function: Threshold
function Threshold_Linha(x : double; p : array of double) : double;

//Activation Function: Bipolar Threshold
function BiThreshold(x : double; p : array of double) : double;
//Derivated of Activation Function: Bipolar Threshold
function BiThreshold_Linha(x : double; p : array of double) : double;

//Activation Function: Linear
function Linear(x : double; p : array of double) : double;
//Derivated of Activation Function: Linear
function Linear_Linha(x : double; p : array of double) : double;

//Activation Function: Other Tests
function PolynomialA(x : double; p : array of double) : double;
//Derivated of Activation Function: Other Tests
function PolynomialA_Linha(x : double; p : array of double) : double;

//Activation Function: Other Tests
function PolynomialB(x : double; p : array of double) : double;
//Derivated of Activation Function: Other Tests
function PolynomialB_Linha(x : double; p : array of double) : double;

//Activation Function: Other Tests
function TrigonometricA(x : double; p : array of double) : double;
//Derivated of Activation Function: Other Tests
function TrigonometricA_Linha(x : double; p : array of double) : double;

//Activation Function: Other Tests
function CrazyA(x : double; p : array of double) : double;
//Derivated of Activation Function: Other Tests
function CrazyA_Linha(x : double; p : array of double) : double;


//Simple Delay Function
function dl_1(delay : integer; data : double) : double;



{------------------------------------------------------------------------------}

implementation
//Needs to be verified ???
uses math;

function logsig(x : double; p : array of double) : double;
  begin
  result := 1 / (1 + exp(-x));
  end;

function logsig_linha(x : double; p : array of double) : double;
  begin
  result := exp(-x) / ((1 + exp(-x)) * (1 + exp(-x)));
  end;

function tansig(x : double; p : array of double) : double;
  begin
  result := tanh(x);
  end;

function tansig_linha(x : double; p : array of double) : double;
  begin
  result := 1 - (tanh(x) * tanh(x));
  end;

function BiLogsig(x : double; p : array of double) : double;
  begin
  result := (2 / (1 + exp(-p[0] * x))) - 1
  end;

function BiLogsig_linha(x : double; p : array of double) : double;
  begin
  result := 2 * (p[0] * exp(-p[0] * x)) / ((1 + exp(-p[0]* x)) * (1 + exp(-p[0]* x)));
  end;

function Threshold(x : double; p : array of double) : double;
  begin
  if x >= 0 then
     result := 1
  else
     result := 0;
  end;

function Threshold_Linha(x : double; p : array of double) : double;
  begin
  result := 1
  end;

function BiThreshold(x : double; p : array of double) : double;
  begin
  if x >= 0 then
     result := 1
  else
     result := -1;
  end;

function BiThreshold_Linha(x : double; p : array of double) : double;
  begin
  result := 1
  end;

function Linear(x : double; p : array of double) : double;
  begin
  result := p[0] * x;
  end;

function Linear_Linha(x : double; p : array of double) : double;
  begin
  result := p[0];
  end;


function PolynomialA(x : double; p : array of double) : double;
  begin
  result := (((x + p[0]) * (x + p[0]))/ (2 * p[1])) - 1;
  if x > 0 then
    result := -result;
  end;

function PolynomialA_Linha(x : double; p : array of double) : double;
  begin
  result := (x - p[0]) / p[1];
  end;

function PolynomialB(x : double; p : array of double) : double;
  begin
  end;

function PolynomialB_Linha(x : double; p : array of double) : double;
  begin
  end;

function TrigonometricA(x : double; p : array of double) : double;
  begin
  end;

function TrigonometricA_Linha(x : double; p : array of double) : double;
  begin
  end;

function CrazyA(x : double; p : array of double) : double;
  begin
  if abs(x) < 0.2 then result := x
  else if abs(x) < 2 then result := ((x - 0.2) * (8 / 18)) + 0.2
  else if x > 0 then result := 1
  else result := -1;
  end;

function CrazyA_Linha(x : double; p : array of double) : double;
  begin
  if abs(x) < 0.2 then result := 1
  else if abs(x) < 2 then result := (8 / 18)
  else result := 0;
  end;


function dl_1(delay : integer; data : double) : double;
  begin
  if delay = 1 then
     result := data
  else
     result := 0;
  end;

{------------------------------------------------------------------------------}

end.
