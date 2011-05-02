{
  @abstract(Unit with the description of the finite automata for the scanning
    of the logic program)
  @author(Rafa "Passarel" Borges <passarel@gmail.com)
  @created(Pelotas, September, 2003)
  @lastmod(Porto Alegre, June 21, 2006)
}
unit UnLogScanAut; //359 lines

interface

uses Grids;

{------------------------------------------------------------------------------}

{Enumeration of the different kind of states on the automata}
type TTipoEstados = (TENaoFinal, TEIdentificador, TEOperador,
                     TEComentario, TESeparador, TESeparadorLi, TEOutro);

{------------------------------------------------------------------------------}

{
@abstract(Class that encapsulates an finite automata to recognition of tokens)

The class @name encapsulates a finte automata, that realize the recognition of
tokens for the lexical analysis of a logic program. This automata load the
definition of the states and the transitition from a file.
The transitions are defined as a incidence matrix, where the lines represents the
states and the colums represents the transition realized for each ASCII character.
}
type TAutomata = class
  private
    //Number of states of the automata
    FEstados : word;

    //Index indicating the kind of each state
    FFinais : array of TTipoEstados;

    //Incidence matrix representing the transitions of the automata
    FDado : array of array of integer;

    //String kept from sucessive processing of characters
    FStringAtual : String;

    //Active state on the  automata
    FEstadoAtual : word;

    //Realizes the processing of the first character of a string
    //@param(str: string to be processed)
    //@param(state: initial state of the process)
    //returs(final state after the processing)
    function next  (str : string; state : integer) : integer;

    //Function to analyse a specific string
    //@param (str: string to be analysed)
    //@param (state : initial state of the analysis)
    //@return(index of the state on the end of the analysis)
    function stage (str : string; state : integer) : integer;

    //Function to get the kind of the state passed as parameter
    //@param(index of the state to get the kind)
    //@return(The kind of the state or "TENaoFinal" if the index passed is -1)
    function final (state : integer) : TTipoEstados;

  public
    //Constructor of the class
    //@param(Estados : number of states of the automata)
    constructor Create(Estados: word);

    //Function to analyse a specific string
    //@param (str: string to be analysed)
    //@return(Kind of the state on the end of the analysis)
    function accept(str : string) : TTipoEstados;

    //procedure to edit a transition on the automata
    //@param(caracter: character related to the transition)
    //@param(state: state on the beggining of the transition(source))
    //@param(prox: state on the end of the transition(target))
    procedure SetAutomata(caracter: char; state, prox : integer);

    //procedure to edit the kind of a state
    //@param(estado: state to be edited)
    //@param(valor: kind to be assigned to the state)
    procedure SetFinais (estado: integer; valor : TTipoEstados);

    //(procedure to perform a visual representation of the automata (as a grid))
    //@param(grid: StringGrid to be filled with the data of the automata)
    procedure PreencheGrid(grid : TStringGrid);

    //Function to get the number of states on the automata
    //@return(Number of states)
    function NEstados : integer;

    //Process an individual character on the automata
    //@param(caracter : character to be processed)
    //return(-1, if the automata leads to a new state, or the index of the last state, case contrary)
    function ExecutaChar(caracter : char) : integer;

    //Load the automata from a file
    //@param(FileName : Name of the file)
    //@return(True if the operation was succeded)
    function LoadFromFile(FileName : String) : boolean;

    //Save the automata to a file
    //@param(FileName : Name of the file)
    //@return(True if the operation was succeded)
    function SaveToFile(FileName : String) : boolean;

    //Load the automata from a text file
    //@param(FileName : Name of the file)
    //@return(True if the operation was succeded)
    function LoadFromTextFile(FileName : String) : boolean;

    //Save the automata to a text file
    //@param(FileName : Name of the file)
    //@return(True if the operation was succeded)
    function SaveToTextFile  (FileName : String) : boolean;

    //reset the automata, generating a new one
    //@param(Number of states of the new automata)
    procedure ResetAut(Estados : word);

    //Reset the variables for a new execution of the automata
    procedure ResetVal;

    //Get the string kept on the last processings
    function  GetStr : string;

    //Reset the string kept on the last processings
    procedure ResetStr;

  end;


{------------------------------------------------------------------------------}

implementation

uses SysUtils;

{------------------------------------------------------------------------------}

{ TAutomata }

{------------------------------------------------------------------------------}

function TAutomata.accept(str: string): TTipoEstados;
  begin
  result := Final(stage(str, 0));
  end;

{------------------------------------------------------------------------------}

constructor TAutomata.Create(Estados: word);
  begin
  inherited Create;
  ResetAut(Estados);
  end;

{------------------------------------------------------------------------------}

function TAutomata.ExecutaChar(caracter: char): integer;
  var prox : integer;
  begin
  prox := FDado[ord(caracter), FEstadoAtual];
  if prox >= 0 then
     begin
     FStringAtual := FStringAtual + caracter;
     FEstadoAtual := prox;
     result := -1;
     end
  else
     result := ord(FFinais[FEstadoAtual]);
  end;

{------------------------------------------------------------------------------}

function TAutomata.final(state: integer): TTipoEstados;
  begin
  if state < 0 then
     result := TENaoFinal
  else
     result := FFinais[state];
  end;

{------------------------------------------------------------------------------}

function TAutomata.GetStr: string;
  begin
  result := FStringAtual;
  end;

{------------------------------------------------------------------------------}

function TAutomata.LoadFromFile(FileName: String): boolean;
  var arq : file;
      setor : array[0..511] of byte;
      quant : integer;
      passo : byte;
      ch    : char;
      i, n, STa, ST : word;
      rrr   : boolean;

{//passo = 0, 1 : Lê nº de estados (n : word);                            }
{//passo = 2 : Lê n  bytes, representando o tipo de estado               }
{//passo = 3, 4, 5, 6, 7: Lê os registros, sendo:                       }
{//                 3:     1º byte referente ao caracter               }
{//                 4 e 5: 2º e 3º bytes referentes ao estado atual   }
{//                 6 e 7: 4º e 5º bytes referentes ao próximo estado}
{//as células não definidos apresentarão dado -1, por default.}

  begin
  rrr := false;
  ch := chr(0);
  ST := 0;
  STa := 0;
  try
    assign(arq, FileName);
    reset(arq, 1);
    quant := 512;
    passo := 0;
    n := 0;
    while not EOF(arq) do
      begin
      BlockRead(arq, setor, 512, quant);
      for i := 0 to quant - 1 do
        begin
        case passo of
          0: begin
             n := setor[i];
             passo := passo + 1;
             end;
          1: begin
             n := n + (setor[i] * 256);
             passo := passo + 1;
             ResetAut(n);
             STa := 0;
             end;
          2: begin
             SetFinais(STa, TTipoEstados(setor[i]));
             STa := STa + 1;
             if STa >= n then
                passo := passo + 1;
             end;
          3: begin
             ch := chr(setor[i]);
             passo := passo + 1;
             end;
          4: begin
             STa := setor[i];
             passo := passo + 1;
             end;
          5: begin
             STa := STa + (setor[i] * 256);
             passo := passo + 1;
             end;
          6: begin
             ST := setor[i];
             passo := passo + 1;
             end;
          7: begin
             ST := ST + (setor[i] * 256);
             passo := passo - 4;
             SetAutomata(ch, STa, ST);
             end;
          end;
        end;
      end;
    rrr := true;
  finally
    close(arq);
    result := rrr;
    end;
  end;

{------------------------------------------------------------------------------}

function TAutomata.LoadFromTextFile(FileName: String): boolean;
  var F : Text;
      NB, NE : word;
      i , j  : word;
      n    : integer;

  begin
  assign(F, FileName);
  reset(F);
  readln(F, NB, NE);
  ResetAut(NE);
  for i := 0 to NE - 1 do
    begin
    read(F, n);
    FFinais[i] := TTipoEstados(n);
    end;
  for i := 0 to NB - 1 do
    for j := 0 to NE - 1 do
      begin
      read(F, n);
      SetAutomata(chr(i), j, n);
      end;
  result := true;
  close(F);
  end;

{------------------------------------------------------------------------------}

function TAutomata.NEstados: integer;
  begin
  result := FEstados;
  end;

{------------------------------------------------------------------------------}

function TAutomata.next(str: string; state: integer): integer;
  begin
  if state < 0 then
     result := state
  else
     result := FDado[ord(str[1]), state];
  end;

{------------------------------------------------------------------------------}

procedure TAutomata.PreencheGrid(grid: TStringGrid);
  var i, j : integer;
  begin
  grid.ColCount := FEstados + 1;
  grid.RowCount := 257;
  for i := 1 to 256 do
    grid.Cells[0, i] := chr(i - 1) + ' - ('+ IntToStr(i - 1) + ')' ;
  for j := 1 to FEstados do
    begin
    grid.Cells[j, 0] := IntToStr(j - 1);
    case FFinais[j - 1] of
      TENaoFinal        : grid.Cells[j, 0] := grid.Cells[j, 0];
      TEIdentificador   : grid.Cells[j, 0] := grid.Cells[j, 0] + ' (Ident.)';
      TEOperador        : grid.Cells[j, 0] := grid.Cells[j, 0] + ' (Oper.)';
      TEComentario      : grid.Cells[j, 0] := grid.Cells[j, 0] + ' (Coment.)';
      TESeparador       : grid.Cells[j, 0] := grid.Cells[j, 0] + ' (Separ.)';
      TESeparadorLi     : grid.Cells[j, 0] := grid.Cells[j, 0] + ' (Linha)';
      TEOutro           : grid.Cells[j, 0] := grid.Cells[j, 0] + ' (Final)';
      end;
    end;

  for i := 1 to 256 do
    for j := 1 to FEstados do
      grid.Cells[j, i] := IntToStr(FDado[i - 1, j - 1]);
  end;

{------------------------------------------------------------------------------}

procedure TAutomata.ResetAut(Estados: word);
  var i, j : integer;
  begin
  FEstados := Estados;
  SetLength(FFinais, Estados);
  SetLength(FDado, 256, Estados);
  for i := 0 to Estados - 1 do
    begin
    FFinais[i] := TENaoFinal;
    for j := 0 to 255 do
      FDado[j, i] := -1;
    end;
  end;

{------------------------------------------------------------------------------}

procedure TAutomata.ResetStr;
  begin
  FStringAtual := '';
  end;

{------------------------------------------------------------------------------}

procedure TAutomata.ResetVal;
  begin
  FStringAtual := '';
  FEstadoAtual := 0;
  end;

{------------------------------------------------------------------------------}

function TAutomata.SaveToFile(FileName: String): boolean;
  var f : file;
      b : byte;
      w, i : word;

  begin
  assign(F, FileName);
  rewrite(F, 1);
  w := FEstados;
  BlockWrite(F, w, 2);
  for i := 0 to FEstados - 1 do
    begin
    b := ord(FFinais[i]);
    BlockWrite(F, b, 1);
    end;
//  j := 0;
  for b := 0 to 255 do
    for i := 0 to FEstados - 1 do
      if FDado[b, i] >= 0 then
         begin
         w := FDado[b, i];
         BlockWrite(F, b, 1);
         BlockWrite(F, i, 2);
         BlockWrite(F, w, 2);
//         j := j + 1;
         end;
  close(F);
  result := true;
  end;

{------------------------------------------------------------------------------}

function TAutomata.SaveToTextFile(FileName: String): boolean;
  var F : Text;
      i , j  : word;
  begin
  assign(F, FileName);
  rewrite(F);
  writeln(F, 255, ' ',  FEstados);

  for i := 0 to FEstados - 1 do
    write(F, ord(TTipoEstados(FFinais[i])), ' ');

  writeln(F);

  for i := 0 to 255 do
    begin
    for j := 0 to FEstados - 1 do
      begin
      write(F, FDado[i, j], ' ');
      end;
    writeln(F);
    end;

  result := true;
  close(F);
  end;

{------------------------------------------------------------------------------}

procedure TAutomata.SetAutomata(caracter: char; state, prox: integer);
  begin
  if state >= 0 then
     FDado[ord(caracter), state] := prox;
  end;

{------------------------------------------------------------------------------}

procedure TAutomata.SetFinais(estado: integer; valor: TTipoEstados);
  begin
  if (estado >= 0) and (estado < FEstados) then
     FFinais[estado] := valor;
  end;

{------------------------------------------------------------------------------}

function TAutomata.stage(str: string; state: integer): integer;
  var str2 : string;
  begin
  if (str = '') or (state < 0) then
     result := state
  else
     begin
     str2   := copy(str, 2, length(str) - 1);
     result := stage(str2, next(str, state));
     end;
  end;

{------------------------------------------------------------------------------}

end.
