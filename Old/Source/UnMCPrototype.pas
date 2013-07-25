unit UnMCPrototype;

interface

uses
  Contnrs;

type TGenMCModule = class
  private
    //Field storing an integer identifying the method
    FTag : integer;
    //Field storing the number of Inputs in the module
    FInputCount  : integer;
    //Field storing the number of Outputs in the module
    FOutputCount : integer;

    //Mutator to the field FTag
    //@param(value: new value to the field)
    procedure SetTag(value : integer);

    //Mutator to the field FInputCount
    //@param(value: new value to the field)
    procedure SetInputCount(value : integer);

    //Mutator to the field FOutputCount
    //@param(value: new value to the field)
    procedure SetOutputCount(value : integer);

    //Accessor to the field FTag
    //@return(The value of the field)
    function GetTag : integer;

    //Accessor to the field FInputCount
    //@return(The value of the field)
    function GetInputCount : integer;

    //Accessor to the field FOutputCount
    //@return(The value of the field)
    function GetOutputCount : integer;

  public
    //Property representing an integer identifying the method
    property Tag : integer read GetTag write SetTag;

    //Property representing the number of Inputs in the module
    property InputCount : integer read GetInputCount write SetInputCount;

    //Property representing the number of Outputs in the module
    property OutputCount : integer read GetOutputCount write SetOutputCount;

    //Abstract method to get the array the values in the input of a module
    //@param(out InArray: array to store the values)
    //@return(true if the method was properly executed, false otherwise)
    function GetInput   (out InArray : array of double) : boolean; virtual; abstract;

    //Abstract method to apply an array the values to the input of a module
    //@param(InArray: array informing the values to be applied)
    //@return(true if the method was properly executed, false otherwise)
    function ApplyInput (InArray : array of double) : boolean; virtual; abstract;

    //Abstract method to get the array the values in the output of a module
    //@param(out OutArray: array to store the values)
    //@return(true if the method was properly executed, false otherwise)
    function GetOutput  (out OutArray : array of double) : boolean; virtual; abstract;

    //Abstract method to apply an array the values to the output of a module (useful for learning)
    //@param(outArray: array informing the values to be applied)
    //@return(true if the method was properly executed, false otherwise)
    function ApplyOutput(OutArray : array of double) : boolean; virtual; abstract;

    //Abstract method to return an string from the meth
    function GetString : string; virtual; abstract;

    //Constructor of the class
    //@param(inputs: number of inputs of the module)
    //@param(outputs: number of outputs of the module)
    constructor Create (inputs, outputs : integer);

  end;

type TMCEnvironment = class
  private
    //Field storing the modules
    FModulesList : TObjectList;

    //Field storing how do the modules communicate between each other in a Synchronous execution
    FCommMatrix : array of array of integer;

    //Field storing how many communication links are established
    FCommMatrixSize : integer;

    //Change the module in a specified position of the list
    //@param(index: position of the module to be changed. If index is greater than or equals to the
    //size of the list, then the module will be inserted in the end of the list
    //@param(value: module to replace the existing one)
    procedure SetModule(index : integer; value : TGenMCModule);

    //Returns the module in a specified position of the list
    //@param(index: position of the desired module)
    //@return(module in the specified position, or nil if the position is out of the list )
    function  GetModule(index : integer) : TGenMCModule;

  public
    //Property representing the modules
    property Module[index : integer] : TGenMCModule read GetModule write SetModule;

    //Procedure adding a module in the end of the list
    //@param(module: module to be added)
    procedure AddModule(module : TGenMCModule);

    //Delete a method in a specified position
    //@param(index: position of the module to be deleted)
    //@return(true if the method was properly executed, false otherwise)
    function DeleteModule(index : integer) : boolean;

    //Add a communication link between two modules
    //@param(inModule : source module of the communication)
    //@param(inIndex : index of the output of the source module)
    //@param(outModule : target module of the communication)
    //@param(outIndex : index of the input of the target module)
    //@return(true if the method was properly executed, false otherwise)
    function AddCommItem(InModule, InIndex, OutModule, OutIndex : integer) : boolean;

    //Delete a communication link between two modules
    //@param(inModule : source module of the communication)
    //@param(inIndex : index of the output of the source module)
    //@param(outModule : target module of the communication)
    //@param(outIndex : index of the input of the target module)
    //@return(true if the method was properly executed, false otherwise)
    function DelCommItem(InModule, InIndex, OutModule, OutIndex : integer) : boolean;

    //Method to return the number of communication links
    //@return(The number of communication links)
    function GetCommItemCount : integer;

    //Performs a single syncronous step, according to the communication matrix
    //@return(true if the method was properly executed, false otherwise)
    function SyncStep : boolean;

    //Method to return the tag from a specified Module
    //@param(module : index of the desired module)
    //@return(tag from the desired module)
    function GetTagFromModule(module : integer) : integer;

    //Method to return the index of the first object with a specified tag
    //@param(tag: the desired tag)
    //@return(The index of the first object which tag is equals to the specified one, or -1 if no object is found)
    function GetModuleIndexFromTag(tag : integer) : integer;

    //Method to get the array of values in the input of a specified module
    //@param(module : index of the desired module)
    //@param(out InArray: array to store the values)
    //@return(true if the method was properly executed, false otherwise)
    function GetInputFromModule  (module : integer; out InArray : array of double) : boolean;

    //Method to apply an array of values to the input of a specified module
    //@param(module : index of the desired module)
    //@param(InArray: array informing the values to be applied)
    //@return(true if the method was properly executed, false otherwise)
    function ApplyInputToModule  (module : integer; InArray : array of double) : boolean;

    //Method to get the array of values in the output of a specified module
    //@param(module : index of the desired module)
    //@param(out OutArray: array to store the values)
    //@return(true if the method was properly executed, false otherwise)
    function GetOutputFromModule (module : integer; out OutArray : array of double) : boolean;

    //Method to apply an array of values to the output of a specified module
    //@param(module : index of the desired module)
    //@param(OutArray: array informing the values to be applied)
    //@return(true if the method was properly executed, false otherwise)
    function ApplyOutputToModule (module : integer; OutArray : array of double) : boolean;

  end;

  

implementation

//--------------------------------------------------------------------------------------------------

{TGenMCModule}

//--------------------------------------------------------------------------------------------------

constructor TGenMCModule.Create (inputs, outputs : integer);
  begin
  FInputCount := inputs;
  FOutputCount := outputs;
  FTag := 0;
  end;

//--------------------------------------------------------------------------------------------------

function TGenMCModule.GetInputCount : integer;
  begin
  result := FInputCount;
  end;

//--------------------------------------------------------------------------------------------------

function TGenMCModule.GetOutputCount : integer;
  begin
  result := FOutputCount;
  end;

//--------------------------------------------------------------------------------------------------

function TGenMCModule.GetTag : integer;
  begin
  result := FTag;
  end;

//--------------------------------------------------------------------------------------------------

procedure TGenMCModule.SetInputCount(value : integer);
  begin
  if value >= 0 then
     FInputCount := value;
  end;

//--------------------------------------------------------------------------------------------------

procedure TGenMCModule.SetOutputCount(value : integer);
  begin
  if value >= 0 then
     FOutputCount := value;
  end;

//--------------------------------------------------------------------------------------------------

procedure TGenMCModule.SetTag(value : integer);
  begin
  FTag := value;
  end;

//--------------------------------------------------------------------------------------------------

 { TMCEnvironment }

//--------------------------------------------------------------------------------------------------

function TMCEnvironment.AddCommItem(InModule, InIndex, OutModule, OutIndex : integer) : boolean;
  begin
  if ((InModule  >= 0) and (InModule  < FModulesList.Count)) and
     ((InIndex   >= 0) and (InModule  < GetModule(InModule).OutputCount)) and
     ((OutModule >= 0) and (OutModule < FModulesList.Count)) and
     ((OutIndex  >= 0) and (OutModule < GetModule(OutModule).InputCount)) then
     begin
     FCommMatrixSize := FCommMatrixSize + 1;
     SetLength(FCommMatrix, FCommMatrixSize, 4);
     FCommMatrix[FCommMatrixSize, 0] := InModule;
     FCommMatrix[FCommMatrixSize, 1] := InModule;
     FCommMatrix[FCommMatrixSize, 2] := InModule;
     FCommMatrix[FCommMatrixSize, 3] := InModule;
     result := true;
     end
  else
     result := false;
  end;

//--------------------------------------------------------------------------------------------------

procedure TMCEnvironment.AddModule(module : TGenMCModule);
  begin
  FModulesList.Add(module);
  end;

//--------------------------------------------------------------------------------------------------

function TMCEnvironment.ApplyInputToModule  (module : integer; InArray : array of double) : boolean;
  begin
  if (module > 0) and (module < FModulesList.Count) then
     begin
     result := GetModule(module).ApplyInput(InArray);
     end
  else
     result := false;
  end;

//--------------------------------------------------------------------------------------------------

function TMCEnvironment.ApplyOutputToModule (module : integer; OutArray : array of double) : boolean;
  begin
  if (module > 0) and (module < FModulesList.Count) then
     begin
     result := GetModule(module).ApplyOutput(OutArray);
     end
  else
     result := false;
  end;

//--------------------------------------------------------------------------------------------------

function TMCEnvironment.DelCommItem(InModule, InIndex, OutModule, OutIndex : integer) : boolean;
  var
    i, j : integer;
  begin
  j := 0;
  for i := 0 to FCommMatrixSize - 1 do
    begin
    if ((InModule  <> FCommMatrix[i, 0]) or (InIndex  <> FCommMatrix[i, 1]) and
        (OutModule <> FCommMatrix[i, 2]) or (OutIndex <> FCommMatrix[i, 3])) then
       begin
       if (i > j) then
          begin
          FCommMatrix[j, 0] := FCommMatrix[i, 0];
          FCommMatrix[j, 1] := FCommMatrix[i, 1];
          FCommMatrix[j, 2] := FCommMatrix[i, 2];
          FCommMatrix[j, 3] := FCommMatrix[i, 3];
          end;
       j := j + 1;
       end;
    end;
  if j < FCommMatrixSize then
     begin
     FCommMatrixSize := j;
     SetLength(FCommMatrix, FCommMatrixSize, 4);
     result := true;
     end
  else
     result := false;
  end;

//--------------------------------------------------------------------------------------------------

function TMCEnvironment.DeleteModule(index : integer) : boolean;
  begin
  if (index > 0) and (index < FModulesList.Count) then
     begin
     FModulesList.Delete(index);
     result := true;
     end
  else
     result := false;
  end;

//--------------------------------------------------------------------------------------------------

function TMCEnvironment.GetCommItemCount : integer;
  begin
  result := FCommMatrixSize;
  end;

//--------------------------------------------------------------------------------------------------

function TMCEnvironment.GetInputFromModule  (module : integer; out InArray : array of double) : boolean;
  begin
  if (module > 0) and (module < FModulesList.Count) then
     begin
     result := GetModule(module).GetInput(InArray);
     end
  else
     result := false;
  end;

//--------------------------------------------------------------------------------------------------

function  TMCEnvironment.GetModule(index : integer) : TGenMCModule;
  begin
  result := nil;
  if (index > 0) and (index < FModulesList.Count) then
     if FModulesList.Items[index] is TGenMCModule then
        result  := FModulesList.Items[index] as TGenMCModule;
  end;

//--------------------------------------------------------------------------------------------------

function TMCEnvironment.GetModuleIndexFromTag(tag : integer) : integer;
  var
    found : boolean;
    i : integer;
  begin
  found := false;
  i := 0;
  while not found and (i < FModulesList.Count) do
    begin
    if GetModule(i).Tag = tag then
       found := true
    else
       i := i + 1;
    end;
  if found then
     result := i
  else
     result := -1;
  end;

//--------------------------------------------------------------------------------------------------

function TMCEnvironment.GetOutputFromModule (module : integer; out OutArray : array of double) : boolean;
  begin
  if (module > 0) and (module < FModulesList.Count) then
     begin
     result := GetModule(module).GetOutput(OutArray);
     end
  else
     result := false;
  end;

//--------------------------------------------------------------------------------------------------

function TMCEnvironment.GetTagFromModule(module : integer) : integer;
  begin
  if (module > 0) and (module < FModulesList.Count) then
     result := GetModule(module).Tag
  else
     result := 0;
  end;

//--------------------------------------------------------------------------------------------------

procedure TMCEnvironment.SetModule(index : integer; value : TGenMCModule);
  begin
  if (index > 0) and (index < FModulesList.Count) then
     FModulesList.Items[index] := value;
  end;

//--------------------------------------------------------------------------------------------------

function TMCEnvironment.SyncStep : boolean;
  var
    InputArrays, OutputArrays : array of array of double;
    i, j : integer;
  begin
  try
    SetLength(InputArrays, FModulesList.Count);
    SetLength(OutputArrays, FModulesList.Count);
    for i := 0 to FModulesList.Count - 1 do
      begin
      SetLength(InputArrays[i],  GetModule(i).InputCount);
      for j := 0 to GetModule(i).InputCount - 1 do
        InputArrays[i, j] := 0;
      SetLength(OutputArrays[i], GetModule(i).OutputCount);
      for j := 0 to GetModule(i).OutputCount - 1 do
        OutputArrays[i, j] := 0;
      end;
    for i := 0 to FModulesList.count - 1 do
      GetModule(i).GetOutput(OutputArrays[i]);
    for i := 0 to FCommMatrixSize - 1 do
      begin
      InputArrays[FCommMatrix[i, 2], FCommMatrix[i, 3]] :=
                                                  OutputArrays[FCommMatrix[i, 0], FCommMatrix[i, 1]];
      end;
    for i := 0 to FModulesList.count - 1 do
      GetModule(i).ApplyInput(InputArrays[i]);

    for i := 0 to FModulesList.Count - 1 do
      begin
      for j := 0 to GetModule(i).InputCount - 1 do
        InputArrays[i, j] := 0;
      for j := 0 to GetModule(i).OutputCount - 1 do
        OutputArrays[i, j] := 0;
      end;
    for i := 0 to FModulesList.count - 1 do
      GetModule(i).GetInput(InputArrays[i]);
    for i := 0 to FCommMatrixSize - 1 do
      begin
      OutputArrays[FCommMatrix[i, 0], FCommMatrix[i, 1]] :=
                                                  InputArrays[FCommMatrix[i, 2], FCommMatrix[i, 3]];
      end;
    for i := 0 to FModulesList.count - 1 do
      GetModule(i).ApplyOutput(OutputArrays[i]);
    result := true;
  except
    result := false;
    end;
  end;

//--------------------------------------------------------------------------------------------------



end.
