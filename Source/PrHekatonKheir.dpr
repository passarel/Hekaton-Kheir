program PrHekatonKheir;

uses
  Forms,
  UnDialTrEvol in 'UnDialTrEvol.pas' {FmTrEvol},
  UnFunctions in 'UnFunctions.pas',
  UnLabelInformation in 'UnLabelInformation.pas',
  UnLogSing in 'UnLogSing.pas',
  UnLogStruct in 'UnLogStruct.pas',
  UnMMStructure in 'UnMMStructure.pas',
  UnMMVisualNew in 'UnMMVisualNew.pas' {FmMMMain},
  UnNetBehaviour in 'UnNetBehaviour.pas',
  UnNetRep in 'UnNetRep.pas',
  UnRafaAux2007 in 'UnRafaAux2007.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Hekaton Kheir (SE version)';
  Application.CreateForm(TFmMMMain, FmMMMain);
  Application.Run;
end.
