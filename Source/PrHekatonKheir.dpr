program PrHekatonKheir;

uses
  Forms,
  UnAbout in 'UnAbout.pas' {FmAbout},
  UnChVis in 'UnChVis.pas' {FmChVis},
  UnDialChConf in 'UnDialChConf.pas' {FmChConf},
  UnDialColumn in 'UnDialColumn.pas' {FmColDial},
  UnDialCross in 'UnDialCross.pas' {FmCross},
  UnDialDinPhil in 'UnDialDinPhil.pas' {FmPhil},
  UnDialDoubleFloat in 'UnDialDoubleFloat.pas' {FmDoubleFloat},
  UnDialEdit in 'UnDialEdit.pas' {FmEdit},
  UnDialFile in 'UnDialFile.pas' {FmFileConf},
  UnDialMerge in 'UnDialMerge.pas' {FmMerge},
  UnDialNode in 'UnDialNode.pas' {FmNode},
  UnDialTrEvol in 'UnDialTrEvol.pas' {FmTrEvol},
  UnDialWizards in 'UnDialWizards.pas' {FmWizards},
  UnFmMain in 'UnFmMain.pas' {MainForm},
  UnFmMuddyChildren in 'UnFmMuddyChildren.pas' {FmMuddyChildren},
  UnFunct in 'UnFunct.pas' {FmFunct},
  UnFunctions in 'UnFunctions.pas',
  UnLogForm in 'UnLogForm.pas' {FmLogic},
  UnLogGen in 'UnLogGen.pas',
  UnLogPars in 'UnLogPars.pas',
  UnLogScan in 'UnLogScan.pas',
  UnLogScanAut in 'UnLogScanAut.pas',
  UnLogSing in 'UnLogSing.pas',
  UnLogStruct in 'UnLogStruct.pas',
  UnMCDiningTable in 'UnMCDiningTable.pas',
  UnMCPhilosopher in 'UnMCPhilosopher.pas',
  UnMCPrototype in 'UnMCPrototype.pas' {Form2},
  UnMMDialTransition in 'UnMMDialTransition.pas' {FmMMTransition},
  UnMMDialVar in 'UnMMDialVar.pas' {FmMMDialVar},
  UnMMStructure in 'UnMMStructure.pas',
  UnMMVisualNew in 'UnMMVisualNew.pas' {FmMMMain},
  UnNetBehaviour in 'UnNetBehaviour.pas',
  UnNetRep in 'UnNetRep.pas',
  UnNetVisual in 'UnNetVisual.pas',
  UnPedagogical in 'UnPedagogical.pas' {FmPedagogical},
  UnRafaAux2007 in 'UnRafaAux2007.pas',
  UnRafaVisual in 'UnRafaVisual.pas',
  UnResults in 'UnResults.pas',
  UnWeightVis in 'UnWeightVis.pas' {FmWeightVis},
  UnPhil in 'UnPhil.pas' {FmOldPhil},
  UnLineComp in 'UnLineComp.pas' {FmLineComp},
  UnLabelInformation in 'UnLabelInformation.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Hekaton Kheir';
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TFmEdit, FmEdit);
  Application.CreateForm(TFmNode, FmNode);
  Application.CreateForm(TFmTrEvol, FmTrEvol);
  Application.CreateForm(TFmLogic, FmLogic);
  Application.CreateForm(TFmWizards, FmWizards);
  Application.CreateForm(TFmFileConf, FmFileConf);
  Application.CreateForm(TFmColDial, FmColDial);
  Application.CreateForm(TFmCross, FmCross);
  Application.CreateForm(TFmAbout, FmAbout);
  Application.CreateForm(TFmMerge, FmMerge);
  Application.CreateForm(TFmPhil, FmPhil);
  Application.CreateForm(TFmFunct, FmFunct);
  Application.CreateForm(TFmMuddyChildren, FmMuddyChildren);
  Application.CreateForm(TFmWeightVis, FmWeightVis);
  Application.CreateForm(TFmPedagogical, FmPedagogical);
  Application.CreateForm(TFmChConf, FmChConf);
  Application.CreateForm(TFmChVis, FmChVis);
  Application.CreateForm(TFmDoubleFloat, FmDoubleFloat);
  Application.CreateForm(TFmMMMain, FmMMMain);
  Application.CreateForm(TFmMMDialVar, FmMMDialVar);
  Application.CreateForm(TFmMMTransition, FmMMTransition);
  Application.CreateForm(TFmOldPhil, FmOldPhil);
  Application.CreateForm(TFmLineComp, FmLineComp);
  Application.Run;
end.
