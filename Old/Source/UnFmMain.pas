{
  @abstract(Unit that contains the main form of Hekatonkheir Project)
  @author(Rafa "Passarel" Borges <passarel@gmail.com>)
  @created(Porto Alegre, September, 2005)
  @lastmod(London, January, 2010)
}

unit UnFmMain;

interface

uses Dialogs, Menus, ImgList, Controls, ExtCtrls, Forms, ComCtrls, ToolWin,
     StdCtrls, Classes, SysUtils, UnNetRep, UnDialWizards, PngImage, UnNetVisual;

//-------------------------------------------------------------------------------------------------

{
@abstract(Class encapsulating the main form of Hekatonkhire project)
}

type TMainForm = class(TForm)

    About1: TMenuItem;
    Addcurrentpositions1: TMenuItem;
    Change1: TMenuItem;
    Clear1: TMenuItem;
    Delete1: TMenuItem;
    DiningPhilosophers1: TMenuItem;
    Execute1: TMenuItem;
    Exit1: TMenuItem;
    Extra1: TMenuItem;
    FromHere1: TMenuItem;
    FunctionAnalyser1: TMenuItem;
    Help1: TMenuItem;
    Image1: TImage;
    ImageList1: TImageList;
    InputFile1: TMenuItem;
    Label1: TLabel;
    Logic1: TMenuItem;
    MainMenu: TMainMenu;
    MuddyChildren1: TMenuItem;
    N5: TMenuItem;
    N6: TMenuItem;
    N7: TMenuItem;
    N8: TMenuItem;
    Network1: TMenuItem;
    NotAvailable1: TMenuItem;
    oHere1: TMenuItem;
    ools1: TMenuItem;
    Open1: TMenuItem;
    OpenDialog1: TOpenDialog;
    OpenDialog2: TOpenDialog;
    Options1: TMenuItem;
    Organize1: TMenuItem;
    OutputCharts1: TMenuItem;
    Panel1: TPanel;
    Panel2: TScrollBox;
    Panel3: TPanel;
    Panel4: TPanel;
    PedagogicalExtraction1: TMenuItem;
    PopupMenu1: TPopupMenu;
    Properties2: TMenuItem;
    raining1: TMenuItem;
    Save1: TMenuItem;
    Save2: TMenuItem;
    SaveDialog1: TSaveDialog;
    SaveDialog2: TSaveDialog;
    SEprototype1: TMenuItem;
    StatusBar1: TStatusBar;
    Synapse1: TMenuItem;
    Timer1: TTimer;
    ToolBar1: TToolBar;
    ToolBar2: TToolBar;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    Visualization1: TMenuItem;
    WeightsAnalysis1: TMenuItem;
    Wizards1: TMenuItem;

    procedure About1Click(Sender: TObject);
    procedure Addcurrentpositions1Click(Sender: TObject);
    procedure Change1Click(Sender: TObject);
    procedure Clear1Click(Sender: TObject);
    procedure Delete1Click(Sender: TObject);
    procedure DiningPhilosophers1Click(Sender: TObject);
    procedure Execute1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FromHere1Click(Sender: TObject);
    procedure FunctionAnalyser1Click(Sender: TObject);
    procedure InputFile1Click(Sender: TObject);
    procedure Logic1Click(Sender: TObject);
    procedure MuddyChildren1Click(Sender: TObject);
    procedure oHere1Click(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure Options1Click(Sender: TObject);
    procedure Organize1Click(Sender: TObject);
    procedure OutputCharts1Click(Sender: TObject);
    procedure Panel2Click(Sender: TObject);
    procedure PedagogicalExtraction1Click(Sender: TObject);
    procedure Properties2Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure Save2Click(Sender: TObject);
    procedure SEprototype1Click(Sender: TObject);
    procedure ToolButton10Click(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
    procedure ToolButton7Click(Sender: TObject);
    procedure ToolButton9Click(Sender: TObject);
    procedure WeightsAnalysis1Click(Sender: TObject);
    procedure Wizards1Click(Sender: TObject);

  private

    //Representation of the network currently in use
    FNetwork : TNetRepVisual;

    //Name of the input file for cross validation purposes
    FCrossFile : string;

    //Name of the report that resulted of the cross validation
    FReportFile : string;

    //Information of the source node to a new Synapse to be inserted
    FTmpSynapseSource : integer;

    //Information of the target node to a new Synapse to be inserted
    FTmpSynapseTarget : integer;

    //Procedure to update the properties tab according to the selected item
    //@param(item: selected item which properties are to be shown)
    procedure UpdateProperties(item : integer);

    //Initial method to visualise the network in the screen
    procedure InitVisualization;

    //Procedure to update the visualization of the network
    //@param(item: selected node in the network)
    procedure UpdateVisual(index : integer);

  end;

//-------------------------------------------------------------------------------------------------

var MainForm : TMainForm;

//-------------------------------------------------------------------------------------------------

implementation

uses Graphics, Types, UnAbout, UnDialChConf, UnDialDinPhil, UnDialEdit,
     UnDialCross, UnDialFile, UnFmMuddyChildren, UnFunct, UnFunctions,
     UnLogForm, UnMMVisualNew, UnPedagogical, UnWeightVis, UnPhil;

{$R *.dfm}

//-------------------------------------------------------------------------------------------------

procedure TMainForm.About1Click(Sender: TObject);
  begin
  FmAbout.ShowModal;
  end;

//-------------------------------------------------------------------------------------------------

procedure TMainForm.Addcurrentpositions1Click(Sender: TObject);
  begin
  if (FTmpSynapseSource >= 0) and (FTmpSynapseTarget >= 0) then
     FNetwork.NetRep.AddArc(FTmpSynapseSource, FTmpSynapseTarget);
  FNetwork.UpdateVisual;
  UpdateProperties(FNetwork.NetVisual.SelectedItem);
  end;

//-------------------------------------------------------------------------------------------------

procedure TMainForm.Change1Click(Sender: TObject);
  begin
  MessageDlg('Option not available in this version', mtInformation, [MbOK], 0);
  end;

//-------------------------------------------------------------------------------------------------

procedure TMainForm.Clear1Click(Sender: TObject);
  begin
  FNetwork.NetRep.ClearArcs;
  FNetwork.NetRep.ClearNodes;
  FmEdit.UpdateNetworkProperties;
  InitVisualization;
  end;

//-------------------------------------------------------------------------------------------------

procedure TMainForm.Delete1Click(Sender: TObject);
  begin
  FNetwork.NetRep.DeleteNode(FNetwork.NetVisual.SelectedItem);
  FNetwork.UpdateVisual;
  UpdateProperties(FNetwork.NetVisual.SelectedItem);
  end;

//-------------------------------------------------------------------------------------------------

procedure TMainForm.DiningPhilosophers1Click(Sender: TObject);
  begin
  FmOldPhil.ShowModal;
//  FmPhil.ShowModal;
  end;

//-------------------------------------------------------------------------------------------------

procedure TMainForm.Execute1Click(Sender: TObject);
  var tmp : string;
  begin
  tmp := FmCross.Execute(FCrossFile, FNetwork.NetRep);
  if tmp <> '' then
     FReportFile := tmp;
  FmEdit.UpdateNetworkProperties;
  InitVisualization;
  end;

//-------------------------------------------------------------------------------------------------

procedure TMainForm.Exit1Click(Sender: TObject);
  begin
  Close;
  end;

//-------------------------------------------------------------------------------------------------

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
  begin
  FNetwork.Free;
  end;

//-------------------------------------------------------------------------------------------------

procedure TMainForm.FormCreate(Sender: TObject);
  begin
  randomize;
  FTmpSynapseSource := -1;
  FTmpSynapseTarget := -1;
  end;

//-------------------------------------------------------------------------------------------------

procedure TMainForm.FormShow(Sender: TObject);
  begin
  FNetwork := TNetRepVisual.CreateFromLists(image1, FMEdit.ListBox1.items, FmEdit.ListBox4.items);
  FNetwork.OnVisualChange := UpdateProperties;
  FmEdit.SetNetwork(FNetwork.NetRep);
  FmEdit.PageControl1.Parent := self;
  FmEdit.PageControl1.Align  := alRight;
  FmEdit.PageControl1.Width := 300;
  FmEdit.OnChangeVisualProperties := UpdateVisual;
  end;

//-------------------------------------------------------------------------------------------------

procedure TMainForm.FromHere1Click(Sender: TObject);
  begin
  FTmpSynapseSource := FNetwork.NetVisual.SelectedItem;
  FromHere1.Caption := '&From here (cur: ' + inttostr(FTmpSynapseSource) + ')';
  oHere1.Caption := '&To here (cur: ' + inttostr(FTmpSynapseSource) + ')';
  end;

//-------------------------------------------------------------------------------------------------

procedure TMainForm.FunctionAnalyser1Click(Sender: TObject);
  begin
  FmFunct.ShowModal;
  end;

//-------------------------------------------------------------------------------------------------

procedure TMainForm.InitVisualization;
   begin
   Panel4.Visible := true;
   image1.Visible := true;
   FNetwork.UpdateVisual;
   end;

//-------------------------------------------------------------------------------------------------

procedure TMainForm.InputFile1Click(Sender: TObject);
  begin
  FCrossFile := FmFileConf.Execute;
  end;

//-------------------------------------------------------------------------------------------------

procedure TMainForm.Logic1Click(Sender: TObject);
  begin
  FmLogic.ChamaInterfaceLogica(FNetwork.NetRep);
  FmEdit.UpdateNetworkProperties;
  InitVisualization;
  end;

//-------------------------------------------------------------------------------------------------

procedure TMainForm.MuddyChildren1Click(Sender: TObject);
  begin
  FmMuddyChildren.ShowModal;
  end;

//-------------------------------------------------------------------------------------------------

procedure TMainForm.oHere1Click(Sender: TObject);
  begin
  FTmpSynapseTarget := FNetwork.NetVisual.SelectedItem;
  FromHere1.Caption := '&From here (cur: ' + inttostr(FTmpSynapseSource) + ')';
  oHere1.Caption := '&To here (cur: ' + inttostr(FTmpSynapseSource) + ')';
  end;

//-------------------------------------------------------------------------------------------------

procedure TMainForm.Open1Click(Sender: TObject);
  begin
  if OpenDialog1.Execute then
     FNetwork.NetRep.LoadFromXML(OpenDialog1.FileName);
  FmEdit.UpdateNetworkProperties;
  InitVisualization;
  end;

//-------------------------------------------------------------------------------------------------

procedure TMainForm.Options1Click(Sender: TObject);
  begin
  MessageDlg('Option not available in this version', mtInformation, [MbOK], 0);
  end;

//-------------------------------------------------------------------------------------------------

procedure TMainForm.Organize1Click(Sender: TObject);
  begin
  MessageDlg('Option not available in this version', mtInformation, [MbOK], 0);
  end;

//-------------------------------------------------------------------------------------------------

procedure TMainForm.OutputCharts1Click(Sender: TObject);
  begin
  if (trim(FReportFile)<> '') and (FileExists(FReportFile)) then
     FmChConf.execute(FReportFile)
  else if OpenDialog2.Execute then
     begin
     FReportFile := OpenDialog2.FileName;
     FmChConf.execute(FReportFile);
     end;
  end;

//-------------------------------------------------------------------------------------------------

procedure TMainForm.Panel2Click(Sender: TObject);
  var
    RUT : TUnitKind;
    nenhum : boolean;
    name : string;
  begin
  nenhum := false;
  RUT := UKNeuron;
  if ToolBar1.Buttons[2].Down then RUT := UKNeuron
  else if ToolBar1.Buttons[3].Down then RUT := UKDelay
  else if ToolBar1.Buttons[4].Down then RUT := UKInput
  else if ToolBar1.Buttons[5].Down then RUT := UKOutput
  else if ToolBar1.Buttons[6].Down then RUT := UKRec
  else nenhum := true;

  //*** Redefine, maybe
  name := 'Unit' + inttostr(FNetwork.NetRep.NoNodes);

  if not nenhum then
     begin
     FNetwork.NetRep.AddNode(name, RUT);
     FNetwork.UpdateVisual;
     end;
  ToolBar1.Buttons[2].Down := false;
  ToolBar1.Buttons[3].Down := false;
  ToolBar1.Buttons[4].Down := false;
  ToolBar1.Buttons[5].Down := false;
  ToolBar1.Buttons[6].Down := false;
  ToolBar1.Buttons[0].Down := true;

  if ToolButton2.Down then
     Label1.Caption := 'Click on a position to insert a neuron'
  else if ToolButton3.Down then
     Label1.Caption := 'Click on a position to insert a delay unit'
  else if ToolButton8.Down then
     Label1.Caption := 'Click on a position to insert an input '
  else if ToolButton11.Down then
     Label1.Caption := 'Click on a position to insert an output'
  else if ToolButton12.Down then
     Label1.Caption := 'Click on a position to insert a recursive link'
  else
     Label1.Caption := 'Visual Editor for Neural Networks (Version 0.1)';
  end;

//-------------------------------------------------------------------------------------------------

procedure TMainForm.PedagogicalExtraction1Click(Sender: TObject);
  begin
  FmPedagogical.ShowModal;
  end;

//-------------------------------------------------------------------------------------------------

procedure TMainForm.Properties2Click(Sender: TObject);
  begin
  Properties2.Checked := not Properties2.Checked;
  if properties2.Checked then
     FmEdit.PageControl1.Width := 300
  else
     FmEdit.PageControl1.Width := 0
  end;

//-------------------------------------------------------------------------------------------------

procedure TMainForm.Save1Click(Sender: TObject);
  begin
  if SaveDialog1.Execute then
     FNetwork.NetRep.SaveToXML(SaveDialog1.FileName);
  end;

//-------------------------------------------------------------------------------------------------

procedure TMainForm.Save2Click(Sender: TObject);
  begin
  MessageDlg('Option not available in this version', mtInformation, [MbOK], 0);
  end;

//-------------------------------------------------------------------------------------------------

procedure TMainForm.SEprototype1Click(Sender: TObject);
  begin
  FmMMMain.ShowModal;
  end;

//-------------------------------------------------------------------------------------------------

procedure TMainForm.ToolButton10Click(Sender: TObject);
  begin
  if SaveDialog1.Execute then
     FNetwork.NetRep.SaveToXML(SaveDialog1.FileName);
  end;

//-------------------------------------------------------------------------------------------------

procedure TMainForm.ToolButton1Click(Sender: TObject);
  begin
  ToolButton2.Down := false;
  ToolButton3.Down := false;
  ToolButton8.Down := false;
  ToolButton11.Down := false;
  ToolButton12.Down := false;
  ToolButton2Click(sender);
  end;

//-------------------------------------------------------------------------------------------------

procedure TMainForm.ToolButton2Click(Sender: TObject);
  begin

  ToolButton4.Down := false;
  ToolButton1.Down := false;
  if ToolButton2.Down then
     Label1.Caption := 'Click on a position to insert a neuron'
  else if ToolButton3.Down then
     Label1.Caption := 'Click on a position to insert a delay unit'
  else if ToolButton8.Down then
     Label1.Caption := 'Click on a position to insert an input '
  else if ToolButton11.Down then
     Label1.Caption := 'Click on a position to insert an output'
  else if ToolButton12.Down then
     Label1.Caption := 'Click on a position to insert a recursive link'
  else
     begin
     Label1.Caption := 'Visual Editor for Neural Networks (Version 0.1)';
     ToolButton1.Down := true;
     end;
  end;

//-------------------------------------------------------------------------------------------------

procedure TMainForm.ToolButton7Click(Sender: TObject);
  begin
  FNetwork.NetRep.ClearArcs;
  FNetwork.NetRep.ClearNodes;
  FmEdit.UpdateNetworkProperties;
  InitVisualization;
  end;

//-------------------------------------------------------------------------------------------------

procedure TMainForm.ToolButton9Click(Sender: TObject);
  begin
  if OpenDialog1.Execute then
     FNetwork.NetRep.LoadFromXML(OpenDialog1.FileName);
  FmEdit.UpdateNetworkProperties;
  InitVisualization;
  end;

//-------------------------------------------------------------------------------------------------

procedure TMainForm.UpdateProperties(item : integer);
  begin
  if item >= 0 then
     FmEdit.UpdateSelectedNode(item)
  else
     FmEdit.UpdateSelectedNode(-1);
  end;

//-------------------------------------------------------------------------------------------------

procedure TMainForm.UpdateVisual(index : integer);
  begin
  FNetwork.NetVisual.SelectedItem := index;
  FNetwork.UpdateVisual;
  end;

//-------------------------------------------------------------------------------------------------

procedure TMainForm.WeightsAnalysis1Click(Sender: TObject);
  begin
  FmWeightVis.ShowModal;
  end;

//-------------------------------------------------------------------------------------------------

procedure TMainForm.Wizards1Click(Sender: TObject);
  begin
  FMWizards.LoadFromWizard(FNetwork.NetRep);
  FmEdit.UpdateNetworkProperties;
  InitVisualization;
  end;

//-------------------------------------------------------------------------------------------------

end.

