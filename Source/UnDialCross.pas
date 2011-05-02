unit UnDialCross;

{
  @abstract(Unit with the dialog that manages the Cross Validation process)
  @author(Rafa "Passarel" Borges <passarel@gmail.com>)
  @created(Porto Alegre, 2006)
  @lastmod(London, January, 2010)
}

interface

uses
  Classes, ComCtrls, Controls, Dialogs, ExtCtrls, Forms, Graphics, JvEdit, JvExStdCtrls,
  JvValidateEdit, Messages, Spin, StdCtrls, SysUtils, UnNetBehaviour, UnNetRep, Variants, Windows;

//-------------------------------------------------------------------------------------------------

type
  TFmCross = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    Edit1: TEdit;
    Edit2: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Memo1: TMemo;
    OpenDialog1: TOpenDialog;
    PageControl1: TPageControl;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioGroup1: TRadioGroup;
    SaveDialog1: TSaveDialog;
    SaveDialog2: TSaveDialog;
    ScrollBox1: TScrollBox;
    ScrollBox2: TScrollBox;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    SpinEdit3: TSpinEdit;
    SpinEdit4: TSpinEdit;
    SpinEdit5: TSpinEdit;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Timer1: TTimer;
    CheckBox1: TCheckBox;
    CheckBox7: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure CheckBox7Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
    procedure SpinEdit2Change(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);

  private
    //Identifies if the "OK" button was pressed
    FExecuted : boolean;

    //String to separate the groups inside the file
    FGroupSeparator : string;

    //String List used to read the input File
    FFileStrl       : TStringList;

    //Count of columns in the input file
    FFColumnsCount  : integer;

    //Array keeping the delay information of the columns in the input file
    FFDelays        : array of integer;

    //Number of groups to cross validation as defined in the input file
    FFGroupsCount   : integer;

    //Array keeping the information of maximum value of the columns in the input file
    FFileMax        : array of double;

    //Array keeping the information of minimum value of the columns in the input file
    FFileMin        : array of double;

    //Array keeping the information of which columns of the input file are input to the network
    FFInputs        : array of boolean;

    //Number of columns of the input file are intput to the network
    FFInputsCount   : integer;

    //Array keeping the information of which columns of the input file are output to the network
    FFOutputs       : array of boolean;

    //Number of columns of the input file are output to the network
    FFOutputsCount   : integer;

    //Number of patterns (lines) in the input file
    FFPatternsCount : integer;

    //Array of edit boxes to set the maximum values of each column representing input
    FInMaxEdits   : array of TJvValidateEdit;

    //Array of labels to set the maximum values of each column representing input
    FInMaxLabels  : array of TLabel;

    //Array of edit boxes to set the minimum values of each column representing input
    FInMinEdits   : array of TJvValidateEdit;

    //Array of labels to set the minimum values of each column representing input
    FInMinLabels  : array of TLabel;

    //Object with the description of the network to be used
    FNetRep   : TNetworkRep;

    //Object with the network where the learning process will be run
    FNetwork  : TRafaANN;

    //Array of edit boxes to set the maximum values of each column representing output
    FOutMaxEdits  : array of TJvValidateEdit;

    //Array of labels to set the maximum values of each column representing output
    FOutMaxLabels : array of TLabel;

    //Array of edit boxes to set the minimum values of each column representing output
    FOutMinEdits  : array of TJvValidateEdit;

    //Array of labels to set the minimum values of each column representing output
    FOutMinLabels : array of TLabel;

    //Name of the file to save the report ofthe learning process
    FReport   : string;

    //Information indicating thart the SpinEdit1 is being edited
    FSpin1 : boolean;

    //Information indicating thart the SpinEdit2 is being edited
    FSpin2 : boolean;

    //Temporary fix to a problem with the manipulation of files - to be removed in future versions
    Gambiarra : boolean;

    //Size of the biggest group(sequence) in the file
    FMaxGroupSize : integer;

    //Indicates if the file contain a line with delay information (to become deprecated)
    FFileContainsDelay : boolean;

    //Procedure called when a button to edit the inputs is pressed
    //@param(Sender: Object that activated the event)
    procedure DefInputClick(Sender : TObject);

    //Procedure called when a button to edit the inputs is pressed
    //@param(Sender: Object that activated the event)
    procedure DefOutputClick(Sender : TObject);

    //Procedure that actually executes the cross validation process
    //@param(ReportFile: Name of the file to save the report with the results)
    procedure ExecuteCross(ReportFile : String);

    //Function to load an input file
    //@param(FileName: Name of the input file to be loaded)
    function LoadFile(FileName : string) : boolean;

    //Procedure to load the data in the interface that configures the inputs and outputs
    procedure LoadIoInterface;



  public
    //Function to show the form, loading the fields according to the input file
    //@param(FileName: Name of the input file to be loaded)
    //@param(PRep: Network to be used in the cross validation system)
    function Execute(crossFile : string; PRep : TNetworkRep) : string;
  end;

//-------------------------------------------------------------------------------------------------

var
  FmCross: TFmCross;

//-------------------------------------------------------------------------------------------------

implementation

{$R *.dfm}

uses UnDialTrEvol, Unfunctions;

{ TFmCross }

//-------------------------------------------------------------------------------------------------

procedure TFmCross.Button1Click(Sender: TObject);
  var
     Xbool : boolean;
     i : integer;
  begin
  if OpenDialog1.Execute then
     begin
     XBool := false;
     case RadioGroup1.ItemIndex of
       0:
         begin
         XBool := loadFile(OpenDialog1.FileName);
         end;
       1:
         begin
         Memo1.Lines.LoadFromFile(OpenDialog1.FileName);
         for i := Memo1.Lines.Count - 1 downto 0 do
           if not DirectoryExists(trim(Memo1.Lines.Strings[i])) then
              Memo1.Lines.Delete(i);
         XBool := (Memo1.Lines.Count > 0);

         end;
       end;
     GroupBox2.Enabled := XBool;
     GroupBox3.Enabled := XBool;
     Button3.Enabled   := XBool;
     end;
  end;

//-------------------------------------------------------------------------------------------------

procedure TFmCross.Button2Click(Sender: TObject);
  begin
  Close;
  end;

//-------------------------------------------------------------------------------------------------

procedure TFmCross.Button3Click(Sender: TObject);
  var
    XOutFileName: TFileName;
    XNetworks, XInputFiles : TStringList;
    XDirectoryList : TStrings;
    i, j, k : integer;
    s : string;
    Xsr: TSearchRec;

  begin
  case RadioGroup1.ItemIndex of
    0:
      if SaveDialog1.Execute then
         ExecuteCross(SaveDialog1.FileName);
    1:
      begin
      XNetworks := TStringList.Create;
      XInputFiles := TStringList.Create;
      XDirectoryList := TStringList.Create;
      for i := 0 to Memo1.Lines.Count - 1 do
        XDirectoryList.Add(Memo1.Lines.Strings[i]);
      for i := 0 to XDirectoryList.Count - 1 do
        begin
        XNetworks.Clear;
        XInputFiles.Clear;
        if FindFirst(XDirectoryList.Strings[i] + '*.*', faAnyFile, XSr) = 0 then
           begin
           if lowerCase(ExtractFileExt(XSr.Name)) = '.xml' then
              XNetworks.Add(XDirectoryList.Strings[i] + ExtractFileName(XSr.Name))
           else if lowerCase(ExtractFileExt(XSr.Name)) = '.pid' then
              XInputFiles.Add(XDirectoryList.Strings[i] + ExtractFileName(XSr.Name));
           while FindNext(XSr) = 0 do
             if lowerCase(ExtractFileExt(XSr.Name)) = '.xml' then
                XNetworks.Add(XDirectoryList.Strings[i] + ExtractFileName(XSr.Name))
             else if lowerCase(ExtractFileExt(XSr.Name)) = '.pid' then
                XInputFiles.Add(XDirectoryList.Strings[i] + ExtractFileName(XSr.Name));
           SysUtils.FindClose(XSr);
           end;
        for j := 0 to XNetworks.count - 1 do
          begin
          FNetRep.LoadFromXML(XNetworks.Strings[j]);
          LoadIoInterface;
          for k := 0 to XInputFiles.Count - 1 do
            if LoadFile(XInputFiles.Strings[k]) then
               begin
               s := ExtractFileName(XInputFiles.Strings[k]);
               s := copy(trim(s), 1, Length(s) - 4);
               XOutFileName := s;
               s := ExtractFileName(XNetworks.Strings[j]);
               s := copy(trim(s), 1, Length(s) - 4);
               XOutFileName := XOutFileName + '_' + s;
               Edit2.Text := XDirectoryList.Strings[i] + XOutFileName;
               ExecuteCross(XDirectoryList.Strings[i] + XOutFileName + '.rrf');
               end;
          end;
        end;
      XNetworks.Free;
      XInputFiles.Free;
      XDirectoryList.Free;
      end;
    end;
  end;

//-------------------------------------------------------------------------------------------------

procedure TFmCross.Button4Click(Sender: TObject);
  var i : integer;
  begin
  for i := 0 to FNetRep.GetOutputCount - 1 do
     begin
     try
       FOutMinEdits[i].Text := FOutMinLabels[i].Caption;
     except
       FOutMinLabels[i].Caption := '???'
       end;
     try
       FOutMaxEdits[i].Text := FOutMaxLabels[i].Caption;
     except
       FOutMaxLabels[i].Caption := '???'
       end;
     end;

  end;

//-------------------------------------------------------------------------------------------------

procedure TFmCross.Button5Click(Sender: TObject);
  var i : integer;
  begin
  for i := 0 to FNetRep.GetInputCount - 1 do
     begin
     try
       FInMinEdits[i].Text := FInMinLabels[i].Caption;
     except
       FInMinLabels[i].Caption := '???'
       end;
     try
       FInMaxEdits[i].Text := FInMaxLabels[i].Caption;
     except
       FInMaxLabels[i].Caption := '???'
       end;
     end;
  end;

//-------------------------------------------------------------------------------------------------

procedure TFmCross.Button6Click(Sender: TObject);
  begin
  If SaveDialog2.Execute then
     Edit2.Text := SaveDialog2.FileName;
  end;

//-------------------------------------------------------------------------------------------------

procedure TFmCross.CheckBox7Click(Sender: TObject);
  begin
  Edit2.Enabled := CheckBox7.Checked;
  Button6.Enabled := CheckBox7.Checked;
  end;

//-------------------------------------------------------------------------------------------------

procedure TFmCross.DefInputClick(Sender: TObject);
  var XIndex : integer;
  begin
  if Sender is TButton then
     begin
     XIndex := (sender as TButton).Tag;
     try
       FInMinEdits[XIndex].Text := FInMinLabels[XIndex].Caption;
     except
       (Sender as TButton).Enabled := false;
       end;
     try
       FInMaxEdits[XIndex].Text := FInMaxLabels[XIndex].Caption;
     except
       (Sender as TButton).Enabled := false;
       end;
     end;
  end;

//-------------------------------------------------------------------------------------------------

procedure TFmCross.DefOutputClick(Sender: TObject);
  var XIndex : integer;
  begin
  if Sender is TButton then
     begin
     XIndex := (sender as TButton).Tag;
     try
       FOutMinEdits[XIndex].Text := FOutMinLabels[XIndex].Caption;
     except
       (Sender as TButton).Enabled := false;
       end;
     try
       FOutMaxEdits[XIndex].Text := FOutMaxLabels[XIndex].Caption;
     except
       (Sender as TButton).Enabled := false;
       end;
     end;
  end;

//-------------------------------------------------------------------------------------------------

function TFmCross.Execute(crossFile: string; PRep : TNetworkRep): string;
  var XBool : boolean;
  begin
  FFileStrl := TStringList.Create;

  RadioGroup1.ItemIndex := 0;
  PageControl1.Visible := true;
  CheckBox5.Visible    := false;
  Memo1.Height := 80;
  GroupBox1.Caption := 'Data set';

  FNetRep := PRep;


  LoadIOInterface;

  XBool := loadFile(crossfile);
  GroupBox2.Enabled := XBool;
  GroupBox3.Enabled := XBool;
  Button3.Enabled   := XBool;
  ShowModal;
  FFileStrl.Free;
  If FExecuted then
     result := FReport
  else
     result := '';
  end;

//-------------------------------------------------------------------------------------------------

procedure TFmCross.ExecuteCross(ReportFile: String);
  var
    XGoOn : boolean;
    XCol, XMaxDelay, XPatCount: integer;
    i, j, k, XL, XGroupSize,  XLine: integer;
    XX, XFTr, XFTs: string;
    XBestEpoch : integer;
    XOrder : array of integer;
    XNewValues : array of double;
    XRel, XRelChk, XXTr, XXTs, XTr, XTs, XTemp : TStringList;
    XTempNet : TNetworkRep;
    XLastTr, XLastTs : boolean;
  begin
  FmTrEvol.ResetDialog(RadioButton2.Checked);
  FGroupSeparator := '-***-';
  XFTr := 'XXXTmpTr.txt';
  XFTs := 'XXXTmpTs.txt';
  FNetwork := TRafaANN.Create;
  FNetwork.AddRealFunction(logsig, logsig_linha);
  FNetwork.AddRealFunction(tansig, tansig_linha);
  FNetwork.AddRealFunction(BiLogsig, BiLogsig_linha);
  FNetwork.AddRealFunction(Threshold, Threshold_Linha);
  FNetwork.AddRealFunction(BiThreshold, BiThreshold_Linha);
  FNetwork.AddRealFunction(Linear, Linear_Linha);
  FNetwork.AddRealFunction(CrazyA, CrazyA_Linha);

  XTempNet := TNetworkRep.Create(nil, nil);
  FNetRep.GetCopy(XTempNet);

  FNetwork.LoadDescription(FNetRep);

  XCol := 0;
  for i := 0 to FFColumnsCount - 1 do
    if FFInputs[i] then
       begin
       XCol := XCol + 1;
       SetLength(XOrder, XCol);
       XOrder[XCol - 1] := i;
       end;

  for i := 0 to FFColumnsCount - 1 do
    if FFOutputs[i] then
       begin
       XCol := XCol + 1;
       SetLength(XOrder, XCol);
       XOrder[XCol - 1] := i + (FFDelays[i] * FFColumnsCount);
       end;

//**** Take it out of here

  if RadioGroup1.ItemIndex = 0 then
     begin
     SetLength(XNewValues, FFInputsCount);
     for i := 0 to FFInputsCount - 1 do
       XNewValues[i] := FInMinEdits[i].AsFloat;
     FNetwork.ChangeInputMin(XNewValues);
     for i := 0 to FFInputsCount - 1 do
       XNewValues[i] := FInMaxEdits[i].AsFloat;
     FNetwork.ChangeInputMax(XNewValues);
     SetLength(XNewValues, FFOutputsCount);
     for i := 0 to FFOutputsCount - 1 do
       XNewValues[i] := FOutMinEdits[i].AsFloat;
     FNetwork.ChangeOutputMin(XNewValues);
     for i := 0 to FFOutputsCount - 1 do
       XNewValues[i] := FOutMaxEdits[i].AsFloat;
     FNetwork.ChangeOutputMax(XNewValues);
     end
  else if CheckBox5.Checked then
     begin
     SetLength(XNewValues, FFInputsCount);
     for i := 0 to FFInputsCount - 1 do
       XNewValues[i] := FFileMin[XOrder[i]];
     FNetwork.ChangeInputMin(XNewValues);
     for i := 0 to FFInputsCount - 1 do
       XNewValues[i] := FFileMax[XOrder[i]];
     FNetwork.ChangeInputMax(XNewValues);
     SetLength(XNewValues, FFOutputsCount);
     for i := 0 to FFOutputsCount - 1 do
       XNewValues[i] := FFileMin[XOrder[i + FFInputsCount] mod FFColumnsCount];
     FNetwork.ChangeOutputMin(XNewValues);
     for i := 0 to FFOutputsCount - 1 do
       XNewValues[i] := FFileMax[XOrder[i + FFInputsCount] mod FFColumnsCount];
     FNetwork.ChangeOutputMax(XNewValues);
     end;


  XMaxDelay := 0;
  for i := 0 to FFColumnsCount - 1 do
    if FFDelays[i] > XMaxDelay then
       XMaxDelay := FFDelays[i];

  XRel := TStringList.Create;
  XRelChk := TStringList.Create;



  XRelChk.Add('*??* ' + inttostr(FFOutputsCount * 2) + ' ' + inttostr(SpinEdit5.Value * 4) + ' 1 -***- ' + inttoStr(FMaxGroupSize));

  XGOon := true;
  k := 0;
  XTemp := TStringList.Create;
  XTemp.Delimiter := ' ';

  while XGoOn and (k < SpinEdit5.Value) do
    begin
    XXTR := TStringList.Create;
    XXTS := TStringList.Create;
    XLastTr := false;
    XLastTs := false;
    for j := 6 to FFileStrl.Count - 1 do
      begin
      XTemp.DelimitedText := FFileStrl[j];
      if (XTemp.Count > 0) and (XTemp[0] <> '') then
         begin
         XL := abs(StrToInt(XTemp[0])) - 1;
         if StrToInt(XTemp[0]) >= 0 then
            XX := '+ '
         else
            XX := '- ';
         for i := 0 to XCol - 1 do
           XX := XX + XTemp[(XOrder[i] mod FFColumnsCount) + 1] + ' ';
         if ((k <= XL) and (XL < k + SpinEdit2.Value)) or ((k + SpinEdit2.Value > FFGroupsCount)
                  and (0 <= XL) and (XL < (k + SpinEdit2.Value - FFGroupsCount))) then
            begin
            XXTs.Add(XX);
            XLastTr := false;
            XLastTs := true;
            end
         else
            begin
            XXTr.Add(XX);
            XLastTr := true;
            XLastTs := false;
            end;
         end
      else
        begin
        if XLastTr then XXTr.Add('');
        if XLastTs then XXTs.Add('');
        end;
      end;

    if FFileContainsDelay then
       begin
       XTR := TStringList.Create;
       XTs := TStringList.Create;

       for j := 0 to XXTr.Count - 1 do
         begin
         if trim(XXTr[j]) = '' then
            XTr.Add('')
         else
            begin
            XX := '';
            XLine := 0;
            i := 0;
            while XLine <= XMaxDelay do
              begin
              if trim(XXTr[(j + i) mod XXtr.Count]) <> '' then
                 begin
                 XX := XX + XXTr[(j + i) mod XXtr.Count] + ' ';
                 XLine := XLine + 1;
                 end;
              i := i + 1;
              end;
            XTemp.DelimitedText := XX;
            XX := XTemp[0] + ' ';
            for i := 0 to XCol - 1 do
              XX := XX + XTemp[i + ((XOrder[i] div FFColumnsCount) * (XCol + 1)) + 1] + ' ';
            XTr.Add(XX);
            end;
         end;
       for j := 0 to XXTs.Count - 1 do
         begin
         if trim(XXTs[j]) = '' then
            XTs.Add('')
         else
            begin
            XX := '';
            XLine := 0;
            i := 0;
            while XLine <= XMaxDelay do
              begin
              if trim(XXTs[(j + i) mod XXts.Count]) <> '' then
                 begin
                 XX := XX + XXTs[(j + i) mod XXts.Count] + ' ';
                 XLine := XLine + 1;
                 end;
              i := i + 1;
              end;
            XTemp.DelimitedText := XX;
            XX := XTemp[0] + ' ';
            for i := 0 to XCol - 1 do
              XX := XX + XTemp[i + ((XOrder[i] div FFColumnsCount) * (XCol + 1)) + 1] + ' ';
            XTs.Add(XX);
            end;
         end;
       XXTr.Free;
       XXTs.Free;
       XXTr := XTr;
       XXTs := XTs;
       //XTr := nil;
       //XTs := nil;
       end;

    if (FMaxGroupSize > 1) then
       begin
       XTr := TStringList.Create;
       XTs := TStringList.Create;
       XGroupSize := 0;
       for j := 0 to XXTr.Count - 1 do
         begin
         if (trim(XXTr.Strings[j]) = '') or (XGroupSize >= FMaxGroupSize) then
            begin
            XTr.Add('');
            XGroupSize := 0;
            end
         else
            begin
            XTr.Add(XXTr.Strings[j]);
            XGroupSize := XGroupSize + 1;;
            end
         end;
       for j := 0 to XXTs.Count - 1 do
         begin
         if (trim(XXTs.Strings[j]) = '') or (XGroupSize >= FMaxGroupSize) then
            begin
            XTs.Add('');
            XGroupSize := 0;
            end
         else
            begin
            XTs.Add(XXTs.Strings[j]);
            XGroupSize := XGroupSize + 1;;
            end
         end;
       XXTr.Free;
       XXTs.Free;
       XXTr := XTr;
       XXTs := XTs;
       end;

    Timer1.Enabled := true;
    Gambiarra := true;

    //***************Improve this gambiarra************************
    while Gambiarra and (FileExists(XFTr) or FileExists(XFTs)) do
      Application.ProcessMessages;
    Timer1.Enabled := false;

    if Gambiarra then
       begin
       try
         XXTr.SaveToFile(XFTr);
       except
         XFtr := XFTr + 'a';
         XXTr.SaveToFile(XFTr);
         end;

       try
         XXTs.SaveToFile(XFTs);
       except
         XFTs := XFTs + 'a';
         XXTs.SaveToFile(XFTs);
         end;

       FNetwork.resetNetwork;
       if SpinEdit1.Value > 0 then
          if RadioButton1.Checked then
             begin
             if SpinEdit2.Value = 0 then XFTs := XFTr;
             XBestEpoch := FNetwork.NewTraining(XFTr, XFTs, ReportFile + '2', SpinEdit3.Value, k,
                          FFGroupsCount, checkbox4.Checked, checkbox3.Checked, CheckBox2.Checked, CheckBox1.Checked);
             end
          else
             XBestEpoch := FNetwork.NewTraining(XFTr, '', ReportFile + '2', SpinEdit3.Value, k,
                          FFGroupsCount, checkbox4.Checked, checkbox3.Checked, CheckBox2.Checked, CheckBox1.Checked)

       else
          XBestEpoch := 0;

       XGoon := XBestEpoch >= 0;

       if SpinEdit1.Value > 0 then
          begin
          XTemp.LoadFromFile(ReportFile + '2');
          XRel.Add('*** Validation ' + inttoStr(k) + ':');
          XRel.Add('');
          XRel.AddStrings(XTemp);
          XRel.Add('');

          XPatCount := FNetwork.NewChecking(XFTr, ReportFile + '3', CheckBox4.Checked, false, SpinEdit4.Value/100);
          XTemp.LoadFromFile(ReportFile + '3');
          XRelChk.Add('*** Validation ' + inttoStr(k) + ', last epoch, training data:');
          XRelChk.Add('');
          XRelChk.Add('*!!* ' + inttostr(XPatCount));
          XRelChk.Add('');
          XRelChk.AddStrings(XTemp);
          XRelChk.Add('');
          end;

       XPatCount := FNetwork.NewChecking(XFTs, ReportFile + '3', CheckBox4.Checked, CheckBox6.Checked, SpinEdit4.Value/100);
       XTemp.LoadFromFile(ReportFile + '3');
       XRelChk.Add('*** Validation ' + inttoStr(k) + ', last epoch, test data:');
       XRelChk.Add('');
       XRelChk.Add('*!!* ' + inttostr(XPatCount));
       XRelChk.Add('');
       XRelChk.AddStrings(XTemp);
       XRelChk.Add('');

       if SpinEdit1.Value > 0 then
          begin
          if CheckBox7.Checked then
             begin
             FNetwork.SaveDescription(XTempNet);
             XTempNet.SaveToXML(Edit2.Text + inttostr(k) + '.xml');
             end;
          FNetwork.LoadBestEpoch;

          XPatCount := FNetwork.NewChecking(XFTr, ReportFile + '3', CheckBox4.Checked, false, SpinEdit4.Value/100);
          XTemp.LoadFromFile(ReportFile + '3');
          XRelChk.Add('*** Validation ' + inttoStr(k) + ', best epoch, training data:');
          XRelChk.Add('');
          XRelChk.Add('*!!* ' + inttostr(XPatCount));
          XRelChk.Add('');
          XRelChk.AddStrings(XTemp);
          XRelChk.Add('');

          XPatCount := FNetwork.NewChecking(XFTs, ReportFile + '3', CheckBox4.Checked, CheckBox6.Checked, SpinEdit4.Value/100);
          XTemp.LoadFromFile(ReportFile + '3');
          XRelChk.Add('*** Validation ' + inttoStr(k) + ', best epoch(' + inttostr(xBestEpoch) + '), test data:');
          XRelChk.Add('');
          XRelChk.Add('*!!* ' + inttostr(XPatCount));
          XRelChk.Add('');
          XRelChk.AddStrings(XTemp);
          XRelChk.Add('');
          end;

       if FileExists(XFTr) then
          SysUtils.Deletefile(XFTr);
       if FileExists(XFTs) then
          SysUtils.DeleteFile(XFTs);
       k := k + 1;
       XXTR.Free;
       XXTs.Free;
       end
    else
       begin
       if FileExists(XFTr) then
          SysUtils.Deletefile(XFTr);
       if FileExists(XFTs) then
          SysUtils.DeleteFile(XFTs);
       XGoOn := false;
       end
    end;

  XRel.SaveToFile(ReportFile + '1');
  XRelChk.SaveToFile(ReportFile);
  try
    SysUtils.DeleteFile(Reportfile + '2');
  except
    MessageDlg('A temporary file, named ' + ReportFile + '2 has been created and couldn''t be deleted',
               MtWarning, [MbOK], 0);
    end;
  try
    SysUtils.DeleteFile(Reportfile + '3');
  except
    MessageDlg('A temporary file, named ' + ReportFile + '2 has been created and couldn''t be deleted',
               MtWarning, [MbOK], 0);
    end;
  XRel.Free;
  XRelChk.Free;
  end;

//-------------------------------------------------------------------------------------------------

procedure TFmCross.FormCreate(Sender: TObject);
  begin
  FSpin1 := false;
  FSpin2 := false;
  end;

//-------------------------------------------------------------------------------------------------

function TFmCross.LoadFile(fileName: string) : boolean;
  var
    s : string;
    tmpStrl : TStringList;
    i, j_in, j_out : integer;
  begin
  Edit1.Text := FileName;
  if FileExists(FileName) then
     begin
     FFileStrl.Clear;
     FFileStrl.LoadFromFile(FileName);
     tmpStrl := TStringList.Create;
     tmpStrl.Delimiter := ' ';
     tmpStrl.DelimitedText := FFileStrl.Strings[0];

     if tmpStrl[0] = '��' then
        begin
        FFColumnsCount  := strtoint(tmpStrl[1]);
        FFPatternsCount := strtoint(tmpStrl[2]);
        FFGroupsCount   := strtoint(tmpStrl[3]);
        FFileContainsDelay := false;
        if tmpStrl.Count > 4 then
           if pos('*', tmpStrl[4]) > 0 then
              begin
              CheckBox1.Checked := (pos('R', tmpStrl[4]) > 0);
              CheckBox6.Checked := (pos('C', tmpStrl[4]) > 0);
              FFileContainsDelay := (pos('D', tmpStrl[4]) > 0)
              end;
        if tmpStrl.Count > 5 then
           FMaxGroupSize := strtoint(tmpStrl[5]);

        FFInputsCount   := 0;
        FFOutputsCount   := 0;

        SetLength(FFInputs, FFColumnsCount);
        SetLength(FFOutputs, FFColumnsCount);
        SetLength(FFDelays, FFColumnsCount);
        SetLength(FFileMin, FFColumnsCount);
        SetLength(FFileMax, FFColumnsCount);

        tmpStrl.DelimitedText := FFileStrl.Strings[2];
        if tmpStrl.Count = FFColumnsCount then
        for i := 0 to FFColumnsCount - 1 do
          FFileMin[i] := strToFloat(tmpStrl[i]);

        tmpStrl.DelimitedText := FFileStrl.Strings[3];
        if tmpStrl.Count = FFColumnsCount then
        for i := 0 to FFColumnsCount - 1 do
          FFileMax[i] := strToFloat(tmpStrl[i]);

        tmpStrl.DelimitedText := FFileStrl.Strings[4];
        if tmpStrl.Count = FFColumnsCount then
           for i := 0 to FFColumnsCount - 1 do
             begin
             if (tmpStrl[i] = 'B') or (tmpStrl[i] = 'I') then
                begin
                FFInputs[i] := true;
                FFinputsCount := FFInputsCount + 1;
                end
             else
                FFInputs[i] := false;
             if (tmpStrl[i] = 'B') or (tmpStrl[i] = 'O') then
                begin
                FFOutputs[i] := true;
                FFOutputsCount := FFOutputsCount + 1;
                end
             else
                FFOutputs[i] := false;
             end;

        tmpStrl.DelimitedText := FFileStrl.Strings[5];

        if FFileContainsDelay then
           if tmpStrl.Count = FFColumnsCount then
              for i := 0 to FFColumnsCount - 1 do
                FFDelays[i] := strToInt(tmpStrl[i]);

        Memo1.Lines.Clear;
        Memo1.Lines.Add('File loaded: ' + FileName);
        memo1.Lines.Add('  Number of Columns: '  + inttostr(FFColumnsCount));
        memo1.Lines.Add('  Number of Inputs: '   + inttostr(FFInputsCount));
        memo1.Lines.Add('  Number of Outputs: '  + inttostr(FFOutputsCount));
        memo1.Lines.Add('');
        memo1.Lines.Add('  Number of Patterns: ' + inttostr(FFPatternsCount));
        memo1.Lines.Add('  Number of Groups: '   + inttostr(FFGroupsCount));

        s := '';
        if FFInputsCount <> FNetRep.GetInputCount then
           begin
           memo1.Lines.Add('');
           memo1.Lines.Add('Error:');
           s := 'Number of inputs declared on file differs ';
           s := s + 'of the number of input units on the network';
           memo1.Lines.Add(s);
           end;
        if FFOutputsCount <> FNetRep.GetOutputCount then
           begin
           memo1.Lines.Add('');
           memo1.Lines.Add('Error:');
           s := 'Number of outputs declared on file differs ';
           s := s + 'of the number of output units on the network';
           memo1.Lines.Add(s);
           end;
        if s = '' then
           begin
           j_in := 0;
           j_out := 0;
           for i := 0 to FFColumnsCount - 1 do
             begin
             if FFInputs[i] then
                begin
                FInMinLabels[j_in].Caption := FloatToStr(FFileMin[i]);
                FInMaxLabels[j_in].Caption := FloatToStr(FFileMax[i]);
                j_in := j_in + 1;
                end;
             if FFOutputs[i] then
                begin
                FOutMinLabels[j_out].Caption := FloatToStr(FFileMin[i]);
                FOutMaxLabels[j_out].Caption := FloatToStr(FFileMax[i]);
                j_out := j_out + 1;
                end;
             end;
           for i := 0 to ScrollBox1.ControlCount - 1 do
             if ScrollBox1.Controls[i] is TButton then
                with ScrollBox1.Controls[i] as TButton do
                  if caption = '-->' then Enabled := true;

           for i := 0 to ScrollBox2.ControlCount - 1 do
             if ScrollBox2.Controls[i] is TButton then
                with ScrollBox2.Controls[i] as TButton do
                  if caption = '-->' then Enabled := true;

           //CheckBox2.Checked  := false;
           //CheckBox3.Checked  := false;
           //CheckBox4.Checked  := false;
           SpinEdit1.MinValue := 0;
           SpinEdit1.MaxValue := FFGroupsCount;
           SpinEdit2.MinValue := 0;
           SpinEdit2.MaxValue := FFGroupsCount ;
           SpinEdit5.MinValue := 1;
           SpinEdit5.MaxValue := FFGroupsCount;
           SpinEdit5.Value    := FFGroupsCount;
           if RadioButton1.Checked then
              begin
              SpinEdit1.Value    := FFGroupsCount - 1;
              SpinEdit2.Value    := 1;
              SpinEdit5.Value    := FFGroupsCount;
              end
           else
              begin
              SpinEdit1.Value    := FFGroupsCount;
              SpinEdit2.Value    := 0;
              SpinEdit5.Value    := 1;
              end;
           result := true
           end
        else
           begin
           memo1.Lines.Add('');
           s := 'The network cannot be trained because there are ';
           s := s + 'incompatibilities between it and the data loaded';
           memo1.Lines.Add(s);
           result := false;
           end;

        //Adjust the remaining of the form
        end

     else
        begin
        Memo1.Lines.Clear;
        Memo1.Lines.Add('File out of format: ' + FileName);
        result := false;
        end;
     end

  else
     begin
     Memo1.Lines.Clear;
     Memo1.Lines.Add('File not found: ' + FileName);
     result := false;
     end;
  end;

//-------------------------------------------------------------------------------------------------

procedure TFmCross.LoadIoInterface;
  var i, XLeft, XTop: integer;
  begin
  //Clear the tabSheets
  SetLength(FInMinLabels,  FNetRep.GetInputCount);
  SetLength(FInMaxLabels,  FNetRep.GetInputCount);
  SetLength(FInMinEdits,   FNetRep.GetInputCount);
  SetLength(FInMaxEdits,   FNetRep.GetInputCount);
  SetLength(FOutMinLabels, FNetRep.GetOutputCount);
  SetLength(FOutMaxLabels, FNetRep.GetOutputCount);
  SetLength(FOutMinEdits,  FNetRep.GetOutputCount);
  SetLength(FOutMaxEdits,  FNetRep.GetOutputCount);

  for i := ScrollBox2.ControlCount - 1 downto 0 do
    ScrollBox2.Controls[i].Free;
  for i := ScrollBox1.ControlCount - 1 downto 0 do
    ScrollBox1.Controls[i].Free;

  XLeft := 16;

  for i := 0 to FNetRep.GetInputCount - 1 do
    begin
    XTop := (i * 80) + 16;
    with TLabel.Create(self) do
      begin
      parent := ScrollBox2;
      AutoSize := false;
      Alignment := taCenter;
      top := XTop;
      left  := XLeft;
      width := 50;
      height := 13;
      Font.Style := [FsBold];
      Caption := 'Input ' + inttostr(i + 1);
      end;
    with TLabel.Create(self) do
      begin
      parent := ScrollBox2;
      AutoSize := false;
      Alignment := taRightJustify;
      top := XTop + 22;
      left  := XLeft;
      width := 50;
      height := 13;
      Caption := 'Min:';
      end;
    with TLabel.Create(self) do
      begin
      parent := ScrollBox2;
      AutoSize := false;
      Alignment := taRightJustify;
      top := XTop + 46;
      left  := XLeft;
      width := 50;
      height := 13;
      Caption := 'Max';
      end;

    with TLabel.Create(self) do
      begin
      parent := ScrollBox2;
      AutoSize := false;
      Alignment := taCenter;
      top := XTop;
      left := XLeft + 54;
      width := 50;
      height := 13;
      Caption := 'File';
      end;
    FInMinLabels[i] := TLabel.Create(self);
    with FInMinLabels[i] do
      begin
      parent := ScrollBox2;
      AutoSize := false;
      Alignment := taCenter;
      top := XTop + 22;
      left := XLeft + 54;
      width := 50;
      height := 13;
      Caption := '???';
      end;
    FInMaxLabels[i] := TLabel.Create(self);
    with FInMaxLabels[i] do
      begin
      parent := ScrollBox2;
      AutoSize := false;
      Alignment := taCenter;
      top := XTop + 46;
      left := XLeft + 54;
      width := 50;
      height := 13;
      Caption := '???';
      end;

    with TButton.Create(Self) do
      begin
      parent := ScrollBox2;
      top := XTop + 30;
      left := XLeft + 108;
      width := 32;
      height := 25;
      Tag := i;
      Caption := '-->';
      OnClick := DefInputClick;
      Enabled := false;
      end;

    with TLabel.Create(self) do
      begin
      parent := ScrollBox2;
      AutoSize := false;
      Alignment := taCenter;
      top := XTop;
      left := XLeft + 150;
      width := 60;
      height := 13;
      Caption := 'Network';
      end;
    FInMinEdits[i] := TJvValidateEdit.Create(self);
    with FInMinEdits[i] do
      begin
      parent := ScrollBox2;
      top := XTop + 20;
      left := XLeft + 150;
      width := 60;
      height := 21;

      value := FNetRep.Input[i].InMin;
      end;
    FInMaxEdits[i] := TJvValidateEdit.Create(self);
    with FInMaxEdits[i] do
      begin
      parent := ScrollBox2;
      top := XTop + 44;
      left := XLeft + 150;
      DisplayFormat := dfFloatGeneral;
      width := 60;
      height := 21;
      value := FNetRep.Input[i].InMax;
      end;
    end;

  for i := 0 to FNetRep.GetOutputCount - 1 do
    begin
    XTop := (i * 80) + 16;
    with TLabel.Create(self) do
      begin
      parent := ScrollBox1;
      AutoSize := false;
      Alignment := taCenter;
      top := XTop;
      left  := XLeft;
      width := 50;
      height := 13;
      Font.Style := [FsBold];
      Caption := 'Output ' + inttostr(i + 1);
      end;
    with TLabel.Create(self) do
      begin
      parent := ScrollBox1;
      AutoSize := false;
      Alignment := taRightJustify;
      top := XTop + 22;
      left  := XLeft;
      width := 50;
      height := 13;
      Caption := 'Min:';
      end;
    with TLabel.Create(self) do
      begin
      parent := ScrollBox1;
      AutoSize := false;
      Alignment := taRightJustify;
      top := XTop + 46;
      left  := XLeft;
      width := 50;
      height := 13;
      Caption := 'Max';
      end;

    with TLabel.Create(self) do
      begin
      parent := ScrollBox1;
      AutoSize := false;
      Alignment := taCenter;
      top := XTop;
      left := XLeft + 54;
      width := 50;
      height := 13;
      Caption := 'File';
      end;
    FOutMinLabels[i] := TLabel.Create(self);
    with FOutMinLabels[i] do
      begin
      parent := ScrollBox1;
      AutoSize := false;
      Alignment := taCenter;
      top := XTop + 22;
      left := XLeft + 54;
      width := 50;
      height := 13;
      Caption := '???';
      end;
    FOutMaxLabels[i] := TLabel.Create(self);
    with FOutMaxLabels[i] do
      begin
      parent := ScrollBox1;
      AutoSize := false;
      Alignment := taCenter;
      top := XTop + 46;
      left := XLeft + 54;
      width := 50;
      height := 13;
      Caption := '???';
      end;

    with TButton.Create(Self) do
      begin
      parent := ScrollBox1;
      top := XTop + 30;
      left := XLeft + 108;
      width := 32;
      height := 25;
      Tag := i;
      Caption := '-->';
      OnClick := DefOutputClick;
      Enabled := false;
      end;

    with TLabel.Create(self) do
      begin
      parent := ScrollBox1;
      AutoSize := false;
      Alignment := taCenter;
      top := XTop;
      left := XLeft + 150;
      width := 60;
      height := 13;
      Caption := 'Network';
      end;
    FOutMinEdits[i] := TJvValidateEdit.Create(self);
    with FOutMinEdits[i] do
      begin
      parent := ScrollBox1;
      top := XTop + 20;
      left := XLeft + 150;
      width := 60;
      height := 21;
      DisplayFormat := dfFloatGeneral;
      value := FNetRep.Output[i].OutMin;
      end;
    FOutMaxEdits[i] := TJvValidateEdit.Create(self);
    with FOutMaxEdits[i] do
      begin
      parent := ScrollBox1;
      top := XTop + 44;
      left := XLeft + 150;
      width := 60;
      height := 21;
      DisplayFormat := dfFloatGeneral;
      value := FNetRep.Output[i].OutMax;
      end;
    end;
  end;

//-------------------------------------------------------------------------------------------------

procedure TFmCross.RadioGroup1Click(Sender: TObject);
  begin
  case RadioGroup1.ItemIndex of
    0:
      begin
      PageControl1.Visible := true;
      CheckBox5.Visible    := false;
      Memo1.Height := 80;
      GroupBox1.Caption := 'Data set';
      end;
    1:
      begin
      PageControl1.Visible := false;
      CheckBox5.Visible    := true;
      Memo1.Height := 310;
      GroupBox1.Caption := 'Directory List';
      end;
    end;
  end;

//-------------------------------------------------------------------------------------------------

procedure TFmCross.SpinEdit1Change(Sender: TObject);
  begin
  if trim(SpinEdit1.Text) <> '' then
     begin
     FSpin1 := true;
     if not FSpin2 then
        SpinEdit2.Value := FFGroupsCount - SpinEdit1.Value;
     Fspin1 := false;
     end;
  end;

//-------------------------------------------------------------------------------------------------

procedure TFmCross.SpinEdit2Change(Sender: TObject);
  begin
  if trim(SpinEdit2.Text) <> '' then
     begin
     FSpin2 := true;
     if not FSpin1 then
        SpinEdit1.Value := FFGroupsCount - SpinEdit2.Value;
     Fspin2 := false;
     end;
  end;

//-------------------------------------------------------------------------------------------------

procedure TFmCross.Timer1Timer(Sender: TObject);
  begin
  if Gambiarra then
     if MessageDlg('There is a problem with the file system. Do you want to abort the operation?',
        MtInformation, [MbYes, MbNo], 0) = MrNo then
        Timer1.Enabled := true
     else
        Gambiarra := false;
  end;

//-------------------------------------------------------------------------------------------------

end.


