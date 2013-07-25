{
  @abstract(Unit that describes the interface for editing logic programs)
  @author(Rafa "Passarel" Borges <passarel@gmail.com)
  @created()
  @lastmod()
}
unit UnLogForm; //265 lines

interface

uses Forms, Dialogs, Menus, Classes, Controls, StdCtrls, UnLogGen, UnNetRep;

{------------------------------------------------------------------------------}

{
@abstract(Classes that encapsulates the visual interface for creation and
  edition of logic programs)
}
type TFmLogic = class(TForm)
    MainMenu1: TMainMenu;
    Arquivo1: TMenuItem;
    Novo1: TMenuItem;
    Abrir1: TMenuItem;
    Salvar1: TMenuItem;
    Sair1: TMenuItem;
    Executar1: TMenuItem;
    Memo1: TMemo;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    procedure Novo1Click(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
    procedure Abrir1Click(Sender: TObject);
    procedure Salvar1Click(Sender: TObject);
    procedure Sair1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure N1Click(Sender: TObject);
    procedure Gerar1Click(Sender: TObject);
    procedure GerarredeCILP1Click(Sender: TObject);
    procedure Executar1Click(Sender: TObject);

  private
    //
    FFileName : string;
    //
    FSaved    : boolean;
    //
    FProgram  : TGenLogicProgram;
    //
    FNetwork  : TNetworkRep;

  public
    //
    //@param
    procedure ChamaInterfaceLogica(rede : TNetworkRep);

  end;

{------------------------------------------------------------------------------}

var FmLogic : TFmLogic;

implementation

{$R *.dfm}

//uses

{------------------------------------------------------------------------------}

{ TFmLogic }

procedure TFmLogic.Novo1Click(Sender: TObject);
  begin
  if not FSaved then
     case MessageDlg('Deseja salvar as alterações?', MtConfirmation, [MbYes, MbNo, MbCancel], 0) of
       MrYes :
         if SaveDialog1.Execute then
            begin
            Memo1.Lines.SaveToFile(SaveDialog1.FileName);
            Memo1.Clear;
            FSaved := true;
            end;
       MrNo :
         begin
         Memo1.Clear;
         FSaved := true;
         end;
       end
  else
     begin
     Memo1.Clear;
     FSaved := true;
     end;
  end;

procedure TFmLogic.Memo1Change(Sender: TObject);
  begin
  FSaved := false;
  end;

procedure TFmLogic.Abrir1Click(Sender: TObject);
  begin
  if not FSaved then
     case MessageDlg('Deseja salvar as alterações?', MtConfirmation, [MbYes, MbNo, MbCancel], 0) of
       MrYes :
         if SaveDialog1.Execute then
            begin
            Memo1.Lines.SaveToFile(SaveDialog1.FileName);
            FSaved := true;
            if OpenDialog1.Execute then
               begin
               FFileName := OpenDialog1.FileName;
               Memo1.Lines.LoadFromFile(FFileName);
               end;
            end;
       MrNo :
         if OpenDialog1.Execute then
            begin
            FFileName := OpenDialog1.FileName;
            Memo1.Lines.LoadFromFile(FFileName);
            FSaved := true;
            end;
       end
  else
     if OpenDialog1.Execute then
        begin
        FFileName := OpenDialog1.FileName;
        Memo1.Lines.LoadFromFile(FFileName);
        FSaved := true;
        end;
  end;
procedure TFmLogic.Salvar1Click(Sender: TObject);
  begin
  if SaveDialog1.Execute then
     begin
     FFileName := SaveDialog1.FileName;
     Memo1.Lines.SaveToFile(FFileName);
     FSaved := true;
     end;
  end;

procedure TFmLogic.Sair1Click(Sender: TObject);
  begin
  Close;
  end;

procedure TFmLogic.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  begin
  if not FSaved then
     case MessageDlg('Deseja salvar as alterações?', MtConfirmation, [MbYes, MbNo, MbCancel], 0) of
       MrYes :
         if SaveDialog1.Execute then
            begin
            Memo1.Lines.SaveToFile(SaveDialog1.FileName);
            Memo1.Clear;
            FSaved := true;
            CanClose := true;
            end;
       MrNo :
         begin
         Memo1.Clear;
         FSaved := true;
         CanClose := true;
         end;
       end
  else
     begin
     Memo1.Clear;
     FSaved := true;
     CanClose := true;
     end;
  end;

procedure TFmLogic.FormCreate(Sender: TObject);
  begin
  FSaved := true;
  FProgram := TGenLogicProgram.Create;
  end;

procedure TFmLogic.N1Click(Sender: TObject);
  begin
  FProgram.LoadFromFile(FFileName);
  FProgram.SaveToFile(FFileName + '1');
  FProgram.SaveSubToFile(FFileName + '2');
  FProgram.GenerateNetwork(FNetwork);


{  if not FSaved then
     case MessageDlg('Esta operação será executada na última versão salva' + #13 +
     'Deseja salvar as alterações', MtConfirmation, [MbYes, MbNo, MbCancel], 0) of
       MrYes :
         if SaveDialog1.Execute then
            begin
            Memo1.Lines.SaveToFile(SaveDialog1.FileName);
            FFileName := SaveDialog1.FileName;
            FProgram.LoadFromFile(FFileName);
//            FProgram.SaveToHK91Network(FNetwork);
            FSaved := true;
            Close;
            end;
       MrNo :
         begin
         FSaved := true;
//         FProgram.SaveToHK91Network(FNetwork);
         Close;
         end;
       end
  else
     begin
//     FProgram.SaveToHK91Network(FNetwork);
     Close;
     end;}
  end;

procedure TFmLogic.Gerar1Click(Sender: TObject);
  begin
  if not FSaved then
     case MessageDlg('Esta operação será executada na última versão salva' + #13 +
     'Deseja salvar as alterações', MtConfirmation, [MbYes, MbNo, MbCancel], 0) of
       MrYes :
         if SaveDialog1.Execute then
            begin
            Memo1.Lines.SaveToFile(SaveDialog1.FileName);
            FFileName := SaveDialog1.FileName;
            FProgram.LoadFromFile(FFileName);
//            FProgram.SaveToKBANNNetwork(FNetwork);
            FSaved := true;
            Close;
            end;
       MrNo :
         begin
         FSaved := true;
//         FProgram.SaveToKBANNNetwork(FNetwork);
         Close;
         end;
       end
  else
     begin
//     FProgram.SaveToKBANNNetwork(FNetwork);
     Close;
     end;
  end;

procedure TFmLogic.ChamaInterfaceLogica(rede: TNetworkRep);
  begin
  FNetwork := Rede;
  ShowModal;
  end;

procedure TFmLogic.GerarredeCILP1Click(Sender: TObject);
  begin
  if not FSaved then
     case MessageDlg('Esta operação será executada na última versão salva' + #13 +
     'Deseja salvar as alterações', MtConfirmation, [MbYes, MbNo, MbCancel], 0) of
       MrYes :
         if SaveDialog1.Execute then
            begin
            Memo1.Lines.SaveToFile(SaveDialog1.FileName);
            FFileName := SaveDialog1.FileName;
            FProgram.LoadFromFile(FFileName);
//            FProgram.SaveToCILPNetwork(FNetwork);
            FSaved := true;
            Close;
            end;
       MrNo :
         begin
         FSaved := true;
//         FProgram.SaveToCILPNetwork(FNetwork);
         Close;
         end;
       end
  else
     begin
//     FProgram.SaveToCILPNetwork(FNetwork);
     Close;
     end;
  end;


procedure TFmLogic.Executar1Click(Sender: TObject);
  begin
  if not FSaved then
     case MessageDlg('Esta operação será executada na última versão salva' + #13 +
     'Deseja salvar as alterações', MtConfirmation, [MbYes, MbNo, MbCancel], 0) of
       MrYes :
         if SaveDialog1.Execute then
            begin
            Memo1.Lines.SaveToFile(SaveDialog1.FileName);
            FFileName := SaveDialog1.FileName;
            FProgram.LoadFromFile(FFileName);
            FProgram.SaveToFile(FFileName + '1');
            FProgram.SaveSubToFile(FFileName + '2');
            FProgram.SaveAtomsToFile(FFileName + '1a');
            FProgram.GenerateNetwork(FNetwork);
            FSaved := true;
            Close;
            end;
       MrNo :
         begin
         FSaved := true;
         FProgram.SaveToFile(FFileName + '1');
         FProgram.SaveSubToFile(FFileName + '2');
         FProgram.SaveAtomsToFile(FFileName + '1a');
         FProgram.GenerateNetwork(FNetwork);
         Close;
         end;
       end
  else
     begin
     FProgram.LoadFromFile(FFileName);
     FProgram.SaveToFile(FFileName + '1');
     FProgram.SaveSubToFile(FFileName + '2');
     FProgram.SaveAtomsToFile(FFileName + '1a');
     FProgram.GenerateNetwork(FNetwork);
     Close;
     end;

  end;

end.
