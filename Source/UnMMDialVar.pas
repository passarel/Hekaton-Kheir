unit UnMMDialVar;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Menus, UnMMStructure;

type
  TFmMMDialVar = class(TForm)
    GroupBox1: TGroupBox;
    ListBox1: TListBox;
    Label1: TLabel;
    ListBox2: TListBox;
    Label2: TLabel;
    PopupMenu1: TPopupMenu;
    Delete1: TMenuItem;
    Sort1: TMenuItem;
    Edit1: TEdit;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    PopupMenu2: TPopupMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Delete1Click(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure Sort1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
  private
    FOK : boolean;
  public
    function Execute(Structure : TRafaMooreMachine) : boolean;
  end;

var
  FmMMDialVar: TFmMMDialVar;

implementation

{$R *.dfm}

procedure TFmMMDialVar.Button1Click(Sender: TObject);
  begin
  if ListBox1.Count + Listbox2.Count <= 20 then
     ListBox1.AddItem(Edit1.Text, nil)
  else
     MessageDlg('Maximum number of variables exceeded', MtError, [MbOK], 0);
  end;

procedure TFmMMDialVar.Button3Click(Sender: TObject);
  begin
  if ListBox1.Count + Listbox2.Count <= 20 then
     ListBox2.AddItem(Edit1.Text, nil)
  else
     MessageDlg('Maximum number of variables exceeded', MtError, [MbOK], 0);
  end;

procedure TFmMMDialVar.Button2Click(Sender: TObject);
  begin
  if ListBox1.ItemIndex >= 0 then
     ListBox1.Items[ListBox1.ItemIndex] := Edit1.Text;
  end;

procedure TFmMMDialVar.Button4Click(Sender: TObject);
  begin
  if ListBox2.ItemIndex >= 0 then
     ListBox2.Items[ListBox2.ItemIndex] := Edit1.Text;
  end;

function TFmMMDialVar.Execute(Structure : TRafaMooreMachine) : boolean;
  begin
  FOK := false;
  ShowModal;
  if FOK and (Structure <> nil) then
     begin
     Structure.LoadFromVariables(ListBox1.Items, ListBox2.Items);
     result := true;
     end
  else
     result := false;
  end;


procedure TFmMMDialVar.Delete1Click(Sender: TObject);
  begin
  ListBox1.DeleteSelected;
  end;

procedure TFmMMDialVar.MenuItem1Click(Sender: TObject);
  begin
  ListBox2.DeleteSelected;
  end;

  

procedure TFmMMDialVar.Sort1Click(Sender: TObject);
  begin
  ListBox1.Sorted := true;
  ListBox1.Sorted := false;
  end;

procedure TFmMMDialVar.MenuItem2Click(Sender: TObject);
  begin
  ListBox2.Sorted := true;
  ListBox2.Sorted := false;
  end;

procedure TFmMMDialVar.Button5Click(Sender: TObject);
  begin
  FOK := true;
  Close;
  end;

procedure TFmMMDialVar.Button6Click(Sender: TObject);
  begin
  Close;
  end;

end.
