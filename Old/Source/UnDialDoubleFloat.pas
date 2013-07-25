{
  @abstract(Unit with dialog to edit two numbers, value and tolerance, used to build charts)
  @author(Rafa "Passarel" Borges <passarel@gmail.com>)
  @created(London, 2008)
  @lastmod(London, January, 2010)
}

unit UnDialDoubleFloat;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

//------------------------------------------------------------------------------------------------

type
  TFmDoubleFloat = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    //Indicates if the OK button was pressed
    FOK : boolean;
  public
    //Shows the form, filling the edit boxes with the given values
    //@param(DoubleValue1: numeric variable representing the first value)
    //@param(DoubleValue1: numeric variable representing the first value)
    //@return(True if the OK button was pressed, false otherwise)
    function Execute(var DoubleValue1 : double; var DoubleValue2 : double) : boolean;
  end;

//------------------------------------------------------------------------------------------------

var
  FmDoubleFloat: TFmDoubleFloat;

//------------------------------------------------------------------------------------------------

implementation

{$R *.dfm}

//------------------------------------------------------------------------------------------------

procedure TFmDoubleFloat.Button2Click(Sender: TObject);
  begin
  FOK := false;
  Close;
  end;

//------------------------------------------------------------------------------------------------

procedure TFmDoubleFloat.Button1Click(Sender: TObject);
  begin
  FOK := true;
  Close;
  end;

//------------------------------------------------------------------------------------------------

function TFmDoubleFloat.Execute(var DoubleValue1 : double; var DoubleValue2 : double) : boolean;
  begin
  FOK := false;
  Edit1.text := Floattostr(DoubleValue1);
  Edit2.text := Floattostr(DoubleValue2);
  ShowModal;
  if FOK then
     begin
     DoubleValue1 := StrToFloat(Edit1.Text);
     DoubleValue2 := StrToFloat(Edit2.Text);
     end;
  result := FOK;
  end;

//------------------------------------------------------------------------------------------------

end.
