{
  @abstract(Information about the Hekaton Kheir project)
  @author(Rafa "Passarel" Borges <passarel@gmail.com>)
  @created(Porto Alegre, September, 2005)
  @lastmod(London, January, 2010)
}

unit UnAbout;

interface

uses
   Classes, Controls, Forms, Messages, StdCtrls, SysUtils, Windows;

//-------------------------------------------------------------------------------------------------

type
  TFmAbout = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  end;

var
  FmAbout: TFmAbout;

//-------------------------------------------------------------------------------------------------

implementation

{$R *.dfm}

//-------------------------------------------------------------------------------------------------

procedure TFmAbout.Button1Click(Sender: TObject);
  begin
  Close;
  end;

//-------------------------------------------------------------------------------------------------  

end.
